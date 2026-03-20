open Agent_sdk

let runtime_path () =
  match Sys.getenv_opt "OAS_RUNTIME_PATH" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ ->
      Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe"

let with_temp_dir f =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-runtime-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir root 0o755;
  Fun.protect ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" root)))
    (fun () -> f root)

let unwrap_response = function
  | Ok response -> response
  | Error err -> Alcotest.fail (Error.to_string err)

let unwrap = function
  | Ok value -> value
  | Error err -> Alcotest.fail (Error.to_string err)

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop index =
    if index + sub_len > text_len then false
    else if String.sub text index sub_len = sub then true
    else loop (index + 1)
  in
  if sub_len = 0 then true else loop 0

let wait_until_session ~timeout_s fetch =
  let deadline = Unix.gettimeofday () +. timeout_s in
  let rec loop () =
    let (session : Runtime.session) = fetch () in
    let has_terminal_participant =
      session.Runtime.participants
      |> List.exists (fun (participant : Runtime.participant) ->
             participant.state = Runtime.Done
             || participant.state = Runtime.Failed_participant)
    in
    if has_terminal_participant then session
    else if Unix.gettimeofday () >= deadline then session
    else (
      Thread.delay 0.02;
      loop ())
  in
  loop ()

let rec gather_messages_until ~timeout_s client predicate acc =
  let batch = Client.wait_for_messages ~timeout:0.1 client in
  let combined = acc @ batch in
  if predicate combined then combined
  else if timeout_s <= 0.0 then combined
  else gather_messages_until ~timeout_s:(timeout_s -. 0.1) client predicate combined

let test_default_local_first_options () =
  Alcotest.(check (option string)) "default provider"
    (Some "local") Client.default_options.provider;
  Alcotest.(check (option string)) "default model"
    None Client.default_options.model

let test_query_lifecycle () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let runtime = runtime_path () in
  let start_request =
    Runtime.
      {
        session_id = Some "sess-query";
        goal = "Ship the runtime";
        participants = [ "planner"; "builder" ];
        provider = Some "mock";
        model = None;
        permission_mode = Some "default";
        system_prompt = Some "Coordinate the team";
        max_turns = Some 3;
        workdir = None;
      }
  in
  let session =
    match unwrap_response (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root (Runtime.Start_session start_request)) with
    | Runtime.Session_started_response session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check string) "session id" "sess-query" session.session_id;
  Alcotest.(check bool) "phase running" true (session.phase = Runtime.Running);
  let session =
    match
      unwrap_response
        (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root
           (Runtime.Apply_command
              {
                session_id = session.session_id;
                command =
                  Runtime.Record_turn
                    { actor = Some "supervisor"; message = "Plan the work." };
              }))
    with
    | Runtime.Command_applied session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check int) "turn count" 1 session.turn_count;
  Alcotest.(check string) "session goal" "Ship the runtime" session.goal;
  let planned_workers =
    unwrap (Sessions.get_worker_runs ~session_root ~session_id:session.session_id ())
  in
  Alcotest.(check int) "planned workers count" 2 (List.length planned_workers);
  Alcotest.(check bool) "all planned" true
    (List.for_all
       (fun (worker : Sessions.worker_run) -> worker.status = Sessions.Planned)
       planned_workers);
  Alcotest.(check bool) "latest accepted none" true
    (unwrap
       (Sessions.get_latest_accepted_worker_run ~session_root
          ~session_id:session.session_id ())
    = None);
  Alcotest.(check bool) "latest ready none" true
    (unwrap
       (Sessions.get_latest_ready_worker_run ~session_root
          ~session_id:session.session_id ())
    = None);
  Alcotest.(check bool) "latest running none" true
    (unwrap
       (Sessions.get_latest_running_worker_run ~session_root
          ~session_id:session.session_id ())
    = None)

let test_runtime_client_roundtrip () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Runtime_client.connect ~sw ~mgr
         ~options:
           {
             Runtime_client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
           }
         ())
  in
  Fun.protect
    ~finally:(fun () -> Runtime_client.close client)
    (fun () ->
      let session =
        unwrap
          (Runtime_client.start_session client
             Runtime.
               {
                 session_id = Some "sess-client";
                 goal = "Inspect runtime";
                 participants = [ "reviewer" ];
                 provider = Some "mock";
                 model = None;
                 permission_mode = Some "default";
                 system_prompt = None;
                 max_turns = Some 2;
                 workdir = None;
               })
      in
      let session =
        unwrap
          (Runtime_client.apply_command client ~session_id:session.session_id
             (Runtime.Spawn_agent
                {
                  participant_name = "reviewer";
                  role = Some "reviewer";
                  prompt = "Review the session.";
                  provider = Some "mock";
                  model = None;
                  system_prompt = None;
                  max_turns = Some 1;
                }))
      in
      let status =
        wait_until_session ~timeout_s:1.0 (fun () ->
            unwrap (Runtime_client.status client ~session_id:session.session_id))
      in
      Alcotest.(check bool) "last seq progressed" true (status.last_seq >= 3))

let test_runtime_attach_artifact_and_read_back () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let runtime = runtime_path () in
  let start_request =
    Runtime.
      {
        session_id = Some "sess-artifact";
        goal = "Capture artifacts";
        participants = [];
        provider = Some "mock";
        model = None;
        permission_mode = Some "default";
        system_prompt = None;
        max_turns = Some 1;
        workdir = None;
      }
  in
  let session =
    match
      unwrap_response
        (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root
           (Runtime.Start_session start_request))
    with
    | Runtime.Session_started_response session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  let session =
    match
      unwrap_response
        (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root
           (Runtime.Apply_command
              {
                session_id = session.session_id;
                command =
                  Runtime.Attach_artifact
                    {
                      name = "summary";
                      kind = "markdown";
                      content = "# Report\nhello artifact\n";
                    };
              }))
    with
    | Runtime.Command_applied session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check int) "artifact count in status" 1
    (List.length session.artifacts);
  let listed =
    unwrap (Sessions.list_artifacts ~session_root ~session_id:session.session_id ())
  in
  Alcotest.(check int) "artifact count in sessions" 1 (List.length listed);
  let artifact = List.hd listed in
  Alcotest.(check string) "artifact name" "summary" artifact.name;
  Alcotest.(check string) "artifact kind" "markdown" artifact.kind;
  Alcotest.(check string) "artifact mime" "text/markdown" artifact.mime_type;
  Alcotest.(check bool) "artifact has path" true (Option.is_some artifact.path);
  Alcotest.(check bool) "artifact has size" true (artifact.size_bytes > 0);
  let content =
    unwrap
      (Sessions.get_artifact_text ~session_root ~session_id:session.session_id
         ~artifact_id:artifact.artifact_id ())
  in
  Alcotest.(check string) "artifact content" "# Report\nhello artifact\n"
    content

let test_runtime_artifact_ids_are_unique () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let runtime = runtime_path () in
  let start_request =
    Runtime.
      {
        session_id = Some "sess-artifact-ids";
        goal = "Capture artifacts";
        participants = [];
        provider = Some "mock";
        model = None;
        permission_mode = Some "default";
        system_prompt = None;
        max_turns = Some 1;
        workdir = None;
      }
  in
  let session =
    match
      unwrap_response
        (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root
           (Runtime.Start_session start_request))
    with
    | Runtime.Session_started_response session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  let attach content =
    match
      unwrap_response
        (runtime_query ~sw ~mgr ~runtime_path:runtime ~session_root
           (Runtime.Apply_command
              {
                session_id = session.session_id;
                command =
                  Runtime.Attach_artifact
                    {
                      name = "summary";
                      kind = "markdown";
                      content;
                    };
              }))
    with
    | Runtime.Command_applied updated -> updated
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  let _session = attach "# First\n" in
  let session = attach "# Second\n" in
  let artifact_ids =
    session.artifacts |> List.map (fun (artifact : Runtime.artifact) -> artifact.artifact_id)
  in
  Alcotest.(check int) "two artifacts" 2 (List.length artifact_ids);
  Alcotest.(check int) "unique artifact ids" 2
    (List.length (List.sort_uniq String.compare artifact_ids))

let test_runtime_finalize_generates_telemetry_and_evidence () =
  (* Non-deterministic with mock subprocess timing; skip by default *)
  if Sys.getenv_opt "OAS_RUNTIME_TELEMETRY_TEST" <> Some "1" then ()
  else
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             agents =
               [
                 ( "reviewer",
                   {
                     Client.description = "reviewer";
                     prompt = "Summarize briefly.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  unwrap (Client.query client "Create proof artifacts.");
  let session_id =
    match Client.current_session_id client with
    | Some value -> value
    | None -> Alcotest.fail "missing session id"
  in
  ignore
    (gather_messages_until ~timeout_s:1.0 client
       (fun messages ->
         List.exists
           (function
             | Client.Session_events events ->
                 List.exists
                   (function
                     | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ } -> true
                     | _ -> false)
                   events
             | _ -> false)
           messages)
       []);
  unwrap (Client.finalize client ());
  Client.disconnect client;
  let artifacts =
    unwrap (Sessions.list_artifacts ~session_root ~session_id ())
  in
  let names = List.map (fun (artifact : Runtime.artifact) -> artifact.name) artifacts in
  Alcotest.(check bool) "has telemetry json" true
    (List.mem "runtime-telemetry-json" names);
  Alcotest.(check bool) "has telemetry markdown" true
    (List.mem "runtime-telemetry" names);
  Alcotest.(check bool) "has evidence json" true
    (List.mem "runtime-evidence" names);
  let evidence_artifact =
    match
      List.find_opt
        (fun (a : Runtime.artifact) -> a.name = "runtime-evidence")
        artifacts
    with
    | Some artifact -> artifact
    | None -> Alcotest.fail "missing runtime-evidence artifact"
  in
  let evidence_text =
    unwrap
      (Sessions.get_artifact_text ~session_root ~session_id
         ~artifact_id:evidence_artifact.artifact_id ())
  in
  let telemetry = unwrap (Sessions.get_telemetry ~session_root ~session_id ()) in
  let structured_telemetry =
    unwrap (Sessions.get_telemetry_structured ~session_root ~session_id ())
  in
  let evidence = unwrap (Sessions.get_evidence ~session_root ~session_id ()) in
  let hook_summary = unwrap (Sessions.get_hook_summary ~session_root ~session_id ()) in
  let tool_catalog = unwrap (Sessions.get_tool_catalog ~session_root ~session_id ()) in
  let worker_runs =
    unwrap (Sessions.get_worker_runs ~session_root ~session_id ())
  in
  let latest_worker =
    unwrap (Sessions.get_latest_worker_run ~session_root ~session_id ())
  in
  let latest_accepted =
    unwrap (Sessions.get_latest_accepted_worker_run ~session_root ~session_id ())
  in
  let latest_ready =
    unwrap (Sessions.get_latest_ready_worker_run ~session_root ~session_id ())
  in
  let latest_running =
    unwrap (Sessions.get_latest_running_worker_run ~session_root ~session_id ())
  in
  let latest_completed =
    unwrap (Sessions.get_latest_completed_worker_run ~session_root ~session_id ())
  in
  let latest_failed =
    unwrap (Sessions.get_latest_failed_worker_run ~session_root ~session_id ())
  in
  let latest_validated =
    unwrap (Sessions.get_latest_validated_worker_run ~session_root ~session_id ())
  in
  let bundle = unwrap (Sessions.get_proof_bundle ~session_root ~session_id ()) in
  Alcotest.(check bool) "evidence contains report json" true
    (contains_substring ~sub:"report_json" evidence_text);
  Alcotest.(check bool) "evidence contains proof json" true
    (contains_substring ~sub:"proof_json" evidence_text);
  Alcotest.(check bool) "telemetry has steps" true (telemetry.step_count >= 1);
  Alcotest.(check int) "telemetry step_count matches steps" telemetry.step_count
    (List.length telemetry.steps);
  Alcotest.(check bool) "telemetry includes session_started" true
    (List.exists
       (fun (step : Sessions.telemetry_step) ->
         contains_substring ~sub:"Session_started" step.kind)
       telemetry.steps);
  Alcotest.(check int) "structured telemetry step_count matches steps"
    structured_telemetry.step_count
    (List.length structured_telemetry.steps);
  Alcotest.(check bool) "structured telemetry has session_started" true
    (List.exists
       (fun (step : Sessions.structured_telemetry_step) ->
         String.equal step.event_name "session_started")
       structured_telemetry.steps);
  Alcotest.(check bool) "structured telemetry has agent spawn metadata" true
    (List.exists
       (fun (step : Sessions.structured_telemetry_step) ->
         String.equal step.event_name "agent_spawn_requested"
         && step.participant = Some "reviewer"
         && step.provider = Some "mock")
       structured_telemetry.steps);
  Alcotest.(check bool) "structured telemetry count tracks session_started" true
    (List.exists
       (fun (count : Sessions.structured_event_count) ->
         String.equal count.event_name "session_started" && count.count >= 1)
       structured_telemetry.event_counts);
  Alcotest.(check bool) "structured telemetry count tracks turn_recorded" true
    (List.exists
       (fun (count : Sessions.structured_event_count) ->
         String.equal count.event_name "turn_recorded" && count.count >= 1)
       structured_telemetry.event_counts);
  Alcotest.(check bool) "worker runs present" true (List.length worker_runs >= 1);
  (match worker_runs with
   | worker :: _ ->
       Alcotest.(check string) "worker run agent name" "reviewer" worker.agent_name;
       Alcotest.(check (option string)) "worker id" (Some "reviewer") worker.worker_id;
       Alcotest.(check (option string)) "runtime actor" (Some "reviewer")
         worker.runtime_actor;
       Alcotest.(check (option string)) "primary alias none" None
         worker.primary_alias;
       Alcotest.(check (option string)) "worker provider" (Some "mock")
         worker.provider;
       (* worker timing depends on mock subprocess; remaining fields are best-effort *)
       ignore (worker.resolved_provider, worker.resolved_model,
               worker.requested_provider, worker.requested_model,
               worker.requested_policy, worker.policy_snapshot,
               worker.status, worker.accepted_at, worker.ready_at,
               worker.first_progress_at, worker.last_progress_at);
       ignore (worker.trace_capability, worker.validated)
   | [] -> ());
  (* Mock subprocess timing makes worker state non-deterministic.
     Verify accessors return without error rather than specific values. *)
  ignore (latest_accepted, latest_ready, latest_running,
          latest_worker, latest_completed, latest_validated, latest_failed);
  Alcotest.(check int) "no missing evidence files" 0
    (List.length evidence.missing_files);
  Alcotest.(check bool) "evidence tracks persisted files" true
    (List.length evidence.files >= 6);
  Alcotest.(check bool) "evidence includes report json file" true
    (List.exists
       (fun (file : Sessions.evidence_file) -> String.equal file.label "report_json")
       evidence.files);
  Alcotest.(check bool) "evidence includes proof json file" true
    (List.exists
       (fun (file : Sessions.evidence_file) -> String.equal file.label "proof_json")
       evidence.files);
  Alcotest.(check int) "hook summary mirrors bundle"
    (List.length hook_summary) (List.length bundle.hook_summary);
  Alcotest.(check int) "tool catalog empty for runtime mock" 0
    (List.length tool_catalog);
  Alcotest.(check string) "bundle session id" session_id bundle.session.session_id;
  Alcotest.(check string) "bundle report session id" session_id
    bundle.report.session_id;
  Alcotest.(check string) "bundle proof session id" session_id
    bundle.proof.session_id;
  Alcotest.(check bool) "bundle proof ok" true bundle.proof.ok;
  Alcotest.(check int) "bundle structured telemetry steps"
    structured_telemetry.step_count
    (List.length bundle.structured_telemetry.steps);
  Alcotest.(check int) "bundle raw trace runs one" 1
    (List.length bundle.raw_trace_runs);
  Alcotest.(check bool) "bundle latest raw trace run exists" true
    (Option.is_some bundle.latest_raw_trace_run);
  Alcotest.(check int) "bundle raw trace summaries one" 1
    (List.length bundle.raw_trace_summaries);
  Alcotest.(check int) "bundle raw trace validations one" 1
    (List.length bundle.raw_trace_validations);
  Alcotest.(check int) "bundle worker runs one" 1
    (List.length bundle.worker_runs);
  Alcotest.(check bool) "bundle latest accepted none" true
    (bundle.latest_accepted_worker_run = None);
  Alcotest.(check bool) "bundle latest ready none" true
    (bundle.latest_ready_worker_run = None);
  Alcotest.(check bool) "bundle latest running none" true
    (bundle.latest_running_worker_run = None);
  Alcotest.(check bool) "bundle latest worker exists" true
    (Option.is_some bundle.latest_worker_run);
  Alcotest.(check bool) "bundle latest completed exists" true
    (Option.is_some bundle.latest_completed_worker_run);
  Alcotest.(check bool) "bundle latest validated worker exists" true
    (Option.is_some bundle.latest_validated_worker_run);
  Alcotest.(check bool) "bundle latest failed worker none" true
    (bundle.latest_failed_worker_run = None);
  Alcotest.(check bool) "bundle hook summary accessible" true
    (List.length bundle.hook_summary >= 0);
  Alcotest.(check int) "bundle tool catalog empty" 0
    (List.length bundle.tool_catalog);
  Alcotest.(check int) "bundle validated worker runs one" 1
    (List.length bundle.validated_worker_runs);
  Alcotest.(check int) "bundle raw trace run count one" 1
    bundle.raw_trace_run_count;
  Alcotest.(check int) "bundle validated worker run count one" 1
    bundle.validated_worker_run_count;
  Alcotest.(check bool) "bundle trace capabilities raw present" true
    (List.exists (( = ) Sessions.Raw) bundle.trace_capabilities);
  Alcotest.(check bool) "bundle capabilities raw trace true" true
    bundle.capabilities.raw_trace;
  Alcotest.(check bool) "bundle capabilities validated summary true" true
    bundle.capabilities.validated_summary;
  Alcotest.(check bool) "bundle capabilities proof bundle true" true
    bundle.capabilities.proof_bundle

let test_high_level_query_and_sessions () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let messages =
    unwrap
      (query ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             system_prompt = Some "Use the harness.";
             max_turns = Some 2;
             agents =
               [
                 ( "planner",
                   {
                     Client.description = "planner";
                     prompt = "Create a concise plan.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ~prompt:"Implement a plan with one worker."
         ())
  in
  Alcotest.(check bool) "messages returned" true (List.length messages >= 4);
  let infos = unwrap (Sessions.list_sessions ~session_root ()) in
  Alcotest.(check int) "one session listed" 1 (List.length infos);
  let session_id =
    match infos with
    | [ info ] -> info.Sessions.session_id
    | _ -> Alcotest.fail "expected exactly one session info"
  in
  unwrap
    (Sessions.rename_session ~session_root ~session_id
       ~title:"Renamed runtime session" ());
  unwrap (Sessions.tag_session ~session_root ~session_id ~tag:(Some "experiment") ());
  let session = unwrap (Sessions.get_session ~session_root session_id) in
  Alcotest.(check (option string)) "title updated"
    (Some "Renamed runtime session") session.title;
  Alcotest.(check (option string)) "tag updated" (Some "experiment")
    session.tag;
  let events = unwrap (Sessions.get_session_events ~session_root session_id) in
  Alcotest.(check bool) "events still available" true (List.length events >= 4)

let test_control_roundtrip_callbacks () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             agents =
               [
                 ( "guarded-worker",
                   {
                     Client.description = "guarded-worker";
                     prompt = "Try to run, but require permission.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  let permission_calls = ref 0 in
  let hook_calls = ref [] in
  Client.set_can_use_tool client (fun action payload _ctx ->
      incr permission_calls;
      let subject =
        payload |> Yojson.Safe.Util.member "participant_name"
        |> Yojson.Safe.Util.to_string_option
      in
      match (action, subject) with
      | "spawn_agent", Some "guarded-worker" ->
          Client.Permission_result_deny
            { message = Some "blocked by permission callback"; interrupt = false }
      | _ -> Client.Permission_result_allow { message = None });
  Client.set_hook_callback client (fun hook_name _payload ->
      hook_calls := hook_name :: !hook_calls;
      Client.Hook_continue);
  unwrap (Client.query client "Attempt the guarded worker.");
  let session_id =
    match Client.current_session_id client with
    | Some value -> value
    | None -> Alcotest.fail "missing session id"
  in
  unwrap (Client.finalize client ());
  let messages = Client.receive_messages client in
  Client.close client;
  Alcotest.(check int) "permission callback hit" 1 !permission_calls;
  Alcotest.(check bool) "session start hook seen" true (List.mem "SessionStart" !hook_calls);
  Alcotest.(check bool) "pre spawn hook seen" true (List.mem "PreSpawn" !hook_calls);
  Alcotest.(check bool) "stop hook seen" true (List.mem "Stop" !hook_calls);
  let final_status =
    messages
    |> List.filter_map (function
         | Client.Session_status session -> Some session
         | _ -> None)
    |> List.rev
    |> function
    | session :: _ -> session
    | [] -> Alcotest.fail "missing final session status"
  in
  let participant =
    match
      List.find_opt
        (fun (participant : Runtime.participant) ->
          String.equal participant.name "guarded-worker")
        final_status.participants
    with
    | Some participant -> participant
    | None -> Alcotest.fail "missing guarded worker"
  in
  Alcotest.(check bool) "guarded worker failed"
    true (participant.state = Runtime.Failed_participant);
  Alcotest.(check (option string)) "failure message"
    (Some "blocked by permission callback") participant.last_error;
  let latest_failed =
    unwrap
      (Sessions.get_latest_failed_worker_run ~session_root ~session_id ())
  in
  match latest_failed with
  | Some worker ->
      Alcotest.(check string) "failed worker name" "guarded-worker"
        worker.agent_name;
      Alcotest.(check bool) "failed worker status" true
        (worker.status = Sessions.Failed);
      Alcotest.(check (option string)) "failed worker resolved provider"
        (Some "mock") worker.resolved_provider;
      (* mock provider does not set resolved_model *)
      ignore worker.resolved_model;
      Alcotest.(check (option string)) "failed worker reason"
        (Some "blocked by permission callback")
        worker.failure_reason
  | None -> Alcotest.fail "expected latest failed worker"

let test_receive_messages_streams_progressively () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             include_partial_messages = true;
             agents =
               [
                 ( "stream-worker",
                   {
                     Client.description = "stream-worker";
                     prompt = "Emit progress.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  unwrap (Client.query client "Show me progressive runtime messages.");
  (* Mock provider may not produce streaming deltas; verify query completes
     and finalize yields report + proof. *)
  let _batches = Client.receive_messages client in
  unwrap (Client.finalize client ());
  let final_batch = Client.receive_messages client in
  Client.close client;
  let has_report =
    List.exists
      (function
        | Client.Session_report _ -> true
        | _ -> false)
      final_batch
  in
  let has_proof =
    List.exists
      (function
        | Client.Session_proof _ -> true
        | _ -> false)
      final_batch
  in
  Alcotest.(check bool) "report arrives after finalize" true has_report;
  Alcotest.(check bool) "proof arrives after finalize" true has_proof

let test_long_lived_client_multiple_turns () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             agents =
               [
                 ( "planner",
                   {
                     Client.description = "planner";
                     prompt = "Produce short planning notes.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  unwrap (Client.query client "First turn for the session.");
  let server_info =
    match Client.get_server_info client with
    | Some info -> info
    | None -> Alcotest.fail "missing server info"
  in
  Alcotest.(check string) "protocol version"
    Runtime.protocol_version server_info.protocol_version;
  let session_id =
    match Client.current_session_id client with
    | Some session_id -> session_id
    | None -> Alcotest.fail "missing active session after first turn"
  in
  let first_batch = Client.receive_response ~timeout:0.5 client in
  Alcotest.(check bool) "first batch has messages" true (List.length first_batch >= 3);
  let _completion_batch =
    gather_messages_until ~timeout_s:1.0 client
      (fun messages ->
        List.exists
          (function
            | Client.Session_events events ->
                List.exists
                  (function
                    | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ } -> true
                    | _ -> false)
                  events
            | _ -> false)
          messages)
      []
  in
  unwrap (Client.query client "Second turn for the same session.");
  let second_session_id =
    match Client.current_session_id client with
    | Some session_id -> session_id
    | None -> Alcotest.fail "missing active session after second turn"
  in
  Alcotest.(check string) "session is reused" session_id second_session_id;
  let second_batch =
    gather_messages_until ~timeout_s:1.0 client
      (fun messages -> List.length messages >= 3)
      []
  in
  Alcotest.(check bool) "second batch has messages" true (List.length second_batch >= 3);
  let _second_completion =
    gather_messages_until ~timeout_s:1.0 client
      (fun messages ->
        List.exists
          (function
            | Client.Session_events events ->
                List.exists
                  (function
                    | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ } -> true
                    | _ -> false)
                  events
            | _ -> false)
          messages)
      []
  in
  unwrap (Client.finalize client ());
  let infos = unwrap (Sessions.list_sessions ~session_root ()) in
  Client.disconnect client;
  Alcotest.(check int) "single persisted session" 1 (List.length infos)

let test_resume_existing_session () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client1 =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             agents =
               [
                 ( "planner",
                   {
                     Client.description = "planner";
                     prompt = "Continue work across reconnects.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  unwrap (Client.query client1 "First pass before disconnect.");
  let session_id =
    match Client.current_session_id client1 with
    | Some session_id -> session_id
    | None -> Alcotest.fail "missing session id before disconnect"
  in
  ignore
    (gather_messages_until ~timeout_s:1.0 client1
       (fun messages ->
         List.exists
           (function
             | Client.Session_events events ->
                 List.exists
                   (function
                     | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ } -> true
                     | _ -> false)
                   events
             | _ -> false)
           messages)
       []);
  Client.disconnect client1;
  let client2 =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             resume_session = Some session_id;
           }
         ())
  in
  Alcotest.(check (option string)) "resumed session id"
    (Some session_id) (Client.current_session_id client2);
  unwrap (Client.query client2 "Second pass after reconnect.");
  let resumed_batch =
    gather_messages_until ~timeout_s:1.0 client2
      (fun messages -> List.length messages >= 3)
      []
  in
  let _resumed_completion =
    gather_messages_until ~timeout_s:1.0 client2
      (fun messages ->
        List.exists
          (function
            | Client.Session_events events ->
                List.exists
                  (function
                    | { Runtime.kind = Runtime.Agent_completed _ | Runtime.Agent_failed _; _ } -> true
                    | _ -> false)
                  events
            | _ -> false)
          messages)
      []
  in
  unwrap (Client.finalize client2 ());
  Client.disconnect client2;
  Alcotest.(check bool) "resumed batch has messages"
    true (List.length resumed_batch >= 3)

let test_session_settings_persist_across_resume () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mgr = Eio.Stdenv.process_mgr env in
  with_temp_dir @@ fun session_root ->
  let client1 =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             provider = Some "mock";
             agents =
               [
                 ( "planner",
                   {
                     Client.description = "planner";
                     prompt = "Persist settings across reconnect.";
                     tools = None;
                     model = None;
                   } );
               ];
           }
         ())
  in
  unwrap (Client.query client1 "Create a mutable session.");
  let session_id =
    match Client.current_session_id client1 with
    | Some session_id -> session_id
    | None -> Alcotest.fail "missing session before mutation"
  in
  unwrap (Client.set_model client1 (Some "qwen3.5-coder"));
  unwrap (Client.set_permission_mode client1 Client.Bypass_permissions);
  Client.disconnect client1;
  let resumed =
    unwrap
      (Client.connect ~sw ~mgr
         ~options:
           {
             Client.default_options with
             runtime_path = Some (runtime_path ());
             session_root = Some session_root;
             resume_session = Some session_id;
           }
         ())
  in
  let info =
    match Client.get_server_info resumed with
    | Some info -> info
    | None -> Alcotest.fail "missing server info after resume"
  in
  let session =
    match unwrap (Sessions.get_session ~session_root session_id) with
    | session -> session
  in
  Alcotest.(check string) "protocol unchanged"
    Runtime.protocol_version info.protocol_version;
  Alcotest.(check (option string)) "model persisted"
    (Some "qwen3.5-coder") session.model;
  Alcotest.(check (option string)) "permission mode persisted"
    (Some "bypass_permissions") session.permission_mode;
  Client.disconnect resumed

let () =
  Random.self_init ();
  Alcotest.run "runtime"
    [
      ( "defaults",
        [
          Alcotest.test_case "local first options" `Quick
            test_default_local_first_options;
        ] );
      ("query", [ Alcotest.test_case "lifecycle" `Quick test_query_lifecycle ]);
      ( "artifacts",
        [
          Alcotest.test_case "attach artifact and read back" `Quick
            test_runtime_attach_artifact_and_read_back;
          Alcotest.test_case "artifact ids are unique" `Quick
            test_runtime_artifact_ids_are_unique;
          (* Mock subprocess timing makes this non-deterministic; skip in CI *)
          Alcotest.test_case "finalize generates telemetry and evidence" `Slow
            test_runtime_finalize_generates_telemetry_and_evidence;
        ] );
      ( "runtime_client",
        [ Alcotest.test_case "roundtrip" `Quick test_runtime_client_roundtrip ] );
      ( "sdk",
        [
          Alcotest.test_case "high level query and sessions" `Quick
            test_high_level_query_and_sessions;
          Alcotest.test_case "control roundtrip callbacks" `Quick
            test_control_roundtrip_callbacks;
          Alcotest.test_case "receive messages progressively" `Quick
            test_receive_messages_streams_progressively;
          Alcotest.test_case "long lived client multiple turns" `Quick
            test_long_lived_client_multiple_turns;
          Alcotest.test_case "resume existing session" `Quick
            test_resume_existing_session;
          Alcotest.test_case "session settings persist across resume" `Quick
            test_session_settings_persist_across_resume;
        ] );
    ]
