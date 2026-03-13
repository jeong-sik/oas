open Agent_sdk

let runtime_path () =
  "/Users/dancer/me/workspace/yousleepwhen/oas/.worktrees/codex-oas-long-lived/_build/default/bin/oas_runtime.exe"

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
    (Some "local-qwen") Client.default_options.provider;
  Alcotest.(check (option string)) "default model"
    (Some "qwen3.5") Client.default_options.model

let test_query_lifecycle () =
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
    match unwrap_response (runtime_query ~runtime_path:runtime ~session_root (Runtime.Start_session start_request)) with
    | Runtime.Session_started_response session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check string) "session id" "sess-query" session.session_id;
  Alcotest.(check bool) "phase running" true (session.phase = Runtime.Running);
  let session =
    match
      unwrap_response
        (runtime_query ~runtime_path:runtime ~session_root
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
  Alcotest.(check string) "session goal" "Ship the runtime" session.goal

let test_runtime_client_roundtrip () =
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Runtime_client.connect
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

let test_high_level_query_and_sessions () =
  with_temp_dir @@ fun session_root ->
  let messages =
    unwrap
      (query
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
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect
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
    (Some "blocked by permission callback") participant.last_error

let test_receive_messages_streams_progressively () =
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect
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
  let streamed_batches =
    gather_messages_until ~timeout_s:1.0 client
      (fun messages ->
        List.exists
          (function
            | Client.Partial_message _ -> true
            | _ -> false)
          messages)
      []
  in
  Alcotest.(check bool) "streamed batches non-empty" true (List.length streamed_batches >= 3);
  let has_output_delta =
    streamed_batches
    |> List.exists (function
         | Client.Partial_message _ -> true
         | Client.Session_events events ->
             List.exists
               (function
                 | { Runtime.kind = Runtime.Agent_output_delta _; _ } -> true
                 | _ -> false)
               events
         | _ -> false)
  in
  Alcotest.(check bool) "output delta present" true has_output_delta;
  let drained_batch = Client.receive_messages client in
  Alcotest.(check int) "buffer drained" 0 (List.length drained_batch);
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
  Alcotest.(check bool) "report arrives later" true has_report;
  Alcotest.(check bool) "proof arrives later" true has_proof

let test_long_lived_client_multiple_turns () =
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Client.connect
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
  with_temp_dir @@ fun session_root ->
  let client1 =
    unwrap
      (Client.connect
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
      (Client.connect
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
  with_temp_dir @@ fun session_root ->
  let client1 =
    unwrap
      (Client.connect
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
      (Client.connect
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
