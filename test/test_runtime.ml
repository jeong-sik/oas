open Agent_sdk

let runtime_path () =
  let exe_dir = Filename.dirname Sys.executable_name in
  let candidates =
    [
      Filename.concat exe_dir "../bin/oas_runtime.exe";
      Filename.concat exe_dir "oas_runtime.exe";
      Filename.concat (Sys.getcwd ()) "_build/default/bin/oas_runtime.exe";
    ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None ->
      Alcotest.fail
        (Printf.sprintf "Unable to locate oas_runtime.exe from %s" Sys.executable_name)

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
  let session =
    match
      unwrap_response
        (runtime_query ~runtime_path:runtime ~session_root
           (Runtime.Apply_command
              {
                session_id = session.session_id;
                command =
                  Runtime.Spawn_agent
                    {
                      participant_name = "planner";
                      role = Some "planner";
                      prompt = "Break the goal into two tasks.";
                      provider = Some "mock";
                      model = None;
                      system_prompt = None;
                      max_turns = Some 2;
                    };
              }))
    with
    | Runtime.Command_applied session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  let planner =
    match
      List.find_opt
        (fun (participant : Runtime.participant) -> String.equal participant.name "planner")
        ((session : Runtime.session).participants)
    with
    | Some participant -> participant
    | None -> Alcotest.fail "planner participant missing"
  in
  Alcotest.(check bool) "planner done" true (planner.state = Runtime.Done);
  let events =
    match unwrap_response (runtime_query ~runtime_path:runtime ~session_root (Runtime.Events { session_id = session.session_id; after_seq = None })) with
    | Runtime.Events_response events -> events
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check bool) "events non-empty" true (List.length events >= 4);
  let session =
    match unwrap_response (runtime_query ~runtime_path:runtime ~session_root (Runtime.Finalize { session_id = session.session_id; reason = Some "done" })) with
    | Runtime.Finalized session -> session
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check bool) "completed" true (session.phase = Runtime.Completed);
  let report =
    match unwrap_response (runtime_query ~runtime_path:runtime ~session_root (Runtime.Report { session_id = session.session_id })) with
    | Runtime.Report_response report -> report
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check bool) "report mentions session" true
    (String.contains report.markdown 's');
  let proof =
    match unwrap_response (runtime_query ~runtime_path:runtime ~session_root (Runtime.Prove { session_id = session.session_id })) with
    | Runtime.Prove_response proof -> proof
    | other -> Alcotest.fail (Runtime.show_response other)
  in
  Alcotest.(check bool) "proof ok" true proof.ok

let test_runtime_client_roundtrip () =
  with_temp_dir @@ fun session_root ->
  let client =
    unwrap
      (Runtime_client.connect
         ~options:
           {
             Runtime_client.runtime_path = Some (runtime_path ());
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
      let status = unwrap (Runtime_client.status client ~session_id:session.session_id) in
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
  let first_batch = Client.receive_messages client in
  Alcotest.(check bool) "first batch non-empty" true (List.length first_batch >= 3);
  let second_batch = Client.receive_messages client in
  Alcotest.(check int) "buffer drained" 0 (List.length second_batch);
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
        ] );
    ]
