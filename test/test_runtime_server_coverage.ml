open Agent_sdk
module Runtime_server = Agent_sdk__Runtime_server

let expect_ok label = function
  | Ok value -> value
  | Error err -> Alcotest.failf "%s: %s" label (Error.to_string err)
;;

let contains label ~needle text =
  Alcotest.(check bool) label true (Util.string_contains ~needle text)
;;

let source_path path =
  if Filename.is_relative path
  then (
    match Sys.getenv_opt "DUNE_SOURCEROOT" with
    | Some root -> Filename.concat root path
    | None -> path)
  else path
;;

let read_source path = In_channel.with_open_text (source_path path) In_channel.input_all

let with_temp_store f =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf
         "oas-runtime-server-cov-%d-%06x"
         (Unix.getpid ())
         (Random.int 0xFFFFFF))
  in
  let store = Runtime_store.create ~root () |> expect_ok "create store" in
  f root store
;;

let make_state env root =
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) ~clock:env#clock () in
  state.session_root <- Some root;
  state
;;

let start_request ?(session_id = "rt-cov") () : Runtime.start_request =
  { session_id = Some session_id
  ; goal = "cover runtime server"
  ; participants = [ "worker" ]
  ; provider = Some "mock"
  ; model = Some "mock-model"
  ; permission_mode = Some "ask"
  ; system_prompt = Some "session prompt"
  ; max_turns = Some 4
  ; workdir = Some "/tmp/oas"
  }
;;

let mk_session ?session_id () =
  Runtime_projection.initial_session (start_request ?session_id ())
;;

let save_session store session =
  Runtime_store.save_session store session |> expect_ok "save session"
;;

let read_events store session_id =
  Runtime_store.read_events store session_id () |> expect_ok "read events"
;;

let event_exists predicate events =
  List.exists (fun (event : Runtime.event) -> predicate event.kind) events
;;

let test_handle_initialize_status_events_report_prove_shutdown () =
  Eio_main.run
  @@ fun env ->
  with_temp_store
  @@ fun root store ->
  Eio.Switch.run
  @@ fun sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) ~clock:env#clock () in
  let init : Runtime.init_request =
    { session_root = Some ("  " ^ root ^ "  ")
    ; provider = Some "mock"
    ; model = Some "mock-model"
    ; permission_mode = Some "ask"
    ; include_partial_messages = false
    ; setting_sources = [ "test" ]
    ; resume_session = None
    ; cwd = Some "/tmp/oas"
    }
  in
  (match Runtime_server.handle_request ~sw state (Runtime.Initialize init) with
   | Ok (Runtime.Initialized response) ->
     Alcotest.(check string) "sdk" "agent_sdk" response.sdk_name;
     Alcotest.(check string) "root" root (Option.value ~default:"" state.session_root);
     Alcotest.(check bool)
       "capability"
       true
       (List.mem "apply_command" response.capabilities)
   | Ok _ -> Alcotest.fail "expected Initialized"
   | Error err -> Alcotest.fail (Error.to_string err));
  let session = mk_session () in
  save_session store session;
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Record_turn { actor = Some "user"; message = "status please" })
    with
    | Ok (Runtime.Command_applied session) -> session
    | Ok _ -> Alcotest.fail "expected Command_applied"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  (match
     Runtime_server.handle_request
       ~sw
       state
       (Runtime.Status { session_id = session.session_id })
   with
   | Ok (Runtime.Status_response loaded) ->
     Alcotest.(check string) "status id" session.session_id loaded.session_id
   | Ok _ -> Alcotest.fail "expected status"
   | Error err -> Alcotest.fail (Error.to_string err));
  (match
     Runtime_server.handle_request
       ~sw
       state
       (Runtime.Events { session_id = session.session_id; after_seq = Some 0 })
   with
   | Ok (Runtime.Events_response events) ->
     Alcotest.(check int) "events" 1 (List.length events)
   | Ok _ -> Alcotest.fail "expected events"
   | Error err -> Alcotest.fail (Error.to_string err));
  (match
     Runtime_server.handle_request
       ~sw
       state
       (Runtime.Report { session_id = session.session_id })
   with
   | Ok (Runtime.Report_response report) ->
     Alcotest.(check string) "report" session.session_id report.session_id
   | Ok _ -> Alcotest.fail "expected report"
   | Error err -> Alcotest.fail (Error.to_string err));
  (match
     Runtime_server.handle_request
       ~sw
       state
       (Runtime.Prove { session_id = session.session_id })
   with
   | Ok (Runtime.Prove_response proof) ->
     Alcotest.(check string) "proof" session.session_id proof.session_id
   | Ok _ -> Alcotest.fail "expected proof"
   | Error err -> Alcotest.fail (Error.to_string err));
  match Runtime_server.handle_request ~sw state Runtime.Shutdown with
  | Ok Runtime.Shutdown_ack -> ()
  | Ok _ -> Alcotest.fail "expected shutdown"
  | Error err -> Alcotest.fail (Error.to_string err)
;;

let test_apply_command_public_paths_and_errors () =
  Eio_main.run
  @@ fun env ->
  with_temp_store
  @@ fun root store ->
  Eio.Switch.run
  @@ fun sw ->
  let state = make_state env root in
  let session = mk_session () in
  save_session store session;
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Record_turn { actor = Some "user"; message = "hello" })
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check int) "record seq" 1 session.last_seq;
      session
    | Ok _ -> Alcotest.fail "expected record response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let input : Runtime.input_request =
    { request_id = "input-1"
    ; participant_name = Some "worker"
    ; question = "Continue?"
    ; schema = Some (`Assoc [ "type", `String "string" ])
    ; timeout_s = None
    ; created_at = 2.0
    }
  in
  let session =
    match
      Runtime_server.apply_command ~sw state store session (Runtime.Request_input input)
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check bool) "pending" true (Option.is_some session.pending_input);
      session
    | Ok _ -> Alcotest.fail "expected input response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  (match
     Runtime_server.apply_command
       ~sw
       state
       store
       session
       (Runtime.Request_input { input with request_id = "   " })
   with
   | Error (Error.Internal msg) -> contains "empty request id" ~needle:"non-empty" msg
   | Error err -> Alcotest.fail (Error.to_string err)
   | Ok _ -> Alcotest.fail "blank request id should fail");
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Update_session_settings
           { model = Some "new-model"; permission_mode = Some "never" })
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check (option string)) "model updated" (Some "new-model") session.model;
      session
    | Ok _ -> Alcotest.fail "expected settings response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Update_session_settings { model = None; permission_mode = None })
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check (option string)) "model preserved" (Some "new-model") session.model;
      Alcotest.(check (option string))
        "permission preserved"
        (Some "never")
        session.permission_mode;
      session
    | Ok _ -> Alcotest.fail "expected settings preservation response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Attach_artifact
           { name = "note"; kind = "text"; content = "artifact body" })
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check int) "artifact count" 1 (List.length session.artifacts);
      session
    | Ok _ -> Alcotest.fail "expected artifact response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let session =
    match
      Runtime_server.apply_command
        ~sw
        state
        store
        session
        (Runtime.Checkpoint { label = Some "before finalize" })
    with
    | Ok (Runtime.Command_applied session) ->
      Alcotest.(check int) "checkpoint seq" 6 session.last_seq;
      session
    | Ok _ -> Alcotest.fail "expected checkpoint response"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let events = read_events store session.session_id in
  Alcotest.(check bool)
    "turn event"
    true
    (event_exists
       (function
         | Runtime.Turn_recorded _ -> true
         | _ -> false)
       events);
  Alcotest.(check bool)
    "checkpoint event"
    true
    (event_exists
       (function
         | Runtime.Checkpoint_saved _ -> true
         | _ -> false)
       events);
  let no_pending = mk_session ~session_id:"rt-no-pending" () in
  save_session store no_pending;
  (match
     Runtime_server.apply_command
       ~sw
       state
       store
       no_pending
       (Runtime.Provide_input
          { request_id = "missing"; response = Runtime.Input_declined })
   with
   | Error (Error.Internal msg) -> contains "no pending" ~needle:"no pending" msg
   | Error err -> Alcotest.fail (Error.to_string err)
   | Ok _ -> Alcotest.fail "provide without pending should fail");
  let pending : Runtime.input_request =
    { request_id = "expected"
    ; participant_name = Some "worker"
    ; question = "Continue?"
    ; schema = None
    ; timeout_s = None
    ; created_at = 2.0
    }
  in
  let pending_session =
    { no_pending with
      session_id = "rt-pending"
    ; phase = Runtime.Input_required
    ; pending_input = Some pending
    ; last_seq = 1
    }
  in
  save_session store pending_session;
  match
    Runtime_server.apply_command
      ~sw
      state
      store
      pending_session
      (Runtime.Provide_input { request_id = "wrong"; response = Runtime.Input_timeout })
  with
  | Error (Error.Internal msg) ->
    contains "mismatch" ~needle:"pending request is expected" msg
  | Error err -> Alcotest.fail (Error.to_string err)
  | Ok _ -> Alcotest.fail "mismatched input id should fail"
;;

let test_finalize_session_active_and_terminal () =
  Eio_main.run
  @@ fun env ->
  with_temp_store
  @@ fun root store ->
  let state = make_state env root in
  let running = mk_session ~session_id:"rt-finalize" () in
  save_session store running;
  let finalized =
    match Runtime_server.finalize_session state store running (Some "done") with
    | Ok (Runtime.Finalized session) ->
      Alcotest.(check string) "outcome" "done" (Option.value ~default:"" session.outcome);
      session
    | Ok _ -> Alcotest.fail "expected finalized"
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let completed =
    { finalized with phase = Runtime.Completed; outcome = Some "already done" }
  in
  save_session store completed;
  match Runtime_server.finalize_session state store completed (Some "ignored") with
  | Ok (Runtime.Finalized session) ->
    Alcotest.(check (option string)) "preserved" (Some "already done") session.outcome
  | Ok _ -> Alcotest.fail "expected finalized terminal session"
  | Error err -> Alcotest.fail (Error.to_string err)
;;

let test_control_channel_uses_stdio_router_not_blocking_stdin () =
  let control_source = read_source "lib/runtime_server_control.ml" in
  let server_source = read_source "lib/runtime_server.ml" in
  Alcotest.(check bool)
    "control helper must not read stdin directly"
    false
    (Util.string_contains ~needle:"input_line stdin" control_source);
  contains
    "control helper awaits promise with timeout"
    ~needle:"Eio.Time.with_timeout_exn"
    control_source;
  contains
    "stdio router delivers control response"
    ~needle:"deliver_control_response"
    server_source;
  contains
    "request handling forked so stdin router keeps running"
    ~needle:"Eio.Fiber.fork"
    server_source
;;

let test_agent_config_uses_resolved_provider_model () =
  let server_source = read_source "lib/runtime_server.ml" in
  contains
    "agent config reads provider resolution model id"
    ~needle:"provider.Provider.model_id"
    server_source;
  contains
    "spawn path passes resolution to agent config"
    ~needle:"agent_config_of_session session resolution detail"
    server_source
;;

let () =
  Alcotest.run
    "Runtime_server_coverage"
    [ ( "request"
      , [ Alcotest.test_case
            "initialize status events report prove shutdown"
            `Quick
            test_handle_initialize_status_events_report_prove_shutdown
        ] )
    ; ( "commands"
      , [ Alcotest.test_case
            "public paths and errors"
            `Quick
            test_apply_command_public_paths_and_errors
        ] )
    ; ( "finalize"
      , [ Alcotest.test_case
            "active and terminal sessions"
            `Quick
            test_finalize_session_active_and_terminal
        ] )
    ; ( "control"
      , [ Alcotest.test_case
            "stdio router owns control responses"
            `Quick
            test_control_channel_uses_stdio_router_not_blocking_stdin
        ; Alcotest.test_case
            "agent config uses resolved provider model"
            `Quick
            test_agent_config_uses_resolved_provider_model
        ] )
    ]
;;
