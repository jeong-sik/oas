open Agent_sdk

let participant_event ?raw_trace_run_id () : Runtime.participant_event =
  { participant_name = "worker-1"
  ; summary = None
  ; provider = None
  ; model = None
  ; error = None
  ; raw_trace_run_id
  ; stop_reason = None
  ; completion_anomaly = None
  ; failure_cause = None
  }
;;

let runtime_event kind : Runtime.event = { seq = 1; ts = 1.0; kind }

let test_event_bus_run_id_uses_participant_raw_trace_run_id () =
  let event =
    runtime_event
      (Runtime.Agent_completed (participant_event ~raw_trace_run_id:"raw-run-1" ()))
  in
  Alcotest.(check (option string))
    "run_id"
    (Some "raw-run-1")
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let test_event_bus_run_id_ignores_blank_raw_trace_run_id () =
  let event =
    runtime_event (Runtime.Agent_failed (participant_event ~raw_trace_run_id:"   " ()))
  in
  Alcotest.(check (option string))
    "run_id"
    None
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let test_event_bus_run_id_uses_output_delta_raw_trace_run_id () =
  let event =
    runtime_event
      (Runtime.Agent_output_delta
         { participant_name = "worker-1"
         ; delta = "delta"
         ; raw_trace_run_id = Some "raw-run-delta"
         })
  in
  Alcotest.(check (option string))
    "run_id"
    (Some "raw-run-delta")
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let test_event_bus_run_id_ignores_blank_output_delta_raw_trace_run_id () =
  let event =
    runtime_event
      (Runtime.Agent_output_delta
         { participant_name = "worker-1"; delta = "delta"; raw_trace_run_id = Some "  " })
  in
  Alcotest.(check (option string))
    "run_id"
    None
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let test_event_bus_run_id_omits_session_events () =
  let event =
    runtime_event (Runtime.Session_started { goal = "test"; participants = [] })
  in
  Alcotest.(check (option string))
    "run_id"
    None
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let input_request : Runtime.input_request =
  { request_id = "input-1"
  ; participant_name = Some "worker-1"
  ; question = "Continue?"
  ; schema = None
  ; timeout_s = None
  ; created_at = 1.0
  }
;;

let test_custom_name_of_kind_all_variants () =
  let participant = participant_event () in
  let cases =
    [ ( Runtime.Session_started { goal = "g"; participants = [ "p" ] }
      , "runtime.session_started" )
    ; ( Runtime.Session_settings_updated { model = Some "m" }
      , "runtime.session_settings_updated" )
    ; ( Runtime.Turn_recorded { actor = Some "user"; message = "hello" }
      , "runtime.turn_recorded" )
    ; Runtime.Input_required input_request, "runtime.input_required"
    ; ( Runtime.Input_provided
          { request_id = "input-1"
          ; participant_name = Some "worker-1"
          ; response = Runtime.Input_answer (`String "yes")
          }
      , "runtime.input_provided" )
    ; ( Runtime.Pending_input_updated
          { input_id = Some "pending-1"
          ; participant_name = Some "worker-1"
          ; source = Some "dashboard"
          ; boundary = Runtime_continuation.Provider_streaming_reasoning
          ; policy = Runtime_continuation.Queue_until_safe_boundary
          ; status = "queued"
          ; message = Some "queued while reasoning"
          ; created_at = 1.0
          }
      , "runtime.pending_input_updated" )
    ; ( Runtime.Agent_spawn_requested
          { participant_name = "worker-1"
          ; role = Some "reviewer"
          ; prompt = "review"
          ; provider = Some "test-provider"
          ; model = Some "test-model"
          }
      , "runtime.agent_spawn_requested" )
    ; Runtime.Agent_became_live participant, "runtime.agent_became_live"
    ; ( Runtime.Agent_output_delta
          { participant_name = "worker-1"; delta = "delta"; raw_trace_run_id = None }
      , "runtime.agent_output_delta" )
    ; Runtime.Agent_completed participant, "runtime.agent_completed"
    ; Runtime.Agent_failed participant, "runtime.agent_failed"
    ; ( Runtime.Artifact_attached
          { artifact_id = "artifact-1"
          ; name = "report"
          ; kind = "text"
          ; mime_type = "text/plain"
          ; path = "/tmp/report.txt"
          ; size_bytes = 12
          }
      , "runtime.artifact_attached" )
    ; ( Runtime.Checkpoint_saved { label = Some "cp"; path = "/tmp/cp" }
      , "runtime.checkpoint_saved" )
    ; Runtime.Finalize_requested { reason = Some "done" }, "runtime.finalize_requested"
    ; Runtime.Session_completed { outcome = Some "ok" }, "runtime.session_completed"
    ; Runtime.Session_failed { outcome = Some "failed" }, "runtime.session_failed"
    ]
  in
  List.iter
    (fun (kind, expected) ->
       Alcotest.(check string)
         expected
         expected
         (Runtime_server_types.custom_name_of_kind kind))
    cases
;;

let test_session_root_request_path_trims_blank_values () =
  Alcotest.(check (option string))
    "blank"
    None
    (Runtime_server_types.session_root_request_path (Some "   "));
  Alcotest.(check (option string))
    "trimmed"
    (Some "/tmp/oas")
    (Runtime_server_types.session_root_request_path (Some "  /tmp/oas  "));
  Alcotest.(check (option string))
    "none"
    None
    (Runtime_server_types.session_root_request_path None)
;;

let cancellable_lane started cancelled blocker () =
  Eio.Promise.resolve started ();
  match Eio.Promise.await blocker with
  | () -> ()
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    ignore (Eio.Promise.try_resolve cancelled ());
    raise exn
;;

let test_settle_session_lane_is_session_scoped () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let a_started, a_started_resolver = Eio.Promise.create () in
  let a_cancelled, a_cancelled_resolver = Eio.Promise.create () in
  let a_blocker, _a_blocker_resolver = Eio.Promise.create () in
  let b_started, b_started_resolver = Eio.Promise.create () in
  let b_cancelled, b_cancelled_resolver = Eio.Promise.create () in
  let b_blocker, b_blocker_resolver = Eio.Promise.create () in
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"session-a"
       ~participant_name:"agent-a"
       (cancellable_lane a_started_resolver a_cancelled_resolver a_blocker)
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"session-b"
       ~participant_name:"agent-b"
       (cancellable_lane b_started_resolver b_cancelled_resolver b_blocker)
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  Eio.Promise.await a_started;
  Eio.Promise.await b_started;
  Runtime_server_types.settle_session_lane state "session-a";
  Alcotest.(check bool)
    "session-a cancelled"
    true
    (Option.is_some (Eio.Promise.peek a_cancelled));
  Alcotest.(check bool)
    "session-b remains live"
    true
    (Option.is_none (Eio.Promise.peek b_cancelled));
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"session-a"
       ~participant_name:"late-agent-a"
       (fun () -> Alcotest.fail "settled session accepted a late participant")
   with
   | Error (Error.Config (InvalidConfig { field = "runtime.session_lane"; _ })) -> ()
   | Error err ->
     Alcotest.failf "unexpected settled-session error: %s" (Error.to_string err)
   | Ok () -> Alcotest.fail "settled session must reject late participants");
  Eio.Promise.resolve b_blocker_resolver ();
  Runtime_server_types.settle_session_lane state "session-b"
;;

let test_shutdown_settles_all_lanes_before_return () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let started, started_resolver = Eio.Promise.create () in
  let cancelled, cancelled_resolver = Eio.Promise.create () in
  let blocker, _blocker_resolver = Eio.Promise.create () in
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"session-shutdown"
       ~participant_name:"agent-shutdown"
       (cancellable_lane started_resolver cancelled_resolver blocker)
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  Eio.Promise.await started;
  Runtime_server_types.settle_all_session_lanes state;
  Alcotest.(check bool)
    "lane cancellation observed before settle returned"
    true
    (Option.is_some (Eio.Promise.peek cancelled));
  match
    Runtime_server_types.fork_participant_lane
      ~sw
      state
      ~session_id:"late-session"
      ~participant_name:"late-agent"
      (fun () -> Alcotest.fail "shutdown accepted a late lane")
  with
  | Error (Error.Config (InvalidConfig { field = "runtime.lifecycle"; _ })) -> ()
  | Error err -> Alcotest.failf "unexpected late-lane error: %s" (Error.to_string err)
  | Ok () -> Alcotest.fail "shutdown must reject late participant lanes"
;;

let () =
  Alcotest.run
    "Runtime_server_types"
    [ ( "event bus run correlation"
      , [ Alcotest.test_case
            "uses participant raw trace run id"
            `Quick
            test_event_bus_run_id_uses_participant_raw_trace_run_id
        ; Alcotest.test_case
            "ignores blank raw trace run id"
            `Quick
            test_event_bus_run_id_ignores_blank_raw_trace_run_id
        ; Alcotest.test_case
            "uses output delta raw trace run id"
            `Quick
            test_event_bus_run_id_uses_output_delta_raw_trace_run_id
        ; Alcotest.test_case
            "ignores blank output delta raw trace run id"
            `Quick
            test_event_bus_run_id_ignores_blank_output_delta_raw_trace_run_id
        ; Alcotest.test_case
            "omits session events"
            `Quick
            test_event_bus_run_id_omits_session_events
        ] )
    ; ( "event names"
      , [ Alcotest.test_case
            "custom names cover all event kinds"
            `Quick
            test_custom_name_of_kind_all_variants
        ] )
    ; ( "session root"
      , [ Alcotest.test_case
            "trims request path"
            `Quick
            test_session_root_request_path_trims_blank_values
        ] )
    ; ( "session lanes"
      , [ Alcotest.test_case
            "settle is session scoped"
            `Quick
            test_settle_session_lane_is_session_scoped
        ; Alcotest.test_case
            "shutdown settles all lanes"
            `Quick
            test_shutdown_settles_all_lanes_before_return
        ] )
    ]
;;
