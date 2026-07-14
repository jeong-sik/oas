open Agent_sdk

let capture_stdout_lines f =
  flush stdout;
  let saved_stdout = Unix.dup Unix.stdout in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd Unix.stdout;
  Unix.close write_fd;
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      Unix.dup2 saved_stdout Unix.stdout;
      Unix.close saved_stdout)
    f;
  let input = Unix.in_channel_of_descr read_fd in
  let rec read_lines acc =
    match input_line input with
    | line -> read_lines (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () -> read_lines [])
;;

let single_protocol_message = function
  | [ line ] ->
    (match Runtime.protocol_message_of_string line with
     | Ok message -> message
     | Error detail -> Alcotest.failf "invalid protocol observation: %s" detail)
  | lines ->
    Alcotest.failf "expected one protocol observation, got %d" (List.length lines)
;;

let participant_common ?raw_trace_run_id () : Runtime.participant_event_common =
  { participant_name = "worker-1"
  ; summary = None
  ; provider = None
  ; model = None
  ; raw_trace_run_id
  }
;;

let participant_completed ?raw_trace_run_id () : Runtime.participant_completed_event =
  { participant = participant_common ?raw_trace_run_id ()
  ; stop_reason = None
  ; completion_anomaly = None
  }
;;

let participant_failed ?raw_trace_run_id () : Runtime.participant_failed_event =
  { participant = participant_common ?raw_trace_run_id ()
  ; failure_cause = Runtime.Execution_error "failed"
  }
;;

let runtime_event kind : Runtime.event = { seq = 1; ts = 1.0; kind }

let test_event_bus_run_id_uses_participant_raw_trace_run_id () =
  let event =
    runtime_event
      (Runtime.Agent_completed (participant_completed ~raw_trace_run_id:"raw-run-1" ()))
  in
  Alcotest.(check (option string))
    "run_id"
    (Some "raw-run-1")
    (Runtime_server_types.event_bus_run_id_of_event event)
;;

let test_event_bus_run_id_ignores_blank_raw_trace_run_id () =
  let event =
    runtime_event (Runtime.Agent_failed (participant_failed ~raw_trace_run_id:"   " ()))
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
  let participant = participant_common () in
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
    ; Runtime.Agent_became_live { participant }, "runtime.agent_became_live"
    ; ( Runtime.Agent_output_delta
          { participant_name = "worker-1"; delta = "delta"; raw_trace_run_id = None }
      , "runtime.agent_output_delta" )
    ; ( Runtime.Agent_completed
          { participant; stop_reason = None; completion_anomaly = None }
      , "runtime.agent_completed" )
    ; ( Runtime.Agent_failed
          { participant; failure_cause = Runtime.Execution_error "failed" }
      , "runtime.agent_failed" )
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

let test_closed_switch_rejects_lane_without_running_callback () =
  Eio_main.run
  @@ fun env ->
  let closed_switch, set_closed_switch = Eio.Promise.create () in
  Eio.Switch.run (fun sw -> Eio.Promise.resolve set_closed_switch sw);
  let closed_switch = Eio.Promise.await closed_switch in
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let callback_ran = ref false in
  (match
     Runtime_server_types.fork_participant_lane
       ~sw:closed_switch
       state
       ~session_id:"closed-switch"
       ~participant_name:"never-started"
       (fun () -> callback_ran := true)
   with
   | Error (Error.Config (InvalidConfig { field = "runtime.lifecycle"; _ })) -> ()
   | Error err ->
     Alcotest.failf "unexpected closed-switch error: %s" (Error.to_string err)
   | Ok () -> Alcotest.fail "a closed switch must reject participant registration");
  Alcotest.(check bool) "callback did not run" false !callback_ran;
  Runtime_server_types.settle_session_lane state "closed-switch"
;;

let test_system_error_is_an_explicit_protocol_observation () =
  Eio_main.run
  @@ fun env ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let message = "participant failure persistence failed" in
  let protocol_message =
    capture_stdout_lines (fun () -> Runtime_server_types.emit_system_error state message)
    |> single_protocol_message
  in
  match protocol_message with
  | Runtime.System_message { level = "error"; message = actual } ->
    Alcotest.(check string) "message" message actual
  | _ -> Alcotest.fail "failure observation must use System_message"
;;

let test_ordinary_lane_escape_is_observed_without_failing_other_lanes () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let callback_started, set_callback_started = Eio.Promise.create () in
  let protocol_message =
    capture_stdout_lines (fun () ->
      (match
         Runtime_server_types.fork_participant_lane
           ~sw
           state
           ~session_id:"ordinary-failure-session"
           ~participant_name:"ordinary-failure-participant"
           (fun () ->
              Eio.Promise.resolve set_callback_started ();
              failwith "ordinary lane failure")
       with
       | Ok () -> ()
       | Error err -> Alcotest.fail (Error.to_string err));
      Eio.Promise.await callback_started;
      Runtime_server_types.settle_session_lane state "ordinary-failure-session")
    |> single_protocol_message
  in
  let survivor_ran, set_survivor_ran = Eio.Promise.create () in
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"surviving-session"
       ~participant_name:"survivor"
       (fun () -> Eio.Promise.resolve set_survivor_ran ())
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  Eio.Promise.await survivor_ran;
  Runtime_server_types.settle_session_lane state "surviving-session";
  match protocol_message with
  | Runtime.System_message { level = "error"; message } ->
    Alcotest.(check bool)
      "session observed"
      true
      (Util.string_contains ~needle:"ordinary-failure-session" message);
    Alcotest.(check bool)
      "participant observed"
      true
      (Util.string_contains ~needle:"ordinary-failure-participant" message);
    Alcotest.(check bool)
      "failure observed"
      true
      (Util.string_contains ~needle:"ordinary lane failure" message)
  | _ -> Alcotest.fail "ordinary lane failure must be externally observed"
;;

exception Registration_switch_cancelled

let test_switch_cancellation_during_registration_settles_lane () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun outer_sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let mutex_held, set_mutex_held = Eio.Promise.create () in
  let release_mutex, set_release_mutex = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:outer_sw (fun () ->
    Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
      Eio.Promise.resolve set_mutex_held ();
      Eio.Promise.await release_mutex));
  Eio.Promise.await mutex_held;
  let registration_started, set_registration_started = Eio.Promise.create () in
  let registration_result, set_registration_result = Eio.Promise.create () in
  let callback_ran = ref false in
  let switch_failed =
    match
      Eio.Switch.run (fun participant_sw ->
        Eio.Fiber.fork ~sw:outer_sw (fun () ->
          Eio.Promise.resolve set_registration_started ();
          let result =
            match
              Runtime_server_types.fork_participant_lane
                ~sw:participant_sw
                state
                ~session_id:"registration-race"
                ~participant_name:"never-ran"
                (fun () -> callback_ran := true)
            with
            | result -> `Returned result
            | exception Eio.Cancel.Cancelled _ -> `Cancelled
          in
          Eio.Promise.resolve set_registration_result result);
        Eio.Promise.await registration_started;
        Eio.Switch.fail participant_sw Registration_switch_cancelled;
        Eio.Promise.resolve set_release_mutex ())
    with
    | () -> false
    | exception Registration_switch_cancelled -> true
  in
  Alcotest.(check bool) "participant switch failed" true switch_failed;
  (match Eio.Promise.await registration_result with
   | `Cancelled -> ()
   | `Returned (Error err) ->
     Alcotest.failf
       "switch cancellation was converted to an SDK error: %s"
       (Error.to_string err)
   | `Returned (Ok ()) -> Alcotest.fail "cancelling switch accepted pending registration");
  Alcotest.(check bool) "callback did not run" false !callback_ran;
  match
    Eio.Time.with_timeout (Eio.Stdenv.clock env) 1.0 (fun () ->
      Runtime_server_types.settle_session_lane state "registration-race";
      Ok ())
  with
  | Ok () -> ()
  | Error `Timeout -> Alcotest.fail "registration race left an unresolved participant"
;;

let mutually_cancellable_lane started cancelled other_cancelled blocker () =
  Eio.Promise.resolve started ();
  match Eio.Promise.await blocker with
  | () -> ()
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    ignore (Eio.Promise.try_resolve cancelled ());
    Eio.Promise.await other_cancelled;
    raise exn
;;

let test_shutdown_cancels_every_lane_before_joining () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
  let a_started, set_a_started = Eio.Promise.create () in
  let b_started, set_b_started = Eio.Promise.create () in
  let a_cancelled, set_a_cancelled = Eio.Promise.create () in
  let b_cancelled, set_b_cancelled = Eio.Promise.create () in
  let a_blocker, _set_a_blocker = Eio.Promise.create () in
  let b_blocker, _set_b_blocker = Eio.Promise.create () in
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"mutual-a"
       ~participant_name:"agent-a"
       (mutually_cancellable_lane set_a_started set_a_cancelled b_cancelled a_blocker)
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  (match
     Runtime_server_types.fork_participant_lane
       ~sw
       state
       ~session_id:"mutual-b"
       ~participant_name:"agent-b"
       (mutually_cancellable_lane set_b_started set_b_cancelled a_cancelled b_blocker)
   with
   | Ok () -> ()
   | Error err -> Alcotest.fail (Error.to_string err));
  Eio.Promise.await a_started;
  Eio.Promise.await b_started;
  (match
     Eio.Time.with_timeout (Eio.Stdenv.clock env) 1.0 (fun () ->
       Runtime_server_types.settle_all_session_lanes state;
       Ok ())
   with
   | Ok () -> ()
   | Error `Timeout ->
     Alcotest.fail "shutdown joined one lane before cancelling the remaining lane");
  Alcotest.(check bool)
    "first lane observed cancellation"
    true
    (Option.is_some (Eio.Promise.peek a_cancelled));
  Alcotest.(check bool)
    "second lane observed cancellation"
    true
    (Option.is_some (Eio.Promise.peek b_cancelled))
;;

let test_reserved_lane_exception_fails_owning_switch () =
  Eio_main.run
  @@ fun env ->
  let raised =
    match
      Eio.Switch.run (fun sw ->
        let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
        (match
           Runtime_server_types.fork_participant_lane
             ~sw
             state
             ~session_id:"reserved"
             ~participant_name:"breaker"
             (fun () -> raise Sys.Break)
         with
         | Ok () -> ()
         | Error err -> Alcotest.fail (Error.to_string err));
        Eio.Fiber.await_cancel ())
    with
    | () -> false
    | exception Sys.Break -> true
  in
  Alcotest.(check bool) "Sys.Break propagated through the switch" true raised
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
        ; Alcotest.test_case
            "closed switch rejects lane"
            `Quick
            test_closed_switch_rejects_lane_without_running_callback
        ; Alcotest.test_case
            "system error is a protocol observation"
            `Quick
            test_system_error_is_an_explicit_protocol_observation
        ; Alcotest.test_case
            "ordinary lane escape is externally observed"
            `Quick
            test_ordinary_lane_escape_is_observed_without_failing_other_lanes
        ; Alcotest.test_case
            "switch cancellation during registration settles lane"
            `Quick
            test_switch_cancellation_during_registration_settles_lane
        ; Alcotest.test_case
            "shutdown cancels all before joining"
            `Quick
            test_shutdown_cancels_every_lane_before_joining
        ; Alcotest.test_case
            "reserved exception fails owning switch"
            `Quick
            test_reserved_lane_exception_fails_owning_switch
        ] )
    ]
;;
