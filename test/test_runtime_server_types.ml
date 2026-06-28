open Agent_sdk

let with_state f =
  Eio_main.run
  @@ fun env ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) ~clock:env#clock () in
  f state
;;

let test_next_control_id_sequential () =
  with_state
  @@ fun state ->
  let ids = List.init 3 (fun _ -> Runtime_server_types.next_control_id state) in
  Alcotest.(check (list string))
    "sequential ids"
    [ "ctrl-000001"; "ctrl-000002"; "ctrl-000003" ]
    ids
;;

let test_next_control_id_unique_across_domains () =
  with_state
  @@ fun state ->
  let workers = 4 in
  let per_worker = 200 in
  let domains =
    List.init workers (fun _ ->
      Domain.spawn (fun () ->
        Array.to_list
          (Array.init per_worker (fun _ -> Runtime_server_types.next_control_id state))))
  in
  let ids = List.concat (List.map Domain.join domains) in
  let module S = Set.Make (String) in
  let uniq = List.fold_left (fun acc id -> S.add id acc) S.empty ids in
  Alcotest.(check int) "all ids returned" (workers * per_worker) (List.length ids);
  Alcotest.(check int) "all ids unique" (workers * per_worker) (S.cardinal uniq)
;;

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
    ; ( Runtime.Session_settings_updated { model = Some "m"; permission_mode = None }
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
          ; provider = Some "mock"
          ; model = Some "mock-model"
          ; permission_mode = Some "ask"
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

let () =
  Alcotest.run
    "Runtime_server_types"
    [ ( "control ids"
      , [ Alcotest.test_case "sequential" `Quick test_next_control_id_sequential
        ; Alcotest.test_case
            "unique across domains"
            `Quick
            test_next_control_id_unique_across_domains
        ] )
    ; ( "event bus run correlation"
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
    ]
;;
