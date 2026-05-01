open Agent_sdk

let with_state f =
  Eio_main.run
  @@ fun env ->
  let state = Runtime_server_types.create ~net:(Eio.Stdenv.net env) () in
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

let test_event_bus_run_id_omits_session_events () =
  let event =
    runtime_event (Runtime.Session_started { goal = "test"; participants = [] })
  in
  Alcotest.(check (option string))
    "run_id"
    None
    (Runtime_server_types.event_bus_run_id_of_event event)
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
            "omits session events"
            `Quick
            test_event_bus_run_id_omits_session_events
        ] )
    ]
;;
