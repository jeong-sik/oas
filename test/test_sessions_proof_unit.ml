open Agent_sdk

(* --- Helpers to construct test data --- *)

let make_participant ?(name = "agent-a") ?(role = None)
    ?(aliases = []) ?(worker_id = None) ?(runtime_actor = None)
    ?(requested_provider = None) ?(requested_model = None)
    ?(requested_policy = None) ?(provider = None) ?(model = None)
    ?(resolved_provider = None) ?(resolved_model = None)
    ?(state = Runtime.Live) ?(summary = None)
    ?(accepted_at = None) ?(ready_at = None)
    ?(first_progress_at = None) ?(started_at = None)
    ?(finished_at = None) ?(last_progress_at = None)
    ?(last_error = None) () : Runtime.participant =
  {
    name;
    role;
    aliases;
    worker_id;
    runtime_actor;
    requested_provider;
    requested_model;
    requested_policy;
    provider;
    model;
    resolved_provider;
    resolved_model;
    state;
    summary;
    accepted_at;
    ready_at;
    first_progress_at;
    started_at;
    finished_at;
    last_progress_at;
    last_error;
  }

let make_session ?(session_id = "sess-001") ?(goal = "test")
    ?(provider = None) ?(model = None) ?(participants = []) ()
    : Runtime.session =
  {
    session_id;
    goal;
    title = None;
    tag = None;
    permission_mode = None;
    phase = Runtime.Completed;
    created_at = 1000.0;
    updated_at = 1001.0;
    provider;
    model;
    system_prompt = None;
    max_turns = 10;
    workdir = None;
    planned_participants = [];
    participants;
    artifacts = [];
    votes = [];
    turn_count = 0;
    last_seq = 0;
    outcome = None;
  }

let make_worker_run ?(worker_run_id = "run-1") ?(worker_id = Some "w-1")
    ?(agent_name = "agent-a") ?(runtime_actor = Some "actor-a")
    ?(role = None) ?(aliases = [])
    ?(primary_alias = None) ?(provider = Some "anthropic")
    ?(model = Some "claude") ?(requested_provider = None)
    ?(requested_model = None) ?(requested_policy = None)
    ?(resolved_provider = Some "anthropic")
    ?(resolved_model = Some "claude") ?(status = Sessions.Completed)
    ?(trace_capability = Sessions.Raw) ?(validated = false)
    ?(tool_names = []) ?(final_text = None) ?(stop_reason = None)
    ?(error = None) ?(failure_reason = None) ?(accepted_at = None)
    ?(ready_at = None) ?(first_progress_at = None)
    ?(started_at = None) ?(finished_at = None)
    ?(last_progress_at = None) ?(policy_snapshot = None)
    ?(paired_tool_result_count = 0) ?(has_file_write = false)
    ?(verification_pass_after_file_write = false) () : Sessions.worker_run =
  {
    worker_run_id;
    worker_id;
    agent_name;
    runtime_actor;
    role;
    aliases;
    primary_alias;
    provider;
    model;
    requested_provider;
    requested_model;
    requested_policy;
    resolved_provider;
    resolved_model;
    status;
    trace_capability;
    validated;
    tool_names;
    final_text;
    stop_reason;
    error;
    failure_reason;
    accepted_at;
    ready_at;
    first_progress_at;
    started_at;
    finished_at;
    last_progress_at;
    policy_snapshot;
    paired_tool_result_count;
    has_file_write;
    verification_pass_after_file_write;
  }

(* --- participant_by_name tests --- *)

let test_participant_by_name_found () =
  let p = make_participant ~name:"alice" () in
  let session = make_session ~participants:[ p ] () in
  let result = Sessions.participant_by_name session "alice" in
  Alcotest.(check bool)
    "found" true
    (Option.is_some result)

let test_participant_by_name_not_found () =
  let p = make_participant ~name:"alice" () in
  let session = make_session ~participants:[ p ] () in
  let result = Sessions.participant_by_name session "bob" in
  Alcotest.(check bool)
    "not found" true
    (Option.is_none result)

let test_participant_by_name_empty () =
  let session = make_session ~participants:[] () in
  let result = Sessions.participant_by_name session "anyone" in
  Alcotest.(check bool)
    "empty participants" true
    (Option.is_none result)

(* --- resolved_provider_of_participant tests --- *)

let test_resolved_provider_from_resolved () =
  let p =
    make_participant ~resolved_provider:(Some "gcp") ~provider:(Some "aws") ()
  in
  let session = make_session ~provider:(Some "default-p") () in
  let result = Sessions.resolved_provider_of_participant session (Some p) in
  Alcotest.(check (option string))
    "resolved_provider wins" (Some "gcp") result

let test_resolved_provider_fallback_to_provider () =
  let p =
    make_participant ~resolved_provider:None ~provider:(Some "aws") ()
  in
  let session = make_session ~provider:(Some "default-p") () in
  let result = Sessions.resolved_provider_of_participant session (Some p) in
  Alcotest.(check (option string))
    "falls back to provider" (Some "aws") result

let test_resolved_provider_fallback_to_session () =
  let session = make_session ~provider:(Some "session-p") () in
  let result = Sessions.resolved_provider_of_participant session None in
  Alcotest.(check (option string))
    "falls back to session" (Some "session-p") result

(* --- resolved_model_of_participant tests --- *)

let test_resolved_model_from_resolved () =
  let p =
    make_participant ~resolved_model:(Some "gpt-4") ~model:(Some "gpt-3") ()
  in
  let session = make_session ~model:(Some "default-m") () in
  let result = Sessions.resolved_model_of_participant session (Some p) in
  Alcotest.(check (option string))
    "resolved_model wins" (Some "gpt-4") result

let test_resolved_model_fallback_to_model () =
  let p =
    make_participant ~resolved_model:None ~model:(Some "gpt-3") ()
  in
  let session = make_session ~model:(Some "default-m") () in
  let result = Sessions.resolved_model_of_participant session (Some p) in
  Alcotest.(check (option string))
    "falls back to model" (Some "gpt-3") result

let test_resolved_model_fallback_to_session () =
  let session = make_session ~model:(Some "session-m") () in
  let result = Sessions.resolved_model_of_participant session None in
  Alcotest.(check (option string))
    "falls back to session" (Some "session-m") result

(* --- worker_status_of_participant tests --- *)

let test_worker_status_failed () =
  let p = make_participant ~state:Runtime.Failed_participant () in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Failed_participant -> Failed" true (result = Sessions.Failed)

let test_worker_status_done () =
  let p = make_participant ~state:Runtime.Done () in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Done -> Completed" true (result = Sessions.Completed)

let test_worker_status_live_with_progress () =
  let p =
    make_participant ~state:Runtime.Live ~started_at:(Some 100.0)
      ~last_progress_at:(Some 150.0) ()
  in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Live with progress -> Running" true (result = Sessions.Running)

let test_worker_status_live_no_progress () =
  let p =
    make_participant ~state:Runtime.Live ~started_at:None
      ~last_progress_at:None ()
  in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Live no progress -> Ready" true (result = Sessions.Ready)

let test_worker_status_idle_same_timestamps () =
  let p =
    make_participant ~state:Runtime.Idle ~started_at:(Some 100.0)
      ~last_progress_at:(Some 100.0) ()
  in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Idle same timestamps -> Ready" true (result = Sessions.Ready)

let test_worker_status_starting () =
  let p = make_participant ~state:Runtime.Starting () in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Starting -> Accepted" true (result = Sessions.Accepted)

let test_worker_status_planned () =
  let p = make_participant ~state:Runtime.Planned () in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Planned -> Planned" true (result = Sessions.Planned)

let test_worker_status_detached () =
  let p = make_participant ~state:Runtime.Detached () in
  let result = Sessions.worker_status_of_participant (Some p) in
  Alcotest.(check bool)
    "Detached -> Planned" true (result = Sessions.Planned)

let test_worker_status_none () =
  let result = Sessions.worker_status_of_participant None in
  Alcotest.(check bool)
    "None -> Completed" true (result = Sessions.Completed)

(* --- worker_order_ts tests --- *)

let test_worker_order_ts_finished () =
  let w = make_worker_run ~finished_at:(Some 300.0)
      ~last_progress_at:(Some 200.0) ~started_at:(Some 100.0) () in
  let result = Sessions.worker_order_ts w in
  Alcotest.(check (option (float 0.01)))
    "finished_at wins" (Some 300.0) result

let test_worker_order_ts_fallback_progress () =
  let w = make_worker_run ~finished_at:None
      ~last_progress_at:(Some 200.0) ~started_at:(Some 100.0) () in
  let result = Sessions.worker_order_ts w in
  Alcotest.(check (option (float 0.01)))
    "last_progress_at fallback" (Some 200.0) result

let test_worker_order_ts_fallback_started () =
  let w = make_worker_run ~finished_at:None
      ~last_progress_at:None ~started_at:(Some 100.0) () in
  let result = Sessions.worker_order_ts w in
  Alcotest.(check (option (float 0.01)))
    "started_at fallback" (Some 100.0) result

let test_worker_order_ts_all_none () =
  let w = make_worker_run ~finished_at:None
      ~last_progress_at:None ~started_at:None () in
  let result = Sessions.worker_order_ts w in
  Alcotest.(check (option (float 0.01)))
    "all None" None result

(* --- sort_worker_runs tests --- *)

let test_sort_worker_runs_by_finished () =
  let w1 = make_worker_run ~worker_run_id:"r1" ~finished_at:(Some 300.0) () in
  let w2 = make_worker_run ~worker_run_id:"r2" ~finished_at:(Some 100.0) () in
  let w3 = make_worker_run ~worker_run_id:"r3" ~finished_at:(Some 200.0) () in
  let sorted = Sessions.sort_worker_runs [ w1; w2; w3 ] in
  let ids =
    List.map (fun (w : Sessions.worker_run) -> w.worker_run_id) sorted
  in
  Alcotest.(check (list string))
    "sorted by finished_at asc" [ "r2"; "r3"; "r1" ] ids

let test_sort_worker_runs_none_last () =
  let w1 =
    make_worker_run ~worker_run_id:"r1" ~finished_at:(Some 100.0) ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"r2" ~finished_at:None
      ~last_progress_at:None ~started_at:None ()
  in
  let sorted = Sessions.sort_worker_runs [ w2; w1 ] in
  let ids =
    List.map (fun (w : Sessions.worker_run) -> w.worker_run_id) sorted
  in
  Alcotest.(check (list string))
    "None timestamps go after Some" [ "r1"; "r2" ] ids

let test_sort_worker_runs_both_none_by_id () =
  let w1 =
    make_worker_run ~worker_run_id:"beta" ~finished_at:None
      ~last_progress_at:None ~started_at:None ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"alpha" ~finished_at:None
      ~last_progress_at:None ~started_at:None ()
  in
  let sorted = Sessions.sort_worker_runs [ w1; w2 ] in
  let ids =
    List.map (fun (w : Sessions.worker_run) -> w.worker_run_id) sorted
  in
  Alcotest.(check (list string))
    "fallback to id sort" [ "alpha"; "beta" ] ids

let test_sort_worker_runs_mixed_timestamps () =
  let w1 =
    make_worker_run ~worker_run_id:"r1"
      ~finished_at:None ~last_progress_at:None ~started_at:(Some 50.0) ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"r2"
      ~finished_at:None ~last_progress_at:(Some 200.0) ~started_at:None ()
  in
  let w3 =
    make_worker_run ~worker_run_id:"r3"
      ~finished_at:(Some 100.0) ~last_progress_at:None ~started_at:None ()
  in
  let sorted = Sessions.sort_worker_runs [ w1; w2; w3 ] in
  let ids =
    List.map (fun (w : Sessions.worker_run) -> w.worker_run_id) sorted
  in
  Alcotest.(check (list string))
    "mixed ts sources" [ "r1"; "r3"; "r2" ] ids

(* --- latest_worker_by tests --- *)

let test_latest_worker_by_all () =
  let w1 =
    make_worker_run ~worker_run_id:"r1" ~finished_at:(Some 100.0) ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"r2" ~finished_at:(Some 200.0) ()
  in
  let result = Sessions.latest_worker_by (fun _ -> true) [ w1; w2 ] in
  Alcotest.(check string)
    "latest by ts" "r2"
    (Option.get result).worker_run_id

let test_latest_worker_by_with_predicate () =
  let w1 =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Completed
      ~finished_at:(Some 100.0) ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"r2" ~status:Sessions.Running
      ~finished_at:(Some 200.0) ()
  in
  let w3 =
    make_worker_run ~worker_run_id:"r3" ~status:Sessions.Completed
      ~finished_at:(Some 300.0) ()
  in
  let result =
    Sessions.latest_worker_by
      (fun (w : Sessions.worker_run) -> w.status = Sessions.Completed)
      [ w1; w2; w3 ]
  in
  Alcotest.(check string)
    "latest completed" "r3"
    (Option.get result).worker_run_id

let test_latest_worker_by_empty () =
  let result = Sessions.latest_worker_by (fun _ -> true) [] in
  Alcotest.(check bool) "None for empty" true (Option.is_none result)

let test_latest_worker_by_no_match () =
  let w1 =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Completed
      ~finished_at:(Some 100.0) ()
  in
  let result =
    Sessions.latest_worker_by
      (fun (w : Sessions.worker_run) -> w.status = Sessions.Failed)
      [ w1 ]
  in
  Alcotest.(check bool) "None when no match" true (Option.is_none result)

(* --- Alcotest runner --- *)

let () =
  Alcotest.run "Sessions_proof unit"
    [
      ( "Sessions.participant_by_name",
        [
          Alcotest.test_case "found" `Quick test_participant_by_name_found;
          Alcotest.test_case "not_found" `Quick
            test_participant_by_name_not_found;
          Alcotest.test_case "empty" `Quick test_participant_by_name_empty;
        ] );
      ( "resolved_provider_of_participant",
        [
          Alcotest.test_case "resolved wins" `Quick
            test_resolved_provider_from_resolved;
          Alcotest.test_case "fallback to provider" `Quick
            test_resolved_provider_fallback_to_provider;
          Alcotest.test_case "fallback to session" `Quick
            test_resolved_provider_fallback_to_session;
        ] );
      ( "resolved_model_of_participant",
        [
          Alcotest.test_case "resolved wins" `Quick
            test_resolved_model_from_resolved;
          Alcotest.test_case "fallback to model" `Quick
            test_resolved_model_fallback_to_model;
          Alcotest.test_case "fallback to session" `Quick
            test_resolved_model_fallback_to_session;
        ] );
      ( "worker_status_of_participant",
        [
          Alcotest.test_case "Failed_participant" `Quick
            test_worker_status_failed;
          Alcotest.test_case "Done" `Quick test_worker_status_done;
          Alcotest.test_case "Live with progress" `Quick
            test_worker_status_live_with_progress;
          Alcotest.test_case "Live no progress" `Quick
            test_worker_status_live_no_progress;
          Alcotest.test_case "Idle same timestamps" `Quick
            test_worker_status_idle_same_timestamps;
          Alcotest.test_case "Starting" `Quick test_worker_status_starting;
          Alcotest.test_case "Planned" `Quick test_worker_status_planned;
          Alcotest.test_case "Detached" `Quick test_worker_status_detached;
          Alcotest.test_case "None" `Quick test_worker_status_none;
        ] );
      ( "worker_order_ts",
        [
          Alcotest.test_case "finished" `Quick test_worker_order_ts_finished;
          Alcotest.test_case "fallback progress" `Quick
            test_worker_order_ts_fallback_progress;
          Alcotest.test_case "fallback started" `Quick
            test_worker_order_ts_fallback_started;
          Alcotest.test_case "all none" `Quick test_worker_order_ts_all_none;
        ] );
      ( "sort_worker_runs",
        [
          Alcotest.test_case "by_finished" `Quick
            test_sort_worker_runs_by_finished;
          Alcotest.test_case "none_last" `Quick
            test_sort_worker_runs_none_last;
          Alcotest.test_case "both_none_by_id" `Quick
            test_sort_worker_runs_both_none_by_id;
          Alcotest.test_case "mixed_timestamps" `Quick
            test_sort_worker_runs_mixed_timestamps;
        ] );
      ( "latest_worker_by",
        [
          Alcotest.test_case "all" `Quick test_latest_worker_by_all;
          Alcotest.test_case "with_predicate" `Quick
            test_latest_worker_by_with_predicate;
          Alcotest.test_case "empty" `Quick test_latest_worker_by_empty;
          Alcotest.test_case "no_match" `Quick test_latest_worker_by_no_match;
        ] );
    ]
