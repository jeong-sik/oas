open Agent_sdk

(* --- Helpers to construct test data --- *)

let make_session ?(session_id = "sess-001") ?(goal = "test")
    ?(provider = None) ?(model = None) ?(participants = []) () : Runtime.session =
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

let make_report_rt ?(session_id = "sess-001") () : Runtime.report =
  {
    session_id;
    summary = [];
    markdown = "";
    generated_at = 1000.0;
  }

let make_proof ?(session_id = "sess-001") () : Runtime.proof =
  {
    session_id;
    ok = true;
    checks = [];
    evidence = [];
    generated_at = 1000.0;
  }

let make_telemetry ?(session_id = "sess-001") () : Sessions.telemetry =
  {
    session_id;
    generated_at = 1000.0;
    step_count = 0;
    event_counts = [];
    steps = [];
  }

let make_structured_telemetry ?(session_id = "sess-001") ()
    : Sessions.structured_telemetry =
  {
    session_id;
    generated_at = 1000.0;
    step_count = 0;
    event_counts = [];
    steps = [];
  }

let make_evidence ?(session_id = "sess-001") () : Sessions.evidence =
  {
    session_id;
    generated_at = 1000.0;
    files = [];
    missing_files = [];
  }

let make_run_ref ?(worker_run_id = "run-1") ?(agent_name = "agent-a") ()
    : Raw_trace.run_ref =
  {
    worker_run_id;
    path = "/tmp/fake";
    start_seq = 0;
    end_seq = 10;
    agent_name;
    session_id = Some "sess-001";
  }

let make_worker_run ?(worker_run_id = "run-1") ?(worker_id = Some "w-1")
    ?(agent_name = "agent-a") ?(runtime_actor = Some "actor-a")
    ?(role = None) ?(aliases = [ "alias-a" ])
    ?(primary_alias = Some "alias-a") ?(provider = Some "anthropic")
    ?(model = Some "claude") ?(requested_provider = None)
    ?(requested_model = None) ?(requested_policy = None)
    ?(resolved_provider = Some "anthropic")
    ?(resolved_model = Some "claude") ?(status = Sessions.Completed)
    ?(trace_capability = Sessions.Raw) ?(validated = true)
    ?(tool_names = []) ?(final_text = None) ?(stop_reason = None)
    ?(error = None) ?(failure_reason = None) ?(accepted_at = None)
    ?(ready_at = None) ?(first_progress_at = None)
    ?(started_at = Some 100.0) ?(finished_at = Some 200.0)
    ?(last_progress_at = Some 150.0) ?(policy_snapshot = None)
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

let make_raw_trace_summary ?(worker_run_id = "run-1")
    ?(agent_name = "agent-a") () : Sessions.raw_trace_summary =
  {
    run_ref = make_run_ref ~worker_run_id ~agent_name ();
    record_count = 5;
    assistant_block_count = 1;
    tool_execution_started_count = 0;
    tool_execution_finished_count = 0;
    hook_invoked_count = 0;
    hook_names = [];
    tool_names = [];
    model = None;
    tool_choice = None;
    enable_thinking = None;
    thinking_budget = None;
    thinking_block_count = 0;
    text_block_count = 0;
    tool_use_block_count = 0;
    tool_result_block_count = 0;
    first_assistant_block_kind = None;
    selection_outcome = "empty";
    saw_tool_use = false;
    saw_thinking = false;
    final_text = None;
    stop_reason = None;
    error = None;
    started_at = Some 100.0;
    finished_at = Some 200.0;
  }

let make_raw_trace_validation ?(worker_run_id = "run-1")
    ?(agent_name = "agent-a") ?(ok = true) () : Sessions.raw_trace_validation
    =
  {
    run_ref = make_run_ref ~worker_run_id ~agent_name ();
    ok;
    checks = [];
    evidence = [];
    paired_tool_result_count = 0;
    has_file_write = false;
    verification_pass_after_file_write = false;
    final_text = None;
    tool_names = [];
    stop_reason = None;
    failure_reason = None;
  }

let make_bundle ?(session_id = "sess-001") ?(worker_runs = [])
    ?(raw_trace_runs = []) ?(raw_trace_summaries = [])
    ?(raw_trace_validations = []) ?(latest_worker_run = None)
    ?(latest_accepted_worker_run = None)
    ?(latest_ready_worker_run = None)
    ?(latest_running_worker_run = None)
    ?(latest_completed_worker_run = None)
    ?(latest_validated_worker_run = None)
    ?(latest_failed_worker_run = None) ?(validated_worker_runs = [])
    ?(raw_trace_run_count = 0) ?(validated_worker_run_count = 0)
    ?(trace_capabilities = []) ?(hook_summary = []) ?(tool_catalog = [])
    ?(proof_bundle_cap = true) () : Sessions.proof_bundle =
  {
    session = make_session ~session_id ();
    report = make_report_rt ~session_id ();
    proof = make_proof ~session_id ();
    telemetry = make_telemetry ~session_id ();
    structured_telemetry = make_structured_telemetry ~session_id ();
    evidence = make_evidence ~session_id ();
    hook_summary;
    tool_catalog;
    latest_raw_trace_run = None;
    raw_trace_runs;
    raw_trace_summaries;
    raw_trace_validations;
    worker_runs;
    latest_accepted_worker_run;
    latest_ready_worker_run;
    latest_running_worker_run;
    latest_worker_run;
    latest_completed_worker_run;
    latest_validated_worker_run;
    latest_failed_worker_run;
    validated_worker_runs;
    raw_trace_run_count;
    validated_worker_run_count;
    trace_capabilities;
    capabilities =
      {
        raw_trace = raw_trace_runs <> [];
        validated_summary =
          raw_trace_summaries <> [] && raw_trace_validations <> [];
        proof_bundle = proof_bundle_cap;
      };
  }

(* Helper: find a check by code in a report *)
let find_check (report : Conformance.report) code =
  List.find (fun (c : Conformance.check) -> c.code = code) report.checks

let find_check_by_name (report : Conformance.report) name =
  List.find (fun (c : Conformance.check) -> c.name = name) report.checks

(* === Test: valid bundle produces all-pass report === *)

let test_report_valid_bundle () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~agent_name:"tester"
      ~status:Sessions.Completed ~validated:true
      ~trace_capability:Sessions.Raw
      ~aliases:[ "alias-a" ] ~primary_alias:(Some "alias-a")
      ~resolved_provider:(Some "anthropic")
      ~resolved_model:(Some "claude") ()
  in
  let ref1 = make_run_ref ~worker_run_id:"r1" ~agent_name:"tester" () in
  let sum1 =
    make_raw_trace_summary ~worker_run_id:"r1" ~agent_name:"tester" ()
  in
  let val1 =
    make_raw_trace_validation ~worker_run_id:"r1" ~agent_name:"tester" ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[ val1 ]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[ w ]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[ Sessions.Raw ] ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check bool) "report.ok is true" true report.ok;
  let failed =
    List.filter (fun (c : Conformance.check) -> not c.passed) report.checks
  in
  Alcotest.(check int) "zero failed checks" 0 (List.length failed)

(* === Test: empty bundle === *)

let test_report_empty_bundle () =
  let bundle = make_bundle () in
  let report = Conformance.report bundle in
  let check_count = List.length report.checks in
  Alcotest.(check bool) "has multiple checks" true (check_count >= 17);
  Alcotest.(check (option string))
    "no latest worker" None report.summary.latest_worker_run_id;
  Alcotest.(check (option string))
    "no latest status" None report.summary.latest_worker_status;
  Alcotest.(check (list string))
    "empty aliases" [] report.summary.latest_worker_aliases

(* === Test: summary fields populated correctly === *)

let test_report_summary_fields () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~agent_name:"tester"
      ~role:(Some "reviewer") ~status:Sessions.Completed
      ~validated:true ~trace_capability:Sessions.Raw
      ~aliases:[ "alias-a" ] ~primary_alias:(Some "alias-a")
      ~resolved_provider:(Some "anthropic")
      ~resolved_model:(Some "claude-3") ()
  in
  let ref1 = make_run_ref ~worker_run_id:"r1" ~agent_name:"tester" () in
  let sum1 =
    make_raw_trace_summary ~worker_run_id:"r1" ~agent_name:"tester" ()
  in
  let val1 =
    make_raw_trace_validation ~worker_run_id:"r1" ~agent_name:"tester" ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[ val1 ]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[ w ]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[ Sessions.Raw ] ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check string)
    "session_id" "sess-001" report.summary.session_id;
  Alcotest.(check int)
    "worker_run_count" 1 report.summary.worker_run_count;
  Alcotest.(check (option string))
    "latest_worker_run_id" (Some "r1") report.summary.latest_worker_run_id;
  Alcotest.(check (option string))
    "latest_worker_agent_name" (Some "tester")
    report.summary.latest_worker_agent_name;
  Alcotest.(check (option string))
    "latest_worker_status" (Some "completed")
    report.summary.latest_worker_status;
  Alcotest.(check (option string))
    "latest_worker_role" (Some "reviewer")
    report.summary.latest_worker_role;
  Alcotest.(check (list string))
    "latest_worker_aliases" [ "alias-a" ]
    report.summary.latest_worker_aliases;
  Alcotest.(check (option bool))
    "latest_worker_validated" (Some true)
    report.summary.latest_worker_validated;
  Alcotest.(check (option string))
    "latest_resolved_provider" (Some "anthropic")
    report.summary.latest_resolved_provider;
  Alcotest.(check (option string))
    "latest_resolved_model" (Some "claude-3")
    report.summary.latest_resolved_model

(* === Test: report with broken bundle (ok=false) === *)

let test_report_broken_bundle () =
  let w =
    make_worker_run ~status:Sessions.Running ~resolved_provider:None
      ~resolved_model:None ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check bool) "report.ok is false" false report.ok

(* === Test: validated_count_mismatch check === *)

let test_check_validated_count_mismatch () =
  let w = make_worker_run ~validated:true ~trace_capability:Sessions.Raw () in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~validated_worker_runs:[]
      ~validated_worker_run_count:5
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "validated_count_mismatch" in
  Alcotest.(check bool) "mismatch detected" false c.passed

(* === Test: latest_worker_inconsistent === *)

let test_check_latest_worker_inconsistent () =
  let w1 = make_worker_run ~worker_run_id:"r1" () in
  let w2 = make_worker_run ~worker_run_id:"r2" () in
  let bundle =
    make_bundle ~worker_runs:[ w1; w2 ] ~latest_worker_run:(Some w1) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_worker_inconsistent" in
  Alcotest.(check bool) "inconsistency detected" false c.passed

(* === Test: latest_accepted/ready/running/completed/validated/failed === *)

let test_check_latest_accepted_consistent () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Accepted ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~latest_accepted_worker_run:(Some w)
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_accepted_inconsistent" in
  Alcotest.(check bool) "accepted consistent" true c.passed

let test_check_latest_ready_inconsistent () =
  let w1 =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Ready ()
  in
  let w2 =
    make_worker_run ~worker_run_id:"r2" ~status:Sessions.Ready ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w1; w2 ]
      ~latest_ready_worker_run:(Some w1)
      ~latest_worker_run:(Some w2) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_ready_inconsistent" in
  Alcotest.(check bool) "ready inconsistent" false c.passed

let test_check_latest_running_consistent () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Running ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~latest_running_worker_run:(Some w)
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_running_inconsistent" in
  Alcotest.(check bool) "running consistent" true c.passed

let test_check_latest_completed_consistent () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Completed ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~latest_completed_worker_run:(Some w)
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_completed_inconsistent" in
  Alcotest.(check bool) "completed consistent" true c.passed

let test_check_latest_validated_inconsistent () =
  let w1 = make_worker_run ~worker_run_id:"r1" ~validated:true () in
  let w2 = make_worker_run ~worker_run_id:"r2" ~validated:true () in
  let bundle =
    make_bundle ~worker_runs:[ w1; w2 ]
      ~latest_validated_worker_run:(Some w1)
      ~latest_worker_run:(Some w2) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_validated_inconsistent" in
  Alcotest.(check bool) "validated inconsistent" false c.passed

let test_check_latest_failed_consistent () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~status:Sessions.Failed
      ~failure_reason:(Some "timeout")
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~latest_failed_worker_run:(Some w)
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "latest_failed_inconsistent" in
  Alcotest.(check bool) "failed consistent" true c.passed

(* === Test: raw_trace_not_addressable === *)

let test_check_raw_trace_not_addressable () =
  let w = make_worker_run ~worker_run_id:"r1" () in
  let orphan_ref = make_run_ref ~worker_run_id:"orphan-99" () in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~raw_trace_runs:[ orphan_ref ]
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "raw_trace_not_addressable" in
  Alcotest.(check bool) "orphan raw trace detected" false c.passed

(* === Test: lifecycle_non_monotonic === *)

let test_check_lifecycle_non_monotonic () =
  let w =
    make_worker_run ~started_at:(Some 300.0)
      ~last_progress_at:(Some 100.0) ~finished_at:(Some 400.0) ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "lifecycle_state_monotonic" in
  Alcotest.(check bool) "non-monotonic detected" false c.passed

let test_check_lifecycle_monotonic_ok () =
  let w =
    make_worker_run ~started_at:(Some 100.0)
      ~last_progress_at:(Some 150.0) ~finished_at:(Some 200.0) ()
  in
  let ref1 = make_run_ref ~worker_run_id:"run-1" () in
  let sum1 = make_raw_trace_summary () in
  let val1 = make_raw_trace_validation () in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[ val1 ]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[ w ]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[ Sessions.Raw ] ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "lifecycle_state_monotonic" in
  Alcotest.(check bool) "monotonic ok" true c.passed

let test_check_lifecycle_none_values () =
  let w =
    make_worker_run ~started_at:None ~last_progress_at:None
      ~finished_at:None
      ~status:Sessions.Planned ~resolved_provider:None
      ~resolved_model:None ~validated:false
      ~trace_capability:Sessions.No_trace
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "lifecycle_state_monotonic" in
  Alcotest.(check bool) "None values pass monotonic" true c.passed

(* === Test: identity_alias_mismatch === *)

let test_check_identity_alias_mismatch () =
  let w =
    make_worker_run ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:(Some "bad-alias") ~aliases:[ "alias-a" ] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "worker_identity_alias_consistent" in
  Alcotest.(check bool) "alias mismatch detected" false c.passed

let test_check_identity_no_worker_id () =
  let w =
    make_worker_run ~worker_id:None ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "worker_identity_alias_consistent" in
  Alcotest.(check bool) "missing worker_id detected" false c.passed

let test_check_identity_consistent () =
  let w =
    make_worker_run ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:(Some "alias-a") ~aliases:[ "alias-a" ] ()
  in
  let ref1 = make_run_ref ~worker_run_id:"run-1" () in
  let sum1 = make_raw_trace_summary () in
  let val1 = make_raw_trace_validation () in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[ val1 ]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[ w ]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[ Sessions.Raw ] ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "worker_identity_alias_consistent" in
  Alcotest.(check bool) "identity consistent" true c.passed

(* === Test: resolved_runtime_missing === *)

let test_check_resolved_runtime_missing () =
  let w =
    make_worker_run ~status:Sessions.Running ~resolved_provider:None
      ~resolved_model:None ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "resolved_runtime_missing" in
  Alcotest.(check bool) "missing resolved for Running" false c.passed

let test_check_resolved_ok_for_planned () =
  let w =
    make_worker_run ~status:Sessions.Planned ~resolved_provider:None
      ~resolved_model:None ~worker_id:(Some "w")
      ~runtime_actor:(Some "a") ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "resolved_runtime_missing" in
  Alcotest.(check bool) "Planned can lack resolved" true c.passed

(* === Test: validated_not_raw_capable === *)

let test_check_validated_not_raw () =
  let w =
    make_worker_run ~validated:true ~trace_capability:Sessions.Summary_only
      ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~validated_worker_runs:[ w ]
      ~validated_worker_run_count:1 ~latest_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "validated_worker_not_raw_capable" in
  Alcotest.(check bool) "validated but not raw" false c.passed

(* === Test: trace_capabilities_inconsistent === *)

let test_check_trace_capabilities_consistent () =
  let w = make_worker_run ~trace_capability:Sessions.Raw () in
  let ref1 = make_run_ref () in
  let sum1 = make_raw_trace_summary () in
  let val1 = make_raw_trace_validation () in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[ val1 ]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[ w ]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[ Sessions.Raw ] ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "trace_capabilities_inconsistent" in
  Alcotest.(check bool) "trace caps consistent" true c.passed

let test_check_trace_capabilities_inconsistent () =
  let w = make_worker_run ~trace_capability:Sessions.Raw () in
  let bundle =
    make_bundle ~worker_runs:[ w ]
      ~trace_capabilities:[ Sessions.Summary_only ]
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "trace_capabilities_inconsistent" in
  Alcotest.(check bool) "trace caps inconsistent" false c.passed

(* === Test: raw_trace_shape_mismatch === *)

let test_check_raw_trace_shape_mismatch () =
  let ref1 = make_run_ref () in
  let sum1 = make_raw_trace_summary () in
  (* no validations, but run_count = 1 and summaries count = 1 *)
  let bundle =
    make_bundle ~raw_trace_runs:[ ref1 ]
      ~raw_trace_summaries:[ sum1 ]
      ~raw_trace_validations:[]
      ~raw_trace_run_count:1 ()
  in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "raw_trace_shapes_consistent" in
  Alcotest.(check bool) "shape mismatch" false c.passed

(* === Test: proof_bundle_available === *)

let test_check_proof_bundle_available () =
  let bundle = make_bundle ~proof_bundle_cap:true () in
  let report = Conformance.report bundle in
  let c = find_check report "proof_bundle_available" in
  Alcotest.(check bool) "proof bundle available" true c.passed

let test_check_proof_bundle_unavailable () =
  let bundle = make_bundle ~proof_bundle_cap:false () in
  let report = Conformance.report bundle in
  let c = find_check report "proof_bundle_available" in
  Alcotest.(check bool) "proof bundle unavailable" false c.passed

(* === Test: hook and tool catalog counts in report === *)

let test_report_hook_summary_count () =
  let hs : Sessions.hook_summary =
    {
      hook_name = "on_start";
      count = 3;
      latest_decision = Some "allow";
      latest_detail = None;
      latest_ts = Some 1000.0;
    }
  in
  let hs2 : Sessions.hook_summary =
    {
      hook_name = "on_tool";
      count = 5;
      latest_decision = None;
      latest_detail = None;
      latest_ts = None;
    }
  in
  let bundle = make_bundle ~hook_summary:[ hs; hs2 ] () in
  let report = Conformance.report bundle in
  Alcotest.(check int) "hook_event_count" 8 report.summary.hook_event_count

let test_report_tool_catalog_count () =
  let t1 : Sessions.tool_contract =
    {
      name = "read_file";
      description = "Read a file";
      origin = None;
      kind = None;
      shell = None;
      notes = [];
      examples = [];
    }
  in
  let bundle = make_bundle ~tool_catalog:[ t1 ] () in
  let report = Conformance.report bundle in
  Alcotest.(check int)
    "tool_catalog_count" 1 report.summary.tool_catalog_count

(* === Test: failed worker in report summary === *)

let test_report_failed_worker () =
  let w =
    make_worker_run ~worker_run_id:"f1" ~status:Sessions.Failed
      ~failure_reason:(Some "timeout") ~error:None
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w)
      ~latest_failed_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "latest_failed_worker_run_id" (Some "f1")
    report.summary.latest_failed_worker_run_id;
  Alcotest.(check (option string))
    "failure_reason" (Some "timeout")
    report.summary.latest_failure_reason

let test_report_failed_error_fallback () =
  let w =
    make_worker_run ~worker_run_id:"f2" ~status:Sessions.Failed
      ~failure_reason:None ~error:(Some "oops")
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w)
      ~latest_failed_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "falls back to error" (Some "oops")
    report.summary.latest_failure_reason

(* === Test: worker_status_name indirectly via report === *)

let test_report_status_planned () =
  let w =
    make_worker_run ~status:Sessions.Planned ~resolved_provider:None
      ~resolved_model:None ~worker_id:(Some "w")
      ~runtime_actor:(Some "a") ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is planned" (Some "planned") report.summary.latest_worker_status

let test_report_status_accepted () =
  let w = make_worker_run ~status:Sessions.Accepted () in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is accepted" (Some "accepted")
    report.summary.latest_worker_status

let test_report_status_ready () =
  let w = make_worker_run ~status:Sessions.Ready () in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is ready" (Some "ready") report.summary.latest_worker_status

let test_report_status_running () =
  let w = make_worker_run ~status:Sessions.Running () in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is running" (Some "running") report.summary.latest_worker_status

let test_report_status_completed () =
  let w = make_worker_run ~status:Sessions.Completed () in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is completed" (Some "completed")
    report.summary.latest_worker_status

let test_report_status_failed () =
  let w =
    make_worker_run ~status:Sessions.Failed ~failure_reason:(Some "err")
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  Alcotest.(check (option string))
    "status is failed" (Some "failed") report.summary.latest_worker_status

(* === Test: direct_evidence check with raw-capable workers === *)

let test_check_direct_evidence_missing_raw () =
  let w =
    make_worker_run ~worker_run_id:"r1" ~trace_capability:Sessions.Raw ()
  in
  (* Worker is Raw but no matching raw_trace_run *)
  let bundle =
    make_bundle ~worker_runs:[ w ] ~raw_trace_runs:[]
      ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "direct_evidence_incomplete" in
  Alcotest.(check bool) "missing raw for raw-capable" false c.passed

let test_check_direct_evidence_summary_only_ok () =
  let w =
    make_worker_run ~worker_run_id:"r1"
      ~trace_capability:Sessions.Summary_only ~validated:false ()
  in
  let bundle =
    make_bundle ~worker_runs:[ w ] ~latest_worker_run:(Some w) ()
  in
  let report = Conformance.report bundle in
  let c = find_check report "direct_evidence_incomplete" in
  Alcotest.(check bool) "summary-only passes" true c.passed

(* === Alcotest runner === *)

let () =
  Alcotest.run "Conformance pure"
    [
      ( "report_basic",
        [
          Alcotest.test_case "valid_bundle" `Quick test_report_valid_bundle;
          Alcotest.test_case "empty_bundle" `Quick test_report_empty_bundle;
          Alcotest.test_case "summary_fields" `Quick
            test_report_summary_fields;
          Alcotest.test_case "broken_bundle" `Quick test_report_broken_bundle;
        ] );
      ( "check_counts",
        [
          Alcotest.test_case "validated_count_mismatch" `Quick
            test_check_validated_count_mismatch;
          Alcotest.test_case "raw_trace_shape_mismatch" `Quick
            test_check_raw_trace_shape_mismatch;
        ] );
      ( "check_latest_workers",
        [
          Alcotest.test_case "latest_worker_inconsistent" `Quick
            test_check_latest_worker_inconsistent;
          Alcotest.test_case "latest_accepted_consistent" `Quick
            test_check_latest_accepted_consistent;
          Alcotest.test_case "latest_ready_inconsistent" `Quick
            test_check_latest_ready_inconsistent;
          Alcotest.test_case "latest_running_consistent" `Quick
            test_check_latest_running_consistent;
          Alcotest.test_case "latest_completed_consistent" `Quick
            test_check_latest_completed_consistent;
          Alcotest.test_case "latest_validated_inconsistent" `Quick
            test_check_latest_validated_inconsistent;
          Alcotest.test_case "latest_failed_consistent" `Quick
            test_check_latest_failed_consistent;
        ] );
      ( "check_lifecycle",
        [
          Alcotest.test_case "non_monotonic" `Quick
            test_check_lifecycle_non_monotonic;
          Alcotest.test_case "monotonic_ok" `Quick
            test_check_lifecycle_monotonic_ok;
          Alcotest.test_case "none_values" `Quick
            test_check_lifecycle_none_values;
        ] );
      ( "check_identity",
        [
          Alcotest.test_case "alias_mismatch" `Quick
            test_check_identity_alias_mismatch;
          Alcotest.test_case "no_worker_id" `Quick
            test_check_identity_no_worker_id;
          Alcotest.test_case "consistent" `Quick
            test_check_identity_consistent;
        ] );
      ( "check_resolved",
        [
          Alcotest.test_case "missing" `Quick
            test_check_resolved_runtime_missing;
          Alcotest.test_case "ok_for_planned" `Quick
            test_check_resolved_ok_for_planned;
        ] );
      ( "check_validated_raw",
        [
          Alcotest.test_case "not_raw_capable" `Quick
            test_check_validated_not_raw;
        ] );
      ( "check_trace_capabilities",
        [
          Alcotest.test_case "consistent" `Quick
            test_check_trace_capabilities_consistent;
          Alcotest.test_case "inconsistent" `Quick
            test_check_trace_capabilities_inconsistent;
        ] );
      ( "check_raw_trace",
        [
          Alcotest.test_case "not_addressable" `Quick
            test_check_raw_trace_not_addressable;
        ] );
      ( "check_proof_bundle",
        [
          Alcotest.test_case "available" `Quick
            test_check_proof_bundle_available;
          Alcotest.test_case "unavailable" `Quick
            test_check_proof_bundle_unavailable;
        ] );
      ( "check_direct_evidence",
        [
          Alcotest.test_case "missing_raw" `Quick
            test_check_direct_evidence_missing_raw;
          Alcotest.test_case "summary_only_ok" `Quick
            test_check_direct_evidence_summary_only_ok;
        ] );
      ( "report_summary",
        [
          Alcotest.test_case "hook_count" `Quick test_report_hook_summary_count;
          Alcotest.test_case "tool_catalog_count" `Quick
            test_report_tool_catalog_count;
          Alcotest.test_case "failed_worker" `Quick test_report_failed_worker;
          Alcotest.test_case "failed_error_fallback" `Quick
            test_report_failed_error_fallback;
        ] );
      ( "worker_status_name",
        [
          Alcotest.test_case "planned" `Quick test_report_status_planned;
          Alcotest.test_case "accepted" `Quick test_report_status_accepted;
          Alcotest.test_case "ready" `Quick test_report_status_ready;
          Alcotest.test_case "running" `Quick test_report_status_running;
          Alcotest.test_case "completed" `Quick test_report_status_completed;
          Alcotest.test_case "failed" `Quick test_report_status_failed;
        ] );
    ]
