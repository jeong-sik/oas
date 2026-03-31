(** Additional conformance tests targeting uncovered check paths.
    Exercises: direct evidence checks, raw trace addressability,
    worker validation + trace cross-checks, hook/tool counts,
    edge cases with multiple workers, and report serialization. *)

open Agent_sdk
open Alcotest

(* ── Helpers (reuse from test_conformance_pure pattern) ───────── *)

let make_session ?(session_id = "sess-c2") ?(goal = "cov2") () : Runtime.session =
  {
    session_id; goal; title = None; tag = None;
    permission_mode = None; phase = Runtime.Completed;
    created_at = 1000.0; updated_at = 1001.0;
    provider = None; model = None; system_prompt = None;
    max_turns = 10; workdir = None;
    planned_participants = []; participants = [];
    artifacts = []; turn_count = 0;
    last_seq = 0; outcome = None;
  }

let make_report ?(session_id = "sess-c2") () : Runtime.report =
  { session_id; summary = []; markdown = ""; generated_at = 1000.0 }

let make_proof ?(session_id = "sess-c2") () : Runtime.proof =
  { session_id; ok = true; checks = []; evidence = []; generated_at = 1000.0 }

let make_telemetry ?(session_id = "sess-c2") () : Sessions.telemetry =
  { session_id; generated_at = 1000.0; step_count = 0;
    event_counts = []; steps = [] }

let make_structured_telemetry ?(session_id = "sess-c2") ()
    : Sessions.structured_telemetry =
  { session_id; generated_at = 1000.0; step_count = 0;
    event_counts = []; steps = [] }

let make_evidence ?(session_id = "sess-c2") () : Sessions.evidence =
  { session_id; generated_at = 1000.0; files = []; missing_files = [] }

let make_run_ref ?(worker_run_id = "run-c2") ?(agent_name = "agent-c2") ()
    : Raw_trace.run_ref =
  { worker_run_id; path = "/tmp/fake"; start_seq = 0; end_seq = 10;
    agent_name; session_id = Some "sess-c2" }

let make_worker ?(worker_run_id = "run-c2") ?(worker_id = Some "w-c2")
    ?(agent_name = "agent-c2") ?(runtime_actor = Some "actor-c2")
    ?(role = None) ?(aliases = ["alias-c2"])
    ?(primary_alias = Some "alias-c2") ?(provider = Some "anthropic")
    ?(model = Some "claude") ?(requested_provider = None)
    ?(requested_model = None) ?(requested_policy = None)
    ?(resolved_provider = Some "anthropic")
    ?(resolved_model = Some "claude") ?(status = Sessions.Completed)
    ?(trace_capability = Sessions.Raw) ?(validated = true)
    ?(tool_names = []) ?(final_text = None) ?(stop_reason = None)
    ?(error = None) ?(failure_reason = None)
    ?(accepted_at = None) ?(ready_at = None)
    ?(first_progress_at = None) ?(started_at = Some 100.0)
    ?(finished_at = Some 200.0) ?(last_progress_at = Some 150.0)
    ?(policy_snapshot = None)
    ?(paired_tool_result_count = 0) ?(has_file_write = false)
    ?(verification_pass_after_file_write = false) () : Sessions.worker_run =
  { worker_run_id; worker_id; agent_name; runtime_actor; role;
    aliases; primary_alias; provider; model;
    requested_provider; requested_model; requested_policy;
    resolved_provider; resolved_model; status; trace_capability;
    validated; tool_names; final_text; stop_reason; error;
    failure_reason; accepted_at; ready_at; first_progress_at;
    started_at; finished_at; last_progress_at; policy_snapshot;
    paired_tool_result_count; has_file_write;
    verification_pass_after_file_write }

let make_summary ?(worker_run_id = "run-c2") ?(agent_name = "agent-c2") ()
    : Raw_trace.run_summary =
  { run_ref = make_run_ref ~worker_run_id ~agent_name ();
    record_count = 5; assistant_block_count = 1;
    tool_execution_started_count = 0; tool_execution_finished_count = 0;
    hook_invoked_count = 0; hook_names = []; tool_names = [];
    final_text = None; stop_reason = None; error = None;
    started_at = Some 100.0; finished_at = Some 200.0 }

let make_validation ?(worker_run_id = "run-c2") ?(agent_name = "agent-c2")
    ?(ok = true) () : Raw_trace.run_validation =
  { run_ref = make_run_ref ~worker_run_id ~agent_name ();
    ok; checks = []; evidence = [];
    paired_tool_result_count = 0; has_file_write = false;
    verification_pass_after_file_write = false;
    final_text = None; tool_names = []; stop_reason = None;
    failure_reason = None }

let make_bundle ?(session_id = "sess-c2") ?(worker_runs = [])
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
  { session = make_session ~session_id ();
    report = make_report ~session_id ();
    proof = make_proof ~session_id ();
    telemetry = make_telemetry ~session_id ();
    structured_telemetry = make_structured_telemetry ~session_id ();
    evidence = make_evidence ~session_id ();
    hook_summary; tool_catalog;
    latest_raw_trace_run = None;
    raw_trace_runs; raw_trace_summaries; raw_trace_validations;
    worker_runs;
    latest_accepted_worker_run; latest_ready_worker_run;
    latest_running_worker_run; latest_worker_run;
    latest_completed_worker_run; latest_validated_worker_run;
    latest_failed_worker_run; validated_worker_runs;
    raw_trace_run_count; validated_worker_run_count;
    trace_capabilities;
    capabilities = {
      raw_trace = raw_trace_runs <> [];
      validated_summary =
        raw_trace_summaries <> [] && raw_trace_validations <> [];
      proof_bundle = proof_bundle_cap;
    } }

let find_check (report : Conformance.report) code =
  List.find (fun (c : Conformance.check) -> c.code = code) report.checks

let find_check_by_name (report : Conformance.report) name =
  List.find (fun (c : Conformance.check) -> c.name = name) report.checks

(* ── Tests ─────────────────────────────────────────────────────── *)

(* Multiple workers with different statuses *)
let test_multi_worker_statuses () =
  let w_planned = make_worker ~worker_run_id:"r-p" ~status:Sessions.Planned
      ~resolved_provider:None ~resolved_model:None
      ~validated:false ~trace_capability:Sessions.No_trace
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] () in
  let w_accepted = make_worker ~worker_run_id:"r-a" ~status:Sessions.Accepted () in
  let w_completed = make_worker ~worker_run_id:"r-c" ~status:Sessions.Completed () in
  let bundle = make_bundle
      ~worker_runs:[w_planned; w_accepted; w_completed]
      ~latest_worker_run:(Some w_completed)
      ~latest_accepted_worker_run:(Some w_accepted)
      ~latest_completed_worker_run:(Some w_completed) () in
  let report = Conformance.report bundle in
  check int "3 workers" 3 report.summary.worker_run_count;
  check (option string) "latest completed"
    (Some "r-c") report.summary.latest_completed_worker_run_id

(* Validated but summary_only trace should fail *)
let test_validated_summary_only () =
  let w = make_worker ~validated:true
      ~trace_capability:Sessions.Summary_only () in
  let bundle = make_bundle ~worker_runs:[w]
      ~validated_worker_runs:[w] ~validated_worker_run_count:1
      ~latest_worker_run:(Some w) ~latest_validated_worker_run:(Some w) () in
  let report = Conformance.report bundle in
  let c = find_check report "validated_worker_not_raw_capable" in
  check bool "summary_only fails" false c.passed

(* No_trace workers pass direct evidence *)
let test_no_trace_passes_evidence () =
  let w = make_worker ~trace_capability:Sessions.No_trace
      ~validated:false () in
  let bundle = make_bundle ~worker_runs:[w]
      ~latest_worker_run:(Some w) () in
  let report = Conformance.report bundle in
  let c = find_check report "direct_evidence_incomplete" in
  check bool "no_trace passes" true c.passed

(* Multiple hooks accumulate count *)
let test_multi_hook_counts () =
  let h1 : Sessions.hook_summary = {
    hook_name = "on_start"; count = 10;
    latest_decision = Some "allow"; latest_detail = None;
    latest_ts = Some 1000.0 } in
  let h2 : Sessions.hook_summary = {
    hook_name = "on_tool"; count = 20;
    latest_decision = None; latest_detail = Some "detail";
    latest_ts = None } in
  let h3 : Sessions.hook_summary = {
    hook_name = "on_end"; count = 5;
    latest_decision = None; latest_detail = None;
    latest_ts = None } in
  let bundle = make_bundle ~hook_summary:[h1; h2; h3] () in
  let report = Conformance.report bundle in
  check int "hook total" 35 report.summary.hook_event_count

(* Multiple tool catalog entries *)
let test_multi_tool_catalog () =
  let t1 : Sessions.tool_contract = {
    name = "read"; description = "Read file";
    origin = Some "local"; kind = Some "file";
    shell = None; notes = ["note1"]; examples = ["ex1"] } in
  let t2 : Sessions.tool_contract = {
    name = "write"; description = "Write file";
    origin = None; kind = None; shell = None;
    notes = []; examples = [] } in
  let t3 : Sessions.tool_contract = {
    name = "exec"; description = "Execute";
    origin = Some "mcp"; kind = Some "shell";
    shell = Some {
      single_command_only = true;
      shell_metacharacters_allowed = false;
      chaining_allowed = false;
      redirection_allowed = false;
      pipes_allowed = false;
      workdir_policy = Some Tool.Required;
    };
    notes = []; examples = [] } in
  let bundle = make_bundle ~tool_catalog:[t1; t2; t3] () in
  let report = Conformance.report bundle in
  check int "tool count" 3 report.summary.tool_catalog_count

(* Raw trace runs matching all workers *)
let test_raw_trace_addressable_multi () =
  let w1 = make_worker ~worker_run_id:"r1" () in
  let w2 = make_worker ~worker_run_id:"r2" ~agent_name:"agent-2" () in
  let ref1 = make_run_ref ~worker_run_id:"r1" () in
  let ref2 = make_run_ref ~worker_run_id:"r2" ~agent_name:"agent-2" () in
  let sum1 = make_summary ~worker_run_id:"r1" () in
  let sum2 = make_summary ~worker_run_id:"r2" ~agent_name:"agent-2" () in
  let val1 = make_validation ~worker_run_id:"r1" () in
  let val2 = make_validation ~worker_run_id:"r2" ~agent_name:"agent-2" () in
  let bundle = make_bundle
      ~worker_runs:[w1; w2]
      ~raw_trace_runs:[ref1; ref2]
      ~raw_trace_summaries:[sum1; sum2]
      ~raw_trace_validations:[val1; val2]
      ~raw_trace_run_count:2
      ~latest_worker_run:(Some w2) () in
  let report = Conformance.report bundle in
  let c = find_check report "raw_trace_not_addressable" in
  check bool "all addressable" true c.passed

(* Failed worker with no reason - check the failed_worker_reason_missing *)
let test_failed_no_reason () =
  let w = make_worker ~status:Sessions.Failed
      ~failure_reason:None ~error:None
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] () in
  let bundle = make_bundle ~worker_runs:[w]
      ~latest_worker_run:(Some w)
      ~latest_failed_worker_run:(Some w) () in
  let report = Conformance.report bundle in
  (* failed_worker_reason_missing check: workers with no error AND no failure_reason pass *)
  let c = find_check report "failed_worker_reason_missing" in
  check bool "no reason passes" true c.passed

(* Worker with error but no failure_reason *)
let test_worker_with_error () =
  let w = make_worker ~status:Sessions.Failed
      ~failure_reason:None ~error:(Some "timeout")
      ~worker_id:(Some "w") ~runtime_actor:(Some "a")
      ~primary_alias:None ~aliases:[] () in
  let bundle = make_bundle ~worker_runs:[w]
      ~latest_worker_run:(Some w)
      ~latest_failed_worker_run:(Some w) () in
  let report = Conformance.report bundle in
  let c = find_check report "failed_worker_reason_missing" in
  check bool "has error passes" true c.passed

(* Report JSON round-trip *)
let test_report_json_roundtrip () =
  let w = make_worker () in
  let ref1 = make_run_ref () in
  let sum1 = make_summary () in
  let val1 = make_validation () in
  let bundle = make_bundle ~worker_runs:[w]
      ~raw_trace_runs:[ref1] ~raw_trace_summaries:[sum1]
      ~raw_trace_validations:[val1]
      ~latest_worker_run:(Some w)
      ~latest_completed_worker_run:(Some w)
      ~latest_validated_worker_run:(Some w)
      ~validated_worker_runs:[w]
      ~raw_trace_run_count:1
      ~validated_worker_run_count:1
      ~trace_capabilities:[Sessions.Raw] () in
  let report = Conformance.report bundle in
  let json = Conformance.report_to_yojson report in
  match Conformance.report_of_yojson json with
  | Ok decoded ->
    check bool "ok matches" report.ok decoded.ok;
    check int "check count" (List.length report.checks) (List.length decoded.checks)
  | Error msg -> fail (Printf.sprintf "roundtrip: %s" msg)

(* Missing raw_trace (missing_raw_trace code) *)
let test_missing_raw_trace_code () =
  let bundle = make_bundle ~raw_trace_runs:[] ~raw_trace_run_count:0 () in
  let report = Conformance.report bundle in
  let c = find_check_by_name report "raw_trace_shapes_consistent" in
  check string "code for empty" "missing_raw_trace" c.code

(* Trace capabilities with multiple types *)
let test_multi_trace_capabilities () =
  let w1 = make_worker ~worker_run_id:"r1"
      ~trace_capability:Sessions.Raw () in
  let w2 = make_worker ~worker_run_id:"r2"
      ~trace_capability:Sessions.Summary_only
      ~validated:false () in
  let bundle = make_bundle ~worker_runs:[w1; w2]
      ~trace_capabilities:[Sessions.Raw; Sessions.Summary_only]
      ~latest_worker_run:(Some w2) () in
  let report = Conformance.report bundle in
  let c = find_check report "trace_capabilities_inconsistent" in
  check bool "multi caps consistent" true c.passed

(* ── Suite ─────────────────────────────────────────────────────── *)

let () =
  run "conformance_cov2" [
    "multi_worker", [
      test_case "multi statuses" `Quick test_multi_worker_statuses;
      test_case "failed no reason" `Quick test_failed_no_reason;
      test_case "worker with error" `Quick test_worker_with_error;
    ];
    "trace_checks", [
      test_case "validated summary_only" `Quick test_validated_summary_only;
      test_case "no_trace evidence" `Quick test_no_trace_passes_evidence;
      test_case "addressable multi" `Quick test_raw_trace_addressable_multi;
      test_case "missing raw trace code" `Quick test_missing_raw_trace_code;
      test_case "multi capabilities" `Quick test_multi_trace_capabilities;
    ];
    "summary_counts", [
      test_case "hook counts" `Quick test_multi_hook_counts;
      test_case "tool catalog" `Quick test_multi_tool_catalog;
    ];
    "serialization", [
      test_case "report roundtrip" `Quick test_report_json_roundtrip;
    ];
  ]
