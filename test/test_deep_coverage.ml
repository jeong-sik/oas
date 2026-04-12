(** Deep coverage tests targeting mid-range uncovered modules.

    Covers: sessions_types, durable, event_forward, builder,
    a2a_task_store, transport, pipeline types, internal_query_engine. *)

open Alcotest
open Agent_sdk

(* ══════════════════════════════════════════════════════════════
   1. Sessions_types — JSON roundtrip for ALL types + edge cases
   ══════════════════════════════════════════════════════════════ *)

(* -- trace_capability -- *)

let test_trace_capability_all_variants () =
  let variants =
    [ Sessions.Raw; Summary_only; No_trace ]
  in
  List.iter (fun v ->
    let json = Sessions.trace_capability_to_yojson v in
    match Sessions.trace_capability_of_yojson json with
    | Ok v2 ->
      check string "roundtrip"
        (Sessions.show_trace_capability v)
        (Sessions.show_trace_capability v2)
    | Error e -> fail (Printf.sprintf "trace_capability roundtrip: %s" e)
  ) variants

let test_trace_capability_invalid () =
  match Sessions.trace_capability_of_yojson (`String "bogus") with
  | Error _ -> ()
  | Ok _ -> fail "expected error on invalid trace_capability"

let test_trace_capability_json_values () =
  check (testable Yojson.Safe.pp Yojson.Safe.equal)
    "Raw" (`List [`String "raw"])
    (Sessions.trace_capability_to_yojson Raw);
  check (testable Yojson.Safe.pp Yojson.Safe.equal)
    "Summary_only" (`List [`String "summary_only"])
    (Sessions.trace_capability_to_yojson Summary_only);
  check (testable Yojson.Safe.pp Yojson.Safe.equal)
    "No_trace" (`List [`String "none"])
    (Sessions.trace_capability_to_yojson No_trace)

(* -- worker_status -- *)

let test_worker_status_all_variants () =
  let variants =
    [ Sessions.Planned; Accepted; Ready; Running; Completed; Failed ]
  in
  List.iter (fun v ->
    let json = Sessions.worker_status_to_yojson v in
    match Sessions.worker_status_of_yojson json with
    | Ok v2 ->
      check string "roundtrip"
        (Sessions.show_worker_status v)
        (Sessions.show_worker_status v2)
    | Error e -> fail (Printf.sprintf "worker_status roundtrip: %s" e)
  ) variants

let test_worker_status_wire_names () =
  check (testable Yojson.Safe.pp Yojson.Safe.equal)
    "Planned" (`List [`String "planned"])
    (Sessions.worker_status_to_yojson Planned);
  check (testable Yojson.Safe.pp Yojson.Safe.equal)
    "Failed" (`List [`String "failed"])
    (Sessions.worker_status_to_yojson Failed)

let test_worker_status_invalid () =
  match Sessions.worker_status_of_yojson (`String "unknown") with
  | Error _ -> ()
  | Ok _ -> fail "expected error on invalid worker_status"

(* -- session_info -- *)

let test_session_info_all_fields () =
  let v : Sessions.session_info = {
    session_id = "sid-001"; title = Some "My Session"; tag = Some "v1";
    goal = "test coverage"; updated_at = 1700000000.0;
    phase = Runtime.Running; participant_count = 5; path = "/tmp/session";
  } in
  match Sessions.session_info_of_yojson (Sessions.session_info_to_yojson v) with
  | Ok v2 ->
    check string "session_id" v.session_id v2.session_id;
    check (option string) "title" v.title v2.title;
    check (option string) "tag" v.tag v2.tag;
    check string "goal" v.goal v2.goal;
    check int "participant_count" v.participant_count v2.participant_count;
    check string "path" v.path v2.path
  | Error e -> fail e

let test_session_info_none_optionals () =
  let v : Sessions.session_info = {
    session_id = "s"; title = None; tag = None;
    goal = "g"; updated_at = 0.0; phase = Runtime.Completed;
    participant_count = 0; path = "/x";
  } in
  match Sessions.session_info_of_yojson (Sessions.session_info_to_yojson v) with
  | Ok v2 ->
    check (option string) "title None" None v2.title;
    check (option string) "tag None" None v2.tag
  | Error e -> fail e

let test_session_info_all_phases () =
  let phases = [ Runtime.Bootstrapping; Running; Waiting_on_workers;
                 Finalizing; Completed; Failed; Cancelled ] in
  List.iter (fun phase ->
    let v : Sessions.session_info = {
      session_id = "s"; title = None; tag = None;
      goal = "g"; updated_at = 0.0; phase;
      participant_count = 0; path = "/";
    } in
    match Sessions.session_info_of_yojson (Sessions.session_info_to_yojson v) with
    | Ok _ -> ()
    | Error e -> fail (Printf.sprintf "phase roundtrip: %s" e)
  ) phases

(* -- telemetry types -- *)

let test_telemetry_event_count () =
  let v : Sessions.telemetry_event_count = { name = "tool.called"; count = 42 } in
  match Sessions.telemetry_event_count_of_yojson
    (Sessions.telemetry_event_count_to_yojson v) with
  | Ok v2 ->
    check string "name" v.name v2.name;
    check int "count" v.count v2.count
  | Error e -> fail e

let test_telemetry_step_some () =
  let v : Sessions.telemetry_step = {
    seq = 1; ts = 1000.0; kind = "agent_output";
    participant = Some "alice"; detail = Some "generated code";
  } in
  match Sessions.telemetry_step_of_yojson
    (Sessions.telemetry_step_to_yojson v) with
  | Ok v2 ->
    check int "seq" v.seq v2.seq;
    check (option string) "participant" v.participant v2.participant;
    check (option string) "detail" v.detail v2.detail
  | Error e -> fail e

let test_telemetry_step_none () =
  let v : Sessions.telemetry_step = {
    seq = 0; ts = 0.0; kind = "k"; participant = None; detail = None;
  } in
  match Sessions.telemetry_step_of_yojson
    (Sessions.telemetry_step_to_yojson v) with
  | Ok v2 ->
    check (option string) "participant" None v2.participant;
    check (option string) "detail" None v2.detail
  | Error e -> fail e

let test_telemetry_full () =
  let v : Sessions.telemetry = {
    session_id = "s1"; generated_at = 1000.0; step_count = 2;
    event_counts = [
      { name = "e1"; count = 3 };
      { name = "e2"; count = 7 };
    ];
    steps = [
      { seq = 0; ts = 1.0; kind = "k1"; participant = None; detail = None };
      { seq = 1; ts = 2.0; kind = "k2"; participant = Some "p"; detail = Some "d" };
    ];
  } in
  match Sessions.telemetry_of_yojson (Sessions.telemetry_to_yojson v) with
  | Ok v2 ->
    check int "step_count" 2 v2.step_count;
    check int "event_counts len" 2 (List.length v2.event_counts);
    check int "steps len" 2 (List.length v2.steps)
  | Error e -> fail e

let test_telemetry_empty_lists () =
  let v : Sessions.telemetry = {
    session_id = "s"; generated_at = 0.0; step_count = 0;
    event_counts = []; steps = [];
  } in
  match Sessions.telemetry_of_yojson (Sessions.telemetry_to_yojson v) with
  | Ok v2 ->
    check int "event_counts" 0 (List.length v2.event_counts);
    check int "steps" 0 (List.length v2.steps)
  | Error e -> fail e

(* -- structured telemetry -- *)

let test_structured_event_count () =
  let v : Sessions.structured_event_count = { event_name = "turn.started"; count = 5 } in
  match Sessions.structured_event_count_of_yojson
    (Sessions.structured_event_count_to_yojson v) with
  | Ok v2 -> check string "event_name" v.event_name v2.event_name
  | Error e -> fail e

let test_structured_telemetry_step_all_some () =
  let v : Sessions.structured_telemetry_step = {
    seq = 1; ts = 1.0; event_name = "tool.called";
    participant = Some "bob"; detail = Some "detail";
    actor = Some "actor"; role = Some "lead";
    provider = Some "anthropic"; model = Some "claude-sonnet-4-6";
    artifact_id = Some "aid"; artifact_name = Some "aname";
    artifact_kind = Some "code"; checkpoint_label = Some "cp1";
    outcome = Some "success";
  } in
  match Sessions.structured_telemetry_step_of_yojson
    (Sessions.structured_telemetry_step_to_yojson v) with
  | Ok v2 ->
    check (option string) "actor" v.actor v2.actor;
    check (option string) "role" v.role v2.role;
    check (option string) "provider" v.provider v2.provider;
    check (option string) "model" v.model v2.model;
    check (option string) "artifact_id" v.artifact_id v2.artifact_id;
    check (option string) "artifact_name" v.artifact_name v2.artifact_name;
    check (option string) "artifact_kind" v.artifact_kind v2.artifact_kind;
    check (option string) "checkpoint_label" v.checkpoint_label v2.checkpoint_label;
    check (option string) "outcome" v.outcome v2.outcome
  | Error e -> fail e

let test_structured_telemetry_step_all_none () =
  let v : Sessions.structured_telemetry_step = {
    seq = 0; ts = 0.0; event_name = "e";
    participant = None; detail = None; actor = None;
    role = None; provider = None; model = None;
    artifact_id = None; artifact_name = None;
    artifact_kind = None; checkpoint_label = None;
    outcome = None;
  } in
  match Sessions.structured_telemetry_step_of_yojson
    (Sessions.structured_telemetry_step_to_yojson v) with
  | Ok v2 ->
    check (option string) "all None participant" None v2.participant;
    check (option string) "all None outcome" None v2.outcome
  | Error e -> fail e

let test_structured_telemetry_full () =
  let v : Sessions.structured_telemetry = {
    session_id = "s"; generated_at = 100.0; step_count = 1;
    event_counts = [{ event_name = "e"; count = 1 }];
    steps = [{
      seq = 0; ts = 0.0; event_name = "e";
      participant = None; detail = None; actor = None;
      role = None; provider = None; model = None;
      artifact_id = None; artifact_name = None;
      artifact_kind = None; checkpoint_label = None;
      outcome = None;
    }];
  } in
  match Sessions.structured_telemetry_of_yojson
    (Sessions.structured_telemetry_to_yojson v) with
  | Ok v2 -> check int "step_count" 1 v2.step_count
  | Error e -> fail e

(* -- evidence types -- *)

let test_evidence_file_roundtrip () =
  let v : Sessions.evidence_file = {
    label = "log"; path = "/var/log/agent.log";
    size_bytes = 1024; md5 = "abc123def456";
  } in
  match Sessions.evidence_file_of_yojson
    (Sessions.evidence_file_to_yojson v) with
  | Ok v2 ->
    check string "label" v.label v2.label;
    check int "size_bytes" v.size_bytes v2.size_bytes;
    check string "md5" v.md5 v2.md5
  | Error e -> fail e

let test_missing_file_roundtrip () =
  let v : Sessions.missing_file = { label = "config"; path = "/etc/config.yml" } in
  match Sessions.missing_file_of_yojson
    (Sessions.missing_file_to_yojson v) with
  | Ok v2 ->
    check string "label" v.label v2.label;
    check string "path" v.path v2.path
  | Error e -> fail e

let test_evidence_full () =
  let v : Sessions.evidence = {
    session_id = "s1"; generated_at = 1000.0;
    files = [
      { label = "f1"; path = "/p1"; size_bytes = 100; md5 = "md5a" };
      { label = "f2"; path = "/p2"; size_bytes = 200; md5 = "md5b" };
    ];
    missing_files = [
      { label = "m1"; path = "/m1" };
    ];
  } in
  match Sessions.evidence_of_yojson (Sessions.evidence_to_yojson v) with
  | Ok v2 ->
    check int "files" 2 (List.length v2.files);
    check int "missing" 1 (List.length v2.missing_files)
  | Error e -> fail e

let test_evidence_empty_lists () =
  let v : Sessions.evidence = {
    session_id = "s"; generated_at = 0.0; files = []; missing_files = [];
  } in
  match Sessions.evidence_of_yojson (Sessions.evidence_to_yojson v) with
  | Ok v2 ->
    check int "files" 0 (List.length v2.files);
    check int "missing" 0 (List.length v2.missing_files)
  | Error e -> fail e

(* -- hook_summary -- *)

let test_hook_summary_all_some () =
  let v : Sessions.hook_summary = {
    hook_name = "before_turn"; count = 5;
    latest_decision = Some "allow";
    latest_detail = Some "auto-approved";
    latest_ts = Some 1700000000.0;
  } in
  match Sessions.hook_summary_of_yojson
    (Sessions.hook_summary_to_yojson v) with
  | Ok v2 ->
    check string "hook_name" v.hook_name v2.hook_name;
    check int "count" v.count v2.count;
    check (option string) "decision" v.latest_decision v2.latest_decision;
    check (option string) "detail" v.latest_detail v2.latest_detail
  | Error e -> fail e

let test_hook_summary_all_none () =
  let v : Sessions.hook_summary = {
    hook_name = "h"; count = 0;
    latest_decision = None; latest_detail = None; latest_ts = None;
  } in
  match Sessions.hook_summary_of_yojson
    (Sessions.hook_summary_to_yojson v) with
  | Ok v2 ->
    check (option string) "decision None" None v2.latest_decision;
    check (option string) "detail None" None v2.latest_detail
  | Error e -> fail e

(* -- tool_contract -- *)

let test_tool_contract_full () =
  let v : Sessions.tool_contract = {
    name = "shell"; description = "run commands";
    origin = Some "builtin"; kind = Some "bash";
    shell = Some {
      single_command_only = true;
      shell_metacharacters_allowed = false;
      chaining_allowed = false;
      redirection_allowed = true;
      pipes_allowed = false; workdir_policy = None;
    };
    notes = ["note1"; "note2"];
    examples = ["ls -la"; "pwd"];
  } in
  match Sessions.tool_contract_of_yojson
    (Sessions.tool_contract_to_yojson v) with
  | Ok v2 ->
    check string "name" v.name v2.name;
    check (option string) "origin" v.origin v2.origin;
    check (option string) "kind" v.kind v2.kind;
    check bool "has shell" true (Option.is_some v2.shell);
    check int "notes" 2 (List.length v2.notes);
    check int "examples" 2 (List.length v2.examples)
  | Error e -> fail e

let test_tool_contract_minimal () =
  let v : Sessions.tool_contract = {
    name = "t"; description = "d";
    origin = None; kind = None; shell = None;
    notes = []; examples = [];
  } in
  match Sessions.tool_contract_of_yojson
    (Sessions.tool_contract_to_yojson v) with
  | Ok v2 ->
    check (option string) "origin None" None v2.origin;
    check bool "no shell" false (Option.is_some v2.shell);
    check int "notes empty" 0 (List.length v2.notes)
  | Error e -> fail e

(* -- evidence_capabilities -- *)

let test_evidence_capabilities_all_true () =
  let v : Sessions.evidence_capabilities = {
    raw_trace = true; validated_summary = true; proof_bundle = true;
  } in
  match Sessions.evidence_capabilities_of_yojson
    (Sessions.evidence_capabilities_to_yojson v) with
  | Ok v2 ->
    check bool "raw_trace" true v2.raw_trace;
    check bool "validated_summary" true v2.validated_summary;
    check bool "proof_bundle" true v2.proof_bundle
  | Error e -> fail e

let test_evidence_capabilities_all_false () =
  let v : Sessions.evidence_capabilities = {
    raw_trace = false; validated_summary = false; proof_bundle = false;
  } in
  match Sessions.evidence_capabilities_of_yojson
    (Sessions.evidence_capabilities_to_yojson v) with
  | Ok v2 ->
    check bool "raw_trace" false v2.raw_trace;
    check bool "validated_summary" false v2.validated_summary;
    check bool "proof_bundle" false v2.proof_bundle
  | Error e -> fail e

(* -- worker_run -- *)

let mk_minimal_worker_run
    ?(status = Sessions.Planned)
    ?(trace_capability = Sessions.No_trace)
    ?(validated = false) () : Sessions.worker_run =
  {
    worker_run_id = "wr-001"; worker_id = None; agent_name = "agent-a";
    runtime_actor = None; role = None; aliases = []; primary_alias = None;
    provider = None; model = None;
    requested_provider = None; requested_model = None; requested_policy = None;
    resolved_provider = None; resolved_model = None;
    status; trace_capability; validated;
    tool_names = []; final_text = None; stop_reason = None;
    error = None; failure_reason = None;
    accepted_at = None; ready_at = None; first_progress_at = None;
    started_at = None; finished_at = None; last_progress_at = None;
    policy_snapshot = None; paired_tool_result_count = 0;
    has_file_write = false; verification_pass_after_file_write = false;
  }

let test_worker_run_minimal () =
  let v = mk_minimal_worker_run () in
  match Sessions.worker_run_of_yojson
    (Sessions.worker_run_to_yojson v) with
  | Ok v2 ->
    check string "worker_run_id" v.worker_run_id v2.worker_run_id;
    check string "agent_name" v.agent_name v2.agent_name;
    check bool "validated" false v2.validated
  | Error e -> fail e

let test_worker_run_fully_populated () =
  let v : Sessions.worker_run = {
    worker_run_id = "wr-full"; worker_id = Some "w1"; agent_name = "agent-b";
    runtime_actor = Some "actor-1"; role = Some "executor";
    aliases = ["alias1"; "alias2"]; primary_alias = Some "alias1";
    provider = Some "anthropic"; model = Some "claude-sonnet-4-6";
    requested_provider = Some "anthropic"; requested_model = Some "claude-sonnet-4-6";
    requested_policy = Some "default";
    resolved_provider = Some "anthropic"; resolved_model = Some "claude-sonnet-4-6";
    status = Completed; trace_capability = Raw; validated = true;
    tool_names = ["shell"; "read_file"];
    final_text = Some "Done."; stop_reason = Some "end_turn";
    error = None; failure_reason = None;
    accepted_at = Some 1000.0; ready_at = Some 1001.0;
    first_progress_at = Some 1002.0; started_at = Some 1000.5;
    finished_at = Some 1005.0; last_progress_at = Some 1004.5;
    policy_snapshot = Some "snapshot-json";
    paired_tool_result_count = 3;
    has_file_write = true; verification_pass_after_file_write = true;
  } in
  match Sessions.worker_run_of_yojson
    (Sessions.worker_run_to_yojson v) with
  | Ok v2 ->
    check string "worker_run_id" "wr-full" v2.worker_run_id;
    check (option string) "worker_id" (Some "w1") v2.worker_id;
    check int "aliases" 2 (List.length v2.aliases);
    check int "tool_names" 2 (List.length v2.tool_names);
    check (option string) "final_text" (Some "Done.") v2.final_text;
    check int "paired_tool_result_count" 3 v2.paired_tool_result_count;
    check bool "has_file_write" true v2.has_file_write;
    check bool "verification_pass" true v2.verification_pass_after_file_write
  | Error e -> fail e

let test_worker_run_each_status () =
  let statuses =
    [ Sessions.Planned; Accepted; Ready; Running; Completed; Failed ]
  in
  List.iter (fun status ->
    let v = mk_minimal_worker_run ~status () in
    match Sessions.worker_run_of_yojson
      (Sessions.worker_run_to_yojson v) with
    | Ok _ -> ()
    | Error e ->
      fail (Printf.sprintf "worker_run status %s: %s"
        (Sessions.show_worker_status status) e)
  ) statuses

let test_worker_run_each_trace () =
  let caps = [ Sessions.Raw; Summary_only; No_trace ] in
  List.iter (fun trace_capability ->
    let v = mk_minimal_worker_run ~trace_capability () in
    match Sessions.worker_run_of_yojson
      (Sessions.worker_run_to_yojson v) with
    | Ok _ -> ()
    | Error e ->
      fail (Printf.sprintf "worker_run trace %s: %s"
        (Sessions.show_trace_capability trace_capability) e)
  ) caps

(* -- show functions -- *)

let test_show_functions () =
  let check_nonempty label s =
    check bool label true (String.length s > 0) in
  check_nonempty "show_trace_capability"
    (Sessions.show_trace_capability Raw);
  check_nonempty "show_worker_status"
    (Sessions.show_worker_status Running);
  check_nonempty "show_session_info"
    (Sessions.show_session_info {
      session_id = "s"; title = None; tag = None;
      goal = "g"; updated_at = 0.0; phase = Runtime.Bootstrapping;
      participant_count = 0; path = "/";
    });
  check_nonempty "show_telemetry_event_count"
    (Sessions.show_telemetry_event_count { name = "x"; count = 0 });
  check_nonempty "show_telemetry_step"
    (Sessions.show_telemetry_step {
      seq = 0; ts = 0.0; kind = "k"; participant = None; detail = None });
  check_nonempty "show_structured_event_count"
    (Sessions.show_structured_event_count { event_name = "e"; count = 0 });
  check_nonempty "show_evidence_file"
    (Sessions.show_evidence_file {
      label = "l"; path = "p"; size_bytes = 0; md5 = "" });
  check_nonempty "show_missing_file"
    (Sessions.show_missing_file { label = "l"; path = "p" });
  check_nonempty "show_hook_summary"
    (Sessions.show_hook_summary {
      hook_name = "h"; count = 0;
      latest_decision = None; latest_detail = None; latest_ts = None });
  check_nonempty "show_tool_contract"
    (Sessions.show_tool_contract {
      name = "t"; description = "d";
      origin = None; kind = None; shell = None;
      notes = []; examples = [] });
  check_nonempty "show_evidence_capabilities"
    (Sessions.show_evidence_capabilities {
      raw_trace = false; validated_summary = false; proof_bundle = false });
  check_nonempty "show_worker_run"
    (Sessions.show_worker_run (mk_minimal_worker_run ()))

(* ══════════════════════════════════════════════════════════════
   2. Durable — additional edge cases
   ══════════════════════════════════════════════════════════════ *)

let test_durable_journal_entry_roundtrip () =
  let je : Durable.journal_entry = {
    step_name = "failing-step";
    started_at = 1000.0;
    completed_at = Some 1001.5;
    input_json = `Assoc [("x", `Int 1)];
    output_json = None;
    error = Some "timeout exceeded";
    attempt = 3;
  } in
  let state = Durable.Completed {
    journal = [je]; final_output = `Null;
  } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Completed { journal = [j]; _ }) ->
    check string "step_name" "failing-step" j.step_name;
    check (option string) "error" (Some "timeout exceeded") j.error;
    check int "attempt" 3 j.attempt;
    (match j.completed_at with
     | Some _ -> ()
     | None -> fail "expected completed_at")
  | _ -> fail "journal entry roundtrip failed"

let test_durable_journal_entry_no_completed () =
  let je : Durable.journal_entry = {
    step_name = "in-flight"; started_at = 500.0;
    completed_at = None; input_json = `String "hello";
    output_json = None; error = None; attempt = 1;
  } in
  let state = Durable.InProgress {
    current_step = "in-flight"; attempt = 1; journal = [je];
  } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (InProgress { journal = [j]; _ }) ->
    check bool "no completed_at" true (j.completed_at = None);
    check (option string) "no error" None j.error
  | _ -> fail "InProgress journal roundtrip"

let test_durable_unknown_state_string () =
  let json = `Assoc [("state", `String "Exploded")] in
  match Durable.execution_state_of_json json with
  | Error msg -> check bool "has message" true (String.length msg > 0)
  | Ok _ -> fail "expected error on unknown state"

let test_durable_resume_unknown_step () =
  let sm = Durable.create ~name:"r" () in
  let sm = Durable.add_step sm {
    name = "real_step"; execute = (fun x -> Ok x); retry_limit = 1;
  } in
  let state = Durable.Failed {
    at_step = "nonexistent"; journal = []; error = "old error";
  } in
  match Durable.resume sm state with
  | Failed { error; _ } ->
    check bool "error mentions not found" true
      (let pat = "not found" in
       try ignore (Str.search_forward (Str.regexp_string pat) error 0); true
       with Not_found -> false)
  | _ -> fail "expected Failed on unknown step resume"

let test_durable_resume_in_progress_noop () =
  let sm = Durable.create ~name:"r" () in
  let state = Durable.InProgress {
    current_step = "x"; attempt = 1; journal = [];
  } in
  match Durable.resume sm state with
  | InProgress _ -> ()
  | _ -> fail "resume of InProgress should be noop"

let test_durable_suspend_completed_noop () =
  let sm = Durable.create ~name:"s" () in
  let state = Durable.Completed { journal = []; final_output = `Null } in
  match Durable.suspend sm state ~reason:"why" with
  | Completed _ -> ()
  | _ -> fail "suspend of Completed should be noop"

let test_durable_suspend_failed_noop () =
  let sm = Durable.create ~name:"s" () in
  let state = Durable.Failed { at_step = "x"; journal = []; error = "e" } in
  match Durable.suspend sm state ~reason:"why" with
  | Failed _ -> ()
  | _ -> fail "suspend of Failed should be noop"

let test_durable_suspend_suspended_noop () =
  let sm = Durable.create ~name:"s" () in
  let state = Durable.Suspended {
    at_step = "x"; journal = []; reason = "old";
  } in
  match Durable.suspend sm state ~reason:"new" with
  | Suspended { reason = "old"; _ } -> ()
  | _ -> fail "suspend of Suspended should be noop"

let test_durable_resume_suspended () =
  let call_count = ref 0 in
  let step : Durable.step = {
    name = "step1"; retry_limit = 1;
    execute = (fun input -> incr call_count; Ok input);
  } in
  let sm = Durable.create ~name:"rs" () in
  let sm = Durable.add_step sm step in
  let state = Durable.Suspended {
    at_step = "step1"; journal = []; reason = "paused";
  } in
  match Durable.resume sm state with
  | Completed _ -> check bool "step was called" true (!call_count > 0)
  | _ -> fail "expected Completed after resuming from Suspended"

let test_durable_multi_journal_roundtrip () =
  let make_je name attempt : Durable.journal_entry = {
    step_name = name;
    started_at = float_of_int attempt *. 100.0;
    completed_at = Some (float_of_int attempt *. 100.0 +. 50.0);
    input_json = `Int attempt;
    output_json = Some (`Int (attempt + 1));
    error = None; attempt;
  } in
  let state = Durable.Completed {
    journal = [make_je "s1" 1; make_je "s2" 2; make_je "s3" 3];
    final_output = `Int 4;
  } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Completed { journal; _ }) ->
    check int "3 entries" 3 (List.length journal)
  | _ -> fail "multi-journal roundtrip"

let test_durable_serialization_journal_with_output () =
  let je : Durable.journal_entry = {
    step_name = "s1"; started_at = 100.0;
    completed_at = Some 200.0;
    input_json = `Assoc [("a", `Bool true)];
    output_json = Some (`List [`Int 1; `Int 2]);
    error = None; attempt = 1;
  } in
  let state = Durable.Failed {
    at_step = "s2"; journal = [je]; error = "crash";
  } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Failed { journal = [j]; _ }) ->
    (match j.output_json with
     | Some (`List [_; _]) -> ()
     | _ -> fail "expected list output in journal")
  | _ -> fail "Failed with journal roundtrip"

(* ══════════════════════════════════════════════════════════════
   3. Event_forward — payload conversion coverage
   ══════════════════════════════════════════════════════════════ *)

let test_event_forward_all_event_types () =
  let ok_result : (Types.api_response, Error.sdk_error) result =
    Ok { id = "r1"; model = "m"; stop_reason = Types.EndTurn;
         content = []; usage = None; telemetry = None }
  in
  let tool_ok : Types.tool_result = Ok { content = "done" } in
  let ev p = Event_bus.mk_event p in
  let events : Event_bus.event list = [
    ev (AgentStarted { agent_name = "a"; task_id = "t1" });
    ev (AgentCompleted { agent_name = "a"; task_id = "t1";
                     result = ok_result; elapsed = 1.5 });
    ev (ToolCalled { agent_name = "a"; tool_name = "shell"; input = `String "ls" });
    ev (ToolCompleted { agent_name = "a"; tool_name = "shell"; output = tool_ok });
    ev (TurnStarted { agent_name = "a"; turn = 0 });
    ev (TurnCompleted { agent_name = "a"; turn = 0 });
    ev (ElicitationCompleted { agent_name = "a"; question = "continue?";
                           response = Hooks.Declined });
    ev (TaskStateChanged { task_id = "t1"; from_state = "submitted"; to_state = "working" });
    ev (Custom ("my_event", `Assoc [("key", `String "val")]));
  ] in
  List.iter (fun event ->
    let type_name = Event_forward.event_type_name event in
    check bool "non-empty type" true (String.length type_name > 0);
    let _agent = Event_forward.agent_name_of_event event in
    let payload = Event_forward.event_to_payload event in
    check string "type matches" type_name payload.event_type;
    let json = Event_forward.payload_to_json payload in
    let open Yojson.Safe.Util in
    ignore (json |> member "event_type" |> to_string);
    ignore (json |> member "data");
    ()
  ) events

let test_event_forward_agent_name_none_cases () =
  let ev1 = Event_bus.mk_event (TaskStateChanged {
    task_id = "t"; from_state = "s1"; to_state = "s2" }) in
  let ev2 = Event_bus.mk_event (Custom ("x", `Null)) in
  check (option string) "TaskStateChanged agent" None
    (Event_forward.agent_name_of_event ev1);
  check (option string) "Custom agent" None
    (Event_forward.agent_name_of_event ev2)

let test_event_forward_agent_name_some_cases () =
  let ev p = Event_bus.mk_event p in
  let events = [
    ev (Event_bus.AgentStarted { agent_name = "a"; task_id = "t" });
    ev (Event_bus.ToolCalled { agent_name = "c"; tool_name = "t"; input = `Null });
    ev (Event_bus.TurnStarted { agent_name = "e"; turn = 0 });
    ev (Event_bus.TurnCompleted { agent_name = "f"; turn = 0 });
  ] in
  List.iter (fun event ->
    match Event_forward.agent_name_of_event event with
    | Some _ -> ()
    | None -> fail "expected Some agent_name"
  ) events

let test_event_forward_payload_no_agent () =
  let p : Event_forward.event_payload = {
    event_type = "test"; timestamp = 0.0;
    agent_name = None; correlation_id = "c1"; run_id = "r1"; data = `Null;
  } in
  let json = Event_forward.payload_to_json p in
  let keys = match json with
    | `Assoc pairs -> List.map fst pairs
    | _ -> []
  in
  check bool "no agent_name key" false (List.mem "agent_name" keys)

let test_event_forward_payload_with_agent () =
  let p : Event_forward.event_payload = {
    event_type = "test"; timestamp = 0.0;
    agent_name = Some "alice"; correlation_id = "c1"; run_id = "r1"; data = `Null;
  } in
  let json = Event_forward.payload_to_json p in
  let keys = match json with
    | `Assoc pairs -> List.map fst pairs
    | _ -> []
  in
  check bool "has agent_name key" true (List.mem "agent_name" keys)

let test_event_forward_counters () =
  let fwd = Event_forward.create ~targets:[] () in
  check int "initial delivered" 0 (Event_forward.delivered_count fwd);
  check int "initial failed" 0 (Event_forward.failed_count fwd)

let test_event_forward_custom_target_inline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let count = ref 0 in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create
    ~targets:[Custom_target {
      name = "counter"; deliver = (fun _ -> incr count);
    }]
    ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus (Event_bus.mk_event (Custom ("ping", `Null)));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  check bool "received" true (!count > 0);
  check bool "delivered > 0" true (Event_forward.delivered_count fwd > 0)

let test_event_forward_stop_idempotent () =
  let fwd = Event_forward.create ~targets:[] () in
  Event_forward.stop fwd;
  Event_forward.stop fwd;
  check int "delivered" 0 (Event_forward.delivered_count fwd)

let test_event_forward_start_idempotent () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bus = Event_bus.create () in
  let fwd = Event_forward.create ~targets:[] () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_forward.stop fwd;
  Eio.Fiber.yield ()

let test_event_forward_file_append () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "oas_deep_fwd_" ".jsonl" in
  at_exit (fun () -> try Sys.remove path with _ -> ());
  let bus = Event_bus.create () in
  let fwd = Event_forward.create
    ~targets:[File_append { path }] ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (Event_bus.mk_event
       (TurnStarted { agent_name = "a"; turn = 1 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  let content = In_channel.with_open_text path In_channel.input_all in
  check bool "file has content" true (String.length content > 0);
  let lines = String.split_on_char '\n' content
    |> List.filter (fun l -> String.length (String.trim l) > 0) in
  List.iter (fun line -> ignore (Yojson.Safe.from_string line)) lines

let test_event_forward_custom_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bus = Event_bus.create () in
  let fwd = Event_forward.create
    ~targets:[Custom_target {
      name = "failing"; deliver = (fun _ -> failwith "delivery error");
    }]
    ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus (Event_bus.mk_event (Custom ("test", `Null)));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  check bool "failed > 0" true (Event_forward.failed_count fwd > 0)

let test_event_forward_batch_size () =
  let fwd = Event_forward.create ~targets:[] ~batch_size:50
    ~flush_interval_s:5.0 () in
  check int "delivered" 0 (Event_forward.delivered_count fwd);
  check int "failed" 0 (Event_forward.failed_count fwd)

(* ══════════════════════════════════════════════════════════════
   4. Transport — default_options
   ══════════════════════════════════════════════════════════════ *)

let test_transport_default_options () =
  let opts = Transport.default_options in
  check (option string) "runtime_path" None opts.runtime_path;
  check (option string) "session_root" None opts.session_root;
  check (option string) "provider" None opts.provider;
  check (option string) "model" None opts.model;
  check (option string) "permission_mode" None opts.permission_mode;
  check bool "include_partial" false opts.include_partial_messages;
  check (list string) "setting_sources" [] opts.setting_sources;
  check (option string) "resume_session" None opts.resume_session;
  check (option string) "cwd" None opts.cwd

let test_transport_options_construction () =
  let opts : Transport.options = {
    runtime_path = Some "/usr/bin/oas_runtime";
    session_root = Some "/tmp/sessions";
    provider = Some "anthropic";
    model = Some "claude-sonnet-4-6";
    permission_mode = Some "default";
    include_partial_messages = true;
    setting_sources = ["user"; "project"];
    resume_session = Some "session-123";
    cwd = Some "/home/user";
  } in
  check (option string) "runtime_path" (Some "/usr/bin/oas_runtime") opts.runtime_path;
  check bool "include_partial" true opts.include_partial_messages;
  check int "setting_sources" 2 (List.length opts.setting_sources)

let test_transport_options_partial () =
  let opts : Transport.options = {
    Transport.default_options with
    provider = Some "openai";
    model = Some "gpt-4";
    include_partial_messages = true;
  } in
  check (option string) "provider" (Some "openai") opts.provider;
  check (option string) "model" (Some "gpt-4") opts.model;
  check bool "partial" true opts.include_partial_messages;
  check (option string) "runtime_path still None" None opts.runtime_path;

  check (list string) "setting_sources still empty" [] opts.setting_sources

(* ══════════════════════════════════════════════════════════════
   5. Builder — validation edge cases
   ══════════════════════════════════════════════════════════════ *)

let with_net f =
  Eio_main.run @@ fun env ->
  f (Eio.Stdenv.net env)

let test_builder_safe_max_turns_zero () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_turns 0
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "max_turns" field
  | _ -> fail "expected Config error for max_turns=0"

let test_builder_safe_max_turns_negative () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_turns (-1)
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "max_turns" field
  | _ -> fail "expected Config error for max_turns=-1"

let test_builder_safe_max_tokens_zero () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_tokens 0
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "max_tokens" field
  | _ -> fail "expected Config error for max_tokens=0"

let test_builder_safe_thinking_budget_without_enable () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_thinking_budget 1024
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "thinking_budget" field
  | _ -> fail "expected Config error"

let test_builder_safe_thinking_budget_with_enable_false () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_enable_thinking false
    |> Builder.with_thinking_budget 1024
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "thinking_budget" field
  | _ -> fail "expected Config error"

let test_builder_safe_negative_cost () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_cost_usd (-1.0)
    |> Builder.build_safe with
  | Error (Config (InvalidConfig { field; _ })) ->
    check string "field" "max_cost_usd" field
  | _ -> fail "expected Config error for negative max_cost_usd"

let test_builder_chaining () =
  with_net @@ fun net ->
  let tool = Tool.create ~name:"echo" ~description:"echo"
    ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  let result =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_name "chained"
    |> Builder.with_system_prompt "You are a test."
    |> Builder.with_max_tokens 2048
    |> Builder.with_max_turns 5
    |> Builder.with_temperature 0.7
    |> Builder.with_top_p 0.9
    |> Builder.with_top_k 40
    |> Builder.with_min_p 0.05
    |> Builder.with_enable_thinking true
    |> Builder.with_thinking_budget 4096
    |> Builder.with_disable_parallel_tool_use true
    |> Builder.with_response_format_json true
    |> Builder.with_cache_system_prompt true
    |> Builder.with_max_input_tokens 100000
    |> Builder.with_max_total_tokens 200000
    |> Builder.with_max_cost_usd 1.0
    |> Builder.with_tool tool
    |> Builder.with_max_idle_turns 5
    |> Builder.with_description "test agent"
    |> Builder.with_base_url "http://localhost:8080"
    |> Builder.build_safe
  in
  match result with
  | Ok agent ->
    let state = Agent.state agent in
    check string "name" "chained" state.config.name;
    check int "max_tokens" 2048 state.config.max_tokens;
    check int "max_turns" 5 state.config.max_turns;
    check bool "thinking" true (state.config.enable_thinking = Some true)
  | Error e ->
    fail (Printf.sprintf "build_safe failed: %s" (Error.to_string e))

let test_builder_with_fallback_no_provider () =
  with_net @@ fun net ->
  let fallback = Provider.local_llm () in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_fallback fallback
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "with_fallback: %s" (Error.to_string e))

let test_builder_with_fallback_existing_cascade () =
  with_net @@ fun net ->
  let primary = Provider.anthropic_sonnet () in
  let fb1 = Provider.local_llm () in
  let fb2 = Provider.custom_provider ~name:"test" () in
  let cascade = Provider.cascade ~primary ~fallbacks:[fb1] in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_cascade cascade
    |> Builder.with_fallback fb2
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "cascade+fallback: %s" (Error.to_string e))

let test_builder_with_contract () =
  with_net @@ fun net ->
  let skill = Skill.of_markdown "# Review\nReview code" in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_skill skill
    |> Builder.with_tool_grants ["shell"; "read"]
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "contract: %s" (Error.to_string e))

let test_builder_with_skills_list () =
  with_net @@ fun net ->
  let s1 = Skill.of_markdown "# Review\nreview" in
  let s2 = Skill.of_markdown "# Deploy\ndeploy" in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_skills [s1; s2]
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "skills: %s" (Error.to_string e))

let test_builder_with_mcp_tool_allowlist () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_mcp_tool_allowlist ["tool_a"; "tool_b"]
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "allowlist: %s" (Error.to_string e))


let test_builder_with_periodic_callback () =
  with_net @@ fun net ->
  let cb : Agent.periodic_callback = { interval_sec = 30.0; callback = (fun () -> ()) } in
  let cb2 : Agent.periodic_callback = { interval_sec = 60.0; callback = (fun () -> ()) } in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_periodic_callback cb
    |> Builder.with_periodic_callbacks [cb2]
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "periodic: %s" (Error.to_string e))

let test_builder_with_tools_list () =
  with_net @@ fun net ->
  let t1 = Tool.create ~name:"t1" ~description:"d1" ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  let t2 = Tool.create ~name:"t2" ~description:"d2" ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_tools [t1; t2]
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "tools: %s" (Error.to_string e))

let test_builder_with_initial_messages () =
  with_net @@ fun net ->
  let msgs : Types.message list = [{ role = User; content = [Text "Hello"]; name = None; tool_call_id = None }] in
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_initial_messages msgs
    |> Builder.build_safe with
  | Ok agent ->
    let state = Agent.state agent in
    check int "initial messages" 1 (List.length state.config.initial_messages)
  | Error e -> fail (Printf.sprintf "initial_messages: %s" (Error.to_string e))

let test_builder_valid_thinking_budget () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_enable_thinking true
    |> Builder.with_thinking_budget 4096
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "valid thinking: %s" (Error.to_string e))

let test_builder_zero_cost () =
  with_net @@ fun net ->
  match Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_cost_usd 0.0
    |> Builder.build_safe with
  | Ok _ -> ()
  | Error e -> fail (Printf.sprintf "zero cost: %s" (Error.to_string e))

(* ══════════════════════════════════════════════════════════════
   6. A2a_task_store — task store via Eio filesystem
   ══════════════════════════════════════════════════════════════ *)

let cleanup_dir tmpdir =
  try
    Array.iter (fun f -> Sys.remove (Filename.concat tmpdir f)) (Sys.readdir tmpdir);
    Sys.rmdir tmpdir
  with _ -> ()

let test_a2a_task_store_lifecycle () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_store_" "" in
  at_exit (fun () -> cleanup_dir tmpdir);
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    let msg : A2a_task.task_message = { role = TaskUser; parts = [Text_part "hello"]; metadata = [] } in
    let task = A2a_task.create msg in
    let task_id = task.id in
    (match A2a_task_store.store_task store task with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    (match A2a_task_store.get_task store task_id with
     | Some t -> check string "id match" task_id t.id
     | None -> fail "task not found after store");
    let tasks = A2a_task_store.list_tasks store in
    check bool "has tasks" true (List.length tasks > 0);
    (match A2a_task_store.delete_task store task_id with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    (match A2a_task_store.get_task store task_id with
     | None -> () | Some _ -> fail "task should be deleted")

let test_a2a_task_store_reload () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_reload_" "" in
  at_exit (fun () -> cleanup_dir tmpdir);
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    let msg : A2a_task.task_message = { role = TaskAgent; parts = [Text_part "response"]; metadata = [] } in
    let task = A2a_task.create msg in
    (match A2a_task_store.store_task store task with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    (match A2a_task_store.reload store with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    check bool "task exists after reload" true (Option.is_some (A2a_task_store.get_task store task.id))

let test_a2a_task_store_gc () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_gc_" "" in
  at_exit (fun () -> cleanup_dir tmpdir);
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    let msg : A2a_task.task_message = { role = TaskUser; parts = [Text_part "old"]; metadata = [] } in
    let task = A2a_task.create msg in
    let task = match A2a_task.transition task Working with Ok t -> t | Error _ -> task in
    let task = match A2a_task.transition task Completed with Ok t -> t | Error _ -> task in
    let old_task = { task with updated_at = 0.0 } in
    (match A2a_task_store.store_task store old_task with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    (match A2a_task_store.gc ~max_age_s:1.0 store with
     | Ok removed -> check bool "removed >= 1" true (removed >= 1)
     | Error _ -> ())

let test_a2a_task_store_invalid_ids () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_invalid_" "" in
  at_exit (fun () -> try Sys.rmdir tmpdir with _ -> ());
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    (match A2a_task_store.delete_task store "" with
     | Error _ -> () | Ok () -> fail "expected error on empty task_id");
    (match A2a_task_store.delete_task store "a/b" with
     | Error _ -> () | Ok () -> fail "expected error on slash");
    (match A2a_task_store.delete_task store "a\000b" with
     | Error _ -> () | Ok () -> fail "expected error on null")

let test_a2a_task_store_get_nonexistent () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_nonex_" "" in
  at_exit (fun () -> try Sys.rmdir tmpdir with _ -> ());
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    check (option string) "nonexistent" None
      (match A2a_task_store.get_task store "does-not-exist" with
       | None -> None | Some t -> Some t.id)

let test_a2a_task_store_overwrite () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_ow_" "" in
  at_exit (fun () -> cleanup_dir tmpdir);
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    let msg : A2a_task.task_message = { role = TaskUser; parts = [Text_part "v1"]; metadata = [] } in
    let task = A2a_task.create msg in
    (match A2a_task_store.store_task store task with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    let task2 = match A2a_task.transition task Working with Ok t -> t | Error _ -> task in
    (match A2a_task_store.store_task store task2 with
     | Ok () -> () | Error e -> fail (Error.to_string e));
    (match A2a_task_store.get_task store task.id with
     | Some t -> check string "state" "working" (A2a_task.task_state_to_string t.state)
     | None -> fail "task disappeared")

let test_a2a_task_store_list_empty () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let tmpdir = Filename.temp_dir "oas_test_empty_" "" in
  at_exit (fun () -> try Sys.rmdir tmpdir with _ -> ());
  let base_dir = Eio.Path.(fs / tmpdir) in
  match A2a_task_store.create base_dir with
  | Error e -> fail (Printf.sprintf "create: %s" (Error.to_string e))
  | Ok store ->
    check int "empty list" 0 (List.length (A2a_task_store.list_tasks store))

(* ══════════════════════════════════════════════════════════════
   Suite
   ══════════════════════════════════════════════════════════════ *)

let () =
  run "deep_coverage" [
    "sessions.trace_capability", [
      test_case "all variants roundtrip" `Quick test_trace_capability_all_variants;
      test_case "invalid string" `Quick test_trace_capability_invalid;
      test_case "wire names" `Quick test_trace_capability_json_values;
    ];
    "sessions.worker_status", [
      test_case "all variants roundtrip" `Quick test_worker_status_all_variants;
      test_case "wire names" `Quick test_worker_status_wire_names;
      test_case "invalid string" `Quick test_worker_status_invalid;
    ];
    "sessions.session_info", [
      test_case "all fields" `Quick test_session_info_all_fields;
      test_case "None optionals" `Quick test_session_info_none_optionals;
      test_case "all phases" `Quick test_session_info_all_phases;
    ];
    "sessions.telemetry", [
      test_case "event_count" `Quick test_telemetry_event_count;
      test_case "step Some" `Quick test_telemetry_step_some;
      test_case "step None" `Quick test_telemetry_step_none;
      test_case "full telemetry" `Quick test_telemetry_full;
      test_case "empty lists" `Quick test_telemetry_empty_lists;
    ];
    "sessions.structured", [
      test_case "event_count" `Quick test_structured_event_count;
      test_case "step all Some" `Quick test_structured_telemetry_step_all_some;
      test_case "step all None" `Quick test_structured_telemetry_step_all_none;
      test_case "full structured" `Quick test_structured_telemetry_full;
    ];
    "sessions.evidence", [
      test_case "evidence_file" `Quick test_evidence_file_roundtrip;
      test_case "missing_file" `Quick test_missing_file_roundtrip;
      test_case "evidence full" `Quick test_evidence_full;
      test_case "evidence empty lists" `Quick test_evidence_empty_lists;
    ];
    "sessions.hook_summary", [
      test_case "all Some" `Quick test_hook_summary_all_some;
      test_case "all None" `Quick test_hook_summary_all_none;
    ];
    "sessions.tool_contract", [
      test_case "full" `Quick test_tool_contract_full;
      test_case "minimal" `Quick test_tool_contract_minimal;
    ];
    "sessions.evidence_capabilities", [
      test_case "all true" `Quick test_evidence_capabilities_all_true;
      test_case "all false" `Quick test_evidence_capabilities_all_false;
    ];
    "sessions.worker_run", [
      test_case "minimal" `Quick test_worker_run_minimal;
      test_case "fully populated" `Quick test_worker_run_fully_populated;
      test_case "each status" `Quick test_worker_run_each_status;
      test_case "each trace" `Quick test_worker_run_each_trace;
    ];
    "sessions.show", [
      test_case "all show functions" `Quick test_show_functions;
    ];
    "durable.journal", [
      test_case "entry with error" `Quick test_durable_journal_entry_roundtrip;
      test_case "entry no completed_at" `Quick test_durable_journal_entry_no_completed;
      test_case "multi journal roundtrip" `Quick test_durable_multi_journal_roundtrip;
      test_case "journal with output" `Quick test_durable_serialization_journal_with_output;
    ];
    "durable.edge_cases", [
      test_case "unknown state string" `Quick test_durable_unknown_state_string;
      test_case "resume unknown step" `Quick test_durable_resume_unknown_step;
      test_case "resume InProgress noop" `Quick test_durable_resume_in_progress_noop;
      test_case "suspend Completed noop" `Quick test_durable_suspend_completed_noop;
      test_case "suspend Failed noop" `Quick test_durable_suspend_failed_noop;
      test_case "suspend Suspended noop" `Quick test_durable_suspend_suspended_noop;
      test_case "resume from Suspended" `Quick test_durable_resume_suspended;
    ];
    "event_forward.payloads", [
      test_case "all event types" `Quick test_event_forward_all_event_types;
      test_case "agent_name None cases" `Quick test_event_forward_agent_name_none_cases;
      test_case "agent_name Some cases" `Quick test_event_forward_agent_name_some_cases;
      test_case "payload no agent" `Quick test_event_forward_payload_no_agent;
      test_case "payload with agent" `Quick test_event_forward_payload_with_agent;
      test_case "counters" `Quick test_event_forward_counters;
      test_case "batch_size param" `Quick test_event_forward_batch_size;
    ];
    "event_forward.targets", [
      test_case "custom target inline" `Quick test_event_forward_custom_target_inline;
      test_case "custom target failure" `Quick test_event_forward_custom_failure;
      test_case "stop idempotent" `Quick test_event_forward_stop_idempotent;
      test_case "start idempotent" `Quick test_event_forward_start_idempotent;
      test_case "file append" `Quick test_event_forward_file_append;
    ];
    "transport.options", [
      test_case "default_options" `Quick test_transport_default_options;
      test_case "options construction" `Quick test_transport_options_construction;
      test_case "options partial override" `Quick test_transport_options_partial;
    ];
    "builder.validation", [
      test_case "max_turns zero" `Quick test_builder_safe_max_turns_zero;
      test_case "max_turns negative" `Quick test_builder_safe_max_turns_negative;
      test_case "max_tokens zero" `Quick test_builder_safe_max_tokens_zero;
      test_case "thinking without enable" `Quick test_builder_safe_thinking_budget_without_enable;
      test_case "thinking with false" `Quick test_builder_safe_thinking_budget_with_enable_false;
      test_case "negative cost" `Quick test_builder_safe_negative_cost;
      test_case "valid thinking" `Quick test_builder_valid_thinking_budget;
      test_case "zero cost" `Quick test_builder_zero_cost;
    ];
    "builder.chaining", [
      test_case "full chain" `Quick test_builder_chaining;
      test_case "fallback no provider" `Quick test_builder_with_fallback_no_provider;
      test_case "fallback existing cascade" `Quick test_builder_with_fallback_existing_cascade;
      test_case "contract/skill" `Quick test_builder_with_contract;
      test_case "skills list" `Quick test_builder_with_skills_list;
      test_case "mcp tool allowlist" `Quick test_builder_with_mcp_tool_allowlist;
      test_case "periodic callback" `Quick test_builder_with_periodic_callback;
      test_case "tools list" `Quick test_builder_with_tools_list;
      test_case "initial messages" `Quick test_builder_with_initial_messages;
    ];
    "a2a_task_store", [
      test_case "lifecycle" `Quick test_a2a_task_store_lifecycle;
      test_case "reload" `Quick test_a2a_task_store_reload;
      test_case "gc" `Quick test_a2a_task_store_gc;
      test_case "invalid ids" `Quick test_a2a_task_store_invalid_ids;
      test_case "get nonexistent" `Quick test_a2a_task_store_get_nonexistent;
      test_case "overwrite" `Quick test_a2a_task_store_overwrite;
      test_case "list empty" `Quick test_a2a_task_store_list_empty;
    ];
  ]
