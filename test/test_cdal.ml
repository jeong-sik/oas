open Agent_sdk

(* ================================================================ *)
(* Execution Mode tests                                              *)
(* ================================================================ *)

let test_execution_mode_to_string () =
  Alcotest.(check string) "diagnose" "diagnose"
    (Execution_mode.to_string Diagnose);
  Alcotest.(check string) "draft" "draft"
    (Execution_mode.to_string Draft);
  Alcotest.(check string) "execute" "execute"
    (Execution_mode.to_string Execute)

let test_execution_mode_of_string () =
  Alcotest.(check (result (of_pp Execution_mode.pp) string))
    "parse diagnose" (Ok Execution_mode.Diagnose)
    (Execution_mode.of_string "diagnose");
  Alcotest.(check (result (of_pp Execution_mode.pp) string))
    "parse execute" (Ok Execution_mode.Execute)
    (Execution_mode.of_string "execute");
  Alcotest.(check bool) "invalid" true
    (Result.is_error (Execution_mode.of_string "unknown"))

let test_can_serve_truth_table () =
  let open Execution_mode in
  (* effective <= requested is valid *)
  Alcotest.(check bool) "D<=D" true (can_serve ~requested:Diagnose ~effective:Diagnose);
  Alcotest.(check bool) "D<=Dr" false (can_serve ~requested:Diagnose ~effective:Draft);
  Alcotest.(check bool) "D<=E" false (can_serve ~requested:Diagnose ~effective:Execute);
  Alcotest.(check bool) "Dr<=D" true (can_serve ~requested:Draft ~effective:Diagnose);
  Alcotest.(check bool) "Dr<=Dr" true (can_serve ~requested:Draft ~effective:Draft);
  Alcotest.(check bool) "Dr<=E" false (can_serve ~requested:Draft ~effective:Execute);
  Alcotest.(check bool) "E<=D" true (can_serve ~requested:Execute ~effective:Diagnose);
  Alcotest.(check bool) "E<=Dr" true (can_serve ~requested:Execute ~effective:Draft);
  Alcotest.(check bool) "E<=E" true (can_serve ~requested:Execute ~effective:Execute)

(* ================================================================ *)
(* Risk Class tests                                                  *)
(* ================================================================ *)

let test_risk_class_max_mode () =
  let open Risk_class in
  Alcotest.(check (option (of_pp Execution_mode.pp)))
    "low" (Some Execution_mode.Execute) (max_mode Low);
  Alcotest.(check (option (of_pp Execution_mode.pp)))
    "medium" (Some Execution_mode.Execute) (max_mode Medium);
  Alcotest.(check (option (of_pp Execution_mode.pp)))
    "high" (Some Execution_mode.Draft) (max_mode High);
  Alcotest.(check (option (of_pp Execution_mode.pp)))
    "critical" None (max_mode Critical)

(* ================================================================ *)
(* Risk Contract tests                                               *)
(* ================================================================ *)

let make_contract ?(mode = Execution_mode.Draft) ?(risk = Risk_class.Medium) () =
  Risk_contract.{
    runtime_constraints = {
      requested_execution_mode = mode;
      risk_class = risk;
      allowed_mutations = ["workspace_only"];
      review_requirement = Some "human_if_execute";
    };
    eval_criteria = `Assoc [
      "success_criteria", `List [`String "tests pass"];
    ];
  }

let test_contract_id_deterministic () =
  let c1 = make_contract () in
  let c2 = make_contract () in
  Alcotest.(check string) "same contract same id"
    (Risk_contract.contract_id c1)
    (Risk_contract.contract_id c2)

let test_contract_id_sensitive () =
  let c1 = make_contract ~mode:Execution_mode.Draft () in
  let c2 = make_contract ~mode:Execution_mode.Execute () in
  Alcotest.(check bool) "different mode different id" true
    (Risk_contract.contract_id c1 <> Risk_contract.contract_id c2)

let test_contract_id_prefix () =
  let c = make_contract () in
  let id = Risk_contract.contract_id c in
  Alcotest.(check bool) "md5: prefix" true
    (String.length id > 4 && String.sub id 0 4 = "md5:")

(* ================================================================ *)
(* Mode Resolver tests                                               *)
(* ================================================================ *)

let test_caps : Cdal_proof.capability_snapshot = {
  tools = ["bash"; "edit"];
  mcp_servers = [];
  max_turns = 10;
  max_tokens = Some 4096;
  thinking_enabled = None;
}

let test_mode_resolver_passthrough () =
  let result = Mode_resolver.resolve
      ~requested:Execution_mode.Draft
      ~risk_class:Risk_class.Low
      ~capabilities:test_caps in
  match result with
  | Ok d ->
    Alcotest.(check string) "mode" "draft" (Execution_mode.to_string d.effective_mode);
    Alcotest.(check string) "source" "passthrough" d.source
  | Error e -> Alcotest.fail e

let test_mode_resolver_high_downgrade () =
  let result = Mode_resolver.resolve
      ~requested:Execution_mode.Execute
      ~risk_class:Risk_class.High
      ~capabilities:test_caps in
  match result with
  | Ok d ->
    Alcotest.(check string) "mode" "draft" (Execution_mode.to_string d.effective_mode);
    Alcotest.(check string) "source" "risk_class_downgrade" d.source
  | Error e -> Alcotest.fail e

let test_mode_resolver_critical_reject () =
  let result = Mode_resolver.resolve
      ~requested:Execution_mode.Execute
      ~risk_class:Risk_class.Critical
      ~capabilities:test_caps in
  Alcotest.(check bool) "critical rejects" true (Result.is_error result)

let test_mode_resolver_downgrade_only () =
  (* High risk + Diagnose requested = still Diagnose (no upgrade) *)
  let result = Mode_resolver.resolve
      ~requested:Execution_mode.Diagnose
      ~risk_class:Risk_class.High
      ~capabilities:test_caps in
  match result with
  | Ok d ->
    Alcotest.(check string) "stays diagnose"
      "diagnose" (Execution_mode.to_string d.effective_mode)
  | Error e -> Alcotest.fail e

(* ================================================================ *)
(* Cdal Proof JSON round-trip tests                                  *)
(* ================================================================ *)

let test_cdal_proof_roundtrip () =
  let proof : Cdal_proof.t = {
    schema_version = 1;
    run_id = "test-run-001";
    contract_id = "md5:abc123";
    requested_execution_mode = Execution_mode.Draft;
    effective_execution_mode = Execution_mode.Diagnose;
    mode_decision_source = "risk_class_downgrade";
    risk_class = Risk_class.High;
    provider_snapshot = {
      provider_name = "anthropic";
      model_id = "claude-sonnet";
      api_version = Some "2025-01-01";
    };
    capability_snapshot = test_caps;
    tool_trace_refs = ["proof-store://test-run-001/tool_traces/trace-0001.jsonl"];
    raw_evidence_refs = [];
    checkpoint_ref = None;
    result_status = Cdal_proof.Completed;
    started_at = 1000.0;
    ended_at = 1005.0;
  } in
  let json = Cdal_proof.to_json proof in
  match Cdal_proof.of_json json with
  | Ok decoded ->
    Alcotest.(check string) "run_id" proof.run_id decoded.run_id;
    Alcotest.(check string) "contract_id" proof.contract_id decoded.contract_id;
    Alcotest.(check int) "schema_version" 1 decoded.schema_version
  | Error e -> Alcotest.fail (Printf.sprintf "decode failed: %s" e)

(* ================================================================ *)
(* Hooks compose tests                                               *)
(* ================================================================ *)

let test_hooks_compose_outer_skip () =
  let outer = Hooks.{
    empty with
    pre_tool_use = Some (fun _ -> Skip);
  } in
  let inner_called = ref false in
  let inner = Hooks.{
    empty with
    pre_tool_use = Some (fun _ -> inner_called := true; Continue);
  } in
  let composed = Hooks.compose ~outer ~inner in
  let event = Hooks.PreToolUse {
    tool_name = "bash"; input = `Null;
    accumulated_cost_usd = 0.0; turn = 1;
  } in
  let decision = Hooks.invoke composed.pre_tool_use event in
  Alcotest.(check bool) "outer skip bypasses inner" false !inner_called;
  Alcotest.(check bool) "decision is skip" true
    (match decision with Hooks.Skip -> true | _ -> false)

let test_hooks_compose_both_continue () =
  let outer_called = ref false in
  let inner_called = ref false in
  let outer = Hooks.{
    empty with
    pre_tool_use = Some (fun _ -> outer_called := true; Continue);
  } in
  let inner = Hooks.{
    empty with
    pre_tool_use = Some (fun _ -> inner_called := true; Continue);
  } in
  let composed = Hooks.compose ~outer ~inner in
  let event = Hooks.PreToolUse {
    tool_name = "bash"; input = `Null;
    accumulated_cost_usd = 0.0; turn = 1;
  } in
  let _ = Hooks.invoke composed.pre_tool_use event in
  Alcotest.(check bool) "outer called" true !outer_called;
  Alcotest.(check bool) "inner called" true !inner_called

(* ================================================================ *)
(* Proof Store tests                                                 *)
(* ================================================================ *)

let test_proof_store_init_and_write () =
  let tmpdir = Filename.concat
      (Filename.get_temp_dir_name ()) "oas-test-proof-store" in
  let config : Proof_store.config = { root = tmpdir } in
  let run_id = "test-run-store-001" in
  Proof_store.init_run config ~run_id;
  let manifest_path = Proof_store.manifest_path config ~run_id in
  let proof : Cdal_proof.t = {
    schema_version = 1;
    run_id;
    contract_id = "md5:test123";
    requested_execution_mode = Execution_mode.Draft;
    effective_execution_mode = Execution_mode.Draft;
    mode_decision_source = "passthrough";
    risk_class = Risk_class.Low;
    provider_snapshot = {
      provider_name = "test"; model_id = "test-model"; api_version = None;
    };
    capability_snapshot = test_caps;
    tool_trace_refs = [];
    raw_evidence_refs = [];
    checkpoint_ref = None;
    result_status = Cdal_proof.Completed;
    started_at = 1000.0;
    ended_at = 1001.0;
  } in
  Proof_store.write_manifest config ~run_id proof;
  Alcotest.(check bool) "manifest exists" true (Sys.file_exists manifest_path);
  (* cleanup *)
  ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))

let test_proof_store_make_ref () =
  let ref_str = Proof_store.make_ref ~run_id:"r1" ~subpath:"tool_traces/t.jsonl" in
  Alcotest.(check string) "ref format"
    "proof-store://r1/tool_traces/t.jsonl" ref_str

(* ================================================================ *)
(* Contract ID edge cases                                            *)
(* ================================================================ *)

let test_contract_id_empty_eval_criteria () =
  let c = Risk_contract.{
    runtime_constraints = {
      requested_execution_mode = Execution_mode.Diagnose;
      risk_class = Risk_class.Low;
      allowed_mutations = [];
      review_requirement = None;
    };
    eval_criteria = `Null;
  } in
  let id = Risk_contract.contract_id c in
  Alcotest.(check bool) "non-empty id" true (String.length id > 4)

(* ================================================================ *)
(* JSON schema conformance -- enum values must be lowercase strings  *)
(* ================================================================ *)

let assert_json_string msg expected json =
  match json with
  | `String s -> Alcotest.(check string) msg expected s
  | other -> Alcotest.fail
      (Printf.sprintf "%s: expected string, got %s" msg
         (Yojson.Safe.to_string other))

let test_execution_mode_json_lowercase () =
  assert_json_string "diagnose" "diagnose" (Execution_mode.to_yojson Diagnose);
  assert_json_string "draft" "draft" (Execution_mode.to_yojson Draft);
  assert_json_string "execute" "execute" (Execution_mode.to_yojson Execute)

let test_risk_class_json_lowercase () =
  assert_json_string "low" "low" (Risk_class.to_yojson Low);
  assert_json_string "medium" "medium" (Risk_class.to_yojson Medium);
  assert_json_string "high" "high" (Risk_class.to_yojson High);
  assert_json_string "critical" "critical" (Risk_class.to_yojson Critical)

let test_result_status_json_lowercase () =
  assert_json_string "completed" "completed"
    (Cdal_proof.result_status_to_yojson Completed);
  assert_json_string "errored" "errored"
    (Cdal_proof.result_status_to_yojson Errored);
  assert_json_string "timed_out" "timed_out"
    (Cdal_proof.result_status_to_yojson Timed_out);
  assert_json_string "cancelled" "cancelled"
    (Cdal_proof.result_status_to_yojson Cancelled)

let test_proof_json_enum_fields () =
  let proof : Cdal_proof.t = {
    schema_version = 1;
    run_id = "test-enum";
    contract_id = "md5:abc";
    requested_execution_mode = Execution_mode.Execute;
    effective_execution_mode = Execution_mode.Draft;
    mode_decision_source = "risk_class_downgrade";
    risk_class = Risk_class.High;
    provider_snapshot = {
      provider_name = "test"; model_id = "m"; api_version = None;
    };
    capability_snapshot = test_caps;
    tool_trace_refs = [];
    raw_evidence_refs = [];
    checkpoint_ref = None;
    result_status = Cdal_proof.Completed;
    started_at = 1000.0;
    ended_at = 1001.0;
  } in
  let json = Cdal_proof.to_json proof in
  let field name = match json with
    | `Assoc fields -> List.assoc name fields
    | _ -> Alcotest.fail "proof json is not an object"
  in
  assert_json_string "requested mode in proof" "execute"
    (field "requested_execution_mode");
  assert_json_string "effective mode in proof" "draft"
    (field "effective_execution_mode");
  assert_json_string "risk class in proof" "high"
    (field "risk_class");
  assert_json_string "result status in proof" "completed"
    (field "result_status")

(* ================================================================ *)
(* Proof_capture integration tests                                   *)
(* ================================================================ *)

let test_contract = make_contract ~mode:Execution_mode.Draft ~risk:Risk_class.Medium ()

let test_mode_decision : Mode_resolver.decision = {
  effective_mode = Execution_mode.Draft;
  source = "passthrough";
}

let make_test_store () =
  let tmpdir = Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-test-capture-%d" (Random.bits () land 0xFFFF)) in
  (Proof_store.{ root = tmpdir }, tmpdir)

let mock_response model : Types.api_response = {
  id = "msg-test"; model;
  stop_reason = Types.EndTurn;
  content = [Types.Text "done"];
  usage = None;
}

let test_proof_capture_lifecycle () =
  let store, tmpdir = make_test_store () in
  let state = Proof_capture.create
      ~store ~contract:test_contract
      ~mode_decision:test_mode_decision
      ~capability_snapshot:test_caps in
  let h = Proof_capture.hooks state in
  (* Simulate: BeforeTurn 1, PreToolUse, PostToolUse, AfterTurn, OnStop *)
  let _ = Hooks.invoke h.before_turn
      (BeforeTurn { turn = 1; messages = [] }) in
  let _ = Hooks.invoke h.pre_tool_use
      (PreToolUse { tool_name = "bash"; input = `String "ls";
                    accumulated_cost_usd = 0.0; turn = 1 }) in
  let _ = Hooks.invoke h.post_tool_use
      (PostToolUse { tool_name = "bash"; input = `String "ls";
                     output = Ok { Types.content = "file.txt" };
                     result_bytes = 8 }) in
  let resp = mock_response "claude-test" in
  let _ = Hooks.invoke h.after_turn
      (AfterTurn { turn = 1; response = resp }) in
  let _ = Hooks.invoke h.on_stop
      (OnStop { reason = Types.EndTurn; response = resp }) in
  let proof = Proof_capture.finalize state ~result_status:Cdal_proof.Completed in
  (* Verify bundle fields *)
  Alcotest.(check int) "schema_version" 1 proof.schema_version;
  Alcotest.(check string) "mode source" "passthrough" proof.mode_decision_source;
  Alcotest.(check string) "model captured" "claude-test" proof.provider_snapshot.model_id;
  Alcotest.(check bool) "started_at > 0" true (proof.started_at > 0.0);
  Alcotest.(check bool) "ended_at >= started_at" true (proof.ended_at >= proof.started_at);
  Alcotest.(check int) "1 tool trace" 1 (List.length proof.tool_trace_refs);
  Alcotest.(check bool) "trace ref has prefix" true
    (String.length (List.hd proof.tool_trace_refs) > 15);
  (* Verify files on disk *)
  let manifest_path = Proof_store.manifest_path store ~run_id:proof.run_id in
  Alcotest.(check bool) "manifest written" true (Sys.file_exists manifest_path);
  ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))

let test_proof_capture_no_hooks_fired () =
  let store, tmpdir = make_test_store () in
  let state = Proof_capture.create
      ~store ~contract:test_contract
      ~mode_decision:test_mode_decision
      ~capability_snapshot:test_caps in
  (* Finalize without firing any hooks -- tests started_at guard *)
  let proof = Proof_capture.finalize state ~result_status:Cdal_proof.Cancelled in
  Alcotest.(check bool) "started_at > 0 (guarded)" true (proof.started_at > 0.0);
  Alcotest.(check bool) "ended_at > 0 (guarded)" true (proof.ended_at > 0.0);
  Alcotest.(check int) "0 tool traces" 0 (List.length proof.tool_trace_refs);
  Alcotest.(check string) "provider unknown" "unknown" proof.provider_snapshot.model_id;
  ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))

let test_proof_capture_multiple_tools () =
  let store, tmpdir = make_test_store () in
  let state = Proof_capture.create
      ~store ~contract:test_contract
      ~mode_decision:test_mode_decision
      ~capability_snapshot:test_caps in
  let h = Proof_capture.hooks state in
  let _ = Hooks.invoke h.before_turn
      (BeforeTurn { turn = 1; messages = [] }) in
  (* Tool 1: success *)
  let _ = Hooks.invoke h.pre_tool_use
      (PreToolUse { tool_name = "read"; input = `String "a.ml";
                    accumulated_cost_usd = 0.0; turn = 1 }) in
  let _ = Hooks.invoke h.post_tool_use
      (PostToolUse { tool_name = "read"; input = `String "a.ml";
                     output = Ok { Types.content = "code" };
                     result_bytes = 4 }) in
  (* Tool 2: failure *)
  let _ = Hooks.invoke h.pre_tool_use
      (PreToolUse { tool_name = "bash"; input = `String "rm /";
                    accumulated_cost_usd = 0.01; turn = 1 }) in
  let _ = Hooks.invoke h.post_tool_use_failure
      (PostToolUseFailure { tool_name = "bash"; input = `String "rm /";
                            error = "permission denied" }) in
  (* Tool 3: error result *)
  let _ = Hooks.invoke h.pre_tool_use
      (PreToolUse { tool_name = "edit"; input = `String "b.ml";
                    accumulated_cost_usd = 0.02; turn = 1 }) in
  let _ = Hooks.invoke h.post_tool_use
      (PostToolUse { tool_name = "edit"; input = `String "b.ml";
                     output = Error { Types.message = "conflict"; recoverable = true };
                     result_bytes = 0 }) in
  let proof = Proof_capture.finalize state ~result_status:Cdal_proof.Completed in
  Alcotest.(check int) "3 tool traces" 3 (List.length proof.tool_trace_refs);
  ignore (Sys.command (Printf.sprintf "rm -rf %s" tmpdir))

(* ================================================================ *)
(* Test runner                                                       *)
(* ================================================================ *)

let () =
  Alcotest.run "CDAL PoC-1" [
    "Execution_mode", [
      Alcotest.test_case "to_string" `Quick test_execution_mode_to_string;
      Alcotest.test_case "of_string" `Quick test_execution_mode_of_string;
      Alcotest.test_case "can_serve truth table" `Quick test_can_serve_truth_table;
    ];
    "Risk_class", [
      Alcotest.test_case "max_mode" `Quick test_risk_class_max_mode;
    ];
    "Risk_contract", [
      Alcotest.test_case "contract_id deterministic" `Quick test_contract_id_deterministic;
      Alcotest.test_case "contract_id sensitive" `Quick test_contract_id_sensitive;
      Alcotest.test_case "contract_id prefix" `Quick test_contract_id_prefix;
    ];
    "Mode_resolver", [
      Alcotest.test_case "passthrough" `Quick test_mode_resolver_passthrough;
      Alcotest.test_case "high downgrade" `Quick test_mode_resolver_high_downgrade;
      Alcotest.test_case "critical reject" `Quick test_mode_resolver_critical_reject;
      Alcotest.test_case "downgrade only" `Quick test_mode_resolver_downgrade_only;
    ];
    "Cdal_proof", [
      Alcotest.test_case "JSON round-trip" `Quick test_cdal_proof_roundtrip;
    ];
    "Hooks.compose", [
      Alcotest.test_case "outer skip bypasses inner" `Quick test_hooks_compose_outer_skip;
      Alcotest.test_case "both continue" `Quick test_hooks_compose_both_continue;
    ];
    "Proof_store", [
      Alcotest.test_case "init and write" `Quick test_proof_store_init_and_write;
      Alcotest.test_case "make_ref format" `Quick test_proof_store_make_ref;
    ];
    "Risk_contract edge", [
      Alcotest.test_case "empty eval_criteria" `Quick test_contract_id_empty_eval_criteria;
    ];
    "JSON schema conformance", [
      Alcotest.test_case "execution_mode lowercase" `Quick test_execution_mode_json_lowercase;
      Alcotest.test_case "risk_class lowercase" `Quick test_risk_class_json_lowercase;
      Alcotest.test_case "result_status lowercase" `Quick test_result_status_json_lowercase;
      Alcotest.test_case "proof bundle enum fields" `Quick test_proof_json_enum_fields;
    ];
    "Proof_capture", [
      Alcotest.test_case "full lifecycle" `Quick test_proof_capture_lifecycle;
      Alcotest.test_case "no hooks fired (guard)" `Quick test_proof_capture_no_hooks_fired;
      Alcotest.test_case "multiple tools" `Quick test_proof_capture_multiple_tools;
    ];
  ]
