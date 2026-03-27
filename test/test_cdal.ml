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
  ]
