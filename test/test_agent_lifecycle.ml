(** Tests for Agent_lifecycle module — snapshot building, runtime names. *)

open Agent_sdk

(* ── build_snapshot tests ─────────────────────────────────── *)

let test_build_snapshot_fresh () =
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:Types.Claude_sonnet_4_6
    Agent_lifecycle.Running
  in
  Alcotest.(check string) "agent_name" "test-agent" snap.agent_name;
  Alcotest.(check bool) "status running" true
    (snap.status = Agent_lifecycle.Running);
  Alcotest.(check (option string)) "no run_id" None snap.current_run_id;
  Alcotest.(check (option (float 0.001))) "no accepted_at" None snap.accepted_at;
  Alcotest.(check (option (float 0.001))) "no finished_at" None snap.finished_at

let test_build_snapshot_merge_previous () =
  let prev = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:Types.Claude_sonnet_4_6
    ~accepted_at:100.0
    ~started_at:101.0
    Agent_lifecycle.Running
  in
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:Types.Claude_sonnet_4_6
    ~previous:prev
    ~finished_at:200.0
    Agent_lifecycle.Completed
  in
  Alcotest.(check (option (float 0.001))) "accepted_at preserved"
    (Some 100.0) snap.accepted_at;
  Alcotest.(check (option (float 0.001))) "started_at preserved"
    (Some 101.0) snap.started_at;
  Alcotest.(check (option (float 0.001))) "finished_at set"
    (Some 200.0) snap.finished_at

let test_build_snapshot_last_error () =
  let prev = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:Types.Claude_sonnet_4_6
    ~last_error:"previous error"
    Agent_lifecycle.Failed
  in
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:Types.Claude_sonnet_4_6
    ~previous:prev
    Agent_lifecycle.Running
  in
  Alcotest.(check (option string)) "last_error carried"
    (Some "previous error") snap.last_error

let test_build_snapshot_with_provider () =
  let provider : Provider.config = {
    provider = Anthropic;
    model_id = "claude-sonnet-4-6";
    api_key_env = "TEST";
  } in
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:(Some provider)
    ~model:Types.Claude_sonnet_4_6
    Agent_lifecycle.Accepted
  in
  Alcotest.(check (option string)) "requested_provider"
    (Some "anthropic") snap.requested_provider;
  Alcotest.(check (option string)) "resolved_model"
    (Some "claude-sonnet-4-6") snap.resolved_model

(* ── provider_runtime_name tests ──────────────────────────── *)

let test_runtime_name_none () =
  Alcotest.(check (option string)) "None provider" None
    (Agent_lifecycle.provider_runtime_name None)

let test_runtime_name_local () =
  let cfg : Provider.config = {
    provider = Local { base_url = "http://localhost:8085" };
    model_id = "test"; api_key_env = "DUMMY";
  } in
  Alcotest.(check (option string)) "local" (Some "local")
    (Agent_lifecycle.provider_runtime_name (Some cfg))

let test_runtime_name_anthropic () =
  let cfg : Provider.config = {
    provider = Anthropic; model_id = "test"; api_key_env = "DUMMY";
  } in
  Alcotest.(check (option string)) "anthropic" (Some "anthropic")
    (Agent_lifecycle.provider_runtime_name (Some cfg))

let test_runtime_name_openai_compat () =
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "http://localhost"; auth_header = None;
      path = "/v1/chat"; static_token = None;
    };
    model_id = "test"; api_key_env = "DUMMY";
  } in
  Alcotest.(check (option string)) "openai-compat" (Some "openai-compat")
    (Agent_lifecycle.provider_runtime_name (Some cfg))

let test_runtime_name_custom () =
  let cfg : Provider.config = {
    provider = Custom_registered { name = "my-custom" };
    model_id = "test"; api_key_env = "DUMMY";
  } in
  Alcotest.(check (option string)) "custom" (Some "custom:my-custom")
    (Agent_lifecycle.provider_runtime_name (Some cfg))

(* ── hook_decision_to_string tests ────────────────────────── *)

let test_hook_decision_strings () =
  Alcotest.(check string) "continue" "continue"
    (Agent_lifecycle.hook_decision_to_string Hooks.Continue);
  Alcotest.(check string) "skip" "skip"
    (Agent_lifecycle.hook_decision_to_string Hooks.Skip)

(* ── Test runner ────────────────────────────────────────── *)

let () =
  Alcotest.run "Agent_lifecycle" [
    "build_snapshot", [
      Alcotest.test_case "fresh snapshot" `Quick test_build_snapshot_fresh;
      Alcotest.test_case "merge previous" `Quick test_build_snapshot_merge_previous;
      Alcotest.test_case "last_error carry" `Quick test_build_snapshot_last_error;
      Alcotest.test_case "with provider" `Quick test_build_snapshot_with_provider;
    ];
    "provider_runtime_name", [
      Alcotest.test_case "None" `Quick test_runtime_name_none;
      Alcotest.test_case "Local" `Quick test_runtime_name_local;
      Alcotest.test_case "Anthropic" `Quick test_runtime_name_anthropic;
      Alcotest.test_case "OpenAICompat" `Quick test_runtime_name_openai_compat;
      Alcotest.test_case "Custom_registered" `Quick test_runtime_name_custom;
    ];
    "hook_decision", [
      Alcotest.test_case "string conversion" `Quick test_hook_decision_strings;
    ];
  ]
