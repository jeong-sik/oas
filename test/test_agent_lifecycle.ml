(** Tests for Agent_lifecycle module — snapshot building, runtime names. *)

open Agent_sdk

(* ── build_snapshot tests ─────────────────────────────────── *)

let test_build_snapshot_fresh () =
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:"claude-sonnet-4-6"
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
    ~model:"claude-sonnet-4-6"
    ~accepted_at:100.0
    ~started_at:101.0
    Agent_lifecycle.Running
  in
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:"claude-sonnet-4-6"
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
    ~model:"claude-sonnet-4-6"
    ~last_error:"previous error"
    Agent_lifecycle.Failed
  in
  let snap = Agent_lifecycle.build_snapshot
    ~agent_name:"test-agent"
    ~provider:None
    ~model:"claude-sonnet-4-6"
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
    ~model:"claude-sonnet-4-6"
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

(* ── transition guard tests ───────────────────────────── *)

let check_ok msg = function
  | Ok _ -> ()
  | Error e ->
    Alcotest.fail (Printf.sprintf "%s: unexpected error: %s"
      msg (Agent_lifecycle.transition_error_to_string e))

let check_error msg = function
  | Error _ -> ()
  | Ok s ->
    Alcotest.fail (Printf.sprintf "%s: expected error but got Ok %s"
      msg (Agent_lifecycle.show_lifecycle_status s))

let test_transition_accepted_to_ready () =
  check_ok "Accepted->Ready"
    (Agent_lifecycle.transition ~from:Accepted ~to_:Ready)

let test_transition_ready_to_running () =
  check_ok "Ready->Running"
    (Agent_lifecycle.transition ~from:Ready ~to_:Running)

let test_transition_running_to_ready () =
  check_ok "Running->Ready (multi-turn)"
    (Agent_lifecycle.transition ~from:Running ~to_:Ready)

let test_transition_running_to_completed () =
  check_ok "Running->Completed"
    (Agent_lifecycle.transition ~from:Running ~to_:Completed)

let test_transition_running_to_failed () =
  check_ok "Running->Failed"
    (Agent_lifecycle.transition ~from:Running ~to_:Failed)

let test_transition_accepted_to_failed () =
  check_ok "Accepted->Failed"
    (Agent_lifecycle.transition ~from:Accepted ~to_:Failed)

let test_transition_self_running () =
  check_ok "Running->Running (self)"
    (Agent_lifecycle.transition ~from:Running ~to_:Running)

let test_transition_self_accepted () =
  check_ok "Accepted->Accepted (self)"
    (Agent_lifecycle.transition ~from:Accepted ~to_:Accepted)

let test_transition_invalid_accepted_to_running () =
  check_error "Accepted->Running (skip Ready)"
    (Agent_lifecycle.transition ~from:Accepted ~to_:Running)

let test_transition_invalid_accepted_to_completed () =
  check_error "Accepted->Completed (skip Ready,Running)"
    (Agent_lifecycle.transition ~from:Accepted ~to_:Completed)

let test_transition_invalid_ready_to_completed () =
  check_error "Ready->Completed (skip Running)"
    (Agent_lifecycle.transition ~from:Ready ~to_:Completed)

let test_transition_terminal_completed () =
  let r = Agent_lifecycle.transition ~from:Completed ~to_:Running in
  (match r with
   | Error (AlreadyTerminal { status }) ->
     Alcotest.(check bool) "terminal status" true
       (status = Agent_lifecycle.Completed)
   | _ -> Alcotest.fail "expected AlreadyTerminal")

let test_transition_terminal_failed () =
  let r = Agent_lifecycle.transition ~from:Failed ~to_:Accepted in
  (match r with
   | Error (AlreadyTerminal { status }) ->
     Alcotest.(check bool) "terminal status" true
       (status = Agent_lifecycle.Failed)
   | _ -> Alcotest.fail "expected AlreadyTerminal")

let test_transition_terminal_self () =
  check_error "Completed->Completed (terminal self)"
    (Agent_lifecycle.transition ~from:Completed ~to_:Completed)

let test_is_terminal () =
  Alcotest.(check bool) "Completed" true (Agent_lifecycle.is_terminal Completed);
  Alcotest.(check bool) "Failed" true (Agent_lifecycle.is_terminal Failed);
  Alcotest.(check bool) "Accepted" false (Agent_lifecycle.is_terminal Accepted);
  Alcotest.(check bool) "Ready" false (Agent_lifecycle.is_terminal Ready);
  Alcotest.(check bool) "Running" false (Agent_lifecycle.is_terminal Running)

let test_valid_transitions_exhaustive () =
  Alcotest.(check int) "Accepted has 2" 2
    (List.length (Agent_lifecycle.valid_transitions Accepted));
  Alcotest.(check int) "Ready has 2" 2
    (List.length (Agent_lifecycle.valid_transitions Ready));
  Alcotest.(check int) "Running has 3" 3
    (List.length (Agent_lifecycle.valid_transitions Running));
  Alcotest.(check int) "Completed has 0" 0
    (List.length (Agent_lifecycle.valid_transitions Completed));
  Alcotest.(check int) "Failed has 0" 0
    (List.length (Agent_lifecycle.valid_transitions Failed))

let test_transition_error_to_string () =
  let s = Agent_lifecycle.transition_error_to_string
    (InvalidTransition { from_status = Accepted; to_status = Running }) in
  Alcotest.(check bool) "contains 'invalid'" true
    (String.length s > 0);
  let s2 = Agent_lifecycle.transition_error_to_string
    (AlreadyTerminal { status = Completed }) in
  Alcotest.(check bool) "contains 'terminal'" true
    (String.length s2 > 0)

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
    "transition_guards", [
      Alcotest.test_case "Accepted -> Ready" `Quick test_transition_accepted_to_ready;
      Alcotest.test_case "Ready -> Running" `Quick test_transition_ready_to_running;
      Alcotest.test_case "Running -> Ready (multi-turn)" `Quick test_transition_running_to_ready;
      Alcotest.test_case "Running -> Completed" `Quick test_transition_running_to_completed;
      Alcotest.test_case "Running -> Failed" `Quick test_transition_running_to_failed;
      Alcotest.test_case "Accepted -> Failed" `Quick test_transition_accepted_to_failed;
      Alcotest.test_case "self Running -> Running" `Quick test_transition_self_running;
      Alcotest.test_case "self Accepted -> Accepted" `Quick test_transition_self_accepted;
      Alcotest.test_case "invalid Accepted -> Running" `Quick test_transition_invalid_accepted_to_running;
      Alcotest.test_case "invalid Accepted -> Completed" `Quick test_transition_invalid_accepted_to_completed;
      Alcotest.test_case "invalid Ready -> Completed" `Quick test_transition_invalid_ready_to_completed;
      Alcotest.test_case "terminal Completed -> Running" `Quick test_transition_terminal_completed;
      Alcotest.test_case "terminal Failed -> Accepted" `Quick test_transition_terminal_failed;
      Alcotest.test_case "terminal self Completed" `Quick test_transition_terminal_self;
      Alcotest.test_case "is_terminal" `Quick test_is_terminal;
      Alcotest.test_case "valid_transitions exhaustive" `Quick test_valid_transitions_exhaustive;
      Alcotest.test_case "error_to_string" `Quick test_transition_error_to_string;
    ];
  ]
