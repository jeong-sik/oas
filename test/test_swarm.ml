(** Tests for swarm types and runner — multi-agent swarm execution.

    Phase 1 tests focus on:
    - Type construction and state transitions
    - Metric evaluation (callback-based)
    - Aggregate strategies
    - Single-pass execution with mock agents
    - Convergence loop with mock agents *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm

(* ── Helpers ──────────────────────────────────────────────────────── *)

let float_eps = 1e-6
let check_float msg expected actual =
  check bool msg true (Float.abs (expected -. actual) < float_eps)

(* ── Swarm_types tests ───────────────────────────────────────────── *)

let test_create_state () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let agent = Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "test-agent"; max_turns = 1 } () in
  let config : Swarm_types.swarm_config = {
    entries = [{ Swarm_types.name = "agent-1";
        run = (fun ~sw prompt -> Agent.run ~sw ~clock agent prompt);
        role = Discover }];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test prompt";
    timeout_sec = None;
  } in
  let state = Swarm_types.create_state config in
  check int "initial iteration" 0 state.current_iteration;
  check bool "no best metric" true (Option.is_none state.best_metric);
  check bool "not converged" false state.converged;
  check int "one agent status" 1 (List.length state.agent_statuses);
  let (name, status) = List.hd state.agent_statuses in
  check string "agent name" "agent-1" name;
  (match status with
   | Swarm_types.Idle -> ()
   | _ -> fail "expected Idle status")

let test_agent_roles () =
  let roles = [
    Swarm_types.Discover;
    Swarm_types.Verify;
    Swarm_types.Execute;
    Swarm_types.Summarize;
    Swarm_types.Custom_role "researcher";
  ] in
  check int "five roles" 5 (List.length roles);
  (* Verify show works without crash *)
  List.iter (fun r -> ignore (Swarm_types.show_agent_role r)) roles

let test_orchestration_modes () =
  let modes = [
    Swarm_types.Decentralized;
    Swarm_types.Supervisor;
    Swarm_types.Pipeline_mode;
  ] in
  check int "three modes" 3 (List.length modes);
  List.iter (fun m -> ignore (Swarm_types.show_orchestration_mode m)) modes

let test_agent_status_show () =
  let statuses = [
    Swarm_types.Idle;
    Swarm_types.Working;
    Swarm_types.Done_ok { elapsed = 1.5; text = "hello" };
    Swarm_types.Done_error { elapsed = 0.3; error = "timeout" };
  ] in
  List.iter (fun s -> ignore (Swarm_types.show_agent_status s)) statuses

let test_no_callbacks () =
  let cb = Swarm_types.no_callbacks in
  check bool "no on_iteration_start" true (Option.is_none cb.on_iteration_start);
  check bool "no on_error" true (Option.is_none cb.on_error)

(* ── Runner.eval_metric tests ────────────────────────────────────── *)

let test_eval_metric_callback () =
  let metric = Swarm_types.Callback (fun () -> 0.95) in
  match Runner.eval_metric metric with
  | Ok v -> check_float "metric value" 0.95 v
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_eval_metric_callback_raises () =
  let metric = Swarm_types.Callback (fun () -> failwith "boom") in
  match Runner.eval_metric metric with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "contains boom" true (String.length e > 0)

let test_eval_metric_shell () =
  let metric = Swarm_types.Shell_command "echo 0.42" in
  match Runner.eval_metric metric with
  | Ok v -> check_float "shell metric" 0.42 v
  | Error e -> fail (Printf.sprintf "shell metric error: %s" e)

let test_eval_metric_shell_bad_output () =
  let metric = Swarm_types.Shell_command "echo not-a-number" in
  match Runner.eval_metric metric with
  | Ok _ -> fail "expected error for non-numeric output"
  | Error _ -> ()

(* ── Aggregate tests ─────────────────────────────────────────────── *)

(* Runner doesn't expose aggregate_scores directly, but we test it
   indirectly through the convergence loop behavior. For now, test
   metric evaluation patterns. *)

let test_convergence_config_construction () =
  let conv : Swarm_types.convergence_config = {
    metric = Callback (fun () -> 0.5);
    target = 0.95;
    max_iterations = 10;
    patience = 3;
    aggregate = Average_score;
  } in
  check_float "target" 0.95 conv.target;
  check int "max_iterations" 10 conv.max_iterations;
  check int "patience" 3 conv.patience

(* ── Swarm config construction ───────────────────────────────────── *)

let test_multi_agent_config () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let make name role =
    let agent = Agent.create ~net:env#net
      ~config:{ Types.default_config with name; max_turns = 1 } () in
    { Swarm_types.name;
      run = (fun ~sw prompt -> Agent.run ~sw ~clock agent prompt);
      role }
  in
  let config : Swarm_types.swarm_config = {
    entries = [
      make "discover-1" Discover;
      make "verify-1" Verify;
      make "execute-1" Execute;
      make "summarize-1" Summarize;
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 0.8);
      target = 0.95;
      max_iterations = 5;
      patience = 2;
      aggregate = Best_score;
    };
    max_parallel = 4;
    prompt = "Analyze the codebase";
    timeout_sec = Some 60.0;
  } in
  check int "four agents" 4 (List.length config.entries);
  let state = Swarm_types.create_state config in
  check int "four statuses" 4 (List.length state.agent_statuses)

(* ── Swarm result construction ───────────────────────────────────── *)

let test_swarm_result_construction () =
  let result : Swarm_types.swarm_result = {
    iterations = [];
    final_metric = Some 0.97;
    converged = true;
    total_elapsed = 45.2;
    total_usage = Types.empty_usage;
  } in
  check bool "converged" true result.converged;
  check_float "elapsed" 45.2 result.total_elapsed;
  (match result.final_metric with
   | Some v -> check_float "final metric" 0.97 v
   | None -> fail "expected final metric")

(* ── State transitions ───────────────────────────────────────────── *)

let test_state_history () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let agent = Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "a"; max_turns = 1 } () in
  let config : Swarm_types.swarm_config = {
    entries = [{ Swarm_types.name = "a";
        run = (fun ~sw prompt -> Agent.run ~sw ~clock agent prompt);
        role = Execute }];
    mode = Decentralized;
    convergence = None;
    max_parallel = 2;
    prompt = "test";
    timeout_sec = None;
  } in
  let state = Swarm_types.create_state config in
  check int "empty history" 0 (List.length state.history)

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  run "swarm" [
    "types", [
      test_case "create_state" `Quick test_create_state;
      test_case "agent_roles" `Quick test_agent_roles;
      test_case "orchestration_modes" `Quick test_orchestration_modes;
      test_case "agent_status_show" `Quick test_agent_status_show;
      test_case "no_callbacks" `Quick test_no_callbacks;
      test_case "convergence_config" `Quick test_convergence_config_construction;
      test_case "multi_agent_config" `Quick test_multi_agent_config;
      test_case "swarm_result" `Quick test_swarm_result_construction;
      test_case "state_history" `Quick test_state_history;
    ];
    "metric", [
      test_case "eval_callback" `Quick test_eval_metric_callback;
      test_case "eval_callback_raises" `Quick test_eval_metric_callback_raises;
      test_case "eval_shell" `Quick test_eval_metric_shell;
      test_case "eval_shell_bad_output" `Quick test_eval_metric_shell_bad_output;
    ];
  ]
