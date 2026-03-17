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
    entries = [Swarm_types.make_entry ~name:"agent-1" ~role:Discover ~clock agent];
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

let test_aggregate_best_score () =
  let result = Runner.aggregate_scores Best_score [0.3; 0.9; 0.5] in
  check_float "best" 0.9 result

let test_aggregate_average () =
  let result = Runner.aggregate_scores Average_score [0.2; 0.4; 0.6] in
  check_float "average" 0.4 result

let test_aggregate_majority_vote () =
  let result = Runner.aggregate_scores Majority_vote [0.5; 0.5; 0.8] in
  check_float "majority" 0.5 result

let test_aggregate_custom () =
  let sum_fn scores = List.fold_left ( +. ) 0.0 scores in
  let result = Runner.aggregate_scores (Custom_agg sum_fn) [1.0; 2.0; 3.0] in
  check_float "custom sum" 6.0 result

let test_aggregate_empty () =
  let result = Runner.aggregate_scores Best_score [] in
  check_float "empty" 0.0 result

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
    Swarm_types.make_entry ~name ~role ~clock agent
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
    entries = [Swarm_types.make_entry ~name:"a" ~role:Execute ~clock agent];
    mode = Decentralized;
    convergence = None;
    max_parallel = 2;
    prompt = "test";
    timeout_sec = None;
  } in
  let state = Swarm_types.create_state config in
  check int "empty history" 0 (List.length state.history)

(* ── Convergence loop tests (mock agents) ────────────────────────── *)

(** Mock run function that always returns a text response. *)
let mock_run text ~sw:_ _prompt =
  Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
       content = [Types.Text text];
       usage = Some { Types.input_tokens = 10; output_tokens = 5;
                      cache_creation_input_tokens = 0;
                      cache_read_input_tokens = 0 } }

let test_convergence_reaches_target () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_count = ref 0 in
  let metric_fn () =
    incr call_count;
    (* Simulate improving metric: 0.5, 0.7, 0.9, 1.0 *)
    match !call_count with
    | 1 -> 0.5 | 2 -> 0.7 | 3 -> 0.9 | _ -> 1.0
  in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "worker-1"; run = mock_run "result-1"; role = Execute };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback metric_fn;
      target = 0.95;
      max_iterations = 10;
      patience = 5;
      aggregate = Best_score;
    };
    max_parallel = 2;
    prompt = "test convergence";
    timeout_sec = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check bool "converged" true result.converged;
    check int "4 iterations" 4 (List.length result.iterations);
    (match result.final_metric with
     | Some v -> check bool "metric >= 0.95" true (v >= 0.95)
     | None -> fail "expected final metric")
  | Error e -> fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))

let test_convergence_patience_exhausted () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let metric_fn () = 0.3 in  (* Never improves *)
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "stuck"; run = mock_run "stuck"; role = Execute };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback metric_fn;
      target = 0.95;
      max_iterations = 20;
      patience = 3;
      aggregate = Best_score;
    };
    max_parallel = 1;
    prompt = "patience test";
    timeout_sec = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check bool "not converged" false result.converged;
    (* patience=3: first iteration sets baseline, then 3 more without improvement *)
    check bool "stopped by patience" true (List.length result.iterations <= 5)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_convergence_max_iterations () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let counter = ref 0 in
  let metric_fn () = incr counter; float_of_int !counter *. 0.1 in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "w"; run = mock_run "x"; role = Execute };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback metric_fn;
      target = 100.0;  (* Unreachable *)
      max_iterations = 3;
      patience = 100;
      aggregate = Best_score;
    };
    max_parallel = 1;
    prompt = "max iter test";
    timeout_sec = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check bool "not converged" false result.converged;
    check int "exactly 3 iterations" 3 (List.length result.iterations)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_single_pass_no_convergence () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover };
      { name = "a2"; run = mock_run "world"; role = Verify };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "single pass";
    timeout_sec = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check bool "not converged" false result.converged;
    check int "1 iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "2 agents" 2 (List.length iter.agent_results)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_callbacks_fire () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let iter_starts = ref 0 in
  let iter_ends = ref 0 in
  let agent_starts = ref 0 in
  let agent_dones = ref 0 in
  let callbacks : Swarm_types.swarm_callbacks = {
    on_iteration_start = Some (fun _ -> incr iter_starts);
    on_iteration_end = Some (fun _ -> incr iter_ends);
    on_agent_start = Some (fun _ -> incr agent_starts);
    on_agent_done = Some (fun _ _ -> incr agent_dones);
    on_converged = None;
    on_error = None;
  } in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "cb-agent"; run = mock_run "ok"; role = Execute };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 1.0);
      target = 0.5;
      max_iterations = 1;
      patience = 1;
      aggregate = Best_score;
    };
    max_parallel = 1;
    prompt = "callback test";
    timeout_sec = None;
  } in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~clock ~callbacks config with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e)));
  check bool "iter_starts fired" true (!iter_starts > 0);
  check bool "iter_ends fired" true (!iter_ends > 0);
  check bool "agent_starts fired" true (!agent_starts > 0);
  check bool "agent_dones fired" true (!agent_dones > 0)

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
    "aggregate", [
      test_case "best_score" `Quick test_aggregate_best_score;
      test_case "average" `Quick test_aggregate_average;
      test_case "majority_vote" `Quick test_aggregate_majority_vote;
      test_case "custom" `Quick test_aggregate_custom;
      test_case "empty" `Quick test_aggregate_empty;
    ];
    "convergence", [
      test_case "reaches_target" `Quick test_convergence_reaches_target;
      test_case "patience_exhausted" `Quick test_convergence_patience_exhausted;
      test_case "max_iterations" `Quick test_convergence_max_iterations;
      test_case "single_pass" `Quick test_single_pass_no_convergence;
      test_case "callbacks_fire" `Quick test_callbacks_fire;
    ];
  ]
