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
open Swarm_types

(* ── Helpers ──────────────────────────────────────────────────────── *)

let float_eps = 1e-6
let check_float msg expected actual =
  check bool msg true (Float.abs (expected -. actual) < float_eps)

(* ── Swarm_types tests ───────────────────────────────────────────── *)

let test_create_state () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let agent = Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "test-agent"; max_turns = 1 } () in
  let config : Swarm_types.swarm_config = {
    entries = [Swarm_types.make_entry ~name:"agent-1" ~role:Discover ~clock agent];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test prompt";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
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
    Swarm_types.Done_ok { elapsed = 1.5; text = "hello"; telemetry = Swarm_types.empty_telemetry };
    Swarm_types.Done_error { elapsed = 0.3; error = "timeout"; telemetry = Swarm_types.empty_telemetry };
  ] in
  List.iter (fun s -> ignore (Swarm_types.show_agent_status s)) statuses

let test_no_callbacks () =
  let cb = Swarm_types.no_callbacks in
  check bool "no on_iteration_start" true (Option.is_none cb.on_iteration_start);
  check bool "no on_error" true (Option.is_none cb.on_error)

(* ── Runner.eval_metric tests ────────────────────────────────────── *)

let test_eval_metric_callback () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let metric = Swarm_types.Callback (fun () -> 0.95) in
  match Runner.eval_metric ~mgr metric with
  | Ok v -> check_float "metric value" 0.95 v
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_eval_metric_callback_raises () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let metric = Swarm_types.Callback (fun () -> failwith "boom") in
  match Runner.eval_metric ~mgr metric with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "contains boom" true (String.length e > 0)

let test_eval_metric_argv () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let metric = Swarm_types.Argv_command ["env"; "printf"; "0.42\n"] in
  match Runner.eval_metric ~mgr metric with
  | Ok v -> check_float "argv metric" 0.42 v
  | Error e -> fail (Printf.sprintf "argv metric error: %s" e)

let test_eval_metric_argv_bad_output () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let metric = Swarm_types.Argv_command ["env"; "printf"; "not-a-number\n"] in
  match Runner.eval_metric ~mgr metric with
  | Ok _ -> fail "expected error for non-numeric output"
  | Error _ -> ()

let test_eval_metric_argv_empty () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  match Runner.eval_metric ~mgr (Swarm_types.Argv_command []) with
  | Ok _ -> fail "expected error for empty argv"
  | Error e -> check bool "mentions empty argv" true (String.length e > 0)

let test_eval_metric_argv_quotes_args () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  let metric = Swarm_types.Argv_command ["missing-command-for-swarm-test"; "arg with space"] in
  match Runner.eval_metric ~mgr metric with
  | Ok _ -> fail "expected missing command error"
  | Error e ->
    check bool "quotes spaced arg" true
      (Astring.String.is_infix ~affix:"'arg with space'" e)

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
  let _ = clock in
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
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
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
  let _ = clock in
  let agent = Agent.create ~net:env#net
    ~config:{ Types.default_config with name = "a"; max_turns = 1 } () in
  let config : Swarm_types.swarm_config = {
    entries = [Swarm_types.make_entry ~name:"a" ~role:Execute ~clock agent];
    mode = Decentralized;
    convergence = None;
    max_parallel = 2;
    prompt = "test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
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
  let _ = clock in
  let call_count = ref 0 in
  let metric_fn () =
    incr call_count;
    (* Simulate improving metric: 0.5, 0.7, 0.9, 1.0 *)
    match !call_count with
    | 1 -> 0.5 | 2 -> 0.7 | 3 -> 0.9 | _ -> 1.0
  in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "worker-1"; run = mock_run "result-1"; role = Execute; get_telemetry = None };
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
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
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
  let _ = clock in
  let metric_fn () = 0.3 in  (* Never improves *)
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "stuck"; run = mock_run "stuck"; role = Execute; get_telemetry = None };
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
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check bool "not converged" false result.converged;
    (* patience=3: first iteration sets baseline, then 3 more without improvement *)
    check bool "stopped by patience" true (List.length result.iterations <= 5)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_convergence_max_iterations () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let counter = ref 0 in
  let metric_fn () = incr counter; float_of_int !counter *. 0.1 in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "w"; run = mock_run "x"; role = Execute; get_telemetry = None };
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
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check bool "not converged" false result.converged;
    check int "exactly 3 iterations" 3 (List.length result.iterations)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_single_pass_no_convergence () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover; get_telemetry = None };
      { name = "a2"; run = mock_run "world"; role = Verify; get_telemetry = None };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "single pass";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check bool "not converged" false result.converged;
    check int "1 iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "2 agents" 2 (List.length iter.agent_results)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_callbacks_fire () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
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
      { name = "cb-agent"; run = mock_run "ok"; role = Execute; get_telemetry = None };
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
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~env ~callbacks config with
   | Ok _ -> ()
   | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e)));
  check bool "iter_starts fired" true (!iter_starts > 0);
  check bool "iter_ends fired" true (!iter_ends > 0);
  check bool "agent_starts fired" true (!agent_starts > 0);
  check bool "agent_dones fired" true (!agent_dones > 0)

(* ── 12-worker swarm harness (pure mock, no LLM) ─────────────────── *)

(** Mock run with latency simulation via Eio.Time.sleep.
    Each agent takes a slightly different time to simulate real concurrency. *)
let mock_run_with_latency ~clock ~latency_ms text ~sw:_ _prompt =
  Eio.Time.sleep clock (float_of_int latency_ms /. 1000.0);
  Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
       content = [Types.Text text];
       usage = Some { Types.input_tokens = 50; output_tokens = 25;
                      cache_creation_input_tokens = 0;
                      cache_read_input_tokens = 0 } }

(** Mock run that can fail on demand. *)
let mock_run_failing ~fail_on_call counter ~sw:_ _prompt =
  incr counter;
  if !counter = fail_on_call then
    Error (Error.Internal "simulated failure")
  else
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text (Printf.sprintf "result-%d" !counter)];
         usage = None }

let test_12_worker_decentralized () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let agent_names = ref [] in
  let callbacks : Swarm_types.swarm_callbacks = {
    on_iteration_start = None;
    on_iteration_end = None;
    on_agent_start = Some (fun name -> agent_names := name :: !agent_names);
    on_agent_done = None;
    on_converged = None;
    on_error = None;
  } in
  let make_worker i role =
    let name = Printf.sprintf "%s-%d" (Swarm_types.show_agent_role role) i in
    let latency = 10 + (i mod 5) * 5 in  (* 10-30ms *)
    { Swarm_types.name;
      run = mock_run_with_latency ~clock ~latency_ms:latency
              (Printf.sprintf "output-%s" name);
      role; get_telemetry = None }
  in
  let config : Swarm_types.swarm_config = {
    entries = [
      make_worker 1 Discover;
      make_worker 2 Discover;
      make_worker 3 Discover;
      make_worker 4 Verify;
      make_worker 5 Verify;
      make_worker 6 Execute;
      make_worker 7 Execute;
      make_worker 8 Execute;
      make_worker 9 Execute;
      make_worker 10 Summarize;
      make_worker 11 Summarize;
      make_worker 12 (Custom_role "review");
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 6;
    prompt = "12-worker harness test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env ~callbacks config with
  | Ok result ->
    check int "1 iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "12 agent results" 12 (List.length iter.agent_results);
    (* All agents should have completed *)
    List.iter (fun (name, status) ->
      match status with
      | Swarm_types.Done_ok _ -> ()
      | _ -> fail (Printf.sprintf "agent %s not Done_ok" name)
    ) iter.agent_results;
    (* All 12 agents should have been started *)
    check int "12 agents started" 12 (List.length !agent_names);
    (* Usage should be accumulated *)
    check bool "has usage" true (result.total_usage.api_calls >= 0)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_12_worker_convergence () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let iteration = ref 0 in
  let metric_fn () =
    incr iteration;
    (* 12 workers contribute: metric rises quickly *)
    min 1.0 (float_of_int !iteration *. 0.35)
  in
  let make_worker i role =
    let name = Printf.sprintf "w%d" i in
    { Swarm_types.name;
      run = mock_run_with_latency ~clock ~latency_ms:5 (Printf.sprintf "r%d" i);
      role; get_telemetry = None }
  in
  let config : Swarm_types.swarm_config = {
    entries = List.init 12 (fun i ->
      let role = match i mod 4 with
        | 0 -> Swarm_types.Discover | 1 -> Verify
        | 2 -> Execute | _ -> Summarize
      in
      make_worker (i + 1) role);
    mode = Decentralized;
    convergence = Some {
      metric = Callback metric_fn;
      target = 0.95;
      max_iterations = 10;
      patience = 5;
      aggregate = Best_score;
    };
    max_parallel = 12;
    prompt = "12-worker convergence";
    timeout_sec = Some 30.0;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    check bool "converged" true result.converged;
    check bool "3 or fewer iterations" true (List.length result.iterations <= 3);
    (* Each iteration should have 12 agent results *)
    List.iter (fun iter ->
      check int "12 per iteration" 12 (List.length iter.agent_results)
    ) result.iterations
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_12_worker_supervisor () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let supervisor_saw_workers = ref false in
  let supervisor_run ~sw:_ prompt =
    (* Supervisor should receive worker summaries *)
    if String.length prompt > 100 then
      supervisor_saw_workers := true;
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text "supervisor synthesis"];
         usage = None }
  in
  let make_worker i =
    { Swarm_types.name = Printf.sprintf "worker-%d" i;
      run = mock_run_with_latency ~clock ~latency_ms:5
              (Printf.sprintf "worker-%d output" i);
      role = Execute; get_telemetry = None }
  in
  let config : Swarm_types.swarm_config = {
    entries =
      { name = "supervisor"; run = supervisor_run; role = Summarize; get_telemetry = None }
      :: List.init 11 (fun i -> make_worker (i + 1));
    mode = Supervisor;
    convergence = None;
    max_parallel = 6;
    prompt = "supervisor test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    (* 11 workers + 1 supervisor = 12 results *)
    check int "12 results" 12 (List.length iter.agent_results);
    check bool "supervisor saw worker outputs" true !supervisor_saw_workers
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_12_worker_pipeline () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let received_prompts = ref [] in
  let pipeline_run name ~sw:_ prompt =
    received_prompts := (name, String.length prompt) :: !received_prompts;
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text (Printf.sprintf "stage-%s-output" name)];
         usage = None }
  in
  let config : Swarm_types.swarm_config = {
    entries = List.init 4 (fun i ->
      let name = Printf.sprintf "stage-%d" i in
      { Swarm_types.name;
        run = pipeline_run name;
        role = Execute; get_telemetry = None });
    mode = Pipeline_mode;
    convergence = None;
    max_parallel = 1;
    prompt = "base prompt";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "4 results" 4 (List.length iter.agent_results);
    (* Later stages should receive longer prompts (base + previous output) *)
    let prompts = List.rev !received_prompts in
    let lengths = List.map snd prompts in
    (* Each stage's prompt should be >= the previous one *)
    let rec increasing = function
      | [] | [_] -> true
      | a :: b :: rest -> a <= b && increasing (b :: rest)
    in
    check bool "prompts grow" true (increasing lengths)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

let test_partial_failure_resilience () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let counter = ref 0 in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "ok-1"; run = mock_run "fine"; role = Execute; get_telemetry = None };
      { name = "fail-1";
        run = mock_run_failing ~fail_on_call:1 counter;
        role = Execute; get_telemetry = None };
      { name = "ok-2"; run = mock_run "also-fine"; role = Execute; get_telemetry = None };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 3;
    prompt = "resilience test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "3 results" 3 (List.length iter.agent_results);
    (* Check that we have both ok and error results *)
    let ok_count = List.length (List.filter (fun (_, s) ->
      match s with Swarm_types.Done_ok _ -> true | _ -> false
    ) iter.agent_results) in
    let err_count = List.length (List.filter (fun (_, s) ->
      match s with Swarm_types.Done_error _ -> true | _ -> false
    ) iter.agent_results) in
    check int "2 ok" 2 ok_count;
    check int "1 error" 1 err_count
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(* ── Review-driven regression tests ──────────────────────────────── *)

(** P1: Single-pass swarm should respect timeout_sec. *)
let test_single_pass_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let slow_run ~sw:_ _prompt =
    Eio.Time.sleep clock 10.0;  (* 10s — will exceed timeout *)
    Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
         content = [Types.Text "late"]; usage = None }
  in
  let config : Swarm_types.swarm_config = {
    entries = [{ name = "slow"; run = slow_run; role = Execute; get_telemetry = None }];
    mode = Decentralized;
    convergence = None;
    max_parallel = 1;
    prompt = "timeout test";
    timeout_sec = Some 0.05;  (* 50ms timeout *)
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok _ -> fail "expected timeout error"
  | Error (Error.Orchestration (Error.TaskTimeout _)) -> ()
  | Error e -> fail (Printf.sprintf "wrong error: %s" (Error.to_string e))

(** P1: Single-pass swarm should accumulate usage from agents. *)
let test_single_pass_usage () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "a1"; run = mock_run "hello"; role = Discover; get_telemetry = None };
      { name = "a2"; run = mock_run "world"; role = Verify; get_telemetry = None };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "usage test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    (* mock_run returns usage with input_tokens=10, output_tokens=5 each *)
    check int "api_calls" 2 result.total_usage.api_calls;
    check int "input_tokens" 20 result.total_usage.total_input_tokens;
    check int "output_tokens" 10 result.total_usage.total_output_tokens
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

(** P2: Average_score aggregate prevents premature convergence from spikes.

    With Best_score, a single spike (0.9) would converge at target=0.5.
    With Average_score, the avg stays below 0.5, proving aggregate is applied. *)
let test_convergence_average_aggregate () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let _ = clock in
  let call_count = ref 0 in
  let metric_fn () =
    incr call_count;
    (* One spike among low values: 0.3, 0.3, 0.3, 0.3, 0.9, 0.3 *)
    if !call_count = 5 then 0.9 else 0.3
  in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "w"; run = mock_run "x"; role = Execute; get_telemetry = None };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback metric_fn;
      target = 0.5;
      max_iterations = 6;
      patience = 10;
      aggregate = Average_score;
    };
    max_parallel = 1;
    prompt = "aggregate test";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~env config with
  | Ok result ->
    (* avg([0.3,0.3,0.3,0.3,0.9,0.3]) = 0.4 < 0.5 → not converged *)
    check bool "not converged (avg too low)" false result.converged;
    check int "ran all 6 iterations" 6 (List.length result.iterations)
  | Error e -> fail (Printf.sprintf "error: %s" (Error.to_string e))

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
      test_case "eval_argv" `Quick test_eval_metric_argv;
      test_case "eval_argv_bad_output" `Quick test_eval_metric_argv_bad_output;
      test_case "eval_argv_empty" `Quick test_eval_metric_argv_empty;
      test_case "eval_argv_quotes_args" `Quick test_eval_metric_argv_quotes_args;
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
    "harness", [
      test_case "12_worker_decentralized" `Quick test_12_worker_decentralized;
      test_case "12_worker_convergence" `Quick test_12_worker_convergence;
      test_case "12_worker_supervisor" `Quick test_12_worker_supervisor;
      test_case "12_worker_pipeline" `Quick test_12_worker_pipeline;
      test_case "partial_failure" `Quick test_partial_failure_resilience;
    ];
    "review_fixes", [
      test_case "single_pass_timeout" `Quick test_single_pass_timeout;
      test_case "single_pass_usage" `Quick test_single_pass_usage;
      test_case "convergence_avg_aggregate" `Quick test_convergence_average_aggregate;
    ];
  ]
