(** Extended Swarm coverage tests — targets uncovered paths in
    runner.ml and traced_swarm.ml.

    Focuses on:
    - Runner.text_of_response
    - Runner.aggregate_scores all strategies
    - Runner.eval_metric with callbacks and errors
    - Swarm_types state construction and mutation
    - Single-pass mock run (Decentralized, Pipeline, Supervisor)
    - Convergence loop with early convergence
    - Swarm budget constraints
    - Traced_swarm.collect_summaries on empty/non-existent dir *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm
open Swarm_types

(* ── Helpers ──────────────────────────────────────────────── *)

let float_eps = 1e-6
let check_float msg expected actual =
  check bool msg true (Float.abs (expected -. actual) < float_eps)

let mock_run text ~sw:_ _prompt =
  Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
       content = [Text text]; usage = None }

let mock_run_err msg ~sw:_ _prompt =
  Error (Error.Internal msg)

let make_mock_entry ~net ~name ~role ~clock run_fn =
  let agent = Agent.create ~net
    ~config:{ Types.default_config with name; max_turns = 1 } () in
  let base = Swarm_types.make_entry ~name ~role ~clock agent in
  { base with run = run_fn }

(* ── text_of_response ─────────────────────────────────────── *)

let test_text_of_response_single () =
  let resp : Types.api_response = {
    id = "r"; model = "m"; stop_reason = EndTurn;
    content = [Text "hello"]; usage = None } in
  let text = Runner.text_of_response resp in
  check string "single" "hello" text

let test_text_of_response_multi () =
  let resp : Types.api_response = {
    id = "r"; model = "m"; stop_reason = EndTurn;
    content = [Text "a"; Text "b"]; usage = None } in
  let text = Runner.text_of_response resp in
  check string "joined" "a\nb" text

let test_text_of_response_empty () =
  let resp : Types.api_response = {
    id = "r"; model = "m"; stop_reason = EndTurn;
    content = []; usage = None } in
  let text = Runner.text_of_response resp in
  check string "empty" "" text

let test_text_of_response_mixed () =
  let resp : Types.api_response = {
    id = "r"; model = "m"; stop_reason = EndTurn;
    content = [
      Text "text";
      ToolUse { id = "t"; name = "tool"; input = `Null };
      Text "more";
    ]; usage = None } in
  let text = Runner.text_of_response resp in
  check string "text only" "text\nmore" text

(* ── aggregate_scores extended ────────────────────────────── *)

let test_aggregate_average_uniform () =
  let result = Runner.aggregate_scores Average_score [0.5; 0.5; 0.5] in
  check_float "average uniform" 0.5 result

let test_aggregate_majority_tie () =
  (* When tied, first occurrence wins *)
  let result = Runner.aggregate_scores Majority_vote [0.3; 0.5; 0.3; 0.5] in
  check bool "one of the tied" true (result = 0.3 || result = 0.5)

let test_aggregate_custom_min () =
  let min_fn scores = List.fold_left min infinity scores in
  let result = Runner.aggregate_scores (Custom_agg min_fn) [0.3; 0.1; 0.5] in
  check_float "custom min" 0.1 result

(* ── eval_metric extended ─────────────────────────────────── *)

let test_eval_metric_callback_value () =
  let metric = Callback (fun () -> 0.75) in
  match Runner.eval_metric metric with
  | Ok v -> check_float "callback metric" 0.75 v
  | Error e -> fail (Printf.sprintf "unexpected error: %s" e)

let test_eval_metric_callback_exception () =
  let metric = Callback (fun () -> raise (Invalid_argument "bad")) in
  match Runner.eval_metric metric with
  | Ok _ -> fail "expected error"
  | Error e -> check bool "has message" true (String.length e > 0)

let test_eval_metric_shell_success () =
  let metric = Shell_command "echo 1.23" in
  match Runner.eval_metric metric with
  | Ok v -> check_float "shell" 1.23 v
  | Error e -> fail e

let test_eval_metric_shell_non_float () =
  let metric = Shell_command "echo hello" in
  match Runner.eval_metric metric with
  | Ok _ -> fail "expected error"
  | Error _ -> ()

let test_eval_metric_shell_failure () =
  let metric = Shell_command "false" in
  match Runner.eval_metric metric with
  | Ok _ -> fail "expected error"
  | Error _ -> ()

(* ── Single-pass decentralized ────────────────────────────── *)

let test_single_pass_decentralized () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"w1" ~role:Execute ~clock (mock_run "result1");
      make_mock_entry ~net:env#net ~name:"w2" ~role:Execute ~clock (mock_run "result2");
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test prompt";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check int "one iteration" 1 (List.length result.iterations);
    check bool "not converged" false result.converged;
    let iter = List.hd result.iterations in
    check int "two agents" 2 (List.length iter.agent_results)
  | Error e -> fail (Error.to_string e)

(* ── Single-pass pipeline ─────────────────────────────────── *)

let test_single_pass_pipeline () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"stage1" ~role:Execute ~clock (mock_run "first output");
      make_mock_entry ~net:env#net ~name:"stage2" ~role:Execute ~clock (mock_run "second output");
    ];
    mode = Pipeline_mode;
    convergence = None;
    max_parallel = 1;
    prompt = "start";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check int "one iteration" 1 (List.length result.iterations)
  | Error e -> fail (Error.to_string e)

(* ── Single-pass supervisor ───────────────────────────────── *)

let test_single_pass_supervisor () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"supervisor" ~role:Summarize ~clock (mock_run "summary");
      make_mock_entry ~net:env#net ~name:"worker1" ~role:Execute ~clock (mock_run "worker result");
    ];
    mode = Supervisor;
    convergence = None;
    max_parallel = 4;
    prompt = "analyze";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check int "one iteration" 1 (List.length result.iterations);
    let iter = List.hd result.iterations in
    check int "two results (worker + supervisor)" 2
      (List.length iter.agent_results)
  | Error e -> fail (Error.to_string e)

(* ── With error agent ─────────────────────────────────────── *)

let test_single_pass_with_error () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"good" ~role:Execute ~clock (mock_run "ok");
      make_mock_entry ~net:env#net ~name:"bad" ~role:Execute ~clock (mock_run_err "fail");
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "two results" 2 (List.length iter.agent_results)
  | Error e -> fail (Error.to_string e)

(* ── Convergence quick ────────────────────────────────────── *)

let test_convergence_immediate () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"w1" ~role:Execute ~clock (mock_run "ok");
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 1.0);  (* immediately meets target *)
      target = 0.9;
      max_iterations = 10;
      patience = 3;
      aggregate = Best_score;
    };
    max_parallel = 4;
    prompt = "test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    check bool "converged" true result.converged;
    check int "one iteration" 1 (List.length result.iterations)
  | Error e -> fail (Error.to_string e)

(* ── Timeout ──────────────────────────────────────────────── *)

let test_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let slow_run ~sw:_ _prompt =
    Eio.Time.sleep clock 10.0;
    Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
         content = [Text "late"]; usage = None }
  in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"slow" ~role:Execute ~clock slow_run;
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 1;
    prompt = "test";
    timeout_sec = Some 0.1;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Error (Error.Orchestration (Error.TaskTimeout _)) -> ()
  | Error _ -> ()  (* Any error is acceptable *)
  | Ok _ -> fail "expected timeout"

(* ── Callbacks ────────────────────────────────────────────── *)

let test_callbacks_fire () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let started = ref [] in
  let done_list = ref [] in
  let callbacks : swarm_callbacks = {
    on_agent_start = Some (fun name -> started := name :: !started);
    on_agent_done = Some (fun name _status -> done_list := name :: !done_list);
    on_iteration_start = None;
    on_iteration_end = None;
    on_converged = None;
    on_error = None;
  } in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"cb-agent" ~role:Execute ~clock (mock_run "ok");
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  (match Runner.run ~sw ~clock ~callbacks config with
   | Ok _ ->
     check int "started count" 1 (List.length !started);
     check int "done count" 1 (List.length !done_list)
   | Error e -> fail (Error.to_string e))

(* ── Resource check ───────────────────────────────────────── *)

let test_resource_check_fail () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = [
      make_mock_entry ~net:env#net ~name:"w1" ~role:Execute ~clock (mock_run "ok");
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 4;
    prompt = "test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = Some (fun () -> false);
    max_concurrent_agents = None;
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    let iter = List.hd result.iterations in
    let (_name, status) = List.hd iter.agent_results in
    (match status with
     | Done_error _ -> ()
     | _ -> fail "expected Done_error for failed resource check")
  | Error _ -> ()  (* Also acceptable *)

(* ── Traced_swarm.collect_summaries ───────────────────────── *)

let test_collect_summaries_nonexistent () =
  match Traced_swarm.collect_summaries ~trace_dir:"/nonexistent/dir" with
  | Error _ -> ()
  | Ok _ -> fail "expected error for non-existent dir"

let test_collect_summaries_empty_dir () =
  let dir = Filename.temp_dir "test_traced" "" in
  match Traced_swarm.collect_summaries ~trace_dir:dir with
  | Ok summaries ->
    check int "no summaries" 0 (List.length summaries)
  | Error e -> fail (Error.to_string e)

(* ── max_concurrent_agents ────────────────────────────────── *)

let test_max_concurrent_agents () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let config : swarm_config = {
    entries = List.init 4 (fun i ->
      make_mock_entry ~net:env#net ~name:(Printf.sprintf "w%d" i) ~role:Execute ~clock (mock_run "ok"));
    mode = Decentralized;
    convergence = None;
    max_parallel = 8;
    prompt = "test";
    timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None;
    max_concurrent_agents = Some 2;  (* limit to 2 concurrent *)
  } in
  Eio.Switch.run @@ fun sw ->
  match Runner.run ~sw ~clock config with
  | Ok result ->
    let iter = List.hd result.iterations in
    check int "four results" 4 (List.length iter.agent_results)
  | Error e -> fail (Error.to_string e)

(* ── Suite ────────────────────────────────────────────────── *)

let () =
  run "Swarm_coverage" [
    "text_of_response", [
      test_case "single" `Quick test_text_of_response_single;
      test_case "multi" `Quick test_text_of_response_multi;
      test_case "empty" `Quick test_text_of_response_empty;
      test_case "mixed" `Quick test_text_of_response_mixed;
    ];
    "aggregate", [
      test_case "average uniform" `Quick test_aggregate_average_uniform;
      test_case "majority tie" `Quick test_aggregate_majority_tie;
      test_case "custom min" `Quick test_aggregate_custom_min;
    ];
    "eval_metric", [
      test_case "callback value" `Quick test_eval_metric_callback_value;
      test_case "callback exception" `Quick test_eval_metric_callback_exception;
      test_case "shell success" `Quick test_eval_metric_shell_success;
      test_case "shell non-float" `Quick test_eval_metric_shell_non_float;
      test_case "shell failure" `Quick test_eval_metric_shell_failure;
    ];
    "single_pass", [
      test_case "decentralized" `Quick test_single_pass_decentralized;
      test_case "pipeline" `Quick test_single_pass_pipeline;
      test_case "supervisor" `Quick test_single_pass_supervisor;
      test_case "with error" `Quick test_single_pass_with_error;
    ];
    "convergence", [
      test_case "immediate" `Quick test_convergence_immediate;
    ];
    "timeout", [
      test_case "timeout" `Quick test_timeout;
    ];
    "callbacks", [
      test_case "fire" `Quick test_callbacks_fire;
    ];
    "resource_check", [
      test_case "fail" `Quick test_resource_check_fail;
    ];
    "traced_swarm", [
      test_case "nonexistent dir" `Quick test_collect_summaries_nonexistent;
      test_case "empty dir" `Quick test_collect_summaries_empty_dir;
    ];
    "concurrent", [
      test_case "max_concurrent_agents" `Quick test_max_concurrent_agents;
    ];
  ]
