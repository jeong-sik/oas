(** Tests for eval harness — single-agent vs swarm baseline comparison.

    All tests use closure mocks (no real LLM). *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm

(* ── Helpers ──────────────────────────────────────────────────────── *)

let float_eps = 1e-3

let mock_run_with_usage text ~input_tokens ~output_tokens ~sw:_ _prompt =
  Ok { Types.id = "mock"; model = "mock"; stop_reason = Types.EndTurn;
       content = [Types.Text text];
       usage = Some { Types.input_tokens; output_tokens;
                      cache_creation_input_tokens = 0;
                      cache_read_input_tokens = 0; cost_usd = None }; telemetry = None }

let mock_run text =
  mock_run_with_usage text ~input_tokens:10 ~output_tokens:5

let failing_run ~sw:_ _prompt =
  Error (Error.Internal "simulated failure")

let task_simple : Eval_harness.eval_task = {
  name = "simple-task";
  prompt = "Compute 2+2";
  expected_output = "4";
}

let task_no_match : Eval_harness.eval_task = {
  name = "no-match-task";
  prompt = "Say hello";
  expected_output = "impossible-substring";
}

(* ── run_single tests ─────────────────────────────────────────────── *)

let test_single_completes () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Eval_harness.run_single ~sw ~clock
    ~task:task_simple
    ~run:(mock_run "The answer is 4") in
  check string "mode" "single" result.mode;
  check string "task_name" "simple-task" result.task_name;
  check bool "completed (substring match)" true result.completed;
  check bool "has output" true (String.length result.output > 0);
  check int "token_count" 15 result.token_count;
  check int "iterations" 1 result.iterations;
  check bool "wall_clock >= 0" true (result.wall_clock_ms >= 0.0)

let test_single_fails () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Eval_harness.run_single ~sw ~clock
    ~task:task_simple
    ~run:failing_run in
  check bool "not completed" false result.completed;
  check int "zero tokens" 0 result.token_count;
  check int "iterations" 1 result.iterations

let test_single_no_match () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let result = Eval_harness.run_single ~sw ~clock
    ~task:task_no_match
    ~run:(mock_run "hello world") in
  check bool "not completed (no substring match)" false result.completed

(* ── run_swarm tests ──────────────────────────────────────────────── *)

let test_swarm_completes () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "w1"; run = mock_run "result-4"; role = Execute;
        get_telemetry = None; extensions = [] };
      { name = "w2"; run = mock_run "also-4"; role = Verify;
        get_telemetry = None; extensions = [] };
      { name = "w3"; run = mock_run "confirmed-4"; role = Summarize;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 3;
    prompt = "";  (* overridden by run_swarm *)
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0;
    collaboration_context = None; resource_check = None;
    max_concurrent_agents = None; enable_streaming = false;
  } in
  let result = Eval_harness.run_swarm ~sw ~env
    ~task:task_simple ~config in
  check string "mode" "swarm" result.mode;
  check bool "completed" true result.completed;
  (* 3 agents x 15 tokens each = 45 *)
  check int "token_count" 45 result.token_count;
  check int "iterations" 1 result.iterations;
  check bool "wall_clock >= 0" true (result.wall_clock_ms >= 0.0)

let test_swarm_with_convergence () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let call_count = ref 0 in
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "w1"; run = mock_run "result-4"; role = Execute;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> incr call_count; if !call_count >= 2 then 1.0 else 0.3);
      target = 0.95;
      max_iterations = 5;
      patience = 5;
      aggregate = Best_score;
    };
    max_parallel = 1;
    prompt = "";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0;
    collaboration_context = None; resource_check = None;
    max_concurrent_agents = None; enable_streaming = false;
  } in
  let result = Eval_harness.run_swarm ~sw ~env
    ~task:task_simple ~config in
  check bool "completed" true result.completed;
  check bool "iterations >= 2" true (result.iterations >= 2)

let test_swarm_fails () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let config : Swarm_types.swarm_config = {
    entries = [
      { name = "fail-1"; run = failing_run; role = Execute;
        get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 1;
    prompt = "";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0;
    collaboration_context = None; resource_check = None;
    max_concurrent_agents = None; enable_streaming = false;
  } in
  let result = Eval_harness.run_swarm ~sw ~env
    ~task:task_simple ~config in
  (* The swarm itself succeeds (partial failure), but output won't match *)
  check bool "not completed" false result.completed

(* ── compare tests ────────────────────────────────────────────────── *)

let make_result ~mode ~completed ~token_count ~wall_clock_ms ~iterations =
  { Eval_harness.mode; task_name = "t"; completed; output = "x";
    token_count; wall_clock_ms; iterations }

let test_compare_single_wins_by_completion () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:100 ~wall_clock_ms:500.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:false
    ~token_count:50 ~wall_clock_ms:200.0 ~iterations:3 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "single" c.recommendation

let test_compare_swarm_wins_by_completion () =
  let single = make_result ~mode:"single" ~completed:false
    ~token_count:10 ~wall_clock_ms:100.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:true
    ~token_count:100 ~wall_clock_ms:500.0 ~iterations:3 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "swarm" c.recommendation

let test_compare_both_fail () =
  let single = make_result ~mode:"single" ~completed:false
    ~token_count:10 ~wall_clock_ms:100.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:false
    ~token_count:50 ~wall_clock_ms:300.0 ~iterations:3 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "equivalent" c.recommendation

let test_compare_single_wins_by_tokens () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:15 ~wall_clock_ms:200.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:true
    ~token_count:45 ~wall_clock_ms:100.0 ~iterations:1 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "single" c.recommendation

let test_compare_swarm_wins_by_tokens () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:100 ~wall_clock_ms:100.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:true
    ~token_count:30 ~wall_clock_ms:200.0 ~iterations:2 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "swarm" c.recommendation

let test_compare_tiebreak_by_time () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:50 ~wall_clock_ms:100.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:true
    ~token_count:50 ~wall_clock_ms:200.0 ~iterations:3 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  check string "recommendation" "single" c.recommendation

(* ── to_json tests ────────────────────────────────────────────────── *)

let test_json_well_formed () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:15 ~wall_clock_ms:100.0 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:true
    ~token_count:45 ~wall_clock_ms:80.0 ~iterations:1 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  let json = Eval_harness.to_json c in
  (* Verify round-trip: serialize then parse *)
  let json_str = Yojson.Safe.to_string json in
  let reparsed = Yojson.Safe.from_string json_str in
  (* Check top-level keys *)
  (match reparsed with
   | `Assoc fields ->
     let keys = List.map fst fields in
     check bool "has task" true (List.mem "task" keys);
     check bool "has single" true (List.mem "single" keys);
     check bool "has swarm" true (List.mem "swarm" keys);
     check bool "has recommendation" true (List.mem "recommendation" keys);
     (* Check recommendation value *)
     (match List.assoc "recommendation" fields with
      | `String "single" -> ()
      | _ -> fail "expected recommendation = single")
   | _ -> fail "expected JSON object")

let test_json_fields_match_result () =
  let single = make_result ~mode:"single" ~completed:true
    ~token_count:15 ~wall_clock_ms:123.456 ~iterations:1 in
  let swarm = make_result ~mode:"swarm" ~completed:false
    ~token_count:45 ~wall_clock_ms:456.789 ~iterations:3 in
  let c = Eval_harness.compare ~single ~swarm ~task:task_simple in
  let json = Eval_harness.to_json c in
  (match json with
   | `Assoc fields ->
     (* Check single result fields *)
     (match List.assoc "single" fields with
      | `Assoc sf ->
        (match List.assoc "token_count" sf with
         | `Int n -> check int "single token_count" 15 n
         | _ -> fail "expected int");
        (match List.assoc "completed" sf with
         | `Bool b -> check bool "single completed" true b
         | _ -> fail "expected bool");
        (match List.assoc "iterations" sf with
         | `Int n -> check int "single iterations" 1 n
         | _ -> fail "expected int");
        (match List.assoc "wall_clock_ms" sf with
         | `Float f ->
           check bool "wall_clock_ms close" true
             (Float.abs (f -. 123.456) < float_eps)
         | _ -> fail "expected float")
      | _ -> fail "expected single object");
     (* Check swarm iterations *)
     (match List.assoc "swarm" fields with
      | `Assoc sf ->
        (match List.assoc "iterations" sf with
         | `Int n -> check int "swarm iterations" 3 n
         | _ -> fail "expected int")
      | _ -> fail "expected swarm object")
   | _ -> fail "expected JSON object")

(* ── Integration: end-to-end single vs swarm ──────────────────────── *)

let test_end_to_end_comparison () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run @@ fun sw ->
  let task : Eval_harness.eval_task = {
    name = "addition";
    prompt = "What is 2+2?";
    expected_output = "4";
  } in
  (* Single agent *)
  let single = Eval_harness.run_single ~sw ~clock ~task
    ~run:(mock_run_with_usage "The answer is 4"
            ~input_tokens:20 ~output_tokens:10) in
  (* 3-agent swarm *)
  let swarm_config : Swarm_types.swarm_config = {
    entries = [
      { name = "compute"; role = Execute; get_telemetry = None; extensions = [];
        run = mock_run_with_usage "computed: 4"
                ~input_tokens:20 ~output_tokens:10 };
      { name = "verify"; role = Verify; get_telemetry = None; extensions = [];
        run = mock_run_with_usage "verified: 4"
                ~input_tokens:20 ~output_tokens:10 };
      { name = "summarize"; role = Summarize; get_telemetry = None; extensions = [];
        run = mock_run_with_usage "summary: 4"
                ~input_tokens:20 ~output_tokens:10 };
    ];
    mode = Decentralized;
    convergence = None;
    max_parallel = 3;
    prompt = "";
    timeout_sec = None;
    budget = Swarm_types.no_budget; max_agent_retries = 0;
    collaboration_context = None; resource_check = None;
    max_concurrent_agents = None; enable_streaming = false;
  } in
  let swarm = Eval_harness.run_swarm ~sw ~env ~task ~config:swarm_config in
  let comparison = Eval_harness.compare ~single ~swarm ~task in
  (* Single uses 30 tokens, swarm uses 90 tokens. Both complete. *)
  check string "recommendation is single" "single" comparison.recommendation;
  check bool "single completed" true comparison.single.completed;
  check bool "swarm completed" true comparison.swarm.completed;
  check int "single tokens" 30 comparison.single.token_count;
  check int "swarm tokens" 90 comparison.swarm.token_count;
  (* JSON output round-trips *)
  let json_str = Yojson.Safe.to_string (Eval_harness.to_json comparison) in
  let _ = Yojson.Safe.from_string json_str in
  ()

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  run "eval_harness" [
    "run_single", [
      test_case "completes" `Quick test_single_completes;
      test_case "fails" `Quick test_single_fails;
      test_case "no_match" `Quick test_single_no_match;
    ];
    "run_swarm", [
      test_case "completes" `Quick test_swarm_completes;
      test_case "with_convergence" `Quick test_swarm_with_convergence;
      test_case "fails" `Quick test_swarm_fails;
    ];
    "compare", [
      test_case "single_wins_completion" `Quick test_compare_single_wins_by_completion;
      test_case "swarm_wins_completion" `Quick test_compare_swarm_wins_by_completion;
      test_case "both_fail" `Quick test_compare_both_fail;
      test_case "single_wins_tokens" `Quick test_compare_single_wins_by_tokens;
      test_case "swarm_wins_tokens" `Quick test_compare_swarm_wins_by_tokens;
      test_case "tiebreak_time" `Quick test_compare_tiebreak_by_time;
    ];
    "to_json", [
      test_case "well_formed" `Quick test_json_well_formed;
      test_case "fields_match" `Quick test_json_fields_match_result;
    ];
    "integration", [
      test_case "end_to_end" `Quick test_end_to_end_comparison;
    ];
  ]
