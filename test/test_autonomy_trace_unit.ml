(** Deep unit tests for Autonomy_trace_analyzer — targeting uncovered classify
    branches and edge cases through the public API.

    Public API: metric_of_summary, divergence_of_pair, analyze,
    verdict_to_json, default_thresholds, and all the types.

    Covers: multi-worker moderate diversity (between thresholds),
    single-worker moderate diversity (Insufficient_data),
    multi-worker insufficient evidence, custom thresholds narrow
    boundaries, classify score accumulation branches,
    divergence edge cases. *)

open Agent_sdk
open Autonomy_trace_analyzer

(* ── Alcotest helpers ──────────────────────────────────────── *)

let classification_to_string = function
  | Autonomous -> "Autonomous"
  | Scripted -> "Scripted"
  | Random -> "Random"
  | Insufficient_data -> "Insufficient_data"

let classification_testable =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (classification_to_string c))
    ( = )

(* ── Fixtures ──────────────────────────────────────────────── *)

let dummy_run_ref : Raw_trace.run_ref =
  {
    worker_run_id = "test-run";
    path = "/dev/null";
    start_seq = 0;
    end_seq = 10;
    agent_name = "test-agent";
    session_id = None;
  }

let make_summary ~tool_names ~tool_execution_started_count
    ~assistant_block_count : Raw_trace.run_summary =
  {
    run_ref = dummy_run_ref;
    record_count = 10;
    assistant_block_count;
    tool_execution_started_count;
    tool_execution_finished_count = tool_execution_started_count;
    hook_invoked_count = 0;
    hook_names = [];
    tool_names;
    final_text = Some "done";
    stop_reason = Some "EndTurn";
    error = None;
    started_at = Some 1000.0;
    finished_at = Some 1010.0;
  }

(* ── metric_of_summary edge cases ──────────────────────────── *)

let test_metric_zero_calls_with_tools () =
  let s =
    make_summary ~tool_names:[ "tool_a" ] ~tool_execution_started_count:0
      ~assistant_block_count:3
  in
  let m = metric_of_summary s in
  Alcotest.(check (float 0.001)) "diversity 0.0 when no calls" 0.0 m.diversity;
  Alcotest.(check int) "unique tools = 1" 1 m.unique_tool_count;
  Alcotest.(check int) "total calls = 0" 0 m.total_calls

let test_metric_turn_count () =
  let s =
    make_summary ~tool_names:[ "a"; "b" ] ~tool_execution_started_count:4
      ~assistant_block_count:12
  in
  let m = metric_of_summary s in
  Alcotest.(check int) "turn_count = assistant_block_count" 12 m.turn_count

let test_metric_high_diversity () =
  let s =
    make_summary
      ~tool_names:[ "a"; "b"; "c"; "d"; "e" ]
      ~tool_execution_started_count:5 ~assistant_block_count:3
  in
  let m = metric_of_summary s in
  Alcotest.(check (float 0.001)) "diversity = 1.0" 1.0 m.diversity;
  Alcotest.(check int) "unique = 5" 5 m.unique_tool_count

let test_metric_low_diversity () =
  let s =
    make_summary ~tool_names:[ "a" ] ~tool_execution_started_count:100
      ~assistant_block_count:50
  in
  let m = metric_of_summary s in
  Alcotest.(check (float 0.001)) "diversity = 0.01" 0.01 m.diversity

(* ── divergence edge cases ─────────────────────────────────── *)

let test_divergence_single_shared () =
  let a : worker_metric =
    {
      diversity = 0.5;
      unique_tool_count = 2;
      total_calls = 4;
      turn_count = 3;
      tool_set = [ "shared"; "only_a" ];
    }
  in
  let b : worker_metric =
    {
      diversity = 0.5;
      unique_tool_count = 2;
      total_calls = 4;
      turn_count = 3;
      tool_set = [ "shared"; "only_b" ];
    }
  in
  let d = divergence_of_pair a b in
  (* union = {shared, only_a, only_b} = 3, intersection = {shared} = 1
     jaccard_distance = 1 - 1/3 = 0.667 *)
  Alcotest.(check (float 0.01)) "jaccard ~0.667" 0.667 d.jaccard_distance;
  Alcotest.(check (list string)) "shared" [ "shared" ] d.shared_tools;
  Alcotest.(check (list string)) "exclusive_a" [ "only_a" ] d.exclusive_a;
  Alcotest.(check (list string)) "exclusive_b" [ "only_b" ] d.exclusive_b

let test_divergence_superset () =
  let a : worker_metric =
    {
      diversity = 1.0;
      unique_tool_count = 3;
      total_calls = 3;
      turn_count = 2;
      tool_set = [ "x"; "y"; "z" ];
    }
  in
  let b : worker_metric =
    {
      diversity = 1.0;
      unique_tool_count = 1;
      total_calls = 1;
      turn_count = 1;
      tool_set = [ "x" ];
    }
  in
  let d = divergence_of_pair a b in
  (* union = 3, inter = 1, jaccard = 1 - 1/3 = 0.667 *)
  Alcotest.(check (float 0.01)) "jaccard for superset" 0.667
    d.jaccard_distance;
  Alcotest.(check (list string)) "exclusive_a" [ "y"; "z" ] d.exclusive_a;
  Alcotest.(check (list string)) "exclusive_b empty" [] d.exclusive_b

(* ── classify: single-worker moderate diversity ────────────── *)

let test_single_worker_moderate_diversity () =
  (* diversity between scripted (0.15) and autonomous (0.3) thresholds *)
  let s =
    make_summary
      ~tool_names:[ "tool_a"; "tool_b" ]
      ~tool_execution_started_count:10 ~assistant_block_count:5
  in
  (* diversity = 2/10 = 0.2, between 0.15 and 0.3 *)
  let v = analyze [ s ] in
  Alcotest.(check classification_testable)
    "moderate single -> insufficient" Insufficient_data v.classification;
  Alcotest.(check bool) "evidence mentions single" true
    (List.exists
       (fun e -> Util.string_contains ~needle:"single_worker" e)
       v.evidence)

(* ── classify: single worker with enough diversity and tools ─ *)

let test_single_worker_autonomous () =
  let s =
    make_summary
      ~tool_names:[ "a"; "b"; "c"; "d" ]
      ~tool_execution_started_count:8 ~assistant_block_count:4
  in
  (* diversity = 4/8 = 0.5 >= 0.3, tools = 4 >= 3 *)
  let v = analyze [ s ] in
  Alcotest.(check classification_testable)
    "single diverse worker -> autonomous" Autonomous v.classification;
  Alcotest.(check bool) "confidence <= 0.7 (lower for single)" true
    (v.confidence <= 0.7)

(* ── classify: single worker scripted ──────────────────────── *)

let test_single_worker_scripted () =
  let s =
    make_summary ~tool_names:[ "only_one" ]
      ~tool_execution_started_count:20 ~assistant_block_count:10
  in
  (* diversity = 1/20 = 0.05 < 0.15 *)
  let v = analyze [ s ] in
  Alcotest.(check classification_testable)
    "single scripted worker" Scripted v.classification

(* ── classify: multi-worker moderate (insufficient evidence) ─ *)

let test_multi_worker_moderate_insufficient () =
  let w1 =
    make_summary
      ~tool_names:[ "a"; "b" ]
      ~tool_execution_started_count:8 ~assistant_block_count:4
  in
  let w2 =
    make_summary
      ~tool_names:[ "a"; "c" ]
      ~tool_execution_started_count:8 ~assistant_block_count:4
  in
  (* diversity each: 2/8 = 0.25 (between 0.15 and 0.3)
     divergence: Jaccard of {a,b} vs {a,c} = 1 - 1/3 = 0.667
     diversity_ok false, divergence_ok true, tools_ok false
     score = 0.4, no diversity_low, no divergence_low -> Insufficient *)
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "moderate everything -> insufficient" Insufficient_data v.classification

(* ── classify: multi-worker all pass ───────────────────────── *)

let test_multi_worker_all_pass_score () =
  let w1 =
    make_summary
      ~tool_names:[ "a"; "b"; "c"; "d"; "e" ]
      ~tool_execution_started_count:5 ~assistant_block_count:3
  in
  let w2 =
    make_summary
      ~tool_names:[ "f"; "g"; "h"; "i"; "j" ]
      ~tool_execution_started_count:5 ~assistant_block_count:3
  in
  (* diversity 1.0, divergence 1.0, tools 5 -> score 1.0 *)
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "all pass -> autonomous" Autonomous v.classification;
  Alcotest.(check (float 0.01)) "confidence = 1.0" 1.0 v.confidence

(* ── classify: low diversity + high divergence -> random ───── *)

let test_multi_worker_random () =
  let w1 =
    make_summary ~tool_names:[ "x" ] ~tool_execution_started_count:20
      ~assistant_block_count:5
  in
  let w2 =
    make_summary ~tool_names:[ "y" ] ~tool_execution_started_count:20
      ~assistant_block_count:5
  in
  (* diversity = 0.05 < 0.15, divergence = 1.0 > 0.8 *)
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "low diversity + high divergence -> random" Random v.classification

(* ── classify: divergence_low -> scripted ──────────────────── *)

let test_multi_worker_divergence_low () =
  let w1 =
    make_summary
      ~tool_names:[ "a"; "b"; "c" ]
      ~tool_execution_started_count:6 ~assistant_block_count:3
  in
  let w2 =
    make_summary
      ~tool_names:[ "a"; "b"; "c" ]
      ~tool_execution_started_count:6 ~assistant_block_count:3
  in
  (* diversity 0.5, divergence 0.0 < 0.05 -> scripted *)
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "zero divergence -> scripted" Scripted v.classification

(* ── classify: high score but no tools_ok ──────────────────── *)

let test_multi_worker_tools_insufficient () =
  let w1 =
    make_summary ~tool_names:[ "a"; "b" ] ~tool_execution_started_count:2
      ~assistant_block_count:2
  in
  let w2 =
    make_summary ~tool_names:[ "c"; "d" ] ~tool_execution_started_count:2
      ~assistant_block_count:2
  in
  (* diversity 1.0, divergence 1.0, tools 2 < 3
     score = 0.3+0.4+0 = 0.7 >= 0.7 -> autonomous *)
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "score = 0.7 -> autonomous" Autonomous v.classification

(* ── verdict_to_json ───────────────────────────────────────── *)

let test_verdict_json_has_fields () =
  let v = analyze [] in
  let j = verdict_to_json v in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "has classification" true
    (Util.string_contains ~needle:"classification" s);
  Alcotest.(check bool) "has confidence" true
    (Util.string_contains ~needle:"confidence" s);
  Alcotest.(check bool) "has evidence" true
    (Util.string_contains ~needle:"evidence" s);
  Alcotest.(check bool) "has metrics" true
    (Util.string_contains ~needle:"metrics" s)

let test_verdict_json_autonomous () =
  let w1 =
    make_summary
      ~tool_names:[ "a"; "b"; "c"; "d" ]
      ~tool_execution_started_count:4 ~assistant_block_count:3
  in
  let w2 =
    make_summary
      ~tool_names:[ "e"; "f"; "g"; "h" ]
      ~tool_execution_started_count:4 ~assistant_block_count:3
  in
  let v = analyze [ w1; w2 ] in
  let j = verdict_to_json v in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "contains Autonomous" true
    (Util.string_contains ~needle:"Autonomous" s)

let test_verdict_json_scripted () =
  let w =
    make_summary ~tool_names:[ "a" ] ~tool_execution_started_count:10
      ~assistant_block_count:5
  in
  let v = analyze [ w; w ] in
  let j = verdict_to_json v in
  let s = Yojson.Safe.to_string j in
  Alcotest.(check bool) "contains Scripted" true
    (Util.string_contains ~needle:"Scripted" s)

(* ── custom thresholds ─────────────────────────────────────── *)

let test_custom_thresholds_narrow_scripted () =
  (* Use thresholds that push the same-tool-set workers into scripted:
     same tools -> divergence = 0.0 < divergence_scripted -> divergence_low *)
  let thresholds =
    {
      diversity_autonomous = 0.5;
      diversity_scripted = 0.49;
      divergence_autonomous = 0.5;
      divergence_scripted = 0.1;
      min_unique_tools = 1;
      random_divergence = 0.99;
    }
  in
  let w1 =
    make_summary ~tool_names:[ "a"; "b" ] ~tool_execution_started_count:5
      ~assistant_block_count:3
  in
  let w2 =
    make_summary ~tool_names:[ "a"; "b" ] ~tool_execution_started_count:5
      ~assistant_block_count:3
  in
  (* diversity 0.4 < 0.49 -> diversity_low
     divergence 0.0 < 0.1 -> divergence_low
     score = 0.0 + 0.0 + 0.3 = 0.3 < 0.7
     divergence_low -> scripted *)
  let v = analyze ~thresholds [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "narrow thresholds -> scripted" Scripted v.classification

let test_custom_thresholds_relaxed () =
  let thresholds =
    {
      diversity_autonomous = 0.01;
      diversity_scripted = 0.005;
      divergence_autonomous = 0.01;
      divergence_scripted = 0.005;
      min_unique_tools = 1;
      random_divergence = 0.99;
    }
  in
  let w1 =
    make_summary ~tool_names:[ "a" ] ~tool_execution_started_count:10
      ~assistant_block_count:5
  in
  let w2 =
    make_summary ~tool_names:[ "b" ] ~tool_execution_started_count:10
      ~assistant_block_count:5
  in
  (* diversity 0.1 >= 0.01, divergence 1.0 >= 0.01, tools 1 >= 1 -> all pass *)
  let v = analyze ~thresholds [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "relaxed thresholds -> autonomous" Autonomous v.classification

(* ── pairwise count ────────────────────────────────────────── *)

let test_four_workers_pairwise_count () =
  let make_worker name =
    make_summary ~tool_names:[ name ] ~tool_execution_started_count:1
      ~assistant_block_count:1
  in
  let v =
    analyze
      [
        make_worker "a";
        make_worker "b";
        make_worker "c";
        make_worker "d";
      ]
  in
  (* 4 choose 2 = 6 pairs *)
  Alcotest.(check int) "6 pairwise divergences" 6
    (List.length v.pairwise_divergences)

(* ── default_thresholds values ─────────────────────────────── *)

let test_default_thresholds () =
  let t = default_thresholds in
  Alcotest.(check (float 0.001)) "diversity_autonomous" 0.3
    t.diversity_autonomous;
  Alcotest.(check (float 0.001)) "diversity_scripted" 0.15
    t.diversity_scripted;
  Alcotest.(check (float 0.001)) "divergence_autonomous" 0.15
    t.divergence_autonomous;
  Alcotest.(check (float 0.001)) "divergence_scripted" 0.05
    t.divergence_scripted;
  Alcotest.(check int) "min_unique_tools" 3 t.min_unique_tools;
  Alcotest.(check (float 0.001)) "random_divergence" 0.8
    t.random_divergence

(* ── empty input ───────────────────────────────────────────── *)

let test_empty_summaries () =
  let v = analyze [] in
  Alcotest.(check classification_testable)
    "empty -> insufficient" Insufficient_data v.classification;
  Alcotest.(check (float 0.001)) "confidence = 0" 0.0 v.confidence;
  Alcotest.(check int) "no metrics" 0 (List.length v.metrics);
  Alcotest.(check int) "no divergences" 0
    (List.length v.pairwise_divergences)

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "autonomy-trace-unit"
    [
      ( "metric",
        [
          Alcotest.test_case "zero calls with tools" `Quick
            test_metric_zero_calls_with_tools;
          Alcotest.test_case "turn count" `Quick test_metric_turn_count;
          Alcotest.test_case "high diversity" `Quick test_metric_high_diversity;
          Alcotest.test_case "low diversity" `Quick test_metric_low_diversity;
        ] );
      ( "divergence",
        [
          Alcotest.test_case "single shared" `Quick
            test_divergence_single_shared;
          Alcotest.test_case "superset" `Quick test_divergence_superset;
        ] );
      ( "classify-single",
        [
          Alcotest.test_case "moderate diversity" `Quick
            test_single_worker_moderate_diversity;
          Alcotest.test_case "autonomous" `Quick test_single_worker_autonomous;
          Alcotest.test_case "scripted" `Quick test_single_worker_scripted;
        ] );
      ( "classify-multi",
        [
          Alcotest.test_case "moderate insufficient" `Quick
            test_multi_worker_moderate_insufficient;
          Alcotest.test_case "all pass score" `Quick
            test_multi_worker_all_pass_score;
          Alcotest.test_case "random" `Quick test_multi_worker_random;
          Alcotest.test_case "divergence low" `Quick
            test_multi_worker_divergence_low;
          Alcotest.test_case "tools insufficient" `Quick
            test_multi_worker_tools_insufficient;
        ] );
      ( "json",
        [
          Alcotest.test_case "verdict fields" `Quick test_verdict_json_has_fields;
          Alcotest.test_case "autonomous json" `Quick test_verdict_json_autonomous;
          Alcotest.test_case "scripted json" `Quick test_verdict_json_scripted;
        ] );
      ( "thresholds",
        [
          Alcotest.test_case "narrow scripted" `Quick
            test_custom_thresholds_narrow_scripted;
          Alcotest.test_case "relaxed" `Quick test_custom_thresholds_relaxed;
          Alcotest.test_case "default values" `Quick test_default_thresholds;
        ] );
      ( "pairwise",
        [
          Alcotest.test_case "four workers count" `Quick
            test_four_workers_pairwise_count;
        ] );
      ( "edge_cases",
        [
          Alcotest.test_case "empty summaries" `Quick test_empty_summaries;
        ] );
    ]
