open Base
(** Test_autonomy_smoke — unit tests for Autonomy_trace_analyzer.

    All tests use synthetic {!Raw_trace.run_summary} fixtures.
    No LLM required, no IO. *)

open Agent_sdk
open Autonomy_trace_analyzer

(* ── Alcotest helpers ──────────────────────────────────────── *)

let classification_to_string = function
  | Autonomous -> "Autonomous"
  | Scripted -> "Scripted"
  | Random -> "Random"
  | Insufficient_data -> "Insufficient_data"
;;

let classification_testable =
  Alcotest.testable
    (fun fmt c -> Format.pp_print_string fmt (classification_to_string c))
    ( = )
;;

(* ── Synthetic fixtures ────────────────────────────────────── *)

let dummy_run_ref : Raw_trace.run_ref =
  { worker_run_id = "test-run"
  ; path = "/dev/null"
  ; start_seq = 0
  ; end_seq = 10
  ; agent_name = "test-agent"
  ; session_id = None
  }
;;

let make_summary ~tool_names ~tool_execution_started_count ~assistant_block_count
  : Raw_trace.run_summary
  =
  { run_ref = dummy_run_ref
  ; record_count = 10
  ; assistant_block_count
  ; tool_execution_started_count
  ; tool_execution_finished_count = tool_execution_started_count
  ; hook_invoked_count = 0
  ; hook_names = []
  ; tool_names
  ; model = None
  ; tool_choice = None
  ; enable_thinking = None
  ; thinking_budget = None
  ; thinking_block_count = 0
  ; text_block_count = assistant_block_count
  ; tool_use_block_count = tool_execution_started_count
  ; tool_result_block_count = 0
  ; first_assistant_block_kind =
      (if tool_execution_started_count > 0
       then Some "tool_use"
       else if assistant_block_count > 0
       then Some "text"
       else None)
  ; selection_outcome =
      (if tool_execution_started_count > 0 && assistant_block_count > 0
       then "mixed"
       else if tool_execution_started_count > 0
       then "tool_only"
       else if assistant_block_count > 0
       then "text_only"
       else "empty")
  ; saw_tool_use = tool_execution_started_count > 0
  ; saw_thinking = false
  ; final_text = Some "done"
  ; stop_reason = Some "EndTurn"
  ; error = None
  ; started_at = Some 1000.0
  ; finished_at = Some 1010.0
  }
;;

(* Worker A: uses same tool repeatedly — low diversity *)
let scripted_summary =
  make_summary
    ~tool_names:[ "file_read" ]
    ~tool_execution_started_count:10
    ~assistant_block_count:5
;;

(* Worker B: uses many different tools — high diversity *)
let diverse_summary =
  make_summary
    ~tool_names:[ "file_read"; "file_write"; "shell_exec"; "web_search"; "code_review" ]
    ~tool_execution_started_count:8
    ~assistant_block_count:6
;;

(* Worker C: completely different tools from B — high divergence *)
let divergent_summary =
  make_summary
    ~tool_names:[ "database_query"; "api_call"; "log_analysis"; "metric_check" ]
    ~tool_execution_started_count:7
    ~assistant_block_count:5
;;

(* Worker D: same tools as B — zero divergence *)
let clone_of_diverse =
  make_summary
    ~tool_names:[ "file_read"; "file_write"; "shell_exec"; "web_search"; "code_review" ]
    ~tool_execution_started_count:9
    ~assistant_block_count:7
;;

(* Worker with no tool calls *)
let empty_summary =
  make_summary ~tool_names:[] ~tool_execution_started_count:0 ~assistant_block_count:2
;;

(* ── Tests ─────────────────────────────────────────────────── *)

let test_metric_diverse () =
  let m = metric_of_summary diverse_summary in
  Alcotest.(check bool) "diversity >= 0.3" true (m.diversity >= 0.3);
  Alcotest.(check int) "unique_tool_count = 5" 5 m.unique_tool_count;
  Alcotest.(check int) "total_calls = 8" 8 m.total_calls
;;

let test_metric_scripted () =
  let m = metric_of_summary scripted_summary in
  Alcotest.(check bool) "diversity < 0.15" true (m.diversity < 0.15);
  Alcotest.(check int) "unique_tool_count = 1" 1 m.unique_tool_count
;;

let test_metric_empty () =
  let m = metric_of_summary empty_summary in
  Alcotest.(check (float 0.001)) "diversity = 0.0 when no calls" 0.0 m.diversity;
  Alcotest.(check int) "unique_tool_count = 0" 0 m.unique_tool_count
;;

let test_divergence_identical () =
  let a = metric_of_summary diverse_summary in
  let b = metric_of_summary clone_of_diverse in
  let d = divergence_of_pair a b in
  Alcotest.(check (float 0.001))
    "jaccard_distance = 0.0 for identical tool sets"
    0.0
    d.jaccard_distance;
  Alcotest.(check int) "no exclusive tools in a" 0 (List.length d.exclusive_a);
  Alcotest.(check int) "no exclusive tools in b" 0 (List.length d.exclusive_b)
;;

let test_divergence_disjoint () =
  let a = metric_of_summary diverse_summary in
  let b = metric_of_summary divergent_summary in
  let d = divergence_of_pair a b in
  Alcotest.(check bool)
    "jaccard_distance > 0.5 for mostly-disjoint sets"
    true
    (d.jaccard_distance > 0.5);
  Alcotest.(check bool) "has exclusive tools in a" true (List.length d.exclusive_a > 0);
  Alcotest.(check bool) "has exclusive tools in b" true (List.length d.exclusive_b > 0)
;;

let test_divergence_completely_disjoint () =
  let a =
    metric_of_summary
      (make_summary
         ~tool_names:[ "alpha"; "beta" ]
         ~tool_execution_started_count:4
         ~assistant_block_count:2)
  in
  let b =
    metric_of_summary
      (make_summary
         ~tool_names:[ "gamma"; "delta" ]
         ~tool_execution_started_count:4
         ~assistant_block_count:2)
  in
  let d = divergence_of_pair a b in
  Alcotest.(check (float 0.001))
    "jaccard_distance = 1.0 for completely disjoint sets"
    1.0
    d.jaccard_distance
;;

let test_divergence_empty_sets () =
  let a = metric_of_summary empty_summary in
  let b = metric_of_summary empty_summary in
  let d = divergence_of_pair a b in
  Alcotest.(check (float 0.001))
    "jaccard_distance = 0.0 for two empty sets"
    0.0
    d.jaccard_distance
;;

let test_verdict_autonomous () =
  let v = analyze [ diverse_summary; divergent_summary ] in
  Alcotest.(check classification_testable)
    "classification = autonomous"
    Autonomous
    v.classification;
  Alcotest.(check bool) "confidence > 0.5" true (v.confidence > 0.5);
  Alcotest.(check bool) "mean_diversity > 0.3" true (v.mean_diversity > 0.3);
  Alcotest.(check bool) "mean_divergence > 0.0" true (v.mean_divergence > 0.0)
;;

let test_verdict_scripted () =
  let v = analyze [ scripted_summary; scripted_summary ] in
  Alcotest.(check classification_testable)
    "classification = scripted"
    Scripted
    v.classification;
  Alcotest.(check bool) "mean_diversity < 0.15" true (v.mean_diversity < 0.15);
  Alcotest.(check (float 0.001)) "mean_divergence = 0.0 (identical)" 0.0 v.mean_divergence
;;

let test_verdict_scripted_identical_diverse () =
  (* Two workers with identical diverse tool sets — high diversity
     but zero divergence → scripted *)
  let v = analyze [ diverse_summary; clone_of_diverse ] in
  Alcotest.(check classification_testable)
    "identical tool sets with divergence=0 → scripted"
    Scripted
    v.classification;
  Alcotest.(check (float 0.001)) "zero divergence" 0.0 v.mean_divergence
;;

let test_verdict_insufficient_empty () =
  let v = analyze [] in
  Alcotest.(check classification_testable)
    "empty list → insufficient"
    Insufficient_data
    v.classification
;;

let test_verdict_single_worker_diverse () =
  let v = analyze [ diverse_summary ] in
  Alcotest.(check classification_testable)
    "single diverse worker"
    Autonomous
    v.classification;
  Alcotest.(check bool) "confidence <= 0.7 (lower for single)" true (v.confidence <= 0.7)
;;

let test_verdict_single_worker_scripted () =
  let v = analyze [ scripted_summary ] in
  Alcotest.(check classification_testable)
    "single scripted worker"
    Scripted
    v.classification
;;

let test_verdict_random () =
  (* Two workers: each uses a single unique tool, but different ones.
     High divergence, low diversity → random *)
  let w1 =
    make_summary
      ~tool_names:[ "random_a" ]
      ~tool_execution_started_count:10
      ~assistant_block_count:2
  in
  let w2 =
    make_summary
      ~tool_names:[ "random_b" ]
      ~tool_execution_started_count:10
      ~assistant_block_count:2
  in
  let v = analyze [ w1; w2 ] in
  Alcotest.(check classification_testable)
    "low diversity + high divergence → random"
    Random
    v.classification
;;

let test_verdict_to_json () =
  let v = analyze [ diverse_summary; divergent_summary ] in
  let json = verdict_to_json v in
  let json_str = Yojson.Safe.to_string json in
  Alcotest.(check bool)
    "JSON contains classification"
    true
    (Util.string_contains ~needle:"Autonomous" json_str);
  Alcotest.(check bool)
    "JSON contains evidence"
    true
    (Util.string_contains ~needle:"evidence" json_str)
;;

let test_three_workers_partial_overlap () =
  (* Three workers with partially overlapping tool sets *)
  let w1 =
    make_summary
      ~tool_names:[ "file_read"; "shell_exec"; "web_search" ]
      ~tool_execution_started_count:6
      ~assistant_block_count:4
  in
  let w2 =
    make_summary
      ~tool_names:[ "shell_exec"; "database_query"; "api_call" ]
      ~tool_execution_started_count:5
      ~assistant_block_count:3
  in
  let w3 =
    make_summary
      ~tool_names:[ "web_search"; "api_call"; "log_analysis" ]
      ~tool_execution_started_count:7
      ~assistant_block_count:5
  in
  let v = analyze [ w1; w2; w3 ] in
  Alcotest.(check classification_testable)
    "3 workers with partial overlap → autonomous"
    Autonomous
    v.classification;
  Alcotest.(check bool)
    "3 pairwise divergences"
    true
    (List.length v.pairwise_divergences = 3);
  Alcotest.(check bool)
    "all divergences > 0"
    true
    (List.for_all
       (fun (d : divergence) -> d.jaccard_distance > 0.0)
       v.pairwise_divergences)
;;

let test_custom_thresholds () =
  (* Very strict thresholds: diversity >= 0.9 and divergence >= 0.99.
     diverse_summary diversity = 5/8 = 0.625 < 0.9 → fails.
     The same data that was autonomous with defaults now fails. *)
  let strict_thresholds =
    { diversity_autonomous = 0.9
    ; diversity_scripted = 0.8
    ; divergence_autonomous = 0.99
    ; divergence_scripted = 0.95
    ; min_unique_tools = 10
    ; random_divergence = 0.8
    }
  in
  let v = analyze ~thresholds:strict_thresholds [ diverse_summary; divergent_summary ] in
  (* diversity 0.598 < 0.8 (low) + divergence 1.0 > 0.8 (high) → Random *)
  Alcotest.(check classification_testable)
    "strict thresholds → not autonomous"
    Random
    v.classification
;;

(* ── Test suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Autonomy Trace Analyzer"
    [ ( "metric"
      , [ Alcotest.test_case "diverse worker" `Quick test_metric_diverse
        ; Alcotest.test_case "scripted worker" `Quick test_metric_scripted
        ; Alcotest.test_case "empty worker" `Quick test_metric_empty
        ] )
    ; ( "divergence"
      , [ Alcotest.test_case "identical sets" `Quick test_divergence_identical
        ; Alcotest.test_case "disjoint sets" `Quick test_divergence_disjoint
        ; Alcotest.test_case
            "completely disjoint"
            `Quick
            test_divergence_completely_disjoint
        ; Alcotest.test_case "empty sets" `Quick test_divergence_empty_sets
        ] )
    ; ( "verdict"
      , [ Alcotest.test_case "autonomous" `Quick test_verdict_autonomous
        ; Alcotest.test_case "scripted" `Quick test_verdict_scripted
        ; Alcotest.test_case
            "scripted identical diverse"
            `Quick
            test_verdict_scripted_identical_diverse
        ; Alcotest.test_case "insufficient empty" `Quick test_verdict_insufficient_empty
        ; Alcotest.test_case
            "single worker diverse"
            `Quick
            test_verdict_single_worker_diverse
        ; Alcotest.test_case
            "single worker scripted"
            `Quick
            test_verdict_single_worker_scripted
        ; Alcotest.test_case "random" `Quick test_verdict_random
        ; Alcotest.test_case "three workers" `Quick test_three_workers_partial_overlap
        ; Alcotest.test_case "custom thresholds" `Quick test_custom_thresholds
        ; Alcotest.test_case "json output" `Quick test_verdict_to_json
        ] )
    ]
;;
