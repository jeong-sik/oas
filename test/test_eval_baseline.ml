open Base
(** Unit tests for Eval_baseline and Eval_report (v0.68.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let make_metric name value : Eval.metric = { name; value; unit_ = None; tags = [] }

let make_run ?(name = "test-agent") metrics verdicts : Eval.run_metrics =
  { run_id = "run-1"
  ; agent_name = name
  ; timestamp = 1000.0
  ; metrics
  ; harness_verdicts = verdicts
  ; trace_summary = None
  }
;;

let pass_verdict : Harness.verdict =
  { passed = true; score = Some 1.0; evidence = []; detail = None }
;;

let fail_verdict : Harness.verdict =
  { passed = false; score = Some 0.0; evidence = [ "failed" ]; detail = Some "bad" }
;;

(* ── Baseline save/load roundtrip ─────────────────── *)

let test_save_load_roundtrip () =
  let rm = make_run [ make_metric "accuracy" (Float_val 0.95) ] [ pass_verdict ] in
  let baseline = Eval_baseline.create ~description:"test baseline" rm in
  let path = Filename.temp_file "oas_eval_" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       (match Eval_baseline.save ~path baseline with
        | Ok () -> ()
        | Error e -> fail (Printf.sprintf "save failed: %s" e));
       match Eval_baseline.load ~path with
       | Ok loaded ->
         check string "description" "test baseline" loaded.description;
         check string "agent" "test-agent" loaded.run_metrics.agent_name;
         check int "metrics count" 1 (List.length loaded.run_metrics.metrics)
       | Error e -> fail (Printf.sprintf "load failed: %s" e))
;;

let test_load_missing_file () =
  match Eval_baseline.load ~path:"/tmp/nonexistent_oas_baseline.json" with
  | Error _ -> ()
  | Ok _ -> fail "should fail on missing file"
;;

(* ── Comparison ───────────────────────────────────── *)

let test_compare_no_regression () =
  let baseline_rm =
    make_run
      [ make_metric "accuracy" (Float_val 0.90); make_metric "latency_ms" (Int_val 200) ]
      []
  in
  let baseline = Eval_baseline.create ~description:"base" baseline_rm in
  let current =
    make_run
      [ make_metric "accuracy" (Float_val 0.92); make_metric "latency_ms" (Int_val 190) ]
      []
  in
  let c = Eval_baseline.compare ~baseline ~current () in
  check bool "passed" true c.passed;
  check int "no regressions" 0 c.regressions
;;

let test_compare_with_regression () =
  let baseline_rm = make_run [ make_metric "accuracy" (Float_val 0.90) ] [] in
  let baseline = Eval_baseline.create ~description:"base" baseline_rm in
  let current = make_run [ make_metric "accuracy" (Float_val 0.80) ] [] in
  let c = Eval_baseline.compare ~baseline ~current () in
  check bool "failed" false c.passed;
  check int "1 regression" 1 c.regressions
;;

let test_compare_added_metric () =
  let baseline_rm = make_run [ make_metric "a" (Float_val 1.0) ] [] in
  let baseline = Eval_baseline.create ~description:"base" baseline_rm in
  let current =
    make_run [ make_metric "a" (Float_val 1.0); make_metric "b" (Float_val 2.0) ] []
  in
  let c = Eval_baseline.compare ~baseline ~current () in
  check bool "passed" true c.passed;
  let added =
    List.filter
      (function
        | Eval_baseline.Added _ -> true
        | _ -> false)
      c.diffs
  in
  check int "1 added" 1 (List.length added)
;;

let test_compare_removed_metric () =
  let baseline_rm =
    make_run [ make_metric "a" (Float_val 1.0); make_metric "b" (Float_val 2.0) ] []
  in
  let baseline = Eval_baseline.create ~description:"base" baseline_rm in
  let current = make_run [ make_metric "a" (Float_val 1.0) ] [] in
  let c = Eval_baseline.compare ~baseline ~current () in
  let removed =
    List.filter
      (function
        | Eval_baseline.Removed _ -> true
        | _ -> false)
      c.diffs
  in
  check int "1 removed" 1 (List.length removed)
;;

(* ── pass@k ───────────────────────────────────────── *)

let test_pass_at_k_all_pass () =
  let runs =
    [ make_run [] [ pass_verdict ]
    ; make_run [] [ pass_verdict ]
    ; make_run [] [ pass_verdict ]
    ]
  in
  check (float 0.01) "100%" 1.0 (Eval_baseline.pass_at_k runs)
;;

let test_pass_at_k_some_fail () =
  let runs =
    [ make_run [] [ pass_verdict ]
    ; make_run [] [ fail_verdict ]
    ; make_run [] [ pass_verdict ]
    ]
  in
  check (float 0.01) "67%" 0.667 (Eval_baseline.pass_at_k runs)
;;

let test_pass_at_k_empty () = check (float 0.01) "0%" 0.0 (Eval_baseline.pass_at_k [])

let test_pass_at_k_skips_unscored_runs () =
  let runs =
    [ make_run [] [ pass_verdict ]; make_run [] []; make_run [] [ fail_verdict ] ]
  in
  check (float 0.01) "50%" 0.5 (Eval_baseline.pass_at_k runs)
;;

(* ── Eval Report ──────────────────────────────────── *)

let test_report_with_baseline () =
  let baseline_rm =
    make_run [ make_metric "accuracy" (Float_val 0.90) ] [ pass_verdict ]
  in
  let baseline = Eval_baseline.create ~description:"base" baseline_rm in
  let runs = [ make_run [ make_metric "accuracy" (Float_val 0.92) ] [ pass_verdict ] ] in
  let report = Eval_report.generate ~baseline runs in
  check string "agent" "test-agent" report.agent_name;
  check int "run count" 1 report.run_count;
  match report.verdict with
  | `Pass -> ()
  | _ -> fail "should pass"
;;

let test_report_no_baseline () =
  let runs = [ make_run [] [ pass_verdict ] ] in
  let report = Eval_report.generate runs in
  match report.verdict with
  | `NoBaseline -> ()
  | _ -> fail "should be no_baseline"
;;

let test_report_to_json () =
  let runs = [ make_run [] [ pass_verdict ] ] in
  let report = Eval_report.generate runs in
  let json = Eval_report.to_json report in
  let open Yojson.Safe.Util in
  let verdict = json |> member "verdict" |> to_string in
  check string "verdict in json" "no_baseline" verdict;
  check int "evaluated runs" 1 (json |> member "evaluated_runs" |> to_int);
  check int "skipped runs" 0 (json |> member "skipped_runs" |> to_int)
;;

let test_report_to_string () =
  let runs = [ make_run [] [ pass_verdict ]; make_run [] [] ] in
  let report = Eval_report.generate runs in
  let s = Eval_report.to_string report in
  check bool "non-empty" true (String.length s > 0);
  check int "evaluated_runs" 1 report.evaluated_runs;
  check int "skipped_runs" 1 report.skipped_runs
;;

(* ── show_diff ────────────────────────────────────── *)

let test_show_diff () =
  let s =
    Eval_baseline.show_diff
      (Regressed
         { name = "acc"; baseline_val = 0.9; current_val = 0.8; delta_pct = -11.1 })
  in
  check bool "contains name" true (String.length s > 0)
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "eval_baseline"
    [ ( "baseline"
      , [ test_case "save/load roundtrip" `Quick test_save_load_roundtrip
        ; test_case "load missing file" `Quick test_load_missing_file
        ] )
    ; ( "compare"
      , [ test_case "no regression" `Quick test_compare_no_regression
        ; test_case "with regression" `Quick test_compare_with_regression
        ; test_case "added metric" `Quick test_compare_added_metric
        ; test_case "removed metric" `Quick test_compare_removed_metric
        ] )
    ; ( "pass_at_k"
      , [ test_case "all pass" `Quick test_pass_at_k_all_pass
        ; test_case "some fail" `Quick test_pass_at_k_some_fail
        ; test_case "empty" `Quick test_pass_at_k_empty
        ; test_case "skips unscored runs" `Quick test_pass_at_k_skips_unscored_runs
        ] )
    ; ( "report"
      , [ test_case "with baseline" `Quick test_report_with_baseline
        ; test_case "no baseline" `Quick test_report_no_baseline
        ; test_case "to_json" `Quick test_report_to_json
        ; test_case "to_string" `Quick test_report_to_string
        ] )
    ; "display", [ test_case "show_diff" `Quick test_show_diff ]
    ]
;;
