(** Tests for Eval module — evaluation framework. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let mk_metric ?unit_ ?(tags = []) name value = { Eval.name; value; unit_; tags }

let metric_identity ?unit_ ?(tags = []) name : Eval.metric_identity =
  { name; unit_; tags }
;;

let metric_spec ?unit_ ?tags name policy : Eval.metric_spec =
  { identity = metric_identity ?unit_ ?tags name; policy }
;;

let threshold ?unit_ ?tags ?max_value ?min_value name : Eval.threshold =
  { identity = metric_identity ?unit_ ?tags name; max_value; min_value }
;;

let mk_run_metrics ?(run_id = "r1") ?(agent_name = "test") metrics =
  { Eval.run_id; agent_name; timestamp = 0.0; metrics; harness_verdicts = [] }
;;

(* ── metric_value tests ───────────────────────────────────────── *)

let test_metric_value_yojson_roundtrip () =
  let values = [ Eval.Int_val 42; Float_val 3.14; Bool_val true; String_val "hello" ] in
  List.iter
    (fun v ->
       let json = Eval.metric_value_to_yojson v in
       match Eval.metric_value_of_yojson json with
       | Ok v2 ->
         Alcotest.(check string)
           "roundtrip"
           (Eval.show_metric_value v)
           (Eval.show_metric_value v2)
       | Error e -> Alcotest.fail e)
    values
;;

(* ── metric tests ─────────────────────────────────────────────── *)

let test_metric_yojson_roundtrip () =
  let m =
    { Eval.name = "latency"
    ; value = Float_val 1.5
    ; unit_ = Some "ms"
    ; tags = [ "env", "test" ]
    }
  in
  let json = Eval.metric_to_yojson m in
  match Eval.metric_of_yojson json with
  | Ok m2 ->
    Alcotest.(check string) "name" m.name m2.name;
    Alcotest.(check string)
      "value"
      (Eval.show_metric_value m.value)
      (Eval.show_metric_value m2.value)
  | Error e -> Alcotest.fail e
;;

let test_metric_yojson_rejects_malformed_tags () =
  List.iter
    (fun tags ->
       let json =
         `Assoc [ "name", `String "latency"; "value", `Float 1.0; "tags", tags ]
       in
       match Eval.metric_of_yojson json with
       | Error _ -> ()
       | Ok _ -> Alcotest.fail "explicit malformed tags must be rejected")
    [ `List [ `String "not-an-object" ]; `Null ]
;;

let test_metric_yojson_rejects_duplicate_fields_and_tags () =
  let valid_tags = `Assoc [ "env", `String "prod" ] in
  let duplicate_tag_key =
    `Assoc
      [ "name", `String "latency"
      ; "value", `Float 1.0
      ; "tags", `Assoc [ "env", `String "prod"; "env", `String "stage" ]
      ]
  in
  let duplicate_top_level first second =
    `Assoc
      [ "name", `String "latency"; "value", `Float 1.0; "tags", first; "tags", second ]
  in
  List.iter
    (fun json ->
       match Eval.metric_of_yojson json with
       | Error _ -> ()
       | Ok _ -> Alcotest.fail "duplicate JSON identity fields must be rejected")
    [ duplicate_tag_key
    ; duplicate_top_level valid_tags `Null
    ; duplicate_top_level `Null valid_tags
    ; `Assoc [ "name", `String "latency"; "name", `String "other"; "value", `Float 1.0 ]
    ; `Assoc
        [ "name", `String "latency"; "value", `Float 1.0; "fallback", `String "invented" ]
    ]
;;

(* ── collector tests ──────────────────────────────────────────── *)

let test_collector_basic () =
  let c = Eval.create_collector ~agent_name:"a" ~run_id:"r" in
  Eval.record c (mk_metric "x" (Int_val 1));
  Eval.record c (mk_metric "y" (Float_val 2.0));
  let rm = Eval.finalize c in
  Alcotest.(check int) "metrics count" 2 (List.length rm.metrics);
  Alcotest.(check string) "agent" "a" rm.agent_name;
  Alcotest.(check string) "run_id" "r" rm.run_id
;;

let test_collector_verdict () =
  let c = Eval.create_collector ~agent_name:"a" ~run_id:"r" in
  let v = { Harness.passed = true; score = Some 1.0; evidence = []; detail = None } in
  Eval.add_verdict c v;
  let rm = Eval.finalize c in
  Alcotest.(check int) "verdicts" 1 (List.length rm.harness_verdicts)
;;

(* ── comparison tests ─────────────────────────────────────────── *)

let comparison_or_fail = function
  | Ok comparison -> comparison
  | Error _ -> Alcotest.fail "expected comparison"
;;

let test_compare_regression () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 100.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 120.0) ] in
  let specs = [ metric_spec "latency" (Lower_is_better (Relative_pct 5.0)) ] in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail in
  Alcotest.(check int) "regressions" 1 (List.length cmp.regressions);
  Alcotest.(check int) "improvements" 0 (List.length cmp.improvements)
;;

let test_compare_improvement () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 100.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 80.0) ] in
  let specs = [ metric_spec "latency" (Lower_is_better (Relative_pct 5.0)) ] in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail in
  Alcotest.(check int) "regressions" 0 (List.length cmp.regressions);
  Alcotest.(check int) "improvements" 1 (List.length cmp.improvements)
;;

let test_compare_unchanged () =
  let baseline = mk_run_metrics [ mk_metric "score" (Float_val 0.95) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "score" (Float_val 0.96) ] in
  let specs = [ metric_spec "score" (Higher_is_better (Relative_pct 5.0)) ] in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail in
  Alcotest.(check int) "unchanged" 1 (List.length cmp.unchanged)
;;

let test_compare_with_specs_higher_is_better () =
  let baseline = mk_run_metrics [ mk_metric "accuracy" (Float_val 0.90) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "accuracy" (Float_val 0.95) ] in
  let specs = [ metric_spec "accuracy" (Higher_is_better (Relative_pct 1.0)) ] in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail in
  Alcotest.(check int) "improvements" 1 (List.length cmp.improvements);
  Alcotest.(check int) "regressions" 0 (List.length cmp.regressions)
;;

let test_compare_with_specs_excludes_unspecified_metrics () =
  let baseline =
    mk_run_metrics
      [ mk_metric "accuracy" (Float_val 0.90); mk_metric "latency" (Float_val 100.0) ]
  in
  let candidate =
    mk_run_metrics
      ~run_id:"r2"
      [ mk_metric "accuracy" (Float_val 0.95); mk_metric "latency" (Float_val 200.0) ]
  in
  let specs = [ metric_spec "accuracy" (Higher_is_better (Relative_pct 1.0)) ] in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail in
  Alcotest.(check int) "selected improvement" 1 (List.length cmp.improvements);
  Alcotest.(check int) "unspecified regression excluded" 0 (List.length cmp.regressions)
;;

let test_compare_rejects_invalid_tolerance () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 100.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 101.0) ] in
  let specs = [ metric_spec "latency" (Lower_is_better (Relative_pct Float.nan)) ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Invalid_numeric_tolerance { identity; _ }) ->
    Alcotest.(check string) "metric name" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Invalid_numeric_tolerance"
;;

let test_compare_rejects_invalid_absolute_tolerances () =
  let baseline = mk_run_metrics [ mk_metric "count" (Int_val 10) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "count" (Int_val 11) ] in
  List.iter
    (fun policy ->
       match
         Eval.compare_with_specs
           ~specs:[ metric_spec "count" policy ]
           ~baseline
           ~candidate
       with
       | Error (Eval.Invalid_numeric_tolerance { identity; _ }) ->
         Alcotest.(check string) "metric name" "count" identity.name
       | Ok _ | Error _ -> Alcotest.fail "expected Invalid_numeric_tolerance")
    [ Exact_numeric (Absolute_int (-1L)); Exact_numeric (Absolute_float Float.infinity) ]
;;

let test_compare_requires_exact_metric_identity () =
  let baseline = mk_run_metrics [ mk_metric ~unit_:"ms" "latency" (Float_val 100.0) ] in
  let candidate =
    mk_run_metrics ~run_id:"r2" [ mk_metric ~unit_:"s" "latency" (Float_val 0.1) ]
  in
  let specs =
    [ metric_spec ~unit_:"ms" "latency" (Lower_is_better (Relative_pct 5.0)) ]
  in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Missing_candidate_metric identity) ->
    Alcotest.(check string) "metric name" "latency" identity.name;
    Alcotest.(check (option string)) "exact unit" (Some "ms") identity.unit_
  | Ok _ | Error _ -> Alcotest.fail "expected Missing_candidate_metric"
;;

let test_compare_rejects_incompatible_metric_values () =
  let baseline = mk_run_metrics [ mk_metric "quality" (String_val "high") ] in
  let candidate =
    mk_run_metrics ~run_id:"r2" [ mk_metric "quality" (String_val "low") ]
  in
  let specs = [ metric_spec "quality" (Higher_is_better (Relative_pct 5.0)) ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Incompatible_metric_values { identity; _ }) ->
    Alcotest.(check string) "metric name" "quality" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Incompatible_metric_values"
;;

let test_compare_rejects_non_finite_metric_value () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val Float.nan) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 1.0) ] in
  let specs = [ metric_spec "latency" (Lower_is_better (Absolute_float 0.1)) ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Non_finite_metric_value { identity; side = Baseline; _ }) ->
    Alcotest.(check string) "metric name" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Non_finite_metric_value"
;;

let test_compare_zero_baseline_requires_absolute_tolerance () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 0.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 0.1) ] in
  let relative_specs = [ metric_spec "latency" (Lower_is_better (Relative_pct 5.0)) ] in
  (match Eval.compare_with_specs ~specs:relative_specs ~baseline ~candidate with
   | Error (Eval.Relative_tolerance_zero_baseline { identity; _ }) ->
     Alcotest.(check string) "metric name" "latency" identity.name
   | Ok _ | Error _ -> Alcotest.fail "expected Relative_tolerance_zero_baseline");
  let absolute_specs = [ metric_spec "latency" (Lower_is_better (Absolute_float 0.2)) ] in
  let comparison =
    Eval.compare_with_specs ~specs:absolute_specs ~baseline ~candidate
    |> comparison_or_fail
  in
  Alcotest.(check int) "within absolute tolerance" 1 (List.length comparison.unchanged)
;;

let test_compare_preserves_exact_int_difference () =
  let baseline = mk_run_metrics [ mk_metric "count" (Int_val (max_int - 1)) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "count" (Int_val max_int) ] in
  let specs = [ metric_spec "count" (Lower_is_better (Absolute_int 0L)) ] in
  let comparison =
    Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail
  in
  Alcotest.(check int) "exact integer boundary" 1 (List.length comparison.regressions)
;;

let test_compare_absolute_float_does_not_compute_relative_delta () =
  let baseline = mk_run_metrics [ mk_metric "range" (Float_val 1e-308) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "range" (Float_val 1.0) ] in
  let specs = [ metric_spec "range" (Exact_numeric (Absolute_float 2.0)) ] in
  let comparison =
    Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail
  in
  match comparison.unchanged with
  | [ delta ] ->
    Alcotest.(check bool) "no unused percentage" true (Option.is_none delta.delta_pct)
  | _ -> Alcotest.fail "expected unchanged absolute comparison"
;;

let test_compare_rejects_derived_numeric_overflow () =
  let baseline = mk_run_metrics [ mk_metric "range" (Float_val (-.Float.max_float)) ] in
  let candidate =
    mk_run_metrics ~run_id:"r2" [ mk_metric "range" (Float_val Float.max_float) ]
  in
  let specs = [ metric_spec "range" (Exact_numeric (Absolute_float 0.0)) ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Non_finite_numeric_result identity) ->
    Alcotest.(check string) "metric name" "range" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Non_finite_numeric_result"
;;

let test_compare_tag_identity_is_order_insensitive () =
  let baseline =
    mk_run_metrics
      [ mk_metric ~tags:[ "env", "prod"; "region", "kr" ] "latency" (Float_val 1.0) ]
  in
  let candidate =
    mk_run_metrics
      ~run_id:"r2"
      [ mk_metric ~tags:[ "region", "kr"; "env", "prod" ] "latency" (Float_val 1.0) ]
  in
  let specs =
    [ metric_spec ~tags:[ "env", "prod"; "region", "kr" ] "latency" Exact_value ]
  in
  let comparison =
    Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail
  in
  Alcotest.(check int) "same identity" 1 (List.length comparison.unchanged)
;;

let test_compare_selects_composite_metric_identities () =
  let metrics first second third =
    [ mk_metric ~unit_:"ms" ~tags:[ "env", "prod" ] "latency" (Float_val first)
    ; mk_metric ~unit_:"ms" ~tags:[ "env", "stage" ] "latency" (Float_val second)
    ; mk_metric ~unit_:"s" ~tags:[ "env", "prod" ] "latency" (Float_val third)
    ]
  in
  let baseline = mk_run_metrics (metrics 100.0 200.0 1.0) in
  let candidate = mk_run_metrics ~run_id:"r2" (metrics 90.0 220.0 1.0) in
  let specs =
    [ metric_spec
        ~unit_:"ms"
        ~tags:[ "env", "prod" ]
        "latency"
        (Lower_is_better (Absolute_float 0.0))
    ; metric_spec
        ~unit_:"ms"
        ~tags:[ "env", "stage" ]
        "latency"
        (Lower_is_better (Absolute_float 0.0))
    ; metric_spec
        ~unit_:"s"
        ~tags:[ "env", "prod" ]
        "latency"
        (Lower_is_better (Absolute_float 0.0))
    ]
  in
  let comparison =
    Eval.compare_with_specs ~specs ~baseline ~candidate |> comparison_or_fail
  in
  Alcotest.(check int) "one exact improvement" 1 (List.length comparison.improvements);
  Alcotest.(check int) "one exact regression" 1 (List.length comparison.regressions);
  Alcotest.(check int) "different unit selected" 1 (List.length comparison.unchanged)
;;

let test_compare_rejects_duplicate_exact_identity () =
  let duplicate = mk_metric ~tags:[ "env", "prod" ] "latency" (Float_val 1.0) in
  let baseline = mk_run_metrics [ duplicate; duplicate ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ duplicate ] in
  let specs = [ metric_spec ~tags:[ "env", "prod" ] "latency" Exact_value ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Duplicate_baseline_metric identity) ->
    Alcotest.(check string) "metric name" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Duplicate_baseline_metric"
;;

let test_compare_rejects_duplicate_spec_and_tag () =
  let metric = mk_metric "latency" (Float_val 1.0) in
  let baseline = mk_run_metrics [ metric ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ metric ] in
  let spec = metric_spec "latency" Exact_value in
  (match Eval.compare_with_specs ~specs:[ spec; spec ] ~baseline ~candidate with
   | Error (Eval.Duplicate_metric_spec identity) ->
     Alcotest.(check string) "duplicate spec" "latency" identity.name
   | Ok _ | Error _ -> Alcotest.fail "expected Duplicate_metric_spec");
  let baseline =
    mk_run_metrics
      [ mk_metric ~tags:[ "env", "prod"; "env", "stage" ] "latency" (Float_val 1.0) ]
  in
  match Eval.compare_with_specs ~specs:[ spec ] ~baseline ~candidate with
  | Error (Eval.Duplicate_metric_tag { identity; side = Baseline; _ }) ->
    Alcotest.(check string) "duplicate tag" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Duplicate_metric_tag"
;;

let test_compare_rejects_non_finite_candidate () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 1.0) ] in
  let candidate =
    mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val Float.infinity) ]
  in
  let specs = [ metric_spec "latency" (Lower_is_better (Absolute_float 0.0)) ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Non_finite_metric_value { identity; side = Candidate; _ }) ->
    Alcotest.(check string) "metric name" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected candidate Non_finite_metric_value"
;;

(* ── threshold tests ──────────────────────────────────────────── *)

let threshold_or_fail = function
  | Ok verdict -> verdict
  | Error _ -> Alcotest.fail "expected threshold verdict"
;;

let test_threshold_pass () =
  let rm = mk_run_metrics [ mk_metric "latency" (Float_val 50.0) ] in
  let ths = [ threshold ~max_value:(Float_val 100.0) "latency" ] in
  let v = Eval.check_thresholds rm ths |> threshold_or_fail in
  Alcotest.(check bool) "passed" true v.passed
;;

let test_threshold_fail_max () =
  let rm = mk_run_metrics [ mk_metric "latency" (Float_val 150.0) ] in
  let ths = [ threshold ~max_value:(Float_val 100.0) "latency" ] in
  let v = Eval.check_thresholds rm ths |> threshold_or_fail in
  Alcotest.(check bool) "failed" false v.passed;
  Alcotest.(check int) "evidence" 1 (List.length v.evidence)
;;

let test_threshold_fail_min () =
  let rm = mk_run_metrics [ mk_metric "score" (Float_val 0.3) ] in
  let ths = [ threshold ~min_value:(Float_val 0.5) "score" ] in
  let v = Eval.check_thresholds rm ths |> threshold_or_fail in
  Alcotest.(check bool) "failed" false v.passed
;;

let test_threshold_requires_exact_metric_identity () =
  let rm = mk_run_metrics [ mk_metric ~unit_:"ms" "latency" (Float_val 50.0) ] in
  let thresholds = [ threshold ~unit_:"s" ~max_value:(Float_val 100.0) "latency" ] in
  match Eval.check_thresholds rm thresholds with
  | Error (Eval.Missing_threshold_metric identity) ->
    Alcotest.(check string) "metric name" "latency" identity.name;
    Alcotest.(check (option string)) "exact unit" (Some "s") identity.unit_
  | Ok _ | Error _ -> Alcotest.fail "expected Missing_threshold_metric"
;;

let test_thresholds_select_composite_metric_identities () =
  let rm =
    mk_run_metrics
      [ mk_metric ~unit_:"ms" ~tags:[ "env", "prod" ] "latency" (Int_val 10)
      ; mk_metric ~unit_:"ms" ~tags:[ "env", "stage" ] "latency" (Int_val 20)
      ; mk_metric ~unit_:"s" ~tags:[ "env", "prod" ] "latency" (Int_val 1)
      ]
  in
  let thresholds =
    [ threshold ~unit_:"ms" ~tags:[ "env", "prod" ] ~max_value:(Int_val 10) "latency"
    ; threshold ~unit_:"ms" ~tags:[ "env", "stage" ] ~max_value:(Int_val 20) "latency"
    ; threshold ~unit_:"s" ~tags:[ "env", "prod" ] ~max_value:(Int_val 1) "latency"
    ]
  in
  let verdict = Eval.check_thresholds rm thresholds |> threshold_or_fail in
  Alcotest.(check bool) "both exact identities pass" true verdict.passed
;;

let test_threshold_evidence_preserves_canonical_identity () =
  let rm =
    mk_run_metrics
      [ mk_metric
          ~unit_:"ms"
          ~tags:[ "region", "kr"; "env", "prod" ]
          "latency"
          (Int_val 20)
      ]
  in
  let thresholds =
    [ threshold
        ~unit_:"ms"
        ~tags:[ "env", "prod"; "region", "kr" ]
        ~max_value:(Int_val 10)
        "latency"
    ]
  in
  let verdict = Eval.check_thresholds rm thresholds |> threshold_or_fail in
  match verdict.evidence with
  | [ evidence ] ->
    Alcotest.(check string)
      "canonical identity"
      {|{"name":"latency","unit":"ms","tags":{"env":"prod","region":"kr"}}=20 exceeds max 10|}
      evidence
  | _ -> Alcotest.fail "expected one threshold violation"
;;

let test_threshold_rejects_duplicate_empty_and_invalid_range () =
  let rm = mk_run_metrics [ mk_metric "latency" (Int_val 10) ] in
  let exact = threshold ~max_value:(Int_val 10) "latency" in
  (match Eval.check_thresholds rm [ exact; exact ] with
   | Error (Eval.Duplicate_threshold identity) ->
     Alcotest.(check string) "duplicate" "latency" identity.name
   | Ok _ | Error _ -> Alcotest.fail "expected Duplicate_threshold");
  (match Eval.check_thresholds rm [ threshold "latency" ] with
   | Error (Eval.Empty_threshold identity) ->
     Alcotest.(check string) "empty" "latency" identity.name
   | Ok _ | Error _ -> Alcotest.fail "expected Empty_threshold");
  match
    Eval.check_thresholds
      rm
      [ threshold ~min_value:(Int_val 11) ~max_value:(Int_val 10) "latency" ]
  with
  | Error (Eval.Invalid_threshold_range identity) ->
    Alcotest.(check string) "range" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Invalid_threshold_range"
;;

let test_threshold_rejects_duplicate_metric_and_non_finite_value () =
  let metric = mk_metric "latency" (Float_val 10.0) in
  let limit = threshold ~max_value:(Float_val 10.0) "latency" in
  (match Eval.check_thresholds (mk_run_metrics [ metric; metric ]) [ limit ] with
   | Error (Eval.Duplicate_threshold_metric identity) ->
     Alcotest.(check string) "duplicate metric" "latency" identity.name
   | Ok _ | Error _ -> Alcotest.fail "expected Duplicate_threshold_metric");
  match
    Eval.check_thresholds
      (mk_run_metrics [ metric ])
      [ threshold ~max_value:(Float_val Float.nan) "latency" ]
  with
  | Error (Eval.Non_finite_threshold_value { identity; _ }) ->
    Alcotest.(check string) "non-finite threshold" "latency" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Non_finite_threshold_value"
;;

(* ── run_metrics serialization ────────────────────────────────── *)

let test_run_metrics_yojson () =
  let rm = mk_run_metrics [ mk_metric "x" (Int_val 42) ] in
  let json = Eval.run_metrics_to_yojson rm in
  match Eval.run_metrics_of_yojson json with
  | Ok rm2 ->
    Alcotest.(check string) "run_id" rm.run_id rm2.run_id;
    Alcotest.(check int) "metrics" 1 (List.length rm2.metrics)
  | Error e -> Alcotest.fail e
;;

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Eval"
    [ ( "metric_value"
      , [ Alcotest.test_case "yojson roundtrip" `Quick test_metric_value_yojson_roundtrip
        ] )
    ; ( "metric"
      , [ Alcotest.test_case "yojson roundtrip" `Quick test_metric_yojson_roundtrip
        ; Alcotest.test_case
            "malformed tags rejected"
            `Quick
            test_metric_yojson_rejects_malformed_tags
        ; Alcotest.test_case
            "duplicate fields and tags rejected"
            `Quick
            test_metric_yojson_rejects_duplicate_fields_and_tags
        ] )
    ; ( "collector"
      , [ Alcotest.test_case "basic" `Quick test_collector_basic
        ; Alcotest.test_case "verdict" `Quick test_collector_verdict
        ] )
    ; ( "comparison"
      , [ Alcotest.test_case "regression" `Quick test_compare_regression
        ; Alcotest.test_case "improvement" `Quick test_compare_improvement
        ; Alcotest.test_case "unchanged" `Quick test_compare_unchanged
        ; Alcotest.test_case
            "spec higher better"
            `Quick
            test_compare_with_specs_higher_is_better
        ; Alcotest.test_case
            "unspecified metrics excluded"
            `Quick
            test_compare_with_specs_excludes_unspecified_metrics
        ; Alcotest.test_case
            "invalid tolerance rejected"
            `Quick
            test_compare_rejects_invalid_tolerance
        ; Alcotest.test_case
            "invalid absolute tolerances rejected"
            `Quick
            test_compare_rejects_invalid_absolute_tolerances
        ; Alcotest.test_case
            "exact metric identity required"
            `Quick
            test_compare_requires_exact_metric_identity
        ; Alcotest.test_case
            "incompatible metric values rejected"
            `Quick
            test_compare_rejects_incompatible_metric_values
        ; Alcotest.test_case
            "non-finite metric rejected"
            `Quick
            test_compare_rejects_non_finite_metric_value
        ; Alcotest.test_case
            "zero baseline requires absolute tolerance"
            `Quick
            test_compare_zero_baseline_requires_absolute_tolerance
        ; Alcotest.test_case
            "exact int difference preserved"
            `Quick
            test_compare_preserves_exact_int_difference
        ; Alcotest.test_case
            "absolute float skips relative delta"
            `Quick
            test_compare_absolute_float_does_not_compute_relative_delta
        ; Alcotest.test_case
            "derived numeric overflow rejected"
            `Quick
            test_compare_rejects_derived_numeric_overflow
        ; Alcotest.test_case
            "tag order is not identity"
            `Quick
            test_compare_tag_identity_is_order_insensitive
        ; Alcotest.test_case
            "composite identities are selected exactly"
            `Quick
            test_compare_selects_composite_metric_identities
        ; Alcotest.test_case
            "duplicate exact identity rejected"
            `Quick
            test_compare_rejects_duplicate_exact_identity
        ; Alcotest.test_case
            "duplicate spec and tag rejected"
            `Quick
            test_compare_rejects_duplicate_spec_and_tag
        ; Alcotest.test_case
            "non-finite candidate rejected"
            `Quick
            test_compare_rejects_non_finite_candidate
        ] )
    ; ( "threshold"
      , [ Alcotest.test_case "pass" `Quick test_threshold_pass
        ; Alcotest.test_case "fail max" `Quick test_threshold_fail_max
        ; Alcotest.test_case "fail min" `Quick test_threshold_fail_min
        ; Alcotest.test_case
            "exact metric identity required"
            `Quick
            test_threshold_requires_exact_metric_identity
        ; Alcotest.test_case
            "composite identities are selected exactly"
            `Quick
            test_thresholds_select_composite_metric_identities
        ; Alcotest.test_case
            "violation evidence preserves identity"
            `Quick
            test_threshold_evidence_preserves_canonical_identity
        ; Alcotest.test_case
            "duplicate empty and invalid range rejected"
            `Quick
            test_threshold_rejects_duplicate_empty_and_invalid_range
        ; Alcotest.test_case
            "duplicate metric and non-finite threshold rejected"
            `Quick
            test_threshold_rejects_duplicate_metric_and_non_finite_value
        ] )
    ; ( "serialization"
      , [ Alcotest.test_case "run_metrics yojson" `Quick test_run_metrics_yojson ] )
    ]
;;
