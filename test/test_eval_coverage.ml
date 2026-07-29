(** Extended coverage tests for the Eval module.

    Existing tests cover:
    - metric_value yojson roundtrip
    - metric yojson roundtrip
    - collector basic, verdict
    - comparison regression/improvement/unchanged
    - threshold pass/fail max/min
    - run_metrics yojson

    This file targets uncovered paths in:
    - Eval.ml: metric_value_of_yojson error path, comparison edge cases
      (baseline zero, non-numeric, custom threshold), pp_* formatters,
      show_run_metrics, metric_of_yojson error path, run_metrics_of_yojson
      error path *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────── *)

let mk_metric ?(unit_ = None) ?(tags = []) name value : Eval.metric =
  { name; value; unit_; tags }
;;

let metric_identity ?unit_ ?(tags = []) name : Eval.metric_identity =
  { name; unit_; tags }
;;

let metric_spec ?unit_ ?tags name policy : Eval.metric_spec =
  { identity = metric_identity ?unit_ ?tags name; policy }
;;

let threshold ?unit_ ?tags ?max_value ?min_value name : Eval.threshold =
  { identity = metric_identity ?unit_ ?tags name; max_value; min_value }
;;

let mk_run ?(run_id = "r1") ?(agent_name = "test") ?(verdicts = []) metrics
  : Eval.run_metrics
  =
  { run_id; agent_name; timestamp = 1000.0; metrics; harness_verdicts = verdicts }
;;

(* ── metric_value_of_yojson error ─────────────────────── *)

let test_metric_value_of_yojson_error () =
  match Eval.metric_value_of_yojson (`List []) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for list"
;;

let test_metric_value_of_yojson_null () =
  match Eval.metric_value_of_yojson `Null with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for null"
;;

(* ── show_metric_value all variants ───────────────────── *)

let test_show_metric_value_int () =
  Alcotest.(check string) "int" "42" (Eval.show_metric_value (Int_val 42))
;;

let test_show_metric_value_bool_false () =
  Alcotest.(check string) "false" "false" (Eval.show_metric_value (Bool_val false))
;;

let test_show_metric_value_string () =
  Alcotest.(check string) "str" "hello" (Eval.show_metric_value (String_val "hello"))
;;

(* ── pp_metric_value ──────────────────────────────────── *)

let test_pp_metric_value () =
  let buf = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer buf in
  Eval.pp_metric_value fmt (Float_val 3.14);
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "contains 3.14" true (Util.string_contains ~needle:"3.14" s)
;;

(* ── pp_metric ────────────────────────────────────────── *)

let test_pp_metric () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  Eval.pp_metric fmt (mk_metric "latency" (Float_val 1.5));
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "contains latency" true (Util.string_contains ~needle:"latency" s)
;;

(* ── show_metric ──────────────────────────────────────── *)

let test_show_metric () =
  let s = Eval.show_metric (mk_metric "x" (Int_val 1)) in
  Alcotest.(check string) "format" "x=1" s
;;

(* ── show_run_metrics ─────────────────────────────────── *)

let test_show_run_metrics () =
  let rm = mk_run [ mk_metric "a" (Int_val 1); mk_metric "b" (Float_val 2.5) ] in
  let s = Eval.show_run_metrics rm in
  Alcotest.(check bool) "contains run=" true (Util.string_contains ~needle:"run=" s);
  Alcotest.(check bool) "contains agent=" true (Util.string_contains ~needle:"agent=" s)
;;

(* ── pp_run_metrics ───────────────────────────────────── *)

let test_pp_run_metrics () =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  let rm = mk_run [ mk_metric "x" (Int_val 1) ] in
  Eval.pp_run_metrics fmt rm;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "non-empty" true (String.length s > 0)
;;

(* ── metric_of_yojson error path ──────────────────────── *)

let test_metric_of_yojson_type_error () =
  let json = `Assoc [ "name", `Int 42 ] in
  match Eval.metric_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad name type"
;;

let test_metric_of_yojson_bad_value () =
  let json = `Assoc [ "name", `String "x"; "value", `List [] ] in
  match Eval.metric_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad value"
;;

(* ── metric_of_yojson: no tags, no unit ───────────────── *)

let test_metric_of_yojson_minimal () =
  let json = `Assoc [ "name", `String "x"; "value", `Int 1 ] in
  match Eval.metric_of_yojson json with
  | Ok m ->
    Alcotest.(check string) "name" "x" m.name;
    Alcotest.(check (option string)) "no unit" None m.unit_;
    Alcotest.(check int) "no tags" 0 (List.length m.tags)
  | Error e -> Alcotest.fail e
;;

(* ── metric_to_yojson: with unit and tags ─────────────── *)

let test_metric_to_yojson_full () =
  let m = mk_metric ~unit_:(Some "ms") ~tags:[ "env", "prod" ] "lat" (Float_val 1.5) in
  let json = Eval.metric_to_yojson m in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "unit" "ms" (json |> member "unit" |> to_string);
  let tags = json |> member "tags" in
  Alcotest.(check string) "env tag" "prod" (tags |> member "env" |> to_string)
;;

(* ── metric_to_yojson: no unit no tags ────────────────── *)

let test_metric_to_yojson_minimal () =
  let m = mk_metric "x" (Int_val 1) in
  let json = Eval.metric_to_yojson m in
  let s = Yojson.Safe.to_string json in
  (* No "unit" or "tags" key *)
  Alcotest.(check bool) "no unit" false (Util.string_contains ~needle:"\"unit\"" s);
  Alcotest.(check bool) "no tags" false (Util.string_contains ~needle:"\"tags\"" s)
;;

(* ── run_metrics_of_yojson error path ─────────────────── *)

let test_run_metrics_of_yojson_type_error () =
  let json = `String "bad" in
  match Eval.run_metrics_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_run_metrics_of_yojson_bad_metric () =
  let json =
    `Assoc
      [ "run_id", `String "r"
      ; "agent_name", `String "a"
      ; "timestamp", `Float 0.0
      ; "metrics", `List [ `Assoc [ "name", `Int 42 ] ]
      ]
  in
  match Eval.run_metrics_of_yojson json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad metric"
;;

(* ── run_metrics_to_yojson with verdicts ──────────────── *)

let test_run_metrics_to_yojson_with_verdicts () =
  let v : Harness.verdict =
    { passed = false; score = None; evidence = [ "e1" ]; detail = Some "d1" }
  in
  let rm = mk_run ~verdicts:[ v ] [ mk_metric "x" (Int_val 1) ] in
  let json = Eval.run_metrics_to_yojson rm in
  let open Yojson.Safe.Util in
  let verdicts = json |> member "harness_verdicts" |> to_list in
  Alcotest.(check int) "1 verdict" 1 (List.length verdicts);
  let v0 = List.hd verdicts in
  Alcotest.(check bool) "passed false" false (v0 |> member "passed" |> to_bool);
  Alcotest.(check string) "detail" "d1" (v0 |> member "detail" |> to_string)
;;

let test_run_metrics_to_yojson_score_none () =
  let v : Harness.verdict =
    { passed = true; score = None; evidence = []; detail = None }
  in
  let rm = mk_run ~verdicts:[ v ] [] in
  let json = Eval.run_metrics_to_yojson rm in
  let open Yojson.Safe.Util in
  let verdicts = json |> member "harness_verdicts" |> to_list in
  let v0 = List.hd verdicts in
  Alcotest.(check bool) "score null" true (v0 |> member "score" = `Null)
;;

(* ── compare: missing metric in candidate ─────────────── *)

let test_compare_missing_candidate_metric () =
  let baseline =
    mk_run
      [ mk_metric "a" (Float_val 1.0)
      ; mk_metric ~unit_:(Some "ms") ~tags:[ "env", "prod" ] "b" (Float_val 2.0)
      ]
  in
  let candidate = mk_run ~run_id:"r2" [ mk_metric "a" (Float_val 1.0) ] in
  let specs =
    [ metric_spec "a" (Lower_is_better (Relative_pct 5.0))
    ; metric_spec
        ~unit_:"ms"
        ~tags:[ "env", "prod" ]
        "b"
        (Lower_is_better (Relative_pct 5.0))
    ]
  in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Error (Eval.Missing_candidate_metric identity) ->
    Alcotest.(check string) "missing metric" "b" identity.name;
    Alcotest.(check (option string)) "missing unit" (Some "ms") identity.unit_;
    Alcotest.(check (list (pair string string)))
      "missing tags"
      [ "env", "prod" ]
      identity.tags
  | Ok _ | Error _ -> Alcotest.fail "expected Missing_candidate_metric"
;;

(* ── compare: string metric unchanged ─────────────────── *)

let test_compare_string_metric () =
  let baseline = mk_run [ mk_metric "name" (String_val "test") ] in
  let candidate = mk_run ~run_id:"r2" [ mk_metric "name" (String_val "test") ] in
  let specs = [ metric_spec "name" Exact_value ] in
  match Eval.compare_with_specs ~specs ~baseline ~candidate with
  | Ok cmp -> Alcotest.(check int) "unchanged" 1 (List.length cmp.unchanged)
  | Error _ -> Alcotest.fail "expected comparison"
;;

(* ── threshold: no matching metric ────────────────────── *)

let test_threshold_no_matching_metric () =
  let rm = mk_run [ mk_metric "x" (Int_val 1) ] in
  let ths = [ threshold ~max_value:(Int_val 10) "missing" ] in
  match Eval.check_thresholds rm ths with
  | Error (Eval.Missing_threshold_metric identity) ->
    Alcotest.(check string) "missing metric" "missing" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Missing_threshold_metric"
;;

(* ── threshold: non-numeric values ────────────────────── *)

let test_threshold_non_numeric () =
  let rm = mk_run [ mk_metric "name" (String_val "test") ] in
  let ths = [ threshold ~max_value:(String_val "z") "name" ] in
  match Eval.check_thresholds rm ths with
  | Error (Eval.Incompatible_threshold_value { identity; _ }) ->
    Alcotest.(check string) "metric name" "name" identity.name
  | Ok _ | Error _ -> Alcotest.fail "expected Incompatible_threshold_value"
;;

(* ── threshold: both max and min ──────────────────────── *)

let test_threshold_both_pass () =
  let rm = mk_run [ mk_metric "x" (Float_val 50.0) ] in
  let ths = [ threshold ~max_value:(Float_val 100.0) ~min_value:(Float_val 10.0) "x" ] in
  let v =
    match Eval.check_thresholds rm ths with
    | Ok verdict -> verdict
    | Error _ -> Alcotest.fail "expected threshold verdict"
  in
  Alcotest.(check bool) "both pass" true v.passed
;;

let test_threshold_both_fail_min () =
  let rm = mk_run [ mk_metric "x" (Float_val 5.0) ] in
  let ths = [ threshold ~max_value:(Float_val 100.0) ~min_value:(Float_val 10.0) "x" ] in
  let v =
    match Eval.check_thresholds rm ths with
    | Ok verdict -> verdict
    | Error _ -> Alcotest.fail "expected threshold verdict"
  in
  Alcotest.(check bool) "fail min" false v.passed
;;

(* ── Test runner ───────────────────────────────────────── *)

let () =
  Alcotest.run
    "Eval_coverage"
    [ ( "metric_value_of_yojson"
      , [ Alcotest.test_case "list error" `Quick test_metric_value_of_yojson_error
        ; Alcotest.test_case "null error" `Quick test_metric_value_of_yojson_null
        ] )
    ; ( "show_metric_value"
      , [ Alcotest.test_case "int" `Quick test_show_metric_value_int
        ; Alcotest.test_case "bool false" `Quick test_show_metric_value_bool_false
        ; Alcotest.test_case "string" `Quick test_show_metric_value_string
        ] )
    ; ( "formatters"
      , [ Alcotest.test_case "pp_metric_value" `Quick test_pp_metric_value
        ; Alcotest.test_case "pp_metric" `Quick test_pp_metric
        ; Alcotest.test_case "show_metric" `Quick test_show_metric
        ; Alcotest.test_case "show_run_metrics" `Quick test_show_run_metrics
        ; Alcotest.test_case "pp_run_metrics" `Quick test_pp_run_metrics
        ] )
    ; ( "metric_of_yojson"
      , [ Alcotest.test_case "type error" `Quick test_metric_of_yojson_type_error
        ; Alcotest.test_case "bad value" `Quick test_metric_of_yojson_bad_value
        ; Alcotest.test_case "minimal" `Quick test_metric_of_yojson_minimal
        ] )
    ; ( "metric_to_yojson"
      , [ Alcotest.test_case "full" `Quick test_metric_to_yojson_full
        ; Alcotest.test_case "minimal" `Quick test_metric_to_yojson_minimal
        ] )
    ; ( "run_metrics_of_yojson"
      , [ Alcotest.test_case "type error" `Quick test_run_metrics_of_yojson_type_error
        ; Alcotest.test_case "bad metric" `Quick test_run_metrics_of_yojson_bad_metric
        ] )
    ; ( "run_metrics_to_yojson"
      , [ Alcotest.test_case
            "with verdicts"
            `Quick
            test_run_metrics_to_yojson_with_verdicts
        ; Alcotest.test_case "score None" `Quick test_run_metrics_to_yojson_score_none
        ] )
    ; ( "compare_extra"
      , [ Alcotest.test_case
            "missing candidate"
            `Quick
            test_compare_missing_candidate_metric
        ; Alcotest.test_case "string metric" `Quick test_compare_string_metric
        ] )
    ; ( "threshold_extra"
      , [ Alcotest.test_case "no matching metric" `Quick test_threshold_no_matching_metric
        ; Alcotest.test_case "non-numeric" `Quick test_threshold_non_numeric
        ; Alcotest.test_case "both pass" `Quick test_threshold_both_pass
        ; Alcotest.test_case "both fail min" `Quick test_threshold_both_fail_min
        ] )
    ]
;;
