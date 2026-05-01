open Base
(** Unit tests for Metric_contract — strict <metric> tag parser
    used by the eval/judge harness. Pure, no IO, no Eio. *)

open Agent_sdk
open Alcotest

let ok = function
  | Ok _ -> true
  | Error _ -> false
;;

let is_err = function
  | Error _ -> true
  | Ok _ -> false
;;

(* ── prompt_snippet ───────────────────────────────────── *)

let test_prompt_snippet_default_name () =
  let s = Metric_contract.prompt_snippet () in
  check
    bool
    "contains default 'score' name"
    true
    (Util.contains_substring_ci ~haystack:s ~needle:"name=\"score\"")
;;

let test_prompt_snippet_custom_name () =
  let s = Metric_contract.prompt_snippet ~metric_name:"loss" () in
  check
    bool
    "contains custom name"
    true
    (Util.contains_substring_ci ~haystack:s ~needle:"name=\"loss\"")
;;

let test_prompt_snippet_mentions_finite () =
  let s = Metric_contract.prompt_snippet () in
  check
    bool
    "warns about FLOAT format"
    true
    (Util.contains_substring_ci ~haystack:s ~needle:"FLOAT")
;;

(* ── parse: happy paths ───────────────────────────────── *)

let test_parse_simple () =
  match Metric_contract.parse "<metric name=\"score\">0.75</metric>" with
  | Ok { name; value } ->
    check string "name" "score" name;
    check (float 1e-9) "value" 0.75 value
  | Error e -> failf "expected Ok, got Error %s" e
;;

let test_parse_with_surrounding_text () =
  match Metric_contract.parse "preamble <metric name=\"acc\">0.92</metric> trailing" with
  | Ok { name; value } ->
    check string "name" "acc" name;
    check (float 1e-9) "value" 0.92 value
  | Error _ -> fail "expected Ok"
;;

let test_parse_negative_value () =
  match Metric_contract.parse "<metric name=\"loss\">-1.5</metric>" with
  | Ok { value; _ } -> check (float 1e-9) "negative ok" (-1.5) value
  | Error _ -> fail "expected Ok"
;;

let test_parse_zero () =
  match Metric_contract.parse "<metric name=\"x\">0.0</metric>" with
  | Ok { value; _ } -> check (float 1e-9) "zero ok" 0.0 value
  | Error _ -> fail "expected Ok"
;;

let test_parse_integer_value () =
  match Metric_contract.parse "<metric name=\"x\">42</metric>" with
  | Ok { value; _ } -> check (float 1e-9) "int widens" 42.0 value
  | Error _ -> fail "expected Ok"
;;

let test_parse_value_with_whitespace () =
  match Metric_contract.parse "<metric name=\"x\">  3.14  </metric>" with
  | Ok { value; _ } -> check (float 1e-9) "trimmed" 3.14 value
  | Error _ -> fail "expected Ok (whitespace inside value)"
;;

let test_parse_name_with_hyphen () =
  match Metric_contract.parse "<metric name=\"f1-score\">0.5</metric>" with
  | Ok { name; _ } -> check string "preserved" "f1-score" name
  | Error _ -> fail "expected Ok"
;;

(* ── parse: expected_name guard ───────────────────────── *)

let test_parse_expected_name_match () =
  let r =
    Metric_contract.parse ~expected_name:"score" "<metric name=\"score\">0.5</metric>"
  in
  check bool "match → Ok" true (ok r)
;;

let test_parse_expected_name_mismatch () =
  match
    Metric_contract.parse ~expected_name:"loss" "<metric name=\"score\">0.5</metric>"
  with
  | Error msg ->
    check
      bool
      "mentions mismatch"
      true
      (Util.contains_substring_ci ~haystack:msg ~needle:"mismatch")
  | Ok _ -> fail "expected Error mismatch"
;;

(* ── parse: error branches ────────────────────────────── *)

let test_parse_no_tag () =
  match Metric_contract.parse "just text, no tag" with
  | Error msg ->
    check
      bool
      "mentions missing tag"
      true
      (Util.contains_substring_ci ~haystack:msg ~needle:"no <metric")
  | Ok _ -> fail "expected Error"
;;

let test_parse_duplicate_tags () =
  let text = "<metric name=\"x\">1.0</metric><metric name=\"x\">2.0</metric>" in
  match Metric_contract.parse text with
  | Error msg ->
    check
      bool
      "mentions multiple"
      true
      (Util.contains_substring_ci ~haystack:msg ~needle:"multiple")
  | Ok _ -> fail "expected Error duplicate"
;;

let test_parse_garbage_value () =
  match Metric_contract.parse "<metric name=\"x\">notanumber</metric>" with
  | Error msg ->
    check
      bool
      "mentions float"
      true
      (Util.contains_substring_ci ~haystack:msg ~needle:"float")
  | Ok _ -> fail "expected Error"
;;

let test_parse_empty_value () =
  match Metric_contract.parse "<metric name=\"x\"> </metric>" with
  | Error _ -> () (* trimmed empty fails float_of_string_opt *)
  | Ok _ -> fail "expected Error on whitespace-only value"
;;

(* ── parse: rejects non-finite ────────────────────────── *)

let test_parse_rejects_nan () =
  let r = Metric_contract.parse "<metric name=\"x\">nan</metric>" in
  check bool "nan rejected" true (is_err r);
  match r with
  | Error msg ->
    check
      bool
      "mentions finite"
      true
      (Util.contains_substring_ci ~haystack:msg ~needle:"finite")
  | _ -> ()
;;

let test_parse_rejects_infinity () =
  let r = Metric_contract.parse "<metric name=\"x\">infinity</metric>" in
  check bool "infinity rejected" true (is_err r)
;;

let test_parse_rejects_neg_infinity () =
  let r = Metric_contract.parse "<metric name=\"x\">-infinity</metric>" in
  check bool "-infinity rejected" true (is_err r)
;;

(* ── parse: tag-name pickiness ────────────────────────── *)

let test_parse_attribute_whitespace () =
  (* The regex tolerates whitespace between `metric` and `name=`. *)
  match Metric_contract.parse "<metric  name=\"x\">1.0</metric>" with
  | Ok { value; _ } -> check (float 1e-9) "tolerates extra spaces" 1.0 value
  | Error _ -> fail "expected Ok with multi-space attribute"
;;

let test_parse_tab_separator () =
  match Metric_contract.parse "<metric\tname=\"x\">1.0</metric>" with
  | Ok { value; _ } -> check (float 1e-9) "tolerates tab" 1.0 value
  | Error _ -> fail "expected Ok with tab separator"
;;

let () =
  run
    "Metric_contract"
    [ ( "prompt_snippet"
      , [ test_case "default name" `Quick test_prompt_snippet_default_name
        ; test_case "custom name" `Quick test_prompt_snippet_custom_name
        ; test_case "mentions FLOAT" `Quick test_prompt_snippet_mentions_finite
        ] )
    ; ( "parse happy"
      , [ test_case "simple" `Quick test_parse_simple
        ; test_case "surrounding text" `Quick test_parse_with_surrounding_text
        ; test_case "negative" `Quick test_parse_negative_value
        ; test_case "zero" `Quick test_parse_zero
        ; test_case "integer widens" `Quick test_parse_integer_value
        ; test_case "internal whitespace" `Quick test_parse_value_with_whitespace
        ; test_case "hyphenated name" `Quick test_parse_name_with_hyphen
        ] )
    ; ( "expected_name"
      , [ test_case "match" `Quick test_parse_expected_name_match
        ; test_case "mismatch" `Quick test_parse_expected_name_mismatch
        ] )
    ; ( "parse errors"
      , [ test_case "no tag" `Quick test_parse_no_tag
        ; test_case "duplicate" `Quick test_parse_duplicate_tags
        ; test_case "garbage value" `Quick test_parse_garbage_value
        ; test_case "empty value" `Quick test_parse_empty_value
        ] )
    ; ( "parse non-finite"
      , [ test_case "nan" `Quick test_parse_rejects_nan
        ; test_case "infinity" `Quick test_parse_rejects_infinity
        ; test_case "-infinity" `Quick test_parse_rejects_neg_infinity
        ] )
    ; ( "tag whitespace"
      , [ test_case "double space" `Quick test_parse_attribute_whitespace
        ; test_case "tab" `Quick test_parse_tab_separator
        ] )
    ]
;;
