(** Unit tests for Tool_input_validation — deterministic schema checking. *)

open Agent_sdk
open Alcotest

let make_param ?(required = true) ~param_type name =
  { Types.name; description = ""; param_type; required }
;;

let make_schema params =
  { Types.name = "test_tool"; description = "test"; parameters = params }
;;

(* ── Required field tests ──────────────────────────────── *)

let test_required_missing () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `Assoc [] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "path" "/room" (List.hd errors).path;
    check string "expected" "string" (List.hd errors).expected;
    check string "actual" "missing" (List.hd errors).actual
  | Tool_input_validation.Valid _ -> fail "expected Invalid for missing required field"
;;

let test_required_present () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `Assoc [ "room", `String "test-room" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "expected Valid for present required field"
;;

let test_optional_missing () =
  let schema =
    make_schema [ make_param ~required:false ~param_type:Types.Integer "timeout" ]
  in
  let input = `Assoc [] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "expected Valid for missing optional field"
;;

let test_null_treated_as_missing () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `Assoc [ "room", `Null ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "actual" "missing" (List.hd errors).actual
  | Tool_input_validation.Valid _ -> fail "expected Invalid for null required field"
;;

(* ── Type checking tests ──────────────────────────────── *)

let test_type_match_string () =
  let schema = make_schema [ make_param ~param_type:Types.String "msg" ] in
  let input = `Assoc [ "msg", `String "hello" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "string should match String"
;;

let test_type_match_integer () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ "count", `Int 42 ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "int should match Integer"
;;

let test_type_mismatch () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ "count", `String "sixty" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "path" "/count" (List.hd errors).path;
    check string "expected" "integer" (List.hd errors).expected
  | Tool_input_validation.Valid _ -> fail "non-numeric string should not match Integer"
;;

let test_int_is_valid_number () =
  let schema = make_schema [ make_param ~param_type:Types.Number "value" ] in
  let input = `Assoc [ "value", `Int 42 ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "int should be valid Number"
;;

(* ── Coercion tests ───────────────────────────────────── *)

let test_coerce_string_to_int () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ "count", `String "42" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "count" coerced in
    check string "coerced to int" "`Int (42)" (Yojson.Safe.show v)
  | Tool_input_validation.Invalid _ -> fail "\"42\" should coerce to integer"
;;

let test_coerce_string_to_bool () =
  let schema = make_schema [ make_param ~param_type:Types.Boolean "flag" ] in
  let input = `Assoc [ "flag", `String "true" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "flag" coerced in
    check string "coerced to bool" "`Bool (true)" (Yojson.Safe.show v)
  | Tool_input_validation.Invalid _ -> fail "\"true\" should coerce to boolean"
;;

let test_coerce_string_to_float () =
  let schema = make_schema [ make_param ~param_type:Types.Number "rate" ] in
  let input = `Assoc [ "rate", `String "3.14" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "rate" coerced in
    (match v with
     | `Float f -> check (float 0.01) "coerced value" 3.14 f
     | _ -> fail "expected float after coercion")
  | Tool_input_validation.Invalid _ -> fail "\"3.14\" should coerce to number"
;;

let test_coerce_int_to_number () =
  let schema = make_schema [ make_param ~param_type:Types.Number "value" ] in
  let input = `Assoc [ "value", `Int 5 ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> () (* int is already valid Number *)
  | Tool_input_validation.Invalid _ -> fail "int should be valid as Number"
;;

let test_no_coerce_non_numeric_string () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ "count", `String "sixty" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid _ -> () (* correct: can't coerce *)
  | Tool_input_validation.Valid _ -> fail "\"sixty\" should not coerce to integer"
;;

(* ── Edge cases ───────────────────────────────────────── *)

let test_empty_params () =
  let schema = make_schema [] in
  let input = `Assoc [ "anything", `String "goes" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "empty params should accept anything"
;;

let test_null_input () =
  let schema = make_schema [] in
  let input = `Null in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "null input with empty params should be valid"
;;

let test_multiple_errors () =
  let schema =
    make_schema
      [ make_param ~param_type:Types.String "room"
      ; make_param ~param_type:Types.Integer "timeout"
      ]
  in
  let input = `Assoc [] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors -> check int "two errors" 2 (List.length errors)
  | Tool_input_validation.Valid _ ->
    fail "expected two errors for two missing required fields"
;;

(* ── format_errors tests ──────────────────────────────── *)

let test_format_errors () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "/room"; expected = "required"; actual = "missing" }
    ; { path = "/count"; expected = "integer"; actual = "string(\"sixty\")" }
    ]
  in
  let msg = Tool_input_validation.format_errors ~tool_name:"test_tool" errors in
  check bool "contains tool name" true (String.length msg > 0);
  check
    bool
    "contains /room"
    true
    (let re = Re.(compile (str "/room")) in
     Re.execp re msg);
  check
    bool
    "contains /count"
    true
    (let re = Re.(compile (str "/count")) in
     Re.execp re msg);
  check
    bool
    "contains fix instruction"
    true
    (let re = Re.(compile (str "Fix the parameters")) in
     Re.execp re msg)
;;

let test_format_errors_inline_missing () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "/name"; expected = "string"; actual = "missing" } ]
  in
  let args = `Assoc [ "op", `String "find"; "pattern", `String "*.ml" ] in
  let msg =
    Tool_input_validation.format_errors_inline ~tool_name:"read_file" ~args errors
  in
  check
    bool
    "contains tool name"
    true
    (let re = Re.(compile (str "read_file")) in
     Re.execp re msg);
  check
    bool
    "contains original JSON"
    true
    (let re = Re.(compile (str "find")) in
     Re.execp re msg);
  check
    bool
    "contains MISSING marker"
    true
    (let re = Re.(compile (str "MISSING")) in
     Re.execp re msg);
  check
    bool
    "contains actual type"
    true
    (let re = Re.(compile (str "string")) in
     Re.execp re msg);
  check
    bool
    "contains field name"
    true
    (let re = Re.(compile (str "\"name\"")) in
     Re.execp re msg)
;;

let test_format_errors_inline_type_error () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "/count"; expected = "integer"; actual = "string(\"sixty\")" } ]
  in
  let args = `Assoc [ "count", `String "sixty" ] in
  let msg =
    Tool_input_validation.format_errors_inline ~tool_name:"test_tool" ~args errors
  in
  check
    bool
    "contains wrong type"
    true
    (let re = Re.(compile (str "wrong type")) in
     Re.execp re msg);
  check
    bool
    "contains expected"
    true
    (let re = Re.(compile (str "integer")) in
     Re.execp re msg);
  check
    bool
    "contains actual value"
    true
    (let re = Re.(compile (str "sixty")) in
     Re.execp re msg)
;;

let test_format_errors_inline_multiple () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "/name"; expected = "string"; actual = "missing" }
    ; { path = "/timeout"; expected = "number"; actual = "string(\"fast\")" }
    ]
  in
  let args = `Assoc [ "timeout", `String "fast" ] in
  let msg =
    Tool_input_validation.format_errors_inline ~tool_name:"test_tool" ~args errors
  in
  check
    bool
    "contains both errors"
    true
    (let re1 = Re.(compile (str "MISSING")) in
     let re2 = Re.(compile (str "wrong type")) in
     Re.execp re1 msg && Re.execp re2 msg)
;;

(* ── Low-level helper coverage ─────────────────────────── *)

let test_describe_json_value_variants () =
  let cases =
    [ `Null, "null"
    ; `Bool true, "boolean(true)"
    ; `Int 7, "integer(7)"
    ; `Float 3.5, "number(3.5)"
    ; `String "abcdefghijklmnopqrstuv", "string(\"abcdefghijklmnopqrst...\")"
    ; `List [], "array"
    ; `Assoc [], "object"
    ; `Intlit "9223372036854775808", "integer(9223372036854775808)"
    ]
  in
  List.iter
    (fun (json, expected) ->
       check string expected expected (Tool_input_validation.describe_json_value json))
    cases
;;

let check_coerce expected_type input expected =
  match Tool_input_validation.try_coerce expected_type input, expected with
  | Some actual, Some expected_json ->
    check bool "coerced json" true (Yojson.Safe.equal actual expected_json)
  | None, None -> ()
  | Some actual, None -> fail ("expected no coercion, got " ^ Yojson.Safe.to_string actual)
  | None, Some expected_json ->
    fail ("expected coercion to " ^ Yojson.Safe.to_string expected_json)
;;

let test_try_coerce_additional_shapes () =
  check_coerce Types.Integer (`Float 4.0) (Some (`Int 4));
  check_coerce Types.Integer (`Float 4.5) None;
  check_coerce Types.String (`Bool false) (Some (`String "false"));
  check_coerce Types.String (`Int 9) (Some (`String "9"));
  check_coerce Types.String (`Float 2.25) (Some (`String "2.25"));
  check_coerce Types.Integer (`Intlit "123") (Some (`Int 123));
  check_coerce Types.Integer (`Intlit "abc") None;
  check_coerce Types.Number (`Intlit "123") (Some (`Float 123.0));
  check_coerce Types.Number (`Intlit "abc") None
;;

let test_matches_type_additional_shapes () =
  check bool "array" true (Tool_input_validation.matches_type Types.Array (`List []));
  check bool "object" true (Tool_input_validation.matches_type Types.Object (`Assoc []));
  check
    bool
    "intlit integer"
    true
    (Tool_input_validation.matches_type Types.Integer (`Intlit "42"));
  check
    bool
    "intlit number"
    true
    (Tool_input_validation.matches_type Types.Number (`Intlit "42"));
  check
    bool
    "bool is not object"
    false
    (Tool_input_validation.matches_type Types.Object (`Bool true))
;;

let test_validate_raw_non_object_input () =
  let schema = make_schema [ make_param ~param_type:Types.Object "_raw" ] in
  match Tool_input_validation.validate schema (`String "not-object") with
  | Tool_input_validation.Invalid [ err ] ->
    check string "path" "/_raw" err.path;
    check string "expected" "object" err.expected;
    check string "actual" "string(\"not-object\")" err.actual
  | Tool_input_validation.Invalid errs ->
    fail ("expected one error, got " ^ string_of_int (List.length errs))
  | Tool_input_validation.Valid _ -> fail "expected raw object mismatch"
;;

let test_validate_null_input_with_required_field () =
  let schema = make_schema [ make_param ~param_type:Types.String "name" ] in
  match Tool_input_validation.validate schema `Null with
  | Tool_input_validation.Invalid [ err ] ->
    check string "path" "/name" err.path;
    check string "actual" Tool_input_validation.missing_actual err.actual
  | Tool_input_validation.Invalid errs ->
    fail ("expected one error, got " ^ string_of_int (List.length errs))
  | Tool_input_validation.Valid _ -> fail "expected required field error"
;;

let test_validate_intlit_and_exact_float_normalization () =
  let schema =
    make_schema
      [ make_param ~param_type:Types.Integer "count"
      ; make_param ~param_type:Types.Integer "exact"
      ]
  in
  let input = `Assoc [ "count", `Intlit "42"; "exact", `Float 5.0 ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    check
      bool
      "count normalized"
      true
      (Yojson.Safe.equal (Yojson.Safe.Util.member "count" coerced) (`Int 42));
    check
      bool
      "exact float normalized"
      true
      (Yojson.Safe.equal (Yojson.Safe.Util.member "exact" coerced) (`Int 5))
  | Tool_input_validation.Invalid errs ->
    fail (Tool_input_validation.format_errors ~tool_name:"test_tool" errs)
;;

(* ── Test runner ──────────────────────────────────────── *)

let () =
  run
    "tool_input_validation"
    [ ( "required"
      , [ test_case "missing required" `Quick test_required_missing
        ; test_case "present required" `Quick test_required_present
        ; test_case "optional missing" `Quick test_optional_missing
        ; test_case "null as missing" `Quick test_null_treated_as_missing
        ] )
    ; ( "type_check"
      , [ test_case "string matches String" `Quick test_type_match_string
        ; test_case "int matches Integer" `Quick test_type_match_integer
        ; test_case "type mismatch" `Quick test_type_mismatch
        ; test_case "int is valid Number" `Quick test_int_is_valid_number
        ] )
    ; ( "coercion"
      , [ test_case "string→int" `Quick test_coerce_string_to_int
        ; test_case "string→bool" `Quick test_coerce_string_to_bool
        ; test_case "string→float" `Quick test_coerce_string_to_float
        ; test_case "int→number" `Quick test_coerce_int_to_number
        ; test_case "non-numeric string fails" `Quick test_no_coerce_non_numeric_string
        ] )
    ; ( "edge_cases"
      , [ test_case "empty params" `Quick test_empty_params
        ; test_case "null input" `Quick test_null_input
        ; test_case "multiple errors" `Quick test_multiple_errors
        ] )
    ; ( "format"
      , [ test_case "format_errors output" `Quick test_format_errors
        ; test_case "inline: missing field" `Quick test_format_errors_inline_missing
        ; test_case "inline: type error" `Quick test_format_errors_inline_type_error
        ; test_case "inline: multiple errors" `Quick test_format_errors_inline_multiple
        ] )
    ; ( "helpers"
      , [ test_case
            "describe_json_value variants"
            `Quick
            test_describe_json_value_variants
        ; test_case
            "try_coerce additional shapes"
            `Quick
            test_try_coerce_additional_shapes
        ; test_case
            "matches_type additional shapes"
            `Quick
            test_matches_type_additional_shapes
        ; test_case "raw non-object input" `Quick test_validate_raw_non_object_input
        ; test_case
            "null input with required field"
            `Quick
            test_validate_null_input_with_required_field
        ; test_case
            "intlit and exact float normalization"
            `Quick
            test_validate_intlit_and_exact_float_normalization
        ] )
    ]
;;
