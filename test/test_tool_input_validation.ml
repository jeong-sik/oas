(** Unit tests for Tool_input_validation — deterministic schema checking. *)

open Agent_sdk
open Alcotest

let make_param ?(required = true) ~param_type name =
  { Types.name; description = ""; param_type; required }
;;

let make_schema params =
  { Types.name = "test_tool"; description = "test"; parameters = params; strict = None }
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

let test_any_accepts_all_json_values () =
  let schema = make_schema [ make_param ~param_type:Types.Any_json "payload" ] in
  List.iter
    (fun value ->
       match Tool_input_validation.validate schema (`Assoc [ "payload", value ]) with
       | Tool_input_validation.Valid _ -> ()
       | Tool_input_validation.Invalid errors ->
         fail
           ("Any_json rejected value: "
            ^ Tool_input_validation.format_errors ~tool_name:"test_tool" errors))
    [ `Null; `String "x"; `Int 1; `Bool true; `List []; `Assoc [] ]
;;

let test_null_accepts_only_null () =
  let schema = make_schema [ make_param ~param_type:Types.Null "nothing" ] in
  (match Tool_input_validation.validate schema (`Assoc [ "nothing", `Null ]) with
   | Tool_input_validation.Valid _ -> ()
   | Tool_input_validation.Invalid errors ->
     fail
       ("Null rejected null: "
        ^ Tool_input_validation.format_errors ~tool_name:"test_tool" errors));
  match Tool_input_validation.validate schema (`Assoc [ "nothing", `String "null" ]) with
  | Tool_input_validation.Invalid _ -> ()
  | Tool_input_validation.Valid _ -> fail "Null accepted a non-null value"
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

let test_coerce_scalar_edges_and_intlit_normalization () =
  let schema =
    make_schema
      [ make_param ~param_type:Types.Integer "whole_float"
      ; make_param ~param_type:Types.String "bool_as_string"
      ; make_param ~param_type:Types.String "int_as_string"
      ; make_param ~param_type:Types.String "float_as_string"
      ; make_param ~param_type:Types.Integer "intlit_as_int"
      ; make_param ~param_type:Types.Number "intlit_as_number"
      ; make_param ~param_type:Types.Boolean "trimmed_bool"
      ]
  in
  let input =
    `Assoc
      [ "whole_float", `Float 7.0
      ; "bool_as_string", `Bool false
      ; "int_as_string", `Int 12
      ; "float_as_string", `Float 1.5
      ; "intlit_as_int", `Intlit "42"
      ; "intlit_as_number", `Intlit "42"
      ; "trimmed_bool", `String " TRUE "
      ]
  in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    check
      string
      "whole float"
      "`Int (7)"
      (Yojson.Safe.show (Yojson.Safe.Util.member "whole_float" coerced));
    check
      string
      "bool string"
      {|`String ("false")|}
      (Yojson.Safe.show (Yojson.Safe.Util.member "bool_as_string" coerced));
    check
      string
      "int string"
      {|`String ("12")|}
      (Yojson.Safe.show (Yojson.Safe.Util.member "int_as_string" coerced));
    check
      string
      "float string"
      {|`String ("1.5")|}
      (Yojson.Safe.show (Yojson.Safe.Util.member "float_as_string" coerced));
    check
      string
      "intlit int"
      "`Int (42)"
      (Yojson.Safe.show (Yojson.Safe.Util.member "intlit_as_int" coerced));
    check
      string
      "intlit number"
      "`Float (42.)"
      (Yojson.Safe.show (Yojson.Safe.Util.member "intlit_as_number" coerced));
    check
      string
      "trimmed bool"
      "`Bool (true)"
      (Yojson.Safe.show (Yojson.Safe.Util.member "trimmed_bool" coerced))
  | Tool_input_validation.Invalid _ -> fail "expected scalar coercions to succeed"
;;

let test_non_integral_float_to_integer_fails () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ "count", `Float 7.25 ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "actual" "number(7.25)" (List.hd errors).actual
  | Tool_input_validation.Valid _ -> fail "expected non-integral float to fail"
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

let test_optional_null_is_valid () =
  let schema =
    make_schema [ make_param ~required:false ~param_type:Types.Object "payload" ]
  in
  let input = `Assoc [ "payload", `Null ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "optional null should be accepted"
;;

let test_non_object_input_treats_declared_field_as_missing () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `List [ `String "not"; `String "an object" ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "path" "/room" (List.hd errors).path;
    check string "actual" "missing" (List.hd errors).actual
  | Tool_input_validation.Valid _ -> fail "expected declared field to be missing"
;;

let test_actual_descriptions_cover_json_shapes () =
  let schema =
    make_schema
      [ make_param ~param_type:Types.Integer "bool_value"
      ; make_param ~param_type:Types.Boolean "float_value"
      ; make_param ~param_type:Types.Integer "long_string"
      ; make_param ~param_type:Types.Object "array_value"
      ; make_param ~param_type:Types.Array "object_value"
      ; make_param ~param_type:Types.Boolean "bad_intlit"
      ]
  in
  let input =
    `Assoc
      [ "bool_value", `Bool true
      ; "float_value", `Float 1.25
      ; "long_string", `String "abcdefghijklmnopqrstuvwxyz"
      ; "array_value", `List [ `Int 1 ]
      ; "object_value", `Assoc [ "k", `String "v" ]
      ; "bad_intlit", `Intlit "999999999999999999999999999999"
      ]
  in
  let actual_for path errors =
    errors
    |> List.find (fun (e : Tool_input_validation.field_error) -> e.path = path)
    |> fun e -> e.actual
  in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check string "bool" "boolean(true)" (actual_for "/bool_value" errors);
    check string "float" "number(1.25)" (actual_for "/float_value" errors);
    check
      string
      "long string preview"
      {|string("abcdefghijklmnopqrst...")|}
      (actual_for "/long_string" errors);
    check string "array" "array" (actual_for "/array_value" errors);
    check string "object" "object" (actual_for "/object_value" errors);
    check
      string
      "intlit"
      "integer(999999999999999999999999999999)"
      (actual_for "/bad_intlit" errors);
    check int "six errors" 6 (List.length errors)
  | Tool_input_validation.Valid _ -> fail "expected shape description errors"
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

let test_format_errors_inline_path_without_slash () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "raw"; expected = "object"; actual = "array" } ]
  in
  let msg =
    Tool_input_validation.format_errors_inline
      ~tool_name:"test_tool"
      ~args:(`List [])
      errors
  in
  check
    bool
    "uses raw path as field name"
    true
    (let re = Re.(compile (str "\"raw\": wrong type")) in
     Re.execp re msg)
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
        ; test_case "any accepts all json values" `Quick test_any_accepts_all_json_values
        ; test_case "null accepts only null" `Quick test_null_accepts_only_null
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
        ; test_case
            "scalar edges and intlit normalization"
            `Quick
            test_coerce_scalar_edges_and_intlit_normalization
        ; test_case
            "non-integral float to integer fails"
            `Quick
            test_non_integral_float_to_integer_fails
        ] )
    ; ( "edge_cases"
      , [ test_case "empty params" `Quick test_empty_params
        ; test_case "null input" `Quick test_null_input
        ; test_case "multiple errors" `Quick test_multiple_errors
        ; test_case "optional null" `Quick test_optional_null_is_valid
        ; test_case
            "non-object input"
            `Quick
            test_non_object_input_treats_declared_field_as_missing
        ; test_case
            "actual descriptions"
            `Quick
            test_actual_descriptions_cover_json_shapes
        ] )
    ; ( "format"
      , [ test_case "format_errors output" `Quick test_format_errors
        ; test_case "inline: missing field" `Quick test_format_errors_inline_missing
        ; test_case "inline: type error" `Quick test_format_errors_inline_type_error
        ; test_case "inline: multiple errors" `Quick test_format_errors_inline_multiple
        ; test_case
            "inline: path without slash"
            `Quick
            test_format_errors_inline_path_without_slash
        ] )
    ]
;;
