(** Unit tests for Tool_input_validation — deterministic schema checking. *)

open Agent_sdk
open Alcotest

let make_param ?(required = true) ~param_type name =
  { Types.name; description = ""; param_type; required }

let make_schema params =
  { Types.name = "test_tool"; description = "test"; parameters = params }

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
  | Tool_input_validation.Valid _ ->
    fail "expected Invalid for missing required field"

let test_required_present () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `Assoc [ ("room", `String "test-room") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ ->
    fail "expected Valid for present required field"

let test_optional_missing () =
  let schema = make_schema [
    make_param ~required:false ~param_type:Types.Integer "timeout"
  ] in
  let input = `Assoc [] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ ->
    fail "expected Valid for missing optional field"

let test_null_treated_as_missing () =
  let schema = make_schema [ make_param ~param_type:Types.String "room" ] in
  let input = `Assoc [ ("room", `Null) ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "actual" "missing" (List.hd errors).actual
  | Tool_input_validation.Valid _ ->
    fail "expected Invalid for null required field"

(* ── Type checking tests ──────────────────────────────── *)

let test_type_match_string () =
  let schema = make_schema [ make_param ~param_type:Types.String "msg" ] in
  let input = `Assoc [ ("msg", `String "hello") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "string should match String"

let test_type_match_integer () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ ("count", `Int 42) ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "int should match Integer"

let test_type_mismatch () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ ("count", `String "sixty") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "one error" 1 (List.length errors);
    check string "path" "/count" (List.hd errors).path;
    check string "expected" "integer" (List.hd errors).expected
  | Tool_input_validation.Valid _ ->
    fail "non-numeric string should not match Integer"

let test_int_is_valid_number () =
  let schema = make_schema [ make_param ~param_type:Types.Number "value" ] in
  let input = `Assoc [ ("value", `Int 42) ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ -> fail "int should be valid Number"

(* ── Coercion tests ───────────────────────────────────── *)

let test_coerce_string_to_int () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ ("count", `String "42") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "count" coerced in
    check string "coerced to int" "`Int (42)" (Yojson.Safe.show v)
  | Tool_input_validation.Invalid _ ->
    fail "\"42\" should coerce to integer"

let test_coerce_string_to_bool () =
  let schema = make_schema [ make_param ~param_type:Types.Boolean "flag" ] in
  let input = `Assoc [ ("flag", `String "true") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "flag" coerced in
    check string "coerced to bool" "`Bool (true)" (Yojson.Safe.show v)
  | Tool_input_validation.Invalid _ ->
    fail "\"true\" should coerce to boolean"

let test_coerce_string_to_float () =
  let schema = make_schema [ make_param ~param_type:Types.Number "rate" ] in
  let input = `Assoc [ ("rate", `String "3.14") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid coerced ->
    let v = Yojson.Safe.Util.member "rate" coerced in
    (match v with
     | `Float f -> check (float 0.01) "coerced value" 3.14 f
     | _ -> fail "expected float after coercion")
  | Tool_input_validation.Invalid _ ->
    fail "\"3.14\" should coerce to number"

let test_coerce_int_to_number () =
  let schema = make_schema [ make_param ~param_type:Types.Number "value" ] in
  let input = `Assoc [ ("value", `Int 5) ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()  (* int is already valid Number *)
  | Tool_input_validation.Invalid _ ->
    fail "int should be valid as Number"

let test_no_coerce_non_numeric_string () =
  let schema = make_schema [ make_param ~param_type:Types.Integer "count" ] in
  let input = `Assoc [ ("count", `String "sixty") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid _ -> ()  (* correct: can't coerce *)
  | Tool_input_validation.Valid _ ->
    fail "\"sixty\" should not coerce to integer"

(* ── Edge cases ───────────────────────────────────────── *)

let test_empty_params () =
  let schema = make_schema [] in
  let input = `Assoc [ ("anything", `String "goes") ] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ ->
    fail "empty params should accept anything"

let test_null_input () =
  let schema = make_schema [] in
  let input = `Null in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Valid _ -> ()
  | Tool_input_validation.Invalid _ ->
    fail "null input with empty params should be valid"

let test_multiple_errors () =
  let schema = make_schema [
    make_param ~param_type:Types.String "room";
    make_param ~param_type:Types.Integer "timeout";
  ] in
  let input = `Assoc [] in
  match Tool_input_validation.validate schema input with
  | Tool_input_validation.Invalid errors ->
    check int "two errors" 2 (List.length errors)
  | Tool_input_validation.Valid _ ->
    fail "expected two errors for two missing required fields"

(* ── format_errors tests ──────────────────────────────── *)

let test_format_errors () =
  let errors : Tool_input_validation.field_error list = [
    { path = "/room"; expected = "required"; actual = "missing" };
    { path = "/count"; expected = "integer"; actual = "string(\"sixty\")" };
  ] in
  let msg = Tool_input_validation.format_errors ~tool_name:"test_tool" errors in
  check bool "contains tool name" true
    (String.length msg > 0);
  check bool "contains /room" true
    (let re = Re.(compile (str "/room")) in Re.execp re msg);
  check bool "contains /count" true
    (let re = Re.(compile (str "/count")) in Re.execp re msg);
  check bool "contains fix instruction" true
    (let re = Re.(compile (str "Fix the parameters")) in Re.execp re msg)

let test_format_errors_inline_missing () =
  let errors : Tool_input_validation.field_error list = [
    { path = "/name"; expected = "string"; actual = "missing" };
  ] in
  let args = `Assoc [ ("op", `String "find"); ("pattern", `String "*.ml") ] in
  let msg = Tool_input_validation.format_errors_inline
    ~tool_name:"keeper_shell_readonly" ~args errors in
  check bool "contains tool name" true
    (let re = Re.(compile (str "keeper_shell_readonly")) in Re.execp re msg);
  check bool "contains original JSON" true
    (let re = Re.(compile (str "find")) in Re.execp re msg);
  check bool "contains MISSING marker" true
    (let re = Re.(compile (str "MISSING")) in Re.execp re msg);
  check bool "contains actual type" true
    (let re = Re.(compile (str "string")) in Re.execp re msg);
  check bool "contains field name" true
    (let re = Re.(compile (str "\"name\"")) in Re.execp re msg)

let test_format_errors_inline_type_error () =
  let errors : Tool_input_validation.field_error list = [
    { path = "/count"; expected = "integer"; actual = "string(\"sixty\")" };
  ] in
  let args = `Assoc [ ("count", `String "sixty") ] in
  let msg = Tool_input_validation.format_errors_inline
    ~tool_name:"test_tool" ~args errors in
  check bool "contains wrong type" true
    (let re = Re.(compile (str "wrong type")) in Re.execp re msg);
  check bool "contains expected" true
    (let re = Re.(compile (str "integer")) in Re.execp re msg);
  check bool "contains actual value" true
    (let re = Re.(compile (str "sixty")) in Re.execp re msg)

let test_format_errors_inline_multiple () =
  let errors : Tool_input_validation.field_error list = [
    { path = "/name"; expected = "string"; actual = "missing" };
    { path = "/timeout"; expected = "number"; actual = "string(\"fast\")" };
  ] in
  let args = `Assoc [ ("timeout", `String "fast") ] in
  let msg = Tool_input_validation.format_errors_inline
    ~tool_name:"test_tool" ~args errors in
  check bool "contains both errors" true
    (let re1 = Re.(compile (str "MISSING")) in
     let re2 = Re.(compile (str "wrong type")) in
     Re.execp re1 msg && Re.execp re2 msg)

(* ── Test runner ──────────────────────────────────────── *)

let () =
  run "tool_input_validation"
    [
      ( "required",
        [
          test_case "missing required" `Quick test_required_missing;
          test_case "present required" `Quick test_required_present;
          test_case "optional missing" `Quick test_optional_missing;
          test_case "null as missing" `Quick test_null_treated_as_missing;
        ] );
      ( "type_check",
        [
          test_case "string matches String" `Quick test_type_match_string;
          test_case "int matches Integer" `Quick test_type_match_integer;
          test_case "type mismatch" `Quick test_type_mismatch;
          test_case "int is valid Number" `Quick test_int_is_valid_number;
        ] );
      ( "coercion",
        [
          test_case "string→int" `Quick test_coerce_string_to_int;
          test_case "string→bool" `Quick test_coerce_string_to_bool;
          test_case "string→float" `Quick test_coerce_string_to_float;
          test_case "int→number" `Quick test_coerce_int_to_number;
          test_case "non-numeric string fails" `Quick test_no_coerce_non_numeric_string;
        ] );
      ( "edge_cases",
        [
          test_case "empty params" `Quick test_empty_params;
          test_case "null input" `Quick test_null_input;
          test_case "multiple errors" `Quick test_multiple_errors;
        ] );
      ( "format",
        [
          test_case "format_errors output" `Quick test_format_errors;
          test_case "inline: missing field" `Quick test_format_errors_inline_missing;
          test_case "inline: type error" `Quick test_format_errors_inline_type_error;
          test_case "inline: multiple errors" `Quick test_format_errors_inline_multiple;
        ] );
    ]
