(** Unit tests for Lenient_json — deterministic LLM output recovery. *)

open Agent_sdk
open Alcotest

(* ── Helpers ─────────────────────────────────────── *)

let json = testable
  (fun ppf j -> Fmt.string ppf (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* ── Direct parse (fast path) ────────────────────── *)

let test_valid_json () =
  let result = Lenient_json.parse {|{"key": "val"}|} in
  check json "direct parse" (`Assoc [("key", `String "val")]) result

let test_valid_array () =
  let result = Lenient_json.parse {|[1, 2, 3]|} in
  check json "array" (`List [`Int 1; `Int 2; `Int 3]) result

(* ── Markdown fence strip ────────────────────────── *)

let test_fence_json () =
  let input = "```json\n{\"room\": \"test\"}\n```" in
  let result = Lenient_json.parse input in
  check json "json fence" (`Assoc [("room", `String "test")]) result

let test_fence_no_lang () =
  let input = "```\n{\"a\": 1}\n```" in
  let result = Lenient_json.parse input in
  check json "no lang fence" (`Assoc [("a", `Int 1)]) result

let test_fence_uppercase () =
  let input = "```JSON\n{\"b\": true}\n```" in
  let result = Lenient_json.parse input in
  check json "uppercase JSON fence" (`Assoc [("b", `Bool true)]) result

let test_strip_markdown_fence_unit () =
  let input = "```json\n{\"x\": 1}\n```" in
  check string "strip fence" {|{"x": 1}|} (Lenient_json.strip_markdown_fence input)

let test_no_fence () =
  let input = {|{"plain": true}|} in
  check string "no fence passthrough"
    {|{"plain": true}|}
    (Lenient_json.strip_markdown_fence input)

(* ── Double-stringify ────────────────────────────── *)

let test_double_stringify () =
  (* LLM sends: "{\"room\": \"lobby\"}" — a JSON string containing escaped JSON *)
  let input = {|"{\"room\": \"lobby\"}"|} in
  let result = Lenient_json.parse input in
  check json "double stringify" (`Assoc [("room", `String "lobby")]) result

let test_double_stringify_not_json () =
  (* Regular string that doesn't contain JSON *)
  let input = {|"hello world"|} in
  let result = Lenient_json.parse input in
  check json "regular string" (`String "hello world") result

let test_unwrap_double_stringify_unit () =
  let input = {|"{\"k\": 1}"|} in
  check string "unwrap" {|{"k": 1}|} (Lenient_json.unwrap_double_stringify input)

(* ── Trailing comma ──────────────────────────────── *)

let test_trailing_comma_object () =
  let input = {|{"a": 1, "b": 2,}|} in
  let result = Lenient_json.parse input in
  check json "trailing comma object"
    (`Assoc [("a", `Int 1); ("b", `Int 2)]) result

let test_trailing_comma_array () =
  let input = "[1, 2, 3,]" in
  let result = Lenient_json.parse input in
  check json "trailing comma array"
    (`List [`Int 1; `Int 2; `Int 3]) result

let test_trailing_comma_nested () =
  let input = {|{"items": [1, 2,], "x": 3,}|} in
  let result = Lenient_json.parse input in
  check json "trailing comma nested"
    (`Assoc [("items", `List [`Int 1; `Int 2]); ("x", `Int 3)]) result

let test_trailing_comma_in_string () =
  (* Comma inside a string should not be removed *)
  let input = {|{"msg": "a, b,"}|} in
  let result = Lenient_json.parse input in
  check json "comma in string preserved"
    (`Assoc [("msg", `String "a, b,")]) result

let test_strip_trailing_commas_unit () =
  check string "strip comma"
    {|{"a": 1}|}
    (Lenient_json.strip_trailing_commas {|{"a": 1,}|})

(* ── Keyword completion ──────────────────────────── *)

let test_complete_true () =
  let input = {|{"flag": tru|} in
  let result = Lenient_json.parse input in
  check json "complete true"
    (`Assoc [("flag", `Bool true)]) result

let test_complete_false () =
  let input = {|{"flag": fals|} in
  let result = Lenient_json.parse input in
  check json "complete false"
    (`Assoc [("flag", `Bool false)]) result

let test_complete_null () =
  let input = {|{"val": nul|} in
  let result = Lenient_json.parse input in
  check json "complete null"
    (`Assoc [("val", `Null)]) result

let test_complete_keywords_unit () =
  check string "complete tru" {|{"f": true|}
    (Lenient_json.complete_keywords {|{"f": tru|})

(* ── Unclosed brackets ──────────────────────────── *)

let test_unclosed_brace () =
  let input = {|{"room": "lobby"|} in
  let result = Lenient_json.parse input in
  check json "unclosed brace"
    (`Assoc [("room", `String "lobby")]) result

let test_unclosed_bracket () =
  let input = {|[1, 2, 3|} in
  let result = Lenient_json.parse input in
  check json "unclosed bracket"
    (`List [`Int 1; `Int 2; `Int 3]) result

let test_unclosed_nested () =
  let input = {|{"items": [1, 2|} in
  let result = Lenient_json.parse input in
  check json "unclosed nested"
    (`Assoc [("items", `List [`Int 1; `Int 2])]) result

let test_close_brackets_unit () =
  check string "close brace"
    {|{"a": 1}|}
    (Lenient_json.close_brackets {|{"a": 1|})

(* ── Combined recovery ──────────────────────────── *)

let test_fence_plus_trailing_comma () =
  let input = "```json\n{\"a\": 1,}\n```" in
  let result = Lenient_json.parse input in
  check json "fence + trailing comma"
    (`Assoc [("a", `Int 1)]) result

let test_fence_plus_unclosed () =
  let input = "```json\n{\"room\": \"test\"\n```" in
  let result = Lenient_json.parse input in
  check json "fence + unclosed"
    (`Assoc [("room", `String "test")]) result

let test_trailing_comma_plus_unclosed () =
  let input = {|{"a": 1, "b": 2,|} in
  let result = Lenient_json.parse input in
  check json "trailing comma + unclosed"
    (`Assoc [("a", `Int 1); ("b", `Int 2)]) result

(* ── Fallback ────────────────────────────────────── *)

let test_total_garbage () =
  let input = "this is not json at all" in
  let result = Lenient_json.parse input in
  check json "garbage fallback"
    (`Assoc [("raw", `String input)]) result

let test_empty_string () =
  let result = Lenient_json.parse "" in
  check json "empty fallback"
    (`Assoc [("raw", `String "")]) result

(* ── api_common integration ──────────────────────── *)

let test_api_common_uses_lenient () =
  (* Verify that api_common.json_of_string_or_raw now uses lenient parsing *)
  let input = {|{"key": "val",}|} in
  let result = Agent_sdk.Api.json_of_string_or_raw input in
  check json "api_common lenient"
    (`Assoc [("key", `String "val")]) result

(* ── Test runner ─────────────────────────────────── *)

let () =
  run "lenient_json"
    [
      ( "direct",
        [
          test_case "valid json" `Quick test_valid_json;
          test_case "valid array" `Quick test_valid_array;
        ] );
      ( "markdown_fence",
        [
          test_case "json fence" `Quick test_fence_json;
          test_case "no lang fence" `Quick test_fence_no_lang;
          test_case "uppercase JSON" `Quick test_fence_uppercase;
          test_case "strip unit" `Quick test_strip_markdown_fence_unit;
          test_case "no fence passthrough" `Quick test_no_fence;
        ] );
      ( "double_stringify",
        [
          test_case "unwrap" `Quick test_double_stringify;
          test_case "regular string" `Quick test_double_stringify_not_json;
          test_case "unwrap unit" `Quick test_unwrap_double_stringify_unit;
        ] );
      ( "trailing_comma",
        [
          test_case "object" `Quick test_trailing_comma_object;
          test_case "array" `Quick test_trailing_comma_array;
          test_case "nested" `Quick test_trailing_comma_nested;
          test_case "in string preserved" `Quick test_trailing_comma_in_string;
          test_case "strip unit" `Quick test_strip_trailing_commas_unit;
        ] );
      ( "keyword_completion",
        [
          test_case "true" `Quick test_complete_true;
          test_case "false" `Quick test_complete_false;
          test_case "null" `Quick test_complete_null;
          test_case "complete unit" `Quick test_complete_keywords_unit;
        ] );
      ( "unclosed_brackets",
        [
          test_case "brace" `Quick test_unclosed_brace;
          test_case "bracket" `Quick test_unclosed_bracket;
          test_case "nested" `Quick test_unclosed_nested;
          test_case "close unit" `Quick test_close_brackets_unit;
        ] );
      ( "combined",
        [
          test_case "fence + trailing comma" `Quick test_fence_plus_trailing_comma;
          test_case "fence + unclosed" `Quick test_fence_plus_unclosed;
          test_case "trailing + unclosed" `Quick test_trailing_comma_plus_unclosed;
        ] );
      ( "fallback",
        [
          test_case "garbage" `Quick test_total_garbage;
          test_case "empty string" `Quick test_empty_string;
        ] );
      ( "integration",
        [
          test_case "api_common uses lenient" `Quick test_api_common_uses_lenient;
        ] );
    ]
