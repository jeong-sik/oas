open Base
(** Deep coverage tests for Structured module.

    Targets uncovered branches in structured.ml (51 uncovered points).
    Focuses on: schema property ordering, complex parse functions,
    retry_result type construction, error path classification,
    and extract_tool_input edge cases not in existing test files. *)

open Agent_sdk
open Types

(* ── Helpers ────────────────────────────────────────────────── *)

let make_response ?(usage = None) content : Types.api_response =
  { id = "m"; model = "m"; stop_reason = EndTurn; content; usage; telemetry = None }
;;

(* ── Schema property ordering: fold_left + List.rev ─────────── *)

(** schema_to_tool_json uses fold_left then List.rev to preserve
    parameter declaration order in properties. Verify ordering. *)
let test_property_ordering_preserved () =
  let schema : unit Structured.schema =
    { name = "ordered"
    ; description = "Test ordering"
    ; params =
        [ { name = "first"; description = "1st"; param_type = String; required = true }
        ; { name = "second"; description = "2nd"; param_type = Integer; required = true }
        ; { name = "third"; description = "3rd"; param_type = Boolean; required = false }
        ]
    ; parse = (fun _ -> Ok ())
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let prop_names =
    json |> member "input_schema" |> member "properties" |> to_assoc |> List.map fst
  in
  Alcotest.(check (list string))
    "property order matches declaration"
    [ "first"; "second"; "third" ]
    prop_names
;;

(** Required list preserves filter_map order (declaration order). *)
let test_required_ordering () =
  let schema : unit Structured.schema =
    { name = "req_order"
    ; description = "Required ordering"
    ; params =
        [ { name = "z_last"; description = "Z"; param_type = String; required = true }
        ; { name = "a_first"; description = "A"; param_type = Integer; required = true }
        ; { name = "m_mid"; description = "M"; param_type = Number; required = false }
        ]
    ; parse = (fun _ -> Ok ())
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required =
    json |> member "input_schema" |> member "required" |> to_list |> List.map to_string
  in
  (* filter_map preserves order: z_last first, a_first second *)
  Alcotest.(check (list string))
    "required in declaration order"
    [ "z_last"; "a_first" ]
    required
;;

(* ── Complex parse functions ────────────────────────────────── *)

(** Schema with a parse that returns a nested record type. *)
let test_complex_parse_success () =
  let schema : (string * float * bool) Structured.schema =
    { name = "complex_parse"
    ; description = "Parse complex structure"
    ; params =
        [ { name = "label"; description = "Label"; param_type = String; required = true }
        ; { name = "score"; description = "Score"; param_type = Number; required = true }
        ; { name = "active"
          ; description = "Active"
          ; param_type = Boolean
          ; required = true
          }
        ]
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          try
            let label = json |> member "label" |> to_string in
            let score = json |> member "score" |> to_float in
            let active = json |> member "active" |> to_bool in
            Ok (label, score, active)
          with
          | exn -> Error (Printexc.to_string exn))
    }
  in
  let input_json =
    `Assoc [ "label", `String "test"; "score", `Float 0.95; "active", `Bool true ]
  in
  let content =
    [ ToolUse { id = "tu_cx"; name = "complex_parse"; input = input_json } ]
  in
  match Structured.extract_tool_input ~schema content with
  | Ok (label, score, active) ->
    Alcotest.(check string) "label" "test" label;
    Alcotest.(check (float 0.001)) "score" 0.95 score;
    Alcotest.(check bool) "active" true active
  | Error e -> Alcotest.fail ("unexpected: " ^ Error.to_string e)
;;

(** Parse function that returns Error with a detailed message. *)
let test_complex_parse_detailed_error () =
  let schema : int Structured.schema =
    { name = "detail_error"
    ; description = "Detailed error"
    ; params = []
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          match json |> member "value" with
          | `Null -> Error "value field is null"
          | `Int n when n < 0 -> Error "value must be non-negative"
          | `Int n -> Ok n
          | _ -> Error "value must be an integer")
    }
  in
  (* Test null value path *)
  let content_null =
    [ ToolUse { id = "tu_n"; name = "detail_error"; input = `Assoc [ "value", `Null ] } ]
  in
  (match Structured.extract_tool_input ~schema content_null with
   | Error (Error.Serialization (JsonParseError { detail })) ->
     Alcotest.(check string) "null error detail" "value field is null" detail
   | Error e -> Alcotest.failf "expected Serialization, got: %s" (Error.to_string e)
   | Ok _ -> Alcotest.fail "expected error");
  (* Test negative value path *)
  let content_neg =
    [ ToolUse
        { id = "tu_neg"; name = "detail_error"; input = `Assoc [ "value", `Int (-5) ] }
    ]
  in
  (match Structured.extract_tool_input ~schema content_neg with
   | Error (Error.Serialization (JsonParseError { detail })) ->
     Alcotest.(check string) "neg error detail" "value must be non-negative" detail
   | Error e -> Alcotest.failf "expected Serialization, got: %s" (Error.to_string e)
   | Ok _ -> Alcotest.fail "expected error");
  (* Test non-integer path *)
  let content_str =
    [ ToolUse
        { id = "tu_s"
        ; name = "detail_error"
        ; input = `Assoc [ "value", `String "not a number" ]
        }
    ]
  in
  match Structured.extract_tool_input ~schema content_str with
  | Error (Error.Serialization (JsonParseError { detail })) ->
    Alcotest.(check string) "string error detail" "value must be an integer" detail
  | Error e -> Alcotest.failf "expected Serialization, got: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "expected error"
;;

(* ── extract_tool_input: multiple ToolUse with different names ── *)

(** When content has ToolUse blocks for different schemas, only the
    matching one is extracted. *)
let test_extract_skips_unrelated_tools () =
  let schema : string Structured.schema =
    { name = "target_tool"
    ; description = "Target"
    ; params = []
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          Ok (json |> member "data" |> to_string))
    }
  in
  let content =
    [ ToolUse { id = "tu_a"; name = "other_tool_1"; input = `Assoc [ "x", `Int 1 ] }
    ; ToolUse { id = "tu_b"; name = "other_tool_2"; input = `Assoc [ "y", `Int 2 ] }
    ; ToolUse
        { id = "tu_c"; name = "target_tool"; input = `Assoc [ "data", `String "found" ] }
    ; ToolUse
        { id = "tu_d"; name = "target_tool"; input = `Assoc [ "data", `String "second" ] }
    ]
  in
  match Structured.extract_tool_input ~schema content with
  | Ok v -> Alcotest.(check string) "first matching tool" "found" v
  | Error e -> Alcotest.fail ("unexpected: " ^ Error.to_string e)
;;

(* ── json_extractor with different JSON structures ──────────── *)

(** json_extractor parsing a JSON array from text block. *)
let test_json_extractor_array () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> to_list |> List.map to_int)
  in
  let resp = make_response [ Text {|[1, 2, 3]|} ] in
  match extract resp with
  | Ok v -> Alcotest.(check (list int)) "parsed array" [ 1; 2; 3 ] v
  | Error e -> Alcotest.fail e
;;

(** json_extractor with nested JSON. *)
let test_json_extractor_nested () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> member "outer" |> member "inner" |> to_string)
  in
  let resp = make_response [ Text {|{"outer": {"inner": "deep"}}|} ] in
  match extract resp with
  | Ok v -> Alcotest.(check string) "nested value" "deep" v
  | Error e -> Alcotest.fail e
;;

(** json_extractor picks first text, ignoring ToolUse blocks. *)
let test_json_extractor_ignores_tool_use () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> member "answer" |> to_int)
  in
  let resp =
    make_response
      [ ToolUse { id = "t1"; name = "tool"; input = `Assoc [ "answer", `Int 999 ] }
      ; Text {|{"answer": 42}|}
      ]
  in
  match extract resp with
  | Ok v -> Alcotest.(check int) "from text not tool_use" 42 v
  | Error e -> Alcotest.fail e
;;

(* ── text_extractor edge cases ──────────────────────────────── *)

(** text_extractor with multi-line text. *)
let test_text_extractor_multiline () =
  let extract =
    Structured.text_extractor (fun s ->
      let lines = String.split_on_char '\n' s in
      Some (List.length lines))
  in
  let resp = make_response [ Text "line1\nline2\nline3" ] in
  match extract resp with
  | Ok v -> Alcotest.(check int) "3 lines" 3 v
  | Error e -> Alcotest.fail e
;;

(** text_extractor with empty string text. *)
let test_text_extractor_empty_string () =
  let extract = Structured.text_extractor (fun s -> if s = "" then None else Some s) in
  let resp = make_response [ Text "" ] in
  match extract resp with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected None for empty string"
;;

(* ── Schema with all 6 param types ─────────────────────────── *)

let test_schema_all_six_param_types () =
  let schema : unit Structured.schema =
    { name = "all_six"
    ; description = "All 6 types"
    ; params =
        [ { name = "s"; description = "string"; param_type = String; required = true }
        ; { name = "i"; description = "integer"; param_type = Integer; required = true }
        ; { name = "n"; description = "number"; param_type = Number; required = true }
        ; { name = "b"; description = "boolean"; param_type = Boolean; required = true }
        ; { name = "a"; description = "array"; param_type = Array; required = true }
        ; { name = "o"; description = "object"; param_type = Object; required = true }
        ]
    ; parse = (fun _ -> Ok ())
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  Alcotest.(check string)
    "string"
    "string"
    (props |> member "s" |> member "type" |> to_string);
  Alcotest.(check string)
    "integer"
    "integer"
    (props |> member "i" |> member "type" |> to_string);
  Alcotest.(check string)
    "number"
    "number"
    (props |> member "n" |> member "type" |> to_string);
  Alcotest.(check string)
    "boolean"
    "boolean"
    (props |> member "b" |> member "type" |> to_string);
  Alcotest.(check string)
    "array"
    "array"
    (props |> member "a" |> member "type" |> to_string);
  Alcotest.(check string)
    "object"
    "object"
    (props |> member "o" |> member "type" |> to_string);
  let required =
    json |> member "input_schema" |> member "required" |> to_list |> List.map to_string
  in
  Alcotest.(check int) "6 required" 6 (List.length required)
;;

(* ── Schema JSON structure validation ───────────────────────── *)

(** Verify the full JSON structure matches Anthropic tool definition format. *)
let test_schema_json_full_structure () =
  let schema : string Structured.schema =
    { name = "get_weather"
    ; description = "Get weather for a city"
    ; params =
        [ { name = "city"
          ; description = "City name"
          ; param_type = String
          ; required = true
          }
        ; { name = "unit"
          ; description = "Temperature unit"
          ; param_type = String
          ; required = false
          }
        ]
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          Ok (json |> member "city" |> to_string))
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  (* Top-level keys *)
  let keys = json |> to_assoc |> List.map fst in
  Alcotest.(check (list string))
    "top-level keys"
    [ "name"; "description"; "input_schema" ]
    keys;
  (* input_schema keys *)
  let is_keys = json |> member "input_schema" |> to_assoc |> List.map fst in
  Alcotest.(check (list string))
    "input_schema keys"
    [ "type"; "properties"; "required" ]
    is_keys;
  (* Property descriptions preserved *)
  let city_desc =
    json
    |> member "input_schema"
    |> member "properties"
    |> member "city"
    |> member "description"
    |> to_string
  in
  Alcotest.(check string) "city description" "City name" city_desc
;;

(* ── extract_tool_input with ToolUse containing complex input ── *)

(** Extract from ToolUse where input is a deeply nested JSON. *)
let test_extract_deeply_nested_input () =
  let schema : string Structured.schema =
    { name = "nested_tool"
    ; description = "Nested"
    ; params = []
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          Ok (json |> member "level1" |> member "level2" |> member "value" |> to_string))
    }
  in
  let nested_input =
    `Assoc [ "level1", `Assoc [ "level2", `Assoc [ "value", `String "deep_value" ] ] ]
  in
  let content =
    [ ToolUse { id = "tu_deep"; name = "nested_tool"; input = nested_input } ]
  in
  match Structured.extract_tool_input ~schema content with
  | Ok v -> Alcotest.(check string) "deep value" "deep_value" v
  | Error e -> Alcotest.fail ("unexpected: " ^ Error.to_string e)
;;

(* ── retry_result type properties ───────────────────────────── *)

(** Test retry_result construction via extract_tool_input.
    Since extract_with_retry needs API, we test the type itself. *)
let test_retry_result_type () =
  let result : string Structured.retry_result =
    { value = "test"
    ; total_usage =
        { total_input_tokens = 100
        ; total_output_tokens = 50
        ; total_cache_creation_input_tokens = 10
        ; total_cache_read_input_tokens = 5
        ; api_calls = 2
        ; estimated_cost_usd = 0.0
        }
    ; attempts = 2
    }
  in
  Alcotest.(check string) "value" "test" result.value;
  Alcotest.(check int) "attempts" 2 result.attempts;
  Alcotest.(check int) "input_tokens" 100 result.total_usage.total_input_tokens;
  Alcotest.(check int) "output_tokens" 50 result.total_usage.total_output_tokens;
  Alcotest.(check int)
    "cache_creation"
    10
    result.total_usage.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 5 result.total_usage.total_cache_read_input_tokens;
  Alcotest.(check int) "api_calls" 2 result.total_usage.api_calls
;;

let test_retry_result_no_usage () =
  let result : int Structured.retry_result =
    { value = 42; total_usage = Types.empty_usage; attempts = 1 }
  in
  Alcotest.(check int) "value" 42 result.value;
  Alcotest.(check int) "attempts" 1 result.attempts;
  Alcotest.(check int) "no api calls" 0 result.total_usage.api_calls
;;

(* ── Suite ──────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "structured_deep"
    [ ( "schema_ordering"
      , [ Alcotest.test_case
            "property ordering preserved"
            `Quick
            test_property_ordering_preserved
        ; Alcotest.test_case "required ordering" `Quick test_required_ordering
        ] )
    ; ( "complex_parse"
      , [ Alcotest.test_case "nested record parse" `Quick test_complex_parse_success
        ; Alcotest.test_case
            "detailed error paths"
            `Quick
            test_complex_parse_detailed_error
        ] )
    ; ( "extract_tool_input_deep"
      , [ Alcotest.test_case
            "skips unrelated tools"
            `Quick
            test_extract_skips_unrelated_tools
        ; Alcotest.test_case "deeply nested input" `Quick test_extract_deeply_nested_input
        ] )
    ; ( "json_extractor_deep"
      , [ Alcotest.test_case "parse array" `Quick test_json_extractor_array
        ; Alcotest.test_case "parse nested" `Quick test_json_extractor_nested
        ; Alcotest.test_case
            "ignores tool_use"
            `Quick
            test_json_extractor_ignores_tool_use
        ] )
    ; ( "text_extractor_deep"
      , [ Alcotest.test_case "multiline" `Quick test_text_extractor_multiline
        ; Alcotest.test_case "empty string" `Quick test_text_extractor_empty_string
        ] )
    ; ( "schema_structure"
      , [ Alcotest.test_case "all 6 param types" `Quick test_schema_all_six_param_types
        ; Alcotest.test_case "full JSON structure" `Quick test_schema_json_full_structure
        ] )
    ; ( "retry_result"
      , [ Alcotest.test_case "with usage" `Quick test_retry_result_type
        ; Alcotest.test_case "no usage" `Quick test_retry_result_no_usage
        ] )
    ]
;;
