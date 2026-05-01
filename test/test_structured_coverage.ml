open Base
(** Additional coverage tests for Structured module.

    Targets uncovered branches in json_extractor (Type_error, Failure),
    text_extractor edge cases, and extract_tool_input variants
    not covered by test_structured.ml. *)

open Agent_sdk
open Types

(* ── Helpers ────────────────────────────────────────────────────── *)

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)

let make_response content : Types.api_response =
  { id = "m"
  ; model = "m"
  ; stop_reason = EndTurn
  ; content
  ; usage = None
  ; telemetry = None
  }
;;

let person_schema : (string * int) Structured.schema =
  { name = "extract_person"
  ; description = "Extract person info"
  ; params =
      [ { name = "name"
        ; description = "Person name"
        ; param_type = String
        ; required = true
        }
      ; { name = "age"
        ; description = "Person age"
        ; param_type = Integer
        ; required = true
        }
      ]
  ; parse =
      (fun json ->
        let open Yojson.Safe.Util in
        try
          let name = json |> member "name" |> to_string in
          let age = json |> member "age" |> to_int in
          Ok (name, age)
        with
        | exn -> Error (Printexc.to_string exn))
  }
;;

(* ── json_extractor: Type_error branch ──────────────────────────── *)

let test_json_extractor_type_error () =
  (* Parser that raises Yojson.Safe.Util.Type_error *)
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> to_int (* will fail on non-int *))
  in
  let resp = make_response [ Text {|"not an int"|} ] in
  match extract resp with
  | Error msg -> check_bool "mentions JSON type" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected Type_error"
;;

(* ── json_extractor: Failure branch ─────────────────────────────── *)

let test_json_extractor_failure () =
  (* Parser that raises Failure *)
  let extract =
    Structured.json_extractor (fun _json -> failwith "custom parse failure")
  in
  let resp = make_response [ Text {|{"valid": true}|} ] in
  match extract resp with
  | Error msg ->
    check_bool "mentions failure" true (String.length msg > 0);
    (* Verify it's the parse failure path *)
    check_bool
      "contains 'parse failure'"
      true
      (let target = "parse failure" in
       let tlen = String.length target in
       let slen = String.length msg in
       let rec has i =
         i + tlen <= slen && (String.sub msg i tlen = target || has (i + 1))
       in
       has 0)
  | Ok _ -> Alcotest.fail "expected Failure"
;;

(* ── json_extractor: takes first text block ─────────────────────── *)

let test_json_extractor_takes_first_text () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> member "v" |> to_int)
  in
  let resp = make_response [ Text {|{"v": 1}|}; Text {|{"v": 2}|} ] in
  match extract resp with
  | Ok v -> Alcotest.(check int) "first text" 1 v
  | Error e -> Alcotest.fail e
;;

(* ── json_extractor: skips non-text blocks ──────────────────────── *)

let test_json_extractor_skips_non_text () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> member "v" |> to_int)
  in
  let resp =
    make_response
      [ Thinking { thinking_type = "s"; content = "thinking" }
      ; ToolUse { id = "tu"; name = "tool"; input = `Null }
      ; Text {|{"v": 99}|}
      ]
  in
  match extract resp with
  | Ok v -> Alcotest.(check int) "after non-text" 99 v
  | Error e -> Alcotest.fail e
;;

(* ── text_extractor: no text content ────────────────────────────── *)

let test_text_extractor_no_text_content () =
  let extract = Structured.text_extractor (fun _ -> Some 0) in
  let resp = make_response [] in
  match extract resp with
  | Error msg -> check_bool "mentions no text" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"
;;

let test_text_extractor_only_non_text () =
  let extract = Structured.text_extractor (fun _ -> Some 0) in
  let resp =
    make_response
      [ ToolUse { id = "t"; name = "n"; input = `Null }
      ; Thinking { thinking_type = "s"; content = "x" }
      ]
  in
  match extract resp with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-text only"
;;

(* ── text_extractor: takes first text ───────────────────────────── *)

let test_text_extractor_takes_first () =
  let extract = Structured.text_extractor (fun s -> Some (String.length s)) in
  let resp = make_response [ Text "short"; Text "much longer text" ] in
  match extract resp with
  | Ok v -> Alcotest.(check int) "first text length" 5 v
  | Error e -> Alcotest.fail e
;;

(* ── text_extractor: parse returns None ─────────────────────────── *)

let test_text_extractor_parse_returns_none () =
  let extract = Structured.text_extractor (fun _ -> None) in
  let resp = make_response [ Text "anything" ] in
  match extract resp with
  | Error msg -> check_bool "mentions None" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"
;;

(* ── extract_tool_input: Image block ignored ────────────────────── *)

let test_extract_ignores_image () =
  let input_json = `Assoc [ "name", `String "Alice"; "age", `Int 30 ] in
  let content =
    [ Image { media_type = "image/png"; data = "abc"; source_type = "base64" }
    ; ToolUse { id = "tu_img"; name = "extract_person"; input = input_json }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    check_string "name" "Alice" name;
    Alcotest.(check int) "age" 30 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)
;;

(* ── extract_tool_input: Document block ignored ─────────────────── *)

let test_extract_ignores_document () =
  let input_json = `Assoc [ "name", `String "Bob"; "age", `Int 25 ] in
  let content =
    [ Document { media_type = "application/pdf"; data = "x"; source_type = "base64" }
    ; ToolUse { id = "tu_doc"; name = "extract_person"; input = input_json }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, _) -> check_string "name" "Bob" name
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)
;;

(* ── extract_tool_input: RedactedThinking ignored ───────────────── *)

let test_extract_ignores_redacted_thinking () =
  let input_json = `Assoc [ "name", `String "Carol"; "age", `Int 40 ] in
  let content =
    [ RedactedThinking "redacted"
    ; ToolUse { id = "tu_rt"; name = "extract_person"; input = input_json }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, _) -> check_string "name" "Carol" name
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)
;;

(* ── extract_tool_input: only wrong-name tools ──────────────────── *)

let test_extract_only_wrong_name_tools () =
  let content =
    [ ToolUse { id = "tu_a"; name = "wrong_tool_a"; input = `Null }
    ; ToolUse { id = "tu_b"; name = "wrong_tool_b"; input = `Null }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error (Error.Internal msg) ->
    check_bool
      "mentions schema name"
      true
      (let tgt = "extract_person" in
       let tlen = String.length tgt in
       let slen = String.length msg in
       let rec has i = i + tlen <= slen && (String.sub msg i tlen = tgt || has (i + 1)) in
       has 0)
  | Error _ -> Alcotest.fail "expected Internal error"
  | Ok _ -> Alcotest.fail "expected error"
;;

(* ── schema_to_tool_json: Array and Object param types ──────────── *)

let test_schema_array_object_param_types () =
  let schema : unit Structured.schema =
    { name = "complex_types"
    ; description = "Test array/object types"
    ; params =
        [ { name = "items"
          ; description = "An array"
          ; param_type = Array
          ; required = true
          }
        ; { name = "config"
          ; description = "An object"
          ; param_type = Object
          ; required = false
          }
        ]
    ; parse = (fun _ -> Ok ())
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  check_string "array type" "array" (props |> member "items" |> member "type" |> to_string);
  check_string
    "object type"
    "object"
    (props |> member "config" |> member "type" |> to_string)
;;

(* ── schema_to_tool_json: single required param ─────────────────── *)

let test_schema_single_required () =
  let schema : string Structured.schema =
    { name = "single"
    ; description = "Single param"
    ; params =
        [ { name = "value"
          ; description = "The value"
          ; param_type = String
          ; required = true
          }
        ]
    ; parse =
        (fun json ->
          let open Yojson.Safe.Util in
          try Ok (json |> member "value" |> to_string) with
          | exn -> Error (Printexc.to_string exn))
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required =
    json |> member "input_schema" |> member "required" |> to_list |> List.map to_string
  in
  Alcotest.(check int) "1 required" 1 (List.length required);
  check_string "required name" "value" (List.hd required)
;;

(* ── schema_to_tool_json: all optional params ───────────────────── *)

let test_schema_all_optional () =
  let schema : unit Structured.schema =
    { name = "all_optional"
    ; description = "All optional"
    ; params =
        [ { name = "a"; description = "A"; param_type = String; required = false }
        ; { name = "b"; description = "B"; param_type = Integer; required = false }
        ]
    ; parse = (fun _ -> Ok ())
    }
  in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required = json |> member "input_schema" |> member "required" |> to_list in
  Alcotest.(check int) "0 required" 0 (List.length required)
;;

(* ── extract_tool_input: Audio block ignored ────────────────────── *)

let test_extract_ignores_audio () =
  let input_json = `Assoc [ "name", `String "Dan"; "age", `Int 35 ] in
  let content =
    [ Audio { media_type = "audio/mp3"; data = "bin"; source_type = "base64" }
    ; ToolUse { id = "tu_aud"; name = "extract_person"; input = input_json }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, _) -> check_string "name" "Dan" name
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)
;;

(* ── json_extractor: valid JSON but wrong structure ─────────────── *)

let test_json_extractor_wrong_structure () =
  let extract =
    Structured.json_extractor (fun json ->
      let open Yojson.Safe.Util in
      json |> member "nonexistent" |> to_string)
  in
  let resp = make_response [ Text {|{"other": 42}|} ] in
  match extract resp with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for missing field"
;;

(* ── extract_tool_input: mixed block types with schema match ────── *)

let test_extract_mixed_blocks () =
  let input_json = `Assoc [ "name", `String "Eve"; "age", `Int 50 ] in
  let content =
    [ Text "preamble"
    ; Thinking { thinking_type = "s"; content = "reasoning" }
    ; ToolResult
        { tool_use_id = "old"; content = "old result"; is_error = false; json = None }
    ; Image { media_type = "image/png"; data = "img"; source_type = "base64" }
    ; ToolUse { id = "tu_mix"; name = "wrong_tool"; input = `Null }
    ; ToolUse { id = "tu_right"; name = "extract_person"; input = input_json }
    ]
  in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    check_string "name" "Eve" name;
    Alcotest.(check int) "age" 50 age
  | Error e -> Alcotest.fail ("unexpected: " ^ Error.to_string e)
;;

(* ── Suite ──────────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "structured_coverage"
    [ ( "json_extractor"
      , [ Alcotest.test_case "type_error" `Quick test_json_extractor_type_error
        ; Alcotest.test_case "failure" `Quick test_json_extractor_failure
        ; Alcotest.test_case
            "takes first text"
            `Quick
            test_json_extractor_takes_first_text
        ; Alcotest.test_case "skips non-text" `Quick test_json_extractor_skips_non_text
        ; Alcotest.test_case "wrong structure" `Quick test_json_extractor_wrong_structure
        ] )
    ; ( "text_extractor"
      , [ Alcotest.test_case "no text content" `Quick test_text_extractor_no_text_content
        ; Alcotest.test_case "only non-text" `Quick test_text_extractor_only_non_text
        ; Alcotest.test_case "takes first" `Quick test_text_extractor_takes_first
        ; Alcotest.test_case
            "parse returns None"
            `Quick
            test_text_extractor_parse_returns_none
        ] )
    ; ( "extract_tool_input_blocks"
      , [ Alcotest.test_case "ignores Image" `Quick test_extract_ignores_image
        ; Alcotest.test_case "ignores Document" `Quick test_extract_ignores_document
        ; Alcotest.test_case
            "ignores RedactedThinking"
            `Quick
            test_extract_ignores_redacted_thinking
        ; Alcotest.test_case "ignores Audio" `Quick test_extract_ignores_audio
        ; Alcotest.test_case
            "only wrong-name tools"
            `Quick
            test_extract_only_wrong_name_tools
        ; Alcotest.test_case "mixed blocks" `Quick test_extract_mixed_blocks
        ] )
    ; ( "schema_to_tool_json"
      , [ Alcotest.test_case
            "array/object types"
            `Quick
            test_schema_array_object_param_types
        ; Alcotest.test_case "single required" `Quick test_schema_single_required
        ; Alcotest.test_case "all optional" `Quick test_schema_all_optional
        ] )
    ]
;;
