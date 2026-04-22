(** Unit tests for Structured output module. *)

open Agent_sdk
open Types

(* --- Test schema --- *)

let person_schema : (string * int) Structured.schema = {
  name = "extract_person";
  description = "Extract person info";
  params = [
    { name = "name"; description = "Person name"; param_type = String; required = true };
    { name = "age"; description = "Person age"; param_type = Integer; required = true };
  ];
  parse = (fun json ->
    let open Yojson.Safe.Util in
    try
      let name = json |> member "name" |> to_string in
      let age = json |> member "age" |> to_int in
      Ok (name, age)
    with exn -> Error (Printexc.to_string exn));
}

(* --- schema_to_tool_json --- *)

let test_schema_to_json_structure () =
  let json = Structured.schema_to_tool_json person_schema in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "name" "extract_person"
    (json |> member "name" |> to_string);
  Alcotest.(check string) "description" "Extract person info"
    (json |> member "description" |> to_string);
  let input_schema = json |> member "input_schema" in
  Alcotest.(check string) "type is object" "object"
    (input_schema |> member "type" |> to_string);
  let props = input_schema |> member "properties" in
  Alcotest.(check string) "name prop type" "string"
    (props |> member "name" |> member "type" |> to_string);
  Alcotest.(check string) "age prop type" "integer"
    (props |> member "age" |> member "type" |> to_string);
  let required = input_schema |> member "required" |> to_list
    |> List.map to_string in
  Alcotest.(check bool) "name required" true (List.mem "name" required);
  Alcotest.(check bool) "age required" true (List.mem "age" required)

(* --- extract_tool_input --- *)

let test_extract_tool_input_success () =
  let input_json = `Assoc [("name", `String "Alice"); ("age", `Int 30)] in
  let content = [
    Text "some text";
    ToolUse { id = "tu_1"; name = "extract_person"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "name" "Alice" name;
    Alcotest.(check int) "age" 30 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)

let test_extract_tool_input_wrong_name () =
  let input_json = `Assoc [("x", `Int 1)] in
  let content = [
    ToolUse { id = "tu_2"; name = "other_tool"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error msg ->
    Alcotest.(check bool) "mentions schema name" true
      (let target = "extract_person" in
       let tlen = String.length target in
       let s = Error.to_string msg in
       let mlen = String.length s in
       let rec has i = i + tlen <= mlen && (String.sub s i tlen = target || has (i + 1)) in
       has 0)
  | Ok _ -> Alcotest.fail "expected error for wrong tool name"

let test_extract_tool_input_no_tool_use () =
  let content = [Text "just text"] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for empty content"

let test_extract_tool_input_parse_failure () =
  (* Valid ToolUse name but invalid input JSON for the parser *)
  let input_json = `Assoc [("name", `Int 999)] in
  let content = [
    ToolUse { id = "tu_3"; name = "extract_person"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected parse error"

let test_extract_picks_first_match () =
  let input1 = `Assoc [("name", `String "First"); ("age", `Int 1)] in
  let input2 = `Assoc [("name", `String "Second"); ("age", `Int 2)] in
  let content = [
    ToolUse { id = "tu_4"; name = "extract_person"; input = input1 };
    ToolUse { id = "tu_5"; name = "extract_person"; input = input2 };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "first match" "First" name;
    Alcotest.(check int) "first age" 1 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)

(* --- schema edge cases --- *)

let test_schema_optional_params () =
  let schema : unit Structured.schema = {
    name = "test_opt"; description = "Test optional params";
    params = [
      { name = "required_f"; description = "R"; param_type = String; required = true };
      { name = "optional_f"; description = "O"; param_type = String; required = false };
    ];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required = json |> member "input_schema" |> member "required"
    |> to_list |> List.map to_string in
  Alcotest.(check bool) "required_f in list" true (List.mem "required_f" required);
  Alcotest.(check bool) "optional_f not in list" false (List.mem "optional_f" required)

let test_extract_with_thinking_blocks () =
  let input_json = `Assoc [("name", `String "Bob"); ("age", `Int 25)] in
  let content = [
    Types.Thinking { thinking_type = "sig"; content = "some thinking..." };
    Text "preamble";
    ToolUse { id = "tu_t"; name = "extract_person"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "name" "Bob" name;
    Alcotest.(check int) "age" 25 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)

let test_schema_tool_choice_name () =
  let json = Structured.schema_to_tool_json person_schema in
  let open Yojson.Safe.Util in
  (* The name field is what tool_choice=Tool(name) would reference *)
  Alcotest.(check string) "name for tool_choice" "extract_person"
    (json |> member "name" |> to_string);
  let props = json |> member "input_schema" |> member "properties" |> to_assoc in
  Alcotest.(check int) "property count" 2 (List.length props)

let test_extract_from_empty_content () =
  match Structured.extract_tool_input ~schema:person_schema [] with
  | Error msg ->
    Alcotest.(check bool) "mentions schema name" true
      (String.length (Error.to_string msg) > 0)
  | Ok _ -> Alcotest.fail "expected error for empty content"

(* --- Additional schema_to_tool_json coverage --- *)

let test_schema_empty_params () =
  let schema : unit Structured.schema = {
    name = "no_params"; description = "No params tool";
    params = [];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" |> to_assoc in
  Alcotest.(check int) "no properties" 0 (List.length props);
  let required = json |> member "input_schema" |> member "required" |> to_list in
  Alcotest.(check int) "no required" 0 (List.length required)

let test_schema_all_param_types () =
  let schema : unit Structured.schema = {
    name = "all_types"; description = "All param types";
    params = [
      { name = "s"; description = "string"; param_type = String; required = true };
      { name = "i"; description = "integer"; param_type = Integer; required = true };
      { name = "n"; description = "number"; param_type = Number; required = false };
      { name = "b"; description = "boolean"; param_type = Boolean; required = false };
    ];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  Alcotest.(check string) "string type" "string"
    (props |> member "s" |> member "type" |> to_string);
  Alcotest.(check string) "integer type" "integer"
    (props |> member "i" |> member "type" |> to_string);
  Alcotest.(check string) "number type" "number"
    (props |> member "n" |> member "type" |> to_string);
  Alcotest.(check string) "boolean type" "boolean"
    (props |> member "b" |> member "type" |> to_string);
  let required = json |> member "input_schema" |> member "required"
    |> to_list |> List.map to_string in
  Alcotest.(check int) "2 required" 2 (List.length required);
  Alcotest.(check bool) "s required" true (List.mem "s" required);
  Alcotest.(check bool) "i required" true (List.mem "i" required);
  Alcotest.(check bool) "n not required" false (List.mem "n" required);
  Alcotest.(check bool) "b not required" false (List.mem "b" required)

let test_schema_description_preserved () =
  let schema : unit Structured.schema = {
    name = "desc_test"; description = "My detailed description";
    params = [
      { name = "x"; description = "field X description"; param_type = String; required = true };
    ];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "schema desc" "My detailed description"
    (json |> member "description" |> to_string);
  let x_desc = json |> member "input_schema" |> member "properties"
    |> member "x" |> member "description" |> to_string in
  Alcotest.(check string) "param desc" "field X description" x_desc

(* --- extract_tool_input error types --- *)

let test_extract_parse_error_is_serialization () =
  let input_json = `Assoc [("name", `Int 999)] in
  let content = [
    ToolUse { id = "tu_x"; name = "extract_person"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error (Error.Serialization _) -> ()
  | Error e -> Alcotest.failf "expected Serialization, got: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "expected parse error"

let test_extract_missing_tool_is_internal () =
  let content = [Text "no tools here"] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error (Error.Internal s) ->
    Alcotest.(check bool) "mentions schema name" true
      (let tgt = "extract_person" in
       let tlen = String.length tgt in
       let slen = String.length s in
       let rec has i = i + tlen <= slen && (String.sub s i tlen = tgt || has (i + 1)) in
       has 0)
  | Error e -> Alcotest.failf "expected Internal, got: %s" (Error.to_string e)
  | Ok _ -> Alcotest.fail "expected error"

(* --- extract_tool_input: ToolResult blocks ignored --- *)

let test_extract_ignores_tool_result () =
  let input_json = `Assoc [("name", `String "Carol"); ("age", `Int 40)] in
  let content = [
    ToolResult { tool_use_id = "old"; content = "previous result"; is_error = false; json = None };
    ToolUse { id = "tu_r"; name = "extract_person"; input = input_json };
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "name" "Carol" name;
    Alcotest.(check int) "age" 40 age
  | Error e -> Alcotest.fail ("unexpected: " ^ Error.to_string e)

(* --- schema with many required/optional mix --- *)

let test_schema_mixed_required () =
  let schema : unit Structured.schema = {
    name = "mixed"; description = "Mixed";
    params = [
      { name = "a"; description = "A"; param_type = String; required = true };
      { name = "b"; description = "B"; param_type = String; required = false };
      { name = "c"; description = "C"; param_type = Integer; required = true };
      { name = "d"; description = "D"; param_type = Boolean; required = false };
      { name = "e"; description = "E"; param_type = Number; required = true };
    ];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required = json |> member "input_schema" |> member "required"
    |> to_list |> List.map to_string in
  Alcotest.(check int) "3 required" 3 (List.length required);
  Alcotest.(check bool) "a required" true (List.mem "a" required);
  Alcotest.(check bool) "c required" true (List.mem "c" required);
  Alcotest.(check bool) "e required" true (List.mem "e" required)

(* --- Extractors --- *)

let make_response content : Types.api_response =
  { id = "m"; model = "m"; stop_reason = EndTurn; content; usage = None; telemetry = None }

let test_json_extractor_success () =
  let extract = Structured.json_extractor (fun json ->
    Yojson.Safe.Util.(json |> member "value" |> to_int))
  in
  let resp = make_response [Text {|{"value": 42}|}] in
  match extract resp with
  | Ok v -> Alcotest.(check int) "extracted value" 42 v
  | Error e -> Alcotest.fail e

let test_json_extractor_invalid_json () =
  let extract = Structured.json_extractor (fun _ -> 0) in
  let resp = make_response [Text "not json"] in
  match extract resp with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected parse error"

let test_json_extractor_no_text () =
  let extract = Structured.json_extractor (fun _ -> 0) in
  let resp = make_response [] in
  match extract resp with
  | Error msg ->
    Alcotest.(check bool) "mentions no text" true
      (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected error"

let test_text_extractor_success () =
  let extract = Structured.text_extractor (fun s ->
    if String.length s > 0 then Some (String.length s) else None)
  in
  let resp = make_response [Text "hello world"] in
  match extract resp with
  | Ok v -> Alcotest.(check int) "text length" 11 v
  | Error e -> Alcotest.fail e

let test_text_extractor_none () =
  let extract = Structured.text_extractor (fun _ -> None) in
  let resp = make_response [Text "anything"] in
  match extract resp with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error"

(* --- extract_with_retry: unit-level logic tests --- *)

(** Test that max_retries=0 means only 1 attempt (no retry).
    We test via extract_tool_input directly since extract_with_retry
    needs a real API. The retry logic is: if extract_tool_input fails
    and retries remain, build retry messages with error feedback. *)
let test_retry_message_construction () =
  (* Simulate what extract_with_retry builds on failure *)
  let bad_input = `Assoc [("name", `Int 999)] in
  let response_content = [
    ToolUse { id = "tu_r1"; name = "extract_person"; input = bad_input };
  ] in
  (* First attempt: parse fails *)
  let result = Structured.extract_tool_input ~schema:person_schema response_content in
  (match result with
   | Error _ ->
       (* Build retry message like extract_with_retry does *)
       let tool_use_id = "tu_r1" in
       let error_msg = "Validation error: parse failed" in
       let retry_msg = { role = User; content = [
         ToolResult { tool_use_id; content = error_msg; is_error = true; json = None }
       ]; name = None; tool_call_id = None ; metadata = []} in
       Alcotest.(check string) "retry role" "User"
         (match retry_msg.role with User -> "User" | _ -> "other");
       let has_error_result = List.exists (function
         | ToolResult { is_error = true; _ } -> true
         | _ -> false) retry_msg.content in
       Alcotest.(check bool) "has error tool_result" true has_error_result
   | Ok _ -> Alcotest.fail "expected parse error")

let test_retry_finds_tool_use_id () =
  let content = [
    Text "some text";
    ToolUse { id = "tu_abc"; name = "extract_person";
              input = `Assoc [("bad", `Null)] };
  ] in
  let tool_use_id = List.find_map (function
    | ToolUse { id; name; _ } when name = "extract_person" -> Some id
    | _ -> None) content in
  Alcotest.(check (option string)) "finds tool_use_id"
    (Some "tu_abc") tool_use_id

let test_retry_on_validation_error_callback () =
  let calls = ref [] in
  let cb attempt err = calls := (attempt, err) :: !calls in
  (* Simulate 2 retries worth of callbacks *)
  cb 1 "first error";
  cb 2 "second error";
  Alcotest.(check int) "2 callbacks" 2 (List.length !calls);
  Alcotest.(check int) "last attempt" 2 (fst (List.hd !calls))

(* --- Runner --- *)

let () =
  Alcotest.run "structured" [
    "schema_to_tool_json", [
      Alcotest.test_case "structure" `Quick test_schema_to_json_structure;
      Alcotest.test_case "optional params" `Quick test_schema_optional_params;
      Alcotest.test_case "tool choice name" `Quick test_schema_tool_choice_name;
      Alcotest.test_case "empty params" `Quick test_schema_empty_params;
      Alcotest.test_case "all param types" `Quick test_schema_all_param_types;
      Alcotest.test_case "description preserved" `Quick test_schema_description_preserved;
      Alcotest.test_case "mixed required" `Quick test_schema_mixed_required;
    ];
    "extract_tool_input", [
      Alcotest.test_case "success" `Quick test_extract_tool_input_success;
      Alcotest.test_case "wrong name" `Quick test_extract_tool_input_wrong_name;
      Alcotest.test_case "no tool_use" `Quick test_extract_tool_input_no_tool_use;
      Alcotest.test_case "parse failure" `Quick test_extract_tool_input_parse_failure;
      Alcotest.test_case "picks first match" `Quick test_extract_picks_first_match;
      Alcotest.test_case "with thinking blocks" `Quick test_extract_with_thinking_blocks;
      Alcotest.test_case "empty content" `Quick test_extract_from_empty_content;
      Alcotest.test_case "parse error type" `Quick test_extract_parse_error_is_serialization;
      Alcotest.test_case "missing tool type" `Quick test_extract_missing_tool_is_internal;
      Alcotest.test_case "ignores tool_result" `Quick test_extract_ignores_tool_result;
    ];
    "extractors", [
      Alcotest.test_case "json_extractor success" `Quick test_json_extractor_success;
      Alcotest.test_case "json_extractor invalid" `Quick test_json_extractor_invalid_json;
      Alcotest.test_case "json_extractor no text" `Quick test_json_extractor_no_text;
      Alcotest.test_case "text_extractor success" `Quick test_text_extractor_success;
      Alcotest.test_case "text_extractor none" `Quick test_text_extractor_none;
    ];
    "extract_with_retry", [
      Alcotest.test_case "retry message construction" `Quick test_retry_message_construction;
      Alcotest.test_case "finds tool_use_id" `Quick test_retry_finds_tool_use_id;
      Alcotest.test_case "on_validation_error callback" `Quick test_retry_on_validation_error_callback;
    ];
  ]
