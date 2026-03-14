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

(* --- Runner --- *)

let () =
  Alcotest.run "structured" [
    "schema_to_tool_json", [
      Alcotest.test_case "structure" `Quick test_schema_to_json_structure;
      Alcotest.test_case "optional params" `Quick test_schema_optional_params;
      Alcotest.test_case "tool choice name" `Quick test_schema_tool_choice_name;
    ];
    "extract_tool_input", [
      Alcotest.test_case "success" `Quick test_extract_tool_input_success;
      Alcotest.test_case "wrong name" `Quick test_extract_tool_input_wrong_name;
      Alcotest.test_case "no tool_use" `Quick test_extract_tool_input_no_tool_use;
      Alcotest.test_case "parse failure" `Quick test_extract_tool_input_parse_failure;
      Alcotest.test_case "picks first match" `Quick test_extract_picks_first_match;
      Alcotest.test_case "with thinking blocks" `Quick test_extract_with_thinking_blocks;
      Alcotest.test_case "empty content" `Quick test_extract_from_empty_content;
    ];
  ]
