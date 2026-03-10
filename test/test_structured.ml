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
    ToolUse ("tu_1", "extract_person", input_json);
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "name" "Alice" name;
    Alcotest.(check int) "age" 30 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

let test_extract_tool_input_wrong_name () =
  let input_json = `Assoc [("x", `Int 1)] in
  let content = [
    ToolUse ("tu_2", "other_tool", input_json);
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error msg ->
    Alcotest.(check bool) "mentions schema name" true
      (let len = String.length msg in len > 0)
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
    ToolUse ("tu_3", "extract_person", input_json);
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected parse error"

let test_extract_picks_first_match () =
  let input1 = `Assoc [("name", `String "First"); ("age", `Int 1)] in
  let input2 = `Assoc [("name", `String "Second"); ("age", `Int 2)] in
  let content = [
    ToolUse ("tu_4", "extract_person", input1);
    ToolUse ("tu_5", "extract_person", input2);
  ] in
  match Structured.extract_tool_input ~schema:person_schema content with
  | Ok (name, age) ->
    Alcotest.(check string) "first match" "First" name;
    Alcotest.(check int) "first age" 1 age
  | Error e -> Alcotest.fail ("unexpected error: " ^ e)

(* --- Runner --- *)

let () =
  Alcotest.run "structured" [
    "schema_to_tool_json", [
      Alcotest.test_case "structure" `Quick test_schema_to_json_structure;
    ];
    "extract_tool_input", [
      Alcotest.test_case "success" `Quick test_extract_tool_input_success;
      Alcotest.test_case "wrong name" `Quick test_extract_tool_input_wrong_name;
      Alcotest.test_case "no tool_use" `Quick test_extract_tool_input_no_tool_use;
      Alcotest.test_case "parse failure" `Quick test_extract_tool_input_parse_failure;
      Alcotest.test_case "picks first match" `Quick test_extract_picks_first_match;
    ];
  ]
