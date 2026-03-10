(** Unit tests for the MCP module.
    Tests pure functions only — no subprocess spawning.

    After the SDK migration (mcp-protocol-sdk v0.10.0), JSON-RPC
    encoding/decoding and MCP type parsing are handled by the SDK.
    These tests cover the oas-specific bridge layer:
    - JSON Schema -> SDK tool_param conversion
    - MCP tool -> SDK Tool.t bridge
    - SDK Mcp_types.tool -> oas mcp_tool conversion *)

open Agent_sdk

(* ── JSON Schema -> SDK tool_param ───────────────────────────────── *)

let check_param_type = Alcotest.testable
  Types.pp_param_type
  (fun a b -> a = b)

let test_type_mapping () =
  let cases = [
    ("string", Types.String); ("integer", Types.Integer);
    ("number", Types.Number); ("boolean", Types.Boolean);
    ("array", Types.Array); ("object", Types.Object);
    ("unknown_type", Types.String);
  ] in
  List.iter (fun (input, expected) ->
    Alcotest.check check_param_type input expected
      (Mcp.json_schema_type_to_param_type input)
  ) cases

let test_json_schema_to_params () =
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "city": {"type": "string", "description": "City name"},
      "units": {"type": "string", "description": "Temperature units"},
      "count": {"type": "integer", "description": "Result count"}
    },
    "required": ["city"]
  }|} in
  let params = Mcp.json_schema_to_params schema in
  Alcotest.(check int) "param count" 3 (List.length params);
  let city = List.hd params in
  Alcotest.(check string) "city name" "city" city.name;
  Alcotest.(check string) "city desc" "City name" city.description;
  Alcotest.check check_param_type "city type" Types.String city.param_type;
  Alcotest.(check bool) "city required" true city.required;
  let units = List.nth params 1 in
  Alcotest.(check bool) "units optional" false units.required;
  let count = List.nth params 2 in
  Alcotest.check check_param_type "count type" Types.Integer count.param_type

let test_json_schema_empty () =
  let schema = Yojson.Safe.from_string {|{"type": "object"}|} in
  let params = Mcp.json_schema_to_params schema in
  Alcotest.(check int) "empty params" 0 (List.length params)

let test_json_schema_no_required () =
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "x": {"type": "number", "description": "X value"}
    }
  }|} in
  let params = Mcp.json_schema_to_params schema in
  Alcotest.(check int) "one param" 1 (List.length params);
  Alcotest.(check bool) "not required" false (List.hd params).required

let test_json_schema_no_description () =
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "flag": {"type": "boolean"}
    }
  }|} in
  let params = Mcp.json_schema_to_params schema in
  Alcotest.(check string) "default desc" "" (List.hd params).description;
  Alcotest.check check_param_type "bool type" Types.Boolean (List.hd params).param_type

(* ── Tool bridge ────────────────────────────────────────────────── *)

let test_mcp_tool_to_sdk_tool () =
  let mcp_tool : Mcp.mcp_tool = {
    name = "echo";
    description = "Echoes input";
    input_schema = Yojson.Safe.from_string {|{
      "type": "object",
      "properties": {
        "text": {"type": "string", "description": "Text to echo"}
      },
      "required": ["text"]
    }|};
  } in
  let call_fn input =
    let open Yojson.Safe.Util in
    let text = input |> member "text" |> to_string in
    Ok ("echo: " ^ text)
  in
  let sdk_tool = Mcp.mcp_tool_to_sdk_tool ~call_fn mcp_tool in
  Alcotest.(check string) "tool name" "echo" sdk_tool.schema.name;
  Alcotest.(check string) "tool desc" "Echoes input" sdk_tool.schema.description;
  Alcotest.(check int) "param count" 1 (List.length sdk_tool.schema.parameters);
  let input = `Assoc [("text", `String "hello")] in
  let result = Tool.execute sdk_tool input in
  Alcotest.(check (result string string)) "execution" (Ok "echo: hello") result

let test_mcp_tool_bridge_error () =
  let mcp_tool : Mcp.mcp_tool = {
    name = "fail";
    description = "Always fails";
    input_schema = `Assoc [("type", `String "object"); ("properties", `Assoc [])];
  } in
  let call_fn _input = Error "server error" in
  let sdk_tool = Mcp.mcp_tool_to_sdk_tool ~call_fn mcp_tool in
  let result = Tool.execute sdk_tool (`Assoc []) in
  Alcotest.(check (result string string)) "error" (Error "server error") result

(* ── SDK tool -> oas mcp_tool conversion ─────────────────────────── *)

let test_mcp_tool_of_sdk_tool () =
  let sdk_tool : Mcp_protocol.Mcp_types.tool = {
    name = "read_file";
    description = Some "Read a file";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("path", `Assoc [
          ("type", `String "string");
          ("description", `String "File path");
        ]);
      ]);
      ("required", `List [`String "path"]);
    ];
    title = None;
    annotations = None;
    icon = None;
  } in
  let mcp_tool = Mcp.mcp_tool_of_sdk_tool sdk_tool in
  Alcotest.(check string) "name" "read_file" mcp_tool.name;
  Alcotest.(check string) "description" "Read a file" mcp_tool.description;
  let params = Mcp.json_schema_to_params mcp_tool.input_schema in
  Alcotest.(check int) "param count" 1 (List.length params);
  Alcotest.(check string) "param name" "path" (List.hd params).name;
  Alcotest.(check bool) "param required" true (List.hd params).required

let test_mcp_tool_of_sdk_tool_no_description () =
  let sdk_tool : Mcp_protocol.Mcp_types.tool = {
    name = "bare";
    description = None;
    input_schema = `Assoc [("type", `String "object"); ("properties", `Assoc [])];
    title = None;
    annotations = None;
    icon = None;
  } in
  let mcp_tool = Mcp.mcp_tool_of_sdk_tool sdk_tool in
  Alcotest.(check string) "default desc" "" mcp_tool.description

let test_text_of_tool_result () =
  let result : Mcp_protocol.Mcp_types.tool_result = {
    content = [
      Mcp_protocol.Mcp_types.TextContent {
        type_ = "text"; text = "line1"; annotations = None };
      Mcp_protocol.Mcp_types.TextContent {
        type_ = "text"; text = "line2"; annotations = None };
    ];
    is_error = None;
    structured_content = None;
  } in
  let text = Mcp.text_of_tool_result result in
  Alcotest.(check string) "concatenated" "line1\nline2" text

let test_text_of_tool_result_mixed () =
  let result : Mcp_protocol.Mcp_types.tool_result = {
    content = [
      Mcp_protocol.Mcp_types.TextContent {
        type_ = "text"; text = "hello"; annotations = None };
      Mcp_protocol.Mcp_types.ImageContent {
        type_ = "image"; data = "base64..."; mime_type = "image/png";
        annotations = None };
      Mcp_protocol.Mcp_types.TextContent {
        type_ = "text"; text = "world"; annotations = None };
    ];
    is_error = None;
    structured_content = None;
  } in
  let text = Mcp.text_of_tool_result result in
  Alcotest.(check string) "text only" "hello\nworld" text

(* ── Test runner ────────────────────────────────────────────────── *)

let () =
  let open Alcotest in
  run "MCP" [
    "schema_conversion", [
      test_case "type mapping" `Quick test_type_mapping;
      test_case "full schema" `Quick test_json_schema_to_params;
      test_case "empty schema" `Quick test_json_schema_empty;
      test_case "no required" `Quick test_json_schema_no_required;
      test_case "no description" `Quick test_json_schema_no_description;
    ];
    "tool_bridge", [
      test_case "mcp_tool_to_sdk_tool" `Quick test_mcp_tool_to_sdk_tool;
      test_case "bridge error propagation" `Quick test_mcp_tool_bridge_error;
    ];
    "sdk_bridge", [
      test_case "mcp_tool_of_sdk_tool" `Quick test_mcp_tool_of_sdk_tool;
      test_case "sdk_tool no description" `Quick test_mcp_tool_of_sdk_tool_no_description;
      test_case "text_of_tool_result" `Quick test_text_of_tool_result;
      test_case "text_of_tool_result mixed content" `Quick test_text_of_tool_result_mixed;
    ];
  ]
