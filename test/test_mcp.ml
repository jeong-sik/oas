(** Unit tests for the MCP module.
    Tests pure functions only — no subprocess spawning. *)

open Agent_sdk

let check_json = Alcotest.testable
  (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
  (fun a b -> Yojson.Safe.equal a b)

(* ── JSON-RPC 2.0 ───────────────────────────────────────────────── *)

let test_encode_request () =
  let json = Mcp.encode_request ~id:1 ~method_:"tools/list" () in
  let expected = `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", `Int 1);
    ("method", `String "tools/list");
    ("params", `Assoc []);
  ] in
  Alcotest.check check_json "request format" expected json

let test_encode_request_with_params () =
  let params = `Assoc [("name", `String "test")] in
  let json = Mcp.encode_request ~id:42 ~method_:"tools/call" ~params () in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "id" 42 (json |> member "id" |> to_int);
  Alcotest.(check string) "method" "tools/call" (json |> member "method" |> to_string);
  Alcotest.(check string) "params.name" "test"
    (json |> member "params" |> member "name" |> to_string)

let test_encode_notification () =
  let json = Mcp.encode_notification ~method_:"notifications/initialized" () in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "jsonrpc" "2.0" (json |> member "jsonrpc" |> to_string);
  Alcotest.(check string) "method" "notifications/initialized"
    (json |> member "method" |> to_string);
  (* Notifications must not have an id field *)
  Alcotest.check check_json "no id" `Null (json |> member "id")

let test_decode_response_success () =
  let json = Yojson.Safe.from_string
    {|{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}|} in
  match Mcp.decode_response json with
  | Ok result ->
    let expected = `Assoc [("tools", `List [])] in
    Alcotest.check check_json "result" expected result
  | Error e ->
    Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e.message)

let test_decode_response_error () =
  let json = Yojson.Safe.from_string
    {|{"jsonrpc":"2.0","id":1,"error":{"code":-32601,"message":"Method not found"}}|} in
  match Mcp.decode_response json with
  | Ok _ -> Alcotest.fail "Expected Error, got Ok"
  | Error e ->
    Alcotest.(check int) "error code" (-32601) e.code;
    Alcotest.(check string) "error message" "Method not found" e.message

let test_decode_response_error_defaults () =
  let json = Yojson.Safe.from_string
    {|{"jsonrpc":"2.0","id":1,"error":{}}|} in
  match Mcp.decode_response json with
  | Ok _ -> Alcotest.fail "Expected Error, got Ok"
  | Error e ->
    Alcotest.(check int) "default code" (-1) e.code;
    Alcotest.(check string) "default message" "Unknown error" e.message

let test_notification_has_no_id () =
  (* Notifications have no "id" — read_response skips them *)
  let notif = Yojson.Safe.from_string
    {|{"jsonrpc":"2.0","method":"notifications/progress","params":{"token":"abc"}}|} in
  let open Yojson.Safe.Util in
  Alcotest.check check_json "no id in notification" `Null (notif |> member "id");
  (* Response always has "id" *)
  let resp = Yojson.Safe.from_string
    {|{"jsonrpc":"2.0","id":1,"result":{"tools":[]}}|} in
  let has_id = match resp |> member "id" with `Null -> false | _ -> true in
  Alcotest.(check bool) "response has id" true has_id

(* ── MCP tool parsing ───────────────────────────────────────────── *)

let test_parse_mcp_tool () =
  let json = Yojson.Safe.from_string {|{
    "name": "get_weather",
    "description": "Get current weather",
    "inputSchema": {
      "type": "object",
      "properties": {
        "location": {"type": "string", "description": "City name"}
      },
      "required": ["location"]
    }
  }|} in
  let tool = Mcp.parse_mcp_tool json in
  Alcotest.(check string) "name" "get_weather" tool.name;
  Alcotest.(check string) "description" "Get current weather" tool.description;
  Alcotest.(check bool) "has schema" true
    (tool.input_schema <> `Null)

let test_parse_mcp_tool_no_description () =
  let json = Yojson.Safe.from_string {|{
    "name": "bare_tool",
    "inputSchema": {"type": "object", "properties": {}}
  }|} in
  let tool = Mcp.parse_mcp_tool json in
  Alcotest.(check string) "name" "bare_tool" tool.name;
  Alcotest.(check string) "default description" "" tool.description

let test_parse_tools_list () =
  let json = Yojson.Safe.from_string {|{
    "tools": [
      {"name": "tool_a", "description": "A", "inputSchema": {"type": "object", "properties": {}}},
      {"name": "tool_b", "description": "B", "inputSchema": {"type": "object", "properties": {}}}
    ]
  }|} in
  let tools = Mcp.parse_tools_list json in
  Alcotest.(check int) "count" 2 (List.length tools);
  Alcotest.(check string) "first name" "tool_a" (List.hd tools).name;
  Alcotest.(check string) "second name" "tool_b" (List.nth tools 1).name

(* ── JSON Schema → SDK tool_param ───────────────────────────────── *)

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

(* ── Test runner ────────────────────────────────────────────────── *)

let () =
  let open Alcotest in
  run "MCP" [
    "json_rpc", [
      test_case "encode_request" `Quick test_encode_request;
      test_case "encode_request with params" `Quick test_encode_request_with_params;
      test_case "encode_notification" `Quick test_encode_notification;
      test_case "decode_response success" `Quick test_decode_response_success;
      test_case "decode_response error" `Quick test_decode_response_error;
      test_case "decode_response error defaults" `Quick test_decode_response_error_defaults;
      test_case "notification vs response id" `Quick test_notification_has_no_id;
    ];
    "mcp_types", [
      test_case "parse_mcp_tool" `Quick test_parse_mcp_tool;
      test_case "parse_mcp_tool no description" `Quick test_parse_mcp_tool_no_description;
      test_case "parse_tools_list" `Quick test_parse_tools_list;
    ];
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
  ]
