(** Unit tests for the Mcp_bridge module.
    Tests pure functions only — no subprocess spawning.
    Verifies that bridge helpers produce the same results as Mcp module. *)

open Agent_sdk

let check_param_type = Alcotest.testable
  (fun fmt pt -> Format.pp_print_string fmt (match pt with
    | Types.String -> "String" | Types.Integer -> "Integer"
    | Types.Number -> "Number" | Types.Boolean -> "Boolean"
    | Types.Array -> "Array" | Types.Object -> "Object"))
  (=)

(* ── json_schema_type_to_param_type ──────────────────────────── *)

let test_type_mapping () =
  let cases = [
    ("string", Types.String); ("integer", Types.Integer);
    ("number", Types.Number); ("boolean", Types.Boolean);
    ("array", Types.Array); ("object", Types.Object);
    ("unknown_type", Types.String);
  ] in
  List.iter (fun (input, expected) ->
    Alcotest.check check_param_type input expected
      (Mcp_bridge.json_schema_type_to_param_type input)
  ) cases

let test_type_mapping_matches_mcp () =
  let inputs = ["string"; "integer"; "number"; "boolean";
                "array"; "object"; "null"; "any"; ""] in
  List.iter (fun input ->
    let bridge_result = Mcp_bridge.json_schema_type_to_param_type input in
    let mcp_result = Mcp.json_schema_type_to_param_type input in
    Alcotest.check check_param_type
      (Printf.sprintf "parity for %S" input)
      mcp_result bridge_result
  ) inputs

(* ── json_schema_to_params ───────────────────────────────────── *)

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
  let params = Mcp_bridge.json_schema_to_params schema in
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
  let params = Mcp_bridge.json_schema_to_params schema in
  Alcotest.(check int) "empty params" 0 (List.length params)

let test_json_schema_no_required () =
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "x": {"type": "number", "description": "X value"}
    }
  }|} in
  let params = Mcp_bridge.json_schema_to_params schema in
  Alcotest.(check int) "one param" 1 (List.length params);
  Alcotest.(check bool) "not required" false (List.hd params).required

let test_json_schema_no_description () =
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "flag": {"type": "boolean"}
    }
  }|} in
  let params = Mcp_bridge.json_schema_to_params schema in
  Alcotest.(check string) "default desc" "" (List.hd params).description;
  Alcotest.check check_param_type "bool type" Types.Boolean
    (List.hd params).param_type

let test_json_schema_parity () =
  let schemas = [
    {|{"type":"object","properties":{"a":{"type":"string"}},"required":["a"]}|};
    {|{"type":"object","properties":{"n":{"type":"number","description":"N"}},"required":[]}|};
    {|{"type":"object","properties":{"arr":{"type":"array"},"obj":{"type":"object"}}}|};
    {|{"type":"object"}|};
  ] in
  List.iter (fun s ->
    let schema = Yojson.Safe.from_string s in
    let bridge_params = Mcp_bridge.json_schema_to_params schema in
    let mcp_params = Mcp.json_schema_to_params schema in
    Alcotest.(check int) "param count parity"
      (List.length mcp_params) (List.length bridge_params);
    List.iter2 (fun (bp : Types.tool_param) (mp : Types.tool_param) ->
      Alcotest.(check string) "name" mp.name bp.name;
      Alcotest.(check string) "desc" mp.description bp.description;
      Alcotest.check check_param_type "type" mp.param_type bp.param_type;
      Alcotest.(check bool) "required" mp.required bp.required;
    ) bridge_params mcp_params
  ) schemas

(* ── to_sdk_tools (without subprocess) ───────────────────────── *)

let test_to_sdk_tools_schema () =
  (* Create a minimal Mcp_bridge.t is not possible without subprocess,
     so we test json_schema_to_params + Tool.create path indirectly.
     The to_sdk_tools function is a thin wrapper over these two. *)
  let schema = Yojson.Safe.from_string {|{
    "type": "object",
    "properties": {
      "text": {"type": "string", "description": "Input text"}
    },
    "required": ["text"]
  }|} in
  let params = Mcp_bridge.json_schema_to_params schema in
  let call_fn input =
    let open Yojson.Safe.Util in
    Ok ("echo: " ^ (input |> member "text" |> to_string))
  in
  let tool = Tool.create
    ~name:"echo" ~description:"Echoes input"
    ~parameters:params call_fn
  in
  Alcotest.(check string) "tool name" "echo" tool.schema.name;
  Alcotest.(check string) "tool desc" "Echoes input" tool.schema.description;
  Alcotest.(check int) "param count" 1 (List.length tool.schema.parameters);
  let result = Tool.execute tool (`Assoc [("text", `String "hi")]) in
  Alcotest.(check (result string string)) "execute" (Ok "echo: hi") result

(* ── Test runner ─────────────────────────────────────────────── *)

let () =
  let open Alcotest in
  run "Mcp_bridge" [
    "type_conversion", [
      test_case "type mapping" `Quick test_type_mapping;
      test_case "type mapping matches Mcp" `Quick test_type_mapping_matches_mcp;
    ];
    "schema_conversion", [
      test_case "full schema" `Quick test_json_schema_to_params;
      test_case "empty schema" `Quick test_json_schema_empty;
      test_case "no required" `Quick test_json_schema_no_required;
      test_case "no description" `Quick test_json_schema_no_description;
      test_case "parity with Mcp" `Quick test_json_schema_parity;
    ];
    "tool_bridge", [
      test_case "schema + Tool.create roundtrip" `Quick test_to_sdk_tools_schema;
    ];
  ]
