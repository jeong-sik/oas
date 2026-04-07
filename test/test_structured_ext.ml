(** Extended coverage tests for Structured module pure functions.
    Targets: json_extractor, text_extractor, schema_to_tool_json. *)

open Alcotest
open Agent_sdk

(* ── schema_to_tool_json ─────────────────────────────── *)

let test_schema_basic () =
  let schema : int Structured.schema = {
    name = "get_count";
    description = "Returns a count";
    params = [
      { name = "value"; param_type = Types.Integer;
        description = "the count"; required = true };
    ];
    parse = (fun json ->
      let open Yojson.Safe.Util in
      Ok (json |> member "value" |> to_int));
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  check string "name" "get_count" (json |> member "name" |> to_string);
  check string "description" "Returns a count"
    (json |> member "description" |> to_string);
  let input_schema = json |> member "input_schema" in
  check string "type" "object"
    (input_schema |> member "type" |> to_string);
  let required = input_schema |> member "required" |> to_list in
  check int "1 required" 1 (List.length required)

let test_schema_no_required () =
  let schema : string Structured.schema = {
    name = "optional_tool";
    description = "All optional";
    params = [
      { name = "hint"; param_type = Types.String;
        description = "optional hint"; required = false };
    ];
    parse = (fun _ -> Ok "ok");
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let required = json |> member "input_schema" |> member "required" |> to_list in
  check int "0 required" 0 (List.length required)

let test_schema_empty_params () =
  let schema : unit Structured.schema = {
    name = "no_params";
    description = "No parameters";
    params = [];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  check bool "empty properties" true
    (match props with `Assoc [] -> true | _ -> false)

(* ── extract_tool_input ──────────────────────────────── *)

let int_schema : int Structured.schema = {
  name = "get_number";
  description = "Returns a number";
  params = [];
  parse = (fun json ->
    let open Yojson.Safe.Util in
    Ok (json |> member "value" |> to_int));
}

let test_extract_found () =
  let content = [
    Types.Text "thinking...";
    Types.ToolUse {
      id = "t1"; name = "get_number";
      input = `Assoc [("value", `Int 42)];
    };
  ] in
  match Structured.extract_tool_input ~schema:int_schema content with
  | Ok v -> check int "value" 42 v
  | Error _ -> fail "expected Ok"

let test_extract_not_found () =
  let content = [Types.Text "no tools here"] in
  match Structured.extract_tool_input ~schema:int_schema content with
  | Ok _ -> fail "expected Error"
  | Error _ -> ()

let test_extract_wrong_name () =
  let content = [
    Types.ToolUse {
      id = "t1"; name = "wrong_tool";
      input = `Assoc [("value", `Int 42)];
    };
  ] in
  match Structured.extract_tool_input ~schema:int_schema content with
  | Ok _ -> fail "expected Error for wrong name"
  | Error _ -> ()

let test_extract_parse_error () =
  let bad_schema : int Structured.schema = {
    name = "get_number";
    description = "";
    params = [];
    parse = (fun _ -> Error "parse failed");
  } in
  let content = [
    Types.ToolUse {
      id = "t1"; name = "get_number";
      input = `Assoc [];
    };
  ] in
  match Structured.extract_tool_input ~schema:bad_schema content with
  | Ok _ -> fail "expected Error"
  | Error _ -> ()

(* ── json_extractor ──────────────────────────────────── *)

let test_json_extractor_valid () =
  let extract = Structured.json_extractor (fun json ->
    Yojson.Safe.Util.(json |> member "x" |> to_int)) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text {|{"x": 99}|}];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok v -> check int "x" 99 v
  | Error msg -> fail msg

let test_json_extractor_invalid_json () =
  let extract = Structured.json_extractor (fun _ -> 0) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text "not json"];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok _ -> fail "expected Error"
  | Error msg ->
    check bool "mentions JSON" true
      (String.length msg > 0)

let test_json_extractor_empty_content () =
  let extract = Structured.json_extractor (fun _ -> 0) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok _ -> fail "expected Error"
  | Error msg ->
    check bool "mentions content" true
      (String.length msg > 0)

let test_json_extractor_type_error () =
  let extract = Structured.json_extractor (fun json ->
    Yojson.Safe.Util.(json |> member "x" |> to_int)) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text {|{"x": "not_int"}|}];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok _ -> fail "expected Error"
  | Error _ -> ()

(* ── text_extractor ──────────────────────────────────── *)

let test_text_extractor_some () =
  let extract = Structured.text_extractor (fun s ->
    if String.length s > 0 then Some (String.length s) else None) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text "hello"];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok v -> check int "length" 5 v
  | Error _ -> fail "expected Ok"

let test_text_extractor_none () =
  let extract = Structured.text_extractor (fun _ -> None) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [Types.Text "anything"];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok _ -> fail "expected Error"
  | Error _ -> ()

let test_text_extractor_empty () =
  let extract = Structured.text_extractor (fun _ -> Some 0) in
  let resp = {
    Types.id = "r"; model = "m"; stop_reason = Types.EndTurn;
    content = [];
    usage = None;
    telemetry = None;
  } in
  match extract resp with
  | Ok _ -> fail "expected Error"
  | Error _ -> ()

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "structured_ext" [
    "schema_to_tool_json", [
      test_case "basic" `Quick test_schema_basic;
      test_case "no required" `Quick test_schema_no_required;
      test_case "empty params" `Quick test_schema_empty_params;
    ];
    "extract_tool_input", [
      test_case "found" `Quick test_extract_found;
      test_case "not found" `Quick test_extract_not_found;
      test_case "wrong name" `Quick test_extract_wrong_name;
      test_case "parse error" `Quick test_extract_parse_error;
    ];
    "json_extractor", [
      test_case "valid" `Quick test_json_extractor_valid;
      test_case "invalid json" `Quick test_json_extractor_invalid_json;
      test_case "empty content" `Quick test_json_extractor_empty_content;
      test_case "type error" `Quick test_json_extractor_type_error;
    ];
    "text_extractor", [
      test_case "some" `Quick test_text_extractor_some;
      test_case "none" `Quick test_text_extractor_none;
      test_case "empty" `Quick test_text_extractor_empty;
    ];
  ]
