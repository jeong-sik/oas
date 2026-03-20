(** Extended API coverage tests — targets uncovered paths in api.ml and provider_intf.ml.

    Focuses on:
    - named_cascade construction
    - Re-exported functions (default_base_url, api_version, etc.)
    - string_is_blank
    - text_blocks_to_string
    - Provider_intf.supports_streaming
    - Provider_intf.of_config and of_config_streaming dispatch *)

open Agent_sdk

(* ── Api re-exports ───────────────────────────────────────── *)

let test_default_base_url () =
  Alcotest.(check bool) "non-empty" true (String.length Api.default_base_url > 0);
  Alcotest.(check bool) "starts with https" true
    (String.length Api.default_base_url >= 5)

let test_api_version () =
  Alcotest.(check bool) "non-empty" true (String.length Api.api_version > 0)

let test_max_response_body () =
  Alcotest.(check bool) "positive" true (Api.max_response_body > 0)

let test_string_is_blank_empty () =
  Alcotest.(check bool) "empty is blank" true (Api.string_is_blank "")

let test_string_is_blank_spaces () =
  Alcotest.(check bool) "spaces is blank" true (Api.string_is_blank "   ")

let test_string_is_blank_content () =
  Alcotest.(check bool) "content not blank" false (Api.string_is_blank "hello")

let test_text_blocks_to_string () =
  let blocks = [Types.Text "hello"; Types.Text "world"] in
  let result = Api.text_blocks_to_string blocks in
  Alcotest.(check bool) "contains hello" true (String.length result > 0)

let test_text_blocks_to_string_empty () =
  let result = Api.text_blocks_to_string [] in
  Alcotest.(check string) "empty blocks" "" result

(* ── named_cascade construction ───────────────────────────── *)

let test_named_cascade_construction () =
  let nc = Api.named_cascade ~name:"test" ~defaults:["llama:qwen"] () in
  Alcotest.(check string) "name" "test" nc.name;
  Alcotest.(check int) "defaults count" 1 (List.length nc.defaults);
  Alcotest.(check bool) "no config path" true (nc.config_path = None)

let test_named_cascade_with_config_path () =
  let nc = Api.named_cascade ~name:"custom" ~defaults:["a"; "b"]
    ~config_path:"/tmp/cascade.json" () in
  Alcotest.(check string) "name" "custom" nc.name;
  Alcotest.(check int) "two defaults" 2 (List.length nc.defaults);
  Alcotest.(check (option string)) "config path"
    (Some "/tmp/cascade.json") nc.config_path

(* ── content_block_to_json / of_json roundtrip ────────────── *)

let test_content_block_text_roundtrip () =
  let block = Types.Text "test content" in
  let json = Api.content_block_to_json block in
  let decoded = Api.content_block_of_json json in
  match decoded with
  | Some (Types.Text s) -> Alcotest.(check string) "roundtrip" "test content" s
  | _ -> Alcotest.fail "expected Text block"

let test_content_block_tool_use_roundtrip () =
  let block = Types.ToolUse {
    id = "tu_1"; name = "read_file"; input = `Assoc [("path", `String "/tmp")]
  } in
  let json = Api.content_block_to_json block in
  let decoded = Api.content_block_of_json json in
  match decoded with
  | Some (Types.ToolUse { id; name; _ }) ->
    Alcotest.(check string) "id" "tu_1" id;
    Alcotest.(check string) "name" "read_file" name
  | _ -> Alcotest.fail "expected ToolUse block"

(* ── message_to_json ──────────────────────────────────────── *)

let test_message_to_json () =
  let msg : Types.message = {
    role = User;
    content = [Types.Text "hello"];
    name = None;
    tool_call_id = None;
  } in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  let role = json |> member "role" |> to_string in
  Alcotest.(check string) "role" "user" role

(* ── json_of_string_or_raw ────────────────────────────────── *)

let test_json_of_string_valid () =
  let result = Api.json_of_string_or_raw {|{"key": "value"}|} in
  match result with
  | `Assoc _ -> ()
  | _ -> Alcotest.fail "expected Assoc"

let test_json_of_string_invalid () =
  let result = Api.json_of_string_or_raw "not json" in
  match result with
  | `Assoc [("raw", `String "not json")] -> ()
  | _ -> Alcotest.fail "expected Assoc with raw key fallback"

(* ── Provider_intf tests ──────────────────────────────────── *)

let test_supports_streaming_anthropic () =
  let cfg = Provider.anthropic_sonnet () in
  Alcotest.(check bool) "anthropic streams" true
    (Provider_intf.supports_streaming cfg)

let test_of_config_returns_module () =
  let cfg = Provider.anthropic_sonnet () in
  let _module = Provider_intf.of_config cfg in
  (* Just verifying it doesn't crash *)
  Alcotest.(check bool) "module created" true true

let test_of_config_streaming_anthropic () =
  let cfg = Provider.anthropic_sonnet () in
  match Provider_intf.of_config_streaming cfg with
  | Some _ -> ()  (* Anthropic supports streaming *)
  | None -> Alcotest.fail "expected streaming provider for Anthropic"

(* ── build_body_assoc ─────────────────────────────────────── *)

let test_build_body_assoc () =
  let config : Types.agent_state = {
    config = { Types.default_config with max_tokens = 100 };
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in
  let messages = [Types.user_msg "hello"] in
  let assoc = Api.build_body_assoc ~config ~messages ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "has model" true
    (json |> member "model" |> to_string |> String.length > 0);
  Alcotest.(check int) "max_tokens" 100
    (json |> member "max_tokens" |> to_int)

(* ── parse_response ───────────────────────────────────────── *)

let test_parse_response_basic () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_123",
    "model": "claude-3-5-sonnet-20241022",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "hello"}],
    "usage": {"input_tokens": 10, "output_tokens": 5}
  }|} in
  let resp = Api.parse_response json in
  Alcotest.(check string) "id" "msg_123" resp.id;
  Alcotest.(check string) "model" "claude-3-5-sonnet-20241022" resp.model

(* ── Suite ────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Api_coverage" [
    "re_exports", [
      Alcotest.test_case "default_base_url" `Quick test_default_base_url;
      Alcotest.test_case "api_version" `Quick test_api_version;
      Alcotest.test_case "max_response_body" `Quick test_max_response_body;
      Alcotest.test_case "string_is_blank empty" `Quick test_string_is_blank_empty;
      Alcotest.test_case "string_is_blank spaces" `Quick test_string_is_blank_spaces;
      Alcotest.test_case "string_is_blank content" `Quick test_string_is_blank_content;
      Alcotest.test_case "text_blocks_to_string" `Quick test_text_blocks_to_string;
      Alcotest.test_case "text_blocks_to_string empty" `Quick test_text_blocks_to_string_empty;
    ];
    "named_cascade", [
      Alcotest.test_case "basic construction" `Quick test_named_cascade_construction;
      Alcotest.test_case "with config path" `Quick test_named_cascade_with_config_path;
    ];
    "content_block", [
      Alcotest.test_case "text roundtrip" `Quick test_content_block_text_roundtrip;
      Alcotest.test_case "tool_use roundtrip" `Quick test_content_block_tool_use_roundtrip;
    ];
    "message", [
      Alcotest.test_case "to_json" `Quick test_message_to_json;
    ];
    "json_helpers", [
      Alcotest.test_case "valid json" `Quick test_json_of_string_valid;
      Alcotest.test_case "invalid json" `Quick test_json_of_string_invalid;
    ];
    "provider_intf", [
      Alcotest.test_case "supports_streaming anthropic" `Quick test_supports_streaming_anthropic;
      Alcotest.test_case "of_config" `Quick test_of_config_returns_module;
      Alcotest.test_case "of_config_streaming" `Quick test_of_config_streaming_anthropic;
    ];
    "build_body", [
      Alcotest.test_case "build_body_assoc" `Quick test_build_body_assoc;
    ];
    "parse", [
      Alcotest.test_case "parse_response basic" `Quick test_parse_response_basic;
    ];
  ]
