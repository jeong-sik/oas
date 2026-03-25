(** Targeted tests for coverage gaps in api.ml, streaming.ml, structured.ml.

    Covers instrumentation points missed by existing tests:
    - Api module re-exports (api.ml lines 6-29)
    - Streaming.emit_synthetic_events for remaining block types
    - Streaming.parse_sse_event edge cases
    - Structured.schema_to_tool_json param type variants *)

open Agent_sdk
open Types

(* ================================================================== *)
(* Api re-export coverage                                              *)
(* These call through Api.* to hit the re-export let bindings         *)
(* ================================================================== *)

let test_api_default_base_url () =
  Alcotest.(check bool) "non-empty" true (String.length Api.default_base_url > 0)

let test_api_version () =
  Alcotest.(check bool) "non-empty" true (String.length Api.api_version > 0)

let test_api_string_is_blank () =
  Alcotest.(check bool) "empty" true (Api.string_is_blank "");
  Alcotest.(check bool) "spaces" true (Api.string_is_blank "   ");
  Alcotest.(check bool) "not blank" false (Api.string_is_blank "a")

let test_api_text_blocks_to_string () =
  let blocks = [Text "hello"; Text "world"] in
  let result = Api.text_blocks_to_string blocks in
  Alcotest.(check string) "joined" "hello\nworld" result

let test_api_text_blocks_empty () =
  let result = Api.text_blocks_to_string [] in
  Alcotest.(check string) "empty" "" result

let test_api_text_blocks_non_text () =
  let blocks = [
    Text "a";
    Thinking { thinking_type = "sig"; content = "think" };
    Text "b";
  ] in
  let result = Api.text_blocks_to_string blocks in
  (* text_blocks_to_string includes Thinking content *)
  Alcotest.(check string) "includes thinking" "a\nthink\nb" result

let test_api_json_of_string_or_raw () =
  let json = Api.json_of_string_or_raw {|{"key":"val"}|} in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "parsed" "val" (json |> member "key" |> to_string)

let test_api_json_of_string_or_raw_plain () =
  let json = Api.json_of_string_or_raw "hello world" in
  Alcotest.(check bool) "non-null" true (json <> `Null)

let test_api_content_block_roundtrip () =
  let block = Text "test" in
  let json = Api.content_block_to_json block in
  let roundtrip = Api.content_block_of_json json in
  match roundtrip with
  | Some (Text s) -> Alcotest.(check string) "roundtrip" "test" s
  | _ -> Alcotest.fail "expected Some Text"

let test_api_message_to_json () =
  let msg = { role = User; content = [Text "hi"]; name = None; tool_call_id = None } in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "user" (json |> member "role" |> to_string)

let test_api_parse_response () =
  let json = `Assoc [
    ("id", `String "msg_1");
    ("model", `String "test");
    ("stop_reason", `String "end_turn");
    ("content", `List [`Assoc [("type", `String "text"); ("text", `String "hi")]]);
    ("usage", `Assoc [
      ("input_tokens", `Int 10);
      ("output_tokens", `Int 5);
    ]);
  ] in
  let resp = Api.parse_response json in
  Alcotest.(check string) "id" "msg_1" resp.id;
  Alcotest.(check string) "model" "test" resp.model

let test_api_build_body_assoc () =
  let config = { default_config with name = "test" } in
  let state = { config; messages = []; turn_count = 0; usage = empty_usage } in
  let msgs = [{ role = User; content = [Text "hello"]; name = None; tool_call_id = None }] in
  let assoc = Api.build_body_assoc ~config:state ~messages:msgs ~stream:false () in
  Alcotest.(check bool) "has model" true
    (List.exists (fun (k, _) -> k = "model") assoc)

let test_api_openai_messages () =
  let msg = { role = User; content = [Text "hi"]; name = None; tool_call_id = None } in
  let result = Api.openai_messages_of_message msg in
  Alcotest.(check bool) "non-empty" true (List.length result > 0)

let test_api_parse_openai_response () =
  let body = {|{"id":"chatcmpl-1","model":"gpt-4o","choices":[{"message":{"role":"assistant","content":"hello"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5}}|} in
  match Api.parse_openai_response_result body with
  | Ok resp -> Alcotest.(check string) "id" "chatcmpl-1" resp.id
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)


(* ================================================================== *)
(* Streaming: emit_synthetic_events remaining block types              *)
(* ================================================================== *)

let collect_events response =
  let events = ref [] in
  Streaming.emit_synthetic_events response (fun evt -> events := evt :: !events);
  List.rev !events

let test_synthetic_document () =
  let response : api_response = {
    id = "msg-doc"; model = "test"; stop_reason = EndTurn;
    content = [Document { media_type = "application/pdf"; data = "base64data"; source_type = "base64" }];
    usage = None;
  } in
  let events = collect_events response in
  Alcotest.(check int) "5 events (no delta for document)" 5 (List.length events);
  (match List.nth events 1 with
   | ContentBlockStart { content_type; _ } ->
     Alcotest.(check string) "type" "text" content_type
   | _ -> Alcotest.fail "expected ContentBlockStart")

let test_synthetic_audio () =
  let response : api_response = {
    id = "msg-audio"; model = "test"; stop_reason = EndTurn;
    content = [Audio { media_type = "audio/wav"; data = "wavdata"; source_type = "base64" }];
    usage = None;
  } in
  let events = collect_events response in
  Alcotest.(check int) "5 events (no delta for audio)" 5 (List.length events)

let test_synthetic_redacted_thinking () =
  let response : api_response = {
    id = "msg-rt"; model = "test"; stop_reason = EndTurn;
    content = [RedactedThinking "redacted"];
    usage = None;
  } in
  let events = collect_events response in
  Alcotest.(check int) "5 events" 5 (List.length events)

let test_synthetic_tool_result () =
  let response : api_response = {
    id = "msg-tr"; model = "test"; stop_reason = EndTurn;
    content = [ToolResult { tool_use_id = "tu1"; content = "result"; is_error = false }];
    usage = None;
  } in
  let events = collect_events response in
  Alcotest.(check int) "5 events" 5 (List.length events)

(* ================================================================== *)
(* Streaming: parse_sse_event edge cases                               *)
(* ================================================================== *)

let test_parse_message_start_null_usage () =
  let data = {|{"type":"message_start","message":{"id":"msg_nu","model":"test","usage":null}}|} in
  match Streaming.parse_sse_event None data with
  | Some (MessageStart { usage = None; _ }) -> ()
  | Some (MessageStart { usage = Some _; _ }) ->
    Alcotest.fail "expected None usage for null"
  | _ -> Alcotest.fail "expected MessageStart"

let test_parse_content_block_start_tool_use () =
  let data = {|{"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"tu_123","name":"calculator"}}|} in
  match Streaming.parse_sse_event None data with
  | Some (ContentBlockStart { index; content_type; tool_id; tool_name }) ->
    Alcotest.(check int) "index" 1 index;
    Alcotest.(check string) "type" "tool_use" content_type;
    Alcotest.(check (option string)) "tool_id" (Some "tu_123") tool_id;
    Alcotest.(check (option string)) "tool_name" (Some "calculator") tool_name
  | _ -> Alcotest.fail "expected ContentBlockStart for tool_use"

let test_parse_delta_unknown_type () =
  let data = {|{"type":"content_block_delta","index":0,"delta":{"type":"future_delta","data":"x"}}|} in
  match Streaming.parse_sse_event None data with
  | Some (ContentBlockDelta { delta = TextDelta s; _ }) ->
    Alcotest.(check string) "fallback to empty TextDelta" "" s
  | _ -> Alcotest.fail "expected ContentBlockDelta with empty TextDelta"

let test_parse_message_delta_no_stop_reason () =
  let data = {|{"type":"message_delta","delta":{},"usage":{"output_tokens":10}}|} in
  match Streaming.parse_sse_event None data with
  | Some (MessageDelta { stop_reason = None; usage = Some u }) ->
    Alcotest.(check int) "output" 10 u.output_tokens
  | _ -> Alcotest.fail "expected MessageDelta with no stop_reason"

let test_parse_message_delta_null_usage () =
  let data = {|{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":null}|} in
  match Streaming.parse_sse_event None data with
  | Some (MessageDelta { stop_reason = Some EndTurn; usage = None }) -> ()
  | _ -> Alcotest.fail "expected MessageDelta with null usage"

let test_parse_message_delta_max_tokens () =
  let data = {|{"type":"message_delta","delta":{"stop_reason":"max_tokens"},"usage":{"output_tokens":4096}}|} in
  match Streaming.parse_sse_event None data with
  | Some (MessageDelta { stop_reason = Some MaxTokens; _ }) -> ()
  | _ -> Alcotest.fail "expected MaxTokens stop_reason"

let test_parse_message_delta_tool_use () =
  let data = {|{"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":100}}|} in
  match Streaming.parse_sse_event None data with
  | Some (MessageDelta { stop_reason = Some StopToolUse; _ }) -> ()
  | _ -> Alcotest.fail "expected StopToolUse stop_reason"

(* ================================================================== *)
(* Structured: param_type variant coverage                             *)
(* ================================================================== *)

let test_schema_all_param_types () =
  let schema : unit Structured.schema = {
    name = "all_types"; description = "Test all param types";
    params = [
      { name = "s"; description = ""; param_type = String; required = true };
      { name = "i"; description = ""; param_type = Integer; required = true };
      { name = "n"; description = ""; param_type = Number; required = true };
      { name = "b"; description = ""; param_type = Boolean; required = true };
      { name = "a"; description = ""; param_type = Array; required = false };
      { name = "o"; description = ""; param_type = Object; required = false };
    ];
    parse = (fun _ -> Ok ());
  } in
  let json = Structured.schema_to_tool_json schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  Alcotest.(check string) "string" "string" (props |> member "s" |> member "type" |> to_string);
  Alcotest.(check string) "integer" "integer" (props |> member "i" |> member "type" |> to_string);
  Alcotest.(check string) "number" "number" (props |> member "n" |> member "type" |> to_string);
  Alcotest.(check string) "boolean" "boolean" (props |> member "b" |> member "type" |> to_string);
  Alcotest.(check string) "array" "array" (props |> member "a" |> member "type" |> to_string);
  Alcotest.(check string) "object" "object" (props |> member "o" |> member "type" |> to_string)

(* ================================================================== *)
(* Runner                                                              *)
(* ================================================================== *)

let () =
  Alcotest.run "coverage_gaps" [
    "api_reexports", [
      Alcotest.test_case "default_base_url" `Quick test_api_default_base_url;
      Alcotest.test_case "api_version" `Quick test_api_version;
      Alcotest.test_case "string_is_blank" `Quick test_api_string_is_blank;
      Alcotest.test_case "text_blocks_to_string" `Quick test_api_text_blocks_to_string;
      Alcotest.test_case "text_blocks_empty" `Quick test_api_text_blocks_empty;
      Alcotest.test_case "text_blocks_non_text" `Quick test_api_text_blocks_non_text;
      Alcotest.test_case "json_of_string_or_raw" `Quick test_api_json_of_string_or_raw;
      Alcotest.test_case "json_of_string_or_raw plain" `Quick test_api_json_of_string_or_raw_plain;
      Alcotest.test_case "content_block_roundtrip" `Quick test_api_content_block_roundtrip;
      Alcotest.test_case "message_to_json" `Quick test_api_message_to_json;
      Alcotest.test_case "parse_response" `Quick test_api_parse_response;
      Alcotest.test_case "build_body_assoc" `Quick test_api_build_body_assoc;
      Alcotest.test_case "openai_messages" `Quick test_api_openai_messages;
      Alcotest.test_case "parse_openai_response" `Quick test_api_parse_openai_response;
    ];
    "streaming_block_types", [
      Alcotest.test_case "document" `Quick test_synthetic_document;
      Alcotest.test_case "audio" `Quick test_synthetic_audio;
      Alcotest.test_case "redacted_thinking" `Quick test_synthetic_redacted_thinking;
      Alcotest.test_case "tool_result" `Quick test_synthetic_tool_result;
    ];
    "streaming_parse_edge", [
      Alcotest.test_case "null usage" `Quick test_parse_message_start_null_usage;
      Alcotest.test_case "tool_use block start" `Quick test_parse_content_block_start_tool_use;
      Alcotest.test_case "unknown delta type" `Quick test_parse_delta_unknown_type;
      Alcotest.test_case "no stop_reason" `Quick test_parse_message_delta_no_stop_reason;
      Alcotest.test_case "null usage delta" `Quick test_parse_message_delta_null_usage;
      Alcotest.test_case "max_tokens" `Quick test_parse_message_delta_max_tokens;
      Alcotest.test_case "tool_use stop" `Quick test_parse_message_delta_tool_use;
    ];
    "structured_types", [
      Alcotest.test_case "all param types" `Quick test_schema_all_param_types;
    ];
  ]
