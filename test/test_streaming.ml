(** Unit tests for SSE streaming types and usage tracking. *)

open Agent_sdk.Types

(* Helper: create api_usage from input/output token counts *)
let make_usage inp out =
  { input_tokens = inp; output_tokens = out;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }

(* ------------------------------------------------------------------ *)
(* Usage tracking                                                       *)
(* ------------------------------------------------------------------ *)

let test_empty_usage () =
  Alcotest.(check int) "total_input_tokens is 0" 0 empty_usage.total_input_tokens;
  Alcotest.(check int) "total_output_tokens is 0" 0 empty_usage.total_output_tokens;
  Alcotest.(check int) "api_calls is 0" 0 empty_usage.api_calls

let test_add_usage_basic () =
  let s = add_usage empty_usage (make_usage 100 50) in
  Alcotest.(check int) "input_tokens accumulated" 100 s.total_input_tokens;
  Alcotest.(check int) "output_tokens accumulated" 50 s.total_output_tokens;
  Alcotest.(check int) "api_calls incremented" 1 s.api_calls

let test_add_usage_multiple () =
  let s0 = add_usage empty_usage (make_usage 100 50) in
  let s1 = add_usage s0 (make_usage 200 80) in
  Alcotest.(check int) "input_tokens summed" 300 s1.total_input_tokens;
  Alcotest.(check int) "output_tokens summed" 130 s1.total_output_tokens;
  Alcotest.(check int) "api_calls counted" 2 s1.api_calls

let test_add_usage_zero_tokens () =
  let s = add_usage empty_usage (make_usage 0 0) in
  Alcotest.(check int) "zero input accepted" 0 s.total_input_tokens;
  Alcotest.(check int) "zero output accepted" 0 s.total_output_tokens;
  Alcotest.(check int) "api_calls still incremented" 1 s.api_calls

let test_add_usage_immutable () =
  let s0 = add_usage empty_usage (make_usage 100 50) in
  let _s1 = add_usage s0 (make_usage 10 5) in
  (* s0 must not be mutated *)
  Alcotest.(check int) "original input_tokens unchanged" 100 s0.total_input_tokens;
  Alcotest.(check int) "original api_calls unchanged" 1 s0.api_calls

(* ------------------------------------------------------------------ *)
(* SSE event type construction                                          *)
(* ------------------------------------------------------------------ *)

let test_sse_message_start () =
  let evt = MessageStart { id = "msg_abc"; model = "claude-sonnet-4";
    usage = Some { input_tokens = 100; output_tokens = 0;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 } } in
  match evt with
  | MessageStart { id; model; usage = Some u } ->
    Alcotest.(check string) "id" "msg_abc" id;
    Alcotest.(check string) "model" "claude-sonnet-4" model;
    Alcotest.(check int) "input_tokens" 100 u.input_tokens
  | _ -> Alcotest.fail "expected MessageStart"

let test_sse_content_block_delta_text () =
  let delta = TextDelta "hello" in
  let evt = ContentBlockDelta { index = 0; delta } in
  match evt with
  | ContentBlockDelta { index; delta = TextDelta s } ->
    Alcotest.(check int) "index" 0 index;
    Alcotest.(check string) "text" "hello" s
  | _ -> Alcotest.fail "expected ContentBlockDelta with TextDelta"

let test_sse_message_stop () =
  match MessageStop with
  | MessageStop -> ()
  | _ -> Alcotest.fail "expected MessageStop"

let test_sse_ping () =
  match Ping with
  | Ping -> ()
  | _ -> Alcotest.fail "expected Ping"

let test_sse_error () =
  let evt = SSEError "overloaded" in
  match evt with
  | SSEError msg -> Alcotest.(check string) "error message" "overloaded" msg
  | _ -> Alcotest.fail "expected SSEError"

(* ------------------------------------------------------------------ *)
(* parse_sse_event                                                      *)
(* ------------------------------------------------------------------ *)

let test_parse_message_start () =
  let data = {|{"type":"message_start","message":{"id":"msg_01","model":"claude-3-7-sonnet-20250219","usage":{"input_tokens":42}}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (MessageStart { id; model; usage }) ->
    Alcotest.(check string) "id" "msg_01" id;
    Alcotest.(check string) "model" "claude-3-7-sonnet-20250219" model;
    (match usage with
     | Some u -> Alcotest.(check int) "input_tokens" 42 u.input_tokens
     | None -> Alcotest.fail "expected Some usage")
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_content_block_start () =
  let data = {|{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (ContentBlockStart { index; content_type; _ }) ->
    Alcotest.(check int) "index" 0 index;
    Alcotest.(check string) "content_type" "text" content_type
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_content_block_delta_text () =
  let data = {|{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"hello"}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (ContentBlockDelta { index; delta = TextDelta s }) ->
    Alcotest.(check int) "index" 0 index;
    Alcotest.(check string) "text" "hello" s
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_content_block_delta_thinking () =
  let data = {|{"type":"content_block_delta","index":1,"delta":{"type":"thinking_delta","thinking":"I think..."}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (ContentBlockDelta { index = 1; delta = ThinkingDelta s }) ->
    Alcotest.(check string) "thinking" "I think..." s
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_content_block_delta_input_json () =
  let data = {|{"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"{\"k\":"}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (ContentBlockDelta { index = 2; delta = InputJsonDelta s }) ->
    Alcotest.(check string) "partial_json" {|{"k":|}  s
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_content_block_stop () =
  let data = {|{"type":"content_block_stop","index":0}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (ContentBlockStop { index }) ->
    Alcotest.(check int) "index" 0 index
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_message_delta_end_turn () =
  let data = {|{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":57}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (MessageDelta { stop_reason; usage }) ->
    (match stop_reason with
     | Some EndTurn -> ()
     | _ -> Alcotest.fail "expected EndTurn stop_reason");
    (match usage with
     | Some u -> Alcotest.(check int) "output_tokens" 57 u.output_tokens
     | None -> Alcotest.fail "expected Some usage")
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_message_stop () =
  let data = {|{"type":"message_stop"}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some MessageStop -> ()
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_ping () =
  let data = {|{"type":"ping"}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some Ping -> ()
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_error_event () =
  let data = {|{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (SSEError msg) ->
    Alcotest.(check string) "error message" "Overloaded" msg
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_invalid_json () =
  match Agent_sdk.Api.parse_sse_event None "not json at all" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for invalid JSON"

let test_parse_unknown_event_type () =
  let data = {|{"type":"unknown_future_event","data":"x"}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for unknown event type"

let test_parse_message_start_with_cache () =
  let data = {|{"type":"message_start","message":{"id":"msg_cache","model":"claude-sonnet-4","usage":{"input_tokens":100,"cache_creation_input_tokens":50,"cache_read_input_tokens":30}}}|} in
  match Agent_sdk.Api.parse_sse_event None data with
  | Some (MessageStart { id; usage; _ }) ->
    Alcotest.(check string) "id" "msg_cache" id;
    (match usage with
     | Some u ->
       Alcotest.(check int) "input_tokens" 100 u.input_tokens;
       Alcotest.(check int) "cache_creation" 50 u.cache_creation_input_tokens;
       Alcotest.(check int) "cache_read" 30 u.cache_read_input_tokens
     | None -> Alcotest.fail "expected Some usage")
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

let test_parse_with_explicit_event_type () =
  (* event_type parameter overrides the 'type' field in JSON *)
  let data = {|{"message":{"id":"msg_02","model":"claude-haiku-4-5-20251001","usage":{"input_tokens":10}}}|} in
  match Agent_sdk.Api.parse_sse_event (Some "message_start") data with
  | Some (MessageStart { id; _ }) ->
    Alcotest.(check string) "id via explicit event type" "msg_02" id
  | Some _ -> Alcotest.fail "unexpected event type"
  | None -> Alcotest.fail "parse returned None"

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  let open Alcotest in
  run "streaming" [
    "usage_tracking", [
      test_case "empty_usage" `Quick test_empty_usage;
      test_case "add_usage_basic" `Quick test_add_usage_basic;
      test_case "add_usage_multiple" `Quick test_add_usage_multiple;
      test_case "add_usage_zero_tokens" `Quick test_add_usage_zero_tokens;
      test_case "add_usage_immutable" `Quick test_add_usage_immutable;
    ];
    "sse_event_types", [
      test_case "message_start" `Quick test_sse_message_start;
      test_case "content_block_delta_text" `Quick test_sse_content_block_delta_text;
      test_case "message_stop" `Quick test_sse_message_stop;
      test_case "ping" `Quick test_sse_ping;
      test_case "sse_error" `Quick test_sse_error;
    ];
    "parse_sse_event", [
      test_case "message_start" `Quick test_parse_message_start;
      test_case "content_block_start" `Quick test_parse_content_block_start;
      test_case "content_block_delta_text" `Quick test_parse_content_block_delta_text;
      test_case "content_block_delta_thinking" `Quick test_parse_content_block_delta_thinking;
      test_case "content_block_delta_input_json" `Quick test_parse_content_block_delta_input_json;
      test_case "content_block_stop" `Quick test_parse_content_block_stop;
      test_case "message_delta_end_turn" `Quick test_parse_message_delta_end_turn;
      test_case "message_stop" `Quick test_parse_message_stop;
      test_case "ping" `Quick test_parse_ping;
      test_case "error_event" `Quick test_parse_error_event;
      test_case "invalid_json" `Quick test_parse_invalid_json;
      test_case "unknown_event_type" `Quick test_parse_unknown_event_type;
      test_case "message_start_with_cache" `Quick test_parse_message_start_with_cache;
      test_case "explicit_event_type" `Quick test_parse_with_explicit_event_type;
    ];
  ]
