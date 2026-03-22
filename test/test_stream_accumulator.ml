(** Unit tests for Streaming.{create_stream_acc, accumulate_event, finalize_stream_acc}. *)

open Agent_sdk
open Types

(* ── Helpers ──────────────────────────────────────────────── *)

let make_usage ?(cache_create=0) ?(cache_read=0) inp out =
  { input_tokens = inp; output_tokens = out;
    cache_creation_input_tokens = cache_create;
    cache_read_input_tokens = cache_read }

let acc_events acc events =
  List.iter (Streaming.accumulate_event acc) events

(* ── create_stream_acc ────────────────────────────────────── *)

let test_create_initial_state () =
  let acc = Streaming.create_stream_acc () in
  Alcotest.(check string) "msg_id empty" "" !(acc.msg_id);
  Alcotest.(check string) "msg_model empty" "" !(acc.msg_model);
  Alcotest.(check int) "input_tokens 0" 0 !(acc.input_tokens);
  Alcotest.(check int) "output_tokens 0" 0 !(acc.output_tokens);
  Alcotest.(check int) "cache_creation 0" 0 !(acc.cache_creation);
  Alcotest.(check int) "cache_read 0" 0 !(acc.cache_read)

(* ── accumulate: MessageStart ─────────────────────────────── *)

let test_accumulate_message_start () =
  let acc = Streaming.create_stream_acc () in
  let evt = MessageStart {
    id = "msg_123"; model = "claude-sonnet-4";
    usage = Some (make_usage 100 0)
  } in
  Streaming.accumulate_event acc evt;
  Alcotest.(check string) "id set" "msg_123" !(acc.msg_id);
  Alcotest.(check string) "model set" "claude-sonnet-4" !(acc.msg_model);
  Alcotest.(check int) "input_tokens" 100 !(acc.input_tokens)

let test_accumulate_message_start_no_usage () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (MessageStart { id = "m1"; model = "test"; usage = None });
  Alcotest.(check string) "id" "m1" !(acc.msg_id);
  Alcotest.(check int) "input stays 0" 0 !(acc.input_tokens)

let test_accumulate_message_start_with_cache () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (MessageStart { id = "m2"; model = "test";
      usage = Some (make_usage ~cache_create:50 ~cache_read:30 200 0) });
  Alcotest.(check int) "cache_creation" 50 !(acc.cache_creation);
  Alcotest.(check int) "cache_read" 30 !(acc.cache_read)

(* ── accumulate: ContentBlockStart ────────────────────────── *)

let test_accumulate_content_block_start_text () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockStart { index = 0; content_type = "text";
                         tool_id = None; tool_name = None });
  Alcotest.(check int) "block_types has entry" 1
    (Hashtbl.length acc.block_types);
  Alcotest.(check string) "type is text" "text"
    (Hashtbl.find acc.block_types 0)

let test_accumulate_content_block_start_tool () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockStart { index = 1; content_type = "tool_use";
                         tool_id = Some "tu_1"; tool_name = Some "calculator" });
  Alcotest.(check string) "tool_id" "tu_1"
    (Hashtbl.find acc.block_tool_ids 1);
  Alcotest.(check string) "tool_name" "calculator"
    (Hashtbl.find acc.block_tool_names 1)

(* ── accumulate: ContentBlockDelta ────────────────────────── *)

let test_accumulate_text_deltas () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockStart { index = 0; content_type = "text";
                         tool_id = None; tool_name = None });
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 0; delta = TextDelta "hello " });
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 0; delta = TextDelta "world" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Alcotest.(check string) "concatenated" "hello world" (Buffer.contents buf)

let test_accumulate_delta_without_start () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 5; delta = TextDelta "orphan" });
  let buf = Hashtbl.find acc.block_texts 5 in
  Alcotest.(check string) "buffer created on-demand" "orphan"
    (Buffer.contents buf)

let test_accumulate_thinking_delta () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockStart { index = 0; content_type = "thinking";
                         tool_id = None; tool_name = None });
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 0; delta = ThinkingDelta "I think" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Alcotest.(check string) "thinking text" "I think" (Buffer.contents buf)

let test_accumulate_input_json_delta () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (ContentBlockStart { index = 0; content_type = "tool_use";
                         tool_id = Some "t1"; tool_name = Some "calc" });
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 0; delta = InputJsonDelta "{\"x\":" });
  Streaming.accumulate_event acc
    (ContentBlockDelta { index = 0; delta = InputJsonDelta "42}" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Alcotest.(check string) "json assembled" "{\"x\":42}"
    (Buffer.contents buf)

(* ── accumulate: MessageDelta ─────────────────────────────── *)

let test_accumulate_message_delta () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (MessageDelta { stop_reason = Some EndTurn;
                    usage = Some (make_usage 0 75) });
  Alcotest.(check int) "output_tokens" 75 !(acc.output_tokens);
  (match !(acc.stop_reason) with
   | EndTurn -> ()
   | _ -> Alcotest.fail "expected EndTurn")

let test_accumulate_message_delta_no_stop () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (MessageDelta { stop_reason = None; usage = None });
  (match !(acc.stop_reason) with
   | EndTurn -> ()
   | _ -> Alcotest.fail "default should be EndTurn")

let test_accumulate_message_delta_cache_update () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc
    (MessageDelta { stop_reason = Some EndTurn;
                    usage = Some { input_tokens = 0; output_tokens = 50;
                                   cache_creation_input_tokens = 25;
                                   cache_read_input_tokens = 10 } });
  Alcotest.(check int) "cache_creation updated" 25 !(acc.cache_creation);
  Alcotest.(check int) "cache_read updated" 10 !(acc.cache_read)

(* ── accumulate: ignored events ───────────────────────────── *)

let test_accumulate_ignores_ping () =
  let acc = Streaming.create_stream_acc () in
  Streaming.accumulate_event acc Ping;
  Streaming.accumulate_event acc MessageStop;
  Streaming.accumulate_event acc (ContentBlockStop { index = 0 });
  Streaming.accumulate_event acc (SSEError "oops");
  Alcotest.(check string) "state unchanged" "" !(acc.msg_id)

(* ── finalize_stream_acc ──────────────────────────────────── *)

let test_finalize_text_response () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "msg_f"; model = "test";
                   usage = Some (make_usage 100 0) };
    ContentBlockStart { index = 0; content_type = "text";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = TextDelta "hello world" };
    MessageDelta { stop_reason = Some EndTurn;
                   usage = Some (make_usage 0 50) };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    Alcotest.(check string) "id" "msg_f" resp.id;
    Alcotest.(check string) "model" "test" resp.model;
    (match resp.stop_reason with EndTurn -> () | _ -> Alcotest.fail "expected EndTurn");
    Alcotest.(check int) "1 content block" 1 (List.length resp.content);
    (match List.hd resp.content with
     | Text s -> Alcotest.(check string) "text" "hello world" s
     | _ -> Alcotest.fail "expected Text");
    (match resp.usage with
     | Some u ->
       Alcotest.(check int) "input" 100 u.input_tokens;
       Alcotest.(check int) "output" 50 u.output_tokens
     | None -> Alcotest.fail "expected usage"))

let test_finalize_thinking_block () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "thinking";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = ThinkingDelta "reasoning here" };
    MessageDelta { stop_reason = Some EndTurn; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    (match List.hd resp.content with
     | Thinking { content; _ } ->
       Alcotest.(check string) "thinking" "reasoning here" content
     | _ -> Alcotest.fail "expected Thinking"))

let test_finalize_tool_use () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "tool_use";
                        tool_id = Some "tu_1"; tool_name = Some "calc" };
    ContentBlockDelta { index = 0; delta = InputJsonDelta "{\"x\":42}" };
    MessageDelta { stop_reason = Some StopToolUse; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    (match List.hd resp.content with
     | ToolUse { id; name; input } ->
       Alcotest.(check string) "tool_id" "tu_1" id;
       Alcotest.(check string) "tool_name" "calc" name;
       Alcotest.(check string) "input" "{\"x\":42}"
         (Yojson.Safe.to_string input)
     | _ -> Alcotest.fail "expected ToolUse"))

let test_finalize_tool_use_invalid_json () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "tool_use";
                        tool_id = Some "tu_2"; tool_name = Some "bad" };
    ContentBlockDelta { index = 0; delta = InputJsonDelta "not json{" };
    MessageDelta { stop_reason = Some EndTurn; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    (match List.hd resp.content with
     | Text s -> Alcotest.(check string) "fallback to text" "not json{" s
     | _ -> Alcotest.fail "expected Text fallback for invalid JSON"))

let test_finalize_multi_block_ordering () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "thinking";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = ThinkingDelta "think" };
    ContentBlockStart { index = 1; content_type = "text";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 1; delta = TextDelta "answer" };
    MessageDelta { stop_reason = Some EndTurn; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    Alcotest.(check int) "2 blocks" 2 (List.length resp.content);
    (match resp.content with
     | [Thinking _; Text "answer"] -> ()
     | _ -> Alcotest.fail "expected [Thinking; Text] in order"))

let test_finalize_no_usage () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "text";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = TextDelta "x" };
    MessageDelta { stop_reason = Some EndTurn; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    Alcotest.(check bool) "no usage" true (Option.is_none resp.usage))

let test_finalize_unknown_block_type_skipped () =
  let acc = Streaming.create_stream_acc () in
  acc_events acc [
    MessageStart { id = "m"; model = "m"; usage = None };
    ContentBlockStart { index = 0; content_type = "future_type";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = TextDelta "data" };
    MessageDelta { stop_reason = Some EndTurn; usage = None };
  ];
  (match Streaming.finalize_stream_acc acc with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    Alcotest.(check int) "unknown type skipped" 0 (List.length resp.content))

(* ── map_http_error ───────────────────────────────────────── *)

let test_map_http_error_http () =
  let err = Streaming.map_http_error
    (Llm_provider.Http_client.HttpError { code = 429; body = "rate limited" }) in
  (match err with
   | Error.Api (Retry.RateLimited _) -> ()
   | _ -> Alcotest.fail "expected RateLimited")

let test_map_http_error_network () =
  let err = Streaming.map_http_error
    (Llm_provider.Http_client.NetworkError { message = "connection refused" }) in
  (match err with
   | Error.Api (Retry.NetworkError { message }) ->
     Alcotest.(check string) "msg" "connection refused" message
   | _ -> Alcotest.fail "expected NetworkError")

(* ── Runner ───────────────────────────────────────────────── *)

let () =
  Alcotest.run "stream_accumulator" [
    "create", [
      Alcotest.test_case "initial state" `Quick test_create_initial_state;
    ];
    "accumulate", [
      Alcotest.test_case "message_start" `Quick test_accumulate_message_start;
      Alcotest.test_case "message_start no usage" `Quick test_accumulate_message_start_no_usage;
      Alcotest.test_case "message_start cache" `Quick test_accumulate_message_start_with_cache;
      Alcotest.test_case "block_start text" `Quick test_accumulate_content_block_start_text;
      Alcotest.test_case "block_start tool" `Quick test_accumulate_content_block_start_tool;
      Alcotest.test_case "text deltas concat" `Quick test_accumulate_text_deltas;
      Alcotest.test_case "delta without start" `Quick test_accumulate_delta_without_start;
      Alcotest.test_case "thinking delta" `Quick test_accumulate_thinking_delta;
      Alcotest.test_case "input_json delta" `Quick test_accumulate_input_json_delta;
      Alcotest.test_case "message_delta" `Quick test_accumulate_message_delta;
      Alcotest.test_case "message_delta no stop" `Quick test_accumulate_message_delta_no_stop;
      Alcotest.test_case "message_delta cache" `Quick test_accumulate_message_delta_cache_update;
      Alcotest.test_case "ignores ping/stop/error" `Quick test_accumulate_ignores_ping;
    ];
    "finalize", [
      Alcotest.test_case "text response" `Quick test_finalize_text_response;
      Alcotest.test_case "thinking block" `Quick test_finalize_thinking_block;
      Alcotest.test_case "tool_use" `Quick test_finalize_tool_use;
      Alcotest.test_case "tool_use invalid json" `Quick test_finalize_tool_use_invalid_json;
      Alcotest.test_case "multi block ordering" `Quick test_finalize_multi_block_ordering;
      Alcotest.test_case "no usage" `Quick test_finalize_no_usage;
      Alcotest.test_case "unknown type skipped" `Quick test_finalize_unknown_block_type_skipped;
    ];
    "map_http_error", [
      Alcotest.test_case "http error" `Quick test_map_http_error_http;
      Alcotest.test_case "network error" `Quick test_map_http_error_network;
    ];
  ]
