(** Unit tests for OpenAI-compatible SSE streaming parser. *)

open Llm_provider.Types

module S = Llm_provider.Streaming

(* ── parse_openai_sse_chunk ─────────────────────────────── *)

let test_parse_text_chunk () =
  let data = {|{"id":"chatcmpl-abc","object":"chat.completion.chunk","model":"gpt-4","choices":[{"index":0,"delta":{"content":"Hello"},"finish_reason":null}]}|} in
  match S.parse_openai_sse_chunk data with
  | Some chunk ->
      Alcotest.(check string) "id" "chatcmpl-abc" chunk.chunk_id;
      Alcotest.(check string) "model" "gpt-4" chunk.chunk_model;
      Alcotest.(check (option string)) "content" (Some "Hello") chunk.delta_content;
      Alcotest.(check (option string)) "finish" None chunk.finish_reason;
      Alcotest.(check int) "no tool_calls" 0 (List.length chunk.delta_tool_calls)
  | None -> Alcotest.fail "expected Some chunk"

let test_parse_done_sentinel () =
  match S.parse_openai_sse_chunk "[DONE]" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for [DONE]"

let test_parse_finish_reason () =
  let data = {|{"id":"c-1","model":"m","choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}|} in
  match S.parse_openai_sse_chunk data with
  | Some chunk ->
      Alcotest.(check (option string)) "finish" (Some "stop") chunk.finish_reason;
      Alcotest.(check (option string)) "no content" None chunk.delta_content
  | None -> Alcotest.fail "expected Some chunk"

let test_parse_tool_call_start () =
  let data = {|{"id":"c-2","model":"m","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"id":"call_abc","type":"function","function":{"name":"get_weather","arguments":""}}]},"finish_reason":null}]}|} in
  match S.parse_openai_sse_chunk data with
  | Some chunk ->
      Alcotest.(check int) "1 tool_call" 1 (List.length chunk.delta_tool_calls);
      let tc = List.hd chunk.delta_tool_calls in
      Alcotest.(check int) "tc_index" 0 tc.tc_index;
      Alcotest.(check (option string)) "tc_id" (Some "call_abc") tc.tc_id;
      Alcotest.(check (option string)) "tc_name" (Some "get_weather") tc.tc_name;
      Alcotest.(check (option string)) "tc_args" (Some "") tc.tc_arguments
  | None -> Alcotest.fail "expected Some chunk"

let test_parse_tool_call_args () =
  let data = {|{"id":"c-3","model":"m","choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"loc"}}]},"finish_reason":null}]}|} in
  match S.parse_openai_sse_chunk data with
  | Some chunk ->
      let tc = List.hd chunk.delta_tool_calls in
      Alcotest.(check (option string)) "args" (Some {|{"loc|}) tc.tc_arguments;
      Alcotest.(check (option string)) "no id" None tc.tc_id;
      Alcotest.(check (option string)) "no name" None tc.tc_name
  | None -> Alcotest.fail "expected Some chunk"

let test_parse_usage () =
  let data = {|{"id":"c-4","model":"m","choices":[{"index":0,"delta":{},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5,"total_tokens":15}}|} in
  match S.parse_openai_sse_chunk data with
  | Some chunk ->
      (match chunk.chunk_usage with
       | Some u ->
           Alcotest.(check int) "input" 10 u.input_tokens;
           Alcotest.(check int) "output" 5 u.output_tokens
       | None -> Alcotest.fail "expected usage")
  | None -> Alcotest.fail "expected Some chunk"

let test_parse_invalid_json () =
  match S.parse_openai_sse_chunk "not json" with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for invalid JSON"

let test_parse_empty_choices () =
  let data = {|{"id":"c-5","model":"m","choices":[]}|} in
  match S.parse_openai_sse_chunk data with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty choices"

(* ── openai_chunk_to_events ─────────────────────────────── *)

let test_events_text_first_chunk () =
  let state = S.create_openai_stream_state () in
  let chunk : S.openai_chunk = {
    chunk_id = "c"; chunk_model = "m"; delta_content = Some "Hi";
    delta_tool_calls = []; finish_reason = None; chunk_usage = None;
  } in
  let events = S.openai_chunk_to_events state chunk in
  Alcotest.(check int) "2 events" 2 (List.length events);
  (match List.nth events 0 with
   | ContentBlockStart { index = 0; content_type; _ } ->
       Alcotest.(check string) "text type" "text" content_type
   | _ -> Alcotest.fail "expected ContentBlockStart");
  (match List.nth events 1 with
   | ContentBlockDelta { index = 0; delta = TextDelta s } ->
       Alcotest.(check string) "text" "Hi" s
   | _ -> Alcotest.fail "expected TextDelta")

let test_events_text_subsequent () =
  let state = S.create_openai_stream_state () in
  (* First chunk starts the block *)
  let _ = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = Some "A";
      delta_tool_calls = []; finish_reason = None; chunk_usage = None } in
  (* Second chunk: no ContentBlockStart *)
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = Some "B";
      delta_tool_calls = []; finish_reason = None; chunk_usage = None } in
  Alcotest.(check int) "1 event" 1 (List.length events);
  (match List.hd events with
   | ContentBlockDelta { delta = TextDelta s; _ } ->
       Alcotest.(check string) "text" "B" s
   | _ -> Alcotest.fail "expected TextDelta only")

let test_events_tool_call () =
  let state = S.create_openai_stream_state () in
  let tc : S.openai_tool_call_delta = {
    tc_index = 0; tc_id = Some "call_1";
    tc_name = Some "calc"; tc_arguments = Some "{\"x\":1}";
  } in
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = None;
      delta_tool_calls = [tc]; finish_reason = None; chunk_usage = None } in
  Alcotest.(check int) "2 events" 2 (List.length events);
  (match List.nth events 0 with
   | ContentBlockStart { content_type; tool_id; tool_name; _ } ->
       Alcotest.(check string) "type" "tool_use" content_type;
       Alcotest.(check (option string)) "tool_id" (Some "call_1") tool_id;
       Alcotest.(check (option string)) "tool_name" (Some "calc") tool_name
   | _ -> Alcotest.fail "expected ContentBlockStart tool_use");
  (match List.nth events 1 with
   | ContentBlockDelta { delta = InputJsonDelta s; _ } ->
       Alcotest.(check string) "args" {|{"x":1}|} s
   | _ -> Alcotest.fail "expected InputJsonDelta")

let test_events_finish_reason () =
  let state = S.create_openai_stream_state () in
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = None;
      delta_tool_calls = [];
      finish_reason = Some "stop"; chunk_usage = None } in
  Alcotest.(check int) "1 event" 1 (List.length events);
  (match List.hd events with
   | MessageDelta { stop_reason = Some EndTurn; _ } -> ()
   | _ -> Alcotest.fail "expected MessageDelta EndTurn")

let test_events_tool_calls_finish () =
  let state = S.create_openai_stream_state () in
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = None;
      delta_tool_calls = [];
      finish_reason = Some "tool_calls"; chunk_usage = None } in
  (match List.hd events with
   | MessageDelta { stop_reason = Some StopToolUse; _ } -> ()
   | _ -> Alcotest.fail "expected StopToolUse")

let test_events_length_finish () =
  let state = S.create_openai_stream_state () in
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = None;
      delta_tool_calls = [];
      finish_reason = Some "length"; chunk_usage = None } in
  (match List.hd events with
   | MessageDelta { stop_reason = Some MaxTokens; _ } -> ()
   | _ -> Alcotest.fail "expected MaxTokens")

let test_events_empty_content_ignored () =
  let state = S.create_openai_stream_state () in
  let events = S.openai_chunk_to_events state
    { chunk_id = "c"; chunk_model = "m"; delta_content = Some "";
      delta_tool_calls = []; finish_reason = None; chunk_usage = None } in
  Alcotest.(check int) "0 events" 0 (List.length events)

let () =
  let open Alcotest in
  run "streaming_openai" [
    "parse_openai_sse_chunk", [
      test_case "text chunk" `Quick test_parse_text_chunk;
      test_case "[DONE] sentinel" `Quick test_parse_done_sentinel;
      test_case "finish_reason" `Quick test_parse_finish_reason;
      test_case "tool_call start" `Quick test_parse_tool_call_start;
      test_case "tool_call args" `Quick test_parse_tool_call_args;
      test_case "usage" `Quick test_parse_usage;
      test_case "invalid JSON" `Quick test_parse_invalid_json;
      test_case "empty choices" `Quick test_parse_empty_choices;
    ];
    "openai_chunk_to_events", [
      test_case "text first chunk" `Quick test_events_text_first_chunk;
      test_case "text subsequent" `Quick test_events_text_subsequent;
      test_case "tool_call" `Quick test_events_tool_call;
      test_case "finish stop" `Quick test_events_finish_reason;
      test_case "finish tool_calls" `Quick test_events_tool_calls_finish;
      test_case "finish length" `Quick test_events_length_finish;
      test_case "empty content ignored" `Quick test_events_empty_content_ignored;
    ];
  ]
