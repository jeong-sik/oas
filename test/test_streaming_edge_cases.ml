open Alcotest
open Llm_provider.Types
module S = Llm_provider.Streaming

let usage ?(input_tokens = 1) ?(output_tokens = 2) () =
  { input_tokens
  ; output_tokens
  ; cache_creation_input_tokens = 0
  ; cache_read_input_tokens = 0
  ; cost_usd = None
  }
;;

let openai_chunk
      ?delta_content
      ?delta_reasoning
      ?(delta_tool_calls = [])
      ?finish_reason
      ?chunk_usage
      ()
  : S.openai_chunk
  =
  { chunk_id = "chunk-1"
  ; chunk_model = "model-1"
  ; delta_content
  ; delta_reasoning
  ; delta_tool_calls
  ; finish_reason
  ; chunk_usage
  }
;;

let ollama_chunk
      ?delta_content
      ?delta_thinking
      ?(tool_calls = [])
      ?done_reason
      ?usage
      ?timings
      ?(is_done = false)
      ()
  : S.ollama_chunk
  =
  { oll_model = "dashscope-3:8b"
  ; oll_delta_content = delta_content
  ; oll_delta_thinking = delta_thinking
  ; oll_tool_calls = tool_calls
  ; oll_done_reason = done_reason
  ; oll_is_done = is_done
  ; oll_usage = usage
  ; oll_timings = timings
  }
;;

let check_event_count label expected events =
  check int label expected (List.length events)
;;

let require_some label = function
  | Some x -> x
  | None -> fail (label ^ ": expected Some")
;;

let test_anthropic_sse_parser_edges () =
  (match
     S.parse_sse_event
       (Some "message_start")
       {|{"message":{"id":"msg-1","model":"m","usage":null}}|}
   with
   | Some (MessageStart { usage = None; _ }) -> ()
   | _ -> fail "message_start without usage should parse");
  (match
     S.parse_sse_event
       (Some "content_block_delta")
       {|{"index":0,"delta":{"type":"future_delta","value":"ignored"}}|}
   with
   | Some (ContentBlockDelta { delta = TextDelta ""; _ }) -> ()
   | _ -> fail "unknown delta type should surface empty TextDelta");
  (match
     S.parse_sse_event
       (Some "message_delta")
       {|{"delta":{"stop_reason":null},"usage":null}|}
   with
   | Some (MessageDelta { stop_reason = None; usage = None }) -> ()
   | _ -> fail "message_delta without usage should parse");
  match S.parse_sse_event (Some "message_start") {|{"message":{"id":1}}|} with
  | Some (SSEParseFailed { reason; _ }) ->
    check bool "type error reason" true (String.starts_with ~prefix:"type_error:" reason)
  | _ -> fail "bad message_start shape should become SSEParseFailed"
;;

let test_first_token_classifier_edges () =
  check
    bool
    "thinking token"
    true
    (S.sse_event_is_first_token_signal
       (ContentBlockDelta { index = 0; delta = ThinkingDelta "plan" }));
  check
    bool
    "json token"
    true
    (S.sse_event_is_first_token_signal
       (ContentBlockDelta { index = 0; delta = InputJsonDelta "{}" }));
  check
    bool
    "empty thinking"
    false
    (S.sse_event_is_first_token_signal
       (ContentBlockDelta { index = 0; delta = ThinkingDelta "" }));
  List.iter
    (fun event ->
       check bool "non-token event" false (S.sse_event_is_first_token_signal event))
    [ ContentBlockStop { index = 0 }
    ; MessageDelta { stop_reason = None; usage = None }
    ; MessageStop
    ; SSEError { message = "boom"; error_type = None; raw = "boom" }
    ; SSEParseFailed { raw = "x"; reason = "bad" }
    ; SSEUnknownEventType { event_type = "future"; raw = "{}" }
    ]
;;

let test_deliverable_progress_classifier_edges () =
  check
    bool
    "text is deliverable"
    true
    (S.sse_event_is_deliverable_progress_signal
       (ContentBlockDelta { index = 0; delta = TextDelta "answer" }));
  check
    bool
    "tool args are deliverable"
    true
    (S.sse_event_is_deliverable_progress_signal
       (ContentBlockDelta { index = 0; delta = InputJsonDelta {|{"x":1}|} }));
  check
    bool
    "tool start is deliverable"
    true
    (S.sse_event_is_deliverable_progress_signal
       (ContentBlockStart
          { index = 0
          ; content_type = "tool_use"
          ; tool_id = Some "call_1"
          ; tool_name = Some "lookup"
          }));
  check
    bool
    "thinking is not deliverable"
    false
    (S.sse_event_is_deliverable_progress_signal
       (ContentBlockDelta { index = 0; delta = ThinkingDelta "private reasoning" }));
  check
    bool
    "empty text is not deliverable"
    false
    (S.sse_event_is_deliverable_progress_signal
       (ContentBlockDelta { index = 0; delta = TextDelta "" }))
;;

let test_thinking_only_timeout_exceeded () =
  check
    bool
    "below timeout"
    false
    (S.thinking_only_timeout_exceeded ~timeout_s:120.0 ~started_at:10.0 ~now:129.9);
  check
    bool
    "at timeout"
    true
    (S.thinking_only_timeout_exceeded ~timeout_s:120.0 ~started_at:10.0 ~now:130.0);
  check
    bool
    "past timeout"
    true
    (S.thinking_only_timeout_exceeded ~timeout_s:120.0 ~started_at:10.0 ~now:131.0)
;;

let test_synthetic_events_media_blocks () =
  let response : api_response =
    { id = "r1"
    ; model = "m1"
    ; stop_reason = EndTurn
    ; usage = Some (usage ())
    ; telemetry = None
    ; content =
        [ Image { media_type = "image/png"; data = "aW1n"; source_type = "base64" }
        ; Document
            { media_type = "application/pdf"; data = "ZG9j"; source_type = "base64" }
        ; Audio { media_type = "wav"; data = "YXVkaW8="; source_type = "base64" }
        ; RedactedThinking "hidden"
        ; ToolResult
            { tool_use_id = "call-1"
            ; content = "ok"
            ; is_error = false
            ; json = None
            ; content_blocks = None
            }
        ]
    }
  in
  let events = ref [] in
  S.emit_synthetic_events response (fun event -> events := event :: !events);
  let events = List.rev !events in
  check_event_count "start + 5 block starts/stops + 3 media deltas + stop" 16 events;
  let starts =
    List.filter_map
      (function
        | ContentBlockStart { content_type; tool_id; _ } -> Some (content_type, tool_id)
        | _ -> None)
      events
  in
  (* Image/Document/Audio use typed media starts/deltas so synthetic streaming
     preserves non-text payloads; RedactedThinking keeps its opaque payload for
     tool-loop round-trips (#2061). *)
  check
    (list (pair string (option string)))
    "media-like synthetic starts"
    [ "image", None
    ; "document", None
    ; "audio", None
    ; "redacted_thinking", Some "hidden"
    ; "text", None
    ]
    starts
;;

let test_openai_parse_edge_shapes () =
  let mixed_tool_calls =
    {|{"id":"c","model":"m","choices":[{"delta":{"tool_calls":[{"index":"bad"},{"index":0,"function":{"arguments":"{}"}}]},"finish_reason":null}],"usage":{"prompt_tokens":4,"completion_tokens":5,"prompt_tokens_details":{"cached_tokens":3}}}|}
  in
  let chunk =
    require_some "openai mixed tool calls" (S.parse_openai_sse_chunk mixed_tool_calls)
  in
  check int "only valid tool call retained" 1 (List.length chunk.delta_tool_calls);
  (match chunk.chunk_usage with
   | Some u ->
     check int "cached tokens" 3 u.cache_read_input_tokens;
     check int "input tokens" 4 u.input_tokens
   | None -> fail "expected usage");
  let non_list_tool_calls =
    {|{"id":"c","model":"m","choices":[{"delta":{"tool_calls":{"unexpected":true}},"finish_reason":null}]}|}
  in
  let chunk =
    require_some
      "openai non-list tool calls"
      (S.parse_openai_sse_chunk non_list_tool_calls)
  in
  check int "non-list tool calls ignored" 0 (List.length chunk.delta_tool_calls)
;;

let test_openai_object_arguments () =
  (* llama.cpp / llama-server (#20198) streams tool-call [arguments] as a
     JSON object rather than a serialized string. [to_string_option] returns
     None for an object, which silently dropped the args (ToolUse with empty
     input). The parser must serialize the object to a string instead. *)
  let object_args =
    {|{"id":"c","model":"m","choices":[{"delta":{"tool_calls":[{"index":0,"function":{"name":"f","arguments":{"x":1}}}]},"finish_reason":null}]}|}
  in
  let chunk = require_some "openai object args" (S.parse_openai_sse_chunk object_args) in
  match chunk.delta_tool_calls with
  | [ tc ] ->
    check
      (option string)
      "object arguments serialized (not dropped to None)"
      (Some {|{"x":1}|})
      tc.tc_arguments
  | _ -> fail "expected exactly one tool call"
;;

let test_openai_event_edge_branches () =
  let state = S.create_openai_stream_state ~provider:"p" ~model:"m" () in
  ignore (S.openai_chunk_to_events state (openai_chunk ~delta_reasoning:"thinking" ()));
  let empty_reasoning_events, telemetry =
    S.openai_chunk_to_events state (openai_chunk ~delta_reasoning:"" ())
  in
  check_event_count "empty reasoning emits no content event" 0 empty_reasoning_events;
  (match telemetry with
   | Some (Llm_provider.Telemetry_event.Thinking_complete r) ->
     check string "provider" "p" r.provider;
     check string "model" "m" r.model
   | _ -> fail "expected thinking completion telemetry");
  state.thinking_state <- S.Thinking_done;
  let repeat_reasoning_events, _ =
    S.openai_chunk_to_events state (openai_chunk ~delta_reasoning:"again" ())
  in
  check_event_count
    "repeat reasoning after done emits delta only"
    1
    repeat_reasoning_events;
  let tool_state = S.create_openai_stream_state () in
  let tc_empty =
    { S.tc_index = 0
    ; tc_id = Some "call-1"
    ; tc_name = Some "search"
    ; tc_arguments = Some ""
    }
  in
  let tc_none = { S.tc_index = 0; tc_id = None; tc_name = None; tc_arguments = None } in
  let first_tool_events, _ =
    S.openai_chunk_to_events tool_state (openai_chunk ~delta_tool_calls:[ tc_empty ] ())
  in
  check_event_count "empty-args tool starts block only" 1 first_tool_events;
  let reused_tool_events, _ =
    S.openai_chunk_to_events tool_state (openai_chunk ~delta_tool_calls:[ tc_none ] ())
  in
  check_event_count "same tool index without args emits nothing" 0 reused_tool_events;
  let refusal_finish_events, _ =
    S.openai_chunk_to_events
      tool_state
      (openai_chunk ~finish_reason:"refusal" ~chunk_usage:(usage ()) ())
  in
  match refusal_finish_events with
  | [ MessageDelta { stop_reason = Some Refusal; usage = Some _ } ] -> ()
  | _ -> fail "expected refusal finish reason to map to Refusal"
;;

let test_gemini_parse_edge_shapes () =
  (match
     S.parse_gemini_sse_chunk
       {|{"modelVersion":"gem","candidates":[],"usageMetadata":null}|}
   with
   | None -> ()
   | Some _ -> fail "empty candidates should be rejected by missing content");
  (match
     S.parse_gemini_sse_chunk
       {|{"modelVersion":"gem","candidates":{"unexpected":true},"usageMetadata":null}|}
   with
   | None -> ()
   | Some _ -> fail "non-list candidates should be rejected by missing content");
  let non_list_parts =
    require_some
      "non-list parts"
      (S.parse_gemini_sse_chunk
         {|{"modelVersion":"gem","candidates":[{"content":{"parts":{"bad":true}}}],"usageMetadata":null}|})
  in
  check int "non-list parts ignored" 0 (List.length non_list_parts.gem_parts);
  match S.parse_gemini_sse_chunk "{not-json" with
  | None -> ()
  | Some _ -> fail "invalid gemini json should return None"
;;

let gemini_chunk ?(parts = []) ?finish_reason ?usage () : S.gemini_chunk =
  { gem_model = "gemini-test"
  ; gem_parts = parts
  ; gem_finish_reason = finish_reason
  ; gem_usage = usage
  }
;;

let test_gemini_event_edge_branches () =
  let state = S.create_openai_stream_state ~provider:"gemini" ~model:"gem" () in
  let thought_part = `Assoc [ "thought", `Bool true; "text", `String "plan" ] in
  ignore (S.gemini_chunk_to_events state (gemini_chunk ~parts:[ thought_part ] ()));
  let no_thought_events, telemetry =
    S.gemini_chunk_to_events state (gemini_chunk ~parts:[ `Assoc [] ] ())
  in
  check_event_count "no-thought chunk emits no events" 0 no_thought_events;
  (match telemetry with
   | Some (Llm_provider.Telemetry_event.Thinking_complete r) ->
     check string "provider" "gemini" r.provider
   | _ -> fail "expected gemini thinking completion telemetry");
  state.thinking_state <- S.Thinking_done;
  let restarted_events, _ =
    S.gemini_chunk_to_events state (gemini_chunk ~parts:[ thought_part ] ())
  in
  check_event_count "thinking after done restarts and emits delta" 1 restarted_events;
  let empty_text_with_call =
    `Assoc
      [ "text", `String ""
      ; ( "functionCall"
        , `Assoc [ "name", `String "lookup"; "args", `Assoc [ "q", `String "seoul" ] ] )
      ]
  in
  let tool_events, _ =
    S.gemini_chunk_to_events
      (S.create_openai_stream_state ())
      (gemini_chunk ~parts:[ empty_text_with_call ] ())
  in
  check_event_count "empty text can still carry function call" 2 tool_events;
  let empty_text_without_call =
    `Assoc [ "text", `String ""; "functionCall", `String "not-an-object" ]
  in
  let ignored_events, _ =
    S.gemini_chunk_to_events
      (S.create_openai_stream_state ())
      (gemini_chunk ~parts:[ empty_text_without_call ] ())
  in
  check_event_count "non-object functionCall ignored" 0 ignored_events;
  let max_tokens_events, _ =
    S.gemini_chunk_to_events
      (S.create_openai_stream_state ())
      (gemini_chunk ~finish_reason:"MAX_TOKENS" ~usage:(usage ()) ())
  in
  (match max_tokens_events with
   | [ MessageDelta { stop_reason = Some MaxTokens; usage = Some _ } ] -> ()
   | _ -> fail "expected gemini max-tokens finish");
  let unknown_events, _ =
    S.gemini_chunk_to_events
      (S.create_openai_stream_state ())
      (gemini_chunk ~finish_reason:"SAFETY" ())
  in
  match unknown_events with
  | [ MessageDelta { stop_reason = Some Refusal; _ } ] -> ()
  | _ -> fail "expected gemini unknown finish"
;;

let test_ollama_parse_edge_shapes () =
  let non_object_message =
    require_some
      "non-object message"
      (S.parse_ollama_ndjson_chunk {|{"model":"m","message":"text","done":false}|})
  in
  check (option string) "no content" None non_object_message.oll_delta_content;
  check int "no tool calls" 0 (List.length non_object_message.oll_tool_calls);
  let args_variants =
    require_some
      "tool argument variants"
      (S.parse_ollama_ndjson_chunk
         {|{"model":"m","message":{"content":"","thinking":"","tool_calls":[{"id":"a","function":{"name":"null_args","arguments":null}},{"function":{"name":"string_args","arguments":"{\"x\":1}"}},{"function":{"name":"bool_args","arguments":true}}]},"done":true,"prompt_eval_duration":1000000,"eval_duration":0}|})
  in
  check (option string) "empty content becomes None" None args_variants.oll_delta_content;
  check
    (option string)
    "empty thinking becomes None"
    None
    args_variants.oll_delta_thinking;
  check int "three tool calls" 3 (List.length args_variants.oll_tool_calls);
  (match args_variants.oll_tool_calls with
   | [ first; second; third ] ->
     check (option string) "null args" None first.oll_tc_arguments;
     check (option string) "string args" (Some {|{"x":1}|}) second.oll_tc_arguments;
     check (option string) "bool args" (Some "true") third.oll_tc_arguments
   | _ -> fail "unexpected tool calls");
  (match args_variants.oll_timings with
   | Some t ->
     check (option (float 0.001)) "prompt ms" (Some 1.0) t.prompt_ms;
     check (option (float 0.001)) "zero eval rate" None t.predicted_per_second
   | None -> fail "expected timings");
  match S.parse_ollama_ndjson_chunk {|{"model":1,"done":false}|} with
  | None -> ()
  | Some _ -> fail "type error should return None"
;;

let test_ollama_event_edge_branches () =
  let state = S.create_openai_stream_state ~provider:"ollama" ~model:"m" () in
  ignore (S.ollama_chunk_to_events state (ollama_chunk ~delta_thinking:"first" ()));
  let repeated_thinking_events, _ =
    S.ollama_chunk_to_events state (ollama_chunk ~delta_thinking:"second" ())
  in
  check_event_count "repeated thinking emits delta only" 1 repeated_thinking_events;
  let empty_thinking_events, empty_tel =
    S.ollama_chunk_to_events state (ollama_chunk ~delta_thinking:"" ())
  in
  check_event_count "empty thinking emits no event" 0 empty_thinking_events;
  (match empty_tel with
   | Some (Llm_provider.Telemetry_event.Thinking_complete r) ->
     check string "provider" "ollama" r.provider
   | _ -> fail "expected empty-thinking telemetry");
  state.thinking_state <- S.Thinking_started 0.0;
  let none_thinking_events, none_tel = S.ollama_chunk_to_events state (ollama_chunk ()) in
  check_event_count "missing thinking closes telemetry only" 0 none_thinking_events;
  (match none_tel with
   | Some (Llm_provider.Telemetry_event.Thinking_complete _) -> ()
   | _ -> fail "expected none-thinking telemetry");
  let text_empty_events, _ =
    S.ollama_chunk_to_events
      (S.create_openai_stream_state ())
      (ollama_chunk ~delta_content:"" ())
  in
  check_event_count "empty text ignored" 0 text_empty_events;
  let tool_state = S.create_openai_stream_state () in
  let tc_empty =
    { S.oll_tc_index = 0
    ; oll_tc_id = Some "call"
    ; oll_tc_name = Some "lookup"
    ; oll_tc_arguments = Some ""
    }
  in
  let tc_none =
    { S.oll_tc_index = 0; oll_tc_id = None; oll_tc_name = None; oll_tc_arguments = None }
  in
  let tool_start_events, _ =
    S.ollama_chunk_to_events tool_state (ollama_chunk ~tool_calls:[ tc_empty ] ())
  in
  check_event_count "empty-args ollama tool starts only" 1 tool_start_events;
  let tool_reuse_events, _ =
    S.ollama_chunk_to_events tool_state (ollama_chunk ~tool_calls:[ tc_none ] ())
  in
  check_event_count "reused ollama tool without args emits nothing" 0 tool_reuse_events;
  let done_none_events, _ =
    S.ollama_chunk_to_events
      (S.create_openai_stream_state ())
      (ollama_chunk ~is_done:true ())
  in
  (match done_none_events with
   | [ MessageDelta { stop_reason = Some EndTurn; _ } ] -> ()
   | _ -> fail "done without reason should be EndTurn");
  let done_length_events, _ =
    S.ollama_chunk_to_events
      (S.create_openai_stream_state ())
      (ollama_chunk ~is_done:true ~done_reason:"length" ())
  in
  (match done_length_events with
   | [ MessageDelta { stop_reason = Some MaxTokens; _ } ] -> ()
   | _ -> fail "done length should be MaxTokens");
  let done_unknown_tool_events, _ =
    S.ollama_chunk_to_events
      (S.create_openai_stream_state ())
      (ollama_chunk ~is_done:true ~done_reason:"future" ~tool_calls:[ tc_none ] ())
  in
  (match done_unknown_tool_events with
   | [ ContentBlockStart _; MessageDelta { stop_reason = Some StopToolUse; _ } ] -> ()
   | _ -> fail "unknown done reason with tools should stop for tool use");
  let done_unknown_events, _ =
    S.ollama_chunk_to_events
      (S.create_openai_stream_state ())
      (ollama_chunk ~is_done:true ~done_reason:"content_filter" ())
  in
  match done_unknown_events with
  | [ MessageDelta { stop_reason = Some (Unknown "content_filter"); _ } ] -> ()
  | _ -> fail "unknown done reason should be preserved"
;;

let () =
  run
    "streaming_edge_cases"
    [ ( "anthropic_sse"
      , [ test_case "parser edge cases" `Quick test_anthropic_sse_parser_edges
        ; test_case
            "first-token classifier edges"
            `Quick
            test_first_token_classifier_edges
        ; test_case
            "deliverable progress classifier edges"
            `Quick
            test_deliverable_progress_classifier_edges
        ; test_case
            "thinking-only timeout predicate"
            `Quick
            test_thinking_only_timeout_exceeded
        ; test_case "synthetic media events" `Quick test_synthetic_events_media_blocks
        ] )
    ; ( "openai_sse"
      , [ test_case "parse edge shapes" `Quick test_openai_parse_edge_shapes
        ; test_case "object-form tool arguments" `Quick test_openai_object_arguments
        ; test_case "event edge branches" `Quick test_openai_event_edge_branches
        ] )
    ; ( "gemini_sse"
      , [ test_case "parse edge shapes" `Quick test_gemini_parse_edge_shapes
        ; test_case "event edge branches" `Quick test_gemini_event_edge_branches
        ] )
    ; ( "ollama_ndjson"
      , [ test_case "parse edge shapes" `Quick test_ollama_parse_edge_shapes
        ; test_case "event edge branches" `Quick test_ollama_event_edge_branches
        ] )
    ]
;;
