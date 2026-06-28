(** Stream accumulator: gather SSE events into a {!Types.api_response}.

    Extracts the stream_acc type and its operations from [Complete].
    Depends only on [Types] -- no provider/backend/transport references.

    @since 0.79.0 *)

(** Internal: accumulate SSE events into content blocks. *)
type stream_acc =
  { id : string ref
  ; model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : Types.stop_reason ref
  ; stop_reason_received : bool ref
  ; terminal_incomplete : bool ref
  ; sse_error : Types.stream_error option ref
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  ; block_thinking_signatures : (int, Buffer.t) Hashtbl.t
  }

let create_stream_acc () =
  { id = ref ""
  ; model = ref ""
  ; input_tokens = ref 0
  ; output_tokens = ref 0
  ; cache_creation = ref 0
  ; cache_read = ref 0
  ; stop_reason = ref Types.EndTurn
  ; stop_reason_received = ref false
  ; terminal_incomplete = ref false
  ; sse_error = ref None
  ; block_texts = Hashtbl.create 4
  ; block_types = Hashtbl.create 4
  ; block_tool_ids = Hashtbl.create 4
  ; block_tool_names = Hashtbl.create 4
  ; block_thinking_signatures = Hashtbl.create 4
  }
;;

let accumulate_event (acc : stream_acc) = function
  | Types.MessageStart { id; model; usage } ->
    acc.id := id;
    acc.model := model;
    (match usage with
     | Some u ->
       acc.input_tokens := u.input_tokens;
       acc.cache_creation := u.cache_creation_input_tokens;
       acc.cache_read := u.cache_read_input_tokens
     | None -> ())
  | Types.ContentBlockStart { index; content_type; tool_id; tool_name } ->
    Hashtbl.replace acc.block_types index content_type;
    Hashtbl.replace acc.block_texts index (Buffer.create 64);
    (match tool_id with
     | Some id -> Hashtbl.replace acc.block_tool_ids index id
     | None -> ());
    (match tool_name with
     | Some n -> Hashtbl.replace acc.block_tool_names index n
     | None -> ())
  | Types.ContentBlockDelta { index; delta } ->
    let buf =
      match Hashtbl.find_opt acc.block_texts index with
      | Some b -> b
      | None ->
        let b = Buffer.create 64 in
        Hashtbl.replace acc.block_texts index b;
        b
    in
    (match delta with
     | Types.TextDelta s | Types.ThinkingDelta s | Types.InputJsonDelta s ->
       Buffer.add_string buf s
     | Types.ThinkingSignatureDelta s ->
       let sig_buf =
         match Hashtbl.find_opt acc.block_thinking_signatures index with
         | Some b -> b
         | None ->
           let b = Buffer.create 256 in
           Hashtbl.replace acc.block_thinking_signatures index b;
           b
       in
       Buffer.add_string sig_buf s)
  | Types.ContentBlockStop _ -> ()
  | Types.MessageDelta { stop_reason; usage } ->
    (match stop_reason with
     | Some sr ->
       acc.stop_reason := sr;
       acc.stop_reason_received := true
     | None -> ());
    (match usage with
     | Some u ->
       (* Additive so token totals are not lost when they arrive in a
          MessageDelta rather than MessageStart. Anthropic carries input_tokens
          in MessageStart and reports input_tokens/cache fields as 0 in its
          message_delta, so [+= 0] preserves that value. OpenAI-compatible
          streaming and Responses streaming deliver final usage only in a
          terminal MessageDelta, so this is the only place those tokens and
          cached-token fields are captured. *)
       acc.input_tokens := !(acc.input_tokens) + u.input_tokens;
       acc.output_tokens := !(acc.output_tokens) + u.output_tokens;
       acc.cache_creation := !(acc.cache_creation) + u.cache_creation_input_tokens;
       acc.cache_read := !(acc.cache_read) + u.cache_read_input_tokens
     | None -> ())
  | Types.SSEError { message; error_type; raw } ->
    acc.sse_error := Some (Types.Stream_provider_error { message; error_type; raw })
  | Types.SSEParseFailed { raw; reason } ->
    acc.sse_error := Some (Types.Stream_parse_failed { reason; raw })
  | Types.SSEUnknownEventType { event_type; raw } ->
    acc.sse_error := Some (Types.Stream_unknown_event { event_type; raw })
  | Types.StreamIncomplete _ -> acc.terminal_incomplete := true
  | Types.MessageStop | Types.Ping | Types.Connected | Types.Timeout _ -> ()
;;

let finalize_stream_acc
      ?(reasoning_visibility = Reasoning_dialect.Provider_hidden)
      (acc : stream_acc)
  =
  match !(acc.sse_error) with
  | Some serr -> Error serr
  | None when not !(acc.stop_reason_received) ->
    (* Stream ended without a terminal MessageDelta carrying a stop_reason.
       This happens when the connection drops mid-stream (End_of_file in
       sse_parser) or the provider sends no stop_reason.  Without this
       check the default EndTurn would make a truncated stream look like
       a successful completion (phantom completion). *)
    Error
      (Types.Stream_parse_failed
         { reason = "stream_terminated_without_stop_reason"; raw = "" })
  | None ->
    let indices =
      Hashtbl.fold (fun k _ acc -> k :: acc) acc.block_types [] |> List.sort compare
    in
    let content =
      List.filter_map
        (fun idx ->
           let text =
             match Hashtbl.find_opt acc.block_texts idx with
             | Some buf -> Buffer.contents buf
             | None -> ""
           in
           match Hashtbl.find_opt acc.block_types idx with
           | Some "text" -> Some (Types.Text text)
           | Some "thinking" ->
             let thinking_type =
               match Hashtbl.find_opt acc.block_thinking_signatures idx with
               | Some buf when Buffer.length buf > 0 -> Buffer.contents buf
               | Some _ | None -> "thinking"
             in
             Some (Types.Thinking { thinking_type; content = text })
           | Some "redacted_thinking" ->
             (match Hashtbl.find_opt acc.block_tool_ids idx with
              | Some data when data <> "" -> Some (Types.RedactedThinking data)
              | Some _ | None -> None)
           | Some "tool_use"
             when !(acc.terminal_incomplete) || !(acc.stop_reason) = Types.MaxTokens ->
             (* A tool call only belongs to a turn the model finished at a tool
                boundary. When the turn was cut off, the accumulated tool block is
                partial: its argument buffer may be empty (cut off before the first
                delta) or even parse as JSON yet be incomplete. Drop it so the
                pipeline does not store a dangling assistant ToolUse that a later
                turn repairs with a synthetic error ToolResult. Mirrors the
                non-streaming parser Backend_openai_responses.parse_response_result,
                which drops ToolUse for incomplete/failed Responses statuses.
                Two cut-off signals:
                - [terminal_incomplete]: an OpenAI Responses [response.incomplete]
                  for ANY reason (max_output_tokens, content_filter, ...), carried
                  via the [StreamIncomplete] event.
                - [stop_reason = MaxTokens]: a token-limit truncation from any
                  provider (e.g. Anthropic/OpenAI streaming) that does not emit a
                  Responses incomplete terminal.
                (#2073 streaming follow-up.) [response.failed]/[error] terminals
                set [sse_error] instead, so finalize returns [Error] before content
                assembly and never reaches this branch. *)
             None
           | Some "tool_use" ->
             let id =
               match Hashtbl.find_opt acc.block_tool_ids idx with
               | Some s -> s
               | None -> ""
             in
             let name =
               match Hashtbl.find_opt acc.block_tool_names idx with
               | Some s -> s
               | None -> ""
             in
             let input =
               try Yojson.Safe.from_string text with
               | Yojson.Json_error _ -> `Assoc []
             in
             Some (Types.ToolUse { id; name; input })
           | Some "tool_result" | Some "tool_result_error" ->
             let tool_use_id =
               match Hashtbl.find_opt acc.block_tool_ids idx with
               | Some s -> s
               | None -> ""
             in
             let is_error =
               match Hashtbl.find_opt acc.block_types idx with
               | Some "tool_result_error" -> true
               | _ -> false
             in
             Some
               (Types.ToolResult
                  { tool_use_id
                  ; content = text
                  ; is_error
                  ; json = (if is_error then None else Types.try_parse_json text)
                  ; content_blocks = None
                  })
           | _ -> None)
        indices
    in
    (* Visible_text policy: a reasoning-only stream (no Text block, no tool
       calls) collapses to content=[Thinking] which every Text-only projection
       reads as empty. Promote the reasoning into a visible Text block for
       provider/model contracts that expose reasoning-only answer text. Mirrors
       the non-streaming parser promotion. *)
    let has_text =
      List.exists
        (function
          | Types.Text _ -> true
          | Types.Thinking _
          | Types.RedactedThinking _
          | Types.ToolUse _
          | Types.ToolResult _
          | Types.Image _
          | Types.Document _
          | Types.Audio _ -> false)
        content
    in
    let has_tool =
      List.exists
        (function
          | Types.ToolUse _ -> true
          | Types.Text _
          | Types.Thinking _
          | Types.RedactedThinking _
          | Types.ToolResult _
          | Types.Image _
          | Types.Document _
          | Types.Audio _ -> false)
        content
    in
    let reasoning_text =
      List.find_map
        (function
          | Types.Thinking { content = c; _ } -> Some c
          | Types.Text _
          | Types.RedactedThinking _
          | Types.ToolUse _
          | Types.ToolResult _
          | Types.Image _
          | Types.Document _
          | Types.Audio _ -> None)
        content
    in
    let promoted_reasoning =
      match reasoning_visibility, has_text, has_tool, reasoning_text with
      | Reasoning_dialect.Visible_text, false, false, Some r when String.trim r <> "" ->
        [ Types.Text r ]
      | _ -> []
    in
    (* Enforce the StopToolUse => has-tool-block invariant now that the full
       block set (including dropped partial tool calls above) is known. A
       reasoning-only or dropped-partial-tool stream that the provider tagged
       finish_reason="tool_calls" must NOT reach the driver as a tool-use turn
       with zero tools -- the pipeline re-issues that forever (infinite Thinking
       loop). SSOT: Stop_reason_wire.reconcile (same rule as the non-streaming
       parser via Stop_reason_wire.of_finish). *)
    let stop_reason =
      Stop_reason_wire.reconcile !(acc.stop_reason) ~has_tool_blocks:has_tool
    in
    Ok
      { Types.id = !(acc.id)
      ; model = !(acc.model)
      ; stop_reason
      ; content = content @ promoted_reasoning
      ; usage =
          Some
            { input_tokens = !(acc.input_tokens)
            ; output_tokens = !(acc.output_tokens)
            ; cache_creation_input_tokens = !(acc.cache_creation)
            ; cache_read_input_tokens = !(acc.cache_read)
            ; cost_usd = None
            }
      ; telemetry = None
      }
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "create_stream_acc has sensible defaults" =
  let acc = create_stream_acc () in
  !(acc.id) = ""
  && !(acc.model) = ""
  && !(acc.input_tokens) = 0
  && !(acc.output_tokens) = 0
  && !(acc.stop_reason) = Types.EndTurn
  && !(acc.sse_error) = None
;;

let%test "accumulate_event MessageStart sets id and model" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageStart { id = "msg-1"; model = "gpt-4"; usage = None });
  !(acc.id) = "msg-1" && !(acc.model) = "gpt-4"
;;

let%test "accumulate_event MessageStart with usage" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageStart
       { id = "msg-2"
       ; model = "m"
       ; usage =
           Some
             { input_tokens = 100
             ; output_tokens = 0
             ; cache_creation_input_tokens = 5
             ; cache_read_input_tokens = 10
             ; cost_usd = None
             }
       });
  !(acc.input_tokens) = 100 && !(acc.cache_creation) = 5 && !(acc.cache_read) = 10
;;

let%test "accumulate_event ContentBlockStart + Delta + Stop" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "text"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "Hello " });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "world" });
  accumulate_event acc (Types.ContentBlockStop { index = 0 });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "Hello world"
;;

let%test "accumulate_event MessageDelta sets stop_reason" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = Some Types.StopToolUse
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 50
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  !(acc.stop_reason) = Types.StopToolUse && !(acc.output_tokens) = 50
;;

let%test "finalize_stream_acc assembles text block" =
  let acc = create_stream_acc () in
  acc.id := "test-id";
  acc.model := "test-model";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "Hello world";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    result.id = "test-id"
    && result.model = "test-model"
    && result.content = [ Types.Text "Hello world" ]
;;

let%test "finalize_stream_acc assembles tool_use block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "my_tool";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"key\":\"val\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { id = "tool-id-1"; name = "my_tool"; input } ] ->
       input = `Assoc [ "key", `String "val" ]
     | _ -> false)
;;

let%test "finalize_stream_acc assembles thinking block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "reasoning...";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.Thinking { thinking_type = "thinking"; content = "reasoning..." } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc multiple blocks ordered by index" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "thinking";
  Hashtbl.replace acc.block_types 1 "text";
  let buf0 = Buffer.create 16 in
  Buffer.add_string buf0 "think";
  let buf1 = Buffer.create 16 in
  Buffer.add_string buf1 "say";
  Hashtbl.replace acc.block_texts 0 buf0;
  Hashtbl.replace acc.block_texts 1 buf1;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result -> List.length result.content = 2
;;

let%test "finalize_stream_acc includes usage" =
  let acc = create_stream_acc () in
  acc.input_tokens := 100;
  acc.output_tokens := 50;
  acc.cache_creation := 10;
  acc.cache_read := 20;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.usage with
     | Some u ->
       u.input_tokens = 100
       && u.output_tokens = 50
       && u.cache_creation_input_tokens = 10
       && u.cache_read_input_tokens = 20
     | None -> false)
;;

(* --- accumulate_event edge cases --- *)

let%test "accumulate_event ContentBlockDelta on unknown index creates buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 99; delta = Types.TextDelta "orphan" });
  match Hashtbl.find_opt acc.block_texts 99 with
  | Some buf -> Buffer.contents buf = "orphan"
  | None -> false
;;

let%test "accumulate_event ContentBlockStart with tool_id and tool_name" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0
       ; content_type = "tool_use"
       ; tool_id = Some "tid-1"
       ; tool_name = Some "my_fn"
       });
  Hashtbl.find acc.block_tool_ids 0 = "tid-1"
  && Hashtbl.find acc.block_tool_names 0 = "my_fn"
;;

let%test "accumulate_event ThinkingDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "thinking"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.ThinkingDelta "step1" });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.ThinkingDelta " step2" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "step1 step2"
;;

let%test "accumulate_event ThinkingSignatureDelta preserves opaque signature" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "thinking"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta
       { index = 0; delta = Types.ThinkingSignatureDelta "sig_opaque" });
  match Hashtbl.find_opt acc.block_thinking_signatures 0 with
  | Some buf -> Buffer.contents buf = "sig_opaque"
  | None -> false
;;

let%test "finalize_stream_acc preserves omitted thinking signature" =
  let acc = create_stream_acc () in
  List.iter
    (accumulate_event acc)
    [ Types.MessageStart { id = "m"; model = "m"; usage = None }
    ; Types.ContentBlockStart
        { index = 0; content_type = "thinking"; tool_id = None; tool_name = None }
    ; Types.ContentBlockDelta
        { index = 0; delta = Types.ThinkingSignatureDelta "sig_opaque" }
    ; Types.MessageDelta { stop_reason = Some Types.EndTurn; usage = None }
    ];
  match finalize_stream_acc acc with
  | Ok { content = [ Types.Thinking { thinking_type; content } ]; _ } ->
    thinking_type = "sig_opaque" && content = ""
  | Ok _ | Error _ -> false
;;

let%test "finalize_stream_acc preserves redacted thinking carrier" =
  let acc = create_stream_acc () in
  List.iter
    (accumulate_event acc)
    [ Types.MessageStart { id = "m"; model = "m"; usage = None }
    ; Types.ContentBlockStart
        { index = 0
        ; content_type = "redacted_thinking"
        ; tool_id = Some "opaque_data"
        ; tool_name = None
        }
    ; Types.MessageDelta { stop_reason = Some Types.StopToolUse; usage = None }
    ];
  match finalize_stream_acc acc with
  | Ok { content = [ Types.RedactedThinking data ]; _ } -> data = "opaque_data"
  | Ok _ | Error _ -> false
;;

let%test "accumulate_event InputJsonDelta appends to buffer" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.ContentBlockStart
       { index = 0; content_type = "tool_use"; tool_id = None; tool_name = None });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.InputJsonDelta "{\"k\":" });
  accumulate_event
    acc
    (Types.ContentBlockDelta { index = 0; delta = Types.InputJsonDelta "\"v\"}" });
  let buf = Hashtbl.find acc.block_texts 0 in
  Buffer.contents buf = "{\"k\":\"v\"}"
;;

let%test "accumulate_event MessageDelta None stop_reason keeps default" =
  let acc = create_stream_acc () in
  accumulate_event acc (Types.MessageDelta { stop_reason = None; usage = None });
  !(acc.stop_reason) = Types.EndTurn
;;

let%test "accumulate_event MessageDelta None usage does not change tokens" =
  let acc = create_stream_acc () in
  acc.output_tokens := 10;
  accumulate_event acc (Types.MessageDelta { stop_reason = None; usage = None });
  !(acc.output_tokens) = 10
;;

let%test "accumulate_event MessageStop is no-op" =
  let acc = create_stream_acc () in
  acc.id := "keep";
  accumulate_event acc Types.MessageStop;
  !(acc.id) = "keep"
;;

let%test "accumulate_event Ping is no-op" =
  let acc = create_stream_acc () in
  accumulate_event acc Types.Ping;
  !(acc.id) = ""
;;

let%test "accumulate_event SSEError records typed provider error" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.SSEError
       { message = "bad"; error_type = Some "rate_limit_exceeded"; raw = "{}" });
  match !(acc.sse_error) with
  | Some (Types.Stream_provider_error { message; error_type; _ }) ->
    message = "bad" && error_type = Some "rate_limit_exceeded"
  | Some (Types.Stream_parse_failed _ | Types.Stream_unknown_event _) | None -> false
;;

(* SSEParseFailed and SSEUnknownEventType replace the previous silent [None]
   discard in [parse_sse_event]. They MUST mark [sse_error] so that
   [finalize_stream_acc] yields [Error _] and the caller can route to
   another provider instead of presenting a phantom completion (a partial
   response with no MessageStop, treated as success by downstream consumers). *)

let%test "accumulate_event SSEParseFailed marks typed parse failure with reason" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.SSEParseFailed { raw = "{not json"; reason = "json_error: Line 1, bytes 0-9" });
  match !(acc.sse_error) with
  | Some (Types.Stream_parse_failed { reason; raw }) ->
    reason = "json_error: Line 1, bytes 0-9" && raw = "{not json"
  | Some (Types.Stream_provider_error _ | Types.Stream_unknown_event _) | None -> false
;;

let%test "accumulate_event SSEUnknownEventType marks typed unknown event with type" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.SSEUnknownEventType
       { event_type = "future_event_v3"; raw = "{\"type\":\"future_event_v3\"}" });
  match !(acc.sse_error) with
  | Some (Types.Stream_unknown_event { event_type; _ }) -> event_type = "future_event_v3"
  | Some (Types.Stream_provider_error _ | Types.Stream_parse_failed _) | None -> false
;;

let%test
    "accumulate_event SSEParseFailed carries raw chunk verbatim (no lossy truncation)"
  =
  let acc = create_stream_acc () in
  let big = String.make 5000 'x' in
  accumulate_event acc (Types.SSEParseFailed { raw = big; reason = "test" });
  (* The typed carrier preserves [raw] whole; any truncation is a display-layer
     concern, not a data-loss point in the accumulator. *)
  match !(acc.sse_error) with
  | Some (Types.Stream_parse_failed { raw; _ }) -> String.length raw = 5000
  | Some (Types.Stream_provider_error _ | Types.Stream_unknown_event _) | None -> false
;;

(* --- finalize_stream_acc edge cases --- *)

let%test "finalize_stream_acc empty produces empty content" =
  let acc = create_stream_acc () in
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc unknown block type filtered out" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "unknown_type";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "data";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc tool_use with invalid json falls back to empty assoc" =
  (* Non-truncated turn: an unparseable buffer still falls back to empty input
     (existing behavior). Truncation is handled by the MaxTokens guard, below. *)
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "not valid json";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { input = `Assoc []; _ } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc drops tool_use on truncated turn (MaxTokens)" =
  (* A truncated turn (Responses response.incomplete -> MaxTokens) must not surface
     a partial tool call, even when the arguments happen to parse as JSON. #2073. *)
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "get_weather";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"city\":\"Paris\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  acc.stop_reason := Types.MaxTokens;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc drops tool_use on incomplete non-token reason" =
  (* A Responses [response.incomplete] for a non-max_output_tokens reason (e.g.
     content_filter) carries [terminal_incomplete] via StreamIncomplete; the tool
     block must drop even though stop_reason is not MaxTokens. #2073. *)
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "get_weather";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"city\":\"Paris\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  acc.stop_reason := Types.Unknown "content_filter";
  acc.terminal_incomplete := true;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result -> result.content = []
;;

let%test "finalize_stream_acc keeps tool_use on StopToolUse (no over-drop)" =
  (* A normal tool-call turn keeps its tool block; the MaxTokens guard must not
     drop tools from a turn that finished at a tool boundary. *)
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  Hashtbl.replace acc.block_tool_ids 0 "tool-id-1";
  Hashtbl.replace acc.block_tool_names 0 "get_weather";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"city\":\"Paris\"}";
  Hashtbl.replace acc.block_texts 0 buf;
  acc.stop_reason := Types.StopToolUse;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse
           { name = "get_weather"; input = `Assoc [ ("city", `String "Paris") ]; _ }
       ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc tool_use missing id/name defaults to empty" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_use";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{}";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolUse { id = ""; name = ""; _ } ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc assembles tool_result block" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "tool_result";
  Hashtbl.replace acc.block_tool_ids 0 "tu_1";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "{\"ok\":true}";
  Hashtbl.replace acc.block_texts 0 buf;
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.ToolResult
           { tool_use_id = "tu_1"
           ; content = "{\"ok\":true}"
           ; json = Some (`Assoc [ ("ok", `Bool true) ])
           ; _
           }
       ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc block with no text buffer produces empty text" =
  let acc = create_stream_acc () in
  Hashtbl.replace acc.block_types 0 "text";
  (* No buffer added for index 0 *)
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error _ -> false
  | Ok result ->
    (match result.content with
     | [ Types.Text "" ] -> true
     | _ -> false)
;;

let%test "finalize_stream_acc returns Error when sse_error is set" =
  let acc = create_stream_acc () in
  acc.id := "partial-id";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "partial content";
  Hashtbl.replace acc.block_texts 0 buf;
  acc.sse_error
  := Some
       (Types.Stream_provider_error
          { message = "server overloaded"
          ; error_type = Some "overloaded_error"
          ; raw = "{}"
          });
  match
    acc.stop_reason_received := true;
    finalize_stream_acc acc
  with
  | Error (Types.Stream_provider_error { message; _ }) -> message = "server overloaded"
  | Error (Types.Stream_parse_failed _ | Types.Stream_unknown_event _) | Ok _ -> false
;;

(* Phantom completion prevention: a stream that ends without a terminal
   MessageDelta carrying a stop_reason must not produce Ok with the
   default EndTurn — that would make a truncated stream look like a
   successful completion. *)
let%test "finalize_stream_acc returns Error when stream has no stop_reason" =
  let acc = create_stream_acc () in
  acc.id := "msg-partial";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "Hello";
  Hashtbl.replace acc.block_texts 0 buf;
  (* No accumulate_event with MessageDelta { stop_reason = Some _; _ } *)
  match finalize_stream_acc acc with
  | Error (Types.Stream_parse_failed { reason; _ }) ->
    String.length reason > 0
    && String.sub reason 0 (String.length "stream_terminated") = "stream_terminated"
  | Error _ | Ok _ -> false
;;

let%test "finalize_stream_acc returns Ok after proper stop_reason received" =
  let acc = create_stream_acc () in
  acc.id := "msg-ok";
  Hashtbl.replace acc.block_types 0 "text";
  let buf = Buffer.create 16 in
  Buffer.add_string buf "Hello";
  Hashtbl.replace acc.block_texts 0 buf;
  accumulate_event
    acc
    (Types.MessageDelta { stop_reason = Some Types.EndTurn; usage = None });
  match finalize_stream_acc acc with
  | Ok resp -> resp.stop_reason = Types.EndTurn
  | Error _ -> false
;;

let%test "accumulate_event multiple MessageDelta accumulates tokens" =
  let acc = create_stream_acc () in
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = None
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 30
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  accumulate_event
    acc
    (Types.MessageDelta
       { stop_reason = None
       ; usage =
           Some
             { input_tokens = 0
             ; output_tokens = 20
             ; cache_creation_input_tokens = 0
             ; cache_read_input_tokens = 0
             ; cost_usd = None
             }
       });
  !(acc.output_tokens) = 50
;;
