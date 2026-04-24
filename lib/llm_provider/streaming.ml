(** SSE event parsing and synthetic event emission.

    Pure functions operating on {!Llm_provider.Types}. No agent_sdk coupling.
    The streaming HTTP client (create_message_stream) remains in agent_sdk. *)

open Types

(** Parse a single SSE data JSON payload into an [sse_event]. *)
let parse_sse_event event_type data_str =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let evt_type = match event_type with
      | Some t -> t
      | None -> json |> member "type" |> to_string_option |> Option.value ~default:""
    in
    match evt_type with
    | "message_start" ->
        let msg = json |> member "message" in
        let id = msg |> member "id" |> to_string in
        let model = msg |> member "model" |> to_string in
        let usage =
          let u = msg |> member "usage" in
          if u = `Null then None
          else
            let input_tokens = u |> member "input_tokens" |> to_int in
            let cache_creation_input_tokens =
              u |> member "cache_creation_input_tokens" |> to_int_option |> Option.value ~default:0 in
            let cache_read_input_tokens =
              u |> member "cache_read_input_tokens" |> to_int_option |> Option.value ~default:0 in
            Some { input_tokens; output_tokens = 0;
                   cache_creation_input_tokens; cache_read_input_tokens;
                   cost_usd = None }
        in
        Some (MessageStart { id; model; usage })
    | "content_block_start" ->
        let index = json |> member "index" |> to_int in
        let cb = json |> member "content_block" in
        let content_type = cb |> member "type" |> to_string in
        let tool_id = cb |> member "id" |> to_string_option in
        let tool_name = cb |> member "name" |> to_string_option in
        Some (ContentBlockStart { index; content_type; tool_id; tool_name })
    | "content_block_delta" ->
        let index = json |> member "index" |> to_int in
        let delta_json = json |> member "delta" in
        let delta_type = delta_json |> member "type" |> to_string in
        let delta = match delta_type with
          | "text_delta" ->
              TextDelta (delta_json |> member "text" |> to_string)
          | "thinking_delta" ->
              ThinkingDelta (delta_json |> member "thinking" |> to_string)
          | "input_json_delta" ->
              InputJsonDelta (delta_json |> member "partial_json" |> to_string)
          | _ ->
              TextDelta ""
        in
        Some (ContentBlockDelta { index; delta })
    | "content_block_stop" ->
        let index = json |> member "index" |> to_int in
        Some (ContentBlockStop { index })
    | "message_delta" ->
        let delta = json |> member "delta" in
        let stop_reason =
          delta |> member "stop_reason" |> to_string_option
          |> Option.map stop_reason_of_string
        in
        let usage =
          let u = json |> member "usage" in
          if u = `Null then None
          else
            let output_tokens = u |> member "output_tokens" |> to_int in
            let cache_creation_input_tokens =
              u |> member "cache_creation_input_tokens" |> to_int_option
              |> Option.value ~default:0 in
            let cache_read_input_tokens =
              u |> member "cache_read_input_tokens" |> to_int_option
              |> Option.value ~default:0 in
            Some { input_tokens = 0; output_tokens;
                   cache_creation_input_tokens; cache_read_input_tokens;
                   cost_usd = None }
        in
        Some (MessageDelta { stop_reason; usage })
    | "message_stop" ->
        Some MessageStop
    | "ping" ->
        Some Ping
    | "error" ->
        let msg = json |> member "error" |> member "message" |> to_string in
        Some (SSEError msg)
    | _ -> None
  with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> None

(** Emit synthetic SSE events from a complete [api_response].
    Used as fallback for non-Anthropic providers that don't support SSE. *)
let emit_synthetic_events (response : api_response) on_event =
  on_event (MessageStart {
    id = response.id;
    model = response.model;
    usage = response.usage;
  });
  List.iteri (fun index block ->
    let content_type, tool_id, tool_name = match block with
      | Text _ -> "text", None, None
      | Thinking _ -> "thinking", None, None
      | ToolUse { id; name; _ } -> "tool_use", Some id, Some name
      | Image _ -> "text", None, None
      | Document _ -> "text", None, None
      | Audio _ -> "text", None, None
      | RedactedThinking _ -> "text", None, None
      | ToolResult _ -> "text", None, None
    in
    on_event (ContentBlockStart { index; content_type; tool_id; tool_name });
    (match block with
     | Text s ->
       on_event (ContentBlockDelta { index; delta = TextDelta s })
     | Thinking { content; _ } ->
       on_event (ContentBlockDelta { index; delta = ThinkingDelta content })
     | ToolUse { input; _ } ->
       on_event (ContentBlockDelta { index; delta = InputJsonDelta (Yojson.Safe.to_string input) })
     | _ -> ());
    on_event (ContentBlockStop { index });
  ) response.content;
  on_event (MessageDelta {
    stop_reason = Some response.stop_reason;
    usage = response.usage;
  });
  on_event MessageStop

(** {1 OpenAI-compatible SSE Streaming}

    OpenAI Chat Completions streaming uses SSE with flat deltas:
    - Text content arrives as [delta.content] strings
    - Tool calls arrive as [delta.tool_calls] with incremental arguments
    - [finish_reason] signals end of a choice
    - No explicit content_block_start/stop events (unlike Anthropic)

    We parse each SSE chunk into {!openai_chunk}, then convert to
    {!sse_event} list using a stateful adapter ({!openai_stream_state}). *)

type openai_tool_call_delta = {
  tc_index: int;
  tc_id: string option;
  tc_name: string option;
  tc_arguments: string option;
}

type openai_chunk = {
  chunk_id: string;
  chunk_model: string;
  delta_content: string option;
  delta_reasoning: string option;
  delta_tool_calls: openai_tool_call_delta list;
  finish_reason: string option;
  chunk_usage: api_usage option;
}

(** Parse a single OpenAI SSE data payload into an {!openai_chunk}.
    Returns [None] for the "[DONE]" sentinel or unparseable data. *)
let parse_openai_sse_chunk data_str : openai_chunk option =
  if data_str = "[DONE]" then None
  else
    let open Yojson.Safe.Util in
    try
      let json = Yojson.Safe.from_string data_str in
      let chunk_id =
        json |> member "id" |> to_string_option |> Option.value ~default:""
      in
      let chunk_model =
        json |> member "model" |> to_string_option |> Option.value ~default:""
      in
      let choice = json |> member "choices" |> index 0 in
      let delta = choice |> member "delta" in
      let delta_content = delta |> member "content" |> to_string_option in
      let delta_reasoning =
        match delta |> member "reasoning_content" |> to_string_option with
        | Some s when String.trim s <> "" -> Some s
        | Some _ | None -> delta |> member "reasoning" |> to_string_option
      in
      let delta_tool_calls =
        match delta |> member "tool_calls" with
        | `List calls ->
            List.filter_map (fun tc ->
              try
                let tc_index = tc |> member "index" |> to_int in
                let tc_id = tc |> member "id" |> to_string_option in
                let fn = tc |> member "function" in
                let tc_name = fn |> member "name" |> to_string_option in
                let tc_arguments = fn |> member "arguments" |> to_string_option in
                Some { tc_index; tc_id; tc_name; tc_arguments }
              with Yojson.Safe.Util.Type_error _ | Not_found -> None
            ) calls
        | _ -> []
      in
      let finish_reason =
        choice |> member "finish_reason" |> to_string_option
      in
      let chunk_usage =
        let u = json |> member "usage" in
        if u = `Null then None
        else
          let cached =
            let d = u |> member "prompt_tokens_details" in
            if d = `Null then 0
            else d |> member "cached_tokens" |> to_int_option
                 |> Option.value ~default:0
          in
          Some {
            input_tokens =
              u |> member "prompt_tokens" |> to_int_option
              |> Option.value ~default:0;
            output_tokens =
              u |> member "completion_tokens" |> to_int_option
              |> Option.value ~default:0;
            cache_creation_input_tokens = 0;
            cache_read_input_tokens = cached;
            cost_usd = None
          }
      in
      Some { chunk_id; chunk_model; delta_content; delta_reasoning;
             delta_tool_calls; finish_reason; chunk_usage }
    with
    | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _
    | Yojson.Json_error _ | Invalid_argument _ -> None

(** Mutable state for converting OpenAI flat deltas to block-based events. *)
type openai_stream_state = {
  mutable thinking_block_started: bool;
  mutable thinking_block_index: int;
  mutable text_block_started: bool;
  mutable text_block_index: int;
  tool_block_indices: (int, int) Hashtbl.t;  (** tool_call index -> block index *)
  mutable next_block_index: int;
}

let create_openai_stream_state () = {
  thinking_block_started = false;
  thinking_block_index = -1;
  text_block_started = false;
  text_block_index = -1;
  tool_block_indices = Hashtbl.create 4;
  next_block_index = 0;
}

(** Convert a parsed {!openai_chunk} into {!sse_event} list.
    Synthesizes [ContentBlockStart] events on first occurrence of
    text content or each new tool_call index. *)
let openai_chunk_to_events (state : openai_stream_state)
    (chunk : openai_chunk) : sse_event list =
  let events = ref [] in
  let emit evt = events := evt :: !events in
  (* Reasoning content delta — emitted before text *)
  (match chunk.delta_reasoning with
   | Some text when text <> "" ->
       if not state.thinking_block_started then begin
         state.thinking_block_index <- state.next_block_index;
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "thinking";
           tool_id = None; tool_name = None });
         state.thinking_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       emit (ContentBlockDelta {
         index = state.thinking_block_index; delta = ThinkingDelta text })
   | _ -> ());
  (* Text content delta *)
  (match chunk.delta_content with
   | Some text when text <> "" ->
       if not state.text_block_started then begin
         state.text_block_index <- state.next_block_index;
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "text";
           tool_id = None; tool_name = None });
         state.text_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       emit (ContentBlockDelta {
         index = state.text_block_index; delta = TextDelta text })
   | _ -> ());
  (* Tool call deltas *)
  List.iter (fun (tc : openai_tool_call_delta) ->
    let block_idx =
      match Hashtbl.find_opt state.tool_block_indices tc.tc_index with
      | Some idx -> idx
      | None ->
          let idx = state.next_block_index in
          Hashtbl.replace state.tool_block_indices tc.tc_index idx;
          emit (ContentBlockStart {
            index = idx; content_type = "tool_use";
            tool_id = tc.tc_id; tool_name = tc.tc_name;
          });
          state.next_block_index <- state.next_block_index + 1;
          idx
    in
    match tc.tc_arguments with
    | Some args when args <> "" ->
        emit (ContentBlockDelta { index = block_idx;
                                  delta = InputJsonDelta args })
    | _ -> ()
  ) chunk.delta_tool_calls;
  (* Finish reason -> MessageDelta *)
  (match chunk.finish_reason with
   | Some reason ->
       let stop_reason = match String.lowercase_ascii reason with
         | "stop" -> EndTurn
         | "tool_calls" -> StopToolUse
         | "length" -> MaxTokens
         | other -> Unknown other
       in
       emit (MessageDelta { stop_reason = Some stop_reason;
                            usage = chunk.chunk_usage })
   | None -> ());
  List.rev !events

(** {1 Gemini SSE Streaming}

    Gemini [streamGenerateContent?alt=sse] emits SSE data lines with
    JSON payloads: [{candidates: [{content: {parts: [...]}}]}].
    Each chunk may contain text, thought, or functionCall parts. *)

type gemini_chunk = {
  gem_model: string;
  gem_parts: Yojson.Safe.t list;
  gem_finish_reason: string option;
  gem_usage: api_usage option;
}

let parse_gemini_sse_chunk data_str : gemini_chunk option =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let gem_model =
      json |> member "modelVersion" |> to_string_option
      |> Option.value ~default:""
    in
    let candidate = match json |> member "candidates" with
      | `List (c :: _) -> c
      | _ -> `Assoc []
    in
    let gem_parts = match candidate |> member "content" |> member "parts" with
      | `List ps -> ps
      | _ -> []
    in
    let gem_finish_reason =
      candidate |> member "finishReason" |> to_string_option
    in
    let gem_usage =
      let um = json |> member "usageMetadata" in
      if um = `Null then None
      else
        Some {
          input_tokens =
            um |> member "promptTokenCount" |> to_int_option
            |> Option.value ~default:0;
          output_tokens =
            um |> member "candidatesTokenCount" |> to_int_option
            |> Option.value ~default:0;
          cache_creation_input_tokens = 0;
          cache_read_input_tokens =
            um |> member "cachedContentTokenCount" |> to_int_option
            |> Option.value ~default:0;
          cost_usd = None;
        }
    in
    Some { gem_model; gem_parts; gem_finish_reason; gem_usage }
  with
  | Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _
  | Yojson.Json_error _ | Invalid_argument _ -> None

let gemini_chunk_to_events (state : openai_stream_state)
    (chunk : gemini_chunk) : sse_event list =
  let open Yojson.Safe.Util in
  let events = ref [] in
  let emit evt = events := evt :: !events in
  List.iter (fun part ->
    let is_thought =
      part |> member "thought" |> to_bool_option
      |> Option.value ~default:false
    in
    match part |> member "text" |> to_string_option with
    | Some text when text <> "" ->
        if is_thought then begin
          if not state.thinking_block_started then begin
            state.thinking_block_index <- state.next_block_index;
            emit (ContentBlockStart {
              index = state.next_block_index; content_type = "thinking";
              tool_id = None; tool_name = None });
            state.thinking_block_started <- true;
            state.next_block_index <- state.next_block_index + 1
          end;
          emit (ContentBlockDelta {
            index = state.thinking_block_index; delta = ThinkingDelta text })
        end else begin
          if not state.text_block_started then begin
            state.text_block_index <- state.next_block_index;
            emit (ContentBlockStart {
              index = state.next_block_index; content_type = "text";
              tool_id = None; tool_name = None });
            state.text_block_started <- true;
            state.next_block_index <- state.next_block_index + 1
          end;
          emit (ContentBlockDelta {
            index = state.text_block_index; delta = TextDelta text })
        end
    | _ ->
        (match part |> member "functionCall" with
         | `Assoc _ as fc ->
             let name = fc |> member "name" |> to_string_option
                        |> Option.value ~default:"" in
             let args = fc |> member "args" in
             let id = Api_common.synthesize_tool_use_id ~name args in
             let idx = state.next_block_index in
             emit (ContentBlockStart {
               index = idx; content_type = "tool_use";
               tool_id = Some id; tool_name = Some name;
             });
             state.next_block_index <- state.next_block_index + 1;
             emit (ContentBlockDelta { index = idx;
                                       delta = InputJsonDelta (Yojson.Safe.to_string args) })
         | _ -> ())
  ) chunk.gem_parts;
  (* Finish reason *)
  (match chunk.gem_finish_reason with
   | Some reason ->
       let stop_reason = match String.uppercase_ascii reason with
         | "STOP" -> EndTurn
         | "MAX_TOKENS" -> MaxTokens
         | other -> Unknown other
       in
       emit (MessageDelta { stop_reason = Some stop_reason;
                            usage = chunk.gem_usage })
   | None -> ());
  List.rev !events

(** {1 Ollama NDJSON Streaming}

    Ollama [/api/chat] with [stream:true] emits one JSON object per
    line. Non-final lines carry a [message.content] delta; the final
    line has [done:true] together with [done_reason] and the four
    timing fields ([prompt_eval_count] / [prompt_eval_duration] /
    [eval_count] / [eval_duration]) that the OpenAI compat path on
    [/v1/chat/completions] strips out. *)

type ollama_tool_call_delta = {
  oll_tc_index: int;
  oll_tc_id: string option;
  oll_tc_name: string option;
  oll_tc_arguments: string option;
}

type ollama_chunk = {
  oll_model: string;
  oll_delta_content: string option;
  oll_delta_thinking: string option;
  oll_tool_calls: ollama_tool_call_delta list;
  oll_done_reason: string option;
  oll_is_done: bool;
  oll_usage: api_usage option;
  oll_timings: inference_timings option;
}

let parse_ollama_ndjson_chunk data_str : ollama_chunk option =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let oll_model =
      json |> member "model" |> to_string_option
      |> Option.value ~default:""
    in
    let oll_is_done =
      json |> member "done" |> to_bool_option
      |> Option.value ~default:false
    in
    let oll_done_reason = json |> member "done_reason" |> to_string_option in
    let message = json |> member "message" in
    let oll_delta_content =
      match message with
      | `Assoc _ ->
          let s = message |> member "content" |> to_string_option in
          (match s with Some "" -> None | other -> other)
      | _ -> None
    in
    let oll_delta_thinking =
      match message with
      | `Assoc _ ->
          let s = message |> member "thinking" |> to_string_option in
          (match s with Some "" -> None | other -> other)
      | _ -> None
    in
    let oll_tool_calls =
      match message with
      | `Assoc _ ->
          (match message |> member "tool_calls" with
           | `List items ->
               List.mapi (fun idx tc ->
                 let func = tc |> member "function" in
                 let oll_tc_name = func |> member "name" |> to_string_option in
                 let oll_tc_id = tc |> member "id" |> to_string_option in
                 let oll_tc_arguments =
                   match func |> member "arguments" with
                   | `Null -> None
                   | (`Assoc _ | `List _) as v ->
                       Some (Yojson.Safe.to_string v)
                   | `String s -> Some s
                   | other -> Some (Yojson.Safe.to_string other)
                 in
                 { oll_tc_index = idx; oll_tc_id;
                   oll_tc_name; oll_tc_arguments }
               ) items
           | _ -> [])
      | _ -> []
    in
    (* Token-count usage. Ollama only emits these on the done chunk. *)
    let oll_usage =
      let input = json |> member "prompt_eval_count" |> to_int_option in
      let output = json |> member "eval_count" |> to_int_option in
      match input, output with
      | None, None -> None
      | _ ->
          Some {
            input_tokens = Option.value ~default:0 input;
            output_tokens = Option.value ~default:0 output;
            cache_creation_input_tokens = 0;
            cache_read_input_tokens = 0;
            cost_usd = None;
          }
    in
    (* inference_timings: same wire-format the non-streaming
       Backend_ollama.parse_ollama_response builds, so downstream
       (record_llm_tok_s_metrics) is byte-identical between the two
       paths. Durations are nanoseconds on the wire; convert to ms
       and tok/s here. *)
    let oll_timings =
      let prompt_n = json |> member "prompt_eval_count" |> to_int_option in
      let prompt_ns = json |> member "prompt_eval_duration" |> to_int_option in
      let predicted_n = json |> member "eval_count" |> to_int_option in
      let predicted_ns = json |> member "eval_duration" |> to_int_option in
      let any_set =
        Option.is_some prompt_n || Option.is_some prompt_ns
        || Option.is_some predicted_n || Option.is_some predicted_ns
      in
      if not any_set then None
      else
        let ms_of_ns ns_opt =
          Option.map (fun ns -> float_of_int ns /. 1e6) ns_opt
        in
        let per_second n_opt ns_opt =
          match n_opt, ns_opt with
          | Some n, Some ns when ns > 0 ->
              Some (float_of_int n /. (float_of_int ns /. 1e9))
          | _ -> None
        in
        Some {
          prompt_n;
          prompt_ms = ms_of_ns prompt_ns;
          prompt_per_second = per_second prompt_n prompt_ns;
          predicted_n;
          predicted_ms = ms_of_ns predicted_ns;
          predicted_per_second = per_second predicted_n predicted_ns;
          cache_n = None;
        }
    in
    Some {
      oll_model;
      oll_delta_content;
      oll_delta_thinking;
      oll_tool_calls;
      oll_done_reason;
      oll_is_done;
      oll_usage;
      oll_timings;
    }
  with
  | Yojson.Json_error _ -> None
  | Type_error (_, _) -> None

(** Convert a parsed {!ollama_chunk} into {!sse_event} list.
    Reuses {!openai_stream_state} for block index tracking. *)
let ollama_chunk_to_events (state : openai_stream_state)
    (chunk : ollama_chunk) : sse_event list =
  let events = ref [] in
  let emit evt = events := evt :: !events in
  (* Thinking content delta *)
  (match chunk.oll_delta_thinking with
   | Some text when text <> "" ->
       if not state.thinking_block_started then begin
         state.thinking_block_index <- state.next_block_index;
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "thinking";
           tool_id = None; tool_name = None });
         state.thinking_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       emit (ContentBlockDelta {
         index = state.thinking_block_index; delta = ThinkingDelta text })
   | _ -> ());
  (* Text content delta *)
  (match chunk.oll_delta_content with
   | Some text when text <> "" ->
       if not state.text_block_started then begin
         state.text_block_index <- state.next_block_index;
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "text";
           tool_id = None; tool_name = None });
         state.text_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       emit (ContentBlockDelta {
         index = state.text_block_index; delta = TextDelta text })
   | _ -> ());
  (* Tool calls. Ollama typically emits these complete in the done
     chunk rather than incrementally. We still keyed-cache by the
     per-tool index so a server that DID stream them incrementally
     would get correct accumulation behaviour. *)
  List.iter (fun (tc : ollama_tool_call_delta) ->
    let block_idx =
      match Hashtbl.find_opt state.tool_block_indices tc.oll_tc_index with
      | Some idx -> idx
      | None ->
          let idx = state.next_block_index in
          Hashtbl.replace state.tool_block_indices tc.oll_tc_index idx;
          emit (ContentBlockStart {
            index = idx; content_type = "tool_use";
            tool_id = tc.oll_tc_id; tool_name = tc.oll_tc_name;
          });
          state.next_block_index <- state.next_block_index + 1;
          idx
    in
    match tc.oll_tc_arguments with
    | Some args when args <> "" ->
        emit (ContentBlockDelta { index = block_idx;
                                  delta = InputJsonDelta args })
    | _ -> ()
  ) chunk.oll_tool_calls;
  (* Terminal chunk: emit MessageDelta with stop_reason + usage. *)
  if chunk.oll_is_done then begin
    let stop_reason =
      match chunk.oll_done_reason with
      | None -> Some EndTurn
      | Some reason ->
          (match String.lowercase_ascii reason with
           | "tool_calls" when chunk.oll_tool_calls <> [] -> Some StopToolUse
           | "length" -> Some MaxTokens
           | "stop" -> Some EndTurn
           | _other when chunk.oll_tool_calls <> [] -> Some StopToolUse
           | other -> Some (Unknown other))
    in
    emit (MessageDelta { stop_reason; usage = chunk.oll_usage })
  end;
  List.rev !events

[@@@coverage off]
(* ── parse_ollama_ndjson_chunk tests ──────────────────────── *)

let%test "parse_ollama_ndjson_chunk: content delta line" =
  let line =
    {|{"model":"qwen3:8b","message":{"role":"assistant","content":"hi"},"done":false}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c ->
    c.oll_model = "qwen3:8b"
    && c.oll_delta_content = Some "hi"
    && c.oll_delta_thinking = None
    && c.oll_tool_calls = []
    && not c.oll_is_done
    && c.oll_usage = None
    && c.oll_timings = None

let%test "parse_ollama_ndjson_chunk: done line carries timings + usage" =
  let line =
    {|{"model":"qwen3:8b","message":{"role":"assistant","content":""},
       "done_reason":"stop","done":true,
       "prompt_eval_count":15,"prompt_eval_duration":300000000,
       "eval_count":50,"eval_duration":1000000000}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c ->
    c.oll_is_done
    && c.oll_done_reason = Some "stop"
    && (match c.oll_usage with
        | Some u -> u.input_tokens = 15 && u.output_tokens = 50
        | None -> false)
    && (match c.oll_timings with
        | Some t ->
          t.predicted_n = Some 50
          && t.prompt_n = Some 15
          && (match t.predicted_per_second with
              | Some v -> abs_float (v -. 50.0) < 0.001
              | None -> false)
          && (match t.prompt_per_second with
              | Some v -> abs_float (v -. 50.0) < 0.001
              | None -> false)
        | None -> false)

let%test "parse_ollama_ndjson_chunk: zero eval_duration → per_second None" =
  let line =
    {|{"model":"qwen3:8b","message":{"role":"assistant","content":""},
       "done":true,"eval_count":10,"eval_duration":0}|}
  in
  match parse_ollama_ndjson_chunk line with
  | Some c ->
    (match c.oll_timings with
     | Some t -> t.predicted_n = Some 10 && t.predicted_per_second = None
     | None -> false)
  | None -> false

let%test "parse_ollama_ndjson_chunk: tool_calls fully formed in done line" =
  let line =
    {|{"model":"qwen3:8b","message":{"role":"assistant","content":"",
       "tool_calls":[{"function":{"name":"foo","arguments":{"x":1}}}]},
       "done":true,"done_reason":"tool_calls"}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c ->
    (match c.oll_tool_calls with
     | [tc] ->
       tc.oll_tc_name = Some "foo"
       && (match tc.oll_tc_arguments with
           | Some args ->
             let json = Yojson.Safe.from_string args in
             json |> Yojson.Safe.Util.member "x" |> Yojson.Safe.Util.to_int = 1
           | None -> false)
     | _ -> false)

let%test "parse_ollama_ndjson_chunk: malformed json → None" =
  parse_ollama_ndjson_chunk "{not valid" = None

(* ── ollama_chunk_to_events tests ─────────────────────────── *)

let%test "ollama_chunk_to_events: content delta emits Start+Delta" =
  let state = create_openai_stream_state () in
  let chunk = {
    oll_model = "qwen3:8b";
    oll_delta_content = Some "hello";
    oll_delta_thinking = None;
    oll_tool_calls = [];
    oll_done_reason = None;
    oll_is_done = false;
    oll_usage = None;
    oll_timings = None;
  } in
  let events = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart { index = 0; content_type = "text"; _ };
      ContentBlockDelta { index = 0; delta = TextDelta "hello" } ] -> true
  | _ -> false

let%test "ollama_chunk_to_events: subsequent content delta reuses block" =
  let state = create_openai_stream_state () in
  let mk text = {
    oll_model = "qwen3:8b";
    oll_delta_content = Some text;
    oll_delta_thinking = None;
    oll_tool_calls = [];
    oll_done_reason = None;
    oll_is_done = false;
    oll_usage = None;
    oll_timings = None;
  } in
  let _ = ollama_chunk_to_events state (mk "he") in
  let events = ollama_chunk_to_events state (mk "llo") in
  (* Second chunk: only Delta, no new Start *)
  match events with
  | [ ContentBlockDelta { index = 0; delta = TextDelta "llo" } ] -> true
  | _ -> false

let%test "ollama_chunk_to_events: done with stop_reason emits MessageDelta" =
  let state = create_openai_stream_state () in
  let chunk = {
    oll_model = "qwen3:8b";
    oll_delta_content = None;
    oll_delta_thinking = None;
    oll_tool_calls = [];
    oll_done_reason = Some "stop";
    oll_is_done = true;
    oll_usage = Some { input_tokens = 10; output_tokens = 20;
                       cache_creation_input_tokens = 0;
                       cache_read_input_tokens = 0; cost_usd = None };
    oll_timings = None;
  } in
  let events = ollama_chunk_to_events state chunk in
  match events with
  | [ MessageDelta { stop_reason = Some EndTurn; usage = Some u } ] ->
    u.input_tokens = 10 && u.output_tokens = 20
  | _ -> false

let%test "ollama_chunk_to_events: tool_calls emit Start+InputJsonDelta" =
  let state = create_openai_stream_state () in
  let chunk = {
    oll_model = "qwen3:8b";
    oll_delta_content = None;
    oll_delta_thinking = None;
    oll_tool_calls = [{
      oll_tc_index = 0; oll_tc_id = None;
      oll_tc_name = Some "search";
      oll_tc_arguments = Some {|{"q":"hello"}|};
    }];
    oll_done_reason = Some "tool_calls";
    oll_is_done = true;
    oll_usage = None;
    oll_timings = None;
  } in
  let events = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart { index = 0; content_type = "tool_use";
                          tool_name = Some "search"; _ };
      ContentBlockDelta { index = 0; delta = InputJsonDelta args };
      MessageDelta { stop_reason = Some StopToolUse; _ } ] ->
    args = {|{"q":"hello"}|}
  | _ -> false

let%test "ollama_chunk_to_events: thinking delta emits thinking block first" =
  let state = create_openai_stream_state () in
  let chunk = {
    oll_model = "qwen3:8b";
    oll_delta_content = None;
    oll_delta_thinking = Some "considering";
    oll_tool_calls = [];
    oll_done_reason = None;
    oll_is_done = false;
    oll_usage = None;
    oll_timings = None;
  } in
  let events = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart { index = 0; content_type = "thinking"; _ };
      ContentBlockDelta { index = 0;
                          delta = ThinkingDelta "considering" } ] -> true
  | _ -> false
