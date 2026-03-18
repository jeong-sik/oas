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
                   cache_creation_input_tokens; cache_read_input_tokens }
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
                   cache_creation_input_tokens; cache_read_input_tokens }
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
      let delta_reasoning = delta |> member "reasoning_content" |> to_string_option in
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
              with _ -> None
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
  mutable text_block_started: bool;
  tool_block_indices: (int, int) Hashtbl.t;  (** tool_call index -> block index *)
  mutable next_block_index: int;
}

let create_openai_stream_state () = {
  thinking_block_started = false;
  text_block_started = false;
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
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "thinking";
           tool_id = None; tool_name = None });
         state.thinking_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       emit (ContentBlockDelta { index = 0; delta = ThinkingDelta text })
   | _ -> ());
  (* Text content delta *)
  (match chunk.delta_content with
   | Some text when text <> "" ->
       if not state.text_block_started then begin
         emit (ContentBlockStart {
           index = state.next_block_index; content_type = "text";
           tool_id = None; tool_name = None });
         state.text_block_started <- true;
         state.next_block_index <- state.next_block_index + 1
       end;
       let text_idx = if state.thinking_block_started then 1 else 0 in
       emit (ContentBlockDelta { index = text_idx; delta = TextDelta text })
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
