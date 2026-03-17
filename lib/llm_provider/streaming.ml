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
