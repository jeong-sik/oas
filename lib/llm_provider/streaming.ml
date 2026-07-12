(** SSE event parsing and synthetic event emission.

    Pure functions operating on {!Llm_provider.Types}. No agent_sdk coupling.
    The streaming HTTP client (create_message_stream) remains in agent_sdk. *)

open Types

(** Parse a single SSE data JSON payload into an [sse_event]. *)
let parse_sse_event event_type data_str =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let evt_type =
      match event_type with
      | Some t -> t
      | None -> Cli_common_json.member_str "type" json
    in
    match evt_type with
    | "message_start" ->
      let msg = json |> member "message" in
      let id = msg |> member "id" |> to_string in
      let model = msg |> member "model" |> to_string in
      let usage =
        let u = msg |> member "usage" in
        if u = `Null
        then None
        else (
          let input_tokens = u |> member "input_tokens" |> to_int in
          let cache_creation_input_tokens =
            u
            |> member "cache_creation_input_tokens"
            |> to_int_option
            |> Option.value ~default:0
          in
          let cache_read_input_tokens =
            u
            |> member "cache_read_input_tokens"
            |> to_int_option
            |> Option.value ~default:0
          in
          Some
            { input_tokens
            ; output_tokens = 0
            ; cache_creation_input_tokens
            ; cache_read_input_tokens
            ; cost_usd = None
            })
      in
      Some (MessageStart { id; model; usage })
    | "content_block_start" ->
      let index = json |> member "index" |> to_int in
      let cb = json |> member "content_block" in
      let content_type = cb |> member "type" |> to_string in
      let tool_id =
        match content_type with
        | "redacted_thinking" -> cb |> member "data" |> to_string_option
        | _ -> cb |> member "id" |> to_string_option
      in
      let tool_name = cb |> member "name" |> to_string_option in
      let non_blank_tool_id =
        match tool_id with
        | Some id when not (Api_common.string_is_blank id) -> Some id
        | Some _ | None -> None
      in
      (match content_type, non_blank_tool_id with
       | "tool_use", None ->
         Some
           (SSEParseFailed
              { raw = Printf.sprintf "anthropic tool_use index %d" index
              ; reason =
                  Printf.sprintf "malformed_tool_use:index:%d:missing_provider_id" index
              })
       | "tool_use", Some tool_id ->
         Some
           (ContentBlockStart { index; content_type; tool_id = Some tool_id; tool_name })
       | _, _ -> Some (ContentBlockStart { index; content_type; tool_id; tool_name }))
    | "content_block_delta" ->
      let index = json |> member "index" |> to_int in
      let delta_json = json |> member "delta" in
      let delta_type = delta_json |> member "type" |> to_string in
      let delta =
        match delta_type with
        | "text_delta" -> TextDelta (delta_json |> member "text" |> to_string)
        | "thinking_delta" -> ThinkingDelta (delta_json |> member "thinking" |> to_string)
        | "signature_delta" ->
          ThinkingSignatureDelta (delta_json |> member "signature" |> to_string)
        | "input_json_delta" ->
          InputJsonDelta (delta_json |> member "partial_json" |> to_string)
        | unknown_delta_type ->
          raise
            (Yojson.Safe.Util.Type_error
               ("unsupported content_block_delta type: " ^ unknown_delta_type, delta_json))
      in
      Some (ContentBlockDelta { index; delta })
    | "content_block_stop" ->
      let index = json |> member "index" |> to_int in
      Some (ContentBlockStop { index })
    | "message_delta" ->
      let delta = json |> member "delta" in
      let stop_reason =
        delta
        |> member "stop_reason"
        |> to_string_option
        |> Option.map stop_reason_of_string
      in
      let usage =
        let u = json |> member "usage" in
        if u = `Null
        then None
        else (
          let output_tokens = u |> member "output_tokens" |> to_int in
          let cache_creation_input_tokens =
            u
            |> member "cache_creation_input_tokens"
            |> to_int_option
            |> Option.value ~default:0
          in
          let cache_read_input_tokens =
            u
            |> member "cache_read_input_tokens"
            |> to_int_option
            |> Option.value ~default:0
          in
          Some
            { input_tokens = 0
            ; output_tokens
            ; cache_creation_input_tokens
            ; cache_read_input_tokens
            ; cost_usd = None
            })
      in
      Some (MessageDelta { stop_reason; usage })
    | "message_stop" -> Some MessageStop
    | "ping" -> Some Ping
    | "error" ->
      let err = json |> member "error" in
      let message =
        match err |> member "message" |> to_string_option with
        | Some m -> m
        | None -> data_str
      in
      let error_type = err |> member "type" |> to_string_option in
      Some (SSEError { message; error_type; raw = data_str })
    | other -> Some (SSEUnknownEventType { event_type = other; raw = data_str })
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Some (SSEParseFailed { raw = data_str; reason = "type_error: " ^ msg })
  | Yojson.Json_error msg ->
    Some (SSEParseFailed { raw = data_str; reason = "json_error: " ^ msg })
;;

(* RFC-OAS-020: TTFT classification — distinguishes Anthropic prelude
   events ([MessageStart], [ContentBlockStart], [Ping]) from the
   first generated token. Capture point in [Complete] uses this
   to fill [Streaming_summary.ttft_ms]. *)
let sse_event_is_first_token_signal (e : sse_event) : bool =
  let non_empty s = String.length s > 0 in
  match e with
  | ContentBlockDelta { delta = TextDelta s; _ } -> non_empty s
  | ContentBlockDelta { delta = ThinkingDelta s; _ } -> non_empty s
  | ContentBlockDelta { delta = ReasoningDetailsDelta { reasoning_content; details }; _ }
    ->
    (match reasoning_content with
     | Some s when non_empty s -> true
     | Some _ | None -> details <> [])
  | ContentBlockDelta { delta = InputJsonDelta s | InputJsonSnapshot s; _ } -> non_empty s
  | ContentBlockDelta { delta = MediaDelta { data; _ }; _ } -> non_empty data
  | ContentBlockDelta { delta = ThinkingSignatureDelta _; _ } -> false
  | MessageStart _
  | ContentBlockStart _
  | ContentBlockStop _
  | MessageDelta _
  | MessageStop
  | Ping
  | SSEError _
  | SSEParseFailed _
  | SSEUnknownEventType _
  | Connected
  | Timeout _
  | StreamIncomplete _ -> false
;;

let sse_event_is_deliverable_progress_signal (e : sse_event) : bool =
  let non_empty s = String.length s > 0 in
  match e with
  | ContentBlockDelta { delta = TextDelta s; _ } -> non_empty s
  | ContentBlockDelta { delta = InputJsonDelta s | InputJsonSnapshot s; _ } -> non_empty s
  | ContentBlockDelta { delta = MediaDelta { data; _ }; _ } -> non_empty data
  | ContentBlockStart { content_type = "tool_use"; _ } -> true
  | ContentBlockDelta { delta = ThinkingSignatureDelta _; _ }
  | ContentBlockDelta { delta = ThinkingDelta _; _ }
  | ContentBlockDelta { delta = ReasoningDetailsDelta _; _ }
  | MessageStart _
  | ContentBlockStart _
  | ContentBlockStop _
  | MessageDelta _
  | MessageStop
  | Ping
  | SSEError _
  | SSEParseFailed _
  | SSEUnknownEventType _
  | Connected
  | Timeout _
  | StreamIncomplete _ -> false
;;

(** Emit synthetic SSE events from a complete [api_response].
    Used as fallback for non-Anthropic providers that don't support SSE. *)
let emit_synthetic_events (response : api_response) on_event =
  on_event
    (MessageStart { id = response.id; model = response.model; usage = response.usage });
  List.iteri
    (fun index block ->
       let content_type, tool_id, tool_name =
         match block with
         | Text _ -> "text", None, None
         | Thinking _ -> "thinking", None, None
         | ReasoningDetails _ -> "reasoning_details", None, None
         | ToolUse { id; name; _ } -> "tool_use", Some id, Some name
         | Image _ -> "image", None, None
         | Document _ -> "document", None, None
         | Audio _ -> "audio", None, None
         | RedactedThinking data -> "redacted_thinking", Some data, None
         | ToolResult _ -> "text", None, None
       in
       on_event (ContentBlockStart { index; content_type; tool_id; tool_name });
       (match block with
        | Text s -> on_event (ContentBlockDelta { index; delta = TextDelta s })
        | Thinking { content; signature } ->
          on_event (ContentBlockDelta { index; delta = ThinkingDelta content });
          (match signature with
           | Some s ->
             on_event (ContentBlockDelta { index; delta = ThinkingSignatureDelta s })
           | None -> ())
        | ReasoningDetails { reasoning_content; details } ->
          on_event
            (ContentBlockDelta
               { index; delta = ReasoningDetailsDelta { reasoning_content; details } })
        | ToolUse { input; _ } ->
          on_event
            (ContentBlockDelta
               { index; delta = InputJsonSnapshot (Yojson.Safe.to_string input) })
        | Image { media_type; data; source_type }
        | Document { media_type; data; source_type }
        | Audio { media_type; data; source_type } ->
          (* Re-emit media payload through the typed media channel so a
             non-streamed media block survives an api_response -> synthetic SSE
             -> accumulator round-trip instead of collapsing to empty text. *)
          on_event
            (ContentBlockDelta
               { index; delta = MediaDelta { media_type; source_type; data } })
        | RedactedThinking _ | ToolResult _ -> ());
       on_event (ContentBlockStop { index }))
    response.content;
  on_event
    (MessageDelta { stop_reason = Some response.stop_reason; usage = response.usage });
  on_event MessageStop
;;

let%test "emit_synthetic_events round-trips a media block through the accumulator" =
  (* A non-streamed api_response carrying an Image survives the synthetic-SSE
     fallback: emit_synthetic_events -> Complete_stream_acc -> finalize yields the
     same Image. Revert the media delta/content_type arms here (or the accumulator
     media arms) and the image collapses to empty text, so this goes red. This is
     the real producer that keeps the new MediaDelta path from being dead code. *)
  let image =
    Image { media_type = "image/png"; data = "iVBORw0KGgo="; source_type = Types.Base64 }
  in
  let response : api_response =
    { id = "msg_rt"
    ; model = "test-model"
    ; stop_reason = EndTurn
    ; content = [ image ]
    ; usage = None
    ; telemetry = None
    }
  in
  let events = ref [] in
  emit_synthetic_events response (fun e -> events := e :: !events);
  let acc = Complete_stream_acc.create_stream_acc () in
  List.iter (Complete_stream_acc.accumulate_event acc) (List.rev !events);
  match Complete_stream_acc.finalize_stream_acc acc with
  | Error _ -> false
  | Ok result -> result.content = [ image ]
;;

(** {1 OpenAI-compatible SSE Streaming}

    Openai Chat Completions streaming uses SSE with flat deltas:
    - Text content arrives as [delta.content] strings
    - Tool calls arrive as [delta.tool_calls] with incremental arguments
    - [finish_reason] signals end of a choice
    - No explicit content_block_start/stop events (unlike Anthropic)

    We parse each SSE chunk into {!openai_chunk}, then convert to
    {!sse_event} list using a stateful adapter ({!openai_stream_state}). *)

(* Wire shape of streamed tool-call arguments. [Args_fragment] is an incremental
   string chunk that the accumulator appends; [Args_complete] is a whole
   object/array value serialized in a single delta, which the accumulator uses
   to replace the block buffer so a provider that re-emits the same complete
   value does not concatenate it into invalid JSON. *)
type tool_call_arguments =
  | Args_fragment of string
  | Args_complete of string

type openai_tool_call_delta =
  { tc_index : int
  ; tc_id : string option
  ; tc_name : string option
  ; tc_arguments : tool_call_arguments option
  }

type openai_reasoning_details_delta =
  { delta_reasoning_content : string option
  ; delta_details : reasoning_detail list
  }

type openai_chunk_parse_error =
  { reason : string
  ; raw : string
  }

type openai_chunk =
  { chunk_id : string
  ; chunk_model : string
  ; delta_content : string option
  ; delta_reasoning : string option
  ; delta_reasoning_details : openai_reasoning_details_delta option
  ; delta_tool_calls : openai_tool_call_delta list
  ; finish_reason : string option
  ; chunk_usage : api_usage option
  ; chunk_parse_error : openai_chunk_parse_error option
  }

(* RFC-OAS-020: TTFT classification for OpenAI-compat / Gemini /
   Ollama chunk streams. [true] when this chunk would surface a
   visible token (or tool-call argument) to the application;
   [false] for role-prelude chunks, [DONE]-only finalisers, and
   empty deltas. *)
let chunk_has_non_empty_delta (c : openai_chunk) : bool =
  let non_empty_opt = function
    | Some s -> String.length s > 0
    | None -> false
  in
  let non_empty_reasoning_details = function
    | Some { delta_reasoning_content; delta_details } ->
      non_empty_opt delta_reasoning_content || delta_details <> []
    | None -> false
  in
  non_empty_opt c.delta_content
  || non_empty_opt c.delta_reasoning
  || non_empty_reasoning_details c.delta_reasoning_details
  || c.delta_tool_calls <> []
;;

(* OpenAI-compatible / GLM / DashScope / Kimi streams terminate with this
   literal SSE payload (the bare token after [data: ]), not a JSON object. SSOT
   for the sentinel shared by the chunk parser, the terminal-event helper, and
   the Responses-API trailer. *)
let openai_done_sentinel = "[DONE]"

let assoc_field_opt name = function
  | `Assoc fields -> List.assoc_opt name fields
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let non_blank_json_string = function
  | `String s when String.trim s <> "" -> Ok (Some s)
  | `String _ -> Ok None
  | `Null -> Ok None
  | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ -> Error "not_string"
;;

let parse_stream_reasoning_detail ~index = function
  | `Assoc fields as raw ->
    let text =
      match List.assoc_opt "text" fields with
      | Some (`String s) when String.trim s <> "" -> Some s
      | Some (`String _)
      | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
      | None -> None
    in
    Ok { raw; text }
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
    Error (Printf.sprintf "malformed_delta_reasoning_details:index:%d:not_object" index)
;;

let parse_stream_reasoning_details = function
  | None -> Ok None
  | Some `Null -> Ok None
  | Some (`List []) -> Ok None
  | Some (`List details) ->
    let rec loop index acc = function
      | [] -> Ok (Some (List.rev acc))
      | detail :: rest ->
        (match parse_stream_reasoning_detail ~index detail with
         | Ok parsed -> loop (index + 1) (parsed :: acc) rest
         | Error _ as error -> error)
    in
    loop 0 [] details
  | Some (`Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _) ->
    Error "malformed_delta_reasoning_details:not_list"
;;

(** Parse a single Openai SSE data payload into an {!openai_chunk}.
    Returns [None] for the {!openai_done_sentinel} ("[DONE]") or unparseable
    data. *)
let parse_openai_sse_chunk ?streaming_reasoning data_str : openai_chunk option =
  if String.equal data_str openai_done_sentinel
  then None
  else
    let open Yojson.Safe.Util in
    try
      let json = Yojson.Safe.from_string data_str in
      let chunk_id = Cli_common_json.member_str "id" json in
      let chunk_model = Cli_common_json.member_str "model" json in
      (* [usage] is top-level, not under a choice, so it is read regardless of
         whether [choices] is populated. With stream_options.include_usage the
         provider sends a final chunk carrying [usage] with an empty [choices]
         array; this is the only chunk that reports token totals. *)
      let chunk_usage =
        let u = json |> member "usage" in
        if u = `Null
        then None
        else (
          let cached =
            let d = u |> member "prompt_tokens_details" in
            if d = `Null then 0 else Cli_common_json.member_int "cached_tokens" d
          in
          Some
            { input_tokens = Cli_common_json.member_int "prompt_tokens" u
            ; output_tokens = Cli_common_json.member_int "completion_tokens" u
            ; cache_creation_input_tokens = 0
            ; cache_read_input_tokens = cached
            ; cost_usd = None
            })
      in
      (* Match the choices list directly rather than [index 0] / [to_list]:
         both raise [Type_error] on a [`Null] or empty array, which the outer
         handler would turn into a dropped chunk. The usage-only final chunk
         has [choices = []] and must still surface its [chunk_usage]. *)
      let choices =
        match json |> member "choices" with
        | `List l -> l
        | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []
      in
      match choices with
      | [] ->
        (* Usage-only final chunk: no delta, no finish_reason. Surface it only
            when it actually carries [usage] (the stream_options.include_usage
            final chunk). A [choices = []] chunk with no usage carries nothing,
            so drop it (None) — this keeps non-usage empty chunks from producing
            an eventless Some. *)
        (match chunk_usage with
         | Some _ ->
           Some
             { chunk_id
             ; chunk_model
             ; delta_content = None
             ; delta_reasoning = None
             ; delta_reasoning_details = None
             ; delta_tool_calls = []
             ; finish_reason = None
             ; chunk_usage
             ; chunk_parse_error = None
             }
         | None -> None)
      | choice :: _ ->
        let delta = choice |> member "delta" in
        let delta_content = delta |> member "content" |> to_string_option in
        let non_blank_delta_field field =
          match delta |> member field |> to_string_option with
          | Some s when String.trim s <> "" -> Some s
          | Some _ | None -> None
        in
        let delta_reasoning, delta_reasoning_details, chunk_parse_error =
          match streaming_reasoning with
          | Some Reasoning_dialect.Delta_reasoning_details ->
            let reasoning_content_result =
              match assoc_field_opt "reasoning_content" delta with
              | None -> Ok None
              | Some value ->
                (match non_blank_json_string value with
                 | Ok _ as ok -> ok
                 | Error _ -> Error "malformed_delta_reasoning_content:not_string")
            in
            let details_result =
              parse_stream_reasoning_details (assoc_field_opt "reasoning_details" delta)
            in
            let split_result =
              match reasoning_content_result, details_result with
              | Ok reasoning_content, Ok details ->
                (match reasoning_content, details with
                 | None, None -> Ok None
                 | Some _, None | _, Some _ ->
                   Ok
                     (Some
                        { delta_reasoning_content = reasoning_content
                        ; delta_details = Option.value details ~default:[]
                        }))
              | Error reason, Ok _ | Ok _, Error reason | Error reason, Error _ ->
                Error reason
            in
            (match split_result with
             | Ok details -> None, details, None
             | Error reason -> None, None, Some { reason; raw = data_str })
          | Some (Reasoning_dialect.Delta_field field) ->
            non_blank_delta_field field, None, None
          | Some
              ( Reasoning_dialect.No_streaming_reasoning
              | Reasoning_dialect.Template_parser ) -> None, None, None
          | None ->
            (match non_blank_delta_field "reasoning_content" with
             | Some _ as reasoning -> reasoning, None, None
             | None -> delta |> member "reasoning" |> to_string_option, None, None)
        in
        let delta_tool_calls =
          match delta |> member "tool_calls" with
          | `List calls ->
            List.filter_map
              (fun tc ->
                 try
                   let tc_index = tc |> member "index" |> to_int in
                   let tc_id = tc |> member "id" |> to_string_option in
                   let fn = tc |> member "function" in
                   let tc_name = fn |> member "name" |> to_string_option in
                   let tc_arguments =
                     (* llama.cpp/llama-server (#20198) streams tool-call
                        arguments as a JSON object/array, not a serialized
                        string. A whole object/array is a complete value, so
                        tag it [Args_complete]: the accumulator replaces the
                        block buffer instead of appending, so a provider that
                        re-emits the same value over multiple chunks does not
                        concatenate it into invalid JSON. A bare string is an
                        incremental fragment ([Args_fragment]). *)
                     match fn |> member "arguments" with
                     | `Null -> None
                     | (`Assoc _ | `List _) as v ->
                       Some (Args_complete (Yojson.Safe.to_string v))
                     | `String s -> Some (Args_fragment s)
                     | other -> Some (Args_complete (Yojson.Safe.to_string other))
                   in
                   Some { tc_index; tc_id; tc_name; tc_arguments }
                 with
                 | Yojson.Safe.Util.Type_error _ | Not_found -> None)
              calls
          | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []
        in
        let finish_reason = choice |> member "finish_reason" |> to_string_option in
        Some
          { chunk_id
          ; chunk_model
          ; delta_content
          ; delta_reasoning
          ; delta_reasoning_details
          ; delta_tool_calls
          ; finish_reason
          ; chunk_usage
          ; chunk_parse_error
          }
    with
    | Yojson.Safe.Util.Type_error _
    | Yojson.Safe.Util.Undefined _
    | Yojson.Json_error _
    | Invalid_argument _ -> None
;;

(** Detect an OpenAI-compatible mid-stream error object
    ([{"error": {"type"; "message"; ...}}]) in a payload that
    [parse_openai_sse_chunk] turned into [None] (such a chunk has no [choices]
    and would otherwise be dropped, letting the stream finalize as a phantom
    completion). Surfaces a typed [SSEError] carrying the provider [type] and
    raw JSON so the accumulator marks the stream failed and the consumer routes
    it through the typed classification path. [None] for the [DONE] sentinel,
    empty/usage-only chunks, and malformed data. *)
let openai_compat_error_event data_str : sse_event option =
  match Yojson.Safe.from_string data_str with
  | exception Yojson.Json_error _ -> None
  | json ->
    let open Yojson.Safe.Util in
    (match json with
     | `Assoc _ ->
       (match json |> member "error" with
        | `Assoc _ as err ->
          let message =
            match err |> member "message" |> to_string_option with
            | Some m -> m
            | None -> data_str
          in
          let error_type = err |> member "type" |> to_string_option in
          Some (SSEError { message; error_type; raw = data_str })
        | `String s -> Some (SSEError { message = s; error_type = None; raw = data_str })
        | _non_error_object -> None)
     | _non_object_payload -> None)
;;

(** Terminal events for an OpenAI-compatible SSE payload that
    {!parse_openai_sse_chunk} returned [None] for. The explicit
    {!openai_done_sentinel} ("[DONE]") becomes a single [MessageStop]: the
    accumulator records a clean stream close and may default a missing
    [stop_reason] to [EndTurn] rather than rejecting a phantom completion (some
    providers send [data: [DONE]] with every prior chunk carrying
    [finish_reason: null]). A provider error object becomes a typed [SSEError]
    (see {!openai_compat_error_event}); any other [None] payload
    (empty/usage-only/malformed) yields no events. A stream truncated WITHOUT a
    [DONE] sentinel never reaches here, so it still finalizes as [Error].

    @since 0.207.25 *)
let openai_compat_terminal_events data_str : sse_event list =
  if String.equal data_str openai_done_sentinel
  then [ MessageStop ]
  else (
    match openai_compat_error_event data_str with
    | Some evt -> [ evt ]
    | None -> [])
;;

(** Mutable state for converting Openai flat deltas to block-based events. *)
type thinking_state =
  | Not_thinking
  | Thinking_started of float
  | Thinking_done

type tool_index_route =
  | Tool_index_single of int
  | Tool_index_ambiguous of int list

type tool_identity_origin =
  | Provider_supplied
  | Oas_allocated

type tool_block_identity =
  { id : string
  ; origin : tool_identity_origin
  }

type openai_stream_state =
  { mutable thinking_block_started : bool
  ; mutable thinking_block_index : int
  ; mutable text_block_started : bool
  ; mutable text_block_index : int
  ; tool_blocks_by_id : (string, int) Hashtbl.t
    (** Secondary lookup index from tool identity to normalized block index.
        [tool_block_identities] remains the identity authority. *)
  ; tool_block_indices : (int, tool_index_route) Hashtbl.t
    (** tool_call wire index -> routing state. Id-less continuation fragments
        can route only while the wire index maps to one known block. Once more
        than one id has used the same wire index, id-less fragments are
        ambiguous and must fail loud rather than silently picking the last
        writer. *)
  ; tool_block_identities : (int, tool_block_identity) Hashtbl.t
    (** Normalized block index -> the identity emitted at block start.  This is
        the sole identity authority for an open tool block. *)
  ; mutable next_block_index : int
  ; mutable thinking_state : thinking_state
  ; provider : string
  ; model : string
  }

let create_openai_stream_state ?(provider = "") ?(model = "") () =
  { thinking_block_started = false
  ; thinking_block_index = -1
  ; text_block_started = false
  ; text_block_index = -1
  ; tool_blocks_by_id = Hashtbl.create 4
  ; tool_block_indices = Hashtbl.create 4
  ; tool_block_identities = Hashtbl.create 4
  ; next_block_index = 0
  ; thinking_state = Not_thinking
  ; provider
  ; model
  }
;;

let tool_index_route_add_block route block_index =
  match route with
  | Tool_index_single existing when existing = block_index -> Tool_index_single existing
  | Tool_index_single existing ->
    Tool_index_ambiguous (List.sort_uniq compare [ existing; block_index ])
  | Tool_index_ambiguous indices ->
    Tool_index_ambiguous (List.sort_uniq compare (block_index :: indices))
;;

let record_tool_index_route table tool_index block_index =
  let route =
    match Hashtbl.find_opt table tool_index with
    | Some route -> tool_index_route_add_block route block_index
    | None -> Tool_index_single block_index
  in
  Hashtbl.replace table tool_index route
;;

type tool_block_index_policy =
  | Next_available
  | Next_after_carrier
  | Wire_index

type idless_tool_semantics =
  (* Argument fragments continue the one call selected by the wire index. *)
  | Continue_by_wire_index
  (* Each complete provider item is a new call occurrence.  Identical
      name/arguments must not collapse into one identity. *)
  | Complete_occurrence

type tool_route_failure =
  | Ambiguous_idless_continuation
  | Late_provider_identity
  | Provider_identity_route_conflict
  | Identity_index_conflict
  | Block_index_collision
  | Missing_block_identity

type tool_block_resolution =
  | Tool_block_resolved of
      { block_index : int
      ; identity : tool_block_identity
      }
  | Tool_block_rejected of sse_event

let non_blank_provider_tool_id = function
  | Some id when not (Api_common.string_is_blank id) -> Some id
  | Some _ | None -> None
;;

let tool_route_failure_reason = function
  | Ambiguous_idless_continuation ->
    "ambiguous_tool_call_index: id-less continuation follows multiple tool identities"
  | Late_provider_identity ->
    "late_provider_tool_call_id: provider identity arrived after an OAS identity was \
     emitted"
  | Provider_identity_route_conflict ->
    "provider_tool_call_id_route_conflict: one provider identity used multiple wire \
     routes"
  | Identity_index_conflict ->
    "tool_identity_index_conflict: identity lookup and block authority disagree"
  | Block_index_collision ->
    "tool_block_index_collision: distinct tool identities require the same normalized \
     block"
  | Missing_block_identity ->
    "missing_tool_block_identity: tool route references a block without identity state"
;;

let reject_tool_block ~protocol ~wire_index failure =
  Tool_block_rejected
    (SSEParseFailed
       { raw = Printf.sprintf "%s tool_call index %d" protocol wire_index
       ; reason = tool_route_failure_reason failure
       })
;;

let register_tool_block_identity state ~block_index identity =
  Hashtbl.replace state.tool_blocks_by_id identity.id block_index;
  Hashtbl.replace state.tool_block_identities block_index identity
;;

let open_tool_block state ~protocol ~wire_index ~provider_tool_id ~tool_name ~index_policy
  =
  let block_index =
    match index_policy with
    | Next_available -> state.next_block_index
    | Next_after_carrier -> state.next_block_index + 1
    | Wire_index -> wire_index
  in
  if Hashtbl.mem state.tool_block_identities block_index
  then reject_tool_block ~protocol ~wire_index Block_index_collision, None
  else (
    let identity =
      match non_blank_provider_tool_id provider_tool_id with
      | Some id -> { id; origin = Provider_supplied }
      | None -> { id = Api_common.fresh_tool_use_id (); origin = Oas_allocated }
    in
    register_tool_block_identity state ~block_index identity;
    state.next_block_index <- max state.next_block_index (block_index + 1);
    ( Tool_block_resolved { block_index; identity }
    , Some
        (ContentBlockStart
           { index = block_index
           ; content_type = "tool_use"
           ; tool_id = Some identity.id
           ; tool_name
           }) ))
;;

let resolve_tool_block
      state
      ~protocol
      ~wire_index
      ~provider_tool_id
      ~tool_name
      ~index_policy
      ~idless_semantics
  =
  let reject failure = reject_tool_block ~protocol ~wire_index failure, None in
  let open_and_record () =
    let resolution, start =
      open_tool_block
        state
        ~protocol
        ~wire_index
        ~provider_tool_id
        ~tool_name
        ~index_policy
    in
    (match resolution with
     | Tool_block_resolved { block_index; _ } ->
       record_tool_index_route state.tool_block_indices wire_index block_index
     | Tool_block_rejected _ -> ());
    resolution, start
  in
  let route_contains_block route block_index =
    match route with
    | Tool_index_single routed -> routed = block_index
    | Tool_index_ambiguous routed -> List.mem block_index routed
  in
  match non_blank_provider_tool_id provider_tool_id with
  | Some id ->
    (match Hashtbl.find_opt state.tool_blocks_by_id id with
     | Some block_index ->
       (match Hashtbl.find_opt state.tool_block_indices wire_index with
        | Some route when route_contains_block route block_index ->
          (match Hashtbl.find_opt state.tool_block_identities block_index with
           | Some identity when String.equal identity.id id ->
             Tool_block_resolved { block_index; identity }, None
           | Some _ -> reject Identity_index_conflict
           | None -> reject Missing_block_identity)
        | Some _ | None -> reject Provider_identity_route_conflict)
     | None ->
       (match Hashtbl.find_opt state.tool_block_indices wire_index with
        | Some (Tool_index_single block_index) ->
          (match Hashtbl.find_opt state.tool_block_identities block_index with
           | Some { origin = Oas_allocated; _ } -> reject Late_provider_identity
           | Some { origin = Provider_supplied; _ } ->
             (match index_policy with
              | Next_available | Next_after_carrier -> open_and_record ()
              | Wire_index -> reject Block_index_collision)
           | None -> reject Missing_block_identity)
        | Some (Tool_index_ambiguous _) ->
          (match index_policy with
           | Next_available | Next_after_carrier -> open_and_record ()
           | Wire_index -> reject Block_index_collision)
        | None -> open_and_record ()))
  | None ->
    (match idless_semantics with
     | Complete_occurrence -> open_and_record ()
     | Continue_by_wire_index ->
       (match Hashtbl.find_opt state.tool_block_indices wire_index with
        | Some (Tool_index_single block_index) ->
          (match Hashtbl.find_opt state.tool_block_identities block_index with
           | Some identity -> Tool_block_resolved { block_index; identity }, None
           | None -> reject Missing_block_identity)
        | Some (Tool_index_ambiguous _) -> reject Ambiguous_idless_continuation
        | None -> open_and_record ()))
;;

(* OpenAI-compatible streams carry no wire [content_block_stop] event (unlike
   Anthropic, whose stops are parsed straight off the wire). Synthesize one
   [ContentBlockStop] per block this state opened so the emitted OAS event
   sequence stays symmetric: every [ContentBlockStart] gets a matching
   [ContentBlockStop]. The stream accumulator ignores stops, but block-lifecycle
   consumers depend on them — a stop closes a tool block so the NEXT provider
   message (a separate stream whose block indices restart at 0) reuses the same
   index without colliding with a still-open block, and so a tool-call-end is
   surfaced for that block. Called once on the terminal [finish_reason] chunk;
   blank/missing-finish chunks never close blocks. *)
let openai_open_block_stops (state : openai_stream_state) : sse_event list =
  let indices =
    (if state.thinking_block_started then [ state.thinking_block_index ] else [])
    @ (if state.text_block_started then [ state.text_block_index ] else [])
    @ Hashtbl.fold
        (fun block_index _identity acc -> block_index :: acc)
        state.tool_block_identities
        []
  in
  indices |> List.sort_uniq compare |> List.map (fun index -> ContentBlockStop { index })
;;

(** Convert a parsed {!openai_chunk} into {!sse_event} list.
    Synthesizes [ContentBlockStart] events on first occurrence of
    text content or each new tool_call index, and a matching
    [ContentBlockStop] for every open block on the terminal finish chunk. *)
let openai_chunk_to_events (state : openai_stream_state) (chunk : openai_chunk)
  : sse_event list * Telemetry_event.t option
  =
  let events = ref [] in
  let emit evt = events := evt :: !events in
  let telemetry_event = ref None in
  match chunk.chunk_parse_error with
  | Some { reason; raw } -> [ SSEParseFailed { reason; raw } ], None
  | None ->
    (* Reasoning content delta — emitted before text *)
    (match chunk.delta_reasoning with
     | Some text when text <> "" ->
       (match state.thinking_state with
        | Not_thinking -> state.thinking_state <- Thinking_started (Unix.gettimeofday ())
        | Thinking_started _ | Thinking_done ->
          (* Already started, or model is re-emitting reasoning after a
           closure — keep current state. Enumerated explicitly so adding
           a new [thinking_state] variant forces review of how it should
           react to a fresh reasoning chunk. *)
          ());
       if not state.thinking_block_started
       then (
         state.thinking_block_index <- state.next_block_index;
         emit
           (ContentBlockStart
              { index = state.next_block_index
              ; content_type = "thinking"
              ; tool_id = None
              ; tool_name = None
              });
         state.thinking_block_started <- true;
         state.next_block_index <- state.next_block_index + 1);
       emit
         (ContentBlockDelta
            { index = state.thinking_block_index; delta = ThinkingDelta text })
     | Some "" ->
       (match state.thinking_state with
        | Thinking_started t0 ->
          let thinking_duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
          state.thinking_state <- Thinking_done;
          telemetry_event
          := Some
               (Telemetry_event.Thinking_complete
                  { provider = state.provider; model = state.model; thinking_duration_ms })
        | Not_thinking | Thinking_done ->
          (* Empty reasoning chunk while never started, or after a prior
           close — no telemetry to emit. Enumerated explicitly so a new
           [thinking_state] variant cannot silently inherit the no-op. *)
          ())
     | Some empty_reasoning ->
       let (_ : string) = empty_reasoning in
       ()
     | None -> ());
    (match chunk.delta_reasoning_details with
     | Some { delta_reasoning_content; delta_details } ->
       (match state.thinking_state with
        | Not_thinking -> state.thinking_state <- Thinking_started (Unix.gettimeofday ())
        | Thinking_started _ | Thinking_done -> ());
       if not state.thinking_block_started
       then (
         state.thinking_block_index <- state.next_block_index;
         emit
           (ContentBlockStart
              { index = state.next_block_index
              ; content_type = "reasoning_details"
              ; tool_id = None
              ; tool_name = None
              });
         state.thinking_block_started <- true;
         state.next_block_index <- state.next_block_index + 1);
       emit
         (ContentBlockDelta
            { index = state.thinking_block_index
            ; delta =
                ReasoningDetailsDelta
                  { reasoning_content = delta_reasoning_content; details = delta_details }
            })
     | None -> ());
    (* Text content delta *)
    (match chunk.delta_content with
     | Some text when text <> "" ->
       if not state.text_block_started
       then (
         state.text_block_index <- state.next_block_index;
         emit
           (ContentBlockStart
              { index = state.next_block_index
              ; content_type = "text"
              ; tool_id = None
              ; tool_name = None
              });
         state.text_block_started <- true;
         state.next_block_index <- state.next_block_index + 1);
       emit (ContentBlockDelta { index = state.text_block_index; delta = TextDelta text })
     | Some empty_text ->
       let (_ : string) = empty_text in
       ()
     | None -> ());
    (* Tool call deltas *)
    List.iter
      (fun (tc : openai_tool_call_delta) ->
         let resolution, start =
           resolve_tool_block
             state
             ~protocol:"openai"
             ~wire_index:tc.tc_index
             ~provider_tool_id:tc.tc_id
             ~tool_name:tc.tc_name
             ~index_policy:Next_available
             ~idless_semantics:Continue_by_wire_index
         in
         Option.iter emit start;
         match resolution, tc.tc_arguments with
         | Tool_block_resolved { block_index; _ }, Some (Args_fragment args)
           when args <> "" ->
           emit (ContentBlockDelta { index = block_index; delta = InputJsonDelta args })
         | Tool_block_resolved { block_index; _ }, Some (Args_complete args)
           when args <> "" ->
           emit
             (ContentBlockDelta { index = block_index; delta = InputJsonSnapshot args })
         | Tool_block_resolved _, Some (Args_fragment _ | Args_complete _)
         | Tool_block_resolved _, None -> ()
         | Tool_block_rejected error, _ -> emit error)
      chunk.delta_tool_calls;
    (* Finish reason -> MessageDelta *)
    (match chunk.finish_reason with
     | Some reason ->
       let stop_reason =
         (* OpenAI wire vocabulary -> PROVISIONAL stop_reason. The accumulated
          tool-block set is unknown at the chunk boundary (tool arguments arrive
          as deltas before this terminal chunk), so a "tool_calls" finish is
          recorded as a provisional StopToolUse and reconciled against the
          assembled content in Complete_stream_acc.finalize_stream_acc
          (Stop_reason_wire.reconcile). SSOT: the wire vocabulary lives in
          Stop_reason_wire, not in an unguarded chunk-level match. *)
         Stop_reason_wire.provisional_of_string reason
       in
       (* Close every block this message opened before the terminal MessageDelta,
          mirroring Anthropic's content_block_stop-then-message_delta ordering.
          Without this the OpenAI-compat stream leaves tool blocks open and a
          downstream per-message block-index consumer collides on the next
          message's reused index. *)
       List.iter emit (openai_open_block_stops state);
       emit (MessageDelta { stop_reason = Some stop_reason; usage = chunk.chunk_usage })
     | None ->
       (* With stream_options.include_usage the provider sends token totals in a
        separate final chunk that has no finish_reason and an empty choices
        array (hence no content/tool deltas). Emit a MessageDelta carrying only
        the usage so the accumulator records it. The finish_reason branch above
        already forwards usage when a stop arrives in the same chunk, so the two
        paths are mutually exclusive and usage is never emitted twice. *)
       (match chunk.chunk_usage with
        | Some _ -> emit (MessageDelta { stop_reason = None; usage = chunk.chunk_usage })
        | None -> ()));
    List.rev !events, !telemetry_event
;;

let test_tool_use_start_with_name = function
  | ContentBlockStart { index; content_type = "tool_use"; tool_name; _ } ->
    Some (index, tool_name)
  | MessageStart _
  | ContentBlockStart _
  | ContentBlockDelta _
  | ContentBlockStop _
  | MessageDelta _
  | MessageStop
  | Ping
  | SSEError _
  | SSEParseFailed _
  | SSEUnknownEventType _
  | Connected
  | Timeout _
  | StreamIncomplete _ -> None
;;

let test_tool_use_start = function
  | ContentBlockStart { content_type = "tool_use"; _ } -> Some ()
  | MessageStart _
  | ContentBlockStart _
  | ContentBlockDelta _
  | ContentBlockStop _
  | MessageDelta _
  | MessageStop
  | Ping
  | SSEError _
  | SSEParseFailed _
  | SSEUnknownEventType _
  | Connected
  | Timeout _
  | StreamIncomplete _ -> None
;;

let test_input_json_delta = function
  | ContentBlockDelta { index; delta = InputJsonDelta s } -> Some (index, s)
  | MessageStart _
  | ContentBlockStart _
  | ContentBlockDelta _
  | ContentBlockStop _
  | MessageDelta _
  | MessageStop
  | Ping
  | SSEError _
  | SSEParseFailed _
  | SSEUnknownEventType _
  | Connected
  | Timeout _
  | StreamIncomplete _ -> None
;;

let%test
    "openai_chunk_to_events: parallel tool calls sharing wire index:0 get distinct blocks"
  =
  (* minimax-m3 (Ollama Cloud) stamps every parallel tool call index:0 but gives
     each a distinct id. Blocks are keyed by id so the three calls do not collapse
     into one buffer, which previously concatenated their arguments into invalid
     JSON and failed the turn with malformed_tool_use_arguments. *)
  let mk id name args : openai_tool_call_delta =
    { tc_index = 0
    ; tc_id = Some id
    ; tc_name = Some name
    ; tc_arguments = Some (Args_fragment args)
    }
  in
  let state = create_openai_stream_state () in
  let chunk =
    { chunk_id = "c"
    ; chunk_model = "minimax-m3"
    ; delta_content = None
    ; delta_reasoning = None
    ; delta_reasoning_details = None
    ; delta_tool_calls =
        [ mk "a" "list_tasks" {|{"limit":5}|}
        ; mk "b" "run_shell" {|{"argv":["git"]}|}
        ; mk "c" "read_file" {|{"file_path":"x"}|}
        ]
    ; finish_reason = None
    ; chunk_usage = None
    ; chunk_parse_error = None
    }
  in
  let events, _ = openai_chunk_to_events state chunk in
  let starts = List.filter_map test_tool_use_start_with_name events in
  let deltas = List.filter_map test_input_json_delta events in
  starts = [ 0, Some "list_tasks"; 1, Some "run_shell"; 2, Some "read_file" ]
  && deltas = [ 0, {|{"limit":5}|}; 1, {|{"argv":["git"]}|}; 2, {|{"file_path":"x"}|} ]
;;

let%test "openai_chunk_to_events: id-less continuation fragment stays in its call's block"
  =
  (* OpenAI streams a tool call's id and name only on its first chunk, then sends
     argument fragments carrying just the index. An id-less continuation must
     route back to the block opened for that index, not open a new one. *)
  let base =
    { chunk_id = "c"
    ; chunk_model = "m"
    ; delta_content = None
    ; delta_reasoning = None
    ; delta_reasoning_details = None
    ; delta_tool_calls = []
    ; finish_reason = None
    ; chunk_usage = None
    ; chunk_parse_error = None
    }
  in
  let state = create_openai_stream_state () in
  let ev1, _ =
    openai_chunk_to_events
      state
      { base with
        delta_tool_calls =
          [ { tc_index = 0
            ; tc_id = Some "a"
            ; tc_name = Some "calc"
            ; tc_arguments = Some (Args_fragment {|{"x":|})
            }
          ]
      }
  in
  let ev2, _ =
    openai_chunk_to_events
      state
      { base with
        delta_tool_calls =
          [ { tc_index = 0
            ; tc_id = None
            ; tc_name = None
            ; tc_arguments = Some (Args_fragment {|1}|})
            }
          ]
      }
  in
  let starts = List.filter_map test_tool_use_start (ev1 @ ev2) in
  let deltas = List.filter_map test_input_json_delta (ev1 @ ev2) in
  List.length starts = 1 && deltas = [ 0, {|{"x":|}; 0, {|1}|} ]
;;

let%test
    "openai_chunk_to_events: gemma-style fragmented parallel calls keep distinct blocks"
  =
  (* gemma4-coder-fable5 (RunPod llama-server) emits parallel tool calls the
     spec-compliant way: distinct wire indices, id and name only on each call's
     first chunk, then argument fragments carrying just the index. Each call's
     fragments must accumulate into its own block (one Start per call, fragments
     routed back by index). Captured wire, 2026-06-30. *)
  let base =
    { chunk_id = "c"
    ; chunk_model = "gemma4-coder-fable5"
    ; delta_content = None
    ; delta_reasoning = None
    ; delta_reasoning_details = None
    ; delta_tool_calls = []
    ; finish_reason = None
    ; chunk_usage = None
    ; chunk_parse_error = None
    }
  in
  let tc tc_index tc_id tc_name args =
    { tc_index; tc_id; tc_name; tc_arguments = Some (Args_fragment args) }
  in
  let state = create_openai_stream_state () in
  let chunks =
    [ [ tc 0 (Some "a") (Some "list_tasks") {|{"li|} ]
    ; [ tc 0 None None {|mit":5}|} ]
    ; [ tc 1 (Some "b") (Some "run_shell") {|{"ar|} ]
    ; [ tc 1 None None {|gv":["git"]}|} ]
    ]
  in
  let events =
    List.concat_map
      (fun tcs -> fst (openai_chunk_to_events state { base with delta_tool_calls = tcs }))
      chunks
  in
  let starts =
    List.filter_map
      (function
        | ContentBlockStart { index; content_type = "tool_use"; tool_name; _ } ->
          Some (index, tool_name)
        | _ -> None)
      events
  in
  let deltas =
    List.filter_map
      (function
        | ContentBlockDelta { index; delta = InputJsonDelta s } -> Some (index, s)
        | _ -> None)
      events
  in
  starts = [ 0, Some "list_tasks"; 1, Some "run_shell" ]
  && deltas = [ 0, {|{"li|}; 0, {|mit":5}|}; 1, {|{"ar|}; 1, {|gv":["git"]}|} ]
;;

let%test "openai_chunk_to_events: id-less continuation on ambiguous index fails loud" =
  let base =
    { chunk_id = "c"
    ; chunk_model = "minimax-m3"
    ; delta_content = None
    ; delta_reasoning = None
    ; delta_reasoning_details = None
    ; delta_tool_calls = []
    ; finish_reason = None
    ; chunk_usage = None
    ; chunk_parse_error = None
    }
  in
  let tc tc_id args =
    { tc_index = 0; tc_id; tc_name = None; tc_arguments = Some (Args_fragment args) }
  in
  let state = create_openai_stream_state () in
  let _ =
    openai_chunk_to_events
      state
      { base with
        delta_tool_calls = [ tc (Some "a") {|{"a":|}; tc (Some "b") {|{"b":|} ]
      }
  in
  let events, _ =
    openai_chunk_to_events state { base with delta_tool_calls = [ tc None {|1}|} ] }
  in
  match events with
  | [ SSEParseFailed { raw; reason } ] ->
    raw = "openai tool_call index 0"
    && reason
       = "ambiguous_tool_call_index: id-less continuation follows multiple tool \
          identities"
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

(** {1 OpenAI Responses API SSE Streaming}

    Responses streaming uses item-level event names:
    - [response.output_text.delta] for assistant text
    - [response.reasoning_summary_text.delta] / [response.reasoning_text.delta]
      for reasoning summaries/text
    - [response.output_item.added] + [response.function_call_arguments.delta]
      for function calls
    - [response.completed] / [response.incomplete] / [response.failed] for
      terminal status and usage.

    Keep this separate from {!parse_openai_sse_chunk}: Responses has no
    [choices[].delta] envelope. *)

let responses_member_string_opt key json =
  let open Yojson.Safe.Util in
  json |> member key |> to_string_option
;;

let responses_member_int_default key json =
  let open Yojson.Safe.Util in
  json |> member key |> to_int_option |> Option.value ~default:0
;;

let responses_advance_next_block_index state index =
  if index >= state.next_block_index then state.next_block_index <- index + 1
;;

let responses_usage_of_response response =
  let open Yojson.Safe.Util in
  match response |> member "usage" with
  | `Assoc _ as usage ->
    let input_tokens = responses_member_int_default "input_tokens" usage in
    let output_tokens = responses_member_int_default "output_tokens" usage in
    let cache_read_input_tokens =
      match usage |> member "input_tokens_details" with
      | `Assoc _ as details -> responses_member_int_default "cached_tokens" details
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> 0
    in
    Some
      { input_tokens
      ; output_tokens
      ; cache_creation_input_tokens = 0
      ; cache_read_input_tokens
      ; cost_usd = None
      }
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
;;

let responses_output_has_function_call response =
  let open Yojson.Safe.Util in
  match response |> member "output" with
  | `List items ->
    List.exists
      (fun item ->
         match responses_member_string_opt "type" item with
         | Some ("function_call" | "custom_tool_call") -> true
         | Some _ | None -> false)
      items
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> false
;;

let responses_incomplete_reason_opt response =
  let open Yojson.Safe.Util in
  match response |> member "incomplete_details" with
  | `Assoc _ as details -> responses_member_string_opt "reason" details
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
;;

let responses_incomplete_reason response =
  responses_incomplete_reason_opt response |> Option.value ~default:"incomplete"
;;

let responses_failed_message response =
  let open Yojson.Safe.Util in
  match response |> member "error" with
  | `Assoc _ as err -> responses_member_string_opt "message" err
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
;;

let responses_stop_reason_of_response response =
  Responses_stop_reason.of_status
    ~status:(responses_member_string_opt "status" response)
    ~incomplete_reason:(responses_incomplete_reason_opt response)
    ~failed_message:(responses_failed_message response)
    ~has_tool_calls:(responses_output_has_function_call response)
;;

let responses_error_message json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Assoc _ as err ->
    responses_member_string_opt "message" err |> Option.value ~default:"Responses error"
  | `String message -> message
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ ->
    responses_member_string_opt "message" json |> Option.value ~default:"Responses error"
;;

let responses_error_type json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Assoc _ as err ->
    (match responses_member_string_opt "type" err with
     | Some _ as t -> t
     | None -> responses_member_string_opt "code" err)
  | `String _ | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `List _ ->
    responses_member_string_opt "code" json
;;

let responses_ensure_thinking_block state emit ~output_index =
  (match state.thinking_state with
   | Not_thinking -> state.thinking_state <- Thinking_started (Unix.gettimeofday ())
   | Thinking_started _ | Thinking_done -> ());
  if not state.thinking_block_started
  then (
    state.thinking_block_index <- output_index;
    emit
      (ContentBlockStart
         { index = output_index
         ; content_type = "thinking"
         ; tool_id = None
         ; tool_name = None
         });
    state.thinking_block_started <- true;
    responses_advance_next_block_index state output_index);
  state.thinking_block_index
;;

let responses_ensure_text_block state emit ~output_index =
  if not state.text_block_started
  then (
    state.text_block_index <- output_index;
    emit
      (ContentBlockStart
         { index = output_index; content_type = "text"; tool_id = None; tool_name = None });
    state.text_block_started <- true;
    responses_advance_next_block_index state output_index);
  state.text_block_index
;;

let responses_tool_id_of_item item = responses_member_string_opt "call_id" item

let responses_ensure_tool_block state emit ~output_index ~tool_id ~tool_name =
  let resolution, start =
    resolve_tool_block
      state
      ~protocol:"openai_responses"
      ~wire_index:output_index
      ~provider_tool_id:tool_id
      ~tool_name
      ~index_policy:Wire_index
      ~idless_semantics:Continue_by_wire_index
  in
  Option.iter emit start;
  match resolution with
  | Tool_block_resolved { block_index; _ } -> Some block_index
  | Tool_block_rejected error ->
    emit error;
    None
;;

let responses_emit_redacted_reasoning_outputs state emit response =
  let open Yojson.Safe.Util in
  match response |> member "output" with
  | `List items ->
    List.iteri
      (fun output_index item ->
         match responses_member_string_opt "type" item with
         | Some "reasoning" ->
           (match item |> member "encrypted_content" |> to_string_option with
            | Some encrypted_content when String.trim encrypted_content <> "" ->
              emit
                (ContentBlockStart
                   { index = output_index
                   ; content_type = "redacted_thinking"
                   ; tool_id = Some (Yojson.Safe.to_string item)
                   ; tool_name = None
                   });
              responses_advance_next_block_index state output_index
            | Some _ | None -> ())
         | Some _ | None -> ())
      items
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> ()
;;

let responses_sse_to_events (state : openai_stream_state) event_type data_str
  : sse_event list * Telemetry_event.t option
  =
  (* The Responses API delivers its terminal status via a [response.completed]
     event before this trailing sentinel, so [DONE] needs no synthesized
     terminal here -- the accumulator already has a stop_reason. *)
  if String.equal data_str openai_done_sentinel
  then [], None
  else (
    match Yojson.Safe.from_string data_str with
    | exception Yojson.Json_error msg ->
      [ SSEParseFailed { raw = data_str; reason = "json_error: " ^ msg } ], None
    | json ->
      let open Yojson.Safe.Util in
      let evt_type =
        match event_type with
        | Some t when String.trim t <> "" -> t
        | Some _ | None -> Cli_common_json.member_str "type" json
      in
      let events = ref [] in
      let emit evt = events := evt :: !events in
      let emit_terminal response =
        emit
          (MessageDelta
             { stop_reason = Some (responses_stop_reason_of_response response)
             ; usage = responses_usage_of_response response
             });
        emit MessageStop
      in
      (match evt_type with
       | "response.created" ->
         let response = json |> member "response" in
         emit
           (MessageStart
              { id = Cli_common_json.member_str "id" response
              ; model = Cli_common_json.member_str "model" response
              ; usage = responses_usage_of_response response
              })
       | "response.output_text.delta" ->
         (match responses_member_string_opt "delta" json with
          | Some delta when delta <> "" ->
            let output_index = responses_member_int_default "output_index" json in
            let index = responses_ensure_text_block state emit ~output_index in
            emit (ContentBlockDelta { index; delta = TextDelta delta })
          | Some _ | None -> ())
       | "response.output_text.done" ->
         if not state.text_block_started
         then (
           match responses_member_string_opt "text" json with
           | Some text when text <> "" ->
             let output_index = responses_member_int_default "output_index" json in
             let index = responses_ensure_text_block state emit ~output_index in
             emit (ContentBlockDelta { index; delta = TextDelta text })
           | Some _ | None -> ())
       | "response.reasoning_summary_text.delta" | "response.reasoning_text.delta" ->
         (match responses_member_string_opt "delta" json with
          | Some delta when delta <> "" ->
            let output_index = responses_member_int_default "output_index" json in
            let index = responses_ensure_thinking_block state emit ~output_index in
            emit (ContentBlockDelta { index; delta = ThinkingDelta delta })
          | Some _ | None -> ())
       | "response.reasoning_summary_text.done" | "response.reasoning_text.done" ->
         if not state.thinking_block_started
         then (
           match responses_member_string_opt "text" json with
           | Some text when text <> "" ->
             let output_index = responses_member_int_default "output_index" json in
             let index = responses_ensure_thinking_block state emit ~output_index in
             emit (ContentBlockDelta { index; delta = ThinkingDelta text })
           | Some _ | None -> ())
       | "response.output_item.added" ->
         let output_index = responses_member_int_default "output_index" json in
         let item = json |> member "item" in
         (match responses_member_string_opt "type" item with
          | Some ("function_call" | "custom_tool_call") ->
            let (_ : int option) =
              responses_ensure_tool_block
                state
                emit
                ~output_index
                ~tool_id:(responses_tool_id_of_item item)
                ~tool_name:(responses_member_string_opt "name" item)
            in
            ()
          | Some _ | None -> ())
       | "response.function_call_arguments.delta" ->
         (match responses_member_string_opt "delta" json with
          | Some delta when delta <> "" ->
            let output_index = responses_member_int_default "output_index" json in
            let index =
              responses_ensure_tool_block
                state
                emit
                ~output_index
                  (* [call_id] is the function-call identity on every official
                   arguments event. [item_id] identifies only the output item
                   and is never substituted for it. *)
                ~tool_id:(responses_member_string_opt "call_id" json)
                ~tool_name:None
            in
            (match index with
             | Some index ->
               emit (ContentBlockDelta { index; delta = InputJsonDelta delta })
             | None -> ())
          | Some _ | None -> ())
       | "response.completed" | "response.incomplete" ->
         let response = json |> member "response" in
         responses_emit_redacted_reasoning_outputs state emit response;
         (* A [response.incomplete] terminal means the turn was cut off, so any
            in-progress tool call is partial. Carry that to the accumulator
            (which drops partial tool blocks at finalize) for ANY incomplete
            reason — [stop_reason = MaxTokens] alone only covers
            max_output_tokens, not e.g. content_filter. (#2073 follow-up.) *)
         (match responses_member_string_opt "status" response with
          | Some "incomplete" ->
            emit (StreamIncomplete { reason = responses_incomplete_reason response })
          | Some _ | None -> ());
         emit_terminal response
       | "response.failed" | "error" ->
         emit
           (SSEError
              { message = responses_error_message json
              ; error_type = responses_error_type json
              ; raw = data_str
              })
       | "response.in_progress"
       | "response.output_item.done"
       | "response.content_part.added"
       | "response.content_part.done"
       | "response.function_call_arguments.done"
       | "response.reasoning_summary_part.added"
       | "response.reasoning_summary_part.done"
       | "response.refusal.delta"
       | "response.refusal.done"
       | "response.queued" -> ()
       | other -> emit (SSEUnknownEventType { event_type = other; raw = data_str }));
      List.rev !events, None)
;;

(** {1 Gemini SSE Streaming}

    Gemini [streamGenerateContent?alt=sse] emits SSE data lines with
    JSON payloads: [{candidates: [{content: {parts: [...]}}]}].
    Each chunk may contain text, thought, or functionCall parts. *)

type gemini_chunk =
  { gem_model : string
  ; gem_parts : Yojson.Safe.t list
  ; gem_finish_reason : string option
  ; gem_usage : api_usage option
  }

let parse_gemini_sse_chunk data_str : gemini_chunk option =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let gem_model = Cli_common_json.member_str "modelVersion" json in
    let candidate =
      match json |> member "candidates" with
      | `List (c :: _) -> c
      | `List [] -> `Assoc []
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ ->
        `Assoc []
    in
    let gem_parts =
      match candidate |> member "content" |> member "parts" with
      | `List ps -> ps
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> []
    in
    let gem_finish_reason = candidate |> member "finishReason" |> to_string_option in
    let gem_usage =
      let um = json |> member "usageMetadata" in
      if um = `Null
      then None
      else
        Some
          { input_tokens = Cli_common_json.member_int "promptTokenCount" um
          ; output_tokens = Cli_common_json.member_int "candidatesTokenCount" um
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens =
              Cli_common_json.member_int "cachedContentTokenCount" um
          ; cost_usd = None
          }
    in
    Some { gem_model; gem_parts; gem_finish_reason; gem_usage }
  with
  | Yojson.Safe.Util.Type_error _
  | Yojson.Safe.Util.Undefined _
  | Yojson.Json_error _
  | Invalid_argument _ -> None
;;

let gemini_chunk_to_events (state : openai_stream_state) (chunk : gemini_chunk)
  : sse_event list * Telemetry_event.t option
  =
  let open Yojson.Safe.Util in
  let events = ref [] in
  let emit evt = events := evt :: !events in
  let telemetry_event = ref None in
  let has_thought_part =
    List.exists
      (fun part ->
         Cli_common_json.member_bool "thought" part
         &&
         match part |> member "text" |> to_string_option with
         | Some text when text <> "" -> true
         | Some empty_text ->
           let (_ : string) = empty_text in
           false
         | None -> false)
      chunk.gem_parts
  in
  (match state.thinking_state, has_thought_part with
   | Not_thinking, true | Thinking_done, true ->
     state.thinking_state <- Thinking_started (Unix.gettimeofday ())
   | Thinking_started t0, false ->
     let thinking_duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
     state.thinking_state <- Thinking_done;
     telemetry_event
     := Some
          (Telemetry_event.Thinking_complete
             { provider = state.provider; model = state.model; thinking_duration_ms })
   | Not_thinking, false | Thinking_done, false | Thinking_started _, true -> ());
  List.iteri
    (fun part_index part ->
       let is_thought = Cli_common_json.member_bool "thought" part in
       let part_thought_signature = Backend_gemini.thought_signature_of_part part in
       let emit_signed_part ~target ~content_type ~delta thought_signature =
         (match target with
          | Backend_gemini.Gemini_text_part -> state.text_block_started <- false
          | Backend_gemini.Gemini_thought_part -> state.thinking_block_started <- false
          | Backend_gemini.Gemini_image_part
          | Backend_gemini.Gemini_audio_part
          | Backend_gemini.Gemini_document_part -> ());
         let carrier_index = state.next_block_index in
         let carrier_payload =
           Backend_gemini.gemini_part_thought_signature_payload ~target ~thought_signature
         in
         emit
           (ContentBlockStart
              { index = carrier_index
              ; content_type = "redacted_thinking"
              ; tool_id = Some carrier_payload
              ; tool_name = None
              });
         let content_index = carrier_index + 1 in
         emit
           (ContentBlockStart
              { index = content_index; content_type; tool_id = None; tool_name = None });
         emit (ContentBlockDelta { index = content_index; delta });
         state.next_block_index <- content_index + 1
       in
       let emit_function_call_delta () =
         match part |> member "functionCall" with
         | `Assoc _ as fc ->
           let name = Cli_common_json.member_str "name" fc in
           let args = fc |> member "args" in
           let provider_tool_id = fc |> member "id" |> to_string_option in
           let thought_signature = part_thought_signature in
           let resolution, start =
             resolve_tool_block
               state
               ~protocol:"gemini"
               ~wire_index:part_index
               ~provider_tool_id
               ~tool_name:(Some name)
               ~index_policy:
                 (match thought_signature with
                  | Some _ -> Next_after_carrier
                  | None -> Next_available)
               ~idless_semantics:Complete_occurrence
           in
           (match resolution with
            | Tool_block_rejected error -> emit error
            | Tool_block_resolved { block_index = idx; identity } ->
              (match start with
               | Some start ->
                 (match thought_signature with
                  | Some thought_signature ->
                    let payload =
                      Backend_gemini.gemini_thought_signature_payload
                        ~tool_use_id:identity.id
                        ~thought_signature
                    in
                    let carrier_index = idx - 1 in
                    emit
                      (ContentBlockStart
                         { index = carrier_index
                         ; content_type = "redacted_thinking"
                         ; tool_id = Some payload
                         ; tool_name = None
                         })
                  | None -> ());
                 emit start
               | None -> ());
              emit
                (ContentBlockDelta
                   { index = idx; delta = InputJsonSnapshot (Yojson.Safe.to_string args) }))
         | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> ()
       in
       let emit_textual_part text =
         match part_thought_signature with
         | Some thought_signature ->
           if is_thought
           then
             emit_signed_part
               ~target:Backend_gemini.Gemini_thought_part
               ~content_type:"thinking"
               ~delta:(ThinkingDelta text)
               thought_signature
           else
             emit_signed_part
               ~target:Backend_gemini.Gemini_text_part
               ~content_type:"text"
               ~delta:(TextDelta text)
               thought_signature
         | None when String.equal text "" -> ()
         | None ->
           if is_thought
           then (
             if not state.thinking_block_started
             then (
               state.thinking_block_index <- state.next_block_index;
               emit
                 (ContentBlockStart
                    { index = state.next_block_index
                    ; content_type = "thinking"
                    ; tool_id = None
                    ; tool_name = None
                    });
               state.thinking_block_started <- true;
               state.next_block_index <- state.next_block_index + 1);
             emit
               (ContentBlockDelta
                  { index = state.thinking_block_index; delta = ThinkingDelta text }))
           else (
             if not state.text_block_started
             then (
               state.text_block_index <- state.next_block_index;
               emit
                 (ContentBlockStart
                    { index = state.next_block_index
                    ; content_type = "text"
                    ; tool_id = None
                    ; tool_name = None
                    });
               state.text_block_started <- true;
               state.next_block_index <- state.next_block_index + 1);
             emit
               (ContentBlockDelta
                  { index = state.text_block_index; delta = TextDelta text }))
       in
       let emit_media_part inline_data =
         let target, block =
           Backend_gemini.media_content_block_of_inline_data inline_data
         in
         let content_type, delta =
           match block with
           | Image { media_type; source_type; data } ->
             "image", MediaDelta { media_type; source_type; data }
           | Audio { media_type; source_type; data } ->
             "audio", MediaDelta { media_type; source_type; data }
           | Document { media_type; source_type; data } ->
             "document", MediaDelta { media_type; source_type; data }
           | Text _
           | Thinking _
           | ReasoningDetails _
           | RedactedThinking _
           | ToolUse _
           | ToolResult _ ->
             raise
               (Backend_gemini.Gemini_api_error
                  "Gemini inlineData decoder produced a non-media content block")
         in
         match part_thought_signature with
         | Some thought_signature ->
           emit_signed_part ~target ~content_type ~delta thought_signature
         | None ->
           let index = state.next_block_index in
           emit
             (ContentBlockStart { index; content_type; tool_id = None; tool_name = None });
           emit (ContentBlockDelta { index; delta });
           state.next_block_index <- index + 1
       in
       let emit_non_text_part () =
         match part |> member "functionCall" with
         | `Assoc _ -> emit_function_call_delta ()
         | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
           (match part |> member "inlineData" with
            | `Assoc _ as inline_data -> emit_media_part inline_data
            | `Null ->
              (match part_thought_signature with
               | Some _ ->
                 raise
                   (Backend_gemini.Gemini_api_error
                      "Gemini thoughtSignature is attached to an unsupported Part")
               | None -> ())
            | _ ->
              raise
                (Backend_gemini.Gemini_api_error "Gemini inlineData must be an object"))
       in
       match part |> member "text" |> to_string_option with
       | Some text when not (String.equal text "") -> emit_textual_part text
       | Some empty_text ->
         (match part |> member "functionCall" with
          | `Assoc _ -> emit_function_call_delta ()
          | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
            (match part |> member "inlineData" with
             | `Assoc _ as inline_data -> emit_media_part inline_data
             | `Null -> emit_textual_part empty_text
             | _ ->
               raise
                 (Backend_gemini.Gemini_api_error "Gemini inlineData must be an object")))
       | None -> emit_non_text_part ())
    chunk.gem_parts;
  (* Finish reason *)
  (match chunk.gem_finish_reason with
   | Some reason ->
     let stop_reason =
       (* Gemini wire vocabulary -> stop_reason. Kept in sync with the
          non-streaming parser (Backend_gemini.parse_response): SAFETY and
          RECITATION both surface as Refusal. *)
       match String.uppercase_ascii reason with
       | "STOP" when Hashtbl.length state.tool_block_indices > 0 -> StopToolUse
       | "STOP" -> EndTurn
       | "MAX_TOKENS" -> MaxTokens
       | "SAFETY" | "RECITATION" -> Refusal
       | other -> Unknown other
     in
     emit (MessageDelta { stop_reason = Some stop_reason; usage = chunk.gem_usage })
   | None -> ());
  List.rev !events, !telemetry_event
;;

(** {1 Ollama NDJSON Streaming}

    Ollama [/api/chat] with [stream:true] emits one JSON object per
    line. Non-final lines carry a [message.content] delta; the final
    line has [done:true] together with [done_reason] and the four
    timing fields ([prompt_eval_count] / [prompt_eval_duration] /
    [eval_count] / [eval_duration]) that the Openai compat path on
    [/v1/chat/completions] strips out. *)

type ollama_tool_call_delta =
  { oll_tc_index : int
  ; oll_tc_id : string option
  ; oll_tc_name : string option
  ; oll_tc_arguments : tool_call_arguments option
  }

type ollama_chunk =
  { oll_model : string
  ; oll_delta_content : string option
  ; oll_delta_thinking : string option
  ; oll_tool_calls : ollama_tool_call_delta list
  ; oll_done_reason : string option
  ; oll_is_done : bool
  ; oll_usage : api_usage option
  ; oll_timings : inference_timings option
  }

let parse_ollama_ndjson_chunk data_str : ollama_chunk option =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let oll_model = Cli_common_json.member_str "model" json in
    let oll_is_done = Cli_common_json.member_bool "done" json in
    let oll_done_reason = json |> member "done_reason" |> to_string_option in
    let message = json |> member "message" in
    let oll_delta_content =
      match message with
      | `Assoc _ ->
        let s = message |> member "content" |> to_string_option in
        (match s with
         | Some "" -> None
         | other -> other)
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
    in
    let oll_delta_thinking =
      match message with
      | `Assoc _ ->
        let s = message |> member "thinking" |> to_string_option in
        (match s with
         | Some "" -> None
         | other -> other)
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> None
    in
    let oll_tool_calls =
      match message with
      | `Assoc _ ->
        (match message |> member "tool_calls" with
         | `List items ->
           List.mapi
             (fun idx tc ->
                let func = tc |> member "function" in
                let oll_tc_name = func |> member "name" |> to_string_option in
                let oll_tc_id = tc |> member "id" |> to_string_option in
                let oll_tc_arguments =
                  match func |> member "arguments" with
                  | `Null -> None
                  | (`Assoc _ | `List _) as v ->
                    Some (Args_complete (Yojson.Safe.to_string v))
                  | `String s -> Some (Args_fragment s)
                  | other -> Some (Args_complete (Yojson.Safe.to_string other))
                in
                { oll_tc_index = idx; oll_tc_id; oll_tc_name; oll_tc_arguments })
             items
         | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Assoc _ -> [])
      | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> []
    in
    (* Token-count usage. Ollama only emits these on the done chunk.
       When both counts are zero we return None, matching the
       non-streaming path in [Backend_ollama.parse_ollama_response]
       so that a downstream consumer's usage-trust classification sees
       [Usage_missing] instead of [zero_token_usage_reported]. *)
    let oll_usage =
      let input = json |> member "prompt_eval_count" |> to_int_option in
      let output = json |> member "eval_count" |> to_int_option in
      match input, output with
      | None, None -> None
      | Some _, None | None, Some _ | Some _, Some _ ->
        let input_tokens = Option.value ~default:0 input in
        let output_tokens = Option.value ~default:0 output in
        if input_tokens = 0 && output_tokens = 0
        then None
        else
          Some
            { input_tokens
            ; output_tokens
            ; cache_creation_input_tokens = 0
            ; cache_read_input_tokens = 0
            ; cost_usd = None
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
        Option.is_some prompt_n
        || Option.is_some prompt_ns
        || Option.is_some predicted_n
        || Option.is_some predicted_ns
      in
      if not any_set
      then None
      else (
        let ms_of_ns ns_opt = Option.map (fun ns -> float_of_int ns /. 1e6) ns_opt in
        let per_second n_opt ns_opt =
          match n_opt, ns_opt with
          | Some n, Some ns when ns > 0 ->
            Some (float_of_int n /. (float_of_int ns /. 1e9))
          | Some _, Some _ | Some _, None | None, Some _ | None, None -> None
        in
        Some
          { prompt_n
          ; prompt_ms = ms_of_ns prompt_ns
          ; prompt_per_second = per_second prompt_n prompt_ns
          ; predicted_n
          ; predicted_ms = ms_of_ns predicted_ns
          ; predicted_per_second = per_second predicted_n predicted_ns
          ; cache_n = None
          })
    in
    Some
      { oll_model
      ; oll_delta_content
      ; oll_delta_thinking
      ; oll_tool_calls
      ; oll_done_reason
      ; oll_is_done
      ; oll_usage
      ; oll_timings
      }
  with
  | Yojson.Json_error _ -> None
  | Type_error (_, _) -> None
;;

(** Convert a parsed {!ollama_chunk} into {!sse_event} list.
    Reuses {!openai_stream_state} for block index tracking. *)
let ollama_chunk_to_events (state : openai_stream_state) (chunk : ollama_chunk)
  : sse_event list * Telemetry_event.t option
  =
  let events = ref [] in
  let emit evt = events := evt :: !events in
  let telemetry_event = ref None in
  (* Thinking content delta *)
  (match chunk.oll_delta_thinking with
   | Some text when text <> "" ->
     (match state.thinking_state with
      | Not_thinking -> state.thinking_state <- Thinking_started (Unix.gettimeofday ())
      | Thinking_started _ | Thinking_done -> ());
     if not state.thinking_block_started
     then (
       state.thinking_block_index <- state.next_block_index;
       emit
         (ContentBlockStart
            { index = state.next_block_index
            ; content_type = "thinking"
            ; tool_id = None
            ; tool_name = None
            });
       state.thinking_block_started <- true;
       state.next_block_index <- state.next_block_index + 1);
     emit
       (ContentBlockDelta
          { index = state.thinking_block_index; delta = ThinkingDelta text })
   | Some empty_thinking ->
     let (_ : string) = empty_thinking in
     (match state.thinking_state with
      | Thinking_started t0 ->
        let thinking_duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        state.thinking_state <- Thinking_done;
        telemetry_event
        := Some
             (Telemetry_event.Thinking_complete
                { provider = state.provider; model = state.model; thinking_duration_ms })
      | Not_thinking | Thinking_done -> ())
   | None ->
     (match state.thinking_state with
      | Thinking_started t0 ->
        let thinking_duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
        state.thinking_state <- Thinking_done;
        telemetry_event
        := Some
             (Telemetry_event.Thinking_complete
                { provider = state.provider; model = state.model; thinking_duration_ms })
      | Not_thinking | Thinking_done -> ()));
  (* Text content delta *)
  (match chunk.oll_delta_content with
   | Some text when text <> "" ->
     if not state.text_block_started
     then (
       state.text_block_index <- state.next_block_index;
       emit
         (ContentBlockStart
            { index = state.next_block_index
            ; content_type = "text"
            ; tool_id = None
            ; tool_name = None
            });
       state.text_block_started <- true;
       state.next_block_index <- state.next_block_index + 1);
     emit (ContentBlockDelta { index = state.text_block_index; delta = TextDelta text })
   | Some empty_text ->
     let (_ : string) = empty_text in
     ()
   | None -> ());
  (* Tool calls. Ollama typically emits these complete in the done
     chunk rather than incrementally. We still keyed-cache by the
     per-tool index so a server that DID stream them incrementally
     would get correct accumulation behaviour. *)
  List.iter
    (fun (tc : ollama_tool_call_delta) ->
       let block_idx =
         resolve_tool_block
           state
           ~protocol:"ollama"
           ~wire_index:tc.oll_tc_index
           ~provider_tool_id:tc.oll_tc_id
           ~tool_name:tc.oll_tc_name
           ~index_policy:Next_available
           ~idless_semantics:Continue_by_wire_index
       in
       let resolution, start = block_idx in
       Option.iter emit start;
       match resolution, tc.oll_tc_arguments with
       | Tool_block_resolved { block_index; _ }, Some (Args_fragment args) when args <> ""
         -> emit (ContentBlockDelta { index = block_index; delta = InputJsonDelta args })
       | Tool_block_resolved { block_index; _ }, Some (Args_complete args) when args <> ""
         ->
         emit (ContentBlockDelta { index = block_index; delta = InputJsonSnapshot args })
       | Tool_block_resolved _, Some (Args_fragment _ | Args_complete _)
       | Tool_block_resolved _, None -> ()
       | Tool_block_rejected error, _ -> emit error)
    chunk.oll_tool_calls;
  (* Terminal chunk: emit MessageDelta with stop_reason + usage. *)
  if chunk.oll_is_done
  then (
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
    emit (MessageDelta { stop_reason; usage = chunk.oll_usage }));
  List.rev !events, !telemetry_event
;;

[@@@coverage off]
(* ── parse_ollama_ndjson_chunk tests ──────────────────────── *)

let%test "parse_ollama_ndjson_chunk: content delta line" =
  let line =
    {|{"model":"dashscope-3:8b","message":{"role":"assistant","content":"hi"},"done":false}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c ->
    c.oll_model = "dashscope-3:8b"
    && c.oll_delta_content = Some "hi"
    && c.oll_delta_thinking = None
    && c.oll_tool_calls = []
    && (not c.oll_is_done)
    && c.oll_usage = None
    && c.oll_timings = None
;;

let%test "parse_ollama_ndjson_chunk: done line carries timings + usage" =
  let line =
    {|{"model":"dashscope-3:8b","message":{"role":"assistant","content":""},
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
    &&
      (match c.oll_timings with
      | Some t ->
        t.predicted_n = Some 50
        && t.prompt_n = Some 15
        && (match t.predicted_per_second with
            | Some v -> abs_float (v -. 50.0) < 0.001
            | None -> false)
        &&
          (match t.prompt_per_second with
          | Some v -> abs_float (v -. 50.0) < 0.001
          | None -> false)
      | None -> false)
;;

let%test "parse_ollama_ndjson_chunk: zero eval_duration → per_second None" =
  let line =
    {|{"model":"dashscope-3:8b","message":{"role":"assistant","content":""},
       "done":true,"eval_count":10,"eval_duration":0}|}
  in
  match parse_ollama_ndjson_chunk line with
  | Some c ->
    (match c.oll_timings with
     | Some t -> t.predicted_n = Some 10 && t.predicted_per_second = None
     | None -> false)
  | None -> false
;;

let%test "parse_ollama_ndjson_chunk: tool_calls fully formed in done line" =
  let line =
    {|{"model":"dashscope-3:8b","message":{"role":"assistant","content":"",
       "tool_calls":[{"function":{"name":"foo","arguments":{"x":1}}}]},
       "done":true,"done_reason":"tool_calls"}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c ->
    (match c.oll_tool_calls with
     | [ tc ] ->
       tc.oll_tc_name = Some "foo"
       &&
         (match tc.oll_tc_arguments with
         | Some (Args_fragment args | Args_complete args) ->
           let json = Yojson.Safe.from_string args in
           json |> Yojson.Safe.Util.member "x" |> Yojson.Safe.Util.to_int = 1
         | None -> false)
     | unexpected_tool_calls ->
       let (_ : ollama_tool_call_delta list) = unexpected_tool_calls in
       false)
;;

let%test "parse_ollama_ndjson_chunk: malformed json → None" =
  parse_ollama_ndjson_chunk "{not valid" = None
;;

let%test "parse_ollama_ndjson_chunk: done with zero token counts → None" =
  let line =
    {|{"model":"dashscope-3:8b","message":{"role":"assistant","content":""},
       "done_reason":"stop","done":true,
       "prompt_eval_count":0,"eval_count":0}|}
  in
  match parse_ollama_ndjson_chunk line with
  | None -> false
  | Some c -> c.oll_is_done && c.oll_usage = None
;;

(* ── ollama_chunk_to_events tests ─────────────────────────── *)

let%test "ollama_chunk_to_events: content delta emits Start+Delta" =
  let state = create_openai_stream_state () in
  let chunk =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = Some "hello"
    ; oll_delta_thinking = None
    ; oll_tool_calls = []
    ; oll_done_reason = None
    ; oll_is_done = false
    ; oll_usage = None
    ; oll_timings = None
    }
  in
  let events, _tel = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart { index = 0; content_type = "text"; _ }
    ; ContentBlockDelta { index = 0; delta = TextDelta "hello" }
    ] -> true
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

let%test "ollama_chunk_to_events: subsequent content delta reuses block" =
  let state = create_openai_stream_state () in
  let mk text =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = Some text
    ; oll_delta_thinking = None
    ; oll_tool_calls = []
    ; oll_done_reason = None
    ; oll_is_done = false
    ; oll_usage = None
    ; oll_timings = None
    }
  in
  let _ = ollama_chunk_to_events state (mk "he") in
  let events, _tel = ollama_chunk_to_events state (mk "llo") in
  (* Second chunk: only Delta, no new Start *)
  match events with
  | [ ContentBlockDelta { index = 0; delta = TextDelta "llo" } ] -> true
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

let%test "ollama_chunk_to_events: done with stop_reason emits MessageDelta" =
  let state = create_openai_stream_state () in
  let chunk =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = None
    ; oll_delta_thinking = None
    ; oll_tool_calls = []
    ; oll_done_reason = Some "stop"
    ; oll_is_done = true
    ; oll_usage =
        Some
          { input_tokens = 10
          ; output_tokens = 20
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    ; oll_timings = None
    }
  in
  let events, _tel = ollama_chunk_to_events state chunk in
  match events with
  | [ MessageDelta { stop_reason = Some EndTurn; usage = Some u } ] ->
    u.input_tokens = 10 && u.output_tokens = 20
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

let%test "ollama_chunk_to_events: done with zero usage → usage=None" =
  let state = create_openai_stream_state () in
  let chunk =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = None
    ; oll_delta_thinking = None
    ; oll_tool_calls = []
    ; oll_done_reason = Some "stop"
    ; oll_is_done = true
    ; oll_usage = None
    ; oll_timings = None
    }
  in
  let events, _tel = ollama_chunk_to_events state chunk in
  match events with
  | [ MessageDelta { stop_reason = Some EndTurn; usage = None } ] -> true
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

let%test "ollama_chunk_to_events: tool_calls emit Start+InputJsonSnapshot" =
  let state = create_openai_stream_state () in
  let chunk =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = None
    ; oll_delta_thinking = None
    ; oll_tool_calls =
        [ { oll_tc_index = 0
          ; oll_tc_id = None
          ; oll_tc_name = Some "search"
          ; oll_tc_arguments = Some (Args_complete {|{"q":"hello"}|})
          }
        ]
    ; oll_done_reason = Some "tool_calls"
    ; oll_is_done = true
    ; oll_usage = None
    ; oll_timings = None
    }
  in
  let events, _tel = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart
        { index = 0
        ; content_type = "tool_use"
        ; tool_id = Some tool_id
        ; tool_name = Some "search"
        }
    ; ContentBlockDelta { index = 0; delta = InputJsonSnapshot args }
    ; MessageDelta { stop_reason = Some StopToolUse; _ }
    ] -> String.starts_with ~prefix:"call_oas_" tool_id && args = {|{"q":"hello"}|}
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

let%test "ollama_chunk_to_events: thinking delta emits thinking block first" =
  let state = create_openai_stream_state () in
  let chunk =
    { oll_model = "dashscope-3:8b"
    ; oll_delta_content = None
    ; oll_delta_thinking = Some "considering"
    ; oll_tool_calls = []
    ; oll_done_reason = None
    ; oll_is_done = false
    ; oll_usage = None
    ; oll_timings = None
    }
  in
  let events, _tel = ollama_chunk_to_events state chunk in
  match events with
  | [ ContentBlockStart { index = 0; content_type = "thinking"; _ }
    ; ContentBlockDelta { index = 0; delta = ThinkingDelta "considering" }
    ] -> true
  | unexpected_events ->
    let (_ : sse_event list) = unexpected_events in
    false
;;

(* parse_sse_event regression: the previous implementation returned [None]
   for both unknown event types and JSON parse failures, silently dropping
   the chunk. Downstream consumers then proceeded as if the chunk were a
   harmless heartbeat (like Ping), eventually finalizing without seeing
   MessageStop and presenting a phantom completion. The accumulator could
   not distinguish "no event in this chunk" from "we lost data here".
   These regression tests pin the new contract: the parser surfaces a
   structured failure event so the accumulator can react. *)

let%test "parse_sse_event: unknown event type yields SSEUnknownEventType" =
  let raw = "{\"type\":\"future_event_v3\"}" in
  match parse_sse_event None raw with
  | Some (SSEUnknownEventType { event_type; raw = r }) ->
    event_type = "future_event_v3" && r = raw
  | unexpected_event ->
    let (_ : sse_event option) = unexpected_event in
    false
;;

let%test "parse_sse_event: malformed JSON yields SSEParseFailed" =
  let raw = "{not valid json" in
  match parse_sse_event None raw with
  | Some (SSEParseFailed { raw = r; reason }) -> r = raw && String.length reason > 0
  | unexpected_event ->
    let (_ : sse_event option) = unexpected_event in
    false
;;

let%test
    "parse_sse_event: type field missing yields SSEUnknownEventType (not silent None)"
  =
  (* The [type] field is absent and event_type is unspecified.
     Cli_common_json.member_str returns "" rather than raising, so the
     match falls into the unknown-type branch. The contract is that we
     emit *something* the consumer can react to; what matters here is
     that we never return [None] for this input. *)
  let raw = "{\"id\":\"foo\"}" in
  match parse_sse_event None raw with
  | Some (SSEUnknownEventType { event_type = ""; _ }) -> true
  | Some (SSEParseFailed _) -> true
  | unexpected_event ->
    let (_ : sse_event option) = unexpected_event in
    false
;;

let%test "parse_sse_event: known event still parses normally" =
  (* Sanity: refactor must not regress the happy path. *)
  let raw = "{\"type\":\"message_stop\"}" in
  match parse_sse_event None raw with
  | Some MessageStop -> true
  | unexpected_event ->
    let (_ : sse_event option) = unexpected_event in
    false
;;
