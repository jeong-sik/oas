(** OpenAI-compatible request serialization.

    Converts agent_sdk Types (content blocks, messages, tools) into
    Openai Chat Completions API JSON format.

    @since 0.92.0 extracted from Backend_openai *)

open Types

let unsupported_media_source ~backend ~block source_type =
  invalid_arg
    (Printf.sprintf
       "%s does not support %s media source kind %s"
       backend
       block
       (Types.media_source_kind_to_string source_type))
;;

let base64_data_url ~backend ~block ~media_type ~data = function
  | Base64 -> Printf.sprintf "data:%s;base64,%s" media_type data
  | (Url | File_id) as source_type -> unsupported_media_source ~backend ~block source_type
;;

let base64_audio_data ~backend ~block ~data = function
  | Base64 -> data
  | (Url | File_id) as source_type -> unsupported_media_source ~backend ~block source_type
;;

let tool_calls_to_openai_json blocks =
  blocks
  |> List.filter_map (function
    | ToolUse { id; name; input } ->
      Some
        (`Assoc
            [ "id", `String id
            ; "type", `String "function"
            ; ( "function"
              , `Assoc
                  [ "name", `String name
                  ; "arguments", `String (Yojson.Safe.to_string input)
                  ] )
            ])
    | Text _
    | Thinking _
    | RedactedThinking _
    | ToolResult _
    | Image _
    | Document _
    | Audio _ -> None)
;;

(** Ollama variant: arguments as raw JSON object, not string.
    Ollama's yyjson parser treats a stringified object as literal text
    and fails with "can't find closing '}' symbol" on subsequent turns. *)
let tool_calls_to_ollama_json blocks =
  blocks
  |> List.filter_map (function
    | ToolUse { id; name; input } ->
      Some
        (`Assoc
            [ "id", `String id
            ; "type", `String "function"
            ; "function", `Assoc [ "name", `String name; "arguments", input ]
            ])
    | Text _
    | Thinking _
    | RedactedThinking _
    | ToolResult _
    | Image _
    | Document _
    | Audio _ -> None)
;;

let openai_content_parts_of_blocks blocks =
  blocks
  |> List.filter_map (function
    | Text s ->
      Some (`Assoc [ "type", `String "text"; "text", `String (Utf8_sanitize.sanitize s) ])
    | Image { media_type; data; source_type } ->
      let url =
        base64_data_url
          ~backend:"openai_chat"
          ~block:"image"
          ~media_type
          ~data
          source_type
      in
      Some
        (`Assoc
            [ "type", `String "image_url"; "image_url", `Assoc [ "url", `String url ] ])
    | Document { media_type; data; source_type } ->
      let url =
        base64_data_url
          ~backend:"openai_chat"
          ~block:"document"
          ~media_type
          ~data
          source_type
      in
      Some
        (`Assoc
            [ "type", `String "image_url"; "image_url", `Assoc [ "url", `String url ] ])
    | Audio { media_type; data; source_type } ->
      let data =
        base64_audio_data ~backend:"openai_chat" ~block:"audio" ~data source_type
      in
      Some
        (`Assoc
            [ "type", `String "input_audio"
            ; "input_audio", `Assoc [ "data", `String data; "format", `String media_type ]
            ])
    | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> None)
;;

let assistant_text_content_of_blocks blocks =
  blocks
  |> List.filter_map (function
    | Text s -> Some (Utf8_sanitize.sanitize s)
    | Thinking _
    | RedactedThinking _
    | ToolUse _
    | ToolResult _
    | Image _
    | Document _
    | Audio _ -> None)
  |> String.concat "\n"
;;

let assistant_reasoning_content_of_blocks blocks =
  blocks
  |> List.filter_map (function
    | Thinking { content; _ } when not (Api_common.string_is_blank content) ->
      Some (Utf8_sanitize.sanitize content)
    | Thinking _ -> None
    | Text _
    | RedactedThinking _
    | ToolUse _
    | ToolResult _
    | Image _
    | Document _
    | Audio _ -> None)
  |> String.concat ""
;;

let assistant_reasoning_details_of_blocks blocks =
  blocks
  |> List.find_map (function
    | RedactedThinking data -> Api_common.openai_chat_reasoning_details_of_redacted data
    | Text _ | Thinking _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ ->
      None)
;;

let openai_tool_message_of_result ~tool_use_id ~content ~content_blocks =
  let content_str =
    match content_blocks with
    | Some blocks ->
      (* OpenAI tool messages accept string content only; encode structured
         blocks as a JSON string so the result is not lost. *)
      Yojson.Safe.to_string (`List (List.map Api_common.content_block_to_json blocks))
    | None -> Utf8_sanitize.sanitize content
  in
  `Assoc
    [ "role", `String "tool"
    ; "tool_call_id", `String tool_use_id
    ; "content", `String content_str
    ]
;;

let openai_tool_messages_of_blocks blocks =
  blocks
  |> List.filter_map (function
    | ToolResult { tool_use_id; content; content_blocks; _ } ->
      Some (openai_tool_message_of_result ~tool_use_id ~content ~content_blocks)
    | Text _
    | Thinking _
    | RedactedThinking _
    | ToolUse _
    | Image _
    | Document _
    | Audio _ -> None)
;;

let messages_of_message_with
      ?(tool_calls_fn = tool_calls_to_openai_json)
      ?(include_reasoning_content = false)
      ?(reasoning_output_wire = Reasoning_dialect.No_output_control)
      ?(assistant_tool_content_format = Capability_vocab.Assistant_tool_content_null)
      ?(modality_priority = Modality.Preserve_input_order)
      (msg : message)
  : Yojson.Safe.t list
  =
  match msg.role with
  | User ->
    (* Apply modality reordering policy before flattening into JSON parts.
       For [Preserve_input_order] (default) this is a no-op; for
       [Visual_first] image/audio/document blocks move ahead of text.
       has_multimodal inspects the input list (pre-reorder) — the boolean
       is invariant under reordering, so either input is correct. *)
    let ordered_content = Modality.reorder modality_priority msg.content in
    let content_parts = openai_content_parts_of_blocks ordered_content in
    let has_multimodal =
      List.exists
        (function
          | Image _ | Document _ | Audio _ -> true
          | Text _ | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> false)
        msg.content
    in
    let user_msgs =
      if content_parts = []
      then []
      else if has_multimodal
      then [ `Assoc [ "role", `String "user"; "content", `List content_parts ] ]
      else (
        let text_content = Api_common.text_blocks_to_string msg.content in
        [ `Assoc [ "role", `String "user"; "content", `String text_content ] ])
    in
    let tool_msgs = openai_tool_messages_of_blocks msg.content in
    (* Legacy compatibility: older histories may pack ToolResult blocks and
       user text into one role:User message. Normal pipeline output records
       ToolResult blocks on role:Tool; this split keeps persisted mixed
       messages wire-compatible without making the mixed shape the invariant. *)
    tool_msgs @ user_msgs
  | Assistant ->
    let text_content = assistant_text_content_of_blocks msg.content in
    let reasoning_content =
      if include_reasoning_content
      then assistant_reasoning_content_of_blocks msg.content
      else ""
    in
    let reasoning_details =
      if include_reasoning_content
      then (
        match reasoning_output_wire with
        | Reasoning_dialect.Reasoning_split ->
          assistant_reasoning_details_of_blocks msg.content
        | Reasoning_dialect.No_output_control -> None)
      else None
    in
    let tool_calls = tool_calls_fn msg.content in
    let assistant_content =
      if Api_common.string_is_blank text_content && tool_calls <> []
      then (
        match assistant_tool_content_format with
        | Capability_vocab.Assistant_tool_content_null -> `Null
        | Capability_vocab.Assistant_tool_content_empty_string -> `String text_content)
      else `String text_content
    in
    let fields = [ "role", `String "assistant"; "content", assistant_content ] in
    let fields =
      match reasoning_details with
      | Some details -> ("reasoning_details", `List details) :: fields
      | None
        when include_reasoning_content
             && not (Api_common.string_is_blank reasoning_content) ->
        ("reasoning_content", `String reasoning_content) :: fields
      | None -> fields
    in
    let fields =
      if
        include_reasoning_content
        && reasoning_details <> None
        && not (Api_common.string_is_blank reasoning_content)
      then ("reasoning_content", `String reasoning_content) :: fields
      else fields
    in
    let fields =
      if tool_calls = [] then fields else ("tool_calls", `List tool_calls) :: fields
    in
    [ `Assoc fields ]
  | System ->
    let text = Api_common.text_blocks_to_string msg.content in
    [ `Assoc [ "role", `String "system"; "content", `String text ] ]
  | Tool ->
    msg.content
    |> openai_tool_messages_of_blocks
    |> (function
     | [] ->
       let text = Api_common.text_blocks_to_string msg.content in
       [ `Assoc [ "role", `String "user"; "content", `String text ] ]
     | tool_msgs -> tool_msgs)
;;

let openai_messages_of_message msg =
  messages_of_message_with ~tool_calls_fn:tool_calls_to_openai_json msg
;;

let glm_messages_of_message msg =
  messages_of_message_with
    ~tool_calls_fn:tool_calls_to_openai_json
    ~include_reasoning_content:true
    ~assistant_tool_content_format:Capability_vocab.Assistant_tool_content_empty_string
    msg
;;

let dialect_messages_of_message
      ?(assistant_tool_content_format = Capability_vocab.Assistant_tool_content_null)
      dialect
      (msg : Types.message)
  =
  let tool_calls = tool_calls_to_openai_json msg.content in
  let include_reasoning_content =
    Reasoning_dialect.should_replay_reasoning
      dialect
      ~assistant_had_tool_call:(tool_calls <> [])
  in
  messages_of_message_with
    ~tool_calls_fn:(fun _ -> tool_calls)
    ~include_reasoning_content
    ~reasoning_output_wire:dialect.Reasoning_dialect.output_wire
    ~assistant_tool_content_format
    msg
;;

let modality_priority_for_model_id model_id =
  match Capabilities.for_model_id model_id with
  | Some c -> c.modality_priority
  | None -> Modality.Preserve_input_order
;;

(** Ollama native [/api/chat] user message serialization.
    Unlike OpenAI-compatible endpoints where [content] may be a string or an
    array of content parts, Ollama's native chat API requires [content] to be a
    plain string and carries image payloads in a separate [images] array of
    base64-encoded strings. Audio is not supported by the native endpoint and
    fails closed instead of being silently dropped.

    Returns [None] when the message carries no representable user content (e.g.
    an orphaned-tool-result message), so callers do not emit an empty
    [content:""] placeholder on the wire. *)
let ollama_native_user_message ~modality_priority content : Yojson.Safe.t option =
  let ordered_content = Modality.reorder modality_priority content in
  let text_parts, images =
    List.fold_left
      (fun (texts, images) block ->
         match block with
         | Text s -> Utf8_sanitize.sanitize s :: texts, images
         | Image { data; source_type = Base64; _ }
         | Document { data; source_type = Base64; _ } ->
           (* Ollama native /api/chat accepts base64 image payloads in the
              images field. Document blocks are forwarded the same way so
              vision models can attempt to process them as pages. *)
           texts, data :: images
         | Image { source_type; _ } ->
           unsupported_media_source ~backend:"ollama_native" ~block:"image" source_type
         | Document { source_type; _ } ->
           unsupported_media_source ~backend:"ollama_native" ~block:"document" source_type
         | Audio { source_type; _ } ->
           unsupported_media_source ~backend:"ollama_native" ~block:"audio" source_type
         | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> texts, images)
      ([], [])
      ordered_content
  in
  let text_content =
    match List.rev text_parts with
    | [] -> ""
    | parts -> String.concat "\n" parts
  in
  match List.rev images with
  | [] when text_content = "" -> None
  | [] -> Some (`Assoc [ "role", `String "user"; "content", `String text_content ])
  | imgs ->
    Some
      (`Assoc
          [ "role", `String "user"
          ; "content", `String text_content
          ; "images", `List (List.map (fun img -> `String img) imgs)
          ])
;;

let ollama_messages_of_message ?(model_id = "") msg =
  let modality_priority = modality_priority_for_model_id model_id in
  match msg.role with
  | User ->
    (* Native /api/chat: content must be a string; images go in images array. *)
    let user_msg = ollama_native_user_message ~modality_priority msg.content in
    let tool_msgs = openai_tool_messages_of_blocks msg.content in
    (match user_msg with
     | None -> tool_msgs
     | Some m -> tool_msgs @ [ m ])
  | System | Assistant | Tool ->
    messages_of_message_with
      ~tool_calls_fn:tool_calls_to_ollama_json
      ~modality_priority
      msg
;;

(** Strip ToolResult blocks that are outside the immediate result span
    following their Assistant ToolUse message. Occurs after context
    compaction drops or reorders a ToolUse while the corresponding
    ToolResult survives.

    Provider request builders call {!close_tool_message_pairs_for_request} so
    OpenAI-compatible, Anthropic, Gemini, and Ollama paths share the same
    outbound history invariant.

    Pure function — no I/O, no mutation. *)
let strip_orphaned_tool_results = Tool_message_pairs.strip_orphaned_tool_results

let close_tool_message_pairs_for_request = Tool_message_pairs.close_for_provider_request

(** Strip Thinking blocks from all messages.

    Some OpenAI-compatible providers emit [reasoning_content] in
    responses but do not accept it in request messages. DeepSeek is an
    exception for tool-call turns, and Qwen/DashScope can opt into replay
    with [preserve_thinking]. Callers that need provider-specific replay
    should use {!dialect_messages_of_message}; this helper remains a blunt
    compatibility strip.

    Pure function — no I/O, no mutation. *)
let strip_thinking_blocks (messages : message list) : message list =
  List.map
    (fun (msg : message) ->
       let filtered =
         List.filter
           (function
             | Thinking _ -> false
             | Text _
             | RedactedThinking _
             | ToolUse _
             | ToolResult _
             | Image _
             | Document _
             | Audio _ -> true)
           msg.content
       in
       if List.length filtered = List.length msg.content
       then msg
       else { msg with content = filtered })
    messages
;;

let tool_choice_to_openai_json = function
  | Auto -> `String "auto"
  | Any -> `String "required"
  | Tool name ->
    `Assoc [ "type", `String "function"; "function", `Assoc [ "name", `String name ] ]
  | None_ -> `String "none"
;;

(* Single source of truth for the OpenAI-compatible parallel-tool-call control.
   Two layers decide *whether* to disable parallel calls (low-level
   [Backend_openai_request] guards on [tools <> []]; agent-state-aware
   [Api_openai] guards on [capabilities.supports_tools]); both serialize the
   field through here so the wire shape lives in one place. OpenAI defaults to
   parallel calls, so the field is emitted only to turn them off. *)
let parallel_tool_calls_fields ~disable_parallel ~tools_present
  : (string * Yojson.Safe.t) list
  =
  if disable_parallel && tools_present then [ "parallel_tool_calls", `Bool false ] else []
;;

let legacy_parameters_to_json_schema params =
  let properties, required =
    List.fold_left
      (fun (props_acc, req_acc) param ->
         match param with
         | `Assoc fields ->
           let name =
             match List.assoc_opt "name" fields with
             | Some (`String s) -> s
             | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
             | None -> ""
           in
           if name = ""
           then props_acc, req_acc
           else (
             let description =
               match List.assoc_opt "description" fields with
               | Some (`String s) -> s
               | Some
                   (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
               | None -> ""
             in
             let type_name =
               match List.assoc_opt "param_type" fields with
               | Some (`String s) -> s
               | Some
                   (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
               | None ->
                 (match List.assoc_opt "type" fields with
                  | Some (`String s) -> s
                  | Some
                      ( `Assoc _
                      | `List _
                      | `Int _
                      | `Intlit _
                      | `Float _
                      | `Bool _
                      | `Null )
                  | None -> "string")
             in
             let prop =
               `Assoc [ "type", `String type_name; "description", `String description ]
             in
             let req_acc =
               match List.assoc_opt "required" fields with
               | Some (`Bool true) -> `String name :: req_acc
               | Some
                   ( `Assoc _ | `List _ | `String _ | `Int _ | `Intlit _ | `Float _
                   | `Bool false
                   | `Null )
               | None -> req_acc
             in
             (name, prop) :: props_acc, req_acc)
         | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
           props_acc, req_acc)
      ([], [])
      params
  in
  `Assoc
    [ "type", `String "object"
    ; "properties", `Assoc (List.rev properties)
    ; "required", `List (List.rev required)
    ]
;;

let build_openai_tool_json = function
  | `Assoc fields ->
    let name =
      match List.assoc_opt "name" fields with
      | Some (`String s) -> s
      | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
        -> "tool"
    in
    let description =
      match List.assoc_opt "description" fields with
      | Some (`String s) -> s
      | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
        -> ""
    in
    let parameters =
      match List.assoc_opt "input_schema" fields with
      | Some schema -> schema
      | None ->
        (match List.assoc_opt "parameters" fields with
         | Some (`List params) -> legacy_parameters_to_json_schema params
         | Some schema -> schema
         | None -> `Assoc [])
    in
    (* Per-function strict mode (OpenAI / DeepSeek Beta / Kimi / MiMo): forward
       it into the function object only when the tool carried [strict], so a
       tool without it keeps the provider default. *)
    let strict_field =
      match List.assoc_opt "strict" fields with
      | Some (`Bool b) -> [ "strict", `Bool b ]
      | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Null)
      | None -> []
    in
    `Assoc
      [ "type", `String "function"
      ; ( "function"
        , `Assoc
            ([ "name", `String name
             ; "description", `String description
             ; "parameters", parameters
             ]
             @ strict_field) )
      ]
  | other -> other
;;
