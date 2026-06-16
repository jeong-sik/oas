(** OpenAI-compatible request serialization.

    Converts agent_sdk Types (content blocks, messages, tools) into
    Openai Chat Completions API JSON format.

    @since 0.92.0 extracted from Backend_openai *)

open Types

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
    | Image { media_type; data; source_type = _ } ->
      Some
        (`Assoc
            [ "type", `String "image_url"
            ; ( "image_url"
              , `Assoc
                  [ "url", `String (Printf.sprintf "data:%s;base64,%s" media_type data) ]
              )
            ])
    | Document { media_type; data; source_type = _ } ->
      Some
        (`Assoc
            [ "type", `String "image_url"
            ; ( "image_url"
              , `Assoc
                  [ "url", `String (Printf.sprintf "data:%s;base64,%s" media_type data) ]
              )
            ])
    | Audio { media_type; data; source_type = _ } ->
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
    let tool_calls = tool_calls_fn msg.content in
    let fields =
      [ "role", `String "assistant"
      ; (if include_reasoning_content
         then "content", `String text_content
         else if Api_common.string_is_blank text_content && tool_calls <> []
         then "content", `Null
         else "content", `String text_content)
      ]
    in
    let fields =
      if include_reasoning_content && not (Api_common.string_is_blank reasoning_content)
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
    msg
;;

let dialect_messages_of_message dialect (msg : Types.message) =
  let tool_calls = tool_calls_to_openai_json msg.content in
  let include_reasoning_content =
    Reasoning_dialect.should_replay_reasoning
      dialect
      ~assistant_had_tool_call:(tool_calls <> [])
  in
  messages_of_message_with
    ~tool_calls_fn:(fun _ -> tool_calls)
    ~include_reasoning_content
    msg
;;

let modality_priority_for_model_id model_id =
  match Capabilities.for_model_id model_id with
  | Some c -> c.modality_priority
  | None -> Modality.Preserve_input_order
;;

let ollama_messages_of_message ?(model_id = "") msg =
  messages_of_message_with
    ~tool_calls_fn:tool_calls_to_ollama_json
    ~modality_priority:(modality_priority_for_model_id model_id)
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
