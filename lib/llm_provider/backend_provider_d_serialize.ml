(** Provider_d-compatible request serialization.

    Converts agent_sdk Types (content blocks, messages, tools) into
    Provider_d Chat Completions API JSON format.

    @since 0.92.0 extracted from Backend_provider_d *)

open Types

let tool_calls_to_provider_d_json blocks =
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

let provider_d_content_parts_of_blocks blocks =
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

let messages_of_message_with
      ?(tool_calls_fn = tool_calls_to_provider_d_json)
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
    let content_parts = provider_d_content_parts_of_blocks ordered_content in
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
    let tool_msgs =
      msg.content
      |> List.filter_map (function
        | ToolResult { tool_use_id; content; _ } ->
          Some
            (`Assoc
                [ "role", `String "tool"
                ; "tool_call_id", `String tool_use_id
                ; "content", `String (Utf8_sanitize.sanitize content)
                ])
        | Text _
        | Thinking _
        | RedactedThinking _
        | ToolUse _
        | Image _
        | Document _
        | Audio _ -> None)
    in
    user_msgs @ tool_msgs
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
    |> List.filter_map (function
      | ToolResult { tool_use_id; content; _ } ->
        Some
          (`Assoc
              [ "role", `String "tool"
              ; "tool_call_id", `String tool_use_id
              ; "content", `String (Utf8_sanitize.sanitize content)
              ])
      | Text _
      | Thinking _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    |> (function
     | [] ->
       let text = Api_common.text_blocks_to_string msg.content in
       [ `Assoc [ "role", `String "user"; "content", `String text ] ]
     | tool_msgs -> tool_msgs)
;;

let provider_d_messages_of_message msg =
  messages_of_message_with ~tool_calls_fn:tool_calls_to_provider_d_json msg
;;

let provider_k_messages_of_message msg =
  messages_of_message_with
    ~tool_calls_fn:tool_calls_to_provider_d_json
    ~include_reasoning_content:true
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

    Provider_d-compatible APIs reject orphaned tool_call_ids; the Anthropic
    API has its own dangling-tool-call repair, so this is Provider_d-path only.

    Pure function — no I/O, no mutation. *)
let strip_orphaned_tool_results (messages : message list) : message list =
  let tool_use_ids (msg : message) =
    List.filter_map
      (function
        | ToolUse { id; _ } -> Some id
        | Text _
        | Thinking _
        | RedactedThinking _
        | ToolResult _
        | Image _
        | Document _
        | Audio _ -> None)
      msg.content
  in
  let tool_result_ids (msg : message) =
    List.filter_map
      (function
        | ToolResult { tool_use_id; _ } -> Some tool_use_id
        | Text _
        | Thinking _
        | RedactedThinking _
        | ToolUse _
        | Image _
        | Document _
        | Audio _ -> None)
      msg.content
  in
  let has_tool_result msg = tool_result_ids msg <> [] in
  let split_tool_result_span messages =
    let rec loop span = function
      | msg :: rest when has_tool_result msg -> loop (msg :: span) rest
      | rest -> List.rev span, rest
    in
    loop [] messages
  in
  let filter_tool_results allowed seen (msg : message) =
    let seen_ref = ref seen in
    let content =
      List.filter
        (function
          | ToolResult { tool_use_id; _ } ->
            let keep =
              List.mem tool_use_id allowed && not (List.mem tool_use_id !seen_ref)
            in
            if keep then seen_ref := tool_use_id :: !seen_ref;
            keep
          | Text _
          | Thinking _
          | RedactedThinking _
          | ToolUse _
          | Image _
          | Document _
          | Audio _ -> true)
        msg.content
    in
    let msg = if content = [] then None else Some { msg with content } in
    msg, !seen_ref
  in
  let filter_result_span allowed span =
    let filtered, _seen =
      List.fold_left
        (fun (acc, seen) msg ->
           let msg, seen = filter_tool_results allowed seen msg in
           match msg with
           | Some msg -> msg :: acc, seen
           | None -> acc, seen)
        ([], [])
        span
    in
    List.rev filtered
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let use_ids = if msg.role = Assistant then tool_use_ids msg else [] in
      if use_ids = []
      then (
        let msg, _seen = filter_tool_results [] [] msg in
        let acc =
          match msg with
          | Some msg -> msg :: acc
          | None -> acc
        in
        aux acc rest)
      else (
        let span, tail = split_tool_result_span rest in
        let filtered_span = filter_result_span use_ids span in
        aux (List.rev_append filtered_span (msg :: acc)) tail)
  in
  aux [] messages
;;

(** Strip Thinking blocks from all messages.
    Provider_g-compatible APIs reject [reasoning_content] in request
    messages — it is response-only. Occurs before serialization so
    theThinking blocks do not leak into the wire format.

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

let tool_choice_to_provider_d_json = function
  | Auto -> `String "auto"
  | Any -> `String "required"
  | Tool name ->
    `Assoc [ "type", `String "function"; "function", `Assoc [ "name", `String name ] ]
  | None_ -> `String "none"
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

let build_provider_d_tool_json = function
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
    `Assoc
      [ "type", `String "function"
      ; ( "function"
        , `Assoc
            [ "name", `String name
            ; "description", `String description
            ; "parameters", parameters
            ] )
      ]
  | other -> other
;;
