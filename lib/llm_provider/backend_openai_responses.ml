(** OpenAI Responses API codec.

    Responses is item-based: assistant messages, reasoning summaries, function
    calls, and function-call outputs are all ordered items. Keep this separate
    from the Chat Completions codec so [choices[].message] assumptions cannot
    leak into Responses request/response handling. *)

open Types

let json_assoc_opt key = function
  | `Assoc fields -> List.assoc_opt key fields
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let opt_bind opt f =
  match opt with
  | Some value -> f value
  | None -> None
;;

let json_string_opt = function
  | `String s -> Some s
  | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let json_int_opt = function
  | `Int n -> Some n
  | `Intlit s -> int_of_string_opt s
  | `Float _ | `String _ | `Assoc _ | `List _ | `Bool _ | `Null -> None
;;

let non_blank_json_string json =
  match json_string_opt json with
  | Some s when not (Api_common.string_is_blank s) -> Some s
  | Some _ | None -> None
;;

let assoc_remove keys fields =
  List.filter (fun (key, _) -> not (List.mem key keys)) fields
;;

let responses_raw_reasoning_item_of_redacted data =
  try
    match Yojson.Safe.from_string data with
    | `Assoc fields as json ->
      (match List.assoc_opt "type" fields with
       | Some (`String "reasoning") -> Some json
       | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
       | Some (`String _)
       | None -> None)
    | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
  with
  | Yojson.Json_error _ -> None
;;

let message_has_responses_raw_reasoning (msg : message) =
  List.exists
    (function
      | RedactedThinking data ->
        Option.is_some (responses_raw_reasoning_item_of_redacted data)
      | Text _ | Thinking _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ ->
        false)
    msg.content
;;

let responses_tool_json tool =
  match Backend_openai_serialize.build_openai_tool_json tool with
  | `Assoc fields ->
    (match List.assoc_opt "function" fields with
     | Some (`Assoc fn_fields) ->
       let passthrough =
         assoc_remove [ "name"; "description"; "parameters"; "strict" ] fn_fields
       in
       let name =
         match List.assoc_opt "name" fn_fields with
         | Some (`String s) -> s
         | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
         | None -> "tool"
       in
       let description =
         match List.assoc_opt "description" fn_fields with
         | Some (`String s) -> s
         | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
         | None -> ""
       in
       let parameters =
         match List.assoc_opt "parameters" fn_fields with
         | Some schema -> schema
         | None -> `Assoc []
       in
       let strict_field =
         match List.assoc_opt "strict" fn_fields with
         | Some (`Bool b) -> [ "strict", `Bool b ]
         | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `String _ | `Null)
         | None -> []
       in
       `Assoc
         ([ "type", `String "function"
          ; "name", `String name
          ; "description", `String description
          ; "parameters", parameters
          ]
          @ strict_field
          @ passthrough)
     | Some (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
       -> `Assoc fields)
  | other -> other
;;

let content_string_of_tool_result ~content ~content_blocks =
  match content_blocks with
  | Some blocks ->
    Yojson.Safe.to_string (`List (List.map Api_common.content_block_to_json blocks))
  | None -> Utf8_sanitize.sanitize content
;;

let unsupported_media_source ~block source_type =
  invalid_arg
    (Printf.sprintf
       "openai_responses does not support %s media source kind %s"
       block
       (Types.media_source_kind_to_string source_type))
;;

let base64_data_url ~block ~media_type ~data = function
  | Base64 -> Printf.sprintf "data:%s;base64,%s" media_type data
  | (Url | File_id) as source_type -> unsupported_media_source ~block source_type
;;

let base64_audio_data ~block ~data = function
  | Base64 -> data
  | (Url | File_id) as source_type -> unsupported_media_source ~block source_type
;;

let input_content_part_of_block = function
  | Text s ->
    Some
      (`Assoc [ "type", `String "input_text"; "text", `String (Utf8_sanitize.sanitize s) ])
  | Image { media_type; data; source_type } ->
    let image_url = base64_data_url ~block:"image" ~media_type ~data source_type in
    Some (`Assoc [ "type", `String "input_image"; "image_url", `String image_url ])
  | Document { media_type; data; source_type } ->
    let file_data = base64_data_url ~block:"document" ~media_type ~data source_type in
    Some (`Assoc [ "type", `String "input_file"; "file_data", `String file_data ])
  | Audio { media_type; data; source_type } ->
    let data = base64_audio_data ~block:"audio" ~data source_type in
    Some
      (`Assoc
          [ "type", `String "input_audio"
          ; "input_audio", `Assoc [ "data", `String data; "format", `String media_type ]
          ])
  | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> None
;;

let output_text_content_part text =
  `Assoc [ "type", `String "output_text"; "text", `String (Utf8_sanitize.sanitize text) ]
;;

let message_item ~role content_blocks =
  match List.filter_map input_content_part_of_block content_blocks with
  | [] ->
    let text = Api_common.text_blocks_to_string content_blocks in
    if Api_common.string_is_blank text
    then []
    else
      [ `Assoc [ "role", `String role; "content", `String (Utf8_sanitize.sanitize text) ]
      ]
  | parts -> [ `Assoc [ "role", `String role; "content", `List parts ] ]
;;

let assistant_output_item_of_block = function
  | Thinking { content; _ } when not (Api_common.string_is_blank content) ->
    Some
      (`Assoc
          [ "type", `String "reasoning"
          ; ( "summary"
            , `List [ `Assoc [ "type", `String "summary_text"; "text", `String content ] ]
            )
          ])
  | Text s when not (Api_common.string_is_blank s) ->
    Some
      (`Assoc
          [ "type", `String "message"
          ; "role", `String "assistant"
          ; "content", `List [ output_text_content_part s ]
          ])
  | RedactedThinking data -> responses_raw_reasoning_item_of_redacted data
  | ToolUse { id; name; input } ->
    Some
      (`Assoc
          [ "type", `String "function_call"
          ; "call_id", `String id
          ; "name", `String name
          ; "arguments", `String (Yojson.Safe.to_string input)
          ])
  | Thinking _ | Text _ | ToolResult _ | Image _ | Document _ | Audio _ -> None
;;

let function_call_output_item ~tool_use_id ~content ~content_blocks =
  `Assoc
    [ "type", `String "function_call_output"
    ; "call_id", `String tool_use_id
    ; "output", `String (content_string_of_tool_result ~content ~content_blocks)
    ]
;;

let tool_result_items blocks =
  blocks
  |> List.filter_map (function
    | ToolResult { tool_use_id; content; content_blocks; _ } ->
      Some (function_call_output_item ~tool_use_id ~content ~content_blocks)
    | Text _
    | Thinking _
    | RedactedThinking _
    | ToolUse _
    | Image _
    | Document _
    | Audio _ -> None)
;;

let input_items_of_message (msg : message) =
  match msg.role with
  | System -> message_item ~role:"system" msg.content
  | User ->
    let tool_results = tool_result_items msg.content in
    let non_tool_content =
      List.filter
        (function
          | ToolResult _ -> false
          | Text _
          | Thinking _
          | RedactedThinking _
          | ToolUse _
          | Image _
          | Document _
          | Audio _ -> true)
        msg.content
    in
    tool_results @ message_item ~role:"user" non_tool_content
  | Assistant -> List.filter_map assistant_output_item_of_block msg.content
  | Tool -> tool_result_items msg.content
;;

let tool_choice_to_responses_json = function
  | Auto -> Some (`String "auto")
  | Any -> Some (`String "required")
  | None_ -> Some (`String "none")
  | Tool name -> Some (`Assoc [ "type", `String "function"; "name", `String name ])
;;

let responses_json_schema_payload schema =
  match Backend_openai_request.openai_json_schema_payload schema with
  | `Assoc fields ->
    let schema_fields =
      assoc_remove [ "description" ] fields
      @
      match List.assoc_opt "description" fields with
      | Some (`String _ as description) -> [ "description", description ]
      | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
        -> []
    in
    `Assoc (("type", `String "json_schema") :: schema_fields)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
    `Assoc
      [ "type", `String "json_schema"
      ; "name", `String (Provider_config.structured_output_name_of_schema schema)
      ; "schema", schema
      ; "strict", `Bool true
      ]
;;

let response_text_config_of_config (config : Provider_config.t) =
  match Backend_openai_request.structured_schema_of_config config with
  | Some schema -> Some (`Assoc [ "format", responses_json_schema_payload schema ])
  | None when config.response_format = JsonMode ->
    Some (`Assoc [ "format", `Assoc [ "type", `String "json_object" ] ])
  | None -> None
;;

let capabilities_of_config = Backend_openai_request.capabilities_of_config

let effective_max_output_tokens (config : Provider_config.t) =
  let caps = capabilities_of_config config in
  match config.max_tokens, caps.max_output_tokens with
  | None, Some cap -> cap
  | None, None -> Constants.resolve_unknown_model_max_tokens_fallback ()
  | Some n, Some cap when n > cap -> cap
  | Some n, _ -> n
;;

let add_sampling_field dialect (config : Provider_config.t) field value body =
  if
    Reasoning_dialect.ignores_sampling_param
      dialect
      ~enable_thinking:config.enable_thinking
      field
  then body
  else (field, value) :: body
;;

let build_request
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  let tools = Backend_openai_request.effective_tools config tools in
  let sanitized_messages =
    Backend_openai_serialize.close_tool_message_pairs_for_request messages
  in
  let dialect = Reasoning_dialect.for_provider_config config in
  let reasoning_effort =
    Backend_openai_request.normalized_reasoning_effort dialect config
  in
  let body =
    [ "model", `String config.model_id
    ; "input", `List (List.concat_map input_items_of_message sanitized_messages)
    ; "max_output_tokens", `Int (effective_max_output_tokens config)
    ]
  in
  let body =
    match config.system_prompt with
    | Some s when not (Api_common.string_is_blank s) ->
      ("instructions", `String (Utf8_sanitize.sanitize s)) :: body
    | Some _ | None -> body
  in
  let body =
    match config.temperature with
    | Some t -> add_sampling_field dialect config "temperature" (`Float t) body
    | None -> body
  in
  let body =
    match config.top_p with
    | Some p -> add_sampling_field dialect config "top_p" (`Float p) body
    | None -> body
  in
  let body =
    match response_text_config_of_config config with
    | Some text -> ("text", text) :: body
    | None -> body
  in
  let body =
    match reasoning_effort with
    | Some effort -> ("reasoning", `Assoc [ "effort", `String effort ]) :: body
    | None -> body
  in
  let body =
    if
      Option.is_some reasoning_effort
      || List.exists message_has_responses_raw_reasoning sanitized_messages
    then ("include", `List [ `String "reasoning.encrypted_content" ]) :: body
    else body
  in
  let body =
    match config.tool_choice, Backend_openai_request.effective_tool_choice config with
    | Some choice, Some _ ->
      (match tool_choice_to_responses_json choice with
       | Some choice -> ("tool_choice", choice) :: body
       | None -> body)
    | Some _, None | None, _ -> body
  in
  let body =
    match tools with
    | [] -> body
    | ts -> ("tools", `List (List.map responses_tool_json ts)) :: body
  in
  let body =
    let tools_present = tools <> [] in
    let caps = capabilities_of_config config in
    let disable_parallel =
      Capabilities.effective_disable_parallel_tool_use
        ~caller_disabled:config.disable_parallel_tool_use
        ~supports_parallel_tool_calls:caps.supports_parallel_tool_calls
        ~tools_present
    in
    Backend_openai_serialize.parallel_tool_calls_fields ~disable_parallel ~tools_present
    @ body
  in
  let body = if stream then ("stream", `Bool true) :: body else body in
  Yojson.Safe.to_string (`Assoc body)
;;

let output_text_of_content = function
  | `Assoc fields ->
    (match List.assoc_opt "type" fields, List.assoc_opt "text" fields with
     | Some (`String ("output_text" | "text")), Some (`String text) -> Some text
     | _, _ -> None)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let content_blocks_of_output_message item =
  match json_assoc_opt "content" item with
  | Some (`List content) ->
    content |> List.filter_map output_text_of_content |> List.map (fun text -> Text text)
  | Some (`String text) when not (Api_common.string_is_blank text) -> [ Text text ]
  | Some (`String _)
  | Some (`Assoc _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
  | None -> []
;;

let reasoning_text_of_summary_item = function
  | `Assoc fields ->
    (match List.assoc_opt "type" fields, List.assoc_opt "text" fields with
     | Some (`String ("summary_text" | "text")), Some (`String text) -> Some text
     | _, _ -> None)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let content_blocks_of_reasoning_item item =
  match opt_bind (json_assoc_opt "encrypted_content" item) non_blank_json_string with
  | Some _ -> [ RedactedThinking (Yojson.Safe.to_string item) ]
  | None ->
    (match json_assoc_opt "summary" item with
     | Some (`List summary) ->
       let content =
         summary |> List.filter_map reasoning_text_of_summary_item |> String.concat "\n"
       in
       if Api_common.string_is_blank content
       then []
       else [ Thinking { thinking_type = "reasoning_summary"; content } ]
     | Some (`String text) when not (Api_common.string_is_blank text) ->
       [ Thinking { thinking_type = "reasoning_summary"; content = text } ]
     | Some (`String _)
     | Some (`Assoc _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
     | None -> [])
;;

let content_block_of_function_call item =
  let call_id =
    match opt_bind (json_assoc_opt "call_id" item) json_string_opt with
    | Some s -> s
    | None ->
      (match opt_bind (json_assoc_opt "id" item) json_string_opt with
       | Some s -> s
       | None -> "")
  in
  let name =
    opt_bind (json_assoc_opt "name" item) json_string_opt |> Option.value ~default:""
  in
  let arguments =
    opt_bind (json_assoc_opt "arguments" item) json_string_opt
    |> Option.value ~default:"{}"
  in
  if Api_common.string_is_blank call_id || Api_common.string_is_blank name
  then None
  else
    Some
      (ToolUse { id = call_id; name; input = Api_common.json_of_string_or_raw arguments })
;;

let content_blocks_of_output_item item =
  match opt_bind (json_assoc_opt "type" item) json_string_opt with
  | Some "message" -> content_blocks_of_output_message item
  | Some "reasoning" -> content_blocks_of_reasoning_item item
  | Some "function_call" ->
    (match content_block_of_function_call item with
     | Some block -> [ block ]
     | None -> [])
  | Some _ | None -> []
;;

let usage_of_response_json json =
  match json_assoc_opt "usage" json with
  | Some (`Assoc fields) ->
    let member_int key = opt_bind (List.assoc_opt key fields) json_int_opt in
    let input_tokens = member_int "input_tokens" |> Option.value ~default:0 in
    let output_tokens = member_int "output_tokens" |> Option.value ~default:0 in
    let cached_tokens =
      match List.assoc_opt "input_tokens_details" fields with
      | Some (`Assoc detail_fields) ->
        opt_bind (List.assoc_opt "cached_tokens" detail_fields) json_int_opt
        |> Option.value ~default:0
      | Some (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
      | None -> 0
    in
    Some
      { input_tokens
      ; output_tokens
      ; cache_creation_input_tokens = 0
      ; cache_read_input_tokens = cached_tokens
      ; cost_usd = None
      }
  | Some (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None ->
    None
;;

let telemetry_of_response_json json =
  match json_assoc_opt "usage" json with
  | Some (`Assoc fields) ->
    let reasoning_tokens =
      match List.assoc_opt "output_tokens_details" fields with
      | Some (`Assoc detail_fields) ->
        opt_bind (List.assoc_opt "reasoning_tokens" detail_fields) json_int_opt
      | Some (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
      | None -> None
    in
    Some { Types.default_inference_telemetry with reasoning_tokens }
  | Some (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None ->
    None
;;

let stop_reason_of_response_json ~has_tool_calls json =
  let status =
    opt_bind (json_assoc_opt "status" json) json_string_opt |> Option.value ~default:""
  in
  (* A terminal [incomplete]/[failed] response status wins over tool-use
     detection: a Responses result can carry a [function_call] item while the
     generation was actually cut off (status="incomplete", e.g. max output
     tokens). Returning [StopToolUse] there would execute partial/invalid tool
     arguments instead of surfacing MaxTokens/an incomplete response. So we
     match the cut-off statuses first, then fall back to tool-use, then the
     normal completion path. (Codex P2, #2048.) *)
  match String.lowercase_ascii status with
  | "incomplete" ->
    let reason =
      match json_assoc_opt "incomplete_details" json with
      | Some details ->
        opt_bind (json_assoc_opt "reason" details) json_string_opt
        |> Option.value ~default:"incomplete"
      | None -> "incomplete"
    in
    if String.equal reason "max_output_tokens" then MaxTokens else Unknown reason
  | "failed" ->
    (match
       match json_assoc_opt "error" json with
       | Some error -> json_assoc_opt "message" error
       | None -> None
     with
     | Some (`String message) -> Unknown message
     | Some (`Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None
       -> Unknown status)
  | _ when has_tool_calls -> StopToolUse
  | "completed" | "" -> EndTurn
  | other -> Unknown other
;;

let parse_response_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    match json_assoc_opt "error" json with
    | Some (`Assoc fields) ->
      let message =
        opt_bind (List.assoc_opt "message" fields) json_string_opt
        |> Option.value ~default:"Unknown API error"
      in
      Error message
    | Some (`String message) -> Error message
    | Some (`List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) | None ->
      let output =
        match json_assoc_opt "output" json with
        | Some (`List items) -> items
        | Some (`Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null)
        | None -> []
      in
      let content = List.concat_map content_blocks_of_output_item output in
      (* Terminal [incomplete]/[failed] responses may carry a partial [function_call]
         item; do not expose it as a dangling ToolUse that the pipeline would try to
         execute or repair. Drop such blocks so the stop reason dominates. *)
      let status =
        opt_bind (json_assoc_opt "status" json) json_string_opt
        |> Option.value ~default:""
      in
      let content =
        match String.lowercase_ascii status with
        | "incomplete" | "failed" ->
          List.filter
            (function
              | ToolUse _ -> false
              | _ -> true)
            content
        | _ -> content
      in
      let has_tool_calls =
        List.exists
          (function
            | ToolUse _ -> true
            | Text _
            | Thinking _
            | RedactedThinking _
            | ToolResult _
            | Image _
            | Document _
            | Audio _ -> false)
          content
      in
      Ok
        { id =
            opt_bind (json_assoc_opt "id" json) json_string_opt
            |> Option.value ~default:""
        ; model =
            opt_bind (json_assoc_opt "model" json) json_string_opt
            |> Option.value ~default:""
        ; stop_reason = stop_reason_of_response_json ~has_tool_calls json
        ; content
        ; usage = usage_of_response_json json
        ; telemetry = telemetry_of_response_json json
        }
  with
  | Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)
;;
