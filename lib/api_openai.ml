(** OpenAI-compatible API request building and response parsing *)

open Types

let tool_calls_to_openai_json blocks =
  blocks
  |> List.filter_map (function
         | ToolUse (id, name, input) ->
             Some
               (`Assoc
                  [
                    ("id", `String id);
                    ("type", `String "function");
                    ( "function",
                      `Assoc
                        [
                          ("name", `String name);
                          ("arguments", `String (Yojson.Safe.to_string input));
                        ] );
                  ])
         | _ -> None)

let openai_content_parts_of_blocks blocks =
  blocks
  |> List.filter_map (function
         | Text s ->
             Some (`Assoc [("type", `String "text"); ("text", `String s)])
         | Image { media_type; data; source_type = _ } ->
             Some (`Assoc [
               ("type", `String "image_url");
               ("image_url", `Assoc [
                 ("url", `String (Printf.sprintf "data:%s;base64,%s" media_type data))
               ])
             ])
         | Document { media_type; data; source_type = _ } ->
             Some (`Assoc [
               ("type", `String "image_url");
               ("image_url", `Assoc [
                 ("url", `String (Printf.sprintf "data:%s;base64,%s" media_type data))
               ])
             ])
         | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> None)

let openai_messages_of_message (msg : message) : Yojson.Safe.t list =
  match msg.role with
  | User ->
      let content_parts = openai_content_parts_of_blocks msg.content in
      let has_multimodal =
        List.exists (function Image _ | Document _ -> true | _ -> false) msg.content
      in
      let user_msgs =
        if content_parts = [] then []
        else if has_multimodal then
          [ `Assoc [
              ("role", `String "user");
              ("content", `List content_parts);
            ] ]
        else
          let text_content = Api_common.text_blocks_to_string msg.content in
          [ `Assoc [
              ("role", `String "user");
              ("content", `String text_content);
            ] ]
      in
      let tool_msgs =
        msg.content
        |> List.filter_map (function
               | ToolResult (tool_use_id, content, _is_error) ->
                   Some
                     (`Assoc
                        [
                          ("role", `String "tool");
                          ("tool_call_id", `String tool_use_id);
                          ("content", `String content);
                        ])
               | _ -> None)
      in
      user_msgs @ tool_msgs
  | Assistant ->
      let text_content = Api_common.text_blocks_to_string msg.content in
      let tool_calls = tool_calls_to_openai_json msg.content in
      let fields =
        [
          ("role", `String "assistant");
          ( if Api_common.string_is_blank text_content && tool_calls <> [] then
              ("content", `Null)
            else
              ("content", `String text_content) );
        ]
      in
      let fields =
        if tool_calls = [] then fields else ("tool_calls", `List tool_calls) :: fields
      in
      [ `Assoc fields ]

let system_message_json (config : agent_state) : Yojson.Safe.t list =
  match config.config.system_prompt with
  | Some s when not (Api_common.string_is_blank s) ->
      [ `Assoc [("role", `String "system"); ("content", `String s)] ]
  | _ -> []

let tool_choice_to_openai_json = function
  | Auto -> `String "auto"
  | Any -> `String "required"
  | Tool name ->
      `Assoc
        [
          ("type", `String "function");
          ("function", `Assoc [("name", `String name)]);
        ]

let build_openai_tool_json = function
  | `Assoc fields ->
      let name =
        match List.assoc_opt "name" fields with
        | Some (`String s) -> s
        | _ -> "tool"
      in
      let description =
        match List.assoc_opt "description" fields with
        | Some (`String s) -> s
        | _ -> ""
      in
      let parameters =
        match List.assoc_opt "input_schema" fields with
        | Some schema -> schema
        | None ->
            (match List.assoc_opt "parameters" fields with
             | Some schema -> schema
             | None -> `Assoc [])
      in
      `Assoc
        [
          ("type", `String "function");
          ( "function",
            `Assoc
              [
                ("name", `String name);
                ("description", `String description);
                ("parameters", parameters);
              ] );
        ]
  | other -> other

let build_openai_body ~config ~messages ?tools () =
  let model_str = model_to_string config.config.model in
  let provider_messages =
    system_message_json config
    @ List.concat_map openai_messages_of_message messages
  in
  let body_assoc =
    [
      ("model", `String model_str);
      ("messages", `List provider_messages);
      ("max_tokens", `Int config.config.max_tokens);
    ]
  in
  let body_assoc =
    match config.config.temperature with
    | Some temp -> ("temperature", `Float temp) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    match tools with
    | Some entries when entries <> [] ->
        ("tools", `List (List.map build_openai_tool_json entries)) :: body_assoc
    | _ -> body_assoc
  in
  let body_assoc =
    match config.config.tool_choice with
    | Some choice -> ("tool_choice", tool_choice_to_openai_json choice) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc =
    if config.config.response_format_json then
      ("response_format", `Assoc [("type", `String "json_object")]) :: body_assoc
    else
      body_assoc
  in
  Yojson.Safe.to_string (`Assoc body_assoc)

let usage_of_openai_json json =
  let open Yojson.Safe.Util in
  let usage = json |> member "usage" in
  if usage = `Null then
    None
  else
    Some
      {
        input_tokens =
          usage |> member "prompt_tokens" |> to_int_option |> Option.value ~default:0;
        output_tokens =
          usage |> member "completion_tokens" |> to_int_option |> Option.value ~default:0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
      }

let parse_openai_response json_str =
  let open Yojson.Safe.Util in
  let raw_json = Yojson.Safe.from_string json_str in
  let json =
    match raw_json with
    | `List (first :: _) -> first
    | other -> other
  in
  match json |> member "error" with
  | `Null ->
      let choice = json |> member "choices" |> index 0 in
      let msg = choice |> member "message" in
      let finish_reason =
        choice |> member "finish_reason" |> to_string_option |> Option.value ~default:"stop"
      in
      let text_content =
        match msg |> member "content" with
        | `String s -> s
        | `Null -> ""
        | `List blocks ->
            blocks
            |> List.filter_map (function
                   | `String s -> Some s
                   | `Assoc fields -> (
                       match List.assoc_opt "text" fields with
                       | Some (`String s) -> Some s
                       | _ -> None)
                   | _ -> None)
            |> String.concat ""
        | _ -> ""
      in
      let tool_blocks =
        match msg |> member "tool_calls" with
        | `List calls ->
            List.filter_map
              (fun tc ->
                try
                  let fn = tc |> member "function" in
                  let arguments =
                    fn |> member "arguments" |> to_string_option |> Option.value ~default:"{}"
                  in
                  Some
                    (ToolUse
                       ( tc |> member "id" |> to_string,
                         fn |> member "name" |> to_string,
                         Api_common.json_of_string_or_raw arguments ))
                with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
              calls
        | _ -> []
      in
      let stop_reason =
        match String.lowercase_ascii finish_reason with
        | "tool_calls" when tool_blocks <> [] -> StopToolUse
        | "length" -> MaxTokens
        | "stop" | "end_turn" -> EndTurn
        | _other when tool_blocks <> [] -> StopToolUse
        | other -> Unknown other
      in
      {
        id = json |> member "id" |> to_string_option |> Option.value ~default:"";
        model = json |> member "model" |> to_string_option |> Option.value ~default:"";
        stop_reason;
        content = (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
        usage = usage_of_openai_json json;
      }
  | err ->
      let msg =
        err |> member "message" |> to_string_option |> Option.value ~default:"Unknown API error"
      in
      failwith msg
