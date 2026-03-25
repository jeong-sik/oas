(** OpenAI-compatible request serialization.

    Converts agent_sdk Types (content blocks, messages, tools) into
    OpenAI Chat Completions API JSON format.

    @since 0.92.0 extracted from Backend_openai *)

open Types

let tool_calls_to_openai_json blocks =
  blocks
  |> List.filter_map (function
         | ToolUse { id; name; input } ->
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
             Some (`Assoc [("type", `String "text"); ("text", `String (Utf8_sanitize.sanitize s))])
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
         | Audio { media_type; data; source_type = _ } ->
             Some (`Assoc [
               ("type", `String "input_audio");
               ("input_audio", `Assoc [
                 ("data", `String data);
                 ("format", `String media_type);
               ])
             ])
         | Thinking _ | RedactedThinking _ | ToolUse _ | ToolResult _ -> None)

let openai_messages_of_message (msg : message) : Yojson.Safe.t list =
  match msg.role with
  | User ->
      let content_parts = openai_content_parts_of_blocks msg.content in
      let has_multimodal =
        List.exists (function Image _ | Document _ | Audio _ -> true | _ -> false) msg.content
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
               | ToolResult { tool_use_id; content; _ } ->
                   Some
                     (`Assoc
                        [
                          ("role", `String "tool");
                          ("tool_call_id", `String tool_use_id);
                          ("content", `String (Utf8_sanitize.sanitize content));
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
  | System ->
      let text = Api_common.text_blocks_to_string msg.content in
      [`Assoc [("role", `String "system"); ("content", `String text)]]
  | Tool ->
      msg.content
      |> List.filter_map (function
             | ToolResult { tool_use_id; content; _ } ->
                 Some (`Assoc [
                   ("role", `String "tool");
                   ("tool_call_id", `String tool_use_id);
                   ("content", `String (Utf8_sanitize.sanitize content));
                 ])
             | _ -> None)
      |> (function
          | [] ->
              let text = Api_common.text_blocks_to_string msg.content in
              [`Assoc [("role", `String "user"); ("content", `String text)]]
          | tool_msgs -> tool_msgs)

let tool_choice_to_openai_json = function
  | Auto -> `String "auto"
  | Any -> `String "required"
  | Tool name ->
      `Assoc
        [
          ("type", `String "function");
          ("function", `Assoc [("name", `String name)]);
        ]
  | None_ -> `String "none"

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
