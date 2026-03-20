(** OpenAI-compatible API response parsing, message serialization,
    and request building.

    Pure functions operating on {!Llm_provider.Types}.
    {!build_request} uses {!Provider_config.t} (no agent_sdk coupling). *)

open Types

(** Raised when the OpenAI-compatible API returns an error in the response body. *)
exception Openai_api_error of string

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
  | System ->
      let text = Api_common.text_blocks_to_string msg.content in
      [`Assoc [("role", `String "system"); ("content", `String text)]]
  | Tool ->
      (* Tool-role messages carry ToolResult blocks; emit proper OpenAI
         "tool" role with tool_call_id so the model receives the result. *)
      msg.content
      |> List.filter_map (function
             | ToolResult { tool_use_id; content; _ } ->
                 Some (`Assoc [
                   ("role", `String "tool");
                   ("tool_call_id", `String tool_use_id);
                   ("content", `String content);
                 ])
             | _ -> None)
      |> (function
          | [] ->
              (* Fallback: no ToolResult blocks, emit as user message *)
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

let strip_json_markdown_fences text =
  let trimmed = String.trim text in
  if String.length trimmed < 7 || String.sub trimmed 0 3 <> "```" then
    trimmed
  else
    match String.split_on_char '\n' trimmed with
    | first :: rest when String.length first >= 3 ->
        (match List.rev rest with
         | last :: middle_rev when String.trim last = "```" ->
             String.concat "\n" (List.rev middle_rev) |> String.trim
         | _ -> trimmed)
    | _ -> trimmed

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

let usage_of_openai_json json =
  let open Yojson.Safe.Util in
  let usage = json |> member "usage" in
  if usage = `Null then
    None
  else
    let prompt_tokens =
      usage |> member "prompt_tokens" |> to_int_option |> Option.value ~default:0 in
    let cached_tokens =
      let details = usage |> member "prompt_tokens_details" in
      if details = `Null then 0
      else details |> member "cached_tokens" |> to_int_option |> Option.value ~default:0
    in
    Some
      {
        input_tokens = prompt_tokens;
        output_tokens =
          usage |> member "completion_tokens" |> to_int_option |> Option.value ~default:0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = cached_tokens;
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
      let text_content =
        let stripped = strip_json_markdown_fences text_content in
        if stripped = text_content then
          text_content
        else
          try
            ignore (Yojson.Safe.from_string stripped);
            stripped
          with Yojson.Json_error _ -> text_content
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
                       { id = tc |> member "id" |> to_string;
                         name = fn |> member "name" |> to_string;
                         input = Api_common.json_of_string_or_raw arguments })
                with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
              calls
        | _ -> []
      in
      let thinking_blocks =
        match msg |> member "reasoning_content" with
        | `String s when not (Api_common.string_is_blank s) ->
            [Thinking { thinking_type = "reasoning"; content = s }]
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
        content = thinking_blocks @ (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
        usage = usage_of_openai_json json;
      }
  | err ->
      let msg =
        err |> member "message" |> to_string_option |> Option.value ~default:"Unknown API error"
      in
      raise (Openai_api_error msg)

(** Build OpenAI Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let provider_messages =
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
         [`Assoc [("role", `String "system"); ("content", `String s)]]
     | _ -> [])
    @ List.concat_map openai_messages_of_message messages
  in
  let body =
    [ ("model", `String config.model_id);
      ("messages", `List provider_messages);
      ("max_tokens", `Int config.max_tokens) ]
  in
  let body = match config.temperature with
    | Some t -> ("temperature", `Float t) :: body
    | None -> body
  in
  let body = match config.top_p with
    | Some p -> ("top_p", `Float p) :: body
    | None -> body
  in
  let body = match config.top_k with
    | Some k -> ("top_k", `Int k) :: body
    | None -> body
  in
  let body = match config.min_p with
    | Some p -> ("min_p", `Float p) :: body
    | None -> body
  in
  let body = match config.enable_thinking with
    | Some enabled ->
        ("chat_template_kwargs",
         `Assoc [("enable_thinking", `Bool enabled)]) :: body
    | None -> body
  in
  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map build_openai_tool_json ts)) :: body
  in
  let body = match config.tool_choice with
    | Some choice -> ("tool_choice", tool_choice_to_openai_json choice) :: body
    | None -> body
  in
  let body =
    if config.disable_parallel_tool_use && tools <> [] then
      ("parallel_tool_calls", `Bool false) :: body
    else body
  in
  let body =
    if config.response_format_json then
      ("response_format",
       `Assoc [("type", `String "json_object")]) :: body
    else body
  in
  let body =
    if stream then ("stream", `Bool true) :: body
    else body
  in
  Yojson.Safe.to_string (`Assoc body)

[@@@coverage off]
(* === Inline tests === *)

let%test "tool_choice_to_openai_json Auto" =
  tool_choice_to_openai_json Auto = `String "auto"

let%test "tool_choice_to_openai_json Any" =
  tool_choice_to_openai_json Any = `String "required"

let%test "tool_choice_to_openai_json None_" =
  tool_choice_to_openai_json None_ = `String "none"

let%test "tool_choice_to_openai_json Tool name" =
  let result = tool_choice_to_openai_json (Tool "my_tool") in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_tool"

let%test "strip_json_markdown_fences plain text unchanged" =
  strip_json_markdown_fences "{\"key\":\"value\"}" = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences strips json fences" =
  let input = "```json\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences strips plain fences" =
  let input = "```\n{\"key\":\"value\"}\n```" in
  strip_json_markdown_fences input = "{\"key\":\"value\"}"

let%test "strip_json_markdown_fences short string unchanged" =
  strip_json_markdown_fences "hi" = "hi"

let%test "tool_calls_to_openai_json extracts ToolUse blocks" =
  let blocks = [
    Text "hello";
    ToolUse { id = "tc1"; name = "fn1"; input = `Assoc [("x", `Int 1)] };
  ] in
  let result = tool_calls_to_openai_json blocks in
  List.length result = 1

let%test "tool_calls_to_openai_json empty for no tool_use" =
  tool_calls_to_openai_json [Text "no tools"] = []

let%test "openai_content_parts_of_blocks filters text and image" =
  let blocks = [
    Text "hello";
    Thinking { thinking_type = "reasoning"; content = "..." };
    ToolUse { id = "tc1"; name = "fn"; input = `Null };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "build_openai_tool_json converts input_schema to parameters" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("input_schema", `Assoc [("type", `String "object")]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "type" |> to_string = "function"
  && result |> member "function" |> member "name" |> to_string = "my_fn"
  && result |> member "function" |> member "parameters" |> member "type" |> to_string = "object"

let%test "build_openai_tool_json non-assoc passthrough" =
  build_openai_tool_json (`String "bad") = `String "bad"

let%test "usage_of_openai_json parses usage" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 100);
      ("completion_tokens", `Int 50);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.input_tokens = 100 && u.output_tokens = 50
  | None -> false

let%test "usage_of_openai_json null usage returns None" =
  let json = `Assoc [("usage", `Null)] in
  usage_of_openai_json json = None

let%test "usage_of_openai_json missing usage returns None" =
  let json = `Assoc [] in
  usage_of_openai_json json = None

let%test "usage_of_openai_json with cached_tokens" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 100);
      ("completion_tokens", `Int 50);
      ("prompt_tokens_details", `Assoc [("cached_tokens", `Int 30)]);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.cache_read_input_tokens = 30
  | None -> false

let%test "parse_openai_response basic text response" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "chatcmpl-1");
    ("model", `String "gpt-4");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `String "Hello world");
        ]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.id = "chatcmpl-1"
  && resp.model = "gpt-4"
  && resp.stop_reason = EndTurn

let%test "parse_openai_response tool calls" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "cmpl-2");
    ("model", `String "gpt-4");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "tool_calls");
        ("message", `Assoc [
          ("content", `Null);
          ("tool_calls", `List [
            `Assoc [
              ("id", `String "call_1");
              ("type", `String "function");
              ("function", `Assoc [
                ("name", `String "get_weather");
                ("arguments", `String "{\"city\":\"Seoul\"}");
              ]);
            ];
          ]);
        ]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.stop_reason = StopToolUse

let%test "parse_openai_response max_tokens stop reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "cmpl-3");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "length");
        ("message", `Assoc [("content", `String "truncated")]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.stop_reason = MaxTokens

let%test "parse_openai_response error raises exception" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc [("message", `String "rate limited")]);
  ]) in
  try
    ignore (parse_openai_response json_str);
    false
  with Openai_api_error msg -> msg = "rate limited"

let%test "openai_messages_of_message user text" =
  let msg = { role = User; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with tool_result" =
  let msg = { role = User; content = [
    Text "follow up";
    ToolResult { tool_use_id = "tc1"; content = "result"; is_error = false };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  (* One user text message + one tool result message *)
  List.length result = 2

let%test "openai_messages_of_message assistant with tool_calls" =
  let msg = { role = Assistant; content = [
    ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message system" =
  let msg = { role = System; content = [Text "system prompt"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

(* --- Additional coverage tests --- *)

let%test "openai_messages_of_message user empty content" =
  let msg = { role = User; content = []; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  (* no content parts and no tool results *)
  result = []

let%test "openai_messages_of_message user with image" =
  let msg = { role = User; content = [
    Image { media_type = "image/png"; data = "abc123"; source_type = "base64" };
    Text "describe this";
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  (* multimodal -> content is a list *)
  List.length result = 1

let%test "openai_messages_of_message user with document" =
  let msg = { role = User; content = [
    Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with audio" =
  let msg = { role = User; content = [
    Audio { media_type = "audio/wav"; data = "audiodata"; source_type = "base64" };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message assistant text only" =
  let msg = { role = Assistant; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" |> to_string = "hello"

let%test "openai_messages_of_message assistant blank text with tool_calls" =
  let msg = { role = Assistant; content = [
    Text "";
    ToolUse { id = "tc1"; name = "fn"; input = `Assoc [] };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "content" = `Null

let%test "openai_messages_of_message Tool role with ToolResult" =
  let msg = { role = Tool; content = [
    ToolResult { tool_use_id = "tc1"; content = "result data"; is_error = false };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1
  && (let json = List.hd result in
      let open Yojson.Safe.Util in
      json |> member "role" |> to_string = "tool")

let%test "openai_messages_of_message Tool role without ToolResult fallback to user" =
  let msg = { role = Tool; content = [Text "fallback"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  let json = List.hd result in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "user"

let%test "build_openai_tool_json with parameters field" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("parameters", `Assoc [("type", `String "object")]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "function" |> member "parameters" |> member "type" |> to_string = "object"

let%test "build_openai_tool_json missing all optional fields" =
  let tool_json = `Assoc [] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  result |> member "function" |> member "name" |> to_string = "tool"
  && result |> member "function" |> member "description" |> to_string = ""

let%test "strip_json_markdown_fences no closing fence" =
  let input = "```json\n{\"key\":\"value\"}" in
  strip_json_markdown_fences input = input

let%test "strip_json_markdown_fences empty content" =
  strip_json_markdown_fences "" = ""

let%test "parse_openai_response unknown finish_reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "something_new");
        ("message", `Assoc [("content", `String "text")]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.stop_reason = Unknown "something_new"

let%test "parse_openai_response end_turn finish_reason" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "end_turn");
        ("message", `Assoc [("content", `String "done")]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.stop_reason = EndTurn

let%test "parse_openai_response null content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [("content", `Null)]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  resp.stop_reason = EndTurn

let%test "parse_openai_response list content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `List [`String "part1"; `String "part2"])
        ]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  match resp.content with
  | [Text t] -> String.length t > 0
  | _ -> false

let%test "parse_openai_response list content with assoc text blocks" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `List [
            `Assoc [("text", `String "block1")];
            `Assoc [("text", `String "block2")];
          ])
        ]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  match resp.content with
  | [Text t] -> t = "block1block2"
  | _ -> false

let%test "parse_openai_response with reasoning_content" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("id", `String "c1");
    ("model", `String "deepseek-r1");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [
          ("content", `String "answer");
          ("reasoning_content", `String "I thought about it");
        ]);
      ];
    ]);
  ]) in
  let resp = parse_openai_response json_str in
  let has_thinking = List.exists (function
    | Thinking _ -> true | _ -> false) resp.content in
  has_thinking

let%test "parse_openai_response JSON list wrapping" =
  let inner = `Assoc [
    ("id", `String "c1");
    ("model", `String "m");
    ("choices", `List [
      `Assoc [
        ("finish_reason", `String "stop");
        ("message", `Assoc [("content", `String "ok")]);
      ];
    ]);
  ] in
  let json_str = Yojson.Safe.to_string (`List [inner]) in
  let resp = parse_openai_response json_str in
  resp.id = "c1"

let%test "parse_openai_response error without message" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc []);
  ]) in
  try
    ignore (parse_openai_response json_str);
    false
  with Openai_api_error msg -> msg = "Unknown API error"

let%test "usage_of_openai_json prompt_tokens_details null" =
  let json = `Assoc [
    ("usage", `Assoc [
      ("prompt_tokens", `Int 50);
      ("completion_tokens", `Int 25);
      ("prompt_tokens_details", `Null);
    ]);
  ] in
  match usage_of_openai_json json with
  | Some u -> u.cache_read_input_tokens = 0
  | None -> false

let%test "openai_content_parts_of_blocks image block" =
  let blocks = [
    Image { media_type = "image/png"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks document block" =
  let blocks = [
    Document { media_type = "application/pdf"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks audio block" =
  let blocks = [
    Audio { media_type = "audio/wav"; data = "abc"; source_type = "base64" };
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks redacted thinking filtered" =
  let blocks = [
    RedactedThinking "secret";
    Text "visible";
  ] in
  let result = openai_content_parts_of_blocks blocks in
  List.length result = 1

let%test "openai_content_parts_of_blocks tool_result filtered" =
  let blocks = [
    ToolResult { tool_use_id = "t1"; content = "result"; is_error = false };
  ] in
  openai_content_parts_of_blocks blocks = []
