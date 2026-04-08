(** OpenAI-compatible API response parsing, message serialization,
    and request building.

    Pure functions operating on {!Llm_provider.Types}.
    {!build_request} uses {!Provider_config.t} (no agent_sdk coupling).

    @since 0.92.0 decomposed into Backend_openai_serialize,
    Backend_openai_parse *)

open Types

(* ── Re-exports from serialization ─────────────────────── *)

let tool_calls_to_openai_json = Backend_openai_serialize.tool_calls_to_openai_json
let openai_content_parts_of_blocks = Backend_openai_serialize.openai_content_parts_of_blocks
let openai_messages_of_message = Backend_openai_serialize.openai_messages_of_message
let tool_choice_to_openai_json = Backend_openai_serialize.tool_choice_to_openai_json
let build_openai_tool_json = Backend_openai_serialize.build_openai_tool_json

(* ── Re-exports from parsing ──────────────────────────── *)

let strip_json_markdown_fences = Backend_openai_parse.strip_json_markdown_fences
let usage_of_openai_json = Backend_openai_parse.usage_of_openai_json

let parse_openai_response_result = Backend_openai_parse.parse_openai_response_result

(* ── Request building ──────────────────────────────────── *)

(** Build OpenAI Chat Completions request body from {!Provider_config.t}.
    Returns a JSON string ready for HTTP POST. *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  let provider_messages =
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
         [`Assoc [("role", `String "system"); ("content", `String (Utf8_sanitize.sanitize s))]]
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
  let supports_tool_choice =
    match Capabilities.for_model_id config.model_id with
    | Some caps -> caps.supports_tool_choice
    | None -> true  (* unknown model — assume support for backward compat *)
  in
  let body = match config.tool_choice with
    | Some choice ->
        if supports_tool_choice
        then ("tool_choice", tool_choice_to_openai_json choice) :: body
        else body  (* tool_choice set but provider does not support it *)
    | None -> body
  in
  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map build_openai_tool_json ts)) :: body
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

let%test "parse_openai_response_result basic text response" =
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
  match parse_openai_response_result json_str with
  | Ok resp ->
    resp.id = "chatcmpl-1"
    && resp.model = "gpt-4"
    && resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result tool calls" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = StopToolUse
  | Error _ -> false

let%test "parse_openai_response_result max_tokens stop reason" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = MaxTokens
  | Error _ -> false

let%test "parse_openai_response_result error returns Error" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc [("message", `String "rate limited")]);
  ]) in
  match parse_openai_response_result json_str with
  | Error msg -> msg = "rate limited"
  | Ok _ -> false

let%test "openai_messages_of_message user text" =
  let msg = { role = User; content = [Text "hello"]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  List.length result = 1

let%test "openai_messages_of_message user with tool_result" =
  let msg = { role = User; content = [
    Text "follow up";
    ToolResult { tool_use_id = "tc1"; content = "result"; is_error = false; json = None };
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
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

let%test "openai_messages_of_message user empty content" =
  let msg = { role = User; content = []; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
  result = []

let%test "openai_messages_of_message user with image" =
  let msg = { role = User; content = [
    Image { media_type = "image/png"; data = "abc123"; source_type = "base64" };
    Text "describe this";
  ]; name = None; tool_call_id = None } in
  let result = openai_messages_of_message msg in
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
    ToolResult { tool_use_id = "tc1"; content = "result data"; is_error = false; json = None };
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

let%test "build_openai_tool_json converts legacy parameter list to json schema" =
  let tool_json = `Assoc [
    ("name", `String "my_fn");
    ("description", `String "does stuff");
    ("parameters", `List [
      `Assoc [
        ("name", `String "query");
        ("description", `String "search query");
        ("param_type", `String "string");
        ("required", `Bool true);
      ];
      `Assoc [
        ("name", `String "limit");
        ("description", `String "max results");
        ("param_type", `String "integer");
        ("required", `Bool false);
      ];
    ]);
  ] in
  let result = build_openai_tool_json tool_json in
  let open Yojson.Safe.Util in
  let parameters = result |> member "function" |> member "parameters" in
  parameters |> member "type" |> to_string = "object"
  && parameters |> member "properties" |> member "query" |> member "type" |> to_string = "string"
  && parameters |> member "properties" |> member "limit" |> member "type" |> to_string = "integer"
  && List.mem "query" (parameters |> member "required" |> to_list |> List.map to_string)

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

let%test "parse_openai_response_result unknown finish_reason" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = Unknown "something_new"
  | Error _ -> false

let%test "parse_openai_response_result end_turn finish_reason" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result null content" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.stop_reason = EndTurn
  | Error _ -> false

let%test "parse_openai_response_result list content" =
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
  match parse_openai_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [Text t] -> String.length t > 0
     | _ -> false)
  | Error _ -> false

let%test "parse_openai_response_result list content with assoc text blocks" =
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
  match parse_openai_response_result json_str with
  | Ok resp ->
    (match resp.content with
     | [Text t] -> t = "block1block2"
     | _ -> false)
  | Error _ -> false

let%test "parse_openai_response_result with reasoning_content" =
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
  match parse_openai_response_result json_str with
  | Ok resp ->
    List.exists (function Thinking _ -> true | _ -> false) resp.content
  | Error _ -> false

let%test "parse_openai_response_result JSON list wrapping" =
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
  match parse_openai_response_result json_str with
  | Ok resp -> resp.id = "c1"
  | Error _ -> false

let%test "parse_openai_response_result error without message" =
  let json_str = Yojson.Safe.to_string (`Assoc [
    ("error", `Assoc []);
  ]) in
  match parse_openai_response_result json_str with
  | Error msg -> msg = "Unknown API error"
  | Ok _ -> false

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
    ToolResult { tool_use_id = "t1"; content = "result"; is_error = false; json = None };
  ] in
  openai_content_parts_of_blocks blocks = []

let%test "build_request includes tool_choice for model with supports_tool_choice=true" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"gpt-4o"
      ~base_url:"http://localhost" ~tool_choice:Any () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"

let%test "build_request includes tool_choice for unknown model (backward compat)" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"mystery-xyz-v1"
      ~base_url:"http://localhost" ~tool_choice:Any () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  json |> member "tool_choice" |> to_string = "required"

let%test "build_request omits tool_choice when tool_choice=None" =
  let config = Provider_config.make ~kind:OpenAI_compat ~model_id:"gpt-4o"
      ~base_url:"http://localhost" () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  match json with
  | `Assoc fields -> not (List.exists (fun (k, _) -> k = "tool_choice") fields)
  | _ -> false
