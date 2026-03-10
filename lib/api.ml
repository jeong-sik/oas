(** Anthropic Claude API client using Eio *)

open Types

let default_base_url = "https://api.anthropic.com"
let api_version = "2023-06-01"

(** Content block <-> JSON *)
let content_block_to_json = function
  | Text s -> `Assoc [("type", `String "text"); ("text", `String s)]
  | Thinking (signature, content) ->
      `Assoc [
        ("type", `String "thinking");
        ("signature", `String signature);
        ("thinking", `String content);
      ]
  | RedactedThinking data ->
      `Assoc [("type", `String "redacted_thinking"); ("data", `String data)]
  | ToolUse (id, name, input) ->
      `Assoc [
        ("type", `String "tool_use");
        ("id", `String id);
        ("name", `String name);
        ("input", input);
      ]
  | ToolResult (tool_use_id, content, is_error) ->
      `Assoc [
        ("type", `String "tool_result");
        ("tool_use_id", `String tool_use_id);
        ("content", `String content);
        ("is_error", `Bool is_error);
      ]
  | Image { media_type; data; source_type } ->
      `Assoc [
        ("type", `String "image");
        ("source", `Assoc [
          ("type", `String source_type);
          ("media_type", `String media_type);
          ("data", `String data);
        ]);
      ]
  | Document { media_type; data; source_type } ->
      `Assoc [
        ("type", `String "document");
        ("source", `Assoc [
          ("type", `String source_type);
          ("media_type", `String media_type);
          ("data", `String data);
        ]);
      ]

let content_block_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "type" |> to_string_option with
  | Some "text" ->
      let text = json |> member "text" |> to_string in
      Some (Text text)
  | Some "thinking" ->
      let signature = json |> member "signature" |> to_string in
      let content = json |> member "thinking" |> to_string in
      Some (Thinking (signature, content))
  | Some "redacted_thinking" ->
      let data = json |> member "data" |> to_string in
      Some (RedactedThinking data)
  | Some "tool_use" ->
      let id = json |> member "id" |> to_string in
      let name = json |> member "name" |> to_string in
      let input = json |> member "input" in
      Some (ToolUse (id, name, input))
  | Some "tool_result" ->
      let tool_use_id = json |> member "tool_use_id" |> to_string in
      let content = json |> member "content" |> to_string in
      let is_error = json |> member "is_error" |> to_bool_option |> Option.value ~default:false in
      Some (ToolResult (tool_use_id, content, is_error))
  | Some "image" ->
      let source = json |> member "source" in
      let source_type = source |> member "type" |> to_string in
      let media_type = source |> member "media_type" |> to_string in
      let data = source |> member "data" |> to_string in
      Some (Image { media_type; data; source_type })
  | Some "document" ->
      let source = json |> member "source" in
      let source_type = source |> member "type" |> to_string in
      let media_type = source |> member "media_type" |> to_string in
      let data = source |> member "data" |> to_string in
      Some (Document { media_type; data; source_type })
  | _ -> None

let message_to_json msg =
  let role_str = match msg.role with User -> "user" | Assistant -> "assistant" in
  `Assoc [
    ("role", `String role_str);
    ("content", `List (List.map content_block_to_json msg.content));
  ]

(** Parse API Response *)
let parse_response json =
  let open Yojson.Safe.Util in
  let id = json |> member "id" |> to_string in
  let model = json |> member "model" |> to_string in
  let stop_reason_str = json |> member "stop_reason" |> to_string in
  let content_list = json |> member "content" |> to_list in
  let content = List.filter_map content_block_of_json content_list in
  let usage =
    let u = json |> member "usage" in
    if u = `Null then None
    else
      let input_tokens = u |> member "input_tokens" |> to_int in
      let output_tokens = u |> member "output_tokens" |> to_int in
      let cache_creation_input_tokens =
        u |> member "cache_creation_input_tokens" |> to_int_option |> Option.value ~default:0 in
      let cache_read_input_tokens =
        u |> member "cache_read_input_tokens" |> to_int_option |> Option.value ~default:0 in
      Some { Types.input_tokens; output_tokens;
             cache_creation_input_tokens; cache_read_input_tokens }
  in
  let stop_reason = stop_reason_of_string stop_reason_str in
  { id; model; stop_reason; content; usage }

(** Create HTTPS upgrade function using tls-eio *)
let make_https () : (Uri.t -> _ -> _) option =
  match Ca_certs.authenticator () with
  | Error _ -> None
  | Ok authenticator ->
    match Tls.Config.client ~authenticator () with
    | Error _ -> None
    | Ok tls_config ->
      Some (fun uri flow ->
        let host =
          match Uri.host uri with
          | None -> None
          | Some h ->
            match Domain_name.of_string h with
            | Error _ -> None
            | Ok dn -> Some (Domain_name.host_exn dn)
        in
        Tls_eio.client_of_flow tls_config ?host flow)

(** Eio Client Call *)
let max_response_body = 10 * 1024 * 1024 (* 10 MB upper bound for API response *)

(** Build request body assoc list shared between stream and non-stream calls *)
let build_body_assoc ~config ~messages ?tools ~stream () =
  let model_str = model_to_string config.config.model in
  let body_assoc = [
    ("model", `String model_str);
    ("max_tokens", `Int config.config.max_tokens);
    ("messages", `List (List.map message_to_json messages));
    ("stream", `Bool stream);
  ] in
  (* Prompt caching is GA since Dec 2024 — no anthropic-beta header needed.
     Only extended TTL (1h) requires a beta header. *)
  let body_assoc = match config.config.system_prompt with
    | Some s when config.config.cache_system_prompt ->
        let cached_block = `Assoc [
          ("type", `String "text");
          ("text", `String s);
          ("cache_control", `Assoc [("type", `String "ephemeral")]);
        ] in
        ("system", `List [cached_block]) :: body_assoc
    | Some s -> ("system", `String s) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match tools with
    | Some t -> ("tools", `List t) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match config.config.tool_choice with
    | Some tc -> ("tool_choice", tool_choice_to_json tc) :: body_assoc
    | None -> body_assoc
  in
  let body_assoc = match config.config.thinking_budget with
    | Some budget ->
        ("thinking", `Assoc [("type", `String "enabled"); ("budget_tokens", `Int budget)]) :: body_assoc
    | None -> body_assoc
  in
  body_assoc

let string_is_blank s =
  String.trim s = ""

let text_blocks_to_string blocks =
  blocks
  |> List.filter_map (function
         | Text s -> Some s
         | Thinking (_, s) -> Some s
         | RedactedThinking _ -> None
         | ToolUse _ | ToolResult _ | Image _ | Document _ -> None)
  |> String.concat "\n"

let json_of_string_or_raw s =
  try Yojson.Safe.from_string s
  with Yojson.Json_error _ -> `Assoc [("raw", `String s)]

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
             (* Encode documents as image_url with data URI — best-effort for
                providers that support inline documents (e.g. GPT-4o PDF). *)
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
          let text_content = text_blocks_to_string msg.content in
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
      let text_content = text_blocks_to_string msg.content in
      let tool_calls = tool_calls_to_openai_json msg.content in
      let fields =
        [
          ("role", `String "assistant");
          ( if string_is_blank text_content && tool_calls <> [] then
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
  | Some s when not (string_is_blank s) ->
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

let build_ollama_chat_body ~config ~messages ?tools () =
  let base = Yojson.Safe.from_string (build_openai_body ~config ~messages ?tools ()) in
  match base with
  | `Assoc fields ->
      Yojson.Safe.to_string
        (`Assoc
          (("stream", `Bool false)
           :: ("think", `Bool false)
           :: ("options", `Assoc [("num_predict", `Int config.config.max_tokens)])
           :: fields))
  | _ -> assert false

let build_ollama_generate_body ~config ~messages () =
  let prompt =
    let system =
      match config.config.system_prompt with
      | Some s when not (string_is_blank s) -> [ "[System] " ^ s ]
      | _ -> []
    in
    let rendered =
      messages
      |> List.concat_map (fun msg ->
             match msg.role with
             | User ->
                 let text = text_blocks_to_string msg.content in
                 if string_is_blank text then [] else [ text ]
             | Assistant ->
                 let text = text_blocks_to_string msg.content in
                 if string_is_blank text then [] else [ "[Assistant] " ^ text ])
    in
    String.concat "\n" (system @ rendered)
  in
  Yojson.Safe.to_string
    (`Assoc
      [
        ("model", `String (model_to_string config.config.model));
        ("prompt", `String prompt);
        ("stream", `Bool false);
        ("think", `Bool false);
        ("options", `Assoc [("num_predict", `Int config.config.max_tokens)]);
      ])

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

let usage_of_ollama_json json =
  let open Yojson.Safe.Util in
  let inp = json |> member "prompt_eval_count" |> to_int_option |> Option.value ~default:0 in
  let out = json |> member "eval_count" |> to_int_option |> Option.value ~default:0 in
  if inp = 0 && out = 0 then None
  else Some { input_tokens = inp; output_tokens = out;
              cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }

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
                         json_of_string_or_raw arguments ))
                with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> None)
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
        content = (if string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
        usage = usage_of_openai_json json;
      }
  | err ->
      let msg =
        err |> member "message" |> to_string_option |> Option.value ~default:"Unknown API error"
      in
      failwith msg

let parse_ollama_chat_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let msg = json |> member "message" in
  let content = msg |> member "content" |> to_string_option |> Option.value ~default:"" in
  let tool_blocks =
    match msg |> member "tool_calls" with
    | `List calls ->
        List.filter_map
          (fun tc ->
            try
              let fn = tc |> member "function" in
              let arguments =
                match fn |> member "arguments" with
                | `String s -> s
                | `Null -> "{}"
                | other -> Yojson.Safe.to_string other
              in
              Some
                (ToolUse
                   ( tc |> member "id" |> to_string_option |> Option.value ~default:"function_call",
                     fn |> member "name" |> to_string,
                     json_of_string_or_raw arguments ))
            with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> None)
          calls
    | _ -> []
  in
  {
    id = json |> member "created_at" |> to_string_option |> Option.value ~default:"";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason = if tool_blocks <> [] then StopToolUse else EndTurn;
    content = (if string_is_blank content then [] else [Text content]) @ tool_blocks;
    usage = usage_of_ollama_json json;
  }

let parse_ollama_generate_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let content = json |> member "response" |> to_string_option |> Option.value ~default:"" in
  {
    id = json |> member "created_at" |> to_string_option |> Option.value ~default:"";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason = EndTurn;
    content = if string_is_blank content then [] else [Text content];
    usage = usage_of_ollama_json json;
  }

let create_message ~sw ~net ?(base_url=default_base_url) ?provider ?clock ?retry_config ~config ~messages ?tools () =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, _key, headers) -> Ok (p, url, headers)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key ->
             Ok
               ( Provider.
                   {
                     provider = Anthropic;
                     model_id = model_to_string config.config.model;
                     api_key_env = "ANTHROPIC_API_KEY";
                   },
                 base_url,
                 [
                   ("Content-Type", "application/json");
                   ("x-api-key", key);
                   ("anthropic-version", api_version);
                 ] )
         | None -> Error "API key env var 'ANTHROPIC_API_KEY' not set")
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, header_list) ->
  let headers = Http.Header.of_list header_list in
  let kind = Provider.request_kind provider_cfg.provider in
  let path = Provider.request_path provider_cfg.provider in
  let body_str =
    match kind with
    | Provider.Anthropic_messages ->
        Yojson.Safe.to_string (`Assoc (build_body_assoc ~config ~messages ?tools ~stream:false ()))
    | Provider.Openai_chat_completions ->
        build_openai_body ~config ~messages ?tools ()
    | Provider.Ollama_chat ->
        build_ollama_chat_body ~config ~messages ?tools ()
    | Provider.Ollama_generate ->
        build_ollama_generate_body ~config ~messages ()
  in
  let uri = Uri.of_string (base_url ^ path) in
  
  let https = make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  let do_request () =
    try
      let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
      match Cohttp.Response.status resp with
      | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          let response =
            match kind with
            | Provider.Anthropic_messages ->
                parse_response (Yojson.Safe.from_string body_str)
            | Provider.Openai_chat_completions ->
                parse_openai_response body_str
            | Provider.Ollama_chat ->
                parse_ollama_chat_response body_str
            | Provider.Ollama_generate ->
                parse_ollama_generate_response body_str
          in
          Ok response
      | status ->
          let code = Cohttp.Code.code_of_status status in
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          Error (Retry.classify_error ~status:code ~body:body_str)
    with exn ->
      Error (Retry.NetworkError { message = Printexc.to_string exn })
  in
  match clock with
  | Some clock ->
      (match Retry.with_retry ~clock ?config:retry_config do_request with
       | Ok _ as success -> success
       | Error err -> Error (Retry.error_message err))
  | None ->
      (match do_request () with
       | Ok _ as success -> success
       | Error err -> Error (Retry.error_message err))
