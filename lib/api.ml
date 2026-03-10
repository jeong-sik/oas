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

let create_message ~sw ~net ?(base_url=default_base_url) ?provider ?clock ?retry_config ~config ~messages ?tools () =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, key, _headers) -> Ok (url, key)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key -> Ok (base_url, key)
         | None -> Error "API key env var 'ANTHROPIC_API_KEY' not set")
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (base_url, api_key) ->
  
  let headers = Http.Header.of_list [
    ("Content-Type", "application/json");
    ("x-api-key", api_key);
    ("anthropic-version", api_version);
  ] in

  let body_assoc = build_body_assoc ~config ~messages ?tools ~stream:false () in
  let body_str = Yojson.Safe.to_string (`Assoc body_assoc) in
  let uri = Uri.of_string (base_url ^ "/v1/messages") in
  
  let https = make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  let do_request () =
    try
      let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
      match Cohttp.Response.status resp with
      | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          let json = Yojson.Safe.from_string body_str in
          Ok (parse_response json)
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

