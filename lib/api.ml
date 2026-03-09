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

(** Parse a single SSE data JSON payload into an sse_event *)
let parse_sse_event event_type data_str =
  let open Yojson.Safe.Util in
  try
    let json = Yojson.Safe.from_string data_str in
    let evt_type = match event_type with
      | Some t -> t
      | None -> json |> member "type" |> to_string_option |> Option.value ~default:""
    in
    match evt_type with
    | "message_start" ->
        let msg = json |> member "message" in
        let id = msg |> member "id" |> to_string in
        let model = msg |> member "model" |> to_string in
        let usage =
          let u = msg |> member "usage" in
          if u = `Null then None
          else Some (u |> member "input_tokens" |> to_int, 0)
        in
        Some (MessageStart { id; model; usage })
    | "content_block_start" ->
        let index = json |> member "index" |> to_int in
        let cb = json |> member "content_block" in
        let content_type = cb |> member "type" |> to_string in
        let tool_id = cb |> member "id" |> to_string_option in
        let tool_name = cb |> member "name" |> to_string_option in
        Some (ContentBlockStart { index; content_type; tool_id; tool_name })
    | "content_block_delta" ->
        let index = json |> member "index" |> to_int in
        let delta_json = json |> member "delta" in
        let delta_type = delta_json |> member "type" |> to_string in
        let delta = match delta_type with
          | "text_delta" ->
              TextDelta (delta_json |> member "text" |> to_string)
          | "thinking_delta" ->
              ThinkingDelta (delta_json |> member "thinking" |> to_string)
          | "input_json_delta" ->
              InputJsonDelta (delta_json |> member "partial_json" |> to_string)
          | _ ->
              TextDelta ""
        in
        Some (ContentBlockDelta { index; delta })
    | "content_block_stop" ->
        let index = json |> member "index" |> to_int in
        Some (ContentBlockStop { index })
    | "message_delta" ->
        let delta = json |> member "delta" in
        let stop_reason =
          delta |> member "stop_reason" |> to_string_option
          |> Option.map stop_reason_of_string
        in
        let usage =
          let u = json |> member "usage" in
          if u = `Null then None
          else Some (0, u |> member "output_tokens" |> to_int)
        in
        Some (MessageDelta { stop_reason; usage })
    | "message_stop" ->
        Some MessageStop
    | "ping" ->
        Some Ping
    | "error" ->
        let msg = json |> member "error" |> member "message" |> to_string in
        Some (SSEError msg)
    | _ -> None
  with Yojson.Safe.Util.Type_error _ | Yojson.Json_error _ -> None

(** Streaming variant of create_message.
    Sends request with stream:true, parses SSE events line-by-line,
    calls on_event for each parsed event, and returns a complete api_response.

    Unlike create_message, this function does not accept retry_config.
    SSE streams deliver partial results incrementally; retrying mid-stream
    would discard already-received data and produce duplicate content.
    Callers that need retry should wrap this function externally. *)
let create_message_stream ~sw ~net ?(base_url=default_base_url) ?provider ~config ~messages ?tools ~on_event () : (api_response, string) result =
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

  let body_assoc = build_body_assoc ~config ~messages ?tools ~stream:true () in
  let body_str = Yojson.Safe.to_string (`Assoc body_assoc) in
  let uri = Uri.of_string (base_url ^ "/v1/messages") in

  let https = make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  try
    let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
    match Cohttp.Response.status resp with
    | `OK ->
        (* Accumulate content blocks as they stream in *)
        let msg_id = ref "" in
        let msg_model = ref "" in
        let input_tokens = ref 0 in
        let output_tokens = ref 0 in
        let stop_reason = ref EndTurn in
        (* Map from block index to accumulated text/json *)
        let block_texts : (int, Buffer.t) Hashtbl.t = Hashtbl.create 4 in
        let block_types : (int, string) Hashtbl.t = Hashtbl.create 4 in
        (* Track tool_use id/name from ContentBlockStart events *)
        let block_tool_ids : (int, string) Hashtbl.t = Hashtbl.create 4 in
        let block_tool_names : (int, string) Hashtbl.t = Hashtbl.create 4 in

        let buf_reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024 * 10) body in
        let current_event_type = ref None in

        let process_line line =
          if String.length line = 0 then
            (* empty line: reset current event type *)
            current_event_type := None
          else if String.length line > 7 && String.sub line 0 7 = "event: " then
            current_event_type := Some (String.sub line 7 (String.length line - 7))
          else if String.length line > 6 && String.sub line 0 6 = "data: " then begin
            let data = String.sub line 6 (String.length line - 6) in
            if data = "[DONE]" then ()
            else begin
              match parse_sse_event !current_event_type data with
              | None -> ()
              | Some evt ->
                  on_event evt;
                  (match evt with
                  | MessageStart { id; model; usage } ->
                      msg_id := id;
                      msg_model := model;
                      (match usage with
                       | Some (inp, _) -> input_tokens := inp
                       | None -> ())
                  | ContentBlockStart { index; content_type; tool_id; tool_name } ->
                      Hashtbl.replace block_types index content_type;
                      Hashtbl.replace block_texts index (Buffer.create 64);
                      (match tool_id with
                       | Some id -> Hashtbl.replace block_tool_ids index id
                       | None -> ());
                      (match tool_name with
                       | Some name -> Hashtbl.replace block_tool_names index name
                       | None -> ())
                  | ContentBlockDelta { index; delta } ->
                      let buf = match Hashtbl.find_opt block_texts index with
                        | Some b -> b
                        | None -> let b = Buffer.create 64 in Hashtbl.replace block_texts index b; b
                      in
                      (match delta with
                       | TextDelta s -> Buffer.add_string buf s
                       | ThinkingDelta s -> Buffer.add_string buf s
                       | InputJsonDelta s -> Buffer.add_string buf s)
                  | MessageDelta { stop_reason = sr; usage } ->
                      (match sr with Some r -> stop_reason := r | None -> ());
                      (match usage with
                       | Some (_, out) -> output_tokens := out
                       | None -> ())
                  | _ -> ())
            end
          end
        in

        let rec read_lines () =
          match Eio.Buf_read.line buf_reader with
          | line -> process_line line; read_lines ()
          | exception End_of_file -> ()
        in
        read_lines ();

        (* Reconstruct content blocks from accumulated data *)
        let content =
          Hashtbl.fold (fun index ctype acc ->
            let text = match Hashtbl.find_opt block_texts index with
              | Some buf -> Buffer.contents buf
              | None -> ""
            in
            let block = match ctype with
              | "text" -> Some (Text text)
              | "thinking" -> Some (Thinking ("", text))
              | "tool_use" ->
                  let tool_id = match Hashtbl.find_opt block_tool_ids index with
                    | Some id -> id | None -> ""
                  in
                  let tool_name = match Hashtbl.find_opt block_tool_names index with
                    | Some name -> name | None -> ""
                  in
                  (try Some (ToolUse (tool_id, tool_name, Yojson.Safe.from_string text))
                   with Yojson.Json_error _ -> Some (Text text))
              | _ -> None
            in
            match block with
            | Some b -> (index, b) :: acc
            | None -> acc
          ) block_types []
          |> List.sort (fun (a, _) (b, _) -> compare a b)
          |> List.map snd
        in

        let usage = if !input_tokens > 0 || !output_tokens > 0
          then Some { Types.input_tokens = !input_tokens;
                      output_tokens = !output_tokens;
                      cache_creation_input_tokens = 0;
                      cache_read_input_tokens = 0 }
          else None
        in
        Ok {
          id = !msg_id;
          model = !msg_model;
          stop_reason = !stop_reason;
          content;
          usage;
        }
    | status ->
        let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
        Error (Printf.sprintf "API Error %s: %s" (Cohttp.Code.string_of_status status) body_str)
  with exn ->
    Error (Printf.sprintf "Network error: %s" (Printexc.to_string exn))
