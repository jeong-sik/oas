(** SSE streaming client for Anthropic Messages API.

    Separated from Api module for modularity.
    - parse_sse_event: pure SSE JSON parser
    - create_message_stream: streaming HTTP with SSE accumulation *)

open Types

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
          else
            let input_tokens = u |> member "input_tokens" |> to_int in
            let cache_creation_input_tokens =
              u |> member "cache_creation_input_tokens" |> to_int_option |> Option.value ~default:0 in
            let cache_read_input_tokens =
              u |> member "cache_read_input_tokens" |> to_int_option |> Option.value ~default:0 in
            Some { Types.input_tokens; output_tokens = 0;
                   cache_creation_input_tokens; cache_read_input_tokens }
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
          else
            let output_tokens = u |> member "output_tokens" |> to_int in
            Some { Types.input_tokens = 0; output_tokens;
                   cache_creation_input_tokens = 0; cache_read_input_tokens = 0 }
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

    Unlike Api.create_message, this function does not accept retry_config.
    SSE streams deliver partial results incrementally; retrying mid-stream
    would discard already-received data and produce duplicate content.
    Callers that need retry should wrap this function externally. *)
let create_message_stream ~sw ~net ?(base_url=Api.default_base_url) ?provider ~config ~messages ?tools ~on_event () : (api_response, string) result =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, key, _headers) -> Ok (p, url, key)
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
                 key )
         | None -> Error "API key env var 'ANTHROPIC_API_KEY' not set")
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, api_key) ->
      (match Provider.request_kind provider_cfg.provider with
       | Provider.Anthropic_messages ->
           let headers = Http.Header.of_list [
             ("Content-Type", "application/json");
             ("x-api-key", api_key);
             ("anthropic-version", Api.api_version);
           ] in
           let body_assoc = Api.build_body_assoc ~config ~messages ?tools ~stream:true () in
           let body_str = Yojson.Safe.to_string (`Assoc body_assoc) in
           let uri = Uri.of_string (base_url ^ "/v1/messages") in
           let https = Api.make_https () in
           let client = Cohttp_eio.Client.make ~https net in
           (try
              let resp, body =
                Cohttp_eio.Client.post ~sw client ~headers
                  ~body:(Cohttp_eio.Body.of_string body_str) uri
              in
              match Cohttp.Response.status resp with
              | `OK ->
                  let msg_id = ref "" in
                  let msg_model = ref "" in
                  let input_tokens = ref 0 in
                  let output_tokens = ref 0 in
                  let cache_creation = ref 0 in
                  let cache_read = ref 0 in
                  let stop_reason = ref EndTurn in
                  let block_texts : (int, Buffer.t) Hashtbl.t = Hashtbl.create 4 in
                  let block_types : (int, string) Hashtbl.t = Hashtbl.create 4 in
                  let block_tool_ids : (int, string) Hashtbl.t = Hashtbl.create 4 in
                  let block_tool_names : (int, string) Hashtbl.t = Hashtbl.create 4 in
                  let buf_reader =
                    Eio.Buf_read.of_flow ~max_size:(1024 * 1024 * 10) body
                  in
                  let current_event_type = ref None in
                  let process_line line =
                    if String.length line = 0 then
                      current_event_type := None
                    else if String.length line > 7 && String.sub line 0 7 = "event: " then
                      current_event_type := Some (String.sub line 7 (String.length line - 7))
                    else if String.length line > 6 && String.sub line 0 6 = "data: " then begin
                      let data = String.sub line 6 (String.length line - 6) in
                      if data <> "[DONE]" then
                        match parse_sse_event !current_event_type data with
                        | None -> ()
                        | Some evt ->
                            on_event evt;
                            (match evt with
                             | MessageStart { id; model; usage } ->
                                 msg_id := id;
                                 msg_model := model;
                                 (match usage with
                                  | Some u ->
                                      input_tokens := u.Types.input_tokens;
                                      cache_creation := u.Types.cache_creation_input_tokens;
                                      cache_read := u.Types.cache_read_input_tokens
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
                                 let buf =
                                   match Hashtbl.find_opt block_texts index with
                                   | Some b -> b
                                   | None ->
                                       let b = Buffer.create 64 in
                                       Hashtbl.replace block_texts index b;
                                       b
                                 in
                                 (match delta with
                                  | TextDelta s -> Buffer.add_string buf s
                                  | ThinkingDelta s -> Buffer.add_string buf s
                                  | InputJsonDelta s -> Buffer.add_string buf s)
                             | MessageDelta { stop_reason = sr; usage } ->
                                 (match sr with Some r -> stop_reason := r | None -> ());
                                 (match usage with
                                  | Some u -> output_tokens := u.Types.output_tokens
                                  | None -> ())
                             | _ -> ())
                    end
                  in
                  let rec read_lines () =
                    match Eio.Buf_read.line buf_reader with
                    | line ->
                        process_line line;
                        read_lines ()
                    | exception End_of_file -> ()
                  in
                  read_lines ();
                  let content =
                    Hashtbl.fold
                      (fun index ctype acc ->
                        let text =
                          match Hashtbl.find_opt block_texts index with
                          | Some buf -> Buffer.contents buf
                          | None -> ""
                        in
                        let block =
                          match ctype with
                          | "text" -> Some (Text text)
                          | "thinking" -> Some (Thinking ("", text))
                          | "tool_use" ->
                              let tool_id =
                                match Hashtbl.find_opt block_tool_ids index with
                                | Some id -> id
                                | None -> ""
                              in
                              let tool_name =
                                match Hashtbl.find_opt block_tool_names index with
                                | Some name -> name
                                | None -> ""
                              in
                              (try
                                 Some (ToolUse (tool_id, tool_name, Yojson.Safe.from_string text))
                               with Yojson.Json_error _ -> Some (Text text))
                          | _ -> None
                        in
                        match block with
                        | Some b -> (index, b) :: acc
                        | None -> acc)
                      block_types []
                    |> List.sort (fun (a, _) (b, _) -> compare a b)
                    |> List.map snd
                  in
                  let usage =
                    if !input_tokens > 0 || !output_tokens > 0
                       || !cache_creation > 0 || !cache_read > 0
                    then
                      Some
                        {
                          Types.input_tokens = !input_tokens;
                          output_tokens = !output_tokens;
                          cache_creation_input_tokens = !cache_creation;
                          cache_read_input_tokens = !cache_read;
                        }
                    else
                      None
                  in
                  Ok
                    {
                      id = !msg_id;
                      model = !msg_model;
                      stop_reason = !stop_reason;
                      content;
                      usage;
                    }
              | status ->
                  let body_str =
                    Eio.Buf_read.(of_flow ~max_size:Api.max_response_body body |> take_all)
                  in
                  Error
                    (Printf.sprintf "API Error %s: %s"
                       (Cohttp.Code.string_of_status status) body_str)
            with exn ->
              Error (Printf.sprintf "Network error: %s" (Printexc.to_string exn)))
       | Provider.Openai_chat_completions
       | Provider.Ollama_chat
       | Provider.Ollama_generate ->
           Error "Streaming is only supported for Anthropic-compatible providers")
