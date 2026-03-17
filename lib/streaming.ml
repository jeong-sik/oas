(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE), OpenAI-compatible (SSE), and
    Ollama Chat (via OpenAI-compat endpoint redirect).
    Pure SSE event parsing and synthetic emission are delegated to
    {!Llm_provider.Streaming}. The HTTP streaming client remains here
    due to agent_state/Provider/Error coupling. *)

open Types

(* Re-export pure functions from llm_provider *)
let parse_sse_event = Llm_provider.Streaming.parse_sse_event
let emit_synthetic_events = Llm_provider.Streaming.emit_synthetic_events

(* ── Shared streaming accumulation ──────────────────────────── *)

type stream_acc = {
  msg_id: string ref;
  msg_model: string ref;
  input_tokens: int ref;
  output_tokens: int ref;
  cache_creation: int ref;
  cache_read: int ref;
  stop_reason: stop_reason ref;
  block_texts: (int, Buffer.t) Hashtbl.t;
  block_types: (int, string) Hashtbl.t;
  block_tool_ids: (int, string) Hashtbl.t;
  block_tool_names: (int, string) Hashtbl.t;
}

let create_stream_acc () = {
  msg_id = ref ""; msg_model = ref "";
  input_tokens = ref 0; output_tokens = ref 0;
  cache_creation = ref 0; cache_read = ref 0;
  stop_reason = ref EndTurn;
  block_texts = Hashtbl.create 4; block_types = Hashtbl.create 4;
  block_tool_ids = Hashtbl.create 4; block_tool_names = Hashtbl.create 4;
}

let accumulate_event (acc : stream_acc) = function
  | MessageStart { id; model; usage } ->
      acc.msg_id := id; acc.msg_model := model;
      (match usage with
       | Some u ->
           acc.input_tokens := u.input_tokens;
           acc.cache_creation := u.cache_creation_input_tokens;
           acc.cache_read := u.cache_read_input_tokens
       | None -> ())
  | ContentBlockStart { index; content_type; tool_id; tool_name } ->
      Hashtbl.replace acc.block_types index content_type;
      Hashtbl.replace acc.block_texts index (Buffer.create 64);
      (match tool_id with
       | Some id -> Hashtbl.replace acc.block_tool_ids index id | None -> ());
      (match tool_name with
       | Some n -> Hashtbl.replace acc.block_tool_names index n | None -> ())
  | ContentBlockDelta { index; delta } ->
      let buf = match Hashtbl.find_opt acc.block_texts index with
        | Some b -> b
        | None ->
            let b = Buffer.create 64 in
            Hashtbl.replace acc.block_texts index b; b
      in
      (match delta with
       | TextDelta s | ThinkingDelta s | InputJsonDelta s ->
           Buffer.add_string buf s)
  | MessageDelta { stop_reason = sr; usage } ->
      (match sr with Some r -> acc.stop_reason := r | None -> ());
      (match usage with
       | Some u ->
           acc.output_tokens := u.output_tokens;
           if u.cache_creation_input_tokens > 0 then
             acc.cache_creation := u.cache_creation_input_tokens;
           if u.cache_read_input_tokens > 0 then
             acc.cache_read := u.cache_read_input_tokens
       | None -> ())
  | _ -> ()

let finalize_stream_acc (acc : stream_acc) =
  let content =
    Hashtbl.fold (fun index ctype items ->
      let text = match Hashtbl.find_opt acc.block_texts index with
        | Some buf -> Buffer.contents buf | None -> "" in
      let block = match ctype with
        | "text" -> Some (Text text)
        | "thinking" ->
            Some (Thinking { thinking_type = ""; content = text })
        | "tool_use" ->
            let tool_id =
              match Hashtbl.find_opt acc.block_tool_ids index with
              | Some id -> id | None -> "" in
            let tool_name =
              match Hashtbl.find_opt acc.block_tool_names index with
              | Some name -> name | None -> "" in
            (try Some (ToolUse { id = tool_id; name = tool_name;
                                 input = Yojson.Safe.from_string text })
             with Yojson.Json_error _ -> Some (Text text))
        | _ -> None
      in
      match block with Some b -> (index, b) :: items | None -> items
    ) acc.block_types []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let usage =
    if !(acc.input_tokens) > 0 || !(acc.output_tokens) > 0
       || !(acc.cache_creation) > 0 || !(acc.cache_read) > 0
    then Some { input_tokens = !(acc.input_tokens);
                output_tokens = !(acc.output_tokens);
                cache_creation_input_tokens = !(acc.cache_creation);
                cache_read_input_tokens = !(acc.cache_read) }
    else None
  in
  { id = !(acc.msg_id); model = !(acc.msg_model);
    stop_reason = !(acc.stop_reason); content; usage }

(** Streaming variant of create_message.
    Sends request with stream:true, parses SSE events line-by-line,
    calls on_event for each parsed event, and returns a complete api_response.

    Unlike Api.create_message, this function does not accept retry_config.
    SSE streams deliver partial results incrementally; retrying mid-stream
    would discard already-received data and produce duplicate content.
    Callers that need retry should wrap this function externally. *)
let create_message_stream ~sw ~net ?(base_url=Api.default_base_url) ?provider ~config ~messages ?tools ~on_event () : (api_response, Error.sdk_error) result =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, key, _headers) -> Ok (p, url, key)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key ->
             let fallback_provider : Provider.config =
               {
                 provider = Provider.Anthropic;
                 model_id = model_to_string config.config.model;
                 api_key_env = "ANTHROPIC_API_KEY";
               }
             in
             Ok
               ( fallback_provider,
                 base_url,
                 key )
         | None -> Error (Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
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
                                      input_tokens := u.input_tokens;
                                      cache_creation := u.cache_creation_input_tokens;
                                      cache_read := u.cache_read_input_tokens
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
                                  | Some u ->
                                      output_tokens := u.output_tokens;
                                      if u.cache_creation_input_tokens > 0 then
                                        cache_creation := u.cache_creation_input_tokens;
                                      if u.cache_read_input_tokens > 0 then
                                        cache_read := u.cache_read_input_tokens
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
                          | "thinking" -> Some (Thinking { thinking_type = ""; content = text })
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
                                 Some (ToolUse { id = tool_id; name = tool_name; input = Yojson.Safe.from_string text })
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
                          input_tokens = !input_tokens;
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
                  let code = Cohttp.Code.code_of_status status in
                  let body_str =
                    Eio.Buf_read.(of_flow ~max_size:Api.max_response_body body |> take_all)
                  in
                  Error (Error.Api (Retry.classify_error ~status:code ~body:body_str))
            with
            | Eio.Io _ as exn ->
              Error (Error.Api (Retry.NetworkError { message = Printexc.to_string exn }))
            | Unix.Unix_error _ as exn ->
              Error (Error.Api (Retry.NetworkError { message = Printexc.to_string exn }))
            | Failure msg ->
              Error (Error.Api (Retry.NetworkError { message = msg }))
            | Yojson.Json_error msg ->
              Error (Error.Api (Retry.NetworkError { message = "JSON parse error: " ^ msg })))
       | Provider.Openai_chat_completions
       | Provider.Ollama_chat ->
           (* OpenAI-compatible SSE streaming.
              Ollama Chat is redirected to /v1/chat/completions for SSE. *)
           let resolved_headers = match Provider.resolve provider_cfg with
             | Ok (_, _, h) -> h
             | Error _ -> [("Content-Type", "application/json")]
           in
           let headers = Http.Header.of_list resolved_headers in
           let stream_path = match provider_cfg.provider with
             | Provider.Ollama _ -> "/v1/chat/completions"
             | _ -> Provider.request_path provider_cfg.provider
           in
           let body_str = Api_openai.build_openai_body
             ~provider_config:provider_cfg ~config ~messages ?tools () in
           let body_with_stream = match Yojson.Safe.from_string body_str with
             | `Assoc fields ->
                 Yojson.Safe.to_string
                   (`Assoc (("stream", `Bool true) :: fields))
             | other -> Yojson.Safe.to_string other
           in
           let uri = Uri.of_string (base_url ^ stream_path) in
           let https = Api.make_https () in
           let client = Cohttp_eio.Client.make ~https net in
           (try
              let resp, body =
                Cohttp_eio.Client.post ~sw client ~headers
                  ~body:(Cohttp_eio.Body.of_string body_with_stream) uri
              in
              match Cohttp.Response.status resp with
              | `OK ->
                  let acc = create_stream_acc () in
                  let buf_reader =
                    Eio.Buf_read.of_flow ~max_size:(1024 * 1024 * 10) body
                  in
                  let oai_state =
                    Llm_provider.Streaming.create_openai_stream_state () in
                  let msg_started = ref false in
                  let process_data data =
                    if data = "[DONE]" then ()
                    else
                      match Llm_provider.Streaming.parse_openai_sse_chunk
                              data with
                      | None -> ()
                      | Some chunk ->
                          if not !msg_started then begin
                            msg_started := true;
                            let start_evt = MessageStart {
                              id = chunk.Llm_provider.Streaming.chunk_id;
                              model = chunk.chunk_model;
                              usage = None } in
                            on_event start_evt;
                            accumulate_event acc start_evt
                          end;
                          let events =
                            Llm_provider.Streaming.openai_chunk_to_events
                              oai_state chunk in
                          List.iter (fun evt ->
                            on_event evt;
                            accumulate_event acc evt
                          ) events
                  in
                  let rec read_lines () =
                    match Eio.Buf_read.line buf_reader with
                    | line ->
                        if String.length line > 6
                           && String.sub line 0 6 = "data: " then
                          process_data
                            (String.sub line 6 (String.length line - 6));
                        read_lines ()
                    | exception End_of_file -> ()
                  in
                  read_lines ();
                  on_event MessageStop;
                  Ok (finalize_stream_acc acc)
              | status ->
                  let code = Cohttp.Code.code_of_status status in
                  let body_str =
                    Eio.Buf_read.(of_flow ~max_size:Api.max_response_body body
                                  |> take_all)
                  in
                  Error (Error.Api (Retry.classify_error ~status:code
                                      ~body:body_str))
            with
            | Eio.Io _ as exn ->
                Error (Error.Api (Retry.NetworkError {
                  message = Printexc.to_string exn }))
            | Unix.Unix_error _ as exn ->
                Error (Error.Api (Retry.NetworkError {
                  message = Printexc.to_string exn }))
            | Failure msg ->
                Error (Error.Api (Retry.NetworkError { message = msg }))
            | Yojson.Json_error msg ->
                Error (Error.Api (Retry.NetworkError {
                  message = "JSON parse error: " ^ msg })))
       | Provider.Ollama_generate
       | Provider.Custom _ ->
           (* Sync fallback: non-streaming call + synthetic events *)
           (match Api.create_message ~sw ~net ~base_url
                    ~provider:provider_cfg ~config ~messages ?tools () with
            | Ok response ->
                emit_synthetic_events response on_event;
                Ok response
            | Error _ as e -> e))
