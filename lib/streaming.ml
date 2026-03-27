(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
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
  sse_error: string option ref;
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
  sse_error = ref None;
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
  | SSEError msg -> acc.sse_error := Some msg
  | _ -> ()

let finalize_stream_acc (acc : stream_acc) =
  match !(acc.sse_error) with
  | Some msg -> Error msg
  | None ->
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
                cache_read_input_tokens = !(acc.cache_read);
                cost_usd = None }
    else None
  in
  Ok { id = !(acc.msg_id); model = !(acc.msg_model);
    stop_reason = !(acc.stop_reason); content; usage }

(* ── HTTP error mapping ─────────────────────────────────────── *)

let map_http_error = function
  | Llm_provider.Http_client.HttpError { code; body } ->
      Error.Api (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message } ->
      Error.Api (Retry.NetworkError { message })

(** Streaming variant of create_message.
    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Custom providers fall back to sync + synthetic events.

    Does not accept retry_config: SSE streams deliver partial results
    incrementally; retrying mid-stream would discard data. *)
let create_message_stream ~sw ~net ?(base_url=Api.default_base_url)
    ?provider ~config ~messages ?tools ~on_event ()
    : (api_response, Error.sdk_error) result =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, key, _headers) -> Ok (p, url, key)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key ->
             let fallback_provider : Provider.config = {
               provider = Provider.Anthropic;
               model_id = model_to_string config.config.model;
               api_key_env = "ANTHROPIC_API_KEY";
             } in
             Ok (fallback_provider, base_url, key)
         | None ->
             Error (Error.Config
               (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, api_key) ->
      (match Provider.request_kind provider_cfg.provider with
       | Provider.Anthropic_messages ->
           let headers = [
             ("Content-Type", "application/json");
             ("x-api-key", api_key);
             ("anthropic-version", Api.api_version);
           ] in
           let body_assoc =
             Api.build_body_assoc ~config ~messages ?tools
               ~stream:true () in
           let body = Yojson.Safe.to_string (`Assoc body_assoc) in
           let url = base_url ^ "/v1/messages" in
           (match Llm_provider.Http_client.with_post_stream
                    ~net ~url ~headers ~body
                    ~f:(fun reader ->
                      let acc = create_stream_acc () in
                      Llm_provider.Http_client.read_sse ~reader
                        ~on_data:(fun ~event_type data ->
                          if data <> "[DONE]" then
                            match parse_sse_event event_type data with
                            | None -> ()
                            | Some evt ->
                                on_event evt; accumulate_event acc evt
                        ) ();
                      on_event MessageStop;
                      finalize_stream_acc acc) with
            | Error e -> Error (map_http_error e)
            | Ok (Ok resp) -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
            | Ok (Error msg) ->
                Error (Error.Api (Retry.NetworkError {
                  message = Printf.sprintf "SSE stream error: %s" msg })))
       | Provider.Openai_chat_completions ->
           (* OpenAI-compatible SSE streaming. *)
           let headers = match Provider.resolve provider_cfg with
             | Ok (_, _, h) -> h
             | Error _ -> [("Content-Type", "application/json")]
           in
           let stream_path = Provider.request_path provider_cfg.provider in
           let body =
             Api_openai.build_openai_body
               ~provider_config:provider_cfg ~config ~messages
               ?tools ()
             |> Llm_provider.Http_client.inject_stream_param
           in
           let url = base_url ^ stream_path in
           (match Llm_provider.Http_client.with_post_stream
                    ~net ~url ~headers ~body
                    ~f:(fun reader ->
                      let acc = create_stream_acc () in
                      let oai_state =
                        Llm_provider.Streaming.create_openai_stream_state () in
                      let msg_started = ref false in
                      Llm_provider.Http_client.read_sse ~reader
                        ~on_data:(fun ~event_type:_ data ->
                          if data = "[DONE]" then ()
                          else
                            match Llm_provider.Streaming.parse_openai_sse_chunk
                                    data with
                            | None -> ()
                            | Some chunk ->
                                if not !msg_started then begin
                                  msg_started := true;
                                  let evt = MessageStart {
                                    id = chunk.chunk_id;
                                    model = chunk.chunk_model;
                                    usage = None } in
                                  on_event evt;
                                  accumulate_event acc evt
                                end;
                                List.iter (fun evt ->
                                  on_event evt;
                                  accumulate_event acc evt
                                ) (Llm_provider.Streaming.openai_chunk_to_events
                                     oai_state chunk)
                        ) ();
                      on_event MessageStop;
                      finalize_stream_acc acc) with
            | Error e -> Error (map_http_error e)
            | Ok (Ok resp) -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
            | Ok (Error msg) ->
                Error (Error.Api (Retry.NetworkError {
                  message = Printf.sprintf "SSE stream error: %s" msg })))
       | Provider.Custom _ ->
           (* Sync fallback: non-streaming call + synthetic events *)
           (match Api.create_message ~sw ~net ~base_url
                    ~provider:provider_cfg ~config ~messages ?tools () with
            | Ok response ->
                emit_synthetic_events response on_event;
                Ok response
            | Error _ as e -> e))
