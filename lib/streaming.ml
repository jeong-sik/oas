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

type stream_acc = Llm_provider.Complete_stream_acc.stream_acc =
  { id : string ref
  ; model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : stop_reason ref
  ; stop_reason_received : bool ref
  ; done_sentinel_seen : bool ref
  ; terminal_incomplete : bool ref
  ; sse_error : stream_error option ref
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  ; block_thinking_signatures : (int, Buffer.t) Hashtbl.t
  ; block_reasoning_details : (int, reasoning_detail list ref) Hashtbl.t
  ; block_media_types : (int, string) Hashtbl.t
  ; block_media_sources : (int, media_source_kind) Hashtbl.t
  }

let create_stream_acc = Llm_provider.Complete_stream_acc.create_stream_acc
let accumulate_event = Llm_provider.Complete_stream_acc.accumulate_event
let finalize_stream_acc = Llm_provider.Complete_stream_acc.finalize_stream_acc

(* ── HTTP error mapping ─────────────────────────────────────── *)

let map_http_error = Http_error_sdk.of_http_error

let map_stream_finalize_result = function
  | Error e -> Error (map_http_error e)
  | Ok result ->
    let result =
      Result.map_error Llm_provider.Complete_stream.http_error_of_stream_error result
      |> Llm_provider.Complete_common.ensure_nonempty_completion
    in
    (match result with
     | Ok resp -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
     | Error err -> Error (map_http_error err))
;;

let map_stream_finalize_result_detailed ~binding = function
  | Error error -> Error (Provider_failure_attribution.of_http_error ~binding error)
  | Ok result ->
    let result =
      Result.map_error Llm_provider.Complete_stream.http_error_of_stream_error result
      |> Llm_provider.Complete_common.ensure_nonempty_completion
    in
    (match result with
     | Ok response -> Ok (Llm_provider.Pricing.annotate_response_cost response)
     | Error error -> Error (Provider_failure_attribution.of_http_error ~binding error))
;;

let%test "map_stream_finalize_result maps typed empty to provider unavailable (oas#2483)" =
  let empty_resp : api_response =
    { id = "m"
    ; model = "test"
    ; stop_reason = EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let max_tokens_resp = { empty_resp with stop_reason = MaxTokens } in
  let ok_resp = { empty_resp with content = [ Text "hi" ] } in
  let fails_closed response =
    match map_stream_finalize_result (Ok (Ok response)) with
    | Error (Error.Provider (Llm_provider.Error.ProviderUnavailable _)) -> true
    | Error _ | Ok _ -> false
  in
  (* A content-bearing completion still finalizes Ok: the guard fires only on
     the all-empty clean close, not on every stream. *)
  let nonempty_ok =
    match map_stream_finalize_result (Ok (Ok ok_resp)) with
    | Ok _ -> true
    | Error _ -> false
  in
  fails_closed empty_resp && fails_closed max_tokens_resp && nonempty_ok
;;

(** Streaming variant of create_message.
    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Custom providers fall back to sync + synthetic events.

    Does not accept retry_config: SSE streams deliver partial results
    incrementally; retrying mid-stream would discard data. *)
let create_message_stream_detailed
      ~sw
      ~net
      ?clock
      ?idle_timeout
      ?(base_url = Api.default_base_url)
      ?provider
      ~config
      ~messages
      ?tools
      ~on_event
      ()
  : (api_response, Provider_failure_attribution.detailed_error) result
  =
  let resolve_result =
    match provider with
    | Some p ->
      (match Provider.resolve p with
       | Ok (url, key, headers) -> Ok (p, url, key, headers)
       | Error e -> Error e)
    | None ->
      (match Llm_provider.Cli_common_env.get "ANTHROPIC_API_KEY" with
       | Some key ->
         let fallback_provider : Provider.config =
           { provider = Provider.Anthropic
           ; model_id = model_to_string config.config.model
           ; api_key_env = "ANTHROPIC_API_KEY"
           }
         in
         Ok (fallback_provider, base_url, key, [])
       | None -> Error (Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
  in
  match resolve_result with
  | Error error ->
    Error (Provider_failure_attribution.of_provider_configuration_error error)
  | Ok (provider_cfg, base_url, api_key, resolved_headers) ->
    let model_spec = Provider.model_spec_of_config provider_cfg in
    let binding =
      Binding_identity.of_resolved_provider
        ~transport:Binding_identity.Http
        ~provider:provider_cfg
        ~base_url
        ~request_path:model_spec.request_path
        ~api_key
    in
    (match model_spec.request_kind with
     | Provider.Anthropic_messages ->
       let headers =
         [ "Content-Type", "application/json"
         ; "x-api-key", api_key
         ; "anthropic-version", Api.api_version
         ]
       in
       let body_assoc = Api.build_body_assoc ~config ~messages ?tools ~stream:true () in
       let body = Yojson.Safe.to_string (`Assoc body_assoc) in
       let url = base_url ^ model_spec.request_path in
       Llm_provider.Http_client.with_post_stream
         ?clock
         ~net
         ~url
         ~headers
         ~body
         ~f:(fun reader ->
           let acc = create_stream_acc () in
           Llm_provider.Http_client.read_sse
             ?clock
             ?idle_timeout
             ~reader
             ~on_data:(fun ~event_type data ->
               if data <> "[DONE]"
               then (
                 match parse_sse_event event_type data with
                 | None ->
                   let evt =
                     SSEParseFailed
                       { raw = data; reason = "anthropic_sse_chunk_parse_failure" }
                   in
                   on_event evt;
                   accumulate_event acc evt
                 | Some evt ->
                   on_event evt;
                   accumulate_event acc evt))
             ();
           if !(acc.stop_reason_received) then on_event MessageStop;
           finalize_stream_acc acc)
         ()
       |> map_stream_finalize_result_detailed ~binding
     | Provider.Openai_chat_completions ->
       (* OpenAI-compatible SSE streaming. *)
       let openai_compat_kind = Llm_provider.Provider_config.OpenAI_compat in
       let auth_headers =
         Provider.auth_headers_only_for_kind ~kind:openai_compat_kind ~api_key
       in
       let headers = resolved_headers @ auth_headers in
       let stream_path = model_spec.request_path in
       (match
          Api_openai.build_openai_body_result
            ~provider_config:provider_cfg
            ~config
            ~messages
            ?tools
            ()
        with
        | Error reason ->
          let error =
            Error.Api
              (Llm_provider.Retry.InvalidRequest
                 { message = "Request rejected: " ^ reason
                 ; reason = Llm_provider.Retry.Unknown_invalid_request
                 })
          in
          Error (Provider_failure_attribution.of_request_validation_error ~binding error)
        | Ok body ->
          let body =
            body
            (* Streaming must request both SSE chunks and usage deltas so the
               accumulator can surface final token/cost metrics. *)
            |> Llm_provider.Http_client.inject_stream_and_options
          in
          let url = base_url ^ stream_path in
          Llm_provider.Http_client.with_post_stream
            ?clock
            ~net
            ~url
            ~headers
            ~body
            ~f:(fun reader ->
              let acc = create_stream_acc () in
              let oai_state = Llm_provider.Streaming.create_openai_stream_state () in
              let msg_started = ref false in
              Llm_provider.Http_client.read_sse
                ?clock
                ?idle_timeout
                ~reader
                ~on_data:(fun ~event_type:_ data ->
                  if data = "[DONE]"
                  then ()
                  else (
                    match Llm_provider.Streaming.parse_openai_sse_chunk data with
                    | None ->
                      let evt =
                        SSEParseFailed
                          { raw = data; reason = "openai_sse_chunk_parse_failure" }
                      in
                      on_event evt;
                      accumulate_event acc evt
                    | Some chunk ->
                      if not !msg_started
                      then (
                        msg_started := true;
                        let evt =
                          MessageStart
                            { id = chunk.chunk_id
                            ; model = chunk.chunk_model
                            ; usage = None
                            }
                        in
                        on_event evt;
                        accumulate_event acc evt);
                      let evs, _tel =
                        Llm_provider.Streaming.openai_chunk_to_events oai_state chunk
                      in
                      List.iter
                        (fun evt ->
                           on_event evt;
                           accumulate_event acc evt)
                        evs))
                ();
              if !(acc.stop_reason_received) then on_event MessageStop;
              finalize_stream_acc acc)
            ()
          |> map_stream_finalize_result_detailed ~binding)
     | Provider.Custom _ ->
       (* Sync fallback: non-streaming call + synthetic events *)
       (match
          Api.create_message_detailed
            ~sw
            ~net
            ~base_url
            ~provider:provider_cfg
            ~config
            ~messages
            ?tools
            ()
        with
        | Ok response ->
          emit_synthetic_events response on_event;
          Ok response
        | Error _ as error -> error))
;;

let create_message_stream
      ~sw
      ~net
      ?clock
      ?idle_timeout
      ?base_url
      ?provider
      ~config
      ~messages
      ?tools
      ~on_event
      ()
  =
  create_message_stream_detailed
    ~sw
    ~net
    ?clock
    ?idle_timeout
    ?base_url
    ?provider
    ~config
    ~messages
    ?tools
    ~on_event
    ()
  |> Result.map_error (fun detailed -> detailed.Provider_failure_attribution.error)
;;
