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
let stream_failed = Llm_provider.Complete_stream_acc.stream_failed

exception Stream_finished

let stop_sse_read_if_terminal acc =
  if stream_failed acc || !(acc.done_sentinel_seen) then raise Stream_finished
;;

let emit_message_stop_if_missing acc ~on_event =
  if
    !(acc.stop_reason_received)
    && (not !(acc.done_sentinel_seen))
    && not (stream_failed acc)
  then on_event MessageStop
;;

let%test "terminal callback is exactly once with finish reason and sentinel" =
  let acc = create_stream_acc () in
  let emitted = ref [] in
  let on_event event = emitted := event :: !emitted in
  accumulate_event acc (MessageDelta { stop_reason = Some EndTurn; usage = None });
  on_event MessageStop;
  accumulate_event acc MessageStop;
  emit_message_stop_if_missing acc ~on_event;
  List.equal ( = ) !emitted [ MessageStop ]
;;

let%test "terminal callback is synthesized once without sentinel" =
  let acc = create_stream_acc () in
  let emitted = ref [] in
  let on_event event = emitted := event :: !emitted in
  accumulate_event acc (MessageDelta { stop_reason = Some EndTurn; usage = None });
  emit_message_stop_if_missing acc ~on_event;
  List.equal ( = ) !emitted [ MessageStop ]
;;

let%test "terminal callback is not synthesized after stream failure" =
  let failures =
    [ SSEError { message = "provider failed"; error_type = None; raw = "error" }
    ; SSEParseFailed { reason = "malformed"; raw = "{" }
    ]
  in
  List.for_all
    (fun failure ->
       let acc = create_stream_acc () in
       let emitted = ref [] in
       accumulate_event acc (MessageDelta { stop_reason = Some EndTurn; usage = None });
       accumulate_event acc failure;
       emit_message_stop_if_missing acc ~on_event:(fun event ->
         emitted := event :: !emitted);
       List.is_empty !emitted)
    failures
;;

let%test "terminal state stops the SSE reader immediately" =
  let acc = create_stream_acc () in
  let lines_seen = ref 0 in
  let flow = Eio.Flow.string_source "data: terminal\n\ndata: late\n\n" in
  let reader = Eio.Buf_read.of_flow ~max_size:1024 flow in
  match
    Llm_provider.Http_client.read_sse
      ~reader
      ~on_data:(fun ~event_type:_ _ ->
        incr lines_seen;
        accumulate_event acc MessageStop;
        stop_sse_read_if_terminal acc)
      ()
  with
  | () -> false
  | exception Stream_finished -> !lines_seen = 1
;;

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

let map_stream_finalize_result_detailed ~binding ~provider_id ~provider_config = function
  | Error error -> Error (Provider_failure_attribution.of_http_error ~binding error)
  | Ok result ->
    let result =
      Result.map_error Llm_provider.Complete_stream.http_error_of_stream_error result
      |> Llm_provider.Complete_common.ensure_nonempty_completion
    in
    (match result with
     | Ok response ->
       Ok
         (Llm_provider.Pricing.annotate_response_cost ~provider_id response
          |> fun response ->
          Llm_provider.Complete_common.patch_telemetry
            response
            ~config:provider_config
            None)
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
    Supports providers with an implemented Anthropic or OpenAI-compatible SSE
    codec. A custom provider without a streaming codec is rejected explicitly.

    Performs exactly one provider stream attempt. Partial events and a terminal
    typed failure are returned unchanged; any later attempt belongs to a new
    caller-owned stream. *)
let create_message_stream_detailed
      ~sw
      ~net
      ?clock
      ?idle_timeout
      ~provider
      ~config
      ~messages
      ?tools
      ~on_event
      ()
  : (api_response, Provider_failure_attribution.detailed_error) result
  =
  let ( let* ) = Result.bind in
  let configuration_error detail =
    Provider_failure_attribution.of_provider_configuration_error
      (Error.Config (InvalidConfig { field = "model_id"; detail }))
  in
  match Provider.resolve provider with
  | Error error ->
    Error (Provider_failure_attribution.of_provider_configuration_error error)
  | Ok (base_url, api_key, resolved_headers) ->
    let provider_cfg = provider in
    let provider_id = Provider_runtime_binding.provider_id_of_config provider_cfg in
    let model_spec = Provider.model_spec_of_config provider_cfg in
    let* binding =
      Binding_identity.of_resolved_provider
        ~transport:Binding_identity.Http
        ~provider:provider_cfg
        ~base_url
        ~request_path:model_spec.request_path
        ~api_key
      |> Result.map_error configuration_error
    in
    (match
       Provider.provider_config_of_agent ~state:config ~base_url (Some provider_cfg)
     with
     | Error error ->
       Error (Provider_failure_attribution.of_runtime_binding_error ~binding error)
     | Ok response_provider_config ->
       let response_provider_config =
         { response_provider_config with
           base_url
         ; request_path = model_spec.request_path
         }
       in
       (match model_spec.request_kind with
        | Provider.Anthropic_messages ->
          let headers =
            [ "Content-Type", "application/json"
            ; "x-api-key", api_key
            ; "anthropic-version", Api.api_version
            ]
          in
          (match
             Api.build_body_assoc_result_for_resolved_config
               ~resolved_config:response_provider_config
               ~cache_extended_ttl:config.config.cache_extended_ttl
               ~messages
               ?tools
               ~stream:true
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
             Error
               (Provider_failure_attribution.of_request_validation_error ~binding error)
           | Ok body_assoc ->
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
                 (match
                    Llm_provider.Http_client.read_sse
                      ?clock
                      ?idle_timeout
                      ~reader
                      ~on_data:(fun ~event_type data ->
                        if (not (stream_failed acc)) && data <> "[DONE]"
                        then (
                          match parse_sse_event event_type data with
                          | None ->
                            let evt =
                              SSEParseFailed
                                { raw = data
                                ; reason = "anthropic_sse_chunk_parse_failure"
                                }
                            in
                            on_event evt;
                            accumulate_event acc evt
                          | Some evt ->
                            on_event evt;
                            accumulate_event acc evt);
                        stop_sse_read_if_terminal acc)
                      ()
                  with
                  | () -> ()
                  | exception Stream_finished -> ()
                  | exception Eio.Time.Timeout when stream_failed acc -> ());
                 emit_message_stop_if_missing acc ~on_event;
                 finalize_stream_acc acc)
               ()
             |> map_stream_finalize_result_detailed
                  ~binding
                  ~provider_id
                  ~provider_config:response_provider_config)
        | Provider.Openai_chat_completions ->
          (* OpenAI-compatible SSE streaming. *)
          let auth_headers =
            Provider.auth_headers_only_for_kind
              ~kind:response_provider_config.kind
              ~api_key
          in
          let headers = resolved_headers @ auth_headers in
          let stream_path = model_spec.request_path in
          (match
             Api_openai.build_openai_body_result_for_resolved_config
               ~resolved_config:response_provider_config
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
             Error
               (Provider_failure_attribution.of_request_validation_error ~binding error)
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
                 (match
                    Llm_provider.Http_client.read_sse
                      ?clock
                      ?idle_timeout
                      ~reader
                      ~on_data:(fun ~event_type:_ data ->
                        if not (stream_failed acc)
                        then (
                          match Llm_provider.Streaming.parse_openai_sse_chunk data with
                          | Llm_provider.Streaming.Openai_chunk chunk ->
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
                              Llm_provider.Streaming.openai_chunk_to_events
                                oai_state
                                chunk
                            in
                            List.iter
                              (fun evt ->
                                 on_event evt;
                                 accumulate_event acc evt)
                              evs
                          | Llm_provider.Streaming.Openai_empty -> ()
                          | ( Llm_provider.Streaming.Openai_done
                            | Llm_provider.Streaming.Openai_provider_error _
                            | Llm_provider.Streaming.Openai_parse_failed _ ) as parsed ->
                            let events, _telemetry =
                              Llm_provider.Streaming.openai_sse_parse_result_to_events
                                oai_state
                                parsed
                            in
                            List.iter
                              (fun event ->
                                 on_event event;
                                 accumulate_event acc event)
                              events);
                        stop_sse_read_if_terminal acc)
                      ()
                  with
                  | () -> ()
                  | exception Stream_finished -> ()
                  | exception Eio.Time.Timeout when stream_failed acc -> ());
                 emit_message_stop_if_missing acc ~on_event;
                 finalize_stream_acc acc)
               ()
             |> map_stream_finalize_result_detailed
                  ~binding
                  ~provider_id
                  ~provider_config:response_provider_config)
        | Provider.Custom name ->
          let error =
            Error.Config
              (UnsupportedProvider
                 { detail =
                     Printf.sprintf
                       "custom provider %S does not declare an implemented streaming \
                        codec"
                       name
                 })
          in
          Error (Provider_failure_attribution.of_runtime_binding_error ~binding error)))
;;

let create_message_stream
      ~sw
      ~net
      ?clock
      ?idle_timeout
      ~provider
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
    ~provider
    ~config
    ~messages
    ?tools
    ~on_event
    ()
  |> Result.map_error (fun detailed -> detailed.Provider_failure_attribution.error)
;;
