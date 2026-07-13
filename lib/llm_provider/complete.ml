(** Standalone LLM completion: build -> HTTP -> parse.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming
    @since 0.54.0  Optional cache + metrics hooks *)

include Complete_sampling
include Complete_common
include Complete_sync
include Complete_stream

let complete
      ~sw
      ~net
      ?clock
      ?(transport : Llm_transport.t option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?(trace_context = [])
      ?(cache : Cache.t option)
      ?(connection_cache : Http_client.cache option)
      ?(metrics : Metrics.t option)
      ?body_timeout_s
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let m =
      match metrics with
      | Some m -> m
      | None -> Metrics.get_global ()
    in
    let model_id = config.model_id in
    let request_config = config_with_trace_context config trace_context in
    (* Cache lookup *)
    (* Compute fingerprint once; reuse for both lookup and store *)
    let cache_key =
      match cache with
      | Some _ -> Some (Cache.request_fingerprint ~config ~messages ~tools ())
      | None -> None
    in
    let cached =
      match cache, cache_key with
      | Some c, Some key ->
        (match c.get ~key with
         | Some json ->
           (match Cache.response_of_json json with
            | Some resp ->
              m.on_cache_hit ~model_id;
              Some (Ok resp)
            | None ->
              m.on_cache_miss ~model_id;
              None)
         | None ->
           m.on_cache_miss ~model_id;
           None)
      | _, _ -> None
    in
    (match cached with
     | Some result -> ensure_nonempty_completion result
     | None ->
       m.on_request_start ~model_id;
       let { Llm_transport.response = result; latency_ms } =
         match transport with
         | Some t ->
           t.complete_sync
             { Llm_transport.config = request_config
             ; messages
             ; tools
             ; capture_id = None
             ; stream_idle_timeout_s = None (* sync path: no streaming idle deadline *)
             }
         | None ->
           let resp, lat =
             complete_http
               ~sw
               ~net
               ?clock
               ~on_http_status:m.on_http_status
               ?body_timeout_s
               ?connection_cache
               ~config:request_config
               ~messages
               ~tools
               ()
           in
           { Llm_transport.response = resp; latency_ms = lat }
       in
       (* HTTP-backed transports bypass complete_http, so emit the status
         here using the transport result. Non-HTTP CLI transports must
         stay silent because they never observed an HTTP status code. *)
       if Option.is_some transport && not (requires_non_http_transport config.kind)
       then (
         match result with
         | Ok _ ->
           m.on_http_status
             ~provider:(Provider_registry.provider_name_of_config config)
             ~model_id
             ~status:200
         | Error (Http_client.HttpError { code; _ }) ->
           m.on_http_status
             ~provider:(Provider_registry.provider_name_of_config config)
             ~model_id
             ~status:code
         | Error _ -> ());
       let result = ensure_nonempty_completion result in
       (match result with
        | Ok resp ->
          let resp = Pricing.annotate_response_cost resp in
          let resp = patch_telemetry resp ~config latency_ms in
          m.on_request_end ~model_id ~latency_ms;
          emit_tool_call_metrics
            m
            ~provider:(Provider_registry.provider_name_of_config config)
            ~model_id
            resp;
          (match resp.usage with
           | Some u ->
             m.on_token_usage
               ~provider:(Provider_registry.provider_name_of_config config)
               ~model_id
               ~input_tokens:u.input_tokens
               ~output_tokens:u.output_tokens
           | None -> ());
          (* Cache store — reuse pre-computed key *)
          (match cache, cache_key with
           | Some c, Some key ->
             let json = Cache.response_to_json resp in
             (try c.set ~key ~ttl_sec:Constants.Cache.default_ttl_sec json with
              | (Eio.Io _ | Sys_error _) as exn ->
                Diag.warn
                  "complete"
                  "cache set failed for key %s: %s"
                  key
                  (Printexc.to_string exn))
           | _, _ -> ());
          Ok resp
        | Error err ->
          let err_str =
            match err with
            | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.NetworkError { message; _ } -> message
            | Http_client.TimeoutError { message; _ } -> message
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { kind; message } ->
              Http_client.provider_failure_to_string ~kind ~message
          in
          m.on_error ~model_id ~error:err_str;
          Error err))
;;

(* ── Streaming ───────────────────────────────────────── *)

let complete_stream
      ~sw
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?(transport : Llm_transport.t option)
      ?capture_id
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?(trace_context = [])
      ~(on_event : Types.sse_event -> unit)
      ?metrics
      ?(connection_cache : Http_client.cache option)
      ?(on_telemetry : (Telemetry_event.t -> unit) option)
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let on_event = emit_stream_event on_event in
    let request_config = config_with_trace_context config trace_context in
    let latency_counter = start_latency_counter ?clock () in
    let metrics_opt = metrics in
    let metrics = Option.value metrics ~default:(Metrics.get_global ()) in
    let on_telemetry_with_metrics evt =
      record_streaming_metrics metrics evt;
      match on_telemetry with
      | Some f -> f evt
      | None -> ()
    in
    let transport_on_telemetry =
      match metrics_opt, on_telemetry with
      | None, None -> None
      | Some _, _ | None, Some _ -> Some on_telemetry_with_metrics
    in
    let result =
      match transport with
      | Some t ->
        t.complete_stream
          ?on_telemetry:transport_on_telemetry
          ~on_event
          { Llm_transport.config = request_config
          ; messages
          ; tools
          ; capture_id
          ; stream_idle_timeout_s
            (* RFC-OAS-026: carry the idle deadline through the transport
               boundary so the [Some t] dispatch can no longer drop it. *)
          }
      | None ->
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s
          ?capture_id
          ~latency_counter
          ?on_telemetry
          ~metrics
          ?connection_cache
          ~config:request_config
          ~messages
          ~tools
          ~on_event
          ()
    in
    Result.map
      (fun resp ->
         let latency_ms = latency_ms_int latency_counter in
         let resp = Pricing.annotate_response_cost resp in
         let existing_telemetry = resp.telemetry in
         let ttfrc_ms = Option.bind existing_telemetry (fun t -> t.ttfrc_ms) in
         let prefill_ms = Option.bind existing_telemetry (fun t -> t.prefill_ms) in
         let resp = patch_telemetry resp ~config ~ttfrc_ms ~prefill_ms latency_ms in
         emit_tool_call_metrics
           metrics
           ~provider:(Provider_registry.provider_name_of_config config)
           ~model_id:config.model_id
           resp;
         resp)
      (ensure_nonempty_completion result)
;;

(* ── HTTP Transport constructor ─────────────────────── *)

let make_http_transport
      ?clock
      ?stream_idle_timeout_s
      ?body_timeout_s
      ?(connection_cache : Http_client.cache option)
      ?latency_counter
      ~sw
      ~net
      ()
  : Llm_transport.t
  =
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        let response, latency_ms =
          complete_http
            ~sw
            ~net
            ?clock
            ?body_timeout_s
            ?connection_cache
            ~config:req.config
            ~messages:req.messages
            ~tools:req.tools
            ()
        in
        { Llm_transport.response; latency_ms })
  ; complete_stream =
      (fun ?on_telemetry ~on_event (req : Llm_transport.completion_request) ->
        (* RFC-OAS-026: the request-borne idle deadline is authoritative;
           fall back to the construction-time value for callers that have not
           migrated to setting it on the request. *)
        let stream_idle_timeout_s =
          match req.stream_idle_timeout_s with
          | Some _ as v -> v
          | None -> stream_idle_timeout_s
        in
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s
          ?capture_id:req.capture_id
          ?connection_cache
          ?latency_counter
          ~config:req.config
          ~messages:req.messages
          ~tools:req.tools
          ~on_event
          ?on_telemetry
          ())
  }
;;
