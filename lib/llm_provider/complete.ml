(** Standalone LLM completion: build -> HTTP -> parse.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry
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
      ?runtime_mcp_policy
      ?(trace_context = [])
      ?(cache : Cache.t option)
      ?(connection_cache : Http_client.cache option)
      ?(metrics : Metrics.t option)
      ?(priority : Request_priority.t option)
      ?body_timeout_s
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let _priority = priority in
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
      | Some _ ->
        Some (Cache.request_fingerprint ~config ~messages ~tools ?runtime_mcp_policy ())
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
             ; runtime_mcp_policy
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

(* ── Retry ───────────────────────────────────────────── *)

(* Retry policy classification moved to {!Retry_classify}; re-exports
   below preserve the public surface that [test_complete_ext] imports
   as [Complete.retry_config] / [Complete.is_retryable] etc.  Type
   re-export is non-private so
   record literals built against [Complete.retry_config] continue to
   match [Retry_classify.retry_config]. *)
type retry_config = Retry_classify.retry_config =
  { max_retries : int
  ; initial_delay_sec : float
  ; max_delay_sec : float
  ; backoff_multiplier : float
  }

let default_retry_config = Retry_classify.default_retry_config
let shared_retry_config_of_complete = Retry_classify.shared_retry_config_of_complete
let classify_retry_error = Retry_classify.classify_retry_error
let is_retryable = Retry_classify.is_retryable

let complete_with_retry
      ~sw
      ~net
      ?transport
      ~clock
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?trace_context
      ?(retry_config = default_retry_config)
      ?cache
      ?connection_cache
      ?metrics
      ?priority
      ?body_timeout_s
      ()
  =
  let m = Option.value metrics ~default:(Metrics.get_global ()) in
  let rc = shared_retry_config_of_complete retry_config in
  let provider = Provider_registry.provider_name_of_config config in
  let model_id = config.model_id in
  let f () =
    complete
      ~sw
      ~net
      ~clock
      ?transport
      ~config
      ~messages
      ~tools
      ?runtime_mcp_policy
      ?trace_context
      ?cache
      ?connection_cache
      ~metrics:m
      ?priority
      ?body_timeout_s
      ()
  in
  let rec loop attempt =
    match f () with
    | Ok _ as success -> success
    | Error err ->
      (match classify_retry_error err with
       | Some api_err when Retry.is_retryable api_err ->
         if attempt >= rc.max_retries
         then Error err
         else (
           Diag.warn
             "complete"
             "retrying provider %s model %s (attempt %d/%d) after error: %s"
             provider
             model_id
             (attempt + 1)
             rc.max_retries
             (Retry.error_message api_err);
           m.on_retry ~provider ~model_id ~attempt:(attempt + 1);
           let delay =
             match api_err with
             | Retry.RateLimited { retry_after = Some ra; _ } -> ra
             | Retry.RateLimited { retry_after = None; _ }
             | Retry.Overloaded _
             | Retry.ServerError _
             | Retry.AuthError _
             | Retry.PaymentRequired _
             | Retry.InvalidRequest _
             | Retry.NotFound _
             | Retry.ContextOverflow _
             | Retry.NetworkError _
             | Retry.Timeout _ -> Retry.calculate_delay rc attempt
           in
           Eio.Time.sleep clock delay;
           loop (attempt + 1))
       | Some _ | None -> Error err)
  in
  loop 0
;;

(* ── Streaming ───────────────────────────────────────── *)

let complete_stream
      ~sw
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?(transport : Llm_transport.t option)
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?(trace_context = [])
      ~(on_event : Types.sse_event -> unit)
      ?metrics
      ?(priority : Request_priority.t option)
      ?(connection_cache : Http_client.cache option)
      ?(on_telemetry : (Telemetry_event.t -> unit) option)
      ()
  =
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let on_event = emit_stream_event on_event in
    let _priority = priority in
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
          ; runtime_mcp_policy
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

let complete_stream_with_retry
      ~sw
      ~net
      ?transport
      ~clock
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?runtime_mcp_policy
      ?trace_context
      ?(retry_config = default_retry_config)
      ~on_event
      ?metrics
      ?priority
      ?connection_cache
      ?stream_idle_timeout_s
      ?on_telemetry
      ()
  =
  let m = Option.value metrics ~default:(Metrics.get_global ()) in
  let rc = shared_retry_config_of_complete retry_config in
  let provider = Provider_registry.provider_name_of_config config in
  let model_id = config.model_id in
  let f () =
    complete_stream
      ~sw
      ~net
      ~clock
      ?transport
      ~config
      ~messages
      ~tools
      ?runtime_mcp_policy
      ?trace_context
      ~on_event
      ~metrics:m
      ?priority
      ?connection_cache
      ?stream_idle_timeout_s
      ?on_telemetry
      ()
  in
  let rec loop attempt =
    match f () with
    | Ok _ as success -> success
    | Error err ->
      (match classify_retry_error err with
       | Some api_err when Retry.is_retryable api_err ->
         if attempt >= rc.max_retries
         then Error err
         else (
           Diag.warn
             "complete"
             "retrying stream provider %s model %s (attempt %d/%d) after error: %s"
             provider
             model_id
             (attempt + 1)
             rc.max_retries
             (Retry.error_message api_err);
           m.on_retry ~provider ~model_id ~attempt:(attempt + 1);
           let delay =
             match api_err with
             | Retry.RateLimited { retry_after = Some ra; _ } -> ra
             | Retry.RateLimited { retry_after = None; _ }
             | Retry.Overloaded _
             | Retry.ServerError _
             | Retry.AuthError _
             | Retry.PaymentRequired _
             | Retry.InvalidRequest _
             | Retry.NotFound _
             | Retry.ContextOverflow _
             | Retry.NetworkError _
             | Retry.Timeout _ -> Retry.calculate_delay rc attempt
           in
           Eio.Time.sleep clock delay;
           loop (attempt + 1))
       | Some _ | None -> Error err)
  in
  loop 0
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

(* ── Streaming Completion ───────────────────────── *)

[@@@coverage off]
(* === Inline tests === *)

let%test "is_retryable 429 rate limit" =
  is_retryable (Http_client.HttpError { code = 429; body = "" }) = true
;;

let%test "is_retryable 429 hard quota is false" =
  not
    (is_retryable
       (Http_client.HttpError
          { code = 429
          ; body =
              {|{"error":{"message":"Insufficient balance or no resource package. Please recharge.","retry_after":5.0}}|}
          }))
;;

let%test "is_retryable 500 server error" =
  is_retryable (Http_client.HttpError { code = 500; body = "" }) = true
;;

let%test "is_retryable 502 bad gateway" =
  is_retryable (Http_client.HttpError { code = 502; body = "" }) = true
;;

let%test "is_retryable 503 service unavailable" =
  is_retryable (Http_client.HttpError { code = 503; body = "" }) = true
;;

let%test "is_retryable 529 overloaded" =
  is_retryable (Http_client.HttpError { code = 529; body = "" }) = true
;;

let%test "is_retryable 400 not retryable" =
  is_retryable (Http_client.HttpError { code = 400; body = "" }) = false
;;

let%test "is_retryable 400 provider malformed-json prose is false" =
  not
    (is_retryable
       (Http_client.HttpError
          { code = 400
          ; body =
              {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
          }))
;;

let%test "is_retryable provider parse error is false" =
  not
    (is_retryable
       (Http_client.ProviderFailure
          { kind = Http_client.Provider_parse_error { parser = Some "glm" }
          ; message = "Unexpected end of input"
          }))
;;

let%test "is_retryable 401 not retryable" =
  is_retryable (Http_client.HttpError { code = 401; body = "" }) = false
;;

let%test "is_retryable 404 not retryable" =
  is_retryable (Http_client.HttpError { code = 404; body = "" }) = false
;;

let%test "is_retryable network error always retryable" =
  is_retryable
    (Http_client.NetworkError { message = "connection refused"; kind = Unknown })
  = true
;;

let%test "is_retryable provider capacity failure is false" =
  not
    (is_retryable
       (Http_client.ProviderFailure
          { kind =
              Http_client.Capacity_exhausted
                { scope = Http_client.Failure_scope_model
                ; retry_after = None
                ; model = Some "gemini-2.5-pro"
                }
          ; message = "capacity exhausted"
          }))
;;

let%test "is_retryable provider hard quota failure is false" =
  not
    (is_retryable
       (Http_client.ProviderFailure
          { kind = Http_client.Hard_quota { retry_after = Some 7603.424 }
          ; message = "terminal quota exhausted"
          }))
;;

let%test "default_retry_config values" =
  default_retry_config.max_retries = 3
  && default_retry_config.initial_delay_sec = 1.0
  && default_retry_config.max_delay_sec = 30.0
  && default_retry_config.backoff_multiplier = 2.0
;;
