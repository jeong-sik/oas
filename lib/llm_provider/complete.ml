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

type prepared_request = Prepared_completion_request.t
type measured_request = Prepared_completion_request.measured
type admitted_request = Prepared_completion_request.admitted
type exact_output_plan = Exact_output_plan.t
type plan_fingerprint = Exact_output_plan.fingerprint

type output_admission_error = Exact_output_plan.output_admission_error =
  | Explicit_capability_snapshot_required
  | Contradictory_output_state
  | Unsupported_output_contract of
      { provider_kind : Provider_config.provider_kind
      ; model_id : string
      ; response_format : Types.response_format
      }
  | Unsupported_exact_cross_feature
  | Global_admission_not_allowed
  | Invalid_connect_timeout of float
  | Invalid_body_timeout of float
  | Caller_supplied_framing_header_not_allowed of string
  | Provider_request_rejected of Http_client.http_error
  | Request_serialization_rejected of Http_client.http_error

type json_validation_provenance = Exact_output_plan.json_validation_provenance =
  | Json_syntax_validated
  | Provider_schema_requested_client_validation_required

type normalized_output = Exact_output_plan.normalized_output =
  | Text_output of string
  | Json_output of
      { value : Yojson.Safe.t
      ; validation : json_validation_provenance
      }

type output_normalization_error = Exact_output_plan.output_normalization_error =
  | Incomplete_structured_response of Types.stop_reason
  | Missing_structured_text
  | Ambiguous_structured_text of int
  | Unexpected_structured_content
  | Invalid_json of string

type effect_phase =
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type receipt_identity =
  { fingerprint : plan_fingerprint
  ; request_body_sha256 : string
  }

type response_receipt =
  { identity : receipt_identity
  ; http_status : int
  }

type one_dispatch_receipt =
  | Before_dispatch_receipt of receipt_identity
  | Dispatch_started_receipt of receipt_identity
  | Response_received_receipt of response_receipt
  | Terminal_receipt of response_receipt

type execute_once_error_cause =
  | Clock_required_for_timeout
  | Frozen_request_mismatch
  | Provider_error of Http_client.http_error
  | Output_normalization_failed of output_normalization_error

type execute_once_error =
  { receipt : one_dispatch_receipt
  ; cause : execute_once_error_cause
  }

type pricing_provenance = Pricing_annotation_omitted

type normalized_outcome =
  { receipt : one_dispatch_receipt
  ; response_format : Types.response_format
  ; response : Types.api_response
  ; output : normalized_output
  ; pricing : pricing_provenance
  }

type context_fit = Prepared_completion_request.context_fit =
  { input_tokens : int
  ; reserved_output_tokens : int
  ; max_context_tokens : int
  }

type fit_error = Prepared_completion_request.fit_error =
  | Context_limit_unknown of { model_id : string }
  | Invalid_context_limit of
      { model_id : string
      ; max_context_tokens : int
      }
  | Output_reservation_unknown of { model_id : string }
  | Context_window_exceeded of context_fit

let prepare_request = Prepared_completion_request.prepare
let measure_request = Prepared_completion_request.measure
let request_measurement = Prepared_completion_request.measurement
let resolve_context_limit = Prepared_completion_request.resolve_context_limit
let admit_request = Prepared_completion_request.admit
let admitted_fit = Prepared_completion_request.admitted_fit
let admit_exact_output = Exact_output_plan.admit
let plan_fingerprint = Exact_output_plan.fingerprint
let plan_fingerprint_to_string = Exact_output_plan.fingerprint_to_string
let plan_request_body_sha256 = Exact_output_plan.request_body_sha256

let receipt_phase = function
  | Before_dispatch_receipt _ -> Before_dispatch
  | Dispatch_started_receipt _ -> Dispatch_started
  | Response_received_receipt _ -> Response_received
  | Terminal_receipt _ -> Terminal
;;

let receipt_dispatch_count = function
  | Before_dispatch_receipt _ -> 0
  | Dispatch_started_receipt _ | Response_received_receipt _ | Terminal_receipt _ -> 1
;;

let receipt_http_status = function
  | Before_dispatch_receipt _ | Dispatch_started_receipt _ -> None
  | Response_received_receipt receipt | Terminal_receipt receipt ->
    Some receipt.http_status
;;

let receipt_identity = function
  | Before_dispatch_receipt identity | Dispatch_started_receipt identity -> identity
  | Response_received_receipt receipt | Terminal_receipt receipt -> receipt.identity
;;

let receipt_fingerprint receipt = (receipt_identity receipt).fingerprint
let receipt_request_body_sha256 receipt = (receipt_identity receipt).request_body_sha256

let complete_prepared_sync
      ~sw
      ~net
      ?clock
      ?(transport : Llm_transport.t option)
      ~(prepared : Prepared_completion_request.t)
      ?(cache : Cache.t option)
      ?(connection_cache : Http_client.cache option)
      ?(metrics : Metrics.t option)
      ?body_timeout_s
      ()
  =
  let request = Prepared_completion_request.request prepared in
  let config = request.Llm_transport.config in
  let messages = request.messages in
  let tools = request.tools in
  let preflight =
    match validate_all config with
    | Error err -> Error err
    | Ok () ->
      Http_client.resolve_explicit_deadline
        ~operation:"Complete.complete"
        ~parameter:"body_timeout_s"
        ~clock
        ~timeout_s:body_timeout_s
  in
  match preflight with
  | Error err -> Error err
  | Ok body_deadline ->
    let m =
      match metrics with
      | Some m -> m
      | None -> Metrics.get_global ()
    in
    let model_id = config.model_id in
    let request_config = config in
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
       let dispatch () =
         match transport with
         | Some t ->
           let run_transport () = t.complete_sync request in
           (match body_deadline with
            | Http_client.Unbounded ->
              Http_client.with_explicit_deadline body_deadline run_transport
            | Http_client.Bounded (clock, timeout_s) ->
              (match
                 Eio.Time.with_timeout clock timeout_s (fun () -> Ok (run_transport ()))
               with
               | Ok result -> result
               | Error `Timeout ->
                 { Llm_transport.response =
                     Error
                       (Http_client.TimeoutError
                          { message =
                              Printf.sprintf
                                "body_timeout_s deadline exceeded after %.17gs \
                                 (Complete.complete injected sync transport)"
                                timeout_s
                          ; phase = Http_client.Non_streaming_body
                          })
                 ; latency_ms = None
                 }))
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
       let { Llm_transport.response = result; latency_ms } =
         (* The permit spans the full provider round-trip; cache hits above
            never take one. Waiting for a permit is queueing, not part of the
            provider interaction, so body_timeout_s does not cover it. *)
         Provider_admission.with_admission ~config:request_config dispatch
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
          let resp =
            Pricing.annotate_response_cost ?provider_id:config.provider_id resp
          in
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

let complete
      ~sw
      ~net
      ?clock
      ?transport
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?(trace_context = [])
      ?cache
      ?connection_cache
      ?metrics
      ?body_timeout_s
      ()
  =
  let prepared = prepare_request ~config ~messages ~tools ~trace_context () in
  complete_prepared_sync
    ~sw
    ~net
    ?clock
    ?transport
    ~prepared
    ?cache
    ?connection_cache
    ?metrics
    ?body_timeout_s
    ()
;;

let complete_admitted
      ~sw
      ~net
      ?clock
      ?transport
      admitted
      ?cache
      ?connection_cache
      ?metrics
      ?body_timeout_s
      ()
  =
  complete_prepared_sync
    ~sw
    ~net
    ?clock
    ?transport
    ~prepared:(Prepared_completion_request.admitted_request admitted)
    ?cache
    ?connection_cache
    ?metrics
    ?body_timeout_s
    ()
;;

let execute_once ~net ?clock ?connection_cache plan =
  let fingerprint = plan_fingerprint plan in
  let request_body_sha256 = plan_request_body_sha256 plan in
  let identity = { fingerprint; request_body_sha256 } in
  let before_dispatch_receipt () = Before_dispatch_receipt identity in
  let dispatch_started_receipt () = Dispatch_started_receipt identity in
  let response_received_receipt http_status =
    Response_received_receipt { identity; http_status }
  in
  let terminal_receipt http_status = Terminal_receipt { identity; http_status } in
  let error receipt cause = Error { receipt; cause } in
  let transport_error_receipt = function
    | Http_client.Before_dispatch_error provider_error ->
      before_dispatch_receipt (), provider_error
    | Http_client.Dispatch_started_error provider_error ->
      dispatch_started_receipt (), provider_error
    | Http_client.Response_received_error { status; error = provider_error } ->
      response_received_receipt status, provider_error
  in
  if not (Exact_output_plan.verify_frozen_request plan)
  then error (before_dispatch_receipt ()) Frozen_request_mismatch
  else (
    match
      ( Exact_output_plan.connect_timeout_s plan
      , Exact_output_plan.body_timeout_s plan
      , clock )
    with
    | connect_timeout_s, body_timeout_s, None
      when Option.is_some connect_timeout_s || Option.is_some body_timeout_s ->
      error (before_dispatch_receipt ()) Clock_required_for_timeout
    | connect_timeout_s, body_timeout_s, _ ->
      (match
         Http_client.post_sync_once
           ?cache:connection_cache
           ?clock
           ?connect_timeout_s
           ?body_timeout_s
           ~net
           ~url:(Exact_output_plan.request_url plan)
           ~headers:(Exact_output_plan.request_headers plan)
           ~body:(Exact_output_plan.request_body plan)
           ()
       with
       | Error transport_error ->
         let receipt, provider_error = transport_error_receipt transport_error in
         error receipt (Provider_error provider_error)
       | Ok raw when raw.status < 200 || raw.status >= 300 ->
         error
           (response_received_receipt raw.status)
           (Provider_error
              (Http_client.HttpError
                 { code = raw.status
                 ; body = raw.body
                 ; retry_after_header = raw.retry_after_header
                 }))
       | Ok raw ->
         (match
            Complete_sync.parse_sync_response
              ~http_codec:(Exact_output_plan.response_codec plan)
              ~provider_kind:(Exact_output_plan.provider_kind plan)
              raw.body
          with
          | Error provider_error ->
            error (response_received_receipt raw.status) (Provider_error provider_error)
          | Ok response ->
            (match Exact_output_plan.normalize plan response with
             | Error normalization_error ->
               error
                 (response_received_receipt raw.status)
                 (Output_normalization_failed normalization_error)
             | Ok output ->
               Ok
                 { receipt = terminal_receipt raw.status
                 ; response_format = Exact_output_plan.response_format plan
                 ; response
                 ; output
                 ; pricing = Pricing_annotation_omitted
                 }))))
;;

(* ── Streaming ───────────────────────────────────────── *)

let complete_prepared_stream
      ~sw
      ~net
      ?clock
      ?(transport : Llm_transport.t option)
      ?wire_observer
      ~(prepared : Prepared_completion_request.t)
      ~(on_event : Types.sse_event -> unit)
      ?metrics
      ?(connection_cache : Http_client.cache option)
      ?(on_telemetry : (Telemetry_event.t -> unit) option)
      ()
  =
  let request = Prepared_completion_request.request prepared in
  let config = request.Llm_transport.config in
  let messages = request.messages in
  let tools = request.tools in
  match validate_all config with
  | Error err -> Error err
  | Ok () ->
    let on_event = emit_stream_event on_event in
    let request_config = config in
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
    let emit_wire_observer_failure failure =
      let event = Telemetry_event.Wire_observer_failure failure in
      try
        record_streaming_metrics metrics event;
        match on_telemetry with
        | Some emit -> emit event
        | None ->
          Diag.warn
            "wire_observer"
            "wire observation was not accepted and no telemetry callback is installed: %s"
            (Wire_observer.show_failure failure)
      with
      | exn ->
        Reserved_exn.reraise_if_reserved exn;
        (* Observation diagnostics must not rewrite a completed provider
           interaction as a provider failure. The original typed failure and
           telemetry callback exception both remain visible here. *)
        Diag.warn
          "wire_observer"
          "wire observer failure telemetry callback raised: %s; original=%s"
          (Printexc.to_string exn)
          (Wire_observer.show_failure failure)
    in
    let request =
      match wire_observer with
      | None -> request
      | Some try_observe ->
        let observe_wire_chunk ~provider ~model ~chunk =
          match
            Wire_observer.observe
              try_observe
              ~capture_id:request.capture_id
              ~provider
              ~model
              ~chunk
          with
          | Ok () -> ()
          | Error failure -> emit_wire_observer_failure failure
        in
        { request with observe_wire_chunk = Some observe_wire_chunk }
    in
    let dispatch () =
      match transport with
      | Some t -> t.complete_stream ?on_telemetry:transport_on_telemetry ~on_event request
      | None ->
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s:request.stream_idle_timeout_s
          ?first_event_timeout_s:request.first_event_timeout_s
          ?body_timeout_s:request.body_timeout_s
          ?observe_wire_chunk:request.observe_wire_chunk
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
    let result =
      (* The permit spans the entire stream: the provider holds the
         connection open until the final SSE event, so concurrency
         accounting must too. *)
      Provider_admission.with_admission ~config:request_config dispatch
    in
    Result.map
      (fun resp ->
         let latency_ms = latency_ms_int latency_counter in
         let resp = Pricing.annotate_response_cost ?provider_id:config.provider_id resp in
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

let complete_stream
      ~sw
      ~net
      ?clock
      ?stream_idle_timeout_s
      ?first_event_timeout_s
      ?body_timeout_s
      ?transport
      ?capture_id
      ?wire_observer
      ~(config : Provider_config.t)
      ~(messages : Types.message list)
      ?(tools = [])
      ?(trace_context = [])
      ~on_event
      ?metrics
      ?connection_cache
      ?on_telemetry
      ()
  =
  let prepared =
    prepare_request
      ~config
      ~messages
      ~tools
      ~trace_context
      ?capture_id
      ?stream_idle_timeout_s
      ?first_event_timeout_s
      ?body_timeout_s
      ()
  in
  complete_prepared_stream
    ~sw
    ~net
    ?clock
    ?transport
    ?wire_observer
    ~prepared
    ~on_event
    ?metrics
    ?connection_cache
    ?on_telemetry
    ()
;;

let complete_stream_admitted
      ~sw
      ~net
      ?clock
      ?transport
      ?wire_observer
      admitted
      ~on_event
      ?metrics
      ?connection_cache
      ?on_telemetry
      ()
  =
  complete_prepared_stream
    ~sw
    ~net
    ?clock
    ?transport
    ?wire_observer
    ~prepared:(Prepared_completion_request.admitted_request admitted)
    ~on_event
    ?metrics
    ?connection_cache
    ?on_telemetry
    ()
;;

(* ── HTTP Transport constructor ─────────────────────── *)

let make_http_transport
      ?clock
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
        complete_stream_http
          ~sw
          ~net
          ?clock
          ?stream_idle_timeout_s:req.stream_idle_timeout_s
          ?first_event_timeout_s:req.first_event_timeout_s
          ?body_timeout_s:req.body_timeout_s
          ?observe_wire_chunk:req.observe_wire_chunk
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
