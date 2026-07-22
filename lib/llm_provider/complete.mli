(** Standalone LLM completion: build request, send via transport, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming
    @since 0.54.0  Optional cache + metrics hooks
    @since 0.78.0  Transport abstraction

    @stability Internal
    @since 0.93.1 *)

(** {1 Canonical Prepared Request} *)

(** One opaque completion request after all caller-owned projection. *)
type prepared_request

(** The same request paired with provider-native measurement evidence. *)
type measured_request

(** The measured request after its declared context window admits it. *)
type admitted_request

type context_fit =
  { input_tokens : int
  ; reserved_output_tokens : int
  ; max_context_tokens : int
  }

type fit_error =
  | Context_limit_unknown of { model_id : string }
  | Invalid_context_limit of
      { model_id : string
      ; max_context_tokens : int
      }
  | Output_reservation_unknown of { model_id : string }
  | Context_window_exceeded of context_fit

(** Build the single request value used by measurement and dispatch. Existing
    [complete] and [complete_stream] calls are compatibility wrappers over this
    constructor; they no longer own a separate request projection. *)
val prepare_request
  :  config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> ?capture_id:string
  -> ?stream_idle_timeout_s:float
  -> ?first_event_timeout_s:float
  -> ?body_timeout_s:float
  -> unit
  -> prepared_request

(** Validate and measure the exact prepared request through the provider-native
    count protocol. Invalid local configuration fails before admission or I/O;
    the count round-trip uses the same provider admission authority as
    completion dispatch. Unsupported protocols return the existing typed
    [Unsupported] measurement error; no estimate is used. *)
val measure_request
  :  ?connection_cache:Http_client.cache
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> prepared_request
  -> (measured_request, Count_tokens_sync.completion_request_error) result

val request_measurement
  :  measured_request
  -> Count_tokens_sync.completion_request_measurement

(** Resolve the validated positive context-token limit from the explicit
    [max_context] config value, or the exact model capability when none was
    supplied. Pure: performs no measurement I/O. [Context_limit_unknown] when no
    limit is declared, [Invalid_context_limit] when it is non-positive. Callers
    resolve this before measurement so a pre-knowable limit failure never costs a
    count round-trip. *)
val resolve_context_limit : prepared_request -> (int, fit_error) result

(** Admit the measured request against the [max_context_tokens] resolved by
    {!resolve_context_limit}. The output-token reservation is the effective value
    carried by the same provider request artifact. A missing reservation and
    context overflow are explicit. *)
val admit_request
  :  max_context_tokens:int
  -> measured_request
  -> (admitted_request, fit_error) result

val admitted_fit : admitted_request -> context_fit

(** {1 Exact Single-provider Output} *)

(** Opaque immutable and reusable plan for one already selected provider slot.
    Each execution is an independent attempt with at most one dispatch. *)
type exact_output_plan
type plan_fingerprint

type output_admission_error =
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

type json_validation_provenance =
  | Json_syntax_validated
  | Provider_schema_requested_client_validation_required

type normalized_output =
  | Text_output of string
  | Json_output of
      { value : Yojson.Safe.t
      ; validation : json_validation_provenance
      }

type output_normalization_error =
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

val receipt_phase : one_dispatch_receipt -> effect_phase
val receipt_dispatch_count : one_dispatch_receipt -> int
val receipt_http_status : one_dispatch_receipt -> int option
val receipt_fingerprint : one_dispatch_receipt -> plan_fingerprint
val receipt_request_body_sha256 : one_dispatch_receipt -> string

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

(** Output admission for the exact config and explicit capability snapshot
    owned by [admitted_request]. Admission validates then freezes the actual URL,
    final application headers, provider codec, serialized body bytes, and
    connect/body deadlines. It serializes exactly once. Tool-use and reasoning
    controls/history are outside this exact surface and are rejected before
    completion dispatch. The input is already an [admitted_request], so its
    provider-native count-token measurement may have performed I/O earlier;
    admission errors promise zero completion dispatches, not zero provider I/O.

    [JsonSchema] means the selected provider serializer emitted its native
    schema request. Locally, OAS validates only returned JSON syntax; callers
    must perform JSON-Schema instance and domain validation.

    A provider response may still contain reasoning blocks. They remain intact
    in [normalized_outcome.response] but never contribute bytes to text or JSON
    normalization: exactly one [Text] block is selected, without concatenating
    reasoning or any other content block. *)
val admit_exact_output
  :  admitted_request
  -> (exact_output_plan, output_admission_error) result

val plan_fingerprint : exact_output_plan -> plan_fingerprint
val plan_fingerprint_to_string : plan_fingerprint -> string
val plan_request_body_sha256 : exact_output_plan -> string

(** Execute only the frozen request. Execution performs no serialization,
    config validation, capability/catalog/pricing lookup, response-cache access,
    injected transport call, retry, fallback, or metrics lookup. One invocation
    issues at most one OAS-owned provider dispatch. The immutable plan is
    reusable; two invocations may therefore issue two independent dispatches.

    [body_timeout_s] frozen by admission is a total HTTP round-trip deadline
    across connect, response headers, and response body. [?connection_cache]
    permits transport connection reuse only. It cannot
    change URL, headers, body bytes, codec, deadlines, or dispatch count.
    Pricing annotation is intentionally omitted and identified by
    [Pricing_annotation_omitted]; measurement may add it out of band. *)
val execute_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?connection_cache:Http_client.cache
  -> exact_output_plan
  -> (normalized_outcome, execute_once_error) result

(** {1 Gemini URL Construction} *)

(** Construct Gemini API URL with model_id in path and optional key param.
    Exposed for testing. *)
val gemini_url : config:Provider_config.t -> stream:bool -> string

(** {1 Transport} *)

(** Opaque handle for a monotonic latency measurement.
    See {!Complete_common.start_latency_counter} for creation. *)
type latency_counter = Complete_common.latency_counter

(** Create an HTTP-based transport.
    Wraps the internal HTTP completion pipeline into a
    {!Llm_transport.t} value that can be passed to [complete]
    or [complete_stream] via [?transport].

    When [connection_cache] is supplied, the transport reuses idle
    HTTP connections and parks them back after each request.

    When [latency_counter] is supplied, the transport's streaming
    path shares that counter instead of allocating its own. The
    per-request [complete_stream] entry point already shares its
    counter for the direct-HTTP path; this parameter is useful when
    constructing a transport that should also share a counter with
    its caller. Arbitrary non-HTTP transports still use their own
    latency measurement because [Llm_transport.t.complete_stream]
    does not accept a counter.

    Streaming idle deadlines are request-owned. Configure them through
    [complete_stream] or the agent option that populates
    [Llm_transport.completion_request.stream_idle_timeout_s]; the transport
    constructor does not carry a competing timeout value.

    @since 0.78.0 *)
val make_http_transport
  :  ?clock:_ Eio.Time.clock
  -> ?body_timeout_s:float
  -> ?connection_cache:Http_client.cache
  -> ?latency_counter:latency_counter
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> unit
  -> Llm_transport.t

(** {1 Sync Completion} *)

(** Execute a single LLM completion round-trip.

    When [transport] is provided, uses that transport for I/O.
    Otherwise falls back to the built-in HTTP transport.

    When [cache] is provided, checks response cache before I/O and stores on success.
    When [connection_cache] is provided, the built-in HTTP transport reuses idle
    connections. It has no effect when a custom [transport] is supplied.
    When [metrics] is provided, fires lifecycle callbacks.

    @return [Ok api_response] on success (possibly from cache)
    @return [Error http_error] on failure. A response with no content blocks
    fails closed as [ProviderFailure { kind = Empty_completion { stop_reason } }]
    for built-in HTTP, cache, and injected transports alike. *)
val complete
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?transport:Llm_transport.t
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> ?cache:Cache.t
  -> ?connection_cache:Http_client.cache
  -> ?metrics:Metrics.t
  -> ?body_timeout_s:float
  -> unit
  -> (Types.api_response, Http_client.http_error) result

(** Dispatch an already measured and admitted request. The transport receives
    the request owned by [admitted_request]; callers cannot substitute config,
    messages, tools, or trace context after measurement. *)
val complete_admitted
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?transport:Llm_transport.t
  -> admitted_request
  -> ?cache:Cache.t
  -> ?connection_cache:Http_client.cache
  -> ?metrics:Metrics.t
  -> ?body_timeout_s:float
  -> unit
  -> (Types.api_response, Http_client.http_error) result
(** [body_timeout_s] is the exact caller-owned deadline, in seconds, for a
    non-streaming transport call after a cache miss. It must be finite and
    greater than zero and requires [clock]. The contract is validated before
    cache lookup, so an invalid value or missing clock is rejected as [Error
    (AcceptRejected _)] even when a cached response exists.

    With no injected [transport], the built-in HTTP path owns the single
    deadline around [Http_client.post_sync]; [config.connect_timeout_s] is not
    added as a nested sync deadline. With an injected [transport], [complete] applies the resolved
    deadline around [Llm_transport.t.complete_sync]. On expiry the result is
    [Error (TimeoutError { phase = Non_streaming_body; _ })] with a
    message that identifies the body deadline. The typed failure is returned
    unchanged so the caller can schedule any later attempt independently.
    Only expiry of this outer deadline is projected that way; an exception
    raised by an injected transport is not relabelled as caller-deadline expiry.

    Distinct from {!complete_stream}'s [stream_idle_timeout_s] (which
    has no analogue here — there are no intermediate lines to count).
    Omitting this argument adds no caller-owned deadline around either sync
    path; an injected transport may still enforce its own internal contract.
    @since 0.195.0 *)

(** {1 Streaming Completion} *)

(** Execute a streaming LLM completion.
    Each SSE event is passed to [on_event] as it arrives.
    Returns the final assembled {!Types.api_response} after the stream ends.
    Non-fatal exceptions raised by [on_event] are logged and do not abort the
    stream assembly.

    [capture_id], when present, is the exact caller-owned request/run identity
    carried through injected transports and the built-in HTTP path. Raw-wire
    observation never synthesizes an identity when it is absent.

    [wire_observer], when present, receives provider chunks only after
    {!Wire_observer} best-effort redaction. It is a caller-owned synchronous
    nonblocking offer: OAS owns no queue, persistence, capacity, retry, path,
    or worker. Rejection and ordinary callback exceptions are emitted as typed
    {!Telemetry_event.Wire_observer_failure} observations without changing the
    provider result. Without [on_telemetry], failures are written to the
    diagnostic sink instead of disappearing silently.

    The built-in HTTP transport offers every raw provider chunk. An injected
    streaming transport receives only an OAS-owned
    {!Llm_transport.completion_request.observe_wire_chunk} sink, never the
    caller callback; it must call that sink for every raw provider chunk if it
    participates in wire observation. OAS therefore retains redaction and
    failure handling even for a custom transport, while the custom transport
    retains responsibility for identifying its raw chunk boundary.

    Redaction is diagnostic sanitization rather than proof that an observation
    is non-sensitive. Callers must retain observations as sensitive data.

    Supports both Anthropic native SSE and OpenAI-compatible SSE formats,
    dispatched by {!Provider_config.t.kind}.

    A finalized response with no content blocks fails closed as
    [ProviderFailure { kind = Empty_completion { stop_reason } }] for built-in
    HTTP and injected transports alike.

    [clock] and [stream_idle_timeout_s] bound inter-line stalls on every HTTP
    streaming path. The idle deadline covers Ollama native NDJSON
    (see {!Http_client.read_ndjson}) and the SSE format used by
    Anthropic, OpenAI-compatible, Gemini, and Glm
    (see {!Http_client.read_sse}). The deadline resets after each
    successful line. Thinking, answer, tool-call, heartbeat, substrate, and
    terminal lines are all liveness; there is no thinking-only or total stream
    wall-clock cutoff.
    SSE keepalive comments reset the deadline like any other line.
    A stalled endpoint surfaces as
    [TimeoutError { phase = Stream_idle state; _ }], where [state]
    records whether the stream was waiting for the first event, answer
    deltas, thinking deltas, tool-call deltas, heartbeat/substrate, or
    completion. The typed failure is returned unchanged so downstream
    orchestration can distinguish streaming/thinking idleness from total-call
    deadlines and schedule any later attempt independently. Non-HTTP transports
    (CLI subprocess) ignore
    [stream_idle_timeout_s].

    RFC-OAS-037: [first_event_timeout_s], when set, bounds the wait for the
    FIRST streaming event separately from [stream_idle_timeout_s]. Until the
    first event arrives the read is bounded by [first_event_timeout_s];
    [stream_idle_timeout_s] arms for inter-token idle only AFTER the first
    event. This prevents a slow-but-alive silent prefill on a large context
    (no keepalives) from being cancelled as [phase=first_token] under the
    short inter-token idle value. When omitted the first-event wait falls back
    to [body_timeout_s] (below), then to [stream_idle_timeout_s] — the bound
    that applied before this change — and stays unarmed when the caller wired
    none of the three. Inter-token idle still guards once the stream produces,
    and the connect timeout still guards connection setup.

    RFC-OAS-037 §4.2: [body_timeout_s] is the total body budget also used by
    the non-streaming path. On the streaming path it is the fallback bound for
    the first-event wait when [first_event_timeout_s] is [None] — the common
    production shape (callers wire [body_timeout_s], not
    [first_event_timeout_s]). *)
val complete_stream
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?stream_idle_timeout_s:float
  -> ?first_event_timeout_s:float
  -> ?body_timeout_s:float
  -> ?transport:Llm_transport.t
  -> ?capture_id:string
  -> ?wire_observer:Wire_observer.try_observe
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> on_event:(Types.sse_event -> unit)
  -> ?metrics:Metrics.t
  -> ?connection_cache:Http_client.cache
  -> ?on_telemetry:(Telemetry_event.t -> unit)
  -> unit
  -> (Types.api_response, Http_client.http_error) result

(** Streaming counterpart of {!complete_admitted}. Capture identity and idle
    deadline are fixed when the request is prepared. Wire observation remains
    an OAS-owned operational sink and cannot alter provider payload fields. *)
val complete_stream_admitted
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?transport:Llm_transport.t
  -> ?wire_observer:Wire_observer.try_observe
  -> admitted_request
  -> on_event:(Types.sse_event -> unit)
  -> ?metrics:Metrics.t
  -> ?connection_cache:Http_client.cache
  -> ?on_telemetry:(Telemetry_event.t -> unit)
  -> unit
  -> (Types.api_response, Http_client.http_error) result
