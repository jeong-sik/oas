(** Standalone LLM completion: build request, send via transport, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming
    @since 0.54.0  Optional cache + metrics hooks
    @since 0.78.0  Transport abstraction

    @stability Internal
    @since 0.93.1 *)

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

    @since 0.78.0 *)
val make_http_transport
  :  ?clock:_ Eio.Time.clock
  -> ?stream_idle_timeout_s:float
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
(** [body_timeout_s] is the exact caller-owned deadline, in seconds, for a
    non-streaming transport call after a cache miss. It must be finite and
    greater than zero and requires [clock]. The contract is validated before
    cache lookup, so an invalid value or missing clock is rejected as [Error
    (AcceptRejected _)] even when a cached response exists.

    With no injected [transport], the built-in HTTP path owns the single
    deadline around [Http_client.post_sync]; [complete] does not add a second
    timeout. With an injected [transport], [complete] applies the resolved
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

    [clock] and [stream_idle_timeout_s] together bound two streaming
    stalls on every HTTP streaming path: inter-line idle, and
    thinking-only generation before the first deliverable text/tool
    signal. The line-idle deadline covers Ollama native NDJSON
    (see {!Http_client.read_ndjson}) and the SSE format used by
    Anthropic, OpenAI-compatible, Gemini, and Glm
    (see {!Http_client.read_sse}). The deadline resets after each
    successful line. The thinking-only guard does not reset on hidden
    reasoning deltas; it is cleared by text or tool-call progress.
    Together these do not cap total stream duration after answer/tool
    progress has started.
    SSE keepalive comments reset the deadline like any other line.
    A stalled endpoint surfaces as
    [TimeoutError { phase = Stream_idle state; _ }], where [state]
    records whether the stream was waiting for the first event, answer
    deltas, thinking deltas, tool-call deltas, heartbeat/substrate, or
    completion. The typed failure is returned unchanged so downstream
    orchestration can distinguish streaming/thinking idleness from total-call
    deadlines and schedule any later attempt independently. Non-HTTP transports
    (CLI subprocess) ignore
    [stream_idle_timeout_s]. *)
val complete_stream
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?stream_idle_timeout_s:float
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
