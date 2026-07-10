(** Standalone LLM completion: build request, send via transport, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry
    @since 0.54.0  Optional cache + metrics hooks
    @since 0.78.0  Transport abstraction
    @since 0.95.0  Optional request priority (accepted, not yet used for scheduling)

    @stability Internal
    @since 0.93.1 *)

(** {1 Gemini URL Construction} *)

(** Construct Gemini API URL with model_id in path and optional key param.
    Exposed for testing. *)
val gemini_url : config:Provider_config.t -> stream:bool -> string

(** {1 Provider Sampling Defaults} *)

(** Sampling parameter defaults per provider kind. *)
type sampling_defaults =
  { default_min_p : float option
  ; default_top_p : float option
  ; default_top_k : int option
  }

(** Get default sampling parameters for a provider kind.
    Local (OpenAI_compat) providers get min_p=0.05.
    Anthropic/Gemini get no defaults (all None). *)
val provider_sampling_defaults : Provider_config.provider_kind -> sampling_defaults

(** Apply provider defaults to a config, preserving explicit values.
    Only fills in [None] fields; explicit values are never overwritten.
    For [OpenAI_compat], [min_p] is auto-filled only when the target
    model (or an unknown localhost endpoint) supports it. *)
val apply_sampling_defaults : Provider_config.t -> Provider_config.t

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
  -> ?runtime_mcp_policy:Llm_transport.runtime_mcp_policy
  -> ?trace_context:(string * string) list
  -> ?cache:Cache.t
  -> ?connection_cache:Http_client.cache
  -> ?metrics:Metrics.t
  -> ?priority:Request_priority.t
  -> ?body_timeout_s:float
  -> unit
  -> (Types.api_response, Http_client.http_error) result
(** [body_timeout_s] caps the total HTTP round-trip time, in seconds,
    on the non-streaming [Http_client.post_sync] path inside [complete].
    Requires [clock]; without one the wrapper is skipped and behaviour
    matches versions < 0.195.0. On expiry the result is
    [Error (TimeoutError { phase = Non_streaming_body; _ })] with a
    message that identifies the body deadline so retry treats it
    as retryable while operators retain attribution.

    Distinct from {!complete_stream}'s [stream_idle_timeout_s] (which
    has no analogue here — there are no intermediate lines to count).
    Non-HTTP transports (CLI subprocess, custom registered) ignore
    [body_timeout_s]. @since 0.195.0 *)

(** {1 Retry} *)

(** Retry configuration with exponential backoff. *)
type retry_config =
  { max_retries : int
  ; initial_delay_sec : float
  ; max_delay_sec : float
  ; backoff_multiplier : float
  }

val default_retry_config : retry_config

(** Classify whether an HTTP error is worth retrying.
    Uses {!Retry.classify_error} for HTTP bodies, so hard-quota 429s
    fail fast while malformed-JSON 400s remain retryable. *)
val is_retryable : Http_client.http_error -> bool

(** Completion with exponential backoff retry.
    Passes [transport], [cache], [connection_cache] and [metrics]
    through to each attempt. *)
val complete_with_retry
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?transport:Llm_transport.t
  -> clock:_ Eio.Time.clock
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?runtime_mcp_policy:Llm_transport.runtime_mcp_policy
  -> ?trace_context:(string * string) list
  -> ?retry_config:retry_config
  -> ?cache:Cache.t
  -> ?connection_cache:Http_client.cache
  -> ?metrics:Metrics.t
  -> ?priority:Request_priority.t
  -> ?body_timeout_s:float
  -> unit
  -> (Types.api_response, Http_client.http_error) result
(** [body_timeout_s] is forwarded to each underlying {!complete} call.
    Each retry attempt gets a fresh deadline; the parameter does not
    cap the total time across all attempts. @since 0.195.0 *)

(** {1 Streaming Completion} *)

(** Execute a streaming LLM completion.
    Each SSE event is passed to [on_event] as it arrives.
    Returns the final assembled {!Types.api_response} after the stream ends.
    Non-fatal exceptions raised by [on_event] are logged and do not abort the
    stream assembly.

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
    completion. Retry layers treat this as retryable while
    downstream policy can distinguish streaming/thinking idleness from
    total-call deadlines. Non-HTTP transports (CLI subprocess) ignore
    [stream_idle_timeout_s]. *)
val complete_stream
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?stream_idle_timeout_s:float
  -> ?transport:Llm_transport.t
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?runtime_mcp_policy:Llm_transport.runtime_mcp_policy
  -> ?trace_context:(string * string) list
  -> on_event:(Types.sse_event -> unit)
  -> ?metrics:Metrics.t
  -> ?priority:Request_priority.t
  -> ?connection_cache:Http_client.cache
  -> ?on_telemetry:(Telemetry_event.t -> unit)
  -> unit
  -> (Types.api_response, Http_client.http_error) result

(** Streaming completion with exponential backoff retry.
    Passes [transport], [connection_cache] and [metrics] through to each attempt. *)
val complete_stream_with_retry
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?transport:Llm_transport.t
  -> clock:_ Eio.Time.clock
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?runtime_mcp_policy:Llm_transport.runtime_mcp_policy
  -> ?trace_context:(string * string) list
  -> ?retry_config:retry_config
  -> on_event:(Types.sse_event -> unit)
  -> ?metrics:Metrics.t
  -> ?priority:Request_priority.t
  -> ?connection_cache:Http_client.cache
  -> ?stream_idle_timeout_s:float
  -> ?on_telemetry:(Telemetry_event.t -> unit)
  -> unit
  -> (Types.api_response, Http_client.http_error) result
