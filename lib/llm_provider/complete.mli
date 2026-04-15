(** Standalone LLM completion: build request, send via transport, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Consumers can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry, cascade
    @since 0.54.0  Optional cache + metrics hooks
    @since 0.78.0  Transport abstraction
    @since 0.95.0  Optional request priority (accepted, not yet used for scheduling)

    @stability Internal
    @since 0.93.1 *)

(** {1 Gemini URL Construction} *)

(** Construct Gemini API URL with model_id in path and optional key param.
    Exposed for testing. *)
val gemini_url :
  config:Provider_config.t -> stream:bool -> string

(** {1 Provider Sampling Defaults} *)

(** Sampling parameter defaults per provider kind. *)
type sampling_defaults = {
  default_min_p : float option;
  default_top_p : float option;
  default_top_k : int option;
}

(** Get default sampling parameters for a provider kind.
    Local (OpenAI_compat) providers get min_p=0.05.
    Anthropic/Gemini get no defaults (all None). *)
val provider_sampling_defaults : Provider_config.provider_kind -> sampling_defaults

(** Apply provider defaults to a config, preserving explicit values.
    Only fills in [None] fields; explicit values are never overwritten. *)
val apply_sampling_defaults : Provider_config.t -> Provider_config.t

(** {1 Transport} *)

(** Create an HTTP-based transport.
    Wraps the internal HTTP completion pipeline into a
    {!Llm_transport.t} value that can be passed to [complete]
    or [complete_stream] via [?transport].

    @since 0.78.0 *)
val make_http_transport :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  Llm_transport.t

(** {1 Sync Completion} *)

(** Execute a single LLM completion round-trip.

    When [transport] is provided, uses that transport for I/O.
    Otherwise falls back to the built-in HTTP transport.

    When [cache] is provided, checks cache before I/O and stores on success.
    When [metrics] is provided, fires lifecycle callbacks.

    @return [Ok api_response] on success (possibly from cache)
    @return [Error http_error] on failure *)
val complete :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?transport:Llm_transport.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
  ?priority:Request_priority.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Retry} *)

(** Retry configuration with exponential backoff. *)
type retry_config = {
  max_retries: int;
  initial_delay_sec: float;
  max_delay_sec: float;
  backoff_multiplier: float;
}

val default_retry_config : retry_config

(** Classify whether an HTTP error is worth retrying.
    Retryable: 429 (rate limit), 500, 502, 503, 529,
    and all network errors. *)
val is_retryable : Http_client.http_error -> bool

(** Completion with exponential backoff retry.
    Passes [transport], [cache] and [metrics] through to each attempt. *)
val complete_with_retry :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?transport:Llm_transport.t ->
  clock:_ Eio.Time.clock ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?retry_config:retry_config ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
  ?priority:Request_priority.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Cascade: Multi-provider Failover} *)

(** Provider cascade: try primary, then each fallback on retryable failure. *)
(** {1 Stream Accumulator} *)

(** Re-exported from {!Complete_stream_acc} for backward compatibility.
    @since 0.79.0 *)
include module type of Complete_stream_acc

(** {1 Streaming Completion} *)

(** Execute a streaming LLM completion.
    Each SSE event is passed to [on_event] as it arrives.
    Returns the final assembled {!Types.api_response} after the stream ends.

    Supports both Anthropic native SSE and OpenAI-compatible SSE formats,
    dispatched by {!Provider_config.t.kind}. *)
val complete_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?transport:Llm_transport.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  on_event:(Types.sse_event -> unit) ->
  ?priority:Request_priority.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

