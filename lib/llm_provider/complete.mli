(** Standalone LLM completion: build request, HTTP POST, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Both OAS and MASC can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry, cascade
    @since 0.54.0  Optional cache + metrics hooks *)

(** {1 Sync Completion} *)

(** Execute a single LLM completion round-trip.

    When [cache] is provided, checks cache before HTTP and stores on success.
    When [metrics] is provided, fires lifecycle callbacks.

    @return [Ok api_response] on success (possibly from cache)
    @return [Error http_error] on HTTP or network failure *)
val complete :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
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
    Passes [cache] and [metrics] through to each attempt. *)
val complete_with_retry :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?retry_config:retry_config ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Cascade: Multi-provider Failover} *)

(** Provider cascade: try primary, then each fallback on retryable failure. *)
type cascade = {
  primary: Provider_config.t;
  fallbacks: Provider_config.t list;
}

(** Execute completion with cascade failover.
    When [clock] is provided, retries are enabled per-provider.
    On retryable failure, tries fallback providers in order.
    Fires [metrics.on_cascade_fallback] on each provider switch. *)
val complete_cascade :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?retry_config:retry_config ->
  cascade:cascade ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Streaming Completion} *)

(** Execute a streaming LLM completion.
    Each SSE event is passed to [on_event] as it arrives.
    Returns the final assembled {!Types.api_response} after the stream ends.

    Supports both Anthropic native SSE and OpenAI-compatible SSE formats,
    dispatched by {!Provider_config.t.kind}. *)
val complete_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  on_event:(Types.sse_event -> unit) ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Streaming Cascade} *)

(** Streaming completion with cascade failover.

    Tries the primary provider with streaming, falling back to
    each fallback provider on retryable errors. Failover only occurs
    before the SSE stream begins (on connection/HTTP errors).
    Once streaming starts, the provider is committed.

    No retry per-provider (streaming is not retryable mid-stream).
    No caching (streaming responses are not cacheable).
    No [accept] validator (events are already emitted to [on_event]).

    @since 0.61.0 *)
val complete_stream_cascade :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  cascade:cascade ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  on_event:(Types.sse_event -> unit) ->
  ?metrics:Metrics.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result
