(** Standalone LLM completion: build request, HTTP POST, parse response.

    Self-contained in llm_provider -- no agent_sdk dependency.
    Both OAS and MASC can call these functions directly.

    @since 0.46.0  Sync completion
    @since 0.53.0  Streaming, retry, cascade *)

(** {1 Sync Completion} *)

(** Execute a single LLM completion round-trip.
    Builds the request body based on {!Provider_config.t.kind},
    sends via {!Http_client.post_sync}, and parses the response.

    @return [Ok api_response] on success
    @return [Error http_error] on HTTP or network failure *)
val complete :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
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
    Requires an Eio clock for sleep between attempts. *)
val complete_with_retry :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?retry_config:retry_config ->
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
    On retryable failure, tries fallback providers in order. *)
val complete_cascade :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?retry_config:retry_config ->
  cascade:cascade ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
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
