(** Metrics hooks for LLM completion observability.

    Consumers inject their own metrics backend (Prometheus, StatsD, log, etc.)
    via the [t] record. OAS never depends on a specific implementation.

    @since 0.54.0

    @stability Internal
    @since 0.93.1 *)

(** Metrics callback interface. All callbacks are optional — provide [noop]
    as a default that does nothing.

    [on_http_status] fires once per HTTP response from a provider,
    regardless of whether the call ultimately succeeds or fails, so
    operators can plot request volume broken down by
    [{provider, model, status}]. Network/timeout errors that never
    produced a status do NOT fire this callback — those still surface
    through [on_error] as before. *)
type t =
  { on_cache_hit : model_id:string -> unit
  ; on_cache_miss : model_id:string -> unit
  ; on_request_start : model_id:string -> unit
  ; on_request_end : model_id:string -> latency_ms:int -> unit
  ; on_error : model_id:string -> error:string -> unit
  ; on_http_status : provider:string -> model_id:string -> status:int -> unit
  ; on_capability_drop : model_id:string -> field:string -> unit
  ; on_retry : provider:string -> model_id:string -> attempt:int -> unit
    (** Fired when a request is retried due to a retryable error.
      @since 0.185.0 *)
  ; on_token_usage
    : provider:string -> model_id:string -> input_tokens:int -> output_tokens:int -> unit
    (** Fired when a response carries usage tokens.
      @since 0.185.0 *)
  }

(** No-op metrics — all callbacks do nothing. *)
val noop : t

(** Install a process-wide metrics sink.  When a caller invokes
    {!Complete.complete} (or a wrapper) without passing [~metrics]
    explicitly, the value installed here is used instead of {!noop}.

    Initialization ordering: {!Complete.complete} resolves
    [get_global ()] at call time, so each call re-reads whatever sink
    is currently installed — subsequent calls after [set_global] see
    the new value.  The only window where callers observe [noop] is
    for HTTP calls issued *before* the host has run its startup
    [set_global]; install the sink as early in bootstrap as possible
    (before any automated agent traffic) to avoid that gap.

    Intended to be called once at startup from the host application
    (e.g. a downstream consumer installs a Prometheus-backed instance).
    Thread-safe:
    a concurrent [set_global] is published atomically via [Atomic.set];
    fibers already holding the previous reference continue to use it
    until their current call returns. *)
val set_global : t -> unit

(** Read the current process-wide metrics sink.  Returns {!noop}
    unless {!set_global} has been called. *)
val get_global : unit -> t

(* ── Per-provider aggregating backend ───────────── *)

(** Immutable snapshot of accumulated counters for a single provider/model
    pair. Suitable for OTLP/Prometheus export or structured logging.

    @since 0.188.0 *)
type provider_snapshot =
  { provider : string
  ; model_id : string
  ; request_total : int
  ; error_total : int
  ; retry_total : int
  ; input_tokens_total : int
  ; output_tokens_total : int
  ; latency_ms_sum : int
  ; latency_ms_count : int
  }

(** Mutable counters for a single aggregation key. *)
type aggregate_state

(** Alias for the hooks record {!t}, exposed so the {!Aggregating}
    submodule can reference it without name collision with its own [t]. *)
type hooks = t

(** Thread-safe aggregating metrics backend.

    Accumulates per-provider counters in a hash table guarded by a
    {!Mutex.t}. Wrap an existing [Metrics.t] via {!Aggregating.create} to
    layer counting on top of any other metrics sink. Call
    {!Aggregating.snapshot} to read all counters as an immutable list.

    @since 0.188.0 *)
module Aggregating : sig
  type t

  (** Build the aggregation key from provider and model_id. *)
  val key : provider:string -> model_id:string -> string

  (** Create a new aggregator. [~inner] is the underlying metrics sink
      that receives callbacks before the aggregator increments its own
      counters. Defaults to {!noop}. *)
  val create : ?inner:hooks -> unit -> t

  (** Derive a hooks record that increments the aggregator's
      counters on each callback, then delegates to the inner sink. *)
  val to_hooks : t -> hooks

  (** Read all accumulated counters as an immutable snapshot list. *)
  val snapshot : t -> provider_snapshot list

  (** Reset all counters to zero. *)
  val reset : t -> unit
end
