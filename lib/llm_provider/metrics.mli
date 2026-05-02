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
