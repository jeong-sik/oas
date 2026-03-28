(** Metrics hooks for LLM completion observability.

    Consumers inject their own metrics backend (Prometheus, StatsD, log, etc.)
    via the [t] record. OAS never depends on a specific implementation.

    @since 0.54.0

    @stability Internal
    @since 0.93.0 *)

(** Metrics callback interface. All callbacks are optional — provide [noop]
    as a default that does nothing. *)
type t = {
  on_cache_hit: model_id:string -> unit;
  on_cache_miss: model_id:string -> unit;
  on_request_start: model_id:string -> unit;
  on_request_end: model_id:string -> latency_ms:int -> unit;
  on_error: model_id:string -> error:string -> unit;
  on_cascade_fallback: from_model:string -> to_model:string -> reason:string -> unit;
}

(** No-op metrics — all callbacks do nothing. *)
val noop : t
