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
type t = {
  on_cache_hit: model_id:string -> unit;
  on_cache_miss: model_id:string -> unit;
  on_request_start: model_id:string -> unit;
  on_request_end: model_id:string -> latency_ms:int -> unit;
  on_error: model_id:string -> error:string -> unit;
  on_cascade_fallback: from_model:string -> to_model:string -> reason:string -> unit;
  on_http_status: provider:string -> model_id:string -> status:int -> unit;
}

(** No-op metrics — all callbacks do nothing. *)
val noop : t
