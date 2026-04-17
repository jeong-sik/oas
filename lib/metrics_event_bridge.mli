(** Metrics ↔ Event_bus bridge.

    Wraps an [Llm_provider.Metrics.t] so that provider fallback is also
    visible on an [Event_bus.t]. Opt-in, preserving the layer boundary between
    the provider library and the agent SDK root.

    @since 0.154.0 *)

(** Compose a metrics sink with event_bus publication. The returned
    sink invokes [base.on_provider_fallback] first (preserving existing
    metrics backend behaviour), then emits a native
    [ProviderFallback { from_model; to_model; reason }]
    event on [bus]. All other callbacks are delegated to [base]
    unchanged.

    [correlation_id] / [run_id] default to empty strings so the emitted
    envelope is still well-formed when per-run identifiers are unknown.
    Downstream consumers that know the run context should supply them. *)
val compose_with_event_bus :
  ?correlation_id:string ->
  ?run_id:string ->
  Event_bus.t ->
  Llm_provider.Metrics.t ->
  Llm_provider.Metrics.t
