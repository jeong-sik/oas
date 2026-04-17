(** Metrics ↔ Event_bus bridge (LT-14 / observability).

    Lives in [agent_sdk] rather than [agent_sdk.llm_provider] because
    [Event_bus] is part of [agent_sdk] and the provider layer must not
    gain a dependency edge back up to the root library. Opt-in: callers
    who want provider fallback visible on the event bus wrap their
    existing [Metrics.t] through here at startup. *)

module M = Llm_provider.Metrics

let compose_with_event_bus
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (base : M.t)
  : M.t =
  {
    base with
    on_provider_fallback = (fun ~from_model ~to_model ~reason ->
      base.on_provider_fallback ~from_model ~to_model ~reason;
      let event =
        Event_bus.mk_event
          ~correlation_id
          ~run_id
          (Event_bus.ProviderFallback { from_model; to_model; reason })
      in
      Event_bus.publish bus event
    );
  }
