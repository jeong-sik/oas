(** Metrics ↔ Event_bus bridge (LT-14 / observability).

    Lives in [agent_sdk] rather than [agent_sdk.llm_provider] because
    [Event_bus] is part of [agent_sdk] and the provider layer must not
    gain a dependency edge back up to the root library. Opt-in: callers
    who want cascade routing visible on the event bus wrap their
    existing [Metrics.t] through here at startup; the raw
    [complete_cascade] path stays unaware of the bus. *)

module M = Llm_provider.Metrics

let compose_with_event_bus
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (base : M.t)
  : M.t =
  {
    base with
    on_cascade_fallback = (fun ~from_model ~to_model ~reason ->
      base.on_cascade_fallback ~from_model ~to_model ~reason;
      let payload =
        `Assoc [
          "from_model", `String from_model;
          "to_model",   `String to_model;
          "reason",     `String reason;
        ]
      in
      let event =
        Event_bus.mk_event
          ~correlation_id
          ~run_id
          (Event_bus.Custom ("cascade_fallback", payload))
      in
      Event_bus.publish bus event
    );
  }
