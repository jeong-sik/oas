(** Slot_scheduler ↔ Event_bus publisher.

    Projects a {!Llm_provider.Slot_scheduler} snapshot onto the supplied
    {!Event_bus} as a native [SlotSchedulerObserved] event. Unlike
    {!Content_replacement_event_bridge}, this module does NOT wrap a mutator
    — the scheduler is read-only from our
    perspective. Callers decide when to publish (typically on grant,
    release, or a periodic tick).

    {1 Payload shape}

    {[
      {
        "max_slots":    int,
        "active":       int,
        "available":    int,
        "queue_length": int,
        "state":        "idle" | "queued" | "saturated"
      }
    ]}

    [state] is a derived discriminator — see {!derive_state}.

    {1 Edge detection}

    This module is deliberately level-based. If a subscriber only cares
    about transitions, it must diff against the previous event on its own
    side. Keeping the bridge stateless avoids having to reason about a
    second mutex/ref inside the scheduler layer.

    @since 0.154.0 *)

(** Derive a three-state discriminator from a scheduler snapshot.

    - ["saturated"] when [available = 0 && queue_length > 0]
      — all slots in use AND at least one fiber waiting.
    - ["queued"]    when [queue_length > 0]
      — waiters exist but capacity is not necessarily exhausted (rare but
      possible during grant race). Treated as a softer congestion signal.
    - ["idle"]      otherwise — either slack capacity OR all slots used
      with no waiters.

    Pure function — safe to call in tests without a real scheduler. *)
val derive_state : Llm_provider.Slot_scheduler.snapshot -> Event_bus.slot_scheduler_state

(** Publish a given snapshot to [bus] as a [SlotSchedulerObserved]
    event. Separated from
    {!publish_snapshot} so tests can drive deterministic snapshots
    without creating a live scheduler.

    [correlation_id] / [run_id] default to empty strings. *)
val publish_snap
  :  ?correlation_id:string
  -> ?run_id:string
  -> Event_bus.t
  -> Llm_provider.Slot_scheduler.snapshot
  -> unit

(** Take a fresh snapshot of [scheduler] and publish it.
    Equivalent to [publish_snap bus (Slot_scheduler.snapshot scheduler)]. *)
val publish_snapshot
  :  ?correlation_id:string
  -> ?run_id:string
  -> Event_bus.t
  -> Llm_provider.Slot_scheduler.t
  -> unit
