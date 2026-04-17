(** Slot_scheduler ↔ Event_bus publisher (LT-14c / observability).

    Third member of the OAS silent-FSM-to-event-bus series after
    {!Metrics_event_bridge} (cascade_fallback) and
    {!Content_replacement_event_bridge} (content_replacement_frozen).

    Shape difference: slot_scheduler already exposes a read-only
    [snapshot] API so this module is a pure publisher rather than a
    mutator wrapper — no Invalid_argument-suppression logic needed. *)

module Sched = Llm_provider.Slot_scheduler

let event_topic = "slot_scheduler_queue"

let derive_state (snap : Sched.snapshot) : string =
  if snap.available = 0 && snap.queue_length > 0 then "saturated"
  else if snap.queue_length > 0 then "queued"
  else "idle"

let payload_of_snap (snap : Sched.snapshot) : Yojson.Safe.t =
  `Assoc [
    "max_slots",    `Int snap.max_slots;
    "active",       `Int snap.active;
    "available",    `Int snap.available;
    "queue_length", `Int snap.queue_length;
    "state",        `String (derive_state snap);
  ]

let publish_snap
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (snap : Sched.snapshot)
  : unit =
  let event =
    Event_bus.mk_event
      ~correlation_id
      ~run_id
      (Event_bus.Custom (event_topic, payload_of_snap snap))
  in
  Event_bus.publish bus event

let publish_snapshot
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (scheduler : Sched.t)
  : unit =
  publish_snap ~correlation_id ~run_id bus (Sched.snapshot scheduler)
