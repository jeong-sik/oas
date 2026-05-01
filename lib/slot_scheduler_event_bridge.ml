open Base
(** Slot_scheduler ↔ Event_bus publisher (LT-14c / observability).

    Projects slot-scheduler snapshots onto the event bus without changing the
    scheduler API itself.

    Shape difference: slot_scheduler already exposes a read-only
    [snapshot] API so this module is a pure publisher rather than a
    mutator wrapper — no Invalid_argument-suppression logic needed. *)

module Sched = Llm_provider.Slot_scheduler

let derive_state (snap : Sched.snapshot) : Event_bus.slot_scheduler_state =
  if snap.available = 0 && snap.queue_length > 0
  then Event_bus.Saturated
  else if snap.queue_length > 0
  then Event_bus.Queued
  else Event_bus.Idle
;;

let publish_snap
      ?(correlation_id = "")
      ?(run_id = "")
      (bus : Event_bus.t)
      (snap : Sched.snapshot)
  : unit
  =
  let event =
    Event_bus.mk_event
      ~correlation_id
      ~run_id
      (Event_bus.SlotSchedulerObserved
         { max_slots = snap.max_slots
         ; active = snap.active
         ; available = snap.available
         ; queue_length = snap.queue_length
         ; state = derive_state snap
         })
  in
  Event_bus.publish bus event
;;

let publish_snapshot
      ?(correlation_id = "")
      ?(run_id = "")
      (bus : Event_bus.t)
      (scheduler : Sched.t)
  : unit
  =
  publish_snap ~correlation_id ~run_id bus (Sched.snapshot scheduler)
;;
