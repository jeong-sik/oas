open Base
(** Tests for Slot_scheduler_event_bridge — LT-14c publisher. *)

open Alcotest
open Agent_sdk
module Bridge = Slot_scheduler_event_bridge
module Sched = Llm_provider.Slot_scheduler

let mk_snap ~max_slots ~active ~queue_length : Sched.snapshot =
  { max_slots; active; available = max_slots - active; queue_length }
;;

(* ── derive_state: pure three-branch discriminator ─────────────── *)

let test_derive_state_idle () =
  let snap = mk_snap ~max_slots:4 ~active:0 ~queue_length:0 in
  check bool "all free is idle" true (Bridge.derive_state snap = Event_bus.Idle);
  let snap2 = mk_snap ~max_slots:4 ~active:4 ~queue_length:0 in
  check
    bool
    "saturated-without-queue is idle"
    true
    (Bridge.derive_state snap2 = Event_bus.Idle)
;;

let test_derive_state_queued () =
  (* available > 0 but queue_length > 0: rare grant-race window *)
  let snap = mk_snap ~max_slots:4 ~active:2 ~queue_length:1 in
  check bool "waiters + slack → queued" true (Bridge.derive_state snap = Event_bus.Queued)
;;

let test_derive_state_saturated () =
  let snap = mk_snap ~max_slots:4 ~active:4 ~queue_length:3 in
  check
    bool
    "full + waiters → saturated"
    true
    (Bridge.derive_state snap = Event_bus.Saturated)
;;

(* ── publish_snap: level-based event emission ──────────────────── *)

let test_publish_snap_fields () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let snap = mk_snap ~max_slots:8 ~active:5 ~queue_length:2 in
  Bridge.publish_snap bus snap;
  let events = Event_bus.drain sub in
  check int "one event" 1 (List.length events);
  match events with
  | [ ev ] ->
    (match ev.payload with
     | Event_bus.SlotSchedulerObserved payload ->
       check int "max_slots" 8 payload.max_slots;
       check int "active" 5 payload.active;
       check int "available" 3 payload.available;
       check int "queue_length" 2 payload.queue_length;
       check bool "state=queued" true (payload.state = Event_bus.Queued)
     | _ -> Alcotest.fail "payload not SlotSchedulerObserved _")
  | _ -> Alcotest.fail "wrong event count"
;;

(* ── envelope carries correlation_id + run_id ──────────────────── *)

let test_envelope_ids_propagated () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Bridge.publish_snap
    ~correlation_id:"corr-slot"
    ~run_id:"run-99"
    bus
    (mk_snap ~max_slots:1 ~active:0 ~queue_length:0);
  let events = Event_bus.drain sub in
  match events with
  | [ ev ] ->
    check string "correlation_id" "corr-slot" ev.meta.correlation_id;
    check string "run_id" "run-99" ev.meta.run_id
  | _ -> Alcotest.fail "wrong event count"
;;

(* ── publish_snapshot: live scheduler round-trip ───────────────── *)

let test_publish_snapshot_live_scheduler () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let scheduler = Sched.create ~max_slots:4 in
  (* Fresh scheduler: all slots available, no waiters → idle. *)
  Bridge.publish_snapshot bus scheduler;
  let events = Event_bus.drain sub in
  match events with
  | [ ev ] ->
    (match ev.payload with
     | Event_bus.SlotSchedulerObserved payload ->
       check bool "state=idle" true (payload.state = Event_bus.Idle);
       check int "active=0" 0 payload.active;
       check int "available=4" 4 payload.available;
       check int "queue=0" 0 payload.queue_length;
       check int "max=4" 4 payload.max_slots
     | _ -> Alcotest.fail "payload not SlotSchedulerObserved _")
  | _ -> Alcotest.fail "wrong event count"
;;

let () =
  run
    "Slot_scheduler_event_bridge"
    [ ( "derive_state"
      , [ test_case "idle" `Quick test_derive_state_idle
        ; test_case "queued" `Quick test_derive_state_queued
        ; test_case "saturated" `Quick test_derive_state_saturated
        ] )
    ; ( "publish"
      , [ test_case "publish_snap carries all fields" `Quick test_publish_snap_fields
        ; test_case "envelope ids propagated" `Quick test_envelope_ids_propagated
        ; test_case
            "publish_snapshot on fresh scheduler"
            `Quick
            test_publish_snapshot_live_scheduler
        ] )
    ]
;;
