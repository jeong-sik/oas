(** Tests for Slot_scheduler_event_bridge — LT-14c publisher. *)

open Alcotest
open Agent_sdk

module Bridge = Slot_scheduler_event_bridge
module Sched = Llm_provider.Slot_scheduler

let extract_queue_payload (ev : Event_bus.event) =
  match ev.payload with
  | Event_bus.Custom ("slot_scheduler_queue", `Assoc kvs) -> Some kvs
  | _ -> None

let string_field kvs key =
  match List.assoc_opt key kvs with
  | Some (`String s) -> s
  | _ -> Alcotest.fail ("missing string field " ^ key)

let int_field kvs key =
  match List.assoc_opt key kvs with
  | Some (`Int n) -> n
  | _ -> Alcotest.fail ("missing int field " ^ key)

let mk_snap ~max_slots ~active ~queue_length : Sched.snapshot =
  { max_slots; active; available = max_slots - active; queue_length }

(* ── derive_state: pure three-branch discriminator ─────────────── *)

let test_derive_state_idle () =
  let snap = mk_snap ~max_slots:4 ~active:0 ~queue_length:0 in
  check string "all free is idle" "idle" (Bridge.derive_state snap);
  let snap2 = mk_snap ~max_slots:4 ~active:4 ~queue_length:0 in
  check string "saturated-without-queue is idle" "idle" (Bridge.derive_state snap2)

let test_derive_state_queued () =
  (* available > 0 but queue_length > 0: rare grant-race window *)
  let snap = mk_snap ~max_slots:4 ~active:2 ~queue_length:1 in
  check string "waiters + slack → queued" "queued" (Bridge.derive_state snap)

let test_derive_state_saturated () =
  let snap = mk_snap ~max_slots:4 ~active:4 ~queue_length:3 in
  check string "full + waiters → saturated" "saturated" (Bridge.derive_state snap)

(* ── publish_snap: level-based event emission ──────────────────── *)

let test_publish_snap_fields () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let snap = mk_snap ~max_slots:8 ~active:5 ~queue_length:2 in
  Bridge.publish_snap bus snap;
  let events = Event_bus.drain sub in
  check int "one event" 1 (List.length events);
  match events with
  | [ev] ->
    (match extract_queue_payload ev with
     | Some kvs ->
       check int "max_slots"    8 (int_field kvs "max_slots");
       check int "active"       5 (int_field kvs "active");
       check int "available"    3 (int_field kvs "available");
       check int "queue_length" 2 (int_field kvs "queue_length");
       check string "state=queued" "queued" (string_field kvs "state")
     | None -> Alcotest.fail "payload not Custom(slot_scheduler_queue, _)")
  | _ -> Alcotest.fail "wrong event count"

(* ── envelope carries correlation_id + run_id ──────────────────── *)

let test_envelope_ids_propagated () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Bridge.publish_snap
    ~correlation_id:"corr-slot"
    ~run_id:"run-99"
    bus
    (mk_snap ~max_slots:1 ~active:0 ~queue_length:0);
  let events = Event_bus.drain sub in
  match events with
  | [ev] ->
    check string "correlation_id" "corr-slot" ev.meta.correlation_id;
    check string "run_id" "run-99" ev.meta.run_id
  | _ -> Alcotest.fail "wrong event count"

(* ── publish_snapshot: live scheduler round-trip ───────────────── *)

let test_publish_snapshot_live_scheduler () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let scheduler = Sched.create ~max_slots:4 in
  (* Fresh scheduler: all slots available, no waiters → idle. *)
  Bridge.publish_snapshot bus scheduler;
  let events = Event_bus.drain sub in
  match events with
  | [ev] ->
    (match extract_queue_payload ev with
     | Some kvs ->
       check string "state=idle" "idle" (string_field kvs "state");
       check int    "active=0"    0      (int_field kvs "active");
       check int    "available=4" 4      (int_field kvs "available");
       check int    "queue=0"     0      (int_field kvs "queue_length");
       check int    "max=4"       4      (int_field kvs "max_slots")
     | None -> Alcotest.fail "payload not Custom(slot_scheduler_queue, _)")
  | _ -> Alcotest.fail "wrong event count"

let () =
  run "Slot_scheduler_event_bridge" [
    "derive_state", [
      test_case "idle"      `Quick test_derive_state_idle;
      test_case "queued"    `Quick test_derive_state_queued;
      test_case "saturated" `Quick test_derive_state_saturated;
    ];
    "publish", [
      test_case "publish_snap carries all fields"     `Quick test_publish_snap_fields;
      test_case "envelope ids propagated"             `Quick test_envelope_ids_propagated;
      test_case "publish_snapshot on fresh scheduler" `Quick test_publish_snapshot_live_scheduler;
    ];
  ]
