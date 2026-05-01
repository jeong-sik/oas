(** Tests for Content_replacement_event_bridge — LT-14b opt-in bridge. *)

open Alcotest
open Agent_sdk
module Bridge = Content_replacement_event_bridge
module S = Content_replacement_state

(* ── record_replacement emits native replaced variant ─────────────── *)

let test_replacement_event () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let state = S.create () in
  Bridge.record_replacement_with_events
    bus
    state
    { tool_use_id = "toolu_01"; preview = "short preview"; original_chars = 9000 };
  check bool "state was mutated" true (S.is_frozen state "toolu_01");
  let events = Event_bus.drain sub in
  check int "one event published" 1 (List.length events);
  match events with
  | [ ev ] ->
    (match ev.payload with
     | Event_bus.ContentReplacementReplaced payload ->
       check string "tool_use_id" "toolu_01" payload.tool_use_id;
       check string "preview" "short preview" payload.preview;
       check int "original_chars" 9000 payload.original_chars;
       check int "seen_count_after" 1 payload.seen_count_after
     | _ -> Alcotest.fail "payload not ContentReplacementReplaced _")
  | _ -> Alcotest.fail "wrong event count"
;;

(* ── record_kept emits native kept variant ───────────────────────── *)

let test_kept_event () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let state = S.create () in
  Bridge.record_kept_with_events bus state "toolu_02";
  check bool "state was mutated" true (S.is_frozen state "toolu_02");
  check
    (option string)
    "no replacement recorded"
    None
    (Option.map
       (fun (r : S.replacement) -> r.preview)
       (S.lookup_replacement state "toolu_02"));
  let events = Event_bus.drain sub in
  match events with
  | [ ev ] ->
    (match ev.payload with
     | Event_bus.ContentReplacementKept payload ->
       check string "tool_use_id" "toolu_02" payload.tool_use_id;
       check int "seen_count_after" 1 payload.seen_count_after
     | _ -> Alcotest.fail "payload not ContentReplacementKept _")
  | _ -> Alcotest.fail "wrong event count"
;;

(* ── envelope carries correlation_id + run_id when supplied ─────── *)

let test_envelope_ids_propagated () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let state = S.create () in
  Bridge.record_replacement_with_events
    ~correlation_id:"corr-crs"
    ~run_id:"run-77"
    bus
    state
    { tool_use_id = "toolu_03"; preview = "p"; original_chars = 10 };
  let events = Event_bus.drain sub in
  match events with
  | [ ev ] ->
    check string "correlation_id on envelope" "corr-crs" ev.meta.correlation_id;
    check string "run_id on envelope" "run-77" ev.meta.run_id
  | _ -> Alcotest.fail "wrong event count"
;;

(* ── double-freeze raises Invalid_argument and does NOT publish ──── *)

let test_double_freeze_no_event () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let state = S.create () in
  Bridge.record_kept_with_events bus state "toolu_04";
  let _ = Event_bus.drain sub in
  (* consume the first (legitimate) event *)
  let raised =
    try
      Bridge.record_replacement_with_events
        bus
        state
        { tool_use_id = "toolu_04"; preview = "p"; original_chars = 1 };
      false
    with
    | Invalid_argument _ -> true
  in
  check bool "second freeze raised" true raised;
  let events_after = Event_bus.drain sub in
  check int "no extra event on raised path" 0 (List.length events_after);
  check int "seen_count unchanged" 1 (S.seen_count state)
;;

(* ── seen_count_after reflects post-state across multiple freezes ── *)

let test_seen_count_growth () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let state = S.create () in
  Bridge.record_kept_with_events bus state "a";
  Bridge.record_replacement_with_events
    bus
    state
    { tool_use_id = "b"; preview = "bb"; original_chars = 100 };
  Bridge.record_kept_with_events bus state "c";
  let events = Event_bus.drain sub in
  check int "three events" 3 (List.length events);
  let counts =
    List.map
      (fun (ev : Event_bus.event) ->
         match ev.payload with
         | Event_bus.ContentReplacementReplaced payload -> payload.seen_count_after
         | Event_bus.ContentReplacementKept payload -> payload.seen_count_after
         | _ -> Alcotest.fail "bad payload")
      events
  in
  check (list int) "counts increase 1,2,3" [ 1; 2; 3 ] counts
;;

let () =
  run
    "Content_replacement_event_bridge"
    [ ( "bridge"
      , [ test_case
            "record_replacement_with_events publishes replaced"
            `Quick
            test_replacement_event
        ; test_case "record_kept_with_events publishes kept" `Quick test_kept_event
        ; test_case "envelope ids propagated" `Quick test_envelope_ids_propagated
        ; test_case
            "double-freeze raises and suppresses event"
            `Quick
            test_double_freeze_no_event
        ; test_case "seen_count_after matches post-state" `Quick test_seen_count_growth
        ] )
    ]
;;
