(** Tests for Metrics_event_bridge — LT-14 opt-in bridge. *)

open Alcotest
open Agent_sdk

let extract_cascade_payload (ev : Event_bus.event) =
  match ev.payload with
  | Event_bus.Custom ("cascade_fallback", `Assoc kvs) -> Some kvs
  | _ -> None

let string_field kvs key =
  match List.assoc_opt key kvs with
  | Some (`String s) -> s
  | _ -> Alcotest.fail ("missing field " ^ key)

(* ── base callback is preserved ──────────────────────────────────── *)

let test_base_callback_invoked () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let observed = ref [] in
  let base = {
    Llm_provider.Metrics.noop with
    on_cascade_fallback = (fun ~from_model ~to_model ~reason ->
      observed := (from_model, to_model, reason) :: !observed)
  } in
  let wrapped = Agent_sdk.Metrics_event_bridge.compose_with_event_bus bus base in
  wrapped.on_cascade_fallback
    ~from_model:"claude" ~to_model:"glm" ~reason:"HTTP 529";
  check int "base invoked exactly once" 1 (List.length !observed);
  (match !observed with
   | (f, t, r) :: _ ->
     check string "from_model" "claude" f;
     check string "to_model" "glm" t;
     check string "reason" "HTTP 529" r
   | [] -> Alcotest.fail "no base invocation")

(* ── event lands on the bus ──────────────────────────────────────── *)

let test_event_published () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let wrapped =
    Agent_sdk.Metrics_event_bridge.compose_with_event_bus bus Llm_provider.Metrics.noop
  in
  wrapped.on_cascade_fallback
    ~from_model:"claude" ~to_model:"glm" ~reason:"HTTP 529";
  let events = Event_bus.drain sub in
  check int "one event received" 1 (List.length events);
  match events with
  | [ev] ->
    (match extract_cascade_payload ev with
     | Some kvs ->
       check string "from_model" "claude" (string_field kvs "from_model");
       check string "to_model"   "glm"    (string_field kvs "to_model");
       check string "reason"     "HTTP 529" (string_field kvs "reason")
     | None -> Alcotest.fail "payload was not Custom(cascade_fallback, _)")
  | _ -> Alcotest.fail "wrong event count"

(* ── envelope carries correlation_id + run_id when supplied ──────── *)

let test_envelope_ids_propagated () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let wrapped =
    Metrics_event_bridge.compose_with_event_bus
      ~correlation_id:"corr-xyz"
      ~run_id:"run-42"
      bus
      Llm_provider.Metrics.noop
  in
  wrapped.on_cascade_fallback
    ~from_model:"a" ~to_model:"b" ~reason:"stub";
  let events = Event_bus.drain sub in
  match events with
  | [ev] ->
    check string "correlation_id on envelope" "corr-xyz" ev.meta.correlation_id;
    check string "run_id on envelope" "run-42" ev.meta.run_id
  | _ -> Alcotest.fail "wrong event count"

(* ── other callbacks are untouched ───────────────────────────────── *)

let test_other_callbacks_delegated () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let cache_hits = ref 0 in
  let base = {
    Llm_provider.Metrics.noop with
    on_cache_hit = (fun ~model_id:_ -> incr cache_hits)
  } in
  let wrapped = Agent_sdk.Metrics_event_bridge.compose_with_event_bus bus base in
  wrapped.on_cache_hit ~model_id:"m1";
  wrapped.on_cache_hit ~model_id:"m2";
  check int "other callbacks delegated" 2 !cache_hits

let () =
  run "Metrics_event_bridge" [
    "compose_with_event_bus", [
      test_case "base callback still invoked" `Quick test_base_callback_invoked;
      test_case "Custom event published" `Quick test_event_published;
      test_case "envelope ids propagated" `Quick test_envelope_ids_propagated;
      test_case "other callbacks delegated" `Quick test_other_callbacks_delegated;
    ]
  ]
