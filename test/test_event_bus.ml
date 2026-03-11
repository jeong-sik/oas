(** Tests for Event_bus — typed publish/subscribe for agent lifecycle events. *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────────── *)

let mock_response text = {
  Types.id = "r-1"; model = "mock"; stop_reason = Types.EndTurn;
  content = [Types.Text text]; usage = None;
}

(* ── create ───────────────────────────────────────────────────────── *)

let test_create_default () =
  let bus = Event_bus.create () in
  check int "no subscribers" 0 (Event_bus.subscriber_count bus)

let test_create_custom_buffer () =
  let bus = Event_bus.create ~buffer_size:8 () in
  check int "no subscribers" 0 (Event_bus.subscriber_count bus)

(* ── subscribe / unsubscribe ──────────────────────────────────────── *)

let test_subscribe_count () =
  let bus = Event_bus.create () in
  let _sub = Event_bus.subscribe bus in
  check int "one subscriber" 1 (Event_bus.subscriber_count bus)

let test_unsubscribe_count () =
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.unsubscribe bus sub;
  check int "zero after unsub" 0 (Event_bus.subscriber_count bus)

(* ── publish / drain ──────────────────────────────────────────────── *)

let test_publish_received () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  let events = Event_bus.drain sub in
  check int "one event" 1 (List.length events)

let test_publish_multiple_subscribers () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub1 = Event_bus.subscribe bus in
  let sub2 = Event_bus.subscribe bus in
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  let e1 = Event_bus.drain sub1 in
  let e2 = Event_bus.drain sub2 in
  check int "sub1 got event" 1 (List.length e1);
  check int "sub2 got event" 1 (List.length e2)

let test_unsubscribed_no_receive () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.unsubscribe bus sub;
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  let events = Event_bus.drain sub in
  check int "no events after unsub" 0 (List.length events)

(* ── drain ────────────────────────────────────────────────────────── *)

let test_drain_fifo () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 1 });
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 2 });
  let events = Event_bus.drain sub in
  check int "three events" 3 (List.length events);
  (* Verify FIFO order *)
  (match events with
   | [TurnStarted r0; TurnStarted r1; TurnStarted r2] ->
     check int "first turn" 0 r0.turn;
     check int "second turn" 1 r1.turn;
     check int "third turn" 2 r2.turn
   | _ -> fail "unexpected event types")

let test_drain_empty () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let events = Event_bus.drain sub in
  check int "no events" 0 (List.length events)

(* ── filters ──────────────────────────────────────────────────────── *)

let test_filter_agent_name () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe ~filter:(Event_bus.filter_agent "alpha") bus in
  Event_bus.publish bus (TurnStarted { agent_name = "alpha"; turn = 0 });
  Event_bus.publish bus (TurnStarted { agent_name = "beta"; turn = 0 });
  Event_bus.publish bus (TurnCompleted { agent_name = "alpha"; turn = 0 });
  let events = Event_bus.drain sub in
  check int "only alpha events" 2 (List.length events)

let test_filter_tools_only () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe ~filter:Event_bus.filter_tools_only bus in
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  Event_bus.publish bus (ToolCalled { agent_name = "a"; tool_name = "calc"; input = `Null });
  Event_bus.publish bus (ToolCompleted { agent_name = "a"; tool_name = "calc"; output = Ok "42" });
  Event_bus.publish bus (TurnCompleted { agent_name = "a"; turn = 0 });
  let events = Event_bus.drain sub in
  check int "only tool events" 2 (List.length events)

let test_accept_all () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe ~filter:Event_bus.accept_all bus in
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  Event_bus.publish bus (ToolCalled { agent_name = "a"; tool_name = "x"; input = `Null });
  Event_bus.publish bus (Custom ("test", `Null));
  let events = Event_bus.drain sub in
  check int "all three events" 3 (List.length events)

(* ── event types ──────────────────────────────────────────────────── *)

let test_custom_event () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let payload = `Assoc [("key", `String "value")] in
  Event_bus.publish bus (Custom ("my_event", payload));
  let events = Event_bus.drain sub in
  check int "one event" 1 (List.length events);
  match events with
  | [Custom (name, _)] -> check string "event name" "my_event" name
  | _ -> fail "expected Custom event"

let test_multiple_event_types () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish bus (AgentStarted { agent_name = "a"; task_id = "t1" });
  Event_bus.publish bus (TurnStarted { agent_name = "a"; turn = 0 });
  Event_bus.publish bus (ToolCalled { agent_name = "a"; tool_name = "f"; input = `Null });
  Event_bus.publish bus (ToolCompleted { agent_name = "a"; tool_name = "f"; output = Ok "ok" });
  Event_bus.publish bus (TurnCompleted { agent_name = "a"; turn = 0 });
  Event_bus.publish bus (AgentCompleted { agent_name = "a"; task_id = "t1";
                                          result = Ok (mock_response "done"); elapsed = 0.1 });
  let events = Event_bus.drain sub in
  check int "six events" 6 (List.length events)

(* ── field access ─────────────────────────────────────────────────── *)

let test_agent_started_fields () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish bus (AgentStarted { agent_name = "worker"; task_id = "t-42" });
  match Event_bus.drain sub with
  | [AgentStarted r] ->
    check string "agent_name" "worker" r.agent_name;
    check string "task_id" "t-42" r.task_id
  | _ -> fail "expected AgentStarted"

let test_tool_called_fields () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let input = `Assoc [("x", `Int 1)] in
  Event_bus.publish bus (ToolCalled { agent_name = "a"; tool_name = "calc"; input });
  match Event_bus.drain sub with
  | [ToolCalled r] ->
    check string "agent_name" "a" r.agent_name;
    check string "tool_name" "calc" r.tool_name;
    check string "input json" {|{"x":1}|} (Yojson.Safe.to_string r.input)
  | _ -> fail "expected ToolCalled"

let test_turn_started_fields () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Event_bus.publish bus (TurnStarted { agent_name = "bot"; turn = 5 });
  match Event_bus.drain sub with
  | [TurnStarted r] ->
    check string "agent_name" "bot" r.agent_name;
    check int "turn" 5 r.turn
  | _ -> fail "expected TurnStarted"

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  run "Event_bus" [
    "create", [
      test_case "default" `Quick test_create_default;
      test_case "custom buffer" `Quick test_create_custom_buffer;
    ];
    "subscribe", [
      test_case "count" `Quick test_subscribe_count;
      test_case "unsubscribe" `Quick test_unsubscribe_count;
    ];
    "publish", [
      test_case "received" `Quick test_publish_received;
      test_case "multiple subscribers" `Quick test_publish_multiple_subscribers;
      test_case "unsubscribed no receive" `Quick test_unsubscribed_no_receive;
    ];
    "drain", [
      test_case "fifo order" `Quick test_drain_fifo;
      test_case "empty" `Quick test_drain_empty;
    ];
    "filters", [
      test_case "agent name" `Quick test_filter_agent_name;
      test_case "tools only" `Quick test_filter_tools_only;
      test_case "accept all" `Quick test_accept_all;
    ];
    "event_types", [
      test_case "custom" `Quick test_custom_event;
      test_case "multiple types" `Quick test_multiple_event_types;
    ];
    "fields", [
      test_case "agent_started" `Quick test_agent_started_fields;
      test_case "tool_called" `Quick test_tool_called_fields;
      test_case "turn_started" `Quick test_turn_started_fields;
    ];
  ]
