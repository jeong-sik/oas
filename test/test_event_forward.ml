(** Tests for Event_forward module — event forwarding to external targets. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let tmp_file () =
  let path = Filename.temp_file "oas_test_fwd_" ".jsonl" in
  at_exit (fun () -> try Sys.remove path with _ -> ());
  path

(** Shorthand: wrap a payload into an event with a fresh envelope. *)
let ev payload = Event_bus.mk_event payload

(* ── Tests ────────────────────────────────────────────────────── *)

let test_event_type_name () =
  let cases = [
    (ev (Event_bus.AgentStarted { agent_name = "a"; task_id = "t" }),
     "agent.started");
    (ev (Event_bus.TurnStarted { agent_name = "a"; turn = 0 }),
     "turn.started");
    (ev (Event_bus.ToolCalled { agent_name = "a"; tool_name = "t"; input = `Null }),
     "tool.called");
    (ev (Event_bus.Custom ("foo", `Null)),
     "foo");
    (ev (Event_bus.ContextOverflowImminent {
           agent_name = "a"; estimated_tokens = 95000;
           limit_tokens = 100000; ratio = 0.95 }),
     "context.overflow_imminent");
    (ev (Event_bus.ContextCompactStarted {
           agent_name = "a"; trigger = "proactive" }),
     "context.compact_started");
  ] in
  List.iter (fun (event, expected) ->
    Alcotest.(check string) "event_type"
      expected (Event_forward.event_type_name event)
  ) cases

let test_event_to_payload () =
  let event =
    ev (Event_bus.TurnStarted { agent_name = "test"; turn = 3 })
  in
  let p = Event_forward.event_to_payload event in
  Alcotest.(check string) "event_type" "turn.started" p.event_type;
  Alcotest.(check (option string)) "agent_name" (Some "test") p.agent_name;
  let json = Event_forward.payload_to_json p in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type in json" "turn.started"
    (json |> member "event_type" |> to_string)

let test_payload_to_json () =
  let p : Event_forward.event_payload = {
    event_type = "test.event";
    timestamp = 1700000000.0;
    agent_name = Some "alice";
    correlation_id = "c1";
    run_id = "r1";
    data = `Assoc [("key", `String "val")];
  } in
  let json = Event_forward.payload_to_json p in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "event_type" "test.event"
    (json |> member "event_type" |> to_string);
  Alcotest.(check string) "agent_name" "alice"
    (json |> member "agent_name" |> to_string);
  Alcotest.(check string) "correlation_id" "c1"
    (json |> member "correlation_id" |> to_string);
  Alcotest.(check string) "run_id" "r1"
    (json |> member "run_id" |> to_string)

let test_file_append_target () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = tmp_file () in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create
    ~targets:[File_append { path }]
    ~batch_size:2 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "a"; turn = 0 }));
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "a"; turn = 1 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  let ic = open_in path in
  let lines = ref 0 in
  (try while true do ignore (input_line ic); incr lines done
   with End_of_file -> ());
  close_in ic;
  Alcotest.(check bool) "has lines" true (!lines > 0)

let test_custom_target () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let received = ref [] in
  let custom_target =
    Event_forward.Custom_target {
      name = "test_sink";
      deliver = (fun p -> received := p :: !received);
    }
  in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create ~targets:[custom_target] ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "b"; turn = 0 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  Alcotest.(check bool) "received events" true (List.length !received > 0)

let test_custom_target_error_handling () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let failing_target =
    Event_forward.Custom_target {
      name = "failing";
      deliver = (fun _ -> failwith "delivery error");
    }
  in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create ~targets:[failing_target] ~batch_size:1 () in
  Log.set_global_level Error;  (* Suppress warn during test *)
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "c"; turn = 0 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  Alcotest.(check bool) "failed count > 0" true
    (Event_forward.failed_count fwd > 0)

let test_delivered_count () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let received = ref 0 in
  let counting_target =
    Event_forward.Custom_target {
      name = "counter";
      deliver = (fun _ -> incr received);
    }
  in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create ~targets:[counting_target] ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "d"; turn = 0 }));
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "d"; turn = 1 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  Alcotest.(check bool) "delivered" true
    (Event_forward.delivered_count fwd >= !received)

let test_create_default_params () =
  let fwd = Event_forward.create ~targets:[] () in
  Alcotest.(check int) "delivered" 0 (Event_forward.delivered_count fwd);
  Alcotest.(check int) "failed" 0 (Event_forward.failed_count fwd)

let test_multiple_targets () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let r1 = ref 0 in
  let r2 = ref 0 in
  let t1 = Event_forward.Custom_target {
    name = "t1"; deliver = (fun _ -> incr r1) } in
  let t2 = Event_forward.Custom_target {
    name = "t2"; deliver = (fun _ -> incr r2) } in
  let bus = Event_bus.create () in
  let fwd = Event_forward.create ~targets:[t1; t2] ~batch_size:1 () in
  Event_forward.start ~sw ~net:(Eio.Stdenv.net env) ~bus fwd;
  Event_bus.publish bus
    (ev (TurnStarted { agent_name = "e"; turn = 0 }));
  Eio.Fiber.yield ();
  Eio.Fiber.yield ();
  Event_forward.stop fwd;
  Eio.Fiber.yield ();
  Alcotest.(check bool) "t1 received" true (!r1 > 0);
  Alcotest.(check bool) "t2 received" true (!r2 > 0)

let test_stop_idempotent () =
  let fwd = Event_forward.create ~targets:[] () in
  Event_forward.stop fwd;
  Event_forward.stop fwd;
  Alcotest.(check int) "delivered" 0 (Event_forward.delivered_count fwd)

let test_agent_completed_payload () =
  let event = ev (Event_bus.AgentCompleted {
    agent_name = "solver";
    task_id = "t1";
    result = Ok {
      Types.id = "msg_1";
      model = "claude-sonnet-4-20250514";
      content = [Text "done"];
      stop_reason = EndTurn;
      usage = None;
      telemetry = None;
    };
    elapsed = 2.5;
  }) in
  let p = Event_forward.event_to_payload event in
  Alcotest.(check string) "type" "agent.completed" p.event_type;
  let open Yojson.Safe.Util in
  let json = p.data in
  Alcotest.(check bool) "success" true (json |> member "success" |> to_bool)

let test_tool_events_payload () =
  let called = ev (Event_bus.ToolCalled {
    agent_name = "x"; tool_name = "search"; input = `String "query" }) in
  let completed = ev (Event_bus.ToolCompleted {
    agent_name = "x"; tool_name = "search";
    output = Ok { Types.content = "result" } }) in
  let p1 = Event_forward.event_to_payload called in
  let p2 = Event_forward.event_to_payload completed in
  Alcotest.(check string) "called type" "tool.called" p1.event_type;
  Alcotest.(check string) "completed type" "tool.completed" p2.event_type

(* ── event_to_payload: remaining event types ──────────────── *)

let test_turn_completed_payload () =
  let evt = ev (Event_bus.TurnCompleted { agent_name = "worker"; turn = 5 }) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "turn.completed" p.event_type;
  Alcotest.(check (option string)) "agent" (Some "worker") p.agent_name

let test_elicitation_completed_payload () =
  let evt = ev (Event_bus.ElicitationCompleted {
    agent_name = "agent"; question = "confirm?";
    response = Hooks.Declined }) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "elicitation.completed" p.event_type;
  Alcotest.(check (option string)) "agent" (Some "agent") p.agent_name

let test_task_state_changed_payload () =
  let evt = ev (Event_bus.TaskStateChanged {
    task_id = "t1"; from_state = "running"; to_state = "completed" }) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "task.state_changed" p.event_type;
  Alcotest.(check (option string)) "no agent" None p.agent_name

let test_custom_event_payload () =
  let evt = ev (Event_bus.Custom ("myevent", `Assoc [("x", `Int 1)])) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "myevent" p.event_type;
  Alcotest.(check (option string)) "no agent" None p.agent_name

let test_agent_started_payload () =
  let evt = ev (Event_bus.AgentStarted { agent_name = "alpha"; task_id = "t1" }) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "agent.started" p.event_type;
  Alcotest.(check (option string)) "agent" (Some "alpha") p.agent_name

let test_tool_completed_error_payload () =
  let evt = ev (Event_bus.ToolCompleted {
    agent_name = "x"; tool_name = "calc";
    output = Error { Types.message = "fail"; recoverable = false } }) in
  let p = Event_forward.event_to_payload evt in
  Alcotest.(check string) "type" "tool.completed" p.event_type;
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "success false" false
    (p.data |> member "success" |> to_bool)

let test_payload_to_json_no_agent () =
  let p : Event_forward.event_payload = {
    event_type = "test"; timestamp = 1.0; agent_name = None;
    correlation_id = "c1"; run_id = "r1";
    data = `Assoc [("x", `Int 1)]; } in
  let json = Event_forward.payload_to_json p in
  let open Yojson.Safe.Util in
  (* Should not have agent_name field *)
  let keys = json |> to_assoc |> List.map fst in
  Alcotest.(check bool) "no agent key" false (List.mem "agent_name" keys)

let test_payload_to_json_with_agent () =
  let p : Event_forward.event_payload = {
    event_type = "test"; timestamp = 1.0; agent_name = Some "bot";
    correlation_id = "c1"; run_id = "r1";
    data = `Null; } in
  let json = Event_forward.payload_to_json p in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "agent" "bot"
    (json |> member "agent_name" |> to_string)

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Event_forward" [
    "event_type", [
      Alcotest.test_case "name mapping" `Quick test_event_type_name;
    ];
    "payload", [
      Alcotest.test_case "event_to_payload" `Quick test_event_to_payload;
      Alcotest.test_case "payload_to_json" `Quick test_payload_to_json;
      Alcotest.test_case "agent_completed" `Quick test_agent_completed_payload;
      Alcotest.test_case "tool events" `Quick test_tool_events_payload;
      Alcotest.test_case "turn_completed" `Quick test_turn_completed_payload;
      Alcotest.test_case "elicitation" `Quick test_elicitation_completed_payload;
      Alcotest.test_case "task_state_changed" `Quick test_task_state_changed_payload;
      Alcotest.test_case "custom event" `Quick test_custom_event_payload;
      Alcotest.test_case "agent_started" `Quick test_agent_started_payload;
      Alcotest.test_case "tool_completed error" `Quick test_tool_completed_error_payload;
      Alcotest.test_case "payload no agent" `Quick test_payload_to_json_no_agent;
      Alcotest.test_case "payload with agent" `Quick test_payload_to_json_with_agent;
    ];
    "file_target", [
      Alcotest.test_case "append" `Quick test_file_append_target;
    ];
    "custom_target", [
      Alcotest.test_case "delivery" `Quick test_custom_target;
      Alcotest.test_case "error handling" `Quick test_custom_target_error_handling;
      Alcotest.test_case "multiple targets" `Quick test_multiple_targets;
    ];
    "lifecycle", [
      Alcotest.test_case "delivered count" `Quick test_delivered_count;
      Alcotest.test_case "create defaults" `Quick test_create_default_params;
      Alcotest.test_case "stop idempotent" `Quick test_stop_idempotent;
    ];
  ]
