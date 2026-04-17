(** Multi-vendor Event_bus taxonomy invariants.

    Every provider OAS supports — Anthropic, OpenAI (+ compatibles),
    Gemini, GLM, OpenRouter, llama.cpp, Ollama, vLLM, LM Studio, etc. —
    produces the same native Event_bus payload variants when running
    through OAS.  This test encodes the taxonomy invariants so any
    refactor that changes the public surface breaks the build here
    instead of silently diverging between providers.

    Invariants checked (see docs/EVENT-CATALOG.md Invariants I1-I6):

    - I1/I2: Envelope fields (correlation_id, run_id, ts) are present and
      preserved across every native variant.
    - event_forward.event_type_name returns the documented flat
      dot-separated string for each native variant.
    - agent_name_of_event resolves correctly for agent-scoped variants.
    - The golden lifecycle transcript (AgentStarted -> TurnStarted ->
      ToolCalled -> ToolCompleted -> TurnCompleted -> AgentCompleted)
      round-trips through the bus without reordering or loss.

    This test does NOT require an LLM; it verifies taxonomy invariants
    directly. A live-mode companion test (not yet authored) would drive
    an actual provider end-to-end and assert the same golden transcript
    emerges. *)

open Alcotest
open Agent_sdk

(* Canonical Types values used for payload construction *)
let stub_api_response : Types.api_response =
  { id = "msg_test";
    model = "any-model";
    stop_reason = Types.EndTurn;
    content = [Types.Text "done"];
    usage = None;
    telemetry = None }

let stub_tool_result : Types.tool_result =
  Ok { Types.content = "ok" }

(* ── I1/I2: envelope preserved across variants ────────────────── *)

let test_envelope_preserved_across_variants () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let corr = "mv-corr" in
  let run_id = "mv-run" in
  let mk p = Event_bus.mk_event ~correlation_id:corr ~run_id p in
  let variants : Event_bus.payload list = [
    AgentStarted { agent_name = "alpha"; task_id = "t1" };
    TurnStarted { agent_name = "alpha"; turn = 0 };
    ToolCalled { agent_name = "alpha"; tool_name = "echo"; input = `Null };
    ToolCompleted { agent_name = "alpha"; tool_name = "echo"; output = stub_tool_result };
    TurnCompleted { agent_name = "alpha"; turn = 0 };
    HandoffRequested { from_agent = "alpha"; to_agent = "beta"; reason = "delegate" };
    HandoffCompleted { from_agent = "alpha"; to_agent = "beta"; elapsed = 0.5 };
    ElicitationCompleted { agent_name = "alpha"; question = "?";
                           response = Hooks.Declined };
    ContextOverflowImminent { agent_name = "alpha";
                              estimated_tokens = 1; limit_tokens = 2; ratio = 0.5 };
    ContextCompactStarted { agent_name = "alpha"; trigger = "proactive" };
    ContextCompacted { agent_name = "alpha";
                       before_tokens = 100; after_tokens = 50; phase = "proactive(50%)" };
    AgentCompleted { agent_name = "alpha"; task_id = "t1";
                     result = Ok stub_api_response; elapsed = 1.0 };
    Custom ("example.namespace.ok", `Null);
  ] in
  List.iter (fun p -> Event_bus.publish bus (mk p)) variants;
  let received = Event_bus.drain sub in
  check int "count" (List.length variants) (List.length received);
  List.iter (fun (e : Event_bus.event) ->
    check string "correlation_id preserved" corr e.meta.correlation_id;
    check string "run_id preserved" run_id e.meta.run_id;
    check bool "ts populated" true (e.meta.ts > 0.0))
    received

(* ── event_type_name mapping is stable ─────────────────────────── *)

let test_event_type_name_mapping () =
  let cases : (Event_bus.payload * string) list = [
    AgentStarted { agent_name = "a"; task_id = "t" }, "agent.started";
    AgentCompleted { agent_name = "a"; task_id = "t";
                     result = Ok stub_api_response; elapsed = 0.0 }, "agent.completed";
    AgentFailed { agent_name = "a"; task_id = "t";
                  error = Error.Config (Error.InvalidConfig { field = "f"; detail = "x" });
                  elapsed = 0.0 }, "agent.failed";
    TurnStarted { agent_name = "a"; turn = 0 }, "turn.started";
    TurnCompleted { agent_name = "a"; turn = 0 }, "turn.completed";
    ToolCalled { agent_name = "a"; tool_name = "t"; input = `Null }, "tool.called";
    ToolCompleted { agent_name = "a"; tool_name = "t"; output = stub_tool_result },
      "tool.completed";
    HandoffRequested { from_agent = "a"; to_agent = "b"; reason = "r" },
      "handoff.requested";
    HandoffCompleted { from_agent = "a"; to_agent = "b"; elapsed = 0.0 },
      "handoff.completed";
    ElicitationCompleted { agent_name = "a"; question = "?";
                           response = Hooks.Declined }, "elicitation.completed";
    ContextOverflowImminent { agent_name = "a";
                              estimated_tokens = 1; limit_tokens = 2; ratio = 0.5 },
      "context.overflow_imminent";
    ContextCompactStarted { agent_name = "a"; trigger = "proactive" },
      "context.compact_started";
    ContextCompacted { agent_name = "a"; before_tokens = 1; after_tokens = 1;
                       phase = "x" }, "context.compacted";
    (* Custom names pass through verbatim — no "custom." prefix. *)
    Custom ("runtime.session_started", `Null), "runtime.session_started";
    Custom ("durable.tool_called", `Null), "durable.tool_called";
    Custom ("provider.anthropic.cache_hit", `Null),
      "provider.anthropic.cache_hit";
    Custom ("myext.foo", `Null), "myext.foo";
  ] in
  List.iter (fun (payload, expected) ->
    let ev = Event_bus.mk_event payload in
    check string (Printf.sprintf "event_type_name for %s" expected)
      expected (Event_forward.event_type_name ev))
    cases

(* ── Agent-scoped filter resolves correctly ───────────────────── *)

let test_filter_agent_covers_new_variants () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe ~filter:(Event_bus.filter_agent "alpha") bus in
  let mk p = Event_bus.mk_event p in
  (* HandoffRequested / HandoffCompleted are scoped by from_agent OR to_agent. *)
  Event_bus.publish bus (mk (HandoffRequested
    { from_agent = "alpha"; to_agent = "beta"; reason = "d" }));
  Event_bus.publish bus (mk (HandoffRequested
    { from_agent = "gamma"; to_agent = "alpha"; reason = "d" }));
  Event_bus.publish bus (mk (HandoffRequested
    { from_agent = "gamma"; to_agent = "delta"; reason = "d" }));
  Event_bus.publish bus (mk (AgentFailed
    { agent_name = "alpha"; task_id = "t";
      error = Error.Config (Error.InvalidConfig { field = "f"; detail = "x" }); elapsed = 0.0 }));
  Event_bus.publish bus (mk (AgentFailed
    { agent_name = "beta"; task_id = "t";
      error = Error.Config (Error.InvalidConfig { field = "f"; detail = "x" }); elapsed = 0.0 }));
  let received = Event_bus.drain sub in
  check int "alpha-scoped count" 3 (List.length received)

(* ── Golden lifecycle transcript ───────────────────────────────── *)

let test_golden_lifecycle_transcript () =
  Eio_main.run @@ fun _env ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let corr = "golden" in
  let run_id = "g1" in
  let mk p = Event_bus.mk_event ~correlation_id:corr ~run_id p in
  (* Any provider that runs an agent with one tool call should produce
     exactly this sequence on Event_bus. *)
  Event_bus.publish bus (mk (AgentStarted { agent_name = "a"; task_id = "t" }));
  Event_bus.publish bus (mk (TurnStarted { agent_name = "a"; turn = 0 }));
  Event_bus.publish bus (mk (ToolCalled
    { agent_name = "a"; tool_name = "echo"; input = `Null }));
  Event_bus.publish bus (mk (ToolCompleted
    { agent_name = "a"; tool_name = "echo"; output = stub_tool_result }));
  Event_bus.publish bus (mk (TurnCompleted { agent_name = "a"; turn = 0 }));
  Event_bus.publish bus (mk (AgentCompleted
    { agent_name = "a"; task_id = "t";
      result = Ok stub_api_response; elapsed = 0.1 }));
  let names =
    Event_bus.drain sub
    |> List.map (fun (e : Event_bus.event) -> Event_forward.event_type_name e)
  in
  check (list string) "golden transcript"
    ["agent.started"; "turn.started"; "tool.called"; "tool.completed";
     "turn.completed"; "agent.completed"]
    names

(* ── Reserved Custom namespace grammar ────────────────────────── *)

let test_reserved_namespace_grammar () =
  (* runtime.*, durable.*, provider.*, oas.* are reserved per catalog.
     Any well-formed Custom name uses '.'-separated lowercase segments. *)
  let ok_names = [
    "runtime.session_started";
    "durable.tool_called";
    "provider.anthropic.cache_hit";
    "provider.ollama.slot_busy";
    "oas.future";
    "myext.subsystem.event";
  ] in
  List.iter (fun name ->
    let ev = Event_bus.mk_event (Custom (name, `Null)) in
    check string (Printf.sprintf "passthrough %s" name)
      name (Event_forward.event_type_name ev))
    ok_names

let () =
  run "Multivendor_events" [
    "invariants", [
      test_case "envelope preserved across all variants" `Quick
        test_envelope_preserved_across_variants;
      test_case "event_type_name mapping stable" `Quick
        test_event_type_name_mapping;
      test_case "filter_agent scopes handoff + agent_failed" `Quick
        test_filter_agent_covers_new_variants;
      test_case "golden lifecycle transcript" `Quick
        test_golden_lifecycle_transcript;
      test_case "reserved namespace grammar" `Quick
        test_reserved_namespace_grammar;
    ];
  ]
