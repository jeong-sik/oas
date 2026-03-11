(** Tests for orchestrator.ml — multi-agent task distribution. *)

open Alcotest
open Agent_sdk
open Types

(* ── Helpers ──────────────────────────────────────────────────────── *)

(** Build a mock api_response with the given text. *)
let mock_response text = {
  id = "resp-1";
  model = "mock";
  stop_reason = EndTurn;
  content = [Text text];
  usage = Some {
    input_tokens = 10;
    output_tokens = 5;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
  };
}

let _mock_error_response = {
  id = "resp-err";
  model = "mock";
  stop_reason = EndTurn;
  content = [Text "error"];
  usage = None;
}

(** Build a task_result from an Ok response. *)
let ok_result ?(task_id="t1") ?(agent_name="a") text = {
  Orchestrator.task_id;
  agent_name;
  result = Ok (mock_response text);
  elapsed = 0.01;
}

(** Build a task_result from an Error. *)
let err_result ?(task_id="t2") ?(agent_name="b") msg = {
  Orchestrator.task_id;
  agent_name;
  result = Error msg;
  elapsed = 0.0;
}

(** Make a mock agent that does not call any API.
    We create a real Agent.t with env#net from Eio_main.
    The agent itself is valid; we just never call Agent.run in
    pure tests. *)
let make_mock_agent env =
  Agent.create ~net:env#net
    ~config:{ default_config with name = "mock"; max_turns = 1 } ()

(* ── create ───────────────────────────────────────────────────────── *)

let test_create_empty () =
  let orch = Orchestrator.create [] in
  check int "no agents" 0 (List.length orch.agents)

let test_create_with_agents () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [("alpha", a)] in
  check int "one agent" 1 (List.length orch.agents)

let test_create_with_config () =
  let cfg = { Orchestrator.default_config with max_parallel = 8 } in
  let orch = Orchestrator.create ~config:cfg [] in
  check int "max_parallel" 8 orch.config.max_parallel

let test_create_preserves_agent_order () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  let orch = Orchestrator.create [("first", a); ("second", b)] in
  let names = List.map fst orch.agents in
  check (list string) "order preserved" ["first"; "second"] names

(* ── add_agent ────────────────────────────────────────────────────── *)

let test_add_agent () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [] in
  let orch' = Orchestrator.add_agent orch "new" a in
  check int "one agent after add" 1 (List.length orch'.agents);
  check int "original unchanged" 0 (List.length orch.agents)

let test_add_agent_prepends () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  let orch = Orchestrator.create [("a", a)] in
  let orch' = Orchestrator.add_agent orch "b" b in
  let names = List.map fst orch'.agents in
  check string "new agent first" "b" (List.hd names)

(* ── find_agent ───────────────────────────────────────────────────── *)

let test_find_agent_present () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [("alpha", a)] in
  check bool "found" true (Option.is_some (Orchestrator.find_agent orch "alpha"))

let test_find_agent_missing () =
  let orch = Orchestrator.create [] in
  check bool "not found" true (Option.is_none (Orchestrator.find_agent orch "ghost"))

let test_find_agent_first_match () =
  Eio_main.run @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  (* Two agents with different names *)
  let orch = Orchestrator.create [("x", a); ("y", b)] in
  check bool "x found" true (Option.is_some (Orchestrator.find_agent orch "x"));
  check bool "y found" true (Option.is_some (Orchestrator.find_agent orch "y"))

(* ── plan construction ────────────────────────────────────────────── *)

let test_plan_sequential () =
  let tasks = [{ Orchestrator.id = "1"; prompt = "hello"; agent_name = "a" }] in
  match Orchestrator.Sequential tasks with
  | Orchestrator.Sequential ts -> check int "one task" 1 (List.length ts)
  | _ -> fail "wrong plan variant"

let test_plan_parallel () =
  let tasks = [
    { Orchestrator.id = "1"; prompt = "a"; agent_name = "x" };
    { Orchestrator.id = "2"; prompt = "b"; agent_name = "y" };
  ] in
  match Orchestrator.Parallel tasks with
  | Orchestrator.Parallel ts -> check int "two tasks" 2 (List.length ts)
  | _ -> fail "wrong plan variant"

let test_plan_fanout () =
  let plan = Orchestrator.FanOut { prompt = "test"; agents = ["a"; "b"; "c"] } in
  match plan with
  | Orchestrator.FanOut { prompt; agents } ->
    check string "prompt" "test" prompt;
    check int "3 agents" 3 (List.length agents)
  | _ -> fail "wrong plan variant"

let test_plan_pipeline () =
  let tasks = [
    { Orchestrator.id = "s1"; prompt = "step1"; agent_name = "a" };
    { Orchestrator.id = "s2"; prompt = "step2"; agent_name = "b" };
  ] in
  match Orchestrator.Pipeline tasks with
  | Orchestrator.Pipeline ts -> check int "two stages" 2 (List.length ts)
  | _ -> fail "wrong plan variant"

let test_plan_empty_sequential () =
  match Orchestrator.Sequential [] with
  | Orchestrator.Sequential ts -> check int "empty" 0 (List.length ts)
  | _ -> fail "wrong plan variant"

(* ── task_result ──────────────────────────────────────────────────── *)

let test_task_result_ok_fields () =
  let tr = ok_result ~task_id:"t-ok" ~agent_name:"alpha" "hello" in
  check string "task_id" "t-ok" tr.task_id;
  check string "agent_name" "alpha" tr.agent_name;
  check bool "is ok" true (Result.is_ok tr.result)

let test_task_result_error_fields () =
  let tr = err_result ~task_id:"t-err" ~agent_name:"beta" "boom" in
  check string "task_id" "t-err" tr.task_id;
  check string "agent_name" "beta" tr.agent_name;
  check bool "is error" true (Result.is_error tr.result)

let test_task_result_elapsed () =
  let tr = { (ok_result "x") with elapsed = 1.5 } in
  check bool "elapsed > 0" true (tr.elapsed > 0.0)

(* ── collect_text ─────────────────────────────────────────────────── *)

let test_collect_text_empty () =
  check string "empty list" "" (Orchestrator.collect_text [])

let test_collect_text_single_ok () =
  let results = [ok_result "hello world"] in
  check string "single text" "hello world" (Orchestrator.collect_text results)

let test_collect_text_multiple_ok () =
  let results = [
    ok_result ~task_id:"1" "first";
    ok_result ~task_id:"2" "second";
  ] in
  check string "joined" "first\nsecond" (Orchestrator.collect_text results)

let test_collect_text_skips_errors () =
  let results = [
    ok_result ~task_id:"1" "good";
    err_result ~task_id:"2" "bad";
    ok_result ~task_id:"3" "also good";
  ] in
  check string "errors skipped" "good\nalso good" (Orchestrator.collect_text results)

let test_collect_text_all_errors () =
  let results = [
    err_result ~task_id:"1" "err1";
    err_result ~task_id:"2" "err2";
  ] in
  check string "all errors" "" (Orchestrator.collect_text results)

let test_collect_text_multiblock () =
  (* Response with multiple Text blocks *)
  let resp = {
    id = "r"; model = "m"; stop_reason = EndTurn;
    content = [Text "line1"; Text "line2"]; usage = None;
  } in
  let tr = { Orchestrator.task_id = "t"; agent_name = "a";
             result = Ok resp; elapsed = 0.0 } in
  check string "multi text blocks" "line1\nline2" (Orchestrator.collect_text [tr])

(* ── all_ok ───────────────────────────────────────────────────────── *)

let test_all_ok_empty () =
  check bool "empty is ok" true (Orchestrator.all_ok [])

let test_all_ok_true () =
  let results = [ok_result "a"; ok_result "b"] in
  check bool "all ok" true (Orchestrator.all_ok results)

let test_all_ok_false () =
  let results = [ok_result "a"; err_result "oops"] in
  check bool "not all ok" false (Orchestrator.all_ok results)

let test_all_ok_single_error () =
  let results = [err_result "fail"] in
  check bool "single error" false (Orchestrator.all_ok results)

(* ── config defaults ──────────────────────────────────────────────── *)

let test_default_config_max_parallel () =
  check int "default max_parallel" 4 Orchestrator.default_config.max_parallel

let test_default_config_no_shared_context () =
  check bool "no shared context" true
    (Option.is_none Orchestrator.default_config.shared_context)

let test_default_config_no_callbacks () =
  check bool "no on_task_start" true
    (Option.is_none Orchestrator.default_config.on_task_start);
  check bool "no on_task_complete" true
    (Option.is_none Orchestrator.default_config.on_task_complete)

let test_default_config_no_timeout () =
  check bool "no timeout" true
    (Option.is_none Orchestrator.default_config.timeout_per_task)

(* ── callback tracking ────────────────────────────────────────────── *)

let test_callback_on_task_start_tracking () =
  let started = ref [] in
  let cfg = { Orchestrator.default_config with
    on_task_start = Some (fun task -> started := task.id :: !started);
  } in
  (* Verify the callback is set *)
  check bool "callback is set" true (Option.is_some cfg.on_task_start);
  (* Manually invoke to test *)
  let task : Orchestrator.task = { id = "cb-1"; prompt = "hi"; agent_name = "a" } in
  (match cfg.on_task_start with
   | Some cb -> cb task
   | None -> ());
  check (list string) "started recorded" ["cb-1"] !started

let test_callback_on_task_complete_tracking () =
  let completed = ref [] in
  let cfg = { Orchestrator.default_config with
    on_task_complete = Some (fun tr -> completed := tr.Orchestrator.task_id :: !completed);
  } in
  let tr = ok_result ~task_id:"cb-2" "done" in
  (match cfg.on_task_complete with
   | Some cb -> cb tr
   | None -> ());
  check (list string) "completed recorded" ["cb-2"] !completed

(* ── run_task: unknown agent error ────────────────────────────────── *)

let test_run_task_unknown_agent () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task =
    { id = "t1"; prompt = "hi"; agent_name = "ghost" }
  in
  let tr = Orchestrator.run_task ~sw orch task in
  check bool "is error" true (Result.is_error tr.result);
  match tr.result with
  | Error msg ->
    check bool "mentions agent name" true
      (String.length msg > 0 && String.sub msg 0 14 = "Unknown agent:")
  | Ok _ -> fail "expected error"

let test_run_task_unknown_agent_callbacks_called () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let start_called = ref false in
  let complete_called = ref false in
  let cfg = { Orchestrator.default_config with
    on_task_start = Some (fun _ -> start_called := true);
    on_task_complete = Some (fun _ -> complete_called := true);
  } in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task =
    { id = "t1"; prompt = "hi"; agent_name = "missing" }
  in
  let _tr = Orchestrator.run_task ~sw orch task in
  check bool "start called" true !start_called;
  check bool "complete called" true !complete_called

let test_run_task_elapsed_nonnegative () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task =
    { id = "t1"; prompt = "hi"; agent_name = "nope" }
  in
  let tr = Orchestrator.run_task ~sw orch task in
  check bool "elapsed >= 0" true (tr.elapsed >= 0.0)

(* ── task field access ────────────────────────────────────────────── *)

let test_task_fields () =
  let task : Orchestrator.task =
    { id = "abc"; prompt = "do something"; agent_name = "worker" }
  in
  check string "id" "abc" task.id;
  check string "prompt" "do something" task.prompt;
  check string "agent_name" "worker" task.agent_name

(* ── event_bus ───────────────────────────────────────────────────── *)

let test_event_bus_config_none () =
  check bool "default event_bus is None" true
    (Option.is_none Orchestrator.default_config.event_bus)

let test_event_bus_receives_started () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task =
    { id = "eb-1"; prompt = "hi"; agent_name = "ghost" }
  in
  let _tr = Orchestrator.run_task ~sw orch task in
  let events = Event_bus.drain sub in
  (* Should have AgentStarted + AgentCompleted *)
  check bool "has events" true (List.length events >= 2);
  match List.hd events with
  | AgentStarted r ->
    check string "agent_name" "ghost" r.agent_name;
    check string "task_id" "eb-1" r.task_id
  | _ -> fail "expected AgentStarted first"

let test_event_bus_receives_completed () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task =
    { id = "eb-2"; prompt = "hi"; agent_name = "phantom" }
  in
  let _tr = Orchestrator.run_task ~sw orch task in
  let events = Event_bus.drain sub in
  let last = List.nth events (List.length events - 1) in
  match last with
  | AgentCompleted r ->
    check string "agent_name" "phantom" r.agent_name;
    check string "task_id" "eb-2" r.task_id;
    check bool "result is error (unknown agent)" true (Result.is_error r.result)
  | _ -> fail "expected AgentCompleted last"

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  run "Orchestrator" [
    "create", [
      test_case "empty" `Quick test_create_empty;
      test_case "with agents" `Quick test_create_with_agents;
      test_case "with config" `Quick test_create_with_config;
      test_case "preserves order" `Quick test_create_preserves_agent_order;
    ];
    "add_agent", [
      test_case "add one" `Quick test_add_agent;
      test_case "prepends" `Quick test_add_agent_prepends;
    ];
    "find_agent", [
      test_case "present" `Quick test_find_agent_present;
      test_case "missing" `Quick test_find_agent_missing;
      test_case "first match" `Quick test_find_agent_first_match;
    ];
    "plan", [
      test_case "sequential" `Quick test_plan_sequential;
      test_case "parallel" `Quick test_plan_parallel;
      test_case "fanout" `Quick test_plan_fanout;
      test_case "pipeline" `Quick test_plan_pipeline;
      test_case "empty sequential" `Quick test_plan_empty_sequential;
    ];
    "task", [
      test_case "fields" `Quick test_task_fields;
    ];
    "task_result", [
      test_case "ok fields" `Quick test_task_result_ok_fields;
      test_case "error fields" `Quick test_task_result_error_fields;
      test_case "elapsed" `Quick test_task_result_elapsed;
    ];
    "collect_text", [
      test_case "empty" `Quick test_collect_text_empty;
      test_case "single ok" `Quick test_collect_text_single_ok;
      test_case "multiple ok" `Quick test_collect_text_multiple_ok;
      test_case "skips errors" `Quick test_collect_text_skips_errors;
      test_case "all errors" `Quick test_collect_text_all_errors;
      test_case "multi text blocks" `Quick test_collect_text_multiblock;
    ];
    "all_ok", [
      test_case "empty" `Quick test_all_ok_empty;
      test_case "all ok" `Quick test_all_ok_true;
      test_case "mixed" `Quick test_all_ok_false;
      test_case "single error" `Quick test_all_ok_single_error;
    ];
    "config", [
      test_case "default max_parallel" `Quick test_default_config_max_parallel;
      test_case "no shared context" `Quick test_default_config_no_shared_context;
      test_case "no callbacks" `Quick test_default_config_no_callbacks;
      test_case "no timeout" `Quick test_default_config_no_timeout;
    ];
    "callbacks", [
      test_case "on_task_start" `Quick test_callback_on_task_start_tracking;
      test_case "on_task_complete" `Quick test_callback_on_task_complete_tracking;
    ];
    "run_task", [
      test_case "unknown agent" `Quick test_run_task_unknown_agent;
      test_case "callbacks called" `Quick test_run_task_unknown_agent_callbacks_called;
      test_case "elapsed nonneg" `Quick test_run_task_elapsed_nonnegative;
    ];
    "event_bus", [
      test_case "config none" `Quick test_event_bus_config_none;
      test_case "receives started" `Quick test_event_bus_receives_started;
      test_case "receives completed" `Quick test_event_bus_receives_completed;
    ];
  ]
