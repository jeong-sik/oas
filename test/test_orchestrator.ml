open Base
(** Tests for orchestrator.ml — multi-agent task distribution. *)

open Alcotest
open Agent_sdk
open Types

(* ── Helpers ──────────────────────────────────────────────────────── *)

(** Build a mock api_response with the given text. *)
let mock_response text =
  { id = "resp-1"
  ; model = "mock"
  ; stop_reason = EndTurn
  ; content = [ Text text ]
  ; usage =
      Some
        { input_tokens = 10
        ; output_tokens = 5
        ; cache_creation_input_tokens = 0
        ; cache_read_input_tokens = 0
        ; cost_usd = None
        }
  ; telemetry = None
  }
;;

let _mock_error_response =
  { id = "resp-err"
  ; model = "mock"
  ; stop_reason = EndTurn
  ; content = [ Text "error" ]
  ; usage = None
  ; telemetry = None
  }
;;

(** Build a task_result from an Ok response. *)
let ok_result ?(task_id = "t1") ?(agent_name = "a") text =
  { Orchestrator.task_id; agent_name; result = Ok (mock_response text); elapsed = 0.01 }
;;

(** Build a task_result from an Error. *)
let err_result ?(task_id = "t2") ?(agent_name = "b") (msg : Error.sdk_error) =
  { Orchestrator.task_id; agent_name; result = Error msg; elapsed = 0.0 }
;;

(** Make a mock agent that does not call any API.
    We create a real Agent.t with env#net from Eio_main.
    The agent itself is valid; we just never call Agent.run in
    pure tests. *)
let make_mock_agent env =
  Agent.create
    ~net:env#net
    ~config:{ default_config with name = "mock"; max_turns = 1 }
    ()
;;

(* ── create ───────────────────────────────────────────────────────── *)

let test_create_empty () =
  let orch = Orchestrator.create [] in
  check int "no agents" 0 (List.length orch.agents)
;;

let test_create_with_agents () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [ "alpha", a ] in
  check int "one agent" 1 (List.length orch.agents)
;;

let test_create_with_config () =
  let cfg = { Orchestrator.default_config with max_parallel = 8 } in
  let orch = Orchestrator.create ~config:cfg [] in
  check int "max_parallel" 8 orch.config.max_parallel
;;

let test_create_preserves_agent_order () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  let orch = Orchestrator.create [ "first", a; "second", b ] in
  let names = List.map fst orch.agents in
  check (list string) "order preserved" [ "first"; "second" ] names
;;

(* ── add_agent ────────────────────────────────────────────────────── *)

let test_add_agent () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [] in
  let orch' = Orchestrator.add_agent orch "new" a in
  check int "one agent after add" 1 (List.length orch'.agents);
  check int "original unchanged" 0 (List.length orch.agents)
;;

let test_add_agent_prepends () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  let orch = Orchestrator.create [ "a", a ] in
  let orch' = Orchestrator.add_agent orch "b" b in
  let names = List.map fst orch'.agents in
  check string "new agent first" "b" (List.hd names)
;;

(* ── find_agent ───────────────────────────────────────────────────── *)

let test_find_agent_present () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let orch = Orchestrator.create [ "alpha", a ] in
  check bool "found" true (Option.is_some (Orchestrator.find_agent orch "alpha"))
;;

let test_find_agent_missing () =
  let orch = Orchestrator.create [] in
  check bool "not found" true (Option.is_none (Orchestrator.find_agent orch "ghost"))
;;

let test_find_agent_first_match () =
  Eio_main.run
  @@ fun env ->
  let a = make_mock_agent env in
  let b = make_mock_agent env in
  (* Two agents with different names *)
  let orch = Orchestrator.create [ "x", a; "y", b ] in
  check bool "x found" true (Option.is_some (Orchestrator.find_agent orch "x"));
  check bool "y found" true (Option.is_some (Orchestrator.find_agent orch "y"))
;;

(* ── plan construction ────────────────────────────────────────────── *)

let test_plan_sequential () =
  let tasks = [ { Orchestrator.id = "1"; prompt = "hello"; agent_name = "a" } ] in
  match Orchestrator.Sequential tasks with
  | Orchestrator.Sequential ts -> check int "one task" 1 (List.length ts)
  | _ -> fail "wrong plan variant"
;;

let test_plan_parallel () =
  let tasks =
    [ { Orchestrator.id = "1"; prompt = "a"; agent_name = "x" }
    ; { Orchestrator.id = "2"; prompt = "b"; agent_name = "y" }
    ]
  in
  match Orchestrator.Parallel tasks with
  | Orchestrator.Parallel ts -> check int "two tasks" 2 (List.length ts)
  | _ -> fail "wrong plan variant"
;;

let test_plan_fanout () =
  let plan = Orchestrator.FanOut { prompt = "test"; agents = [ "a"; "b"; "c" ] } in
  match plan with
  | Orchestrator.FanOut { prompt; agents } ->
    check string "prompt" "test" prompt;
    check int "3 agents" 3 (List.length agents)
  | _ -> fail "wrong plan variant"
;;

let test_plan_pipeline () =
  let tasks =
    [ { Orchestrator.id = "s1"; prompt = "step1"; agent_name = "a" }
    ; { Orchestrator.id = "s2"; prompt = "step2"; agent_name = "b" }
    ]
  in
  match Orchestrator.Pipeline tasks with
  | Orchestrator.Pipeline ts -> check int "two stages" 2 (List.length ts)
  | _ -> fail "wrong plan variant"
;;

let test_plan_empty_sequential () =
  match Orchestrator.Sequential [] with
  | Orchestrator.Sequential ts -> check int "empty" 0 (List.length ts)
  | _ -> fail "wrong plan variant"
;;

(* ── task_result ──────────────────────────────────────────────────── *)

let test_task_result_ok_fields () =
  let tr = ok_result ~task_id:"t-ok" ~agent_name:"alpha" "hello" in
  check string "task_id" "t-ok" tr.task_id;
  check string "agent_name" "alpha" tr.agent_name;
  check bool "is ok" true (Result.is_ok tr.result)
;;

let test_task_result_error_fields () =
  let tr = err_result ~task_id:"t-err" ~agent_name:"beta" (Error.Internal "boom") in
  check string "task_id" "t-err" tr.task_id;
  check string "agent_name" "beta" tr.agent_name;
  check bool "is error" true (Result.is_error tr.result)
;;

let test_task_result_elapsed () =
  let tr = { (ok_result "x") with elapsed = 1.5 } in
  check bool "elapsed > 0" true (tr.elapsed > 0.0)
;;

(* ── collect_text ─────────────────────────────────────────────────── *)

let test_collect_text_empty () =
  check string "empty list" "" (Orchestrator.collect_text [])
;;

let test_collect_text_single_ok () =
  let results = [ ok_result "hello world" ] in
  check string "single text" "hello world" (Orchestrator.collect_text results)
;;

let test_collect_text_multiple_ok () =
  let results = [ ok_result ~task_id:"1" "first"; ok_result ~task_id:"2" "second" ] in
  check string "joined" "first\nsecond" (Orchestrator.collect_text results)
;;

let test_collect_text_skips_errors () =
  let results =
    [ ok_result ~task_id:"1" "good"
    ; err_result ~task_id:"2" (Error.Internal "bad")
    ; ok_result ~task_id:"3" "also good"
    ]
  in
  check string "errors skipped" "good\nalso good" (Orchestrator.collect_text results)
;;

let test_collect_text_all_errors () =
  let results =
    [ err_result ~task_id:"1" (Error.Internal "err1")
    ; err_result ~task_id:"2" (Error.Internal "err2")
    ]
  in
  check string "all errors" "" (Orchestrator.collect_text results)
;;

let test_collect_text_multiblock () =
  (* Response with multiple Text blocks *)
  let resp =
    { id = "r"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "line1"; Text "line2" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let tr =
    { Orchestrator.task_id = "t"; agent_name = "a"; result = Ok resp; elapsed = 0.0 }
  in
  check string "multi text blocks" "line1\nline2" (Orchestrator.collect_text [ tr ])
;;

(* ── all_ok ───────────────────────────────────────────────────────── *)

let test_all_ok_empty () = check bool "empty is ok" true (Orchestrator.all_ok [])

let test_all_ok_true () =
  let results = [ ok_result "a"; ok_result "b" ] in
  check bool "all ok" true (Orchestrator.all_ok results)
;;

let test_all_ok_false () =
  let results = [ ok_result "a"; err_result (Error.Internal "oops") ] in
  check bool "not all ok" false (Orchestrator.all_ok results)
;;

let test_all_ok_single_error () =
  let results = [ err_result (Error.Internal "fail") ] in
  check bool "single error" false (Orchestrator.all_ok results)
;;

(* ── config defaults ──────────────────────────────────────────────── *)

let test_default_config_max_parallel () =
  check int "default max_parallel" 4 Orchestrator.default_config.max_parallel
;;

let test_default_config_no_shared_context () =
  check
    bool
    "no shared context"
    true
    (Option.is_none Orchestrator.default_config.shared_context)
;;

let test_default_config_no_callbacks () =
  check
    bool
    "no on_task_start"
    true
    (Option.is_none Orchestrator.default_config.on_task_start);
  check
    bool
    "no on_task_complete"
    true
    (Option.is_none Orchestrator.default_config.on_task_complete)
;;

let test_default_config_no_timeout () =
  check
    bool
    "no timeout"
    true
    (Option.is_none Orchestrator.default_config.timeout_per_task)
;;

(* ── callback tracking ────────────────────────────────────────────── *)

let test_callback_on_task_start_tracking () =
  let started = ref [] in
  let cfg =
    { Orchestrator.default_config with
      on_task_start = Some (fun task -> started := task.id :: !started)
    }
  in
  (* Verify the callback is set *)
  check bool "callback is set" true (Option.is_some cfg.on_task_start);
  (* Manually invoke to test *)
  let task : Orchestrator.task = { id = "cb-1"; prompt = "hi"; agent_name = "a" } in
  (match cfg.on_task_start with
   | Some cb -> cb task
   | None -> ());
  check (list string) "started recorded" [ "cb-1" ] !started
;;

let test_callback_on_task_complete_tracking () =
  let completed = ref [] in
  let cfg =
    { Orchestrator.default_config with
      on_task_complete =
        Some (fun tr -> completed := tr.Orchestrator.task_id :: !completed)
    }
  in
  let tr = ok_result ~task_id:"cb-2" "done" in
  (match cfg.on_task_complete with
   | Some cb -> cb tr
   | None -> ());
  check (list string) "completed recorded" [ "cb-2" ] !completed
;;

(* ── run_task: unknown agent error ────────────────────────────────── *)

let test_run_task_unknown_agent () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task = { id = "t1"; prompt = "hi"; agent_name = "ghost" } in
  let tr = Orchestrator.run_task ~sw orch task in
  check bool "is error" true (Result.is_error tr.result);
  match tr.result with
  | Error msg ->
    check
      bool
      "mentions agent name"
      true
      (let s = Error.to_string msg in
       String.length s > 0 && String.sub s 0 14 = "Unknown agent:")
  | Ok _ -> fail "expected error"
;;

let test_run_task_unknown_agent_callbacks_called () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let start_called = ref false in
  let complete_called = ref false in
  let cfg =
    { Orchestrator.default_config with
      on_task_start = Some (fun _ -> start_called := true)
    ; on_task_complete = Some (fun _ -> complete_called := true)
    }
  in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task = { id = "t1"; prompt = "hi"; agent_name = "missing" } in
  let _tr = Orchestrator.run_task ~sw orch task in
  check bool "start called" true !start_called;
  check bool "complete called" true !complete_called
;;

let test_run_task_elapsed_nonnegative () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task = { id = "t1"; prompt = "hi"; agent_name = "nope" } in
  let tr = Orchestrator.run_task ~sw orch task in
  check bool "elapsed >= 0" true (tr.elapsed >= 0.0)
;;

(* ── task field access ────────────────────────────────────────────── *)

let test_task_fields () =
  let task : Orchestrator.task =
    { id = "abc"; prompt = "do something"; agent_name = "worker" }
  in
  check string "id" "abc" task.id;
  check string "prompt" "do something" task.prompt;
  check string "agent_name" "worker" task.agent_name
;;

(* ── event_bus ───────────────────────────────────────────────────── *)

let test_event_bus_config_none () =
  check
    bool
    "default event_bus is None"
    true
    (Option.is_none Orchestrator.default_config.event_bus)
;;

let test_event_bus_receives_started () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task = { id = "eb-1"; prompt = "hi"; agent_name = "ghost" } in
  let _tr = Orchestrator.run_task ~sw orch task in
  let events = Event_bus.drain sub in
  (* Should have AgentStarted + AgentCompleted *)
  check bool "has events" true (List.length events >= 2);
  match (List.hd events).payload with
  | AgentStarted r ->
    check string "agent_name" "ghost" r.agent_name;
    check string "task_id" "eb-1" r.task_id
  | _ -> fail "expected AgentStarted first"
;;

let test_event_bus_receives_completed () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  let cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:cfg [] in
  let task : Orchestrator.task = { id = "eb-2"; prompt = "hi"; agent_name = "phantom" } in
  let _tr = Orchestrator.run_task ~sw orch task in
  let events = Event_bus.drain sub in
  (* Since v0.154.0, error paths emit AgentFailed AFTER AgentCompleted,
     so AgentCompleted is no longer guaranteed to be the last event.
     Look it up by payload shape instead. *)
  let completed =
    List.find_opt
      (fun (e : Event_bus.event) ->
         match e.payload with
         | AgentCompleted _ -> true
         | _ -> false)
      events
  in
  match Option.map (fun (e : Event_bus.event) -> e.payload) completed with
  | Some (AgentCompleted r) ->
    check string "agent_name" "phantom" r.agent_name;
    check string "task_id" "eb-2" r.task_id;
    check bool "result is error (unknown agent)" true (Result.is_error r.result);
    (* AgentFailed companion event must also be present. *)
    check
      bool
      "AgentFailed companion event emitted"
      true
      (List.exists
         (fun (e : Event_bus.event) ->
            match e.payload with
            | AgentFailed _ -> true
            | _ -> false)
         events)
  | _ -> fail "expected AgentCompleted event"
;;

(* ── execute_sequential: unknown agents ─────────────────────────── *)

let test_execute_sequential_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute_sequential ~sw orch [] in
  check int "empty results" 0 (List.length results)
;;

let test_execute_sequential_multiple_unknown () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let tasks =
    [ { Orchestrator.id = "t1"; prompt = "a"; agent_name = "x" }
    ; { id = "t2"; prompt = "b"; agent_name = "y" }
    ]
  in
  let results = Orchestrator.execute_sequential ~sw orch tasks in
  check int "2 results" 2 (List.length results);
  check
    bool
    "both error"
    true
    (List.for_all (fun r -> Result.is_error r.Orchestrator.result) results)
;;

(* ── execute_parallel ─────────────────────────────────────────── *)

let test_execute_parallel_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute_parallel ~sw orch [] in
  check int "empty results" 0 (List.length results)
;;

let test_execute_parallel_unknown_agents () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let tasks =
    [ { Orchestrator.id = "p1"; prompt = "x"; agent_name = "a1" }
    ; { id = "p2"; prompt = "y"; agent_name = "a2" }
    ; { id = "p3"; prompt = "z"; agent_name = "a3" }
    ]
  in
  let results = Orchestrator.execute_parallel ~sw orch tasks in
  check int "3 results" 3 (List.length results);
  check
    bool
    "all errors"
    true
    (List.for_all (fun r -> Result.is_error r.Orchestrator.result) results)
;;

(* ── execute_fan_out ──────────────────────────────────────────── *)

let test_execute_fan_out_unknown_agents () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results =
    Orchestrator.execute_fan_out ~sw orch ~prompt:"test" ~agents:[ "a"; "b" ]
  in
  check int "2 results" 2 (List.length results);
  (* Each result should have fanout-N as task_id *)
  let task_ids = List.map (fun r -> r.Orchestrator.task_id) results in
  check bool "has fanout-0" true (List.mem "fanout-0" task_ids);
  check bool "has fanout-1" true (List.mem "fanout-1" task_ids)
;;

let test_execute_fan_out_empty_agents () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute_fan_out ~sw orch ~prompt:"test" ~agents:[] in
  check int "empty results" 0 (List.length results)
;;

(* ── execute_pipeline: with unknown agents ────────────────────── *)

let test_execute_pipeline_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute_pipeline ~sw orch [] in
  check int "empty results" 0 (List.length results)
;;

let test_execute_pipeline_error_carries_prev_text () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  (* Both agents unknown, so both fail; prev_text stays None *)
  let orch = Orchestrator.create [] in
  let tasks =
    [ { Orchestrator.id = "s1"; prompt = "step1"; agent_name = "ghost1" }
    ; { id = "s2"; prompt = "step2"; agent_name = "ghost2" }
    ]
  in
  let results = Orchestrator.execute_pipeline ~sw orch tasks in
  check int "2 results" 2 (List.length results);
  check
    bool
    "both errors"
    true
    (List.for_all (fun r -> Result.is_error r.Orchestrator.result) results)
;;

(* ── execute: plan dispatch ───────────────────────────────────── *)

let test_execute_dispatches_sequential () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute ~sw orch (Sequential []) in
  check int "sequential empty" 0 (List.length results)
;;

let test_execute_dispatches_parallel () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute ~sw orch (Parallel []) in
  check int "parallel empty" 0 (List.length results)
;;

let test_execute_dispatches_fanout () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute ~sw orch (FanOut { prompt = "test"; agents = [] }) in
  check int "fanout empty" 0 (List.length results)
;;

let test_execute_dispatches_pipeline () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.execute ~sw orch (Pipeline []) in
  check int "pipeline empty" 0 (List.length results)
;;

(* ── fan_out convenience ──────────────────────────────────────── *)

let test_fan_out_convenience () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  (* fan_out with no agents creates empty task list *)
  let results = Orchestrator.fan_out ~sw orch "test prompt" in
  check int "no agents = no results" 0 (List.length results)
;;

(* ── pipeline convenience ─────────────────────────────────────── *)

let test_pipeline_convenience () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let results = Orchestrator.pipeline ~sw orch [] in
  check int "empty pipeline" 0 (List.length results)
;;

(* ── collect_text: with non-text blocks ───────────────────────── *)

let test_collect_text_with_tool_use () =
  let resp =
    { id = "r"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content =
        [ Text "before"
        ; ToolUse { id = "tu1"; name = "search"; input = `Null }
        ; Text "after"
        ]
    ; usage = None
    ; telemetry = None
    }
  in
  let tr =
    { Orchestrator.task_id = "t"; agent_name = "a"; result = Ok resp; elapsed = 0.0 }
  in
  check string "filters non-text" "before\nafter" (Orchestrator.collect_text [ tr ])
;;

(* ── eval_condition: edge cases ───────────────────────────────── *)

let test_eval_condition_result_ok_none () =
  check bool "ResultOk with None" true (Orchestrator.eval_condition None ResultOk)
;;

let test_eval_condition_custom_none () =
  let pred _tr = false in
  check bool "Custom with None" true (Orchestrator.eval_condition None (Custom_cond pred))
;;

let test_eval_condition_text_contains_error () =
  let tr = err_result ~task_id:"e" (Error.Internal "err") in
  check
    bool
    "TextContains on error"
    false
    (Orchestrator.eval_condition (Some tr) (TextContains "anything"))
;;

let test_eval_condition_and_empty () =
  check bool "And empty" true (Orchestrator.eval_condition None (And []))
;;

let test_eval_condition_or_empty () =
  check bool "Or empty" false (Orchestrator.eval_condition None (Or []))
;;

(* ── execute_conditional: basic paths ─────────────────────────── *)

let test_execute_conditional_step () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task = { id = "s1"; prompt = "x"; agent_name = "ghost" } in
  let results = Orchestrator.execute_conditional ~sw orch (Step task) in
  check int "one result" 1 (List.length results);
  check bool "is error" true (Result.is_error (List.hd results).result)
;;

let test_execute_conditional_branch_true () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "ga" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "gb" } in
  let plan =
    Orchestrator.Branch
      { condition = Always; if_true = Step task_a; if_false = Step task_b }
  in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "one result" 1 (List.length results);
  check string "took true branch" "a" (List.hd results).task_id
;;

let test_execute_conditional_branch_false () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "ga" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "gb" } in
  let plan =
    Orchestrator.Branch
      { condition = Not Always; if_true = Step task_a; if_false = Step task_b }
  in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "one result" 1 (List.length results);
  check string "took false branch" "b" (List.hd results).task_id
;;

let test_execute_conditional_sequence () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "ga" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "gb" } in
  let plan = Orchestrator.Sequence [ Step task_a; Step task_b ] in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "two results" 2 (List.length results)
;;

let test_execute_conditional_sequence_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let plan = Orchestrator.Sequence [] in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "empty" 0 (List.length results)
;;

let test_execute_conditional_cond_parallel () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task_a : Orchestrator.task = { id = "a"; prompt = "a"; agent_name = "ga" } in
  let task_b : Orchestrator.task = { id = "b"; prompt = "b"; agent_name = "gb" } in
  let plan = Orchestrator.Cond_parallel [ Step task_a; Step task_b ] in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "two results" 2 (List.length results)
;;

let test_execute_conditional_loop_max_iterations () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task = { id = "loop"; prompt = "x"; agent_name = "ghost" } in
  (* until = Not Always means never stops, so it runs max_iterations times *)
  let plan =
    Orchestrator.Loop
      { body = Step task
      ; until = Not Always
      ; (* never true *)
        max_iterations = 3
      }
  in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "3 iterations" 3 (List.length results)
;;

let test_execute_conditional_loop_stops_on_condition () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let orch = Orchestrator.create [] in
  let task : Orchestrator.task = { id = "loop"; prompt = "x"; agent_name = "ghost" } in
  (* until = Always means stops after first iteration *)
  let plan =
    Orchestrator.Loop { body = Step task; until = Always; max_iterations = 10 }
  in
  let results = Orchestrator.execute_conditional ~sw orch plan in
  check int "1 iteration (until=Always)" 1 (List.length results)
;;

(* ── Consensus ─────────────────────────────────────────────────────── *)

let test_consensus_first_ok () =
  let results =
    [ err_result ~task_id:"c0" ~agent_name:"a0" (Error.Internal "fail")
    ; ok_result ~task_id:"c1" ~agent_name:"a1" "winner"
    ; ok_result ~task_id:"c2" ~agent_name:"a2" "also ok"
    ]
  in
  match Orchestrator.select_winner FirstOk results with
  | Some tr -> check string "first ok agent" "a1" tr.agent_name
  | None -> fail "expected a winner"
;;

let test_consensus_majority_text () =
  let results =
    [ ok_result ~task_id:"c0" ~agent_name:"a0" "yes"
    ; ok_result ~task_id:"c1" ~agent_name:"a1" "no"
    ; ok_result ~task_id:"c2" ~agent_name:"a2" "yes"
    ]
  in
  match Orchestrator.select_winner MajorityText results with
  | Some tr ->
    (* "yes" appears twice, "no" once *)
    let text = Orchestrator.collect_text [ tr ] in
    check string "majority text" "yes" text
  | None -> fail "expected a winner"
;;

let test_consensus_best_by () =
  let score_fn tr = tr.Orchestrator.elapsed in
  let results =
    [ { (ok_result ~task_id:"c0" ~agent_name:"slow" "x") with elapsed = 1.0 }
    ; { (ok_result ~task_id:"c1" ~agent_name:"fast" "y") with elapsed = 5.0 }
    ; { (ok_result ~task_id:"c2" ~agent_name:"mid" "z") with elapsed = 3.0 }
    ]
  in
  match Orchestrator.select_winner (BestBy score_fn) results with
  | Some tr -> check string "highest score" "fast" tr.agent_name
  | None -> fail "expected a winner"
;;

let test_consensus_all_error () =
  let results =
    [ err_result ~task_id:"c0" ~agent_name:"a0" (Error.Internal "e1")
    ; err_result ~task_id:"c1" ~agent_name:"a1" (Error.Internal "e2")
    ]
  in
  match Orchestrator.select_winner FirstOk results with
  | None -> ()
  | Some _ -> fail "expected no winner"
;;

(* ── Hierarchical ──────────────────────────────────────────────── *)

let test_hierarchical_basic () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  (* Two sub-orchestrators, each with no agents (returns UnknownAgent errors).
     Hierarchical wraps each sub-orch result into a single task_result. *)
  let sub1 = Orchestrator.create [] in
  let sub2 = Orchestrator.create [] in
  let plan1 =
    Orchestrator.Sequential
      [ { Orchestrator.id = "s1"; prompt = "p"; agent_name = "ghost" } ]
  in
  let plan2 =
    Orchestrator.Sequential [ { id = "s2"; prompt = "q"; agent_name = "ghost" } ]
  in
  let results =
    Orchestrator.execute_hierarchical ~sw [ "sub-a", sub1, plan1; "sub-b", sub2, plan2 ]
  in
  check int "2 hierarchical results" 2 (List.length results);
  List.iter
    (fun tr -> check bool "has result" true (Result.is_ok tr.Orchestrator.result))
    results
;;

(* ── Suite ────────────────────────────────────────────────────────── *)

let () =
  run
    "Orchestrator"
    [ ( "create"
      , [ test_case "empty" `Quick test_create_empty
        ; test_case "with agents" `Quick test_create_with_agents
        ; test_case "with config" `Quick test_create_with_config
        ; test_case "preserves order" `Quick test_create_preserves_agent_order
        ] )
    ; ( "add_agent"
      , [ test_case "add one" `Quick test_add_agent
        ; test_case "prepends" `Quick test_add_agent_prepends
        ] )
    ; ( "find_agent"
      , [ test_case "present" `Quick test_find_agent_present
        ; test_case "missing" `Quick test_find_agent_missing
        ; test_case "first match" `Quick test_find_agent_first_match
        ] )
    ; ( "plan"
      , [ test_case "sequential" `Quick test_plan_sequential
        ; test_case "parallel" `Quick test_plan_parallel
        ; test_case "fanout" `Quick test_plan_fanout
        ; test_case "pipeline" `Quick test_plan_pipeline
        ; test_case "empty sequential" `Quick test_plan_empty_sequential
        ] )
    ; "task", [ test_case "fields" `Quick test_task_fields ]
    ; ( "task_result"
      , [ test_case "ok fields" `Quick test_task_result_ok_fields
        ; test_case "error fields" `Quick test_task_result_error_fields
        ; test_case "elapsed" `Quick test_task_result_elapsed
        ] )
    ; ( "collect_text"
      , [ test_case "empty" `Quick test_collect_text_empty
        ; test_case "single ok" `Quick test_collect_text_single_ok
        ; test_case "multiple ok" `Quick test_collect_text_multiple_ok
        ; test_case "skips errors" `Quick test_collect_text_skips_errors
        ; test_case "all errors" `Quick test_collect_text_all_errors
        ; test_case "multi text blocks" `Quick test_collect_text_multiblock
        ; test_case "with tool_use" `Quick test_collect_text_with_tool_use
        ] )
    ; ( "all_ok"
      , [ test_case "empty" `Quick test_all_ok_empty
        ; test_case "all ok" `Quick test_all_ok_true
        ; test_case "mixed" `Quick test_all_ok_false
        ; test_case "single error" `Quick test_all_ok_single_error
        ] )
    ; ( "config"
      , [ test_case "default max_parallel" `Quick test_default_config_max_parallel
        ; test_case "no shared context" `Quick test_default_config_no_shared_context
        ; test_case "no callbacks" `Quick test_default_config_no_callbacks
        ; test_case "no timeout" `Quick test_default_config_no_timeout
        ] )
    ; ( "callbacks"
      , [ test_case "on_task_start" `Quick test_callback_on_task_start_tracking
        ; test_case "on_task_complete" `Quick test_callback_on_task_complete_tracking
        ] )
    ; ( "run_task"
      , [ test_case "unknown agent" `Quick test_run_task_unknown_agent
        ; test_case "callbacks called" `Quick test_run_task_unknown_agent_callbacks_called
        ; test_case "elapsed nonneg" `Quick test_run_task_elapsed_nonnegative
        ] )
    ; ( "event_bus"
      , [ test_case "config none" `Quick test_event_bus_config_none
        ; test_case "receives started" `Quick test_event_bus_receives_started
        ; test_case "receives completed" `Quick test_event_bus_receives_completed
        ] )
    ; ( "execute_sequential"
      , [ test_case "empty" `Quick test_execute_sequential_empty
        ; test_case "multiple unknown" `Quick test_execute_sequential_multiple_unknown
        ] )
    ; ( "execute_parallel"
      , [ test_case "empty" `Quick test_execute_parallel_empty
        ; test_case "unknown agents" `Quick test_execute_parallel_unknown_agents
        ] )
    ; ( "execute_fan_out"
      , [ test_case "unknown agents" `Quick test_execute_fan_out_unknown_agents
        ; test_case "empty agents" `Quick test_execute_fan_out_empty_agents
        ] )
    ; ( "execute_pipeline"
      , [ test_case "empty" `Quick test_execute_pipeline_empty
        ; test_case
            "error carries prev"
            `Quick
            test_execute_pipeline_error_carries_prev_text
        ] )
    ; ( "execute"
      , [ test_case "sequential" `Quick test_execute_dispatches_sequential
        ; test_case "parallel" `Quick test_execute_dispatches_parallel
        ; test_case "fanout" `Quick test_execute_dispatches_fanout
        ; test_case "pipeline" `Quick test_execute_dispatches_pipeline
        ] )
    ; ( "convenience"
      , [ test_case "fan_out" `Quick test_fan_out_convenience
        ; test_case "pipeline" `Quick test_pipeline_convenience
        ] )
    ; ( "eval_condition"
      , [ test_case "result_ok none" `Quick test_eval_condition_result_ok_none
        ; test_case "custom none" `Quick test_eval_condition_custom_none
        ; test_case "text_contains error" `Quick test_eval_condition_text_contains_error
        ; test_case "and empty" `Quick test_eval_condition_and_empty
        ; test_case "or empty" `Quick test_eval_condition_or_empty
        ] )
    ; ( "conditional"
      , [ test_case "step" `Quick test_execute_conditional_step
        ; test_case "branch true" `Quick test_execute_conditional_branch_true
        ; test_case "branch false" `Quick test_execute_conditional_branch_false
        ; test_case "sequence" `Quick test_execute_conditional_sequence
        ; test_case "sequence empty" `Quick test_execute_conditional_sequence_empty
        ; test_case "cond_parallel" `Quick test_execute_conditional_cond_parallel
        ; test_case
            "loop max_iterations"
            `Quick
            test_execute_conditional_loop_max_iterations
        ; test_case
            "loop stops on condition"
            `Quick
            test_execute_conditional_loop_stops_on_condition
        ] )
    ; ( "consensus"
      , [ test_case "first_ok" `Quick test_consensus_first_ok
        ; test_case "majority_text" `Quick test_consensus_majority_text
        ; test_case "best_by" `Quick test_consensus_best_by
        ; test_case "all_error" `Quick test_consensus_all_error
        ] )
    ; "hierarchical", [ test_case "sub_orchestrators" `Quick test_hierarchical_basic ]
    ]
;;
