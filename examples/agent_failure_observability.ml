open Base
(** AgentFailed observability example.

    Demonstrates the [AgentFailed] native variant added in v0.154.0.
    Subscribers that want to react to agent failures directly (log to
    Sentry, publish to a dashboard, raise an alert) no longer need to
    pattern-match on [AgentCompleted] and destructure its [result] —
    they can match on [AgentFailed] and receive the [Error.sdk_error]
    straight from the envelope.

    The example uses the orchestrator's "unknown agent" failure path,
    so it needs no LLM, no network, and no mock server.

    Usage:
      dune exec --root . examples/agent_failure_observability.exe *)

open Agent_sdk

let red s = Printf.sprintf "\027[31m%s\027[0m" s
let green s = Printf.sprintf "\027[32m%s\027[0m" s
let dim s = Printf.sprintf "\027[2m%s\027[0m" s

let print_event (event : Event_bus.event) =
  let name = Event_forward.event_type_name event in
  let corr = event.meta.correlation_id in
  match event.payload with
  | AgentStarted { agent_name; task_id } ->
    Printf.printf
      "  %s %s agent=%s task=%s\n%!"
      (dim (Printf.sprintf "[%s]" corr))
      (green name)
      agent_name
      task_id
  | AgentCompleted { agent_name; result; elapsed; _ } ->
    let status =
      match result with
      | Ok _ -> green "ok"
      | Error _ -> red "err"
    in
    Printf.printf
      "  %s %s agent=%s status=%s elapsed=%.3fs\n%!"
      (dim (Printf.sprintf "[%s]" corr))
      name
      agent_name
      status
      elapsed
  | AgentFailed { agent_name; task_id; error; elapsed } ->
    Printf.printf
      "  %s %s agent=%s task=%s elapsed=%.3fs\n%!"
      (dim (Printf.sprintf "[%s]" corr))
      (red name)
      agent_name
      task_id
      elapsed;
    Printf.printf "    %s %s\n%!" (red "error:") (Error.to_string error)
  | _ -> ()
;;

let () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let bus = Event_bus.create () in
  let sub = Event_bus.subscribe bus in
  Printf.printf "\n=== Agent failure observability (v0.154.0) ===\n\n";
  Printf.printf "Subscribing to Event_bus and running a task against an\n";
  Printf.printf "agent that is not registered with the orchestrator.\n";
  Printf.printf "Expect: AgentStarted -> AgentCompleted(Error) -> AgentFailed\n\n";
  let orch_cfg = { Orchestrator.default_config with event_bus = Some bus } in
  let orch = Orchestrator.create ~config:orch_cfg [] in
  let task =
    { Orchestrator.id = "demo-failure"
    ; prompt = "anything"
    ; agent_name = "nonexistent-agent"
    }
  in
  let _tr = Orchestrator.run_task ~sw orch task in
  Printf.printf "Events received (in order):\n";
  List.iter print_event (Event_bus.drain sub);
  Printf.printf
    "\n%s the AgentFailed event exposes the error directly —\n"
    (dim "observation:");
  Printf.printf "  downstream observers do not need to destructure\n";
  Printf.printf "  AgentCompleted.result to react to failures.\n\n"
;;
