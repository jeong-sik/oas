open Types
open Agent_types
open Agent_trace

(** Turn pipeline: 6-stage decomposition of agent turn execution.

    Replaces the monolithic run_turn_core with named stages:
    1. Input   — lifecycle, BeforeTurn hook, elicitation
    2. Parse   — BeforeTurnParams hook, context reduction, tool preparation
    3. Route   — provider selection, API call dispatch (sync/stream)
    4. Collect — usage accumulation, AfterTurn hook, events, message append
    5. Execute — tool execution on StopToolUse (idle detection, guardrails)
    6. Output  — stop reason → turn_outcome *)

open Types
open Agent_types
open Agent_trace
open Result_syntax

type api_strategy =
  | Sync
  | Stream of
      { on_event : Types.sse_event -> unit
      ; on_telemetry : (Llm_provider.Telemetry_event.t -> unit) option
      }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted
  | IdleSkipped

(* Publish an event, logging only genuine delivery failures. Re-raises
   [Eio.Cancel.Cancelled] so a fiber cancelled mid-publish — e.g. parked in
   [Event_bus.publish] on a full subscriber stream under the [Block] policy —
   unwinds instead of being absorbed by the catch-all (structured concurrency).
   Mirrors the transport-drain handler in [Http_client]. Shared by [Pipeline]
   and [Pipeline_stage_prepare], which previously held verbatim copies; the
   re-raise arm was missing from both. *)
let safe_publish ~log bus event =
  try Event_bus.publish bus event with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    Log.warn log "Event_bus.publish failed" [ Log.S ("error", Printexc.to_string exn) ]
;;

let%test "safe_publish does not absorb Eio cancellation" =
  (* Reproduces the swallowed-cancellation bug: a fiber blocked inside
     [Event_bus.publish] (Block policy, full one-slot stream, no drainer) is
     cancelled; [safe_publish] must let Cancelled propagate. Without the
     re-raise arm the catch-all returns unit and [propagated] stays false. *)
  Eio_main.run (fun _env ->
    let bus = Event_bus.create ~buffer_size:1 () in
    let _sub = Event_bus.subscribe bus in
    let log = Log.create ~module_name:"pipeline_common_test" () in
    let ev = Event_bus.mk_event (Event_bus.TurnStarted { agent_name = "t"; turn = 0 }) in
    (* Fill the single slot so the next publish parks in Stream.add. *)
    safe_publish ~log bus ev;
    let propagated = ref false in
    Eio.Fiber.first
      (fun () ->
         try safe_publish ~log bus ev with
         | Eio.Cancel.Cancelled _ as e ->
           propagated := true;
           raise e)
      (fun () ->
         (* Let the publish park on the full stream before winning the race. *)
         Eio.Fiber.yield ();
         Eio.Fiber.yield ());
    !propagated)
;;

let event_envelope agent : Event_bus.envelope =
  let session_id = Option.bind agent.options.raw_trace Raw_trace.session_id in
  let worker_run_id =
    Option.bind (lifecycle_snapshot agent) (fun s -> s.current_run_id)
  in
  let correlation_id =
    match session_id with
    | Some s -> s
    | None -> Event_bus.fresh_id ()
  in
  let run_id =
    match worker_run_id with
    | Some r -> r
    | None -> Event_bus.fresh_id ()
  in
  Event_bus.mk_envelope ~correlation_id ~run_id ()
;;

let total_prompt_tokens_for_agent agent messages =
  let raw_tokens =
    List.fold_left
      (fun acc msg -> acc + Context_reducer.estimate_message_tokens msg)
      0
      messages
  in
  raw_tokens + Agent_turn.tiered_memory_tokens agent.options.tiered_memory
;;
