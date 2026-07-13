open Types
open Agent_types
open Agent_trace

(** Turn pipeline: 6-stage decomposition of agent turn execution.

    Replaces the monolithic run_turn_core with named stages:
    1. Input   — lifecycle, BeforeTurn hook, elicitation
    2. Parse   — BeforeTurnParams hook, context reduction, tool preparation
    3. Route   — provider selection, API call dispatch (sync/stream)
    4. Collect — usage accumulation, AfterTurn hook, events, message append
    5. Execute — exact-name tool execution on StopToolUse
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

(** Current timestamp. Prefer the Eio [clock] when available so tests and
    structured-concurrency code observe a consistent time source; fall back
    to [Unix.gettimeofday] only when running outside an Eio environment. *)
let timestamp_now ?clock () =
  match clock with
  | Some clock -> Eio.Time.now clock
  | None -> Unix.gettimeofday ()
;;

(* The bus enqueue is lossless and does not wait for subscriber drain. Do not
   absorb failures here: callers must observe cancellation or allocation/runtime
   faults instead of continuing after an event was not published. *)
let safe_publish ~log:_ bus event = Event_bus.publish bus event

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
