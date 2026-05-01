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

let ( let* ) = Result.bind

type api_strategy =
  | Sync
  | Stream of { on_event : Types.sse_event -> unit }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted
  | IdleSkipped

let validate_completion_contract agent (response : Types.api_response) =
  let supports_tool_choice =
    match agent.options.provider with
    | Some cfg -> (Provider.capabilities_for_config cfg).supports_tool_choice
    | None -> false
  in
  let contract =
    Completion_contract.of_tool_choice
      ~supports_tool_choice
      agent.state.config.tool_choice
  in
  match
    Completion_contract.validate_response
      ~tools:(Tool_set.to_list agent.tools)
      ~required_tool_satisfaction:agent.options.required_tool_satisfaction
      ~contract
      response
  with
  | Ok () -> Ok ()
  | Error reason -> Error (Error.Agent (CompletionContractViolation { contract; reason }))
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
