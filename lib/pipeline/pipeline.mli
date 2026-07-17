(** Turn pipeline: 6-stage decomposition of agent turn execution.

    [Input] -> [Parse] -> [Route] -> [Collect] -> [Output]
    (Execute runs inside Output on StopToolUse.)

    Each stage is a well-defined function.  The pipeline coordinates
    them, threading agent state through mutable updates.  This module
    replaces the monolithic [run_turn_core] in agent.ml.

    @stability Evolving
    @since 0.93.1 *)

type api_strategy =
  | Sync
  | Stream of
      { on_event : Types.sse_event -> unit
      ; on_telemetry : (Llm_provider.Telemetry_event.t -> unit) option
      }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted of Agent_types.checkpoint_stage

(** Persist [state] using the same pre-commit checkpoint transaction as turn
    collection. The live agent state is not changed by this function. *)
val persist_turn_checkpoint_for_state
  :  Agent_types.t
  -> Agent_types.checkpoint_stage
  -> Types.agent_state
  -> (unit, Error.sdk_error) result

(** Run a single agent turn through the 6-stage pipeline.
    Equivalent to the previous [run_turn_core].

    [before_tool_execution], when present, runs exactly once for a non-empty
    tool batch after assistant collection has committed successfully and before
    the first tool hook or implementation starts.  Exceptions propagate and
    prevent tool execution.

    [before_provider_attempt], when present, receives the exact immutable
    binding selected for dispatch after configuration resolution and before
    provider I/O. A typed rejection prevents that provider attempt. *)
val run_turn
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> api_strategy:api_strategy
  -> ?raw_trace_run:Raw_trace.active_run
  -> ?on_provider_failure:(Provider_failure_attribution.t option -> unit)
  -> ?before_provider_attempt:(Binding_identity.t -> (unit, Error.sdk_error) result)
  -> ?before_tool_execution:(unit -> unit)
  -> Agent_types.t
  -> (turn_outcome, Error.sdk_error) result
