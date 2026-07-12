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
  | ToolsExecuted of Tool_failure_episode.completed_round option
  | IdleSkipped (** on_idle hook returned Skip — agent should stop gracefully. *)

(** Persist [state] using the same pre-commit checkpoint transaction as turn
    collection. The live agent state is not changed by this function. *)
val persist_turn_checkpoint_for_state
  :  Agent_types.t
  -> Agent_types.checkpoint_stage
  -> Types.agent_state
  -> (unit, Error.sdk_error) result

(** Run a single agent turn through the 6-stage pipeline.
    Equivalent to the previous [run_turn_core]. *)
val run_turn
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> api_strategy:api_strategy
  -> ?raw_trace_run:Raw_trace.active_run
  -> ?recovery_context:string
  -> ?on_provider_failure:(Provider_failure_attribution.t option -> unit)
  -> Agent_types.t
  -> (turn_outcome, Error.sdk_error) result
