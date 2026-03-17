(** Turn pipeline: 6-stage decomposition of agent turn execution.

    [Input] -> [Parse] -> [Route] -> [Execute] -> [Collect] -> [Output]

    Each stage is a well-defined function.  The pipeline coordinates
    them, threading agent state through mutable updates.  This module
    replaces the monolithic [run_turn_core] in agent.ml. *)

type api_strategy =
  | Sync
  | Stream of { on_event: Types.sse_event -> unit }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted

(** Run a single agent turn through the 6-stage pipeline.
    Equivalent to the previous [run_turn_core]. *)
val run_turn :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  api_strategy:api_strategy ->
  ?raw_trace_run:Raw_trace.active_run ->
  Agent_types.t ->
  (turn_outcome, Error.sdk_error) result
