(** Swarm runner — multi-agent parallel execution with convergence loops.

    Runs N agents in one of three modes:
    - {b Decentralized}: all agents run in parallel via [Eio.Fiber.List.map].
    - {b Supervisor}: workers run in parallel, then supervisor synthesizes.
    - {b Pipeline}: agents run sequentially, each receiving previous output.

    When [convergence] is configured, the runner loops until the metric
    reaches the target, patience is exhausted, or max_iterations is hit.
    Swarm state is protected by [Eio.Mutex] during convergence loops.

    When [enable_streaming = true] in {!Swarm_types.swarm_config}, agents
    communicate via {!Swarm_channel.t} message streams instead of
    collecting batch results.  The external interface ({!run}) is
    unchanged — streaming is an internal optimization.

    @since 0.42.0 *)

open Agent_sdk

(** {1 Main entry point} *)

(** Run a swarm of agents according to {!Swarm_types.swarm_config}.

    Without [convergence]: single pass, all agents run once.
    With [convergence]: iterative loop with metric evaluation.
    With [timeout_sec]: aborts with [TaskTimeout] if exceeded.
    With [enable_streaming = true]: uses {!Swarm_channel}-based message
    passing between agents (opt-in, backward compatible).

    @param callbacks Optional lifecycle callbacks (default: {!Swarm_types.no_callbacks}) *)
val run :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  ?callbacks:Swarm_types.swarm_callbacks ->
  Swarm_types.swarm_config ->
  (Swarm_types.swarm_result, Error.sdk_error) result

(** {1 Metric utilities} *)

(** Evaluate a metric source. [Shell_command] runs a shell command and
    parses stdout as float. [Callback] invokes the function directly. *)
val eval_metric : Swarm_types.metric_source -> (float, string) result

(** Aggregate a list of scores using the given strategy. *)
val aggregate_scores :
  Swarm_types.aggregate_strategy -> float list -> float

(** Extract text content from an API response. *)
val text_of_response : Types.api_response -> string
