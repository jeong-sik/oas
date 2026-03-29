(** Traced Swarm — Runner.run with automatic per-agent trace recording.

    Convenience layer on top of {!Runner} and {!Raw_trace}.
    Creates N agents from a base builder, attaches a unique JSONL trace
    sink to each, runs them via {!Runner.run}, and collects
    {!Raw_trace.run_summary} values from the resulting trace files.

    @since 0.51.0

    @stability Evolving
    @since 0.93.1 *)

open Agent_sdk

type traced_run_result = {
  swarm_result : Swarm_types.swarm_result;
  summaries : Raw_trace.run_summary list;
  trace_dir : string;
}

(** Run [workers] agents in parallel with automatic trace collection.

    Each agent is derived from [base_builder] with a unique name
    (worker-1, worker-2, ...) and a per-agent {!Raw_trace} sink
    writing to [trace_dir/worker-N.jsonl].

    @param mode Orchestration mode (default: Decentralized)
    @param trace_dir Directory for JSONL trace files (default: temp dir) *)
val run_traced :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock ; process_mgr : _ Eio.Process.mgr ; .. > ->
  workers:int ->
  base_builder:Builder.t ->
  ?mode:Swarm_types.orchestration_mode ->
  ?callbacks:Swarm_types.swarm_callbacks ->
  ?trace_dir:string ->
  prompt:string ->
  unit ->
  (traced_run_result, Error.sdk_error) result

(** Collect {!Raw_trace.run_summary} from all .jsonl files in [trace_dir].
    Skips files that cannot be parsed or contain no valid runs. *)
val collect_summaries :
  trace_dir:string ->
  (Raw_trace.run_summary list, Error.sdk_error) result
