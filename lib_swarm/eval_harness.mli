(** Eval harness — single-agent vs swarm baseline comparison.

    Runs the same task in single-agent mode and swarm mode (N agents),
    then compares accuracy, token cost, wall-clock time, and iteration
    count. Uses closure-based mocks — no real LLM needed.

    Reference: arXiv 2512.08296 (45% saturation threshold).

    @since 0.93.2 *)

open Agent_sdk

(** {1 Task definition} *)

type eval_task = {
  name: string;
  prompt: string;
  expected_output: string;  (** Substring match for accuracy check. *)
}

(** {1 Evaluation result} *)

type eval_result = {
  mode: string;  (** ["single"] or ["swarm"] *)
  task_name: string;
  completed: bool;
  output: string;
  token_count: int;
  wall_clock_ms: float;
  iterations: int;
}

(** {1 Comparison} *)

type comparison = {
  task: eval_task;
  single: eval_result;
  swarm: eval_result;
  recommendation: string;  (** ["single"], ["swarm"], or ["equivalent"] *)
}

(** {1 Running} *)

(** Run a single agent on the task.  The [run] closure provides the
    LLM mock — it receives the prompt and returns an api_response. *)
val run_single :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  task:eval_task ->
  run:(sw:Eio.Switch.t -> string -> (Types.api_response, Error.sdk_error) result) ->
  eval_result

(** Run a swarm configuration on the task.  The swarm config must
    include agent entries with closure-based run functions. *)
val run_swarm :
  sw:Eio.Switch.t ->
  env:< clock : _ Eio.Time.clock ; process_mgr : _ Eio.Process.mgr ; .. > ->
  task:eval_task ->
  config:Swarm_types.swarm_config ->
  eval_result

(** Compare single-agent and swarm results to produce a recommendation. *)
val compare : single:eval_result -> swarm:eval_result -> task:eval_task -> comparison

(** Serialize a comparison to JSON. *)
val to_json : comparison -> Yojson.Safe.t
