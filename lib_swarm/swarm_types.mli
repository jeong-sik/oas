(** Swarm types — core type definitions for multi-agent swarm execution.

    Part of the [agent_sdk_swarm] library (Layer 2).

    @since 0.42.0 *)

open Agent_sdk

(** {1 Agent Roles} *)

type agent_role =
  | Discover
  | Verify
  | Execute
  | Summarize
  | Custom_role of string
[@@deriving show]

(** {1 Orchestration} *)

type orchestration_mode =
  | Decentralized
  | Supervisor
  | Pipeline_mode
[@@deriving show]

type aggregate_strategy =
  | Best_score
  | Average_score
  | Majority_vote
  | Custom_agg of (float list -> float)

type metric_source =
  | Shell_command of string
  | Callback of (unit -> float)

type convergence_config = {
  metric: metric_source;
  target: float;
  max_iterations: int;
  patience: int;
  aggregate: aggregate_strategy;
}

(** {1 Swarm Configuration} *)

(** Closure-based agent entry. [run] captures the agent and clock
    so that the swarm runner only needs [sw] and [prompt]. *)
type agent_entry = {
  name: string;
  run: sw:Eio.Switch.t -> string -> (Types.api_response, Error.sdk_error) result;
  role: agent_role;
}

(** Wrap an {!Agent.t} into an {!agent_entry}.
    The clock is captured in the closure. *)
val make_entry :
  name:string ->
  role:agent_role ->
  clock:_ Eio.Time.clock ->
  Agent.t ->
  agent_entry

type resource_budget = {
  max_total_tokens: int option;
  max_total_time_sec: float option;
  max_total_api_calls: int option;
}

val no_budget : resource_budget

type swarm_config = {
  entries: agent_entry list;
  mode: orchestration_mode;
  convergence: convergence_config option;
  max_parallel: int;
  prompt: string;
  timeout_sec: float option;
  budget: resource_budget;
}

(** {1 Execution State} *)

type agent_status =
  | Idle
  | Working
  | Done_ok of { elapsed: float; text: string }
  | Done_error of { elapsed: float; error: string }
[@@deriving show]

type iteration_record = {
  iteration: int;
  metric_value: float option;
  agent_results: (string * agent_status) list;
  elapsed: float;
  timestamp: float;
}

type swarm_state = {
  config: swarm_config;
  mutable current_iteration: int;
  mutable best_metric: float option;
  mutable best_iteration: int;
  mutable patience_counter: int;
  mutable agent_statuses: (string * agent_status) list;
  mutable history: iteration_record list;
  mutable converged: bool;
}

val create_state : swarm_config -> swarm_state

(** {1 Callbacks} *)

type swarm_callbacks = {
  on_iteration_start: (int -> unit) option;
  on_iteration_end: (iteration_record -> unit) option;
  on_agent_start: (string -> unit) option;
  on_agent_done: (string -> agent_status -> unit) option;
  on_converged: (swarm_state -> unit) option;
  on_error: (string -> unit) option;
}

val no_callbacks : swarm_callbacks

(** {1 Result} *)

type swarm_result = {
  iterations: iteration_record list;
  final_metric: float option;
  converged: bool;
  total_elapsed: float;
  total_usage: Types.usage_stats;
}
