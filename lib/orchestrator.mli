(** Multi-agent orchestration.

    Distributes tasks across named agents and collects results.
    External code decides the execution plan (Sequential, Parallel,
    FanOut, Pipeline); the LLM does not participate in routing.

    @stability Evolving
    @since 0.93.0 *)

(** {2 Core types} *)

type task = {
  id: string;
  prompt: string;
  agent_name: string;
}

type task_result = {
  task_id: string;
  agent_name: string;
  result: (Types.api_response, Error.sdk_error) result;
  elapsed: float;
}

type plan =
  | Sequential of task list
  | Parallel of task list
  | FanOut of { prompt: string; agents: string list }
  | Pipeline of task list

type config = {
  max_parallel: int;
  shared_context: Context.t option;
  on_task_start: (task -> unit) option;
  on_task_complete: (task_result -> unit) option;
  timeout_per_task: float option;
  event_bus: Event_bus.t option;
}

val default_config : config

(** {2 Orchestrator} *)

type t = {
  agents: (string * Agent.t) list;
  config: config;
}

val create : ?config:config -> (string * Agent.t) list -> t
val add_agent : t -> string -> Agent.t -> t
val find_agent : t -> string -> Agent.t option

(** {2 Plan execution} *)

val run_task : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task -> task_result
val execute_sequential : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task list -> task_result list
val execute_parallel : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task list -> task_result list
val execute_fan_out : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> prompt:string -> agents:string list -> task_result list
val execute_pipeline : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task list -> task_result list
val execute : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> plan -> task_result list
val fan_out : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> string -> task_result list
val pipeline : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> task list -> task_result list

(** {2 Conditional orchestration} *)

type route_condition =
  | Always
  | ResultOk
  | TextContains of string
  | Custom_cond of (task_result -> bool)
  | And of route_condition list
  | Or of route_condition list
  | Not of route_condition

type conditional_plan =
  | Step of task
  | Branch of { condition: route_condition; if_true: conditional_plan; if_false: conditional_plan }
  | Sequence of conditional_plan list
  | Cond_parallel of conditional_plan list
  | Loop of { body: conditional_plan; until: route_condition; max_iterations: int }

val eval_condition : task_result option -> route_condition -> bool
val execute_conditional : sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock -> t -> conditional_plan -> task_result list

(** {2 Utilities} *)

val collect_text : task_result list -> string
val all_ok : task_result list -> bool

(** {2 Consensus Orchestration} *)

(** Strategy for selecting a winner from multiple agent results. *)
type selection_strategy =
  | FirstOk
  | BestBy of (task_result -> float)
  | MajorityText

val select_winner : selection_strategy -> task_result list -> task_result option

(** Run all [agents] with the same [prompt] in parallel, then select
    a winner using [strategy]. Returns [(all_results, winner)]. *)
val execute_consensus :
  sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock ->
  t -> prompt:string -> agents:string list ->
  strategy:selection_strategy ->
  task_result list * task_result option

(** {2 Hierarchical Orchestration} *)

(** Run sub-orchestrators as independent units. Each
    [(label, sub_orch, sub_plan)] is executed and its collected text
    becomes a single task_result attributed to [label]. *)
val execute_hierarchical :
  sw:Eio.Switch.t -> ?clock:_ Eio.Time.clock ->
  ?max_parallel:int ->
  (string * t * plan) list ->
  task_result list
