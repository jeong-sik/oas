(** Swarm types — core type definitions for multi-agent swarm execution.

    Part of the [agent_sdk_swarm] library (Layer 2).
    Depends on [agent_sdk] types through the public API.

    @since 0.42.0 *)

open Agent_sdk

(* ── Agent Roles ─────────────────────────────────────────────────── *)

type agent_role =
  | Discover
  | Verify
  | Execute
  | Summarize
  | Custom_role of string
[@@deriving show]

(* ── Orchestration ───────────────────────────────────────────────── *)

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
  | Argv_command of string list
  | Callback of (unit -> float)

type convergence_config = {
  metric: metric_source;
  target: float;
  max_iterations: int;
  patience: int;
  aggregate: aggregate_strategy;
}

(* ── Agent Telemetry ────────────────────────────────────────────── *)

(** Per-agent telemetry collected after each run.
    Exposed to swarm consumers (e.g. MASC) so they don't need to
    re-query Layer 1 internals. *)
type agent_telemetry = {
  trace_ref: Raw_trace.run_ref option;
  usage: Types.usage_stats option;
  turn_count: int;
}
[@@deriving show]

let empty_telemetry = { trace_ref = None; usage = None; turn_count = 0 }

(* ── Swarm Configuration ────────────────────────────────────────── *)

type agent_entry = {
  name: string;
  run: sw:Eio.Switch.t -> string -> (Types.api_response, Error.sdk_error) result;
  role: agent_role;
  get_telemetry: (unit -> agent_telemetry) option;
}


(** Wrap an [Agent.t] into an [agent_entry]. Clock is captured via closure.
    Automatically wires [get_telemetry] to extract [Agent.last_raw_trace_run]. *)
let make_entry ~name ~role ~(clock : _ Eio.Time.clock) (agent : Agent.t) =
  { name;
    run = (fun ~sw prompt -> Agent.run ~sw ~clock agent prompt);
    role;
    get_telemetry = Some (fun () ->
      let state = Agent.state agent in
      { trace_ref = Agent.last_raw_trace_run agent;
        usage = Some state.usage;
        turn_count = state.turn_count }) }

type resource_budget = {
  max_total_tokens: int option;
  max_total_time_sec: float option;
  max_total_api_calls: int option;
}

let no_budget = {
  max_total_tokens = None;
  max_total_time_sec = None;
  max_total_api_calls = None;
}

(** Swarm configuration.

    [collaboration] optionally attaches a {!Collaboration.t} to track
    shared state across the swarm.  When present, the runner updates
    participant states as agents start/complete.  Defaults to [None]
    for backward compatibility — existing call sites are unaffected. *)
type swarm_config = {
  entries: agent_entry list;
  mode: orchestration_mode;
  convergence: convergence_config option;
  max_parallel: int;
  prompt: string;
  timeout_sec: float option;
  budget: resource_budget;
  max_agent_retries: int;
  collaboration: Collaboration.t option;
  resource_check: (unit -> bool) option;
  max_concurrent_agents: int option;
  enable_streaming: bool;
}

(* ── Execution State ────────────────────────────────────────────── *)

type agent_status =
  | Idle
  | Working
  | Done_ok of { elapsed: float; text: string;
                 telemetry: agent_telemetry }
  | Done_error of { elapsed: float; error: string;
                    telemetry: agent_telemetry }
[@@deriving show]

type iteration_record = {
  iteration: int;
  metric_value: float option;
  agent_results: (string * agent_status) list;
  elapsed: float;
  timestamp: float;
  (* Layer 1 telemetry — aggregated from agent runs *)
  trace_refs: Raw_trace.run_ref list;
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
} [@@warning "-69"]

(* ── State Operations ───────────────────────────────────────────── *)

let create_state config =
  let statuses =
    List.map (fun (e : agent_entry) -> (e.name, Idle)) config.entries
  in
  { config;
    current_iteration = 0;
    best_metric = None;
    best_iteration = 0;
    patience_counter = 0;
    agent_statuses = statuses;
    history = [];
    converged = false;
  }

(* ── Callbacks ──────────────────────────────────────────────────── *)

type swarm_callbacks = {
  on_iteration_start: (int -> unit) option;
  on_iteration_end: (iteration_record -> unit) option;
  on_agent_start: (string -> unit) option;
  on_agent_done: (string -> agent_status -> unit) option;
  on_converged: (swarm_state -> unit) option;
  on_error: (string -> unit) option;
}

let no_callbacks = {
  on_iteration_start = None;
  on_iteration_end = None;
  on_agent_start = None;
  on_agent_done = None;
  on_converged = None;
  on_error = None;
}

(* ── Result ─────────────────────────────────────────────────────── *)

type swarm_result = {
  iterations: iteration_record list;
  final_metric: float option;
  converged: bool;
  total_elapsed: float;
  total_usage: Types.usage_stats;
}
