(** Core types for the Agent module.

    [Agent_types.t] exposes record fields for library-internal code.
    External consumers should use the abstract [Agent.t] and its
    accessor functions instead. *)

(** {1 Configuration} *)

type periodic_callback = {
  interval_sec: float;
  callback: unit -> unit;
}

type options = {
  base_url: string;
  provider: Provider.config option;
  cascade: Provider.cascade option;
  max_idle_turns: int;
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
  guardrails_async: Guardrails_async.t;
  tracer: Tracing.t;
  raw_trace: Raw_trace.t option;
  approval: Hooks.approval_callback option;
  context_reducer: Context_reducer.t option;
  context_injector: Hooks.context_injector option;
  mcp_clients: Mcp.managed list;
  event_bus: Event_bus.t option;
  skill_registry: Skill_registry.t option;
  elicitation: Hooks.elicitation_callback option;
  description: string option;
  periodic_callbacks: periodic_callback list;
  memory: Memory.t option;
}

(** {1 Lifecycle re-exports} *)

type lifecycle_status = Agent_lifecycle.lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed
[@@deriving show]

type lifecycle_snapshot = Agent_lifecycle.lifecycle_snapshot = {
  current_run_id: string option;
  agent_name: string;
  worker_id: string option;
  runtime_actor: string option;
  status: lifecycle_status;
  requested_provider: string option;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  last_error: string option;
  accepted_at: float option;
  ready_at: float option;
  first_progress_at: float option;
  started_at: float option;
  last_progress_at: float option;
  finished_at: float option;
}

(** {1 Agent state} *)

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

(** Mutable agent record — library-internal only.
    External code must use [Agent.t] (abstract) and its accessors.

    All mutable fields are protected by [mu].  Use [set_state],
    [update_state], [set_lifecycle], etc. rather than direct assignment
    to prevent lost-update races from parallel tool-execution fibers or
    periodic callbacks. *)
type t = {
  mu: Eio.Mutex.t;
  mutable state: Types.agent_state;
  mutable lifecycle: lifecycle_snapshot option;
  mutable last_tool_calls: tool_call_fingerprint list option;
  mutable consecutive_idle_turns: int;
  named_cascade: Api.named_cascade option;
  tools: Tool_set.t;
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  context: Context.t;
  options: options;
}

(** {1 Defaults} *)

val default_options : options

(** {1 Accessors} *)

val state : t -> Types.agent_state
val lifecycle : t -> lifecycle_snapshot option
val tools : t -> Tool_set.t
val context : t -> Context.t
val options : t -> options
val net : t -> [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
val set_state : t -> Types.agent_state -> unit
val update_state : t -> (Types.agent_state -> Types.agent_state) -> unit
val set_consecutive_idle_turns : t -> int -> unit
val description : t -> string option
val memory : t -> Memory.t option

(** {1 SDK version} *)

val sdk_version : string

(** {1 Construction} *)

val create :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?config:Types.agent_config ->
  ?tools:Tool.t list ->
  ?context:Context.t ->
  ?named_cascade:Api.named_cascade ->
  ?options:options ->
  unit -> t

val clone : ?copy_context:bool -> t -> t

(** {1 Agent card} *)

val card : t -> Agent_card.agent_card

(** {1 Lifecycle management} *)

val set_lifecycle :
  t ->
  ?current_run_id:string ->
  ?worker_id:string ->
  ?runtime_actor:string ->
  ?last_error:string ->
  ?accepted_at:float ->
  ?ready_at:float ->
  ?first_progress_at:float ->
  ?started_at:float ->
  ?last_progress_at:float ->
  ?finished_at:float ->
  Agent_lifecycle.lifecycle_status -> unit

(** {1 Trace / Checkpoint} *)

val last_raw_trace_run : t -> Raw_trace.run_ref option
val lifecycle_snapshot : t -> lifecycle_snapshot option
val close : t -> unit
