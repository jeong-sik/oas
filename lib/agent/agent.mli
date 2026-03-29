(** Agent: structured concurrency agent with hooks, tools, and handoffs.

    {b type t is abstract.}  Use accessor functions below to inspect
    agent state.  Direct record field access is restricted to
    library-internal code via [Agent_types.t].

    @stability Stable
    @since 0.93.1 *)

(** {1 Types (re-exported from Agent_types)} *)

type periodic_callback = Agent_types.periodic_callback = {
  interval_sec: float;
  callback: unit -> unit;
}

type options = Agent_types.options = {
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
  allowed_paths: string list;
  operator_policy: Guardrails.tool_filter option;
}

type lifecycle_status = Agent_lifecycle.lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed

type lifecycle_snapshot = Agent_lifecycle.lifecycle_snapshot

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

(** Abstract agent type. *)
type t

(** {1 Accessors} *)

val state : t -> Types.agent_state
val lifecycle : t -> lifecycle_snapshot option
val tools : t -> Tool_set.t
val context : t -> Context.t
val options : t -> options
val net : t -> [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
val description : t -> string option
val memory : t -> Memory.t option
val allowed_paths : t -> string list
val named_cascade : t -> Api.named_cascade option

(** {1 Defaults} *)

val default_options : options
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

(** {1 Agent Card} *)

val card : t -> Agent_card.agent_card

(** {1 Execution} *)

type api_strategy =
  | Sync
  | Stream of { on_event: Types.sse_event -> unit }

val run :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  t -> string ->
  (Types.api_response, Error.sdk_error) result

val run_stream :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  on_event:(Types.sse_event -> unit) ->
  t -> string ->
  (Types.api_response, Error.sdk_error) result

val run_turn_stream :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  on_event:(Types.sse_event -> unit) ->
  t ->
  ([ `Complete of Types.api_response | `ToolsExecuted ],
   Error.sdk_error) result

(** {1 Handoff} *)

val run_with_handoffs :
  sw:Eio.Switch.t ->
  ?clock:_ Eio.Time.clock ->
  t ->
  targets:Handoff.handoff_target list ->
  string ->
  (Types.api_response, Error.sdk_error) result

(** {1 Checkpoint / Resume} *)

val resume :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  checkpoint:Checkpoint.t ->
  ?tools:Tool.t list ->
  ?context:Context.t ->
  ?named_cascade:Api.named_cascade ->
  ?options:options ->
  ?config:Types.agent_config ->
  unit -> t

val checkpoint :
  ?session_id:string ->
  ?working_context:Yojson.Safe.t ->
  t ->
  Checkpoint.t

(** {1 Lifecycle} *)

val last_raw_trace_run : t -> Raw_trace.run_ref option
val close : t -> unit
val lifecycle_snapshot : t -> lifecycle_snapshot option

(** {1 Internal (testing only -- do not use in production code)} *)

val set_state : t -> Types.agent_state -> unit
val update_state : t -> (Types.agent_state -> Types.agent_state) -> unit
val set_consecutive_idle_turns : t -> int -> unit
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
val base_messages : t -> Types.message list
val check_loop_guard : t -> Error.sdk_error option
