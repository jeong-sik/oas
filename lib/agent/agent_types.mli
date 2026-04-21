(** Core types for the Agent module.

    [Agent_types.t] exposes record fields for library-internal code.
    External consumers should use the abstract [Agent.t] and its
    accessor functions instead.

    @stability Internal
    @since 0.93.1 *)

(** {1 Configuration} *)

type periodic_callback = {
  interval_sec: float;
  callback: unit -> unit;
}

type options = {
  base_url: string;
  provider: Provider.config option;
  max_idle_turns: int;
  idle_final_warning_at: int option;
    (** Threshold for [Hooks.on_idle_escalated] to emit
        [Hooks.Idle_severity.Final_warning]. When [None], the runtime
        derives [max_idle_turns - 1] when [max_idle_turns > 1]. *)
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
  guardrails_async: Guardrails_async.t;
  tracer: Tracing.t;
  raw_trace: Raw_trace.t option;
  approval: Hooks.approval_callback option;
  tool_retry_policy: Tool_retry_policy.t option;
  context_reducer: Context_reducer.t option;
  context_injector: Hooks.context_injector option;
  mcp_clients: Mcp.managed list;
  event_bus: Event_bus.t option;
  skill_registry: Skill_registry.t option;
      (** Discovery/metadata path only.  Surfaced via {!Agent.card} for
          A2A negotiation.  Does not affect runtime prompt composition. *)
  elicitation: Hooks.elicitation_callback option;
  description: string option;
  periodic_callbacks: periodic_callback list;
  memory: Memory.t option;
  allowed_paths: string list;
  operator_policy: Guardrails.tool_filter option;
    (** Operator-level tool policy.  When [Some], overrides the agent-level
        [guardrails.tool_filter].  Injected at agent creation time.
        @since 0.94.0 *)
  policy_channel: Policy_channel.t option;
    (** Shared channel for lazy tool policy propagation to spawned agents.
        When [Some], the agent polls this channel at each turn boundary
        and applies any accumulated {!Tool_op.t} to its operator policy.
        Parent and children share the same channel reference.
        @since 0.100.0 *)
  tool_selector: Tool_selector.strategy option;
    (** Tool selection strategy for large tool catalogs (20+ tools).
        When [Some], narrows the visible tool set per turn based on the
        user's query, improving selection accuracy from ~42% to 83-100%.
        Applied after guardrails and operator policy filtering.
        @since 0.100.0 *)
  priority: Llm_provider.Request_priority.t option;
    (** Scheduling priority for LLM requests at the options level.
        When [Some], overrides [agent_config.priority] on the resume path.
        For the Builder path, use {!Builder.with_priority} instead.
        @since 0.102.0 *)
  slot_id: int option;
    (** Pin LLM requests to a specific llama-server slot for KV cache reuse.
        When [Some n], adds ["id_slot": n] to OpenAI-compat request body.
        @since 0.109.0 *)
  on_run_complete: (bool -> unit) option;
    (** Optional callback invoked when a run finishes.  Receives [true]
        on success, [false] on error.  Runs before lifecycle state is
        updated.  Intended for emitting eval metrics, flushing OTel
        spans, or other end-of-run side effects.
        @since 0.110.0 *)
  tool_result_relocation:
    (Tool_result_store.t * Content_replacement_state.t) option;
    (** Optional tool result relocation.  When provided,
        {!Agent_turn.make_tool_results} persists large results to disk
        and replaces them with previews.  The {!Content_replacement_state}
        freezes replacement decisions for prompt cache stability.
        @since 0.128.0 *)
  journal: Durable_event.journal option;
    (** Optional event-sourced journal for crash recovery and replay.
        When provided, lifecycle events are appended alongside
        [Event_bus] publishes, enabling offline replay via
        {!Durable_event.replay_summary}.
        @since 0.133.0 *)
  transport: Llm_provider.Llm_transport.t option;
    (** Optional non-HTTP transport override.  Required for CLI provider
        kinds ([Claude_code], [Codex_cli], [Gemini_cli]) which cannot be
        reached over HTTP.  When [Some t], {!Pipeline.stage_route}
        dispatches via {!Llm_provider.Complete.complete} with this
        transport; when [None], the HTTP path is used.
        @since 0.156.0 *)
  summarizer: (Types.message list -> string) option;
    (** Optional custom extractive summarizer used by
        {!Budget_strategy.reduce_for_budget} when the Emergency phase
        triggers [Summarize_old].  When [None], the built-in
        {!Budget_strategy.default_summarizer} is used.  Consumers can
        supply a domain-aware summarizer to strip or transform
        application-specific markers before they are re-injected as
        compacted history.
        @since 0.150.0 *)
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
val allowed_paths : t -> string list

(** {1 SDK version} *)

val sdk_version : string

(** {1 Construction} *)

val create :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?config:Types.agent_config ->
  ?tools:Tool.t list ->
  ?context:Context.t ->
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
