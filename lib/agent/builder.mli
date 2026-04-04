(** Builder pattern for Agent creation.

    Provides a flat, chainable API as an alternative to nested
    [Agent.create] params. Use [build_safe] for validated construction.

    @stability Stable
    @since 0.93.1 *)

type t

val create :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  model:Types.model -> t

(** {2 Configuration} *)

val with_name : string -> t -> t
val with_system_prompt : string -> t -> t
val with_max_tokens : int -> t -> t
val with_max_turns : int -> t -> t
val with_temperature : float -> t -> t
val with_top_p : float -> t -> t
val with_top_k : int -> t -> t
val with_min_p : float -> t -> t
val with_enable_thinking : bool -> t -> t
val with_thinking_budget : int -> t -> t
val with_tool_choice : Types.tool_choice -> t -> t
val with_disable_parallel_tool_use : bool -> t -> t
val with_max_input_tokens : int -> t -> t
val with_max_total_tokens : int -> t -> t
val with_initial_messages : Types.message list -> t -> t
val with_max_cost_usd : float -> t -> t
val with_response_format_json : bool -> t -> t
val with_cache_system_prompt : bool -> t -> t

(** Enable or disable yielding when the agent is about to call a tool.

    When [true], the agent yields before invoking a tool, triggering
    any [on_yield] hooks and requiring a corresponding [on_resume] to
    continue execution. Only affects [on_yield]/[on_resume] hook
    behavior; does not change model or tool semantics.

    @since 0.99.7 *)
val with_yield_on_tool : bool -> t -> t

(** {2 Tools and MCP} *)

val with_tools : Tool.t list -> t -> t
val with_tool : Tool.t -> t -> t
val with_mcp_clients : Mcp.managed list -> t -> t

(** {2 Runtime options} *)

val with_hooks : Hooks.hooks -> t -> t
val with_guardrails : Guardrails.t -> t -> t
val with_guardrails_async : Guardrails_async.t -> t -> t

(** Set operator-level tool policy.
    Takes precedence over agent-level [guardrails.tool_filter].
    Logged for auditability when applied.

    Priority: turn_params.tool_filter_override > operator_policy > agent guardrails

    @since 0.94.0 *)
val with_operator_policy : Guardrails.tool_filter -> t -> t

(** Set scheduling priority for LLM requests made by this agent.
    @since 0.96.0 *)
val with_priority : Llm_provider.Request_priority.t -> t -> t

val with_tracer : Tracing.t -> t -> t
val with_raw_trace : Raw_trace.t -> t -> t
val with_approval : Hooks.approval_callback -> t -> t
val with_tool_retry_policy : Tool_retry_policy.t -> t -> t
val with_context_reducer : Context_reducer.t -> t -> t

(** Set context reduction thresholds.
    [compact_ratio] determines when to compact (default 0.8).
    [prepare_ratio] and [handoff_ratio] are stored for future use.

    @since 0.79.0 *)
val with_context_thresholds :
  compact_ratio:float ->
  ?prepare_ratio:float ->
  ?handoff_ratio:float ->
  t -> t
val with_context : Context.t -> t -> t
val with_context_injector : Hooks.context_injector -> t -> t
val with_event_bus : Event_bus.t -> t -> t
val with_max_idle_turns : int -> t -> t
val with_elicitation : Hooks.elicitation_callback -> t -> t
val with_description : string -> t -> t
val with_memory : Memory.t -> t -> t
val with_allowed_paths : string list -> t -> t
val with_periodic_callback : Agent.periodic_callback -> t -> t
val with_periodic_callbacks : Agent.periodic_callback list -> t -> t

(** {2 Provider} *)

val with_provider : Provider.config -> t -> t
val with_provider_config : Llm_provider.Provider_config.t -> t -> t
val with_base_url : string -> t -> t
val with_cascade : Provider.cascade -> t -> t
val with_named_cascade : Api.named_cascade -> t -> t
val with_fallback : Provider.config -> t -> t

(** {2 Contract} *)

val with_contract : Contract.t -> t -> t

(** {3 Runtime skill composition}

    Skills added via [with_skill] / [with_skills] are composed into the
    agent's system prompt at build time.  Each skill body is rendered as
    a [\[Skill: <name>\]] section appended to the base system prompt.

    This is the {b runtime path}: it affects what the LLM sees on every
    turn.  For metadata-only registration (Agent Card export, A2A
    discovery, skill inventory) use {!with_skill_registry} instead. *)

val with_skill : Skill.t -> t -> t
val with_skills : Skill.t list -> t -> t

val with_tool_grants : string list -> t -> t
val with_mcp_tool_allowlist : string list -> t -> t

(** {2 Logging} *)

val with_log_level : Log.level -> t -> t
val with_log_sink : Log.sink -> t -> t
val with_event_targets : Event_forward.target list -> t -> t

(** {3 Discovery / metadata skill registry}

    Attach a {!Skill_registry.t} for discovery and metadata export only.
    Skills in the registry are surfaced via {!Agent.card} (Agent Card)
    for A2A negotiation, capability queries, and skill inventory.

    {b Does NOT affect runtime prompt composition.}  The registry
    contents are never injected into the agent's system prompt.  To make
    a skill influence LLM behavior, use {!with_skill} / {!with_skills}.

    An agent can use both paths simultaneously: registry skills for
    external discovery, and contract skills for prompt composition. *)
val with_skill_registry : Skill_registry.t -> t -> t

(** Set progressive tool disclosure strategy.
    Tools are revealed in phases across turns (Gather -> Act -> Verify).
    Installs a BeforeTurn hook that overrides tool_filter per turn.
    @since 0.81.0 *)
val with_progressive_tools : Progressive_tools.disclosure_strategy -> t -> t

(** {2 Build} *)

(** Build the agent. May raise on invalid config.
    @deprecated Use {!build_safe} for validated construction. *)
val build : t -> Agent.t

(** Build with validation. Returns [Error] for invalid config
    (e.g. max_turns <= 0, thinking_budget without enable_thinking). *)
val build_safe : t -> (Agent.t, Error.sdk_error) result
