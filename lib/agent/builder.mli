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
val with_cache_extended_ttl : bool -> t -> t

(** Enable or disable yielding when the agent is about to call a tool.

    When [true], the agent yields before invoking a tool, triggering
    any [on_yield] hooks and requiring a corresponding [on_resume] to
    continue execution. Only affects [on_yield]/[on_resume] hook
    behavior; does not change model or tool semantics.

    @since 0.99.7 *)
val with_yield_on_tool : bool -> t -> t

(** Set a custom exit predicate evaluated before each turn.

    The predicate receives the current [turn_count].  When it returns
    [true], the agent loop exits with {!Error.ExitConditionMet}.
    Consumers can close over any external state they need (e.g.
    elapsed time, external budget signals).

    @since 0.115.0 *)
val with_exit_condition : (int -> bool) -> t -> t

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

(** Pin LLM requests to a specific llama-server slot for KV cache reuse.
    @since 0.109.0 *)
val with_slot_id : int -> t -> t

val with_tracer : Tracing.t -> t -> t
val with_raw_trace : Raw_trace.t -> t -> t
val with_approval : Hooks.approval_callback -> t -> t
val with_tool_retry_policy : Tool_retry_policy.t -> t -> t
val with_context_reducer : Context_reducer.t -> t -> t

(** Set context reduction thresholds.
    [compact_ratio] determines when to compact (default 0.8).
    [?context_window_tokens] overrides the reducer's context-window budget basis.
    This is used to estimate available input/context capacity for reduction
    decisions, and is distinct from [with_max_tokens], which controls the
    agent's per-response output token limit.
    When omitted, derives from [max_input_tokens], then [max_total_tokens],
    then falls back to 200_000.  Values <= 0 are ignored.
    [prepare_ratio] and [handoff_ratio] are stored for future use.

    @since 0.79.0
    @since 0.110.0 [?context_window_tokens] parameter *)
val with_context_thresholds :
  compact_ratio:float ->
  ?context_window_tokens:int ->
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
(* with_event_targets removed — was a no-op.  See oas#669. *)

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

(** Set tool selection strategy for large tool catalogs.
    When tool count > 15, selector narrows candidates per turn
    before sending schemas to the LLM.

    Can be combined with [with_progressive_tools]: progressive disclosure
    determines the available pool, then selector narrows further
    within that pool.

    @since 0.100.0 *)
val with_tool_selector : Tool_selector.strategy -> t -> t

(** Set a shared policy channel for lazy tool policy propagation.
    Children that share the same channel pick up parent policy changes
    at their next turn boundary via lock-free polling.
    @since 0.136.1 *)
val with_policy_channel : Policy_channel.t -> t -> t

(** {2 Run lifecycle} *)

(** Set a callback invoked when a run finishes.  Receives [true] on
    success, [false] on error.  Intended for emitting eval metrics,
    flushing OTel spans, or other end-of-run side effects.
    @since 0.110.0 *)
val with_on_run_complete : (bool -> unit) -> t -> t

(** Enable tool result relocation.  Large results are persisted to disk
    and replaced with previews.  Decisions are frozen in the
    {!Content_replacement_state} for prompt cache stability.
    @since 0.128.0 *)
val with_tool_result_relocation :
  store:Tool_result_store.t ->
  state:Content_replacement_state.t ->
  t -> t

(** Attach an event-sourced journal for crash recovery and replay.
    @since 0.133.0 *)
val with_journal : Durable_event.journal -> t -> t

(** Override the Budget_strategy Emergency-phase summarizer with a
    domain-aware function.  Routed into [Agent.options.summarizer] and
    forwarded to {!Budget_strategy.reduce_for_budget} when compaction
    triggers.  Leave unset to use the built-in
    {!Budget_strategy.default_summarizer}.
    @since 0.150.0 *)
val with_summarizer : (Types.message list -> string) -> t -> t

(** Install an [on_run_complete] callback that persists the journal
    to [path] whenever the agent run finishes (success or failure).
    Equivalent to attaching a journal and then calling
    {!Agent.save_journal} in a callback, but bundled so consumers
    declare the intent in one line.

    If a journal is not explicitly attached, this builder also
    creates a fresh one so the dump is non-empty.
    @since 0.135.0 *)
val with_auto_dump_journal : path:string -> t -> t

(** {2 Build} *)

(** Build the agent. May raise on invalid config.
    @deprecated Use {!build_safe} for validated construction. *)
val build : t -> Agent.t

(** Build with validation. Returns [Error] for invalid config
    (e.g. max_turns <= 0, thinking_budget without enable_thinking). *)
val build_safe : t -> (Agent.t, Error.sdk_error) result
