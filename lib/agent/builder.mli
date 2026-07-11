(** Builder pattern for Agent creation.

    Provides a flat, chainable API as an alternative to nested
    [Agent.create] params. Use [build_safe] for validated construction.

    @stability Stable
    @since 0.93.1 *)

type t

val create : net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t -> model:Types.model -> t

(** {2 Configuration} *)

val with_name : string -> t -> t
val with_system_prompt : string -> t -> t
val with_max_tokens : int -> t -> t

(* [with_max_turns 0] disables the turn-count limit; positive values enforce
    a finite limit. *)
val with_max_turns : int -> t -> t
val with_temperature : float -> t -> t
val with_top_p : float -> t -> t
val with_top_k : int -> t -> t
val with_min_p : float -> t -> t
val with_enable_thinking : bool -> t -> t
val with_preserve_thinking : bool -> t -> t
val with_thinking_budget : int -> t -> t
val with_tool_choice : Types.tool_choice -> t -> t
val with_response_format : Types.response_format -> t -> t
val with_disable_parallel_tool_use : bool -> t -> t
val with_initial_messages : Types.message list -> t -> t
val with_response_format_json : bool -> t -> t
val with_cache_system_prompt : bool -> t -> t
val with_cache_extended_ttl : bool -> t -> t

(** Configure whether OAS performs an internal compact-and-retry after a
    provider [ContextOverflow]. Defaults to [true] for standalone agents.
    Set [false] when a higher-level coordinator owns turn-level retry. *)
val with_auto_context_overflow_retry : bool -> t -> t

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

(** Require the run to end with user-facing final text.

    When [true], if a run is about to terminate with tool activity but no
    visible text (the "tool-only turn ended without a final reply" symptom) —
    either a terminal turn with no text, or [max_turns] reached after a tool
    turn — the agent performs exactly ONE additional model turn with tools
    withheld, so the model itself authors a textual answer. This adds no
    turn/token limit (the answer is LLM-authored, not synthesized) and runs at
    most once per run. Default [false]. *)
val with_ensure_final_text : bool -> t -> t

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
val with_trace_link : (string * string) option -> t -> t
val with_raw_trace : Raw_trace.t -> t -> t
val with_approval : Hooks.approval_callback -> t -> t

val with_missing_approval_callback_policy
  :  Hooks.missing_approval_callback_policy
  -> t
  -> t

val with_context_reducer : Context_reducer.t -> t -> t

(** Set context reduction thresholds.
    [compact_ratio] determines when to compact and must be greater than 0.0
    and less than 1.0.
    [?context_window_tokens] overrides the reducer's context-window budget basis.
    This is used to estimate available input/context capacity for reduction
    decisions, and is distinct from [with_max_tokens], which controls the
    agent's per-response output token limit.
    When omitted, derives from the provider's resolved context window,
    then falls back to 200_000.  Values <= 0 are ignored.
    [prepare_ratio] and [handoff_ratio] are stored for future use.

    @since 0.79.0
    @since 0.110.0 [?context_window_tokens] parameter *)
val with_context_thresholds
  :  compact_ratio:float
  -> ?context_window_tokens:int
  -> ?prepare_ratio:float
  -> ?handoff_ratio:float
  -> t
  -> t

val with_context : Context.t -> t -> t
val with_context_injector : Hooks.context_injector -> t -> t

(** Attach a caller-owned event bus, replacing the default per-agent bus. *)
val with_event_bus : Event_bus.t -> t -> t

(** Opt out of observability-as-default by clearing the event bus.

    {!create} installs a fresh per-agent {!Event_bus.t} so Turn / Tool /
    InferenceTelemetry events are emitted without the caller opting in. Use
    this when no events should be produced at all (e.g. a latency-sensitive
    agent with no observer). With no subscriber the default bus only costs a
    mutex acquire per event, so prefer leaving it on unless you have a reason.
    @since 0.207.19 *)
val without_event_bus : t -> t

val with_max_execution_time : float -> t -> t

(** Set the per-line idle deadline applied to streaming HTTP responses
    (Ollama NDJSON, Anthropic / Openai / Gemini / Glm SSE). Resets after
    each successful line, so this caps inter-chunk silence — not total
    stream duration. A stalled endpoint surfaces as
    [TimeoutError { phase = Stream_idle state; _ }], preserving whether
    the stream was waiting for answer/thinking/tool-call progress.
    @since 0.176.0 *)
val with_stream_idle_timeout : float -> t -> t

(** Set the total deadline applied to non-streaming HTTP completion body
    consumption. Requires a clock to be provided to the underlying request;
    without one the wrapper is skipped. A timeout surfaces as
    [TimeoutError { phase = Non_streaming_body; _ }] which the retry layer
    treats as retryable. Streaming requests ignore this knob and rely on
    [with_stream_idle_timeout] for inter-line liveness so active long
    streams are not killed by total duration. @since 0.181.0 *)
val with_body_timeout : float -> t -> t

(** Set the agent-level inactivity deadline for the entire run. The timer
    resets on execution activity — a streamed token (every [on_event],
    including reasoning/thinking deltas) or a completed turn — and fires
    only after this many seconds of genuine silence, surfacing as
    [Error.AgentExecutionIdleTimeout]. Unlike [with_max_execution_time]
    (a total wall-clock that also kills a healthy-but-slow run) this never
    cancels a stream that is still producing output, so it is the liveness
    guard while [with_max_execution_time] becomes a generous backstop.
    Complements [with_stream_idle_timeout] (per-line, single stream) by
    spanning the gaps between turns. Requires a clock on [run]/[run_stream];
    without one the watchdog is skipped. For non-streaming [run], activity
    is observed only at turn boundaries. @since 0.201.0 *)
val with_execution_idle_timeout : float -> t -> t

val with_max_idle_turns : int -> t -> t
val with_idle_final_warning_at : int -> t -> t
val with_elicitation : Hooks.elicitation_callback -> t -> t
val with_description : string -> t -> t
val with_allowed_paths : string list -> t -> t
val with_periodic_callback : Agent.periodic_callback -> t -> t
val with_periodic_callbacks : Agent.periodic_callback list -> t -> t

(** {2 Provider} *)

val with_provider : Provider.config -> t -> t
val with_provider_config : Llm_provider.Provider_config.t -> t -> t
val with_base_url : string -> t -> t

(** Inject an {!Llm_provider.Llm_transport.t} for non-HTTP providers.
    Required for CLI provider kinds ([Claude_code], [Codex],
    [Gemini], [Kimi]) which are reached via subprocess rather
    than HTTP.
    For HTTP kinds (Anthropic/Gemini/Glm/Ollama/OpenAI_compat) the
    transport is unused and can be left unset.

    The transport must outlive the agent's [run] call.
    @since 0.156.0 *)
val with_transport : Llm_provider.Llm_transport.t -> t -> t

(** Inject a request-scoped runtime MCP policy for CLI transports.
    This is orthogonal to inline [Tool.t] schemas: transports such as
    Claude Code and Codex CLI can expose MCP tools directly from the
    subprocess runtime.
    @since 0.164.0 *)
val with_runtime_mcp_policy : Llm_provider.Llm_transport.runtime_mcp_policy -> t -> t

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

(** Set tool schema disclosure level for this agent.

    Default behavior (no call) is [Tool.Full_schema], preserving the
    legacy contract that every surviving tool's [input_schema] is sent
    to the LLM on every turn.

    [Tool.Minimal_index] omits [input_schema] from every tool — tokens
    are minimized but the model must compose [function_call] arguments
    without seeing the parameter schema. Verify model compatibility
    before using.

    [Tool.Hybrid { full_names }] sends [Full_schema] for the named tools
    and [Minimal_index] for the rest. Pairs naturally with
    {!with_tool_selector}: pre-selected top-K can be promoted to full
    while the remainder stays as an index.

    @since 0.194.0 *)
val with_disclosure_level : Tool.disclosure_level -> t -> t

(** Install a per-turn resolver that adapts disclosure based on the
    most recent tool results.

    The resolver receives the latest [Types.tool_result] list extracted
    from message history (left-to-right; empty list if no tool results
    seen yet). It returns:
    - [Some override]: use [override] for this turn only.
    - [None]: fall through to the static [with_disclosure_level] value
      (or [Tool.Full_schema] if none was set).

    Typical pattern — "demote to Full_schema when the previous turn's
    tool call failed validation":

    {[
      let resolver (results : Types.tool_result list) =
        let has_error = List.exists Result.is_error results in
        if has_error then Some Tool.Full_schema else None
      in
      Builder.with_disclosure_resolver resolver builder
    ]}

    Caller owns the policy (TTL, sticky promotion, session scope, etc.);
    OAS only provides the mechanism. Combine freely with
    {!with_disclosure_level} and {!with_tool_selector}.

    @since 0.195.0 *)
val with_disclosure_resolver
  :  (Types.tool_result list -> Tool.disclosure_level option)
  -> t
  -> t

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
val with_tool_result_relocation
  :  store:Tool_result_store.t
  -> state:Content_replacement_state.t
  -> t
  -> t

(** Attach an event-sourced journal for crash recovery and replay.
    @since 0.133.0 *)
val with_journal : Durable_event.journal -> t -> t

(** Attach a caller-owned turn-boundary checkpoint sink. The sink is stored
    on the built {!Agent.t}, not in {!Agent.options}, so existing callers that
    construct options records remain source-compatible.
    @since 0.193.9 *)
val with_checkpoint_sink : Agent.checkpoint_sink -> t -> t

(** Install the LLM judge used for adjacent typed failed-tool recovery.
    [with_yield_on_tool true] is required so the main provider lease is not held
    while the judge completion is waiting. *)
val with_tool_failure_judge : Tool_failure_recovery.judge -> t -> t

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

(** Build with validation. Returns [Error] for invalid config
    (e.g. max_turns < 0, thinking_budget without enable_thinking). *)
val build_safe : t -> (Agent.t, Error.sdk_error) result
