(** Core types for the Agent module.

    [Agent_types.t] exposes record fields for library-internal code.
    External consumers should use the abstract [Agent.t] and its
    accessor functions instead.

    @stability Internal
    @since 0.93.1 *)

(** {1 Configuration} *)

type periodic_callback =
  { interval_sec : float
  ; callback : unit -> unit
  }

type checkpoint_stage =
  | After_assistant_collected
  | After_tool_results_appended
  | After_retry_feedback_appended

val checkpoint_stage_to_string : checkpoint_stage -> string

type checkpoint_snapshot =
  { stage : checkpoint_stage
  ; turn : int
  ; checkpoint : Checkpoint.t
  ; timestamp : float
  }

type checkpoint_sink = checkpoint_snapshot -> (unit, string) result

type recovery_state =
  { last_completed_round : Tool_failure_episode.completed_round option
  ; pending_episodes : Tool_failure_episode.t list option
  ; pending_receipt : Tool_failure_recovery.receipt option
  ; restore_error : Error.sdk_error option
  }

val empty_recovery_state : recovery_state

type options =
  { base_url : string
  ; provider : Provider.config option
  ; max_execution_time_s : float option
    (** Maximum allowed execution time for the entire agent run (including all turns and tool calls).
        If exceeded, the run terminates safely and returns a Timeout error. *)
  ; stream_idle_timeout_s : float option
    (** Inter-line idle deadline applied to streaming HTTP responses.
        Threaded through {!Pipeline.stage_route} into
        {!Llm_provider.Complete.complete_stream}, which forwards it to
        {!Llm_provider.Http_client.read_ndjson} (Ollama native NDJSON)
        and {!Llm_provider.Http_client.read_sse} (Anthropic / Openai-
        compatible / Gemini / Glm). The deadline resets after each
        successful line, so this caps inter-chunk silence — not total
        stream duration. A stalled endpoint surfaces as
        [TimeoutError { phase = Stream_idle state; _ }], preserving
        whether the stream was waiting for answer/thinking/tool-call
        progress. CLI transports honour the parallel
        [stdout_idle_timeout_s] knob via the transport's own config.
        @since 0.176.0 *)
  ; body_timeout_s : float option
    (** Total deadline applied to non-streaming HTTP completion body
        consumption. Threaded through {!Pipeline.stage_route} into
        {!Llm_provider.Complete.complete} /
        {!Llm_provider.Complete.complete_with_retry}, where it wraps the
        synchronous HTTP body read in [Eio.Time.with_timeout_exn].
        Requires [clock] to be supplied; without a clock the wrapper is
        skipped. A timeout surfaces as
        [TimeoutError { phase = Non_streaming_body; _ }] which the retry
        layer treats as retryable. Streaming requests deliberately ignore
        this field and use [stream_idle_timeout_s] for inter-line liveness
        so active long streams are not killed by a total body deadline.
        @since 0.181.0 *)
  ; execution_idle_timeout_s : float option
    (** Agent-level inactivity deadline for the entire run. The timer
        resets on execution activity — a streamed token (every
        [on_event], including reasoning/thinking deltas) or a completed
        turn — and fires only after [execution_idle_timeout_s] of genuine
        silence, surfacing as [Error.AgentExecutionIdleTimeout]. Unlike
        [max_execution_time_s] (a total wall-clock that also kills a
        healthy-but-slow run) this never cancels a stream that is still
        producing output, so it is the liveness guard while
        [max_execution_time_s] becomes a generous backstop.

        Complements [stream_idle_timeout_s], which caps per-line silence
        at the HTTP layer for a single stream; this knob spans the whole
        run, including the gaps between turns (tool execution, turn
        transitions) where no stream is open.

        Requires [clock] to be supplied to [run]/[run_stream]; without a
        clock the watchdog is skipped and behaviour matches earlier
        versions. A non-positive value disables the watchdog (treated like
        [None]). For non-streaming [run], activity is observed only at turn
        boundaries (no token signal), so set this above the longest
        expected single-turn latency or prefer [run_stream].
        @since 0.201.0 *)
  ; max_idle_turns : int
  ; idle_final_warning_at : int option
    (** Threshold for [Hooks.on_idle_escalated] to emit
        [Hooks.Idle_severity.Final_warning]. When [None], the runtime
        derives [max_idle_turns - 1] when [max_idle_turns > 1]. *)
  ; hooks : Hooks.hooks
  ; guardrails : Guardrails.t
  ; guardrails_async : Guardrails_async.t
  ; tracer : Tracing.t
  ; trace_link : (string * string) option
  ; raw_trace : Raw_trace.t option
  ; approval : Hooks.approval_callback option
  ; missing_approval_callback_policy : Hooks.missing_approval_callback_policy
  ; context_reducer : Context_reducer.t option
  ; context_injector : Hooks.context_injector option
  ; mcp_clients : Mcp.managed list
  ; event_bus : Event_bus.t option
  ; skill_registry : Skill_registry.t option
    (** Discovery/metadata path only.  Surfaced via {!Agent.card} for
          A2A negotiation.  Does not affect runtime prompt composition. *)
  ; elicitation : Hooks.elicitation_callback option
  ; description : string option
  ; periodic_callbacks : periodic_callback list
  ; allowed_paths : string list
  ; operator_policy : Guardrails.tool_filter option
    (** Operator-level tool policy.  When [Some], overrides the agent-level
        [guardrails.tool_filter].  Injected at agent creation time.
        @since 0.94.0 *)
  ; policy_channel : Policy_channel.t option
    (** Shared channel for lazy tool policy propagation to spawned agents.
        When [Some], the agent polls this channel at each turn boundary
        and applies any accumulated {!Tool_op.t} to its operator policy.
        Parent and children share the same channel reference.
        @since 0.100.0 *)
  ; tool_selector : Tool_selector.strategy option
    (** Tool selection strategy for large tool catalogs (20+ tools).
        When [Some], narrows the visible tool set per turn based on the
        user's query, improving selection accuracy from ~42% to 83-100%.
        Applied after guardrails and operator policy filtering.
        @since 0.100.0 *)
  ; disclosure_level : Tool.disclosure_level option
    (** Controls how each surviving tool's schema is serialized.
        [None] preserves legacy behavior ([Full_schema]). See
        {!Tool.disclosure_level} for risk notes — [Minimal_index] omits
        [input_schema] and may break models that need it to compose
        arguments.
        @since 0.194.0 *)
  ; disclosure_resolver : (Types.tool_result list -> Tool.disclosure_level option) option
    (** Optional resolver that decides the disclosure level for the
        next turn based on the most recent tool results. When
        [Some f], it is called with the latest [Types.tool_result] list
        extracted from message history. [Some override] uses [override]
        this turn; [None] falls back to the static [disclosure_level].
        Caller owns the policy (TTL, sticky promotion, session scope);
        OAS provides the mechanism only.
        @since 0.195.0 *)
  ; priority : Llm_provider.Request_priority.t option
    (** Scheduling priority for LLM requests at the options level.
        When [Some], overrides [agent_config.priority] on the resume path.
        For the Builder path, use {!Builder.with_priority} instead.
        @since 0.102.0 *)
  ; slot_id : int option
    (** Pin LLM requests to a specific llama-server slot for KV cache reuse.
        When [Some n], adds ["id_slot": n] to OpenAI-compat request body.
        @since 0.109.0 *)
  ; on_run_complete : (bool -> unit) option
    (** Optional callback invoked when a run finishes.  Receives [true]
        on success, [false] on error.  Runs before lifecycle state is
        updated.  Intended for emitting eval metrics, flushing OTel
        spans, or other end-of-run side effects.
        @since 0.110.0 *)
  ; tool_result_relocation : (Tool_result_store.t * Content_replacement_state.t) option
    (** Optional tool result relocation.  When provided,
        {!Agent_turn.make_tool_results} persists large results to disk
        and replaces them with previews.  The {!Content_replacement_state}
        freezes replacement decisions for prompt cache stability.
        @since 0.128.0 *)
  ; journal : Durable_event.journal option
    (** Optional event-sourced journal for crash recovery and replay.
        When provided, lifecycle events are appended alongside
        [Event_bus] publishes, enabling offline replay via
        {!Durable_event.replay_summary}.
        @since 0.133.0 *)
  ; transport : Llm_provider.Llm_transport.t option
    (** Optional non-HTTP transport override.  Required for CLI provider
        kinds ([Claude_code], [Codex], [Gemini], [Kimi])
        which cannot be reached over HTTP.  When [Some t], {!Pipeline.stage_route}
        dispatches via {!Llm_provider.Complete.complete} with this
        transport; when [None], the HTTP path is used.
        @since 0.156.0 *)
  ; runtime_mcp_policy : Llm_provider.Llm_transport.runtime_mcp_policy option
    (** Optional request-scoped MCP exposure policy for CLI transports.
        When [Some], the transport may expose runtime MCP tools without
        relying on inline [Tool.t] schemas.
        @since 0.164.0 *)
  ; summarizer : (Types.message list -> string) option
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

type lifecycle_snapshot = Agent_lifecycle.lifecycle_snapshot =
  { current_run_id : string option
  ; agent_name : string
  ; worker_id : string option
  ; runtime_actor : string option
  ; status : lifecycle_status
  ; requested_provider : string option
  ; requested_model : string option
  ; resolved_provider : string option
  ; resolved_model : string option
  ; last_error : string option
  ; accepted_at : float option
  ; ready_at : float option
  ; first_progress_at : float option
  ; started_at : float option
  ; last_progress_at : float option
  ; finished_at : float option
  }

(** {1 Agent state} *)

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

(** Idle detection state snapshot. *)
type idle_state = Agent_turn.idle_state =
  { last_tool_calls : tool_call_fingerprint list option
  ; consecutive_idle_turns : int
  }

(** Mutable agent record — library-internal only.
    External code must use [Agent.t] (abstract) and its accessors.

    All mutable fields are protected by [mu].  Use [set_state],
    [update_state], [set_lifecycle], etc. rather than direct assignment
    to prevent lost-update races from parallel tool-execution fibers or
    periodic callbacks. *)
type t =
  { mu : Eio.Mutex.t
  ; mutable state : Types.agent_state
  ; mutable lifecycle : lifecycle_snapshot option
  ; mutable last_tool_calls : tool_call_fingerprint list option
  ; mutable consecutive_idle_turns : int
  ; auto_context_overflow_retry : bool
  ; tools : Tool_set.t
  ; net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  ; context : Context.t
  ; options : options
  ; checkpoint_sink : checkpoint_sink option
  ; tool_failure_judge : Tool_failure_recovery.judge option
  ; mutable recovery_state : recovery_state
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
val recovery_state : t -> recovery_state
val set_recovery_state : t -> recovery_state -> unit
val update_recovery_state : t -> (recovery_state -> recovery_state) -> unit
val recovery_run_boundary_metadata : t -> Types.metadata
val set_consecutive_idle_turns : t -> int -> unit
val get_consecutive_idle_turns : t -> int

(** Mutex-protected atomic update of both idle-detection fields.
    Callers should use this instead of assigning [last_tool_calls] and
    [consecutive_idle_turns] directly, so the two fields stay consistent
    under concurrent tool-execution fibers and periodic callbacks. *)
val set_idle_state : t -> idle_state -> unit

(** Reset idle-detection counters to their initial (zero) state.
    Equivalent to [set_idle_state t { last_tool_calls = None; consecutive_idle_turns = 0 }]. *)
val reset_idle_state : t -> unit

val description : t -> string option
val allowed_paths : t -> string list

(** {1 SDK version} *)

val sdk_version : string

(** {1 Construction} *)

val create
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?config:Types.agent_config
  -> ?tools:Tool.t list
  -> ?context:Context.t
  -> ?options:options
  -> ?auto_context_overflow_retry:bool
  -> ?checkpoint_sink:checkpoint_sink
  -> ?tool_failure_judge:Tool_failure_recovery.judge
  -> unit
  -> t

val clone : ?copy_context:bool -> t -> t

(** {1 Agent card} *)

val card : t -> Agent_card.agent_card

(** {1 Lifecycle management} *)

val set_lifecycle
  :  t
  -> ?current_run_id:string
  -> ?worker_id:string
  -> ?runtime_actor:string
  -> ?last_error:string
  -> ?accepted_at:float
  -> ?ready_at:float
  -> ?first_progress_at:float
  -> ?started_at:float
  -> ?last_progress_at:float
  -> ?finished_at:float
  -> Agent_lifecycle.lifecycle_status
  -> unit

(** {1 Trace / Checkpoint} *)

val last_raw_trace_run : t -> Raw_trace.run_ref option
val lifecycle_snapshot : t -> lifecycle_snapshot option
val close : t -> unit
