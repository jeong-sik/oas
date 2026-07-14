(** Agent: structured concurrency agent with hooks, tools, and handoffs.

    {b type t is abstract.}  Use accessor functions below to inspect
    agent state.  Direct record field access is restricted to
    library-internal code via [Agent_types.t].

    @stability Stable
    @since 0.93.1 *)

(** {1 Types (re-exported from Agent_types)} *)

type periodic_callback = Agent_types.periodic_callback =
  { interval_sec : float
  ; callback : unit -> unit
  }

type checkpoint_stage = Agent_types.checkpoint_stage =
  | After_assistant_collected
  | After_tool_results_appended
  | After_context_injection

val checkpoint_stage_to_string : checkpoint_stage -> string

type checkpoint_snapshot = Agent_types.checkpoint_snapshot =
  { stage : checkpoint_stage
  ; turn : int
  ; checkpoint : Checkpoint.t
  ; timestamp : float
  }

type checkpoint_sink = Agent_types.checkpoint_sink

type options = Agent_types.options =
  { base_url : string
  ; provider : Provider.config option
  ; stream_idle_timeout_s : float option
  ; body_timeout_s : float option
  ; hooks : Hooks.hooks
  ; guardrails_async : Guardrails_async.t
  ; tracer : Tracing.t
  ; trace_link : (string * string) option
  ; raw_trace : Raw_trace.t option
  ; context_injector : Hooks.context_injector option
  ; mcp_clients : Mcp.managed list
  ; event_bus : Event_bus.t option
  ; skill_registry : Skill_registry.t option
    (** Discovery/metadata path only.  Surfaced via {!Agent.card} for
          A2A negotiation.  Does not affect runtime prompt composition. *)
  ; elicitation : Hooks.elicitation_callback option
  ; description : string option
  ; periodic_callbacks : periodic_callback list
  ; slot_id : int option
  ; on_run_complete : (bool -> unit) option
  ; journal : Durable_event.journal option
  ; transport : Llm_provider.Llm_transport.t option
  }

type lifecycle_status = Agent_lifecycle.lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed

type lifecycle_snapshot = Agent_lifecycle.lifecycle_snapshot

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

(** {1 Defaults} *)

val default_options : options
val sdk_version : string

(** {1 Construction} *)

(** [checkpoint_sink] attaches an optional caller-owned mutation-boundary
    checkpoint sink. The pipeline updates live state first, then emits a typed
    snapshot after assistant collection, after base tool-result append, and
    after successful context injection. A sink failure is returned before the
    pipeline advances to the next mutation stage. The sink is passed here
    rather than through {!options} so callers that construct options records
    remain source-compatible. *)
val create
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> config:Types.agent_config
  -> ?tools:Tool.t list
  -> ?context:Context.t
  -> ?options:options
  -> ?checkpoint_sink:checkpoint_sink
  -> unit
  -> t

val clone : ?copy_context:bool -> t -> t

(** {1 Agent Card} *)

val card : t -> Agent_card.agent_card

(** {1 Execution} *)

type api_strategy =
  | Sync
  | Stream of
      { on_event : Types.sse_event -> unit
      ; on_telemetry : (Llm_provider.Telemetry_event.t -> unit) option
      }

(** Additive provider-failure carrier.  Existing execution entry points are
    exact projections of this record's [error] field. *)
type detailed_error = Provider_failure_attribution.detailed_error =
  { error : Error.sdk_error
  ; provider_failure : Provider_failure_attribution.t option
  }

(** Detailed counterpart of {!run}. *)
val run_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> string
  -> (Types.api_response, detailed_error) result

(** Run the agent until a provider returns a terminal response or a typed error.
    The tool loop is unbounded: OAS does not stop it because of turn count,
    idle-turn count, tool-round count, accumulated cost, or token usage. Those
    values are observations only.

    The caller owns the lifetime of the run through Eio structured
    concurrency. Use a caller-owned child switch or cancellation scope to stop
    one run without cancelling unrelated lanes, and wrap that scope in an
    objective deadline when a whole-run timeout is required. Supplying [clock]
    does not create such a deadline; it supplies time to periodic callbacks and
    to explicitly configured provider body or stream-idle timeouts.
    Caller-initiated [Eio.Cancel.Cancelled] propagates and is not converted into
    an agent result.

    [on_yield] is called when the agent enters tool execution and [on_resume]
    before the next LLM turn, allowing callers to release/re-acquire provider
    capacity. They are invoked only when
    [agent_config.yield_on_tool = true].
    @since 0.100.0 *)
val run
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> string
  -> (Types.api_response, Error.sdk_error) result

(** Run agent to completion with a user-authored content block list.
    This is the multimodal entrypoint for callers that need to pass text
    together with images, documents, or audio. Text blocks are UTF-8 sanitized;
    non-text media payloads are preserved. *)
val run_blocks
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> Types.content_block list
  -> (Types.api_response, Error.sdk_error) result

(** Detailed counterpart of {!run_blocks}. *)
val run_blocks_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> Types.content_block list
  -> (Types.api_response, detailed_error) result

(** Stream a full agent run. Non-fatal exceptions raised by [on_event] are
    logged and do not abort the run. *)
val run_stream
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> string
  -> (Types.api_response, Error.sdk_error) result

(** Detailed counterpart of {!run_stream}. *)
val run_stream_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> string
  -> (Types.api_response, detailed_error) result

(** Stream a full agent run with a user-authored content block list.
    See {!run_blocks}. *)
val run_stream_blocks
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> Types.content_block list
  -> (Types.api_response, Error.sdk_error) result

(** Detailed counterpart of {!run_stream_blocks}. *)
val run_stream_blocks_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_yield:(unit -> unit)
  -> ?on_resume:(unit -> unit)
  -> t
  -> Types.content_block list
  -> (Types.api_response, detailed_error) result

(** Stream one agent turn. Non-fatal exceptions raised by [on_event] are
    logged and do not abort the turn. *)
val run_turn_stream
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_telemetry:(Llm_provider.Telemetry_event.t -> unit)
  -> t
  -> ([ `Complete of Types.api_response | `ToolsExecuted ], Error.sdk_error) result

(** Detailed counterpart of {!run_turn_stream}. *)
val run_turn_stream_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> on_event:(Types.sse_event -> unit)
  -> ?on_telemetry:(Llm_provider.Telemetry_event.t -> unit)
  -> t
  -> ([ `Complete of Types.api_response | `ToolsExecuted ], detailed_error) result

(** Append an elicitation response to the agent conversation so callers that
    received {!Error.InputRequired} can resume with {!run_turn_stream} or an
    equivalent turn driver. [Declined] and [Timeout] preserve the legacy
    callback behavior and do not append a synthetic user message. *)
val provide_input : t -> Error.input_required -> Hooks.elicitation_response -> unit

(** {1 Handoff} *)

val run_with_handoffs
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> t
  -> targets:Handoff.handoff_target list
  -> string
  -> (Types.api_response, Error.sdk_error) result

(** Detailed counterpart of {!run_with_handoffs}. *)
val run_with_handoffs_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> t
  -> targets:Handoff.handoff_target list
  -> string
  -> (Types.api_response, detailed_error) result

val run_with_handoffs_blocks
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> t
  -> targets:Handoff.handoff_target list
  -> Types.content_block list
  -> (Types.api_response, Error.sdk_error) result

(** Detailed counterpart of {!run_with_handoffs_blocks}. *)
val run_with_handoffs_blocks_detailed
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> t
  -> targets:Handoff.handoff_target list
  -> Types.content_block list
  -> (Types.api_response, detailed_error) result

(** {1 Checkpoint / Resume} *)

(** [resume ... ?checkpoint_sink ()] restores messages, usage, turn count, and
    the default context from a checkpoint. If [config] is supplied, it is the
    complete caller-owned runtime configuration. Otherwise, configuration fields
    represented by the checkpoint are restored over current defaults, and
    non-persisted runtime fields use those defaults. The optional sink is the same caller-owned
    turn-boundary checkpoint sink used by {!create}. It is not stored in
    {!options}; pass it explicitly when resumed turns should continue emitting
    crash-recovery checkpoints. *)
val resume
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> checkpoint:Checkpoint.t
  -> ?tools:Tool.t list
  -> ?context:Context.t
  -> ?options:options
  -> ?checkpoint_sink:checkpoint_sink
  -> ?config:Types.agent_config
  -> unit
  -> t

val checkpoint : ?session_id:string -> ?working_context:Yojson.Safe.t -> t -> Checkpoint.t

(** {1 Lifecycle} *)

val last_raw_trace_run : t -> Raw_trace.run_ref option
val close : t -> unit
val lifecycle_snapshot : t -> lifecycle_snapshot option

(** {1 Internal (testing only -- do not use in production code)} *)

val set_state : t -> Types.agent_state -> unit
val update_state : t -> (Types.agent_state -> Types.agent_state) -> unit

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

val base_messages : t -> Types.message list

(** Dump the agent's Durable_event journal to [path] as JSONL.
    Returns [Error "no journal"] when the agent was built without
    {!Builder.with_journal}.  Thin wrapper over
    {!Durable_event.save_to_file}.
    @since 0.135.0 *)
val save_journal : t -> string -> (unit, string) result
