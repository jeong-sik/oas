open Types

let _log = Log.create ~module_name:"agent_types" ()

type periodic_callback =
  { interval_sec : float
  ; callback : unit -> unit
  }

type checkpoint_stage =
  | After_assistant_collected
  | After_tool_results_appended
  | After_retry_feedback_appended

let checkpoint_stage_to_string = function
  | After_assistant_collected -> "after_assistant_collected"
  | After_tool_results_appended -> "after_tool_results_appended"
  | After_retry_feedback_appended -> "after_retry_feedback_appended"
;;

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

let empty_recovery_state =
  { last_completed_round = None
  ; pending_episodes = None
  ; pending_receipt = None
  ; restore_error = None
  }
;;

type options =
  { base_url : string
  ; provider : Provider.config option
  ; max_execution_time_s : float option
  ; stream_idle_timeout_s : float option
  ; body_timeout_s : float option
  ; execution_idle_timeout_s : float option
  ; max_idle_turns : int
  ; idle_final_warning_at : int option
  ; hooks : Hooks.hooks
  ; guardrails : Guardrails.t
  ; guardrails_async : Guardrails_async.t
  ; tracer : Tracing.t
  ; trace_link : (string * string) option
    (** Optional (trace_id, span_id) of a parent span to link to.
        Used to connect OAS agent turns to an external trace root. *)
  ; raw_trace : Raw_trace.t option
  ; approval : Hooks.approval_callback option
  ; missing_approval_callback_policy : Hooks.missing_approval_callback_policy
  ; context_reducer : Context_reducer.t option
  ; context_injector : Hooks.context_injector option
  ; mcp_clients : Mcp.managed list
  ; event_bus : Event_bus.t option
  ; skill_registry : Skill_registry.t option
  ; elicitation : Hooks.elicitation_callback option
  ; description : string option
  ; periodic_callbacks : periodic_callback list
  ; allowed_paths : string list
  ; operator_policy : Guardrails.tool_filter option
  ; policy_channel : Policy_channel.t option
  ; tool_selector : Tool_selector.strategy option
  ; disclosure_level : Tool.disclosure_level option
  ; disclosure_resolver : (Types.tool_result list -> Tool.disclosure_level option) option
    (** Optional resolver that decides the disclosure level for the
        next turn based on the most recent tool results.  When
        [Some f], it is called before each turn with the latest
        [Types.tool_result] list extracted from message history.
        - [f results = Some override] uses [override] for this turn.
        - [f results = None] falls back to the static
          [disclosure_level] field.
        When [None], [disclosure_level] is used as-is.
        Caller owns the policy (TTL, sticky promotion, session scope);
        OAS only provides the mechanism.
        @since 0.195.0 *)
  ; priority : Llm_provider.Request_priority.t option
  ; slot_id : int option
  ; on_run_complete : (bool -> unit) option
    (** Optional callback invoked when a run finishes.  Receives [true]
        on success, [false] on error.  Runs before lifecycle state is
        updated.  Intended for emitting eval metrics, flushing OTel
        spans, or other end-of-run side effects.  The callback must
        not raise; exceptions are caught and logged. *)
  ; tool_result_relocation : (Tool_result_store.t * Content_replacement_state.t) option
    (** Optional tool result relocation.  When provided,
        {!Agent_turn.make_tool_results} persists large results to disk
        and replaces them with previews.  The [Content_replacement_state]
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
        kinds ([Claude_code], [Codex], [Gemini], [Kimi]) which cannot be
        reached over HTTP.  When [Some t], {!Pipeline.stage_route}
        dispatches via {!Llm_provider.Complete.complete} with this
        transport; when [None], the HTTP path is used.
        @since 0.156.0 *)
  ; runtime_mcp_policy : Llm_provider.Llm_transport.runtime_mcp_policy option
    (** Optional request-scoped MCP exposure policy for CLI transports.
        When [Some], the transport may expose runtime MCP tools without
        relying on inline [Tool.t] schemas.
        @since 0.164.0 *)
  ; summarizer : (message list -> string) option
    (** Optional custom extractive summarizer used by
        {!Budget_strategy.reduce_for_budget} when the Emergency phase
        triggers [Summarize_old].  When [None], the built-in
        [Budget_strategy.default_summarizer] is used (first text block
        of each message, truncated to 100 chars). Consumers that need
        application-specific summary semantics can supply a summarizer that
        transforms messages before they are re-injected as compacted history.
        @since 0.150.0 *)
  }

(* Re-export lifecycle types from Agent_lifecycle.
   Type equations make these structurally identical so existing code
   using Agent.Accepted, Agent.lifecycle_snapshot, etc. still works. *)
type lifecycle_status = Agent_lifecycle.lifecycle_status =
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed
[@@deriving show, yojson]

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
[@@deriving yojson]

let default_options =
  { base_url = Api.default_base_url
  ; provider = None
  ; max_execution_time_s = None
  ; stream_idle_timeout_s = None
  ; body_timeout_s = None
  ; execution_idle_timeout_s = None
  ; max_idle_turns = 3
  ; idle_final_warning_at = None
  ; hooks = Hooks.empty
  ; guardrails = Guardrails.default
  ; guardrails_async = Guardrails_async.empty
  ; tracer = Tracing.null
  ; trace_link = None
  ; raw_trace = None
  ; approval = None
  ; missing_approval_callback_policy = Hooks.Execute_without_callback
  ; context_reducer = Some Defaults.default_context_reducer
  ; context_injector = None
  ; mcp_clients = []
  ; event_bus = None
  ; skill_registry = None
  ; elicitation = None
  ; description = None
  ; periodic_callbacks = []
  ; allowed_paths = []
  ; operator_policy = None
  ; policy_channel = None
  ; tool_selector = None
  ; disclosure_level = None
  ; disclosure_resolver = None
  ; priority = None
  ; slot_id = None
  ; on_run_complete = None
  ; tool_result_relocation = None
  ; journal = None
  ; transport = None
  ; runtime_mcp_policy = None
  ; summarizer = None
  }
;;

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

type idle_state = Agent_turn.idle_state =
  { last_tool_calls : tool_call_fingerprint list option
  ; consecutive_idle_turns : int
  }

type t =
  { mu : Eio.Mutex.t
  ; mutable state : agent_state
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

(* Public accessors — .mli exposes Agent.t as abstract *)
let state t = t.state
let lifecycle t = t.lifecycle
let tools t = t.tools
let context t = t.context
let options t = t.options
let net t = t.net

(** Mutex-protected write to [state].  All mutations of [t.state] should
    go through this function to prevent lost-update races when parallel
    tool-execution fibers or periodic callbacks yield between read and
    write. *)
let set_state t s = Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.state <- s)

(** Read-modify-write [state] under the mutex.  Callers pass a pure
    function [f : agent_state -> agent_state]; the read + write happen
    inside a single critical section so no concurrent update is lost. *)
let update_state t f =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.state <- f t.state)
;;

let recovery_state t = Eio.Mutex.use_ro t.mu (fun () -> t.recovery_state)

let set_recovery_state t recovery_state =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.recovery_state <- recovery_state)
;;

let update_recovery_state t f =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.recovery_state <- f t.recovery_state)
;;

let recovery_run_boundary_metadata t =
  if Option.is_some t.tool_failure_judge
  then Types.Conversation_metadata.run_boundary
  else []
;;

let set_consecutive_idle_turns t n =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.consecutive_idle_turns <- n)
;;

let get_consecutive_idle_turns t =
  Eio.Mutex.use_ro t.mu (fun () -> t.consecutive_idle_turns)
;;

(** Mutex-protected atomic update of both idle-detection fields.  Keeps
    [last_tool_calls] and [consecutive_idle_turns] consistent under
    concurrent updates from parallel tool-execution fibers or periodic
    callbacks. *)
let set_idle_state t (s : Agent_turn.idle_state) =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
    t.last_tool_calls <- s.last_tool_calls;
    t.consecutive_idle_turns <- s.consecutive_idle_turns)
;;

let reset_idle_state t = set_idle_state t (Agent_turn.reset_idle_detection ()).new_state
let description t = t.options.description
let allowed_paths t = t.options.allowed_paths
let sdk_version = Sdk_version.version

let provider_name (cfg : Provider.config) =
  match cfg.provider with
  | Provider.Anthropic -> "anthropic"
  | Provider.OpenAICompat _ -> "openai-compat"
  | Provider.Local _ -> "local"
  | Provider.Custom_registered { name } -> name
;;

let card t =
  let supported_providers =
    match t.options.provider with
    | Some p -> [ provider_name p ]
    | None -> [ "anthropic" ]
  in
  let skills =
    match t.options.skill_registry with
    | Some reg ->
      List.map
        (fun (s : Skill.t) -> { Agent_card.name = s.name; description = s.description })
        (Skill_registry.list reg)
    | None -> []
  in
  Agent_card.of_info
    { agent_name = t.state.config.name
    ; agent_description = t.options.description
    ; version = sdk_version
    ; config = t.state.config
    ; tool_schemas =
        List.map (fun (tool : Tool.t) -> tool.schema) (Tool_set.to_list t.tools)
    ; supported_providers
    ; mcp_clients_count = List.length t.options.mcp_clients
    ; has_elicitation = Option.is_some t.options.elicitation
    ; skills
    }
;;

(** Mutex-protected lifecycle update.  Multiple parallel tool-execution
    fibers call this concurrently via [on_tool_execution_started] /
    [on_tool_execution_finished] callbacks.  Without the mutex the
    read of [agent.lifecycle] (for [?previous]) and the subsequent
    write could interleave, losing an update.

    Validates the transition against {!Agent_lifecycle.valid_transitions}.
    Invalid transitions are rejected: the state is not updated and a
    structured error record is logged. *)
let set_lifecycle
      agent
      ?current_run_id
      ?worker_id
      ?runtime_actor
      ?last_error
      ?accepted_at
      ?ready_at
      ?first_progress_at
      ?started_at
      ?last_progress_at
      ?finished_at
      status
  =
  Eio.Mutex.use_rw ~protect:true agent.mu (fun () ->
    let allowed =
      match agent.lifecycle with
      | Some prev ->
        (match Agent_lifecycle.transition ~from:prev.status ~to_:status with
         | Error e ->
           Log.error
             _log
             "invalid lifecycle transition"
             [ Log.S ("agent", agent.state.config.name)
             ; Log.S ("error", Agent_lifecycle.transition_error_to_string e)
             ];
           false
         | Ok _ -> true)
      | None -> true
    in
    if allowed
    then
      agent.lifecycle
      <- Some
           (Agent_lifecycle.build_snapshot
              ~agent_name:agent.state.config.name
              ~provider:agent.options.provider
              ~model:agent.state.config.model
              ?previous:agent.lifecycle
              ?current_run_id
              ?worker_id
              ?runtime_actor
              ?last_error
              ?accepted_at
              ?ready_at
              ?first_progress_at
              ?started_at
              ?last_progress_at
              ?finished_at
              status))
;;

let create
      ~net
      ?(config = default_config)
      ?(tools = [])
      ?context
      ?(options = default_options)
      ?(auto_context_overflow_retry = true)
      ?checkpoint_sink
      ?tool_failure_judge
      ()
  =
  let mcp_tools =
    List.concat_map (fun (m : Mcp.managed) -> m.tools) options.mcp_clients
  in
  let all_tools = Tool_set.merge (Tool_set.of_list tools) (Tool_set.of_list mcp_tools) in
  let state =
    { config; messages = config.initial_messages; turn_count = 0; usage = empty_usage }
  in
  let ctx =
    match context with
    | Some c -> c
    | None -> Context.create ()
  in
  { mu = Eio.Mutex.create ()
  ; state
  ; lifecycle = None
  ; last_tool_calls = None
  ; consecutive_idle_turns = 0
  ; auto_context_overflow_retry
  ; tools = all_tools
  ; net
  ; context = ctx
  ; options
  ; checkpoint_sink
  ; tool_failure_judge
  ; recovery_state = empty_recovery_state
  }
;;

let clone ?(copy_context = false) agent =
  let ctx = if copy_context then Context.copy agent.context else Context.create () in
  let state =
    { config = agent.state.config
    ; messages = agent.state.messages
    ; turn_count = agent.state.turn_count
    ; usage = agent.state.usage
    }
  in
  { mu = Eio.Mutex.create ()
  ; state
  ; lifecycle = agent.lifecycle
  ; last_tool_calls = None
  ; consecutive_idle_turns = 0
  ; auto_context_overflow_retry = agent.auto_context_overflow_retry
  ; tools = agent.tools
  ; net = agent.net
  ; context = ctx
  ; options = agent.options
  ; checkpoint_sink = agent.checkpoint_sink
  ; tool_failure_judge = agent.tool_failure_judge
  ; recovery_state = agent.recovery_state
  }
;;

let last_raw_trace_run agent =
  match agent.options.raw_trace with
  | Some sink -> Raw_trace.last_run sink
  | None -> None
;;

let lifecycle_snapshot agent = agent.lifecycle
let close agent = Mcp.close_all agent.options.mcp_clients
