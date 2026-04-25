open Types

type periodic_callback = {
  interval_sec: float;
  callback: unit -> unit;
}

type tiered_memory = Types.tiered_memory = {
  long_term: string option;
  mid_term: string option;
  short_term: string option;
}

type options = {
  base_url: string;
  provider: Provider.config option;
  max_execution_time_s: float option;
  stream_idle_timeout_s: float option;
  max_idle_turns: int;
  idle_final_warning_at: int option;
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
  guardrails_async: Guardrails_async.t;
  tracer: Tracing.t;
  raw_trace: Raw_trace.t option;
  approval: Hooks.approval_callback option;
  tool_retry_policy: Tool_retry_policy.t option;
  context_reducer: Context_reducer.t option;
  tiered_memory: tiered_memory option;
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
  policy_channel: Policy_channel.t option;
  tool_selector: Tool_selector.strategy option;
  priority: Llm_provider.Request_priority.t option;
  slot_id: int option;
  on_run_complete: (bool -> unit) option;
    (** Optional callback invoked when a run finishes.  Receives [true]
        on success, [false] on error.  Runs before lifecycle state is
        updated.  Intended for emitting eval metrics, flushing OTel
        spans, or other end-of-run side effects.  The callback must
        not raise; exceptions are caught and logged. *)
  tool_result_relocation:
    (Tool_result_store.t * Content_replacement_state.t) option;
    (** Optional tool result relocation.  When provided,
        {!Agent_turn.make_tool_results} persists large results to disk
        and replaces them with previews.  The [Content_replacement_state]
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
        kinds ([Claude_code], [Codex_cli], [Gemini_cli], [Kimi_cli]) which cannot be
        reached over HTTP.  When [Some t], {!Pipeline.stage_route}
        dispatches via {!Llm_provider.Complete.complete} with this
        transport; when [None], the HTTP path is used.
        @since 0.156.0 *)
  runtime_mcp_policy:
    Llm_provider.Llm_transport.runtime_mcp_policy option;
    (** Optional request-scoped MCP exposure policy for CLI transports.
        When [Some], the transport may expose runtime MCP tools without
        relying on inline [Tool.t] schemas.
        @since 0.164.0 *)
  summarizer: (message list -> string) option;
    (** Optional custom extractive summarizer used by
        {!Budget_strategy.reduce_for_budget} when the Emergency phase
        triggers [Summarize_old].  When [None], the built-in
        [Budget_strategy.default_summarizer] is used (first text block
        of each message, truncated to 100 chars).  Consumers that embed
        domain-specific structured markers in message bodies (for example
        their own [STATE] blocks or other custom envelopes) can supply a
        summarizer that strips or transforms those markers before they are
        re-injected as compacted history.
        @since 0.150.0 *)
  required_tool_satisfaction: Completion_contract.required_tool_satisfaction;
    (** Predicate used when validating [tool_choice] contracts that require
        tool use. Defaults to accepting any ToolUse for backward compatibility;
        runtimes with effect metadata can require productive/non-read-only
        tools without changing provider wire semantics.
        @since 0.171.0 *)
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

let default_options = {
  base_url = Api.default_base_url;
  provider = None;
  max_execution_time_s = None;
  stream_idle_timeout_s = None;
  max_idle_turns = 3;
  idle_final_warning_at = None;
  hooks = Hooks.empty;
  guardrails = Guardrails.default;
  guardrails_async = Guardrails_async.empty;
  tracer = Tracing.null;
  raw_trace = None;
  approval = None;
  tool_retry_policy = None;
  context_reducer = Some Defaults.default_context_reducer;
  tiered_memory = None;
  context_injector = None;
  mcp_clients = [];
  event_bus = None;
  skill_registry = None;
  elicitation = None;
  description = None;
  memory = None;
  periodic_callbacks = [];
  allowed_paths = [];
  operator_policy = None;
  policy_channel = None;
  tool_selector = None;
  priority = None;
  slot_id = None;
  on_run_complete = None;
  tool_result_relocation = None;
  journal = None;
  transport = None;
  runtime_mcp_policy = None;
  summarizer = None;
  required_tool_satisfaction = Completion_contract.any_tool_call_satisfies;
}

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

type t = {
  mu: Eio.Mutex.t;
  mutable state: agent_state;
  mutable lifecycle: lifecycle_snapshot option;
  mutable last_tool_calls: tool_call_fingerprint list option;
  mutable consecutive_idle_turns: int;
  tools: Tool_set.t;
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  context: Context.t;
  options: options;
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
let set_state t s =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.state <- s)

(** Read-modify-write [state] under the mutex.  Callers pass a pure
    function [f : agent_state -> agent_state]; the read + write happen
    inside a single critical section so no concurrent update is lost. *)
let update_state t f =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () -> t.state <- f t.state)

let set_consecutive_idle_turns t n =
  Eio.Mutex.use_rw ~protect:true t.mu (fun () ->
    t.consecutive_idle_turns <- n)

let description t = t.options.description
let memory t = t.options.memory
let allowed_paths t = t.options.allowed_paths

let sdk_version = Sdk_version.version

let card t =
  Agent_card.of_info {
    agent_name = t.state.config.name;
    agent_description = t.options.description;
    version = sdk_version;
    config = t.state.config;
    tool_schemas = List.map (fun (tool : Tool.t) -> tool.schema) (Tool_set.to_list t.tools);
    provider = t.options.provider;
    mcp_clients_count = List.length t.options.mcp_clients;
    has_elicitation = Option.is_some t.options.elicitation;
    skill_registry = t.options.skill_registry;
  }

(** Mutex-protected lifecycle update.  Multiple parallel tool-execution
    fibers call this concurrently via [on_tool_execution_started] /
    [on_tool_execution_finished] callbacks.  Without the mutex the
    read of [agent.lifecycle] (for [?previous]) and the subsequent
    write could interleave, losing an update.

    Validates the transition against {!Agent_lifecycle.valid_transitions}.
    Invalid transitions are rejected: the state is not updated and an
    error is logged to stderr. *)
let set_lifecycle agent ?current_run_id ?worker_id ?runtime_actor ?last_error
    ?accepted_at ?ready_at ?first_progress_at ?started_at ?last_progress_at
    ?finished_at status =
  Eio.Mutex.use_rw ~protect:true agent.mu (fun () ->
    let allowed = match agent.lifecycle with
      | Some prev ->
        (match Agent_lifecycle.transition ~from:prev.status ~to_:status with
         | Error e ->
           Printf.eprintf "[ERROR] %s (agent=%s)\n%!"
             (Agent_lifecycle.transition_error_to_string e)
             agent.state.config.name;
           false
         | Ok _ -> true)
      | None -> true
    in
    if allowed then
      agent.lifecycle <- Some (Agent_lifecycle.build_snapshot
        ~agent_name:agent.state.config.name
        ~provider:agent.options.provider
        ~model:agent.state.config.model
        ?previous:agent.lifecycle
        ?current_run_id ?worker_id ?runtime_actor ?last_error
        ?accepted_at ?ready_at ?first_progress_at ?started_at
        ?last_progress_at ?finished_at status))

let create ~net ?(config=default_config) ?(tools=[]) ?context
    ?(options=default_options) () =
  let mcp_tools =
    List.concat_map (fun (m : Mcp.managed) -> m.tools) options.mcp_clients
  in
  let all_tools = Tool_set.merge (Tool_set.of_list tools) (Tool_set.of_list mcp_tools) in
  let state = { config; messages = config.initial_messages; turn_count = 0; usage = empty_usage } in
  let ctx = match context with
    | Some c -> c
    | None -> Context.create ()
  in
  { mu = Eio.Mutex.create ();
    state; lifecycle = None; last_tool_calls = None; consecutive_idle_turns = 0;
    tools = all_tools; net; context = ctx; options }

let clone ?(copy_context=false) agent =
  let ctx = if copy_context then Context.copy agent.context
            else Context.create () in
  let state = {
    config = agent.state.config;
    messages = agent.state.messages;
    turn_count = agent.state.turn_count;
    usage = agent.state.usage;
  } in
  { mu = Eio.Mutex.create ();
    state; lifecycle = agent.lifecycle; last_tool_calls = None;
    consecutive_idle_turns = 0; tools = agent.tools; net = agent.net;
    context = ctx; options = agent.options }

let last_raw_trace_run agent =
  match agent.options.raw_trace with
  | Some sink -> Raw_trace.last_run sink
  | None -> None

let lifecycle_snapshot agent = agent.lifecycle

let close agent = Mcp.close_all agent.options.mcp_clients
