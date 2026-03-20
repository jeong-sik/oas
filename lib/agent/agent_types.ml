open Types

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
  cascade = None;
  max_idle_turns = 3;
  hooks = Hooks.empty;
  guardrails = Guardrails.default;
  guardrails_async = Guardrails_async.empty;
  tracer = Tracing.null;
  raw_trace = None;
  approval = None;
  context_reducer = Some Defaults.default_context_reducer;
  context_injector = None;
  mcp_clients = [];
  event_bus = None;
  skill_registry = None;
  elicitation = None;
  description = None;
  periodic_callbacks = [];
}

type tool_call_fingerprint = Agent_turn.tool_call_fingerprint

type t = {
  mutable state: agent_state;
  mutable lifecycle: lifecycle_snapshot option;
  mutable last_tool_calls: tool_call_fingerprint list option;
  mutable consecutive_idle_turns: int;
  named_cascade: Api.named_cascade option;
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
let set_state t s = t.state <- s
let description t = t.options.description

let sdk_version = Sdk_version.version

let card t =
  Agent_card.of_info {
    agent_name = t.state.config.name;
    agent_description = t.options.description;
    version = sdk_version;
    config = t.state.config;
    tool_schemas = List.map (fun (tool : Tool.t) -> tool.schema) (Tool_set.to_list t.tools);
    provider = t.options.provider;
    cascade = t.options.cascade;
    mcp_clients_count = List.length t.options.mcp_clients;
    has_elicitation = Option.is_some t.options.elicitation;
    skill_registry = t.options.skill_registry;
  }

let set_lifecycle agent ?current_run_id ?worker_id ?runtime_actor ?last_error
    ?accepted_at ?ready_at ?first_progress_at ?started_at ?last_progress_at
    ?finished_at status =
  agent.lifecycle <- Some (Agent_lifecycle.build_snapshot
    ~agent_name:agent.state.config.name
    ~provider:agent.options.provider
    ~model:agent.state.config.model
    ?previous:agent.lifecycle
    ?current_run_id ?worker_id ?runtime_actor ?last_error
    ?accepted_at ?ready_at ?first_progress_at ?started_at
    ?last_progress_at ?finished_at status)

let create ~net ?(config=default_config) ?(tools=[]) ?context ?named_cascade
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
  { state; lifecycle = None; last_tool_calls = None; consecutive_idle_turns = 0;
    named_cascade;
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
  { state; lifecycle = agent.lifecycle; last_tool_calls = None;
    consecutive_idle_turns = 0; tools = agent.tools; net = agent.net;
    named_cascade = agent.named_cascade;
    context = ctx; options = agent.options }

let last_raw_trace_run agent =
  match agent.options.raw_trace with
  | Some sink -> Raw_trace.last_run sink
  | None -> None

let lifecycle_snapshot agent = agent.lifecycle

let close agent = Mcp.close_all agent.options.mcp_clients
