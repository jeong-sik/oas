(** Builder pattern for Agent creation.
    Provides a flat, chainable API as an alternative to nested Agent.create params. *)

open Types

type t = {
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  model: model;
  name: string;
  system_prompt: string option;
  max_tokens: int;
  max_turns: int;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format_json: bool;
  thinking_budget: int option;
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
  tools: Tool.t list;
  context: Context.t option;
  base_url: string;
  provider: Provider.config option;
  cascade: Provider.cascade option;
  max_idle_turns: int;
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
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
  periodic_callbacks: Agent.periodic_callback list;
  contract: Contract.t;
}

let create ~net ~model =
  {
    net; model;
    name = default_config.name;
    system_prompt = default_config.system_prompt;
    max_tokens = default_config.max_tokens;
    max_turns = default_config.max_turns;
    temperature = default_config.temperature;
    top_p = default_config.top_p;
    top_k = default_config.top_k;
    min_p = default_config.min_p;
    enable_thinking = default_config.enable_thinking;
    response_format_json = default_config.response_format_json;
    thinking_budget = default_config.thinking_budget;
    tool_choice = default_config.tool_choice;
    disable_parallel_tool_use = default_config.disable_parallel_tool_use;
    cache_system_prompt = default_config.cache_system_prompt;
    max_input_tokens = default_config.max_input_tokens;
    max_total_tokens = default_config.max_total_tokens;
    tools = [];
    context = None;
    base_url = Api.default_base_url;
    provider = None;
    cascade = None;
    max_idle_turns = 3;
    hooks = Hooks.empty;
    guardrails = Guardrails.default;
    tracer = Tracing.null;
    raw_trace = None;
    approval = None;
    context_reducer = None;
    context_injector = None;
    mcp_clients = [];
    event_bus = None;
    skill_registry = None;
    elicitation = None;
    description = None;
    periodic_callbacks = [];
    contract = Contract.empty;
  }

let with_system_prompt prompt b = { b with system_prompt = Some prompt }
let with_name name b = { b with name }
let with_max_tokens n b = { b with max_tokens = n }
let with_max_turns n b = { b with max_turns = n }
let with_temperature t b = { b with temperature = Some t }
let with_top_p p b = { b with top_p = Some p }
let with_top_k k b = { b with top_k = Some k }
let with_min_p p b = { b with min_p = Some p }
let with_enable_thinking enabled b = { b with enable_thinking = Some enabled }
let with_tools tools b = { b with tools }
let with_tool tool b = { b with tools = b.tools @ [tool] }
let with_hooks hooks b = { b with hooks }
let with_tracer tracer b = { b with tracer }
let with_raw_trace raw_trace b = { b with raw_trace = Some raw_trace }
let with_approval approval b = { b with approval = Some approval }
let with_context_reducer reducer b = { b with context_reducer = Some reducer }
let with_context ctx b = { b with context = Some ctx }
let with_provider provider b = { b with provider = Some provider }
let with_base_url url b = { b with base_url = url }
let with_mcp_clients clients b = { b with mcp_clients = clients }
let with_guardrails guardrails b = { b with guardrails }
let with_contract contract b =
  { b with contract = Contract.merge b.contract contract }
let with_skill skill b =
  with_contract (Contract.with_skill skill Contract.empty) b
let with_skills skills b =
  with_contract (Contract.with_skills skills Contract.empty) b
let with_tool_grants tool_names b =
  with_contract (Contract.with_tool_grants tool_names Contract.empty) b
let with_mcp_tool_allowlist tool_names b =
  with_contract (Contract.with_mcp_tool_allowlist tool_names Contract.empty) b
let with_tool_choice tc b = { b with tool_choice = Some tc }
let with_disable_parallel_tool_use v b = { b with disable_parallel_tool_use = v }
let with_thinking_budget n b = { b with thinking_budget = Some n }
let with_max_input_tokens n b = { b with max_input_tokens = Some n }
let with_max_total_tokens n b = { b with max_total_tokens = Some n }
let with_response_format_json v b = { b with response_format_json = v }
let with_cache_system_prompt v b = { b with cache_system_prompt = v }
let with_event_bus bus b = { b with event_bus = Some bus }
let with_cascade cascade b = { b with cascade = Some cascade }
let with_max_idle_turns n b = { b with max_idle_turns = n }
let with_context_injector injector b = { b with context_injector = Some injector }
let with_skill_registry reg b = { b with skill_registry = Some reg }
let with_elicitation cb b = { b with elicitation = Some cb }
let with_description desc b = { b with description = Some desc }
let with_periodic_callback cb b =
  { b with periodic_callbacks = b.periodic_callbacks @ [cb] }
let with_periodic_callbacks cbs b =
  { b with periodic_callbacks = b.periodic_callbacks @ cbs }
let with_log_level level _b = Log.set_global_level level; _b
let with_log_sink sink _b = Log.add_sink sink; _b
let with_fallback fallback b =
  let casc = match b.cascade with
    | Some c -> { c with Provider.fallbacks = c.fallbacks @ [fallback] }
    | None ->
      let primary = match b.provider with
        | Some p -> p
        | None -> Provider.anthropic_sonnet ()
      in
      Provider.cascade ~primary ~fallbacks:[fallback]
  in
  { b with cascade = Some casc }

let build b =
  let tools = Contract.filter_tools b.contract b.tools in
  let mcp_clients = Contract.filter_mcp_clients b.contract b.mcp_clients in
  let context = Contract.context_with_contract ?context:b.context b.contract in
  let config = {
    name = b.name;
    model = b.model;
    system_prompt = Contract.compose_system_prompt ?base:b.system_prompt b.contract;
    max_tokens = b.max_tokens;
    max_turns = b.max_turns;
    temperature = b.temperature;
    top_p = b.top_p;
    top_k = b.top_k;
    min_p = b.min_p;
    enable_thinking = b.enable_thinking;
    response_format_json = b.response_format_json;
    thinking_budget = b.thinking_budget;
    tool_choice = b.tool_choice;
    disable_parallel_tool_use = b.disable_parallel_tool_use;
    cache_system_prompt = b.cache_system_prompt;
    max_input_tokens = b.max_input_tokens;
    max_total_tokens = b.max_total_tokens;
  } in
  let options = {
    Agent.base_url = b.base_url;
    provider = b.provider;
    cascade = b.cascade;
    max_idle_turns = b.max_idle_turns;
    hooks = b.hooks;
    guardrails = b.guardrails;
    tracer = b.tracer;
    raw_trace = b.raw_trace;
    approval = b.approval;
    context_reducer = b.context_reducer;
    context_injector = b.context_injector;
    mcp_clients;
    event_bus = b.event_bus;
    skill_registry = b.skill_registry;
    elicitation = b.elicitation;
    description = b.description;
    periodic_callbacks = b.periodic_callbacks;
  } in
  Agent.create ~net:b.net ~config ~tools ?context ~options ()

let build_safe b =
  if b.max_turns <= 0 then
    Error (Error.Config (Error.InvalidConfig {
      field = "max_turns";
      detail = Printf.sprintf "must be > 0, got %d" b.max_turns;
    }))
  else if b.max_tokens <= 0 then
    Error (Error.Config (Error.InvalidConfig {
      field = "max_tokens";
      detail = Printf.sprintf "must be > 0, got %d" b.max_tokens;
    }))
  else
    match b.thinking_budget, b.enable_thinking with
    | Some _, (None | Some false) ->
        Error (Error.Config (Error.InvalidConfig {
          field = "thinking_budget";
          detail = "thinking_budget requires enable_thinking = true";
        }))
    | _ -> Ok (build b)
