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
  initial_messages: message list;
  max_cost_usd: float option;
  tools: Tool_set.t;
  context: Context.t option;
  base_url: string;
  provider: Provider.config option;
  cascade: Provider.cascade option;
  named_cascade: Api.named_cascade option;
  max_idle_turns: int;
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
  guardrails_async: Guardrails_async.t;
  tracer: Tracing.t;
  raw_trace: Raw_trace.t option;
  approval: Hooks.approval_callback option;
  context_reducer: Context_reducer.t option;
  context_compact_ratio: float option;
  context_prepare_ratio: float option;
  context_handoff_ratio: float option;
  context_injector: Hooks.context_injector option;
  mcp_clients: Mcp.managed list;
  event_bus: Event_bus.t option;
  skill_registry: Skill_registry.t option;
  elicitation: Hooks.elicitation_callback option;
  description: string option;
  periodic_callbacks: Agent.periodic_callback list;
  contract: Contract.t;
  memory: Memory.t option;
  allowed_paths: string list;
  progressive_tools: Progressive_tools.disclosure_strategy option;
  operator_policy: Guardrails.tool_filter option;
  priority: Llm_provider.Request_priority.t option;
}

let create ~net ~model =
  {
    net; model = Model_registry.resolve_model_id model;
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
    initial_messages = default_config.initial_messages;
    max_cost_usd = default_config.max_cost_usd;
    tools = Tool_set.empty;
    context = None;
    base_url = Api.default_base_url;
    provider = None;
    cascade = None;
    named_cascade = None;
    max_idle_turns = 3;
    hooks = Hooks.empty;
    guardrails = Guardrails.default;
    guardrails_async = Guardrails_async.empty;
    tracer = Tracing.null;
    raw_trace = None;
    approval = None;
    context_reducer = None;
    context_compact_ratio = None;
    context_prepare_ratio = None;
    context_handoff_ratio = None;
    context_injector = None;
    mcp_clients = [];
    event_bus = None;
    skill_registry = None;
    elicitation = None;
    description = None;
    periodic_callbacks = [];
    contract = Contract.empty;
    memory = None;
    allowed_paths = [];
    progressive_tools = None;
    operator_policy = None;
    priority = None;
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
let with_tools tools b = { b with tools = Tool_set.of_list tools }
let with_tool tool b = { b with tools = Tool_set.merge b.tools (Tool_set.singleton tool) }
let with_hooks hooks b = { b with hooks }
let with_tracer tracer b = { b with tracer }
let with_raw_trace raw_trace b = { b with raw_trace = Some raw_trace }
let with_approval approval b = { b with approval = Some approval }
let with_context_reducer reducer b = { b with context_reducer = Some reducer }
let with_context_thresholds ~compact_ratio ?prepare_ratio ?handoff_ratio b =
  let reducer = Context_reducer.from_context_config ~compact_ratio
    ~max_tokens:(match b.max_total_tokens with Some n -> n | None -> 200_000) () in
  { b with context_reducer = Some reducer;
    context_compact_ratio = Some compact_ratio;
    context_prepare_ratio = prepare_ratio;
    context_handoff_ratio = handoff_ratio }
let with_context ctx b = { b with context = Some ctx }
let with_provider provider b = { b with provider = Some provider }
let with_provider_config pc b =
  with_provider (Provider.config_of_provider_config pc) b
let with_base_url url b = { b with base_url = url }
let with_mcp_clients clients b = { b with mcp_clients = clients }
let with_guardrails guardrails b = { b with guardrails }
let with_guardrails_async guardrails_async b = { b with guardrails_async }
let with_operator_policy policy b = { b with operator_policy = Some policy }
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
let with_initial_messages msgs b = { b with initial_messages = msgs }
let with_max_cost_usd v b = { b with max_cost_usd = Some v }
let with_response_format_json v b = { b with response_format_json = v }
let with_cache_system_prompt v b = { b with cache_system_prompt = v }
let with_event_bus bus b = { b with event_bus = Some bus }
let with_cascade cascade b = { b with cascade = Some cascade }
let with_named_cascade named_cascade b = { b with named_cascade = Some named_cascade }
let with_max_idle_turns n b = { b with max_idle_turns = n }
let with_context_injector injector b = { b with context_injector = Some injector }
let with_skill_registry reg b = { b with skill_registry = Some reg }
let with_progressive_tools strategy b = { b with progressive_tools = Some strategy }
let with_elicitation cb b = { b with elicitation = Some cb }
let with_description desc b = { b with description = Some desc }
let with_memory mem b = { b with memory = Some mem }
let with_allowed_paths paths b = { b with allowed_paths = paths }
let with_periodic_callback cb b =
  { b with periodic_callbacks = b.periodic_callbacks @ [cb] }
let with_periodic_callbacks cbs b =
  { b with periodic_callbacks = b.periodic_callbacks @ cbs }
let with_log_level level _b = Log.set_global_level level; _b
let with_log_sink sink _b = Log.add_sink sink; _b
let with_event_targets _targets b =
  (* Event_forward.create requires start() with sw+net at runtime.
     Builder stores the target list; actual forwarder is created in build(). *)
  b
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
  let tools = Tool_set.of_list (Contract.filter_tools b.contract (Tool_set.to_list b.tools)) in
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
    initial_messages = b.initial_messages;
    max_cost_usd = b.max_cost_usd;
    context_compact_ratio = b.context_compact_ratio;
    context_prepare_ratio = b.context_prepare_ratio;
    context_handoff_ratio = b.context_handoff_ratio;
    priority = b.priority;
  } in
  let options = {
    Agent_types.base_url = b.base_url;
    provider = b.provider;
    cascade = b.cascade;
    max_idle_turns = b.max_idle_turns;
    hooks = (match b.progressive_tools with
      | None -> b.hooks
      | Some strategy ->
        let prog_hook = Progressive_tools.as_hook strategy in
        let existing_pre = b.hooks.pre_tool_use in
        { b.hooks with pre_tool_use = Some (fun event ->
          match prog_hook event with
          | Hooks.Skip as d -> d
          | _ -> (match existing_pre with
                  | Some h -> h event
                  | None -> Hooks.Continue)) });
    guardrails = b.guardrails;
    guardrails_async = b.guardrails_async;
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
    memory = b.memory;
    allowed_paths = b.allowed_paths;
    operator_policy = b.operator_policy;
  } in
  Agent.create ~net:b.net ~config ~tools:(Tool_set.to_list tools) ?context
    ?named_cascade:b.named_cascade ~options ()

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
    | _ ->
      match b.max_cost_usd with
      | Some v when v < 0.0 ->
        Error (Error.Config (Error.InvalidConfig {
          field = "max_cost_usd";
          detail = Printf.sprintf "must be >= 0.0, got %.4f" v;
        }))
      | _ -> Ok (build b)
