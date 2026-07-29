(** Shared capability, pricing, authentication, and agent-turn projections. *)

include Llm_provider.Capabilities

(* ── Pricing: per-model cost estimation ────────────────────────── *)

(* Pricing is sourced exclusively from the model catalog.  Unknown models stay
   [None]; this boundary does not infer a free price from provider or model-id
   text. *)
type pricing = Llm_provider.Pricing.pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type cache_price_component = Llm_provider.Pricing.cache_price_component =
  | Cache_creation
  | Cache_read

type cost_estimate = Llm_provider.Pricing.cost_estimate =
  | Estimated of float
  | Incomplete of cache_price_component list

let pricing_for_model_opt = Llm_provider.Pricing.pricing_for_model_opt
let estimate_cost = Llm_provider.Pricing.estimate_cost

let provider_config_with_agent_config
      ~(config : Types.agent_config)
      (provider_config : Llm_provider.Provider_config.t)
  =
  let model_id = Types.model_to_string config.model in
  let response_format = config.response_format in
  let max_context, model_capabilities_override, supports_structured_output_override =
    if model_id = provider_config.model_id
    then
      ( provider_config.max_context
      , provider_config.model_capabilities_override
      , provider_config.supports_structured_output_override )
    else (
      let target_model =
        { provider_config with
          model_id
        ; max_context = None
        ; model_capabilities_override = None
        ; supports_structured_output_override = None
        }
      in
      let max_context =
        Option.bind
          (Llm_provider.Provider_config.capabilities_for_config_model target_model)
          (fun capabilities -> capabilities.max_context_tokens)
      in
      max_context, None, None)
  in
  { provider_config with
    model_id
  ; max_context
  ; model_capabilities_override
  ; supports_structured_output_override
  ; max_tokens = config.max_tokens
  ; temperature = config.temperature
  ; top_p = config.top_p
  ; top_k = config.top_k
  ; min_p = config.min_p
  ; system_prompt = config.system_prompt
  ; enable_thinking = config.enable_thinking
  ; preserve_thinking = config.preserve_thinking
  ; thinking_budget = config.thinking_budget
  ; reasoning_effort = config.reasoning_effort
  ; tool_choice = config.tool_choice
  ; disable_parallel_tool_use = config.disable_parallel_tool_use
  ; response_format
  ; cache_system_prompt = config.cache_system_prompt
  }
;;
