(** LLM Provider abstraction.

    @stability Stable
    @since 0.93.1 *)

type provider =
  | Local of { base_url: string }
  | Anthropic
  | OpenAICompat of {
      base_url: string;
      auth_header: string option;
      path: string;
      static_token: string option;
    }
  | Custom_registered of { name: string }

type config = {
  provider: provider;
  model_id: string;
  api_key_env: string;
}

type request_kind =
  | Anthropic_messages
  | Openai_chat_completions
  | Custom of string

type capabilities = {
  max_context_tokens: int option;
  max_output_tokens: int option;
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_parallel_tool_calls: bool;
  supports_reasoning: bool;
  supports_extended_thinking: bool;
  supports_reasoning_budget: bool;
  supports_response_format_json: bool;
  supports_structured_output: bool;
  supports_multimodal_inputs: bool;
  supports_image_input: bool;
  supports_audio_input: bool;
  supports_video_input: bool;
  supports_native_streaming: bool;
  supports_system_prompt: bool;
  supports_caching: bool;
  supports_top_k: bool;
  supports_min_p: bool;
  supports_computer_use: bool;
  supports_code_execution: bool;
}

type model_spec = {
  provider: provider;
  model_id: string;
  api_key_env: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
}

val request_kind : provider -> request_kind
val request_path : provider -> string
val default_capabilities : capabilities
val capabilities_for_model : provider:provider -> model_id:string -> capabilities
val capabilities_for_config : config -> capabilities
val model_spec_of_config : config -> model_spec

(** Resolve provider config to (base_url, api_key, headers) *)
val resolve : config -> (string * string * (string * string) list, Error.sdk_error) result

(** Pre-built provider configs *)
val local_llm : unit -> config
val anthropic_sonnet : unit -> config
val anthropic_haiku : unit -> config
val anthropic_opus : unit -> config
val openrouter : ?model_id:string -> unit -> config

(** @deprecated Use {!local_llm} instead *)
val local_qwen : unit -> config

(** @deprecated Use {!local_llm} instead. MLX removed. *)
val local_mlx : unit -> config

(** {2 Cascade: multi-provider failover} *)

type cascade = {
  primary: config;
  fallbacks: config list;
}

val cascade : primary:config -> fallbacks:config list -> cascade

(** {2 Pricing: per-model cost estimation} *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
  cache_write_multiplier: float;
  cache_read_multiplier: float;
}

val zero_pricing : pricing
val pricing_for_model_opt : string -> pricing option
val pricing_for_model : string -> pricing
val pricing_for_provider : provider:provider -> model_id:string -> pricing
val estimate_cost :
  pricing:pricing ->
  input_tokens:int ->
  output_tokens:int ->
  ?cache_creation_input_tokens:int ->
  ?cache_read_input_tokens:int ->
  unit -> float

(** {2 Custom Provider Registry} *)

type provider_impl = {
  name: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
  build_body:
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit -> string;
  parse_response: string -> Types.api_response;
  resolve: config -> (string * string * (string * string) list, Error.sdk_error) result;
}

val register_provider : provider_impl -> unit
val find_provider : string -> provider_impl option
val registered_providers : unit -> string list
val custom_provider : name:string -> ?model_id:string -> ?api_key_env:string -> unit -> config

(** Well-known env var name for a provider kind.
    Returns empty string for providers that don't need auth (Local, Claude_code).
    @since 0.87.0 *)
val default_api_key_env_of_kind :
  Llm_provider.Provider_config.provider_kind -> string

(** Convert a {!Llm_provider.Provider_config.t} (from Cascade_config)
    into a {!config}.  Falls back to {!default_api_key_env_of_kind}
    when [api_key] is empty.
    @since 0.84.0
    @since 0.87.0 — env var fallback *)
val config_of_provider_config : Llm_provider.Provider_config.t -> config
