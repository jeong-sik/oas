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

type modality =
  | Text
  | Image
  | Audio
  | Video
  | Multimodal

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
  (* Provider identity *)
  is_ollama: bool;
}

type inference_contract = {
  provider: provider;
  model_id: string;
  modality: modality;
  task: string option;
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
val modality_to_string : modality -> string
val modality_of_capabilities : capabilities -> modality
val default_capabilities : capabilities
val capabilities_for_model : provider:provider -> model_id:string -> capabilities
val capabilities_for_config : config -> capabilities

(** Resolve the provider's declared context window from an optional
    [config], falling back to [~fallback] when the config is [None] or
    the capability reports [None]/[<= 0].

    Shared by [Pipeline.proactive_context_window_tokens] and
    [Builder.with_context_thresholds] so both agree on the
    "provider → capabilities → max_context_tokens" step. The two call
    sites pass different [~fallback] values intentionally (Pipeline is
    stricter at 128K, Builder more permissive at 200K).

    @since 0.123.0 *)
val resolve_max_context_tokens : fallback:int -> config option -> int

val inference_contract_of_model_spec : model_spec -> inference_contract
val inference_contract_of_config : config -> inference_contract
val validate_inference_contract :
  capabilities:capabilities ->
  inference_contract ->
  (unit, Error.sdk_error) result
val model_spec_of_config : config -> model_spec

(** Resolve provider config to (base_url, api_key, headers) *)
val resolve : config -> (string * string * (string * string) list, Error.sdk_error) result

(** Pre-built provider configs *)
val local_llm : unit -> config
val anthropic_sonnet : unit -> config
val anthropic_haiku : unit -> config
val anthropic_opus : unit -> config
val openrouter : ?model_id:string -> unit -> config

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

(** Convert a {!Llm_provider.Provider_config.t} into a {!config}.
    Falls back to {!default_api_key_env_of_kind} when [api_key] is
    empty.
    @since 0.84.0
    @since 0.87.0 — env var fallback *)
val config_of_provider_config : Llm_provider.Provider_config.t -> config
