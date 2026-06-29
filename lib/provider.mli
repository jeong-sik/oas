(** LLM Provider abstraction.

    @stability Stable
    @since 0.93.1 *)

type provider =
  | Local of { base_url : string }
  | Anthropic
  | OpenAICompat of
      { base_url : string
      ; auth_header : string option
      ; path : string
      ; static_token : string option
      }
  | Custom_registered of { name : string }

type config =
  { provider : provider
  ; model_id : string
  ; api_key_env : string
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

(** Wire-format for controlling thinking/reasoning on OpenAI-compat backends.

    {b API stability note (pre-1.0).}  This type is part of the Stable
    surface but is intentionally a transparent equation of
    {!Llm_provider.Capabilities.thinking_control_format} (which is
    declared Internal).  Two consequences callers should know about.

    First, adding a constructor (e.g. [Reasoning_effort] in 0.195.0)
    is source-breaking for downstream exhaustive [match] expressions.
    Constructors are added here only when a new wire format is required
    to talk to a real backend — they will not be added for refactoring
    convenience.

    Second, the shared identity with the Internal type is deliberate:
    it closes a long-standing source of duplication where the same enum
    had to be hand-synchronized in two modules.  Treating these as two
    nominally-distinct types reintroduces that duplication — full
    decoupling is a stability-RFC concern, tracked separately from this
    PR.

    @since 0.93.1 *)
type thinking_control_format = Llm_provider.Capabilities.thinking_control_format =
  | No_thinking_control
  | Thinking_object
  | Thinking_object_only (** @since 0.196.11 *)
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think (** @since 0.207.22 *)
  | Reasoning_effort (** @since 0.195.0 *)
  | Enable_thinking (** @since 0.196.11 *)

type preserve_thinking_control_format =
      Llm_provider.Capabilities.preserve_thinking_control_format =
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Top_level_preserve_thinking
  | Always_preserved_thinking

type reasoning_visibility_override =
      Llm_provider.Capabilities.reasoning_visibility_override =
  | Default_reasoning_visibility
  | Force_provider_hidden
  | Force_visible_channel
  | Force_visible_text

type reasoning_replay_override = Llm_provider.Capabilities.reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_preserve_always

type assistant_tool_content_format =
      Llm_provider.Capabilities.assistant_tool_content_format =
  | Assistant_tool_content_null
  | Assistant_tool_content_empty_string

type capabilities =
  { max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_named_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; supports_runtime_mcp_tools : bool
  ; supports_runtime_tool_events : bool
  ; assistant_tool_content_format : assistant_tool_content_format
  ; supports_reasoning : bool
  ; supports_extended_thinking : bool
  ; supports_reasoning_budget : bool
  ; thinking_control_format : thinking_control_format
  ; preserve_thinking_control_format : preserve_thinking_control_format
  ; reasoning_visibility_override : reasoning_visibility_override
  ; reasoning_replay_override : reasoning_replay_override
  ; supports_response_format_json : bool
  ; supports_structured_output : bool
  ; supports_multimodal_inputs : bool
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
  ; modality_priority : Llm_provider.Modality.priority
  ; supports_native_streaming : bool
  ; supports_system_prompt : bool
  ; supports_caching : bool
  ; supports_prompt_caching : bool
  ; prompt_cache_alignment : int option
  ; supports_top_k : bool
  ; supports_min_p : bool
  ; supports_seed : bool
  ; supports_seed_with_images : bool
  ; supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* Usage reporting *)
    emits_usage_tokens : bool
  ; supported_models : string list option
  }

type inference_contract =
  { provider : provider
  ; model_id : string
  ; modality : modality
  ; task : string option
  }

type model_spec =
  { provider : provider
  ; model_id : string
  ; api_key_env : string
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
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

val validate_inference_contract
  :  capabilities:capabilities
  -> inference_contract
  -> (unit, Error.sdk_error) result

val model_spec_of_config : config -> model_spec

(** Resolve provider config to (base_url, api_key, headers) *)
val resolve : config -> (string * string * (string * string) list, Error.sdk_error) result

(** Return only the auth-specific headers for a given provider kind.
    Unlike [headers_with_auth_for_kind] which returns the full header list
    (including Content-Type), this returns only the authentication header
    so it can be merged with existing non-auth headers at request time.
    This keeps [Provider_config.t.headers] free of sensitive tokens. *)
val auth_headers_only_for_kind
  :  kind:Llm_provider.Provider_config.provider_kind
  -> api_key:string
  -> (string * string) list

(** Pre-built provider configs *)
val local_llm : unit -> config

val anthropic_sonnet : unit -> config
val anthropic_haiku : unit -> config
val anthropic_opus : unit -> config
val openrouter : ?model_id:string -> unit -> config

(** {2 Pricing: per-model cost estimation} *)

(* Re-exported from [Llm_provider.Pricing] so [Provider.pricing] is the same type
   (pricing is catalog-sourced via that module; see provider.ml). *)
type pricing = Llm_provider.Pricing.pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

val zero_pricing : pricing
val pricing_for_model_opt : string -> pricing option
val pricing_for_model : string -> pricing
val pricing_for_provider : provider:provider -> model_id:string -> pricing

val estimate_cost
  :  pricing:pricing
  -> input_tokens:int
  -> output_tokens:int
  -> ?cache_creation_input_tokens:int
  -> ?cache_read_input_tokens:int
  -> unit
  -> float

(** {2 Custom Provider Registry} *)

type provider_impl =
  { name : string
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  ; build_body :
      config:Types.agent_state
      -> messages:Types.message list
      -> ?tools:Yojson.Safe.t list
      -> unit
      -> string
  ; parse_response : string -> Types.api_response
  ; resolve : config -> (string * string * (string * string) list, Error.sdk_error) result
  }

val register_provider : provider_impl -> unit
val find_provider : string -> provider_impl option
val registered_providers : unit -> string list

val custom_provider
  :  name:string
  -> ?model_id:string
  -> ?api_key_env:string
  -> unit
  -> config

(** Well-known env var name for a provider kind.
    Returns empty string for providers that don't need auth
    (Local and the CLI transports).
    @since 0.87.0 *)
val default_api_key_env_of_kind : Llm_provider.Provider_config.provider_kind -> string

(** Convert a {!Llm_provider.Provider_config.t} into a {!config}.
    Falls back to {!default_api_key_env_of_kind} when [api_key] is
    empty.
    @since 0.84.0
    @since 0.87.0 — env var fallback *)
val config_of_provider_config : Llm_provider.Provider_config.t -> config

(** Forward adapter: build a {!Llm_provider.Provider_config.t} from an
    agent state and optional {!config}.  Sampling params, tool_choice,
    thinking controls come from [state.config]; provider kind,
    headers, request_path, and api_key come from [provider_opt]
    (or the [ANTHROPIC_API_KEY] fallback when [None]).

    [OpenAICompat] provider collapses to [OpenAI_compat] kind: the
    legacy {!config} variant does not distinguish arbitrary
    OpenAI-compatible endpoints from named providers carrying their own
    kind.  Callers needing kind + arbitrary URL should construct
    {!Llm_provider.Provider_config.t} via
    {!Llm_provider.Provider_config.make} directly.

    [Custom_registered {name}] preserves the registry-declared
    {!Llm_provider.Provider_config.provider_kind} by looking [name] up
    in {!Llm_provider.Provider_registry.default} and using
    [entry.defaults.kind].  Errors with [InvalidConfig] when [name] is
    not registered.

    @since 0.155.0
    @since 0.161.0 — Custom_registered kind preservation *)
val provider_config_of_agent
  :  state:Types.agent_state
  -> base_url:string
  -> config option
  -> (Llm_provider.Provider_config.t, Error.sdk_error) result
