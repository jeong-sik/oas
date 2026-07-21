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
  | Thinking_object_adaptive (** @since 0.207.33 *)
  | Thinking_object_only (** @since 0.196.11 *)
  | Chat_template_kwargs
  | Chat_template_token of string (** @since 0.207.22 — token carried in constructor *)
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

type reasoning_replay_override = Llm_provider.Capabilities.reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_latest_user_turn_tool_calls
  | Force_preserve_always

type assistant_tool_content_format =
      Llm_provider.Capabilities.assistant_tool_content_format =
  | Assistant_tool_content_null
  | Assistant_tool_content_empty_string

type reasoning_output_format = Llm_provider.Capabilities.reasoning_output_format =
  | No_reasoning_output_format
  | Split_reasoning_fields

type reasoning_streaming_format = Llm_provider.Capabilities.reasoning_streaming_format =
  | Default_reasoning_streaming
  | No_reasoning_streaming
  | Delta_reasoning_field of string
  | Template_reasoning_streaming

type sampling_parameter = Llm_provider.Capabilities.sampling_parameter =
  | Temperature
  | Top_p
  | Top_k
  | Min_p
  | Presence_penalty
  | Frequency_penalty
  | Seed

(** Catalog-declared inference task for non-chat models (audio transcription,
    speech synthesis, image/video generation). A value is only ever set by an
    explicit [task] field on a [models.toml] catalog entry — it is never
    inferred from the model id. This closed variant replaced the former
    model-id substring heuristic and the [string option] payload it produced. *)
type task = Llm_provider.Capabilities.task =
  | Transcription
  | Speech
  | Image_generation
  | Video_generation

(* Re-export the canonical capabilities record from [Llm_provider.Capabilities]
   with its type equality exposed, so downstream consumers (e.g. catalog
   overlays) can pass an [Llm_provider.Capabilities.capabilities] straight into
   [register_provider] instead of hand-copying all fields. provider.ml already
   [include]s [Llm_provider.Capabilities]; this only surfaces that identity
   through the interface. The field list is kept for in-repo documentation and
   is checked against the source record by the compiler. *)
type capabilities = Llm_provider.Capabilities.capabilities =
  { max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_required_tool_choice : bool
  ; supports_named_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; assistant_tool_content_format : assistant_tool_content_format
  ; supports_reasoning : bool
  ; supports_extended_thinking : bool
  ; supports_reasoning_budget : bool
  ; accepted_reasoning_efforts : Llm_provider.Reasoning_effort.t list option
  ; thinking_control_format : thinking_control_format
  ; preserve_thinking_control_format : preserve_thinking_control_format
  ; reasoning_output_format : reasoning_output_format
  ; reasoning_streaming_format : reasoning_streaming_format
  ; reasoning_replay_override : reasoning_replay_override
  ; supports_response_format_json : bool
  ; supports_structured_output : bool
  ; supports_multimodal_inputs : bool
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
  ; modality_priority : Llm_provider.Modality.priority
  ; task : task option
  ; supports_native_streaming : bool
  ; supports_system_prompt : bool
  ; supports_caching : bool
  ; supports_prompt_caching : bool
  ; prompt_cache_alignment : int option
  ; supports_top_k : bool
  ; supports_min_p : bool
  ; supports_seed : bool
  ; supports_seed_with_images : bool
  ; ignored_sampling_parameters : sampling_parameter list
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
  ; task : task option
    (** Catalog-declared task threaded from [capabilities.task]; [None] for
        every model whose catalog entry declares no [task] field. *)
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
val task_to_string : task -> string
val modality_of_capabilities : capabilities -> modality
val default_capabilities : capabilities
val capabilities_for_model : provider:provider -> model_id:string -> capabilities
val capabilities_for_config : config -> capabilities
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

(** Explicit provider configs. These constructors never consult ambient
    endpoint/model settings and never choose a model alias. *)
val local_llm : base_url:string -> model_id:string -> unit -> config

val anthropic : model_id:string -> unit -> config
val openrouter : model_id:string -> unit -> config

(** {2 Pricing: per-model cost estimation} *)

(* Re-exported from [Llm_provider.Pricing] so [Provider.pricing] is the same type
   (pricing is catalog-sourced via that module; see provider.ml). *)
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

(** Return catalog pricing for a model. When [provider_id] is supplied, the
    exact provider/model row wins; a provider-independent row is used only when
    that exact row is absent. Provider identity is never inferred from endpoint
    or model syntax. *)
val pricing_for_model_opt : ?provider_id:string -> string -> pricing option

(** Compute an exact cost or report the cache price components required by the
    observed usage but absent from the selected catalog row. *)
val estimate_cost
  :  pricing:pricing
  -> input_tokens:int
  -> output_tokens:int
  -> ?cache_creation_input_tokens:int
  -> ?cache_read_input_tokens:int
  -> unit
  -> cost_estimate

(** {2 Custom Provider Registry} *)

type provider_impl =
  { name : string
  ; provider_kind : Llm_provider.Provider_config.provider_kind
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  ; resolve : config -> (string * string * (string * string) list, Error.sdk_error) result
  }

val register_provider : provider_impl -> unit
val find_provider : string -> provider_impl option
val registered_providers : unit -> string list

val custom_provider
  :  name:string
  -> model_id:string
  -> ?api_key_env:string
  -> unit
  -> config

(** Forward adapter: build a {!Llm_provider.Provider_config.t} from an
    agent state and optional {!config}. Sampling params, tool_choice,
    thinking controls come from [state.config]; provider kind,
    headers, request_path, and api_key come from [provider_opt]. [None]
    is rejected explicitly; this boundary never selects a default provider,
    endpoint, model, or credential name.

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

(** Project the caller-owned agent turn controls onto an exact provider
    configuration. Provider identity, wire kind, endpoint, credential, headers,
    and request path remain unchanged. For the same model, explicit limits and
    capability overrides remain unchanged. When the turn selects another
    model, parent-model overrides are cleared and the target model's context
    limit is resolved through {!Llm_provider.Provider_config}'s capability
    SSOT, so handoffs cannot inherit the parent's model window. *)
val provider_config_with_agent_config
  :  config:Types.agent_config
  -> Llm_provider.Provider_config.t
  -> Llm_provider.Provider_config.t
