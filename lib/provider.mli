(** Shared capability, pricing, authentication, and agent-turn projections.

    @stability Stable
    @since 0.93.1 *)

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
  | Thinking_object_clear_thinking

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
   overlays) do not hand-copy fields. The field list is kept for in-repo
   documentation and is checked against the source record by the compiler. *)
type capabilities = Llm_provider.Capabilities.capabilities =
  { max_context_tokens : int option
  ; serving_constraint : Llm_provider.Serving_constraint.t option
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
  ; supports_document_input : bool
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

val default_capabilities : capabilities

(** Return only the auth-specific headers for a given provider kind.
    This keeps [Provider_config.t.headers] free of sensitive tokens until
    request time. *)
val auth_headers_only_for_kind
  :  kind:Llm_provider.Provider_config.provider_kind
  -> api_key:string
  -> (string * string) list

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
