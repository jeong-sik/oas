(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

type thinking_control_format = Capability_vocab.thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object (** Top-level [thinking] object plus optional [reasoning_effort]. *)
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking
  (** DashScope-style top-level [enable_thinking] / [preserve_thinking] bools
      plus optional [thinking_budget]. *)
[@@deriving show, eq]

type preserve_thinking_control_format =
      Capability_vocab.preserve_thinking_control_format =
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Top_level_preserve_thinking
  | Always_preserved_thinking

type reasoning_replay_override = Capability_vocab.reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_preserve_always

type assistant_tool_content_format = Capability_vocab.assistant_tool_content_format =
  | Assistant_tool_content_null
  | Assistant_tool_content_empty_string

type reasoning_output_format = Capability_vocab.reasoning_output_format =
  | No_reasoning_output_format
  | Split_reasoning_fields

type reasoning_streaming_format = Capability_vocab.reasoning_streaming_format =
  | Default_reasoning_streaming
  | No_reasoning_streaming
  | Delta_reasoning_field of string
  | Template_reasoning_streaming

(** Catalog-declared inference task for non-chat models. [None] on every
    chat/completion model; a value is only ever set by an explicit [task]
    field on a model catalog entry — never inferred from the model id. *)
type task = Capability_vocab.task =
  | Transcription
  | Speech
  | Image_generation
  | Video_generation

type capabilities =
  { (* Numeric limits *)
    max_context_tokens : int option
  ; max_output_tokens : int option
  ; (* Tool use *)
    supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_required_tool_choice : bool
  ; supports_named_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; supports_runtime_mcp_tools : bool
  ; supports_runtime_tool_events : bool
  ; assistant_tool_content_format : assistant_tool_content_format
  ; (* Thinking / reasoning *)
    supports_reasoning : bool
  ; supports_extended_thinking : bool
  ; supports_reasoning_budget : bool
  ; accepted_reasoning_efforts : Reasoning_effort.t list option
    (** Model/provider-specific subset of canonical reasoning efforts accepted
        by the request wire format. [None] means no subset is declared and the
        dialect vocabulary applies; [Some values] is enforced before request
        serialization. *)
  ; thinking_control_format : thinking_control_format
  ; preserve_thinking_control_format : preserve_thinking_control_format
  ; reasoning_output_format : reasoning_output_format
  ; reasoning_streaming_format : reasoning_streaming_format
  ; reasoning_replay_override : reasoning_replay_override
  ; (* Output format *)
    supports_response_format_json : bool
  ; supports_structured_output : bool
  ; (* Input modalities *)
    supports_multimodal_inputs : bool
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
  ; modality_priority : Modality.priority
  ; (* Inference task *)
    task : task option
    (** Inference task declared by the model catalog entry (transcription,
        speech, image/video generation). [None] = no declared task; there is
        no model-id inference fallback. *)
  ; (* Protocol *)
    supports_native_streaming : bool
  ; supports_system_prompt : bool
  ; supports_caching : bool
  ; supports_prompt_caching : bool
  ; prompt_cache_alignment : int option
  ; (* Sampling parameters *)
    supports_top_k : bool
  ; supports_min_p : bool
  ; supports_seed : bool
    (** Deterministic seed for reproducible sampling.
      @since 0.185.0 *)
  ; supports_seed_with_images : bool
    (** Whether seed determinism is maintained when image inputs are present.
      Local providers (Ollama) achieve near-perfect reproducibility; cloud
      providers (Openai, Gemini) do not guarantee it.
      @since 0.185.0 *)
  ; (* Advanced modalities *)
    supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* Usage reporting *)
    emits_usage_tokens : bool
    (** Whether the provider's standard response carries usage tokens
      (input_tokens/output_tokens).

      @since 0.170.9 *)
  ; (* Model limitations *)
    supported_models : string list option
    (** Explicit list of supported models if the provider is restricted
        to a specific set (e.g. Kimi to "kimi-for-coding").
        [None] means no strict client-side restriction. *)
  }

val default_capabilities : capabilities
val anthropic_capabilities : capabilities
val kimi_capabilities : capabilities
val openai_compat_chat_capabilities : capabilities
val openai_compat_chat_extended_capabilities : capabilities
val gemini_capabilities : capabilities
val ollama_capabilities : capabilities
val ollama_cloud_capabilities : capabilities
val dashscope_capabilities : capabilities
val glm_capabilities : capabilities

(** NVIDIA NIM Nvidia capabilities: Llama-based, chat_template_kwargs thinking.
    @since 0.185.0 *)
val provider_l_capabilities : capabilities

(** Resolve the exact chat-template token for models whose
    [thinking_control_format] is [Chat_template_token]. The token is catalog /
    manifest data, not a hardcoded backend constant. *)
val thinking_control_token_for_provider_model_id
  :  provider_label:string
  -> model_id:string
  -> string option

(** Effective request-level parallel tool-use disablement.

    Explicit caller disablement always wins. Otherwise, when a request carries
    tools and the selected provider/model says it does not support parallel tool
    calls, callers should serialize the provider-specific "disable parallel
    tool use" wire field when the provider has one. *)
val effective_disable_parallel_tool_use
  :  caller_disabled:bool
  -> supports_parallel_tool_calls:bool
  -> tools_present:bool
  -> bool

(** Anthropic thinking-control protocol for a model family.

    Older/current manual-thinking models accept
    [thinking: {"type":"enabled","budget_tokens":N}]. Newer adaptive models
    use [thinking: {"type":"adaptive"}] and optional [output_config.effort].
    Some models require adaptive thinking, and some always run adaptive
    thinking without an explicit [thinking] request field. *)
type anthropic_thinking_control =
  | Anthropic_manual_budget
  | Anthropic_adaptive_preferred
  | Anthropic_adaptive_only
  | Anthropic_always_adaptive

(** Return the documented thinking-control protocol for an Anthropic model id.
    Input is expected lowercased, but the function trims and lowercases for
    callers that pass raw config values. *)
val anthropic_thinking_control_of_id : string -> anthropic_thinking_control

(** Typed Gemini model family. SSOT for the [gemini-*] prefix dispatch that
    used to live as scattered [String.starts_with] calls. Downstream code
    should switch on this variant rather than re-compare strings.

    @since 0.196.3 *)
type gemini_family =
  | Gemini_3_1 (** [gemini-3.1.*] *)
  | Gemini_3 (** [gemini-3.*] but not 3.1 *)
  | Gemini_2_5 (** [gemini-2.5.*] (legacy line) *)
  | Gemini_other of string (** Unknown gemini id, or non-gemini id (literal retained). *)

(** Gemini thinking-control protocol for a model family. Gemini 3+ uses
    [thinkingLevel], while Gemini 2.5 uses [thinkingBudget]. *)
type gemini_thinking_control =
  | Gemini_thinking_budget
  | Gemini_thinking_level of { supports_minimal : bool }
  | Gemini_unknown_thinking_control

(** Classify a model id into a [gemini_family]. Order: [3.1] before [3] so the
    more specific prefix wins. Input is expected lowercased; callers that
    cannot lowercase first should normalize via [String.lowercase_ascii] at
    the boundary. *)
val gemini_family_of_id : string -> gemini_family

(** Return the documented thinking-control protocol for a Gemini model id.
    Accepts raw config values; trims and lowercases before delegating to the
    bounded {!gemini_family_of_id} classifier. *)
val gemini_thinking_control_of_id : string -> gemini_thinking_control

(** Look up capabilities for [model_id] in the loaded model catalog only
    (no manifest consultation).

    The catalog is resolved by {!Model_catalog.global}, in order: runtime
    override installed via {!Model_catalog.set_global}, the
    [OAS_MODEL_CATALOG] environment variable, then the packaged default
    [models.toml]. Ambient discovery is cached after first load; embedding
    hosts and test harnesses can call [Model_catalog.preload_global], inject
    [OAS_MODEL_CATALOG] during bootstrap, or install an explicit runtime
    override.

    Returns [None] when no catalog is available or when no catalog entry
    prefix-matches [model_id]; there is no in-code fallback table. *)
val for_model_id_catalog : string -> capabilities option

(** True when [model_id] explicitly carries [provider_label] using the same
    provider-qualified separators as {!for_provider_model_id}. This is syntax
    recognition only; callers still decide whether the declaration is
    authoritative for their boundary. *)
val model_id_has_provider_label : provider_label:string -> model_id:string -> bool

(** Look up capabilities for [model_id] with a provider-qualified catalog
    override first.

    Provider-qualified entries use [<provider_label>/<model_id>],
    [<provider_label>:<model_id>], or [<provider_label>.<model_id>] prefixes in
    [models.toml]. The dot form covers embedding runtimes that flatten
    [provider_label] and [model_id] into one model identifier while keeping the
    same provider-qualified catalog semantics. When no qualified entry matches,
    [allow_bare_fallback] controls whether this falls back to {!for_model_id}.
    This lets transports such as Ollama Cloud override bare model-family entries
    that are shared with other providers (for example [glm-5] or [kimi-k2.6])
    without coupling the catalog to any embedding application. *)
val for_provider_model_id
  :  allow_bare_fallback:bool
  -> provider_label:string
  -> model_id:string
  -> capabilities option

(** Lookup capabilities for a known model_id.

    Checks the globally loaded model catalog first, then the capability
    manifest. Returns [None] when neither source has a matching entry;
    there is no built-in fallback table. *)
val for_model_id : string -> capabilities option

(** Lookup capabilities for a provider label string.

    Recognized labels (case-insensitive, whitespace trimmed):
    [anthropic] / [claude], [openai_compat] / [openai],
    [gemini], [ollama], [glm] / [zhipu], [kimi], [dashscope],
    [xai], [mistral], [cohere], [mimo], [nvidia].

    Canonical labels and aliases for the closed {!Provider_kind.t} space are
    normalized to a typed kind first, then delegated to {!capabilities_of_kind}.
    String-only presets that are not expressible as a provider kind stay at this
    label boundary.

    Returns [None] for labels outside this set. Intended for adapter
    layers that track provider kind as a string (e.g. config loaders,
    metrics exporters) and want a single SSOT for provider-level
    capability flags.

    @since 0.170.9 *)
val capabilities_for_provider_label : string -> capabilities option

(** Capabilities preset for a canonical {!Provider_kind.t}.

    Maps the 7 closed variants directly to their presets without serialising
    the kind to a string and re-parsing it. Use this when the caller already
    holds a typed {!Provider_kind.t}; {!capabilities_for_provider_label}
    delegates canonical labels here and only keeps string-only presets (e.g.
    ["openai_chat_extended"], ["ollama_cloud"], ["xai"], ["mistral"],
    ["cohere"], ["mimo"], ["nvidia"]) at the label boundary.

    @since 0.209.0 *)
val capabilities_of_kind : Provider_kind.t -> capabilities

(** Merge Discovery ctx_size into existing capabilities. *)
val with_context_size : capabilities -> ctx_size:int -> capabilities

(** Update tool support from Discovery. *)
val with_tool_support : capabilities -> supports_tools:bool -> capabilities

(** {2 Capability Manifest} *)

(** Apply a {!Capability_manifest.entry} on top of a provider-preset base.

    [entry.base_label] (if present) is a parsed {!Capability_manifest.base_label}
    and is resolved via {!capabilities_for_provider_label}; an absent label falls
    back to {!default_capabilities}.  Each [Some] field in [entry] overrides the
    corresponding field; [None] fields inherit from the base.

    @since 0.188.0 *)
val apply_manifest_entry : Capability_manifest.entry -> capabilities

(** Look up capabilities for [model_id] against an explicit manifest,
    falling back to the catalog lookup ({!for_model_id_catalog}) on a
    miss.

    Useful for testing the manifest integration path without relying on
    the [OAS_CAPABILITY_MANIFEST] env var.

    @since 0.188.0 *)
val for_model_id_with_manifest : Capability_manifest.t -> string -> capabilities option

(** {2 Capability Drift Detection} *)

type drift_observation =
  | Usage_missing_but_declared
  | Tools_used_but_declared_unsupported
  | Thinking_returned_but_declared_unsupported
  | Stop_tool_use_but_declared_unsupported
[@@deriving show]

(** Compare an {!api_response} against declared {!capabilities}.
    Returns observations where actual behavior contradicts
    the capability record. Empty list = no drift detected.

    @since 0.185.0 *)
val detect_drift : capabilities -> Types.api_response -> drift_observation list
