(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

type thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object (** Top-level [thinking] object plus optional [reasoning_effort]. *)
  | Thinking_object_only
  (** Kimi-style: top-level [thinking] object without [reasoning_effort]. *)
  | Chat_template_kwargs
  (** llama-server/vLLM/SGLang style:
      [{"chat_template_kwargs":{"enable_thinking":b,"preserve_thinking":b}}] *)
  | Chat_template_token
  (** Chat-template control token style: the request builder injects a model
      token such as [<|think|>] into the rendered conversation instead of
      sending a top-level thinking field. *)
  | Reasoning_effort
  (** Openai-style top-level [reasoning_effort] string field. The typed value
      set lives in {!Reasoning_effort}; provider-specific aliases are applied
      by {!Reasoning_dialect.normalize_effort_value}. Disabled reasoning is
      represented by omitting the field. Ollama's OpenAI-compatible mode uses
      this shape. *)
  | Enable_thinking
  (** DashScope-style top-level [enable_thinking] / [preserve_thinking] bools
      plus optional [thinking_budget]. *)

type preserve_thinking_control_format =
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  (** [thinking.keep = "all"] inside the top-level
      [thinking] object. *)
  | Chat_template_kwargs_preserve_thinking
  (** Self-hosted chat-template kwargs [preserve_thinking] flag. *)
  | Top_level_preserve_thinking
  (** Provider top-level [preserve_thinking] flag, separate from
      [enable_thinking]. *)
  | Always_preserved_thinking
  (** Provider always requires historical reasoning replay and has no
      request-time preserve toggle. *)

type reasoning_visibility_override =
  | Default_reasoning_visibility
  | Force_provider_hidden
  | Force_visible_channel
  | Force_visible_text

type reasoning_replay_override = Capability_vocab.reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_preserve_always

type capabilities =
  { (* Numeric limits *)
    max_context_tokens : int option
  ; max_output_tokens : int option
  ; (* Tool use *)
    supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; supports_runtime_mcp_tools : bool
  ; supports_runtime_tool_events : bool
  ; (* Thinking / reasoning *)
    supports_reasoning : bool
  ; supports_extended_thinking : bool
  ; supports_reasoning_budget : bool
  ; thinking_control_format : thinking_control_format
  ; preserve_thinking_control_format : preserve_thinking_control_format
  ; reasoning_visibility_override : reasoning_visibility_override
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
    Input is expected lowercased, matching {!gemini_family_of_id}. *)
val gemini_thinking_control_of_id : string -> gemini_thinking_control

(** Look up capabilities for [model_id] in the loaded model catalog only
    (no manifest consultation).

    The catalog is resolved by {!Model_catalog.global}, in order: runtime
    override installed via {!Model_catalog.set_global}, the
    [OAS_MODEL_CATALOG] environment variable, a cwd-parent walk (depth 10)
    for [models.toml] and then [oas-models.toml], then
    [~/.config/oas/models.toml]. The ambient discovery (env + cwd walk +
    XDG path) is sampled lazily once per process.

    Returns [None] when no catalog is available or when no catalog entry
    prefix-matches [model_id]; there is no in-code fallback table. *)
val for_model_id_catalog : string -> capabilities option

(** Look up capabilities for [model_id] with a provider-qualified catalog
    override first.

    Provider-qualified entries use [<provider_label>/<model_id>] or
    [<provider_label>:<model_id>] prefixes in [models.toml]. When no qualified
    entry matches, this falls back to {!for_model_id}. This lets transports
    such as Ollama Cloud override bare model-family entries that are shared
    with other providers (for example [glm-5] or [kimi-k2.6]) without coupling
    the catalog to any embedding application. *)
val for_provider_model_id
  :  provider_label:string
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
    [gemini], [ollama], [glm] / [zhipu], [kimi], [dashscope], [nvidia].

    Returns [None] for labels outside this set. Intended for adapter
    layers that track provider kind as a string (e.g. config loaders,
    metrics exporters) and want a single SSOT for provider-level
    capability flags.

    @since 0.170.9 *)
val capabilities_for_provider_label : string -> capabilities option

(** Merge Discovery ctx_size into existing capabilities. *)
val with_context_size : capabilities -> ctx_size:int -> capabilities

(** Update tool support from Discovery. *)
val with_tool_support : capabilities -> supports_tools:bool -> capabilities

(** {2 Capability Manifest} *)

(** Apply a {!Capability_manifest.entry} on top of a provider-preset base.

    [entry.base_label] (if present) is resolved via
    {!capabilities_for_provider_label}; unknown or absent labels fall back
    to {!default_capabilities}.  Each [Some] field in [entry] overrides the
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
