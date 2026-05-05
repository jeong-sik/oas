(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

type thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object (** DeepSeek-style: {"thinking":{"type":"enabled"}} *)
  | Chat_template_kwargs
  (** llama-server style: {"chat_template_kwargs":{"enable_thinking":b}} *)

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
  ; (* Output format *)
    supports_response_format_json : bool
  ; supports_structured_output : bool
  ; (* Input modalities *)
    supports_multimodal_inputs : bool
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
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
      providers (OpenAI, Gemini) do not guarantee it.
      @since 0.185.0 *)
  ; (* Advanced modalities *)
    supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* Provider identity *)
    is_ollama : bool
  ; (* Usage reporting *)
    emits_usage_tokens : bool
    (** Whether the provider's standard response carries usage tokens
      (input_tokens/output_tokens). CLI-class wrappers (codex_cli,
      gemini_cli, kimi_cli) strip usage before returning, so
      downstream metrics coverage gating must treat text-only turns
      against them as structurally unreported rather than a gap.

      @since 0.170.9 *)
  ; (* Model limitations *)
    supported_models : string list option
    (** Explicit list of supported models if the provider is restricted
        to a specific set (e.g. Kimi CLI to "kimi-for-coding").
        [None] means no strict client-side restriction. *)
  }

val default_capabilities : capabilities
val anthropic_capabilities : capabilities
val kimi_capabilities : capabilities
val openai_chat_capabilities : capabilities
val openai_chat_extended_capabilities : capabilities
val gemini_capabilities : capabilities
val ollama_capabilities : capabilities
val dashscope_capabilities : capabilities
val glm_capabilities : capabilities
val claude_code_capabilities : capabilities
val gemini_cli_capabilities : capabilities
val kimi_cli_capabilities : capabilities
val codex_cli_capabilities : capabilities

(** NVIDIA NIM Nemotron capabilities: Llama-based, chat_template_kwargs thinking.
    @since 0.185.0 *)
val nemotron_capabilities : capabilities

(** Lookup capabilities for a known model_id.
    Returns [None] if the model is not in the built-in table. *)
val for_model_id : string -> capabilities option

(** Lookup capabilities for a provider label string.

    Recognized labels (case-insensitive, whitespace trimmed):
    [anthropic], [openai] / [openai_chat], [openai_chat_extended],
    [gemini], [ollama], [glm] / [glm-coding], [kimi], [nemotron],
    [claude_code], [gemini_cli], [kimi_cli], [codex_cli].

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
    falling back to the built-in static prefix table on a miss.

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
