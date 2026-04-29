(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

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
  ; (* Sampling parameters *)
    supports_top_k : bool
  ; supports_min_p : bool
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

(** Lookup capabilities for a known model_id.
    Returns [None] if the model is not in the built-in table. *)
val for_model_id : string -> capabilities option

(** Lookup capabilities for a provider label string.

    Recognized labels (case-insensitive, whitespace trimmed):
    [anthropic], [openai] / [openai_chat], [openai_chat_extended],
    [gemini], [ollama], [glm] / [glm-coding], [kimi],
    [claude_code], [gemini_cli], [kimi_cli], [codex_cli].

    Returns [None] for labels outside this set. Intended for adapter
    layers that track provider kind as a string (e.g. config loaders,
    metrics exporters) and want a single SSOT for provider-level
    capability flags.

    @since 0.170.9 *)
val capabilities_for_provider_label : string -> capabilities option

(** Merge Discovery ctx_size into existing capabilities. *)
val with_context_size : capabilities -> ctx_size:int -> capabilities
