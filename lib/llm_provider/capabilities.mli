(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

type thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object
  (** Provider_g-style: top-level [thinking] object plus [reasoning_effort]. *)
  | Thinking_object_only
  (** Provider_c K2.5-style: top-level [thinking] object without [reasoning_effort]. *)
  | Chat_template_kwargs
  (** llama-server style: {"chat_template_kwargs":{"enable_thinking":b}} *)
  | Reasoning_effort
  (** Provider_d-style top-level [reasoning_effort] string field. The set of
      values this codebase emits is [{"none","low","medium","high"}] —
      see {!Provider_config.effort_of_thinking_config}. (Provider_d's spec
      also accepts ["minimal"], but no current OAS request builder emits
      it.) Ollama's Provider_d-compatible mode uses this shape. *)
  | Enable_thinking
  (** Provider_h-style top-level [enable_thinking] bool plus optional
      [thinking_budget]. *)

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
      providers (Provider_d, Provider_f) do not guarantee it.
      @since 0.185.0 *)
  ; (* Advanced modalities *)
    supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* Usage reporting *)
    emits_usage_tokens : bool
    (** Whether the provider's standard response carries usage tokens
      (input_tokens/output_tokens). CLI-class wrappers (cli_tool_a,
      cli_tool_b, cli_tool_c) strip usage before returning, so
      downstream metrics coverage gating must treat text-only turns
      against them as structurally unreported rather than a gap.

      @since 0.170.9 *)
  ; (* Model limitations *)
    supported_models : string list option
    (** Explicit list of supported models if the provider is restricted
        to a specific set (e.g. Provider_c CLI to "provider_c-for-coding").
        [None] means no strict client-side restriction. *)
  }

val default_capabilities : capabilities
val provider_a_capabilities : capabilities
val provider_c_capabilities : capabilities
val provider_d_chat_capabilities : capabilities
val provider_d_chat_extended_capabilities : capabilities
val provider_f_capabilities : capabilities
val ollama_capabilities : capabilities
val provider_h_capabilities : capabilities
val provider_k_capabilities : capabilities
val agent_llm_a_code_capabilities : capabilities
val provider_f_cli_capabilities : capabilities
val provider_c_cli_capabilities : capabilities
val agent_code_cli_capabilities : capabilities

(** NVIDIA NIM Provider_l capabilities: Llama-based, chat_template_kwargs thinking.
    @since 0.185.0 *)
val provider_l_capabilities : capabilities

(** Typed Provider_f model family. SSOT for the [provider_f-*] prefix dispatch that
    used to live as scattered [String.starts_with] calls. Downstream code
    should switch on this variant rather than re-compare strings.

    @since 0.196.3 *)
type provider_f_family =
  | Provider_f_3_1 (** [provider_f-3.1.*] *)
  | Provider_f_3 (** [provider_f-3.*] but not 3.1 *)
  | Provider_f_2_5 (** [provider_f-2.5.*] (legacy line) *)
  | Provider_f_other of string
  (** Unknown provider_f id, or non-provider_f id (literal retained). *)

(** Classify a model id into a [provider_f_family]. Order: [3.1] before [3] so the
    more specific prefix wins. Input is expected lowercased; callers that
    cannot lowercase first should normalize via [String.lowercase_ascii] at
    the boundary. *)
val provider_f_family_of_id : string -> provider_f_family

(** Typed route selected by the built-in static capability table.

    This is the closed-sum replacement for the former monolithic
    [for_model_id_static] string ladder. Prefix/string normalization stays
    inside {!static_model_route_of_id}; capability construction switches on this
    variant so adding or removing a model family is an exhaustive code change. *)
type static_model_route =
  | Agent_llm_a_opus_4
  | Agent_llm_a_sonnet_4
  | Agent_llm_a_haiku_4
  | Provider_d_5
  | Provider_d_4_1
  | Provider_d_4o
  | Provider_f of provider_f_family
  | Provider_c_for_coding
  | Provider_c_k2
  | Provider_h_3
  | Provider_n_4
  | Provider_g_v4_flash
  | Provider_g_v4_pro
  | Provider_j_large
  | Provider_j_small
  | Provider_m_command
  | Provider_e_grok
  | Provider_l of { has_vision : bool }
  | Provider_f_gemma_4 of { has_large_audio : bool }
  | Glm_4_7_flash
  | Glm_4_5_flash_air
  | Glm_5_turbo
  | Glm_5v_turbo
  | Glm_ocr
  | Glm_4_6_vision_reasoning
  | Glm_4_5_vision_reasoning
  | Glm_5_code
  | Glm_4_5_text
  | Glm_full_text
  | Glm_4_flash
  | Glm_4v
  | Glm_4

(** Resolve a raw model id to the static capability-table route.
    Input is case-insensitive and trims the Ollama Cloud [":cloud"] suffix
    before family classification. *)
val static_model_route_of_id : string -> static_model_route option

(** Lookup capabilities for a known model_id.
    Returns [None] if the model is not in the built-in table. *)
val for_model_id : string -> capabilities option

(** Lookup capabilities for a provider label string.

    Recognized labels (case-insensitive, whitespace trimmed):
    [provider_a], [provider_d] / [provider_d_chat], [provider_d_chat_extended],
    [provider_f], [ollama], [provider_k] / [provider_k-coding], [provider_c], [provider_l],
    [cli_tool_d], [cli_tool_b], [cli_tool_c], [cli_tool_a].

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
