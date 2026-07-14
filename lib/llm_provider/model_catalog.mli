(** Dynamic Model Catalog TOML loader.

    Integrates per-model capability and pricing overrides from TOML configurations,
    replacing hardcoded code-level registries. *)

type model_entry =
  { id_prefix : string
  ; base_label : string option
    (** Registry provider identity for OpenAI-compatible model families whose
        wire kind alone would otherwise collapse to [openai_compat]. This is
        not a capability preset; [base_label] remains the capability base. *)
  ; provider_name : string option
  ; max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_required_tool_choice : bool option
  ; supports_named_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; assistant_tool_content_format : string option
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
  ; accepted_reasoning_efforts : string list option
  ; supports_response_format_json : bool option
  ; supports_structured_output : bool option
  ; supports_multimodal_inputs : bool option
  ; supports_image_input : bool option
  ; supports_audio_input : bool option
  ; supports_video_input : bool option
  ; modality_priority : string option
  ; task : Capability_vocab.task option
    (** Catalog-declared inference task for non-chat models (transcription,
        speech, image/video generation). Parsed fail-closed against
        {!Capability_vocab.task_values}; [None] means the entry declares no
        task — it is never inferred from the model id. *)
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; ignored_sampling_parameters : Capability_vocab.sampling_parameter list option
    (** Request sampling parameters that this catalog row declares must not be
        serialized. Parsed fail-closed against
        {!Capability_vocab.sampling_parameter_values}. *)
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  ; thinking_control_format : Capability_vocab.thinking_control_format option
    (** Joined from the TOML [thinking_control_format] and [thinking_control_token]
        keys at parse time: [chat_template_token] carries its token in the
        [Chat_template_token] constructor, so a tokenless row (or a token without
        that format) fails closed during {!load_file}. *)
  ; anthropic_thinking_control : Capability_vocab.anthropic_thinking_control option
    (** Anthropic Messages API thinking-control policy. This is explicit
        catalog data; request builders must not infer it from model-id text. *)
  ; preserve_thinking_control_format : string option
  ; reasoning_output_format : string option
  ; reasoning_streaming_format : string option
  ; reasoning_replay : string option
  ; input_per_million : float option
  ; output_per_million : float option
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type provider_entry = Model_provider_catalog.entry =
  { id : string
  ; aliases : string list
  ; kind : Provider_kind.t
  ; identity_kinds : Provider_kind.t list
  ; base_url : string
  ; base_url_env : string option
  ; request_path : string
  ; api_key_env : string
  ; default_model : string option
  ; capabilities_base : string option
  ; identity_hosts : string list
  }

type t

(** Raised by {!global} when the build-time generated catalog violates the
    catalog syntax or schema. The generated catalog is an OAS build invariant,
    so there is no empty-catalog fallback. *)
exception Invalid_embedded_catalog of string

val empty : t
val of_model_entries : model_entry list -> t
val model_entries : t -> model_entry list
val provider_entries : t -> provider_entry list

(** Parse and validate a model catalog from an in-memory TOML value. [source]
    is included in syntax-error diagnostics. This is the typed boundary for
    callers that already own the catalog contents; no global state is changed. *)
val of_toml_string : source:string -> string -> (t, string) result

val load_file : string -> (t, string) result

(** Load the build-time embedded default [models.toml].

    The embedded value is generated directly from the OAS-owned root
    [models.toml], so linked consumers do not depend on a working directory,
    installation prefix, or host filesystem layout. Returns [Error] when the
    embedded TOML cannot be parsed; callers that require catalog-backed
    capability decisions should propagate that error rather than falling back
    silently. *)
val load_default : unit -> (t, string) result

(** Longest-prefix lookup across provider-independent rows using the catalog's
    exact declared [id_prefix] syntax. Provider-scoped rows are excluded. *)
val lookup : t -> string -> model_entry option

(** Longest-prefix lookup across rows whose declared [provider_name] exactly
    matches [provider_name]. The provider and model remain separate values;
    OAS never synthesizes slash, colon, or dot-qualified model ids. *)
val lookup_for_provider
  :  t
  -> provider_name:string
  -> model_id:string
  -> model_entry option

(** Return the catalog-declared provider identity for a concrete endpoint.

    Matching is exact and declaration-driven: [base_url] matches either the
    provider row's canonical [base_url], its optional [base_url_env] override,
    or one of its exact [identity_hosts]. No model id is consulted. *)
val provider_label_for_base_url
  :  ?getenv:(string -> string option)
  -> t
  -> kind:Provider_kind.t
  -> base_url:string
  -> string option

(** Like {!provider_label_for_base_url}, but also requires the request path to
    match the provider row. Use this for telemetry/provider-name identity where
    the wire shape matters, not for capability host gates. *)
val provider_label_for_endpoint
  :  ?getenv:(string -> string option)
  -> t
  -> kind:Provider_kind.t
  -> base_url:string
  -> request_path:string
  -> string option

(** Return the active model catalog.

    Resolution order:
    - runtime override installed with {!set_global}
    - build-time embedded OAS [models.toml]

    The embedded result is cached after the first load. Invalid generated data
    raises {!Invalid_embedded_catalog}; it never becomes [None] or an empty
    catalog. OAS does not inspect an environment variable for an alternate
    catalog. Callers that need a custom catalog must call {!load_file} and
    {!set_global} explicitly.

    {!clear_global} clears the runtime override and embedded cache. *)
val global : unit -> t option

val set_global : t -> unit
val clear_global : unit -> unit
