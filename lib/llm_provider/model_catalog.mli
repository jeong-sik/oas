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
  ; base_url : string
  ; base_url_env : string option
  ; request_path : string
  ; api_key_env : string
  ; default_model : string option
  ; capabilities_base : string option
  ; identity_hosts : string list
  }

type t

val empty : t
val of_model_entries : model_entry list -> t
val model_entries : t -> model_entry list
val provider_entries : t -> provider_entry list
val load_file : string -> (t, string) result
val load_runtime_file : string -> t option

(** Candidate locations for the packaged default [models.toml].

    The paths come from Dune's site metadata and, for uninstalled development
    builds, Dune's source-root metadata. The list preserves missing candidates
    so {!load_default} can report exactly what it tried. *)
val default_catalog_paths : unit -> string list

(** Load the packaged default [models.toml].

    Returns [Error] when the default catalog cannot be found or parsed; callers
    that require catalog-backed capability decisions should propagate that error
    rather than falling back silently. *)
val load_default : unit -> (t, string) result

(** Longest-prefix lookup for catalog model IDs.

    In addition to exact catalog syntax, [lookup] accepts a flattened
    [<provider_label>.<model_id>] value and resolves it against
    [<provider_label>/<model_id>] or [<provider_label>:<model_id>] entries. This
    keeps embedding runtimes that use dot-qualified model identifiers on the
    same provider-qualified catalog path rather than falling back to generic
    OpenAI-compatible capabilities. *)
val lookup : t -> string -> model_entry option

(** Return the catalog-declared provider identity for the longest matching
    [id_prefix], if that entry declares one. *)
val provider_name_for_model_id : t -> string -> string option

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
    - [OAS_MODEL_CATALOG], when set to a non-empty path
    - packaged default [models.toml] installed through the agent_sdk
      [model_catalog] Dune site, or the source-root [models.toml] when running
      from an uninstalled Dune build

    The ambient result is cached after the first load. {!clear_global} clears
    the runtime override and the ambient cache. *)
val global : unit -> t option

val preload_global : unit -> unit
val set_global : t -> unit
val clear_global : unit -> unit
