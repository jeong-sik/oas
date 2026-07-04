(** External JSON capability manifest for model-specific overrides.

    Allows operators and model deployers to describe capabilities for
    custom, quantized, or future model variants without requiring an
    OAS code change.

    The manifest is a JSON file with the following shape:

    {[
      {
        "schema_version": 1,
        "models": [
          {
            "id_prefix": "my-local-llama",
            "base": "openai_chat",
            "max_context_tokens": 131072,
            "supports_tools": true,
            "supports_reasoning": false
          }
        ]
      }
    ]}

    Set [OAS_CAPABILITY_MANIFEST] to the file path to load it at
    runtime.  The manifest layer sits below the model catalog used by
    [Capabilities.for_model_id]; catalog rows remain the authoritative
    model capability source when both layers match the same model.
    When no catalog row matches, the manifest can supply capabilities
    for custom deployments.

    Priority (highest first):
    + 1. Model catalog row matching by [id_prefix] (case-insensitive prefix)
    + 2. Manifest entry matching by [id_prefix] (case-insensitive prefix)
    + 3. Discovery-based inference / caller-provided default

    @since 0.188.0 *)

(** Canonical provider capability preset label accepted by the manifest parser. *)
type base_label = private string

(** Validate and canonicalize a provider capability preset label. *)
val base_label_of_string : string -> (base_label, string) result

(** Return the canonical wire label. *)
val base_label_to_string : base_label -> string

(** One entry in the capability manifest.

    [id_prefix] is matched as a case-insensitive prefix against the
    model ID being looked up.  [base] names a provider preset from
    {!Capabilities.capabilities_for_provider_label} (e.g.
    ["openai_chat"], ["anthropic"]); when absent, the built-in
    [default_capabilities] is used.  Unrecognised labels are rejected
    by {!base_label_of_string} and by the JSON parser.

    All other fields are optional overrides: a [None] value means
    "inherit from the base", a [Some v] value replaces the base value. *)
type entry =
  { id_prefix : string
  ; base_label : base_label option
    (** Provider preset label, e.g. ["openai_chat"] or ["anthropic"]. *)
  ; max_context_tokens : int option (** [None] = inherit from base. *)
  ; max_output_tokens : int option (** [None] = inherit from base. *)
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_required_tool_choice : bool option
  ; supports_named_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; assistant_tool_content_format : string option
    (** Wire shape for assistant messages that contain tool calls but no visible
        text (null / empty_string). *)
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
  ; accepted_reasoning_efforts : string list option
    (** Optional subset of canonical reasoning effort values this model accepts
        (none / minimal / low / medium / high / xhigh). *)
  ; supports_response_format_json : bool option
  ; supports_structured_output : bool option
  ; supports_multimodal_inputs : bool option
  ; supports_image_input : bool option
  ; supports_audio_input : bool option
  ; supports_video_input : bool option
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; ignored_sampling_parameters : Capability_vocab.sampling_parameter list option
    (** Request sampling parameters that this manifest row declares must not be
        serialized. *)
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  ; thinking_control_format : string option
    (** Canonical thinking-wire format (none / thinking_object /
        thinking_object_adaptive / thinking_object_only / chat_template_kwargs /
        chat_template_token / reasoning_effort / enable_thinking); applied in
        {!Capabilities.apply_manifest_entry}. *)
  ; thinking_control_token : string option
    (** Exact chat-template token used when [thinking_control_format] is
        [chat_template_token]. *)
  ; preserve_thinking_control_format : string option
    (** Canonical historical reasoning preservation wire format (none /
        thinking_object_keep_all / chat_template_kwargs_preserve_thinking /
        top_level_preserve_thinking / always_preserved); applied in
        {!Capabilities.apply_manifest_entry}. *)
  ; reasoning_output_format : string option
    (** Canonical request-side reasoning output split control (none /
        split_reasoning_fields); applied in {!Capabilities.apply_manifest_entry}. *)
  ; reasoning_streaming_format : string option
    (** Canonical streaming reasoning side-channel (default / none /
        template_parser / delta:<field>); applied in
        {!Capabilities.apply_manifest_entry}. *)
  ; reasoning_replay : string option
    (** Optional multi-turn reasoning replay policy override (default /
        no_replay / drop_without_tool / preserve_always). *)
  }

(** A parsed capability manifest: an ordered list of model entries.

    Earlier entries take priority in {!lookup}. *)
type t = entry list

(** [of_json json] parses a manifest from a [Yojson.Safe.t] value.

    Returns [Error msg] when [schema_version] is missing or not 1,
    the root object contains an unknown field, a model entry contains
    an unknown field, a model entry is missing the required [id_prefix]
    field, or [base] names an unknown provider preset.  The
    non-operational [_comment] field is accepted at the root and entry
    levels. *)
val of_json : Yojson.Safe.t -> (t, string) result

(** [load_file path] reads and parses a manifest from the given file
    path.  Returns [Error msg] on I/O or JSON parse errors. *)
val load_file : string -> (t, string) result

(** [load_runtime_file path] reads a manifest for runtime use and emits
    operator diagnostics for success or failure. *)
val load_runtime_file : string -> t option

(** [lookup t model_id] returns the first entry whose [id_prefix] is a
    case-insensitive prefix of [model_id].  Returns [None] when no
    entry matches. *)
val lookup : t -> string -> entry option

(** The currently active manifest.

    Resolution order (highest priority first):
    + 1. Runtime override set by {!set_global} — embedding hosts
        (e.g. the embedding host loading its declarative manifest) install
        a programmatic manifest at boot.
    + 2. [OAS_CAPABILITY_MANIFEST] env var pointing at a JSON file
        (cached after first load when no runtime override is installed).

    Returns [None] when neither source supplies a manifest. *)
val global : unit -> t option

val preload_global : unit -> unit

(** [set_global m] installs [m] as the runtime-override manifest,
    shadowing any [OAS_CAPABILITY_MANIFEST]-loaded entries until
    {!clear_global} is called.

    catalog (e.g. the embedding host's declarative manifest) and want
    OAS to consume the same capability data without round-tripping
    through a JSON file.

    Safe under multi-domain concurrency via [Atomic.t] internally;
    concurrent [set_global]/[clear_global]/[global] are race-free but
    the resulting observed value is whichever set/clear was atomically
    most recent.

    @since 0.194.0 *)
val set_global : t -> unit

(** [clear_global ()] removes the runtime override and lets
    {!global} fall back to the env-var-loaded manifest (or [None]).

    @since 0.194.0 *)
val clear_global : unit -> unit
