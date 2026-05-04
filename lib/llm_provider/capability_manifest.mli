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
    runtime.  When the env var is unset the manifest layer is
    transparent — [Capabilities.for_model_id] falls through to the
    built-in static table exactly as before.

    Priority (highest first):
    + 1. Manifest entry matching by [id_prefix] (case-insensitive prefix)
    + 2. Built-in [Capabilities.for_model_id] prefix table
    + 3. Discovery-based inference / caller-provided default

    @since 0.188.0 *)

(** One entry in the capability manifest.

    [id_prefix] is matched as a case-insensitive prefix against the
    model ID being looked up.  [base] names a provider preset from
    {!Capabilities.capabilities_for_provider_label} (e.g.
    ["openai_chat"], ["anthropic"]); when absent or unrecognised the
    built-in [default_capabilities] is used.

    All other fields are optional overrides: a [None] value means
    "inherit from the base", a [Some v] value replaces the base value. *)
type entry =
  { id_prefix : string
  ; base_label : string option
      (** Provider preset label, e.g. ["openai_chat"] or ["anthropic"]. *)
  ; max_context_tokens : int option  (** [None] = inherit from base. *)
  ; max_output_tokens : int option  (** [None] = inherit from base. *)
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
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
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  }

(** A parsed capability manifest: an ordered list of model entries.

    Earlier entries take priority in {!lookup}. *)
type t = entry list

(** [of_json json] parses a manifest from a [Yojson.Safe.t] value.

    Returns [Error msg] when [schema_version] is missing or not 1,
    or when a model entry is missing the required [id_prefix] field. *)
val of_json : Yojson.Safe.t -> (t, string) result

(** [load_file path] reads and parses a manifest from the given file
    path.  Returns [Error msg] on I/O or JSON parse errors. *)
val load_file : string -> (t, string) result

(** [lookup t model_id] returns the first entry whose [id_prefix] is a
    case-insensitive prefix of [model_id].  Returns [None] when no
    entry matches. *)
val lookup : t -> string -> entry option

(** The globally loaded manifest (lazy, loaded once on first use).

    Reads [OAS_CAPABILITY_MANIFEST] env var on first call.  Returns
    [None] when the variable is unset or when the file cannot be
    loaded (a warning is emitted via {!Diag} in that case). *)
val global : unit -> t option
