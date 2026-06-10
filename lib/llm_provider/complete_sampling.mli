(** Sampling parameter defaults and Gemini URL construction.

    Extracted from {!Complete} to keep the main completion module
    focused on request/response lifecycle.

    @since 0.205.9 *)

(** {1 Gemini URL Construction} *)

(** Construct Gemini API URL with model_id in path and optional key param.
    Exposed for testing. *)
val gemini_url : config:Provider_config.t -> stream:bool -> string

(** {1 Provider Sampling Defaults} *)

(** Sampling parameter defaults per provider kind. *)
type sampling_defaults =
  { default_min_p : float option
  ; default_top_p : float option
  ; default_top_k : int option
  }

(** Get default sampling parameters for a provider kind.
    Local (OpenAI_compat) providers get min_p=0.05.
    Anthropic/Gemini get no defaults (all None). *)
val provider_sampling_defaults : Provider_config.provider_kind -> sampling_defaults

(** Apply provider defaults to a config, preserving explicit values.
    Only fills in [None] fields; explicit values are never overwritten.
    For [OpenAI_compat], [min_p] is auto-filled only when the target
    model (or an unknown localhost endpoint) supports it. *)
val apply_sampling_defaults : Provider_config.t -> Provider_config.t

(** Compute the reasoning_effort string that was sent for the given config.
    Delegates to {!Provider_config.reasoning_effort_of_config}. *)
val reasoning_effort_of_config : Provider_config.t -> string option
