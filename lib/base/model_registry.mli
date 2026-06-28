(** Model registry: alias resolution and model ID normalization.

    The single source of truth for model alias → canonical API ID mapping.
    New models are added here only.

    @stability Evolving
    @since 0.93.1 *)

(** Environment variable consulted by {!default_model_id_value}. *)
val default_model_id_env_var : string

(** Compile-time fallback when {!default_model_id_env_var} is unset or empty. *)
val default_model_id_fallback : string

(** Resolve the default model ID at call time.

    [getenv] exists for deterministic tests and for callers that already carry
    an explicit environment boundary. Production callers use
    {!Llm_provider.Cli_common_env.get}. *)
val default_model_id_value : ?getenv:(string -> string option) -> unit -> string

(** Compatibility snapshot of the default model ID.
    Prefer {!default_model_id_value} when the value must reflect environment
    changes after module initialization. *)
val default_model_id : string

(** Resolve a model alias or short name to its full API model ID.
    Unknown strings pass through unchanged (custom model support). *)
val resolve_model_id : string -> string
