(** Model registry: alias resolution and model ID normalization.

    The single source of truth for model alias → canonical API ID mapping.
    New models are added here only.

    @stability Evolving
    @since 0.93.1 *)

(** The default model ID, overridable via OAS_DEFAULT_MODEL env var. *)
val default_model_id : string

(** Resolve a model alias or short name to its full API model ID.
    Unknown strings pass through unchanged (custom model support). *)
val resolve_model_id : string -> string
