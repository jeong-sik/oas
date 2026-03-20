(** Default configuration constants with environment variable overrides.

    Each value falls back to a compile-time default when the
    corresponding [OAS_*] environment variable is unset or empty. *)

(** {1 Helpers} *)

(** Return [Sys.getenv var] if non-empty, otherwise [default]. *)
val env_or : string -> string -> string

(** {1 Endpoint defaults} *)

val local_llm_url : string
val fallback_provider : string

(** {1 Context reducer} *)

(** Default context reducer: repair dangling tool calls, prune old tool
    args, and drop thinking blocks.  Applied automatically unless the
    user provides a custom reducer. *)
val default_context_reducer : Context_reducer.t
