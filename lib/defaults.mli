(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding [OAS_*] environment variable is unset or empty.

    @stability Internal
    @since 0.93.1 *)

(** Read an environment variable, falling back to [default]
    if the variable is unset or empty. *)
val env_or : string -> string -> string

(** Local LLM server URL.
    Reads [OAS_LOCAL_LLM_URL], falling back to [OAS_LOCAL_QWEN_URL],
    then ["http://127.0.0.1:8085"]. *)
val local_llm_url : string

(** Fallback provider name.
    Reads [OAS_FALLBACK_PROVIDER], defaults to ["local"]. *)
val fallback_provider : string

(** Default context reducer: repair dangling tool calls, prune old
    tool args, and drop thinking blocks. *)
val default_context_reducer : Context_reducer.t
