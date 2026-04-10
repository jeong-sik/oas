(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding [OAS_*] environment variable is unset or empty.

    @stability Internal
    @since 0.93.1 *)

(** Read an environment variable, falling back to [default]
    if the variable is unset or empty. *)
val env_or : string -> string -> string

(** Read an integer environment variable, falling back to [default]
    if the variable is unset, empty, non-numeric, or non-positive. *)
val int_env_or : int -> string -> int

(** Read a float environment variable, falling back to [default]
    if the variable is unset, empty, non-numeric, or non-positive. *)
val float_env_or : float -> string -> float

(** Read a boolean environment variable, falling back to [default]
    if the variable is unset, empty, or invalid. *)
val bool_env_or : bool -> string -> bool

(** Local LLM server URL.
    Reads [OAS_LOCAL_LLM_URL], falling back to [OAS_LOCAL_QWEN_URL],
    then {!Llm_provider.Constants.Endpoints.default_url}. *)
val local_llm_url : string

(** Fallback provider name.
    Reads [OAS_FALLBACK_PROVIDER], defaults to ["local"]. *)
val fallback_provider : string

(** Explicit gate for runtime-only test providers such as ["mock"] and ["echo"].
    Disabled by default; tests must opt in via [OAS_ALLOW_TEST_PROVIDERS]. *)
val allow_test_providers : unit -> bool

(** Default context reducer: repair dangling tool calls, prune old
    tool args, and drop thinking blocks. *)
val default_context_reducer : Context_reducer.t
