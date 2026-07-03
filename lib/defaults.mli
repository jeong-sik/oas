(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding [OAS_*] environment variable is unset or empty.

    @stability Internal
    @since 0.93.1 *)

(** Read an environment variable, falling back to [default]
    if the variable is unset or empty. *)
val env_or : string -> string -> string

(** Read an integer environment variable, falling back to [default]
    if the variable is unset, empty, non-numeric, or negative. *)
val int_env_or : int -> string -> int

(** Read a float environment variable, falling back to [default]
    if the variable is unset, empty, non-numeric, or negative. *)
val float_env_or : float -> string -> float

(** Read a boolean environment variable, falling back to [default]
    if the variable is unset, empty, or invalid. *)
val bool_env_or : bool -> string -> bool

(** Local LLM server URL.
    Reads [OAS_LOCAL_LLM_URL] at call time, falling back to
    {!Llm_provider.Constants.Endpoints.default_url}. *)
val resolve_local_llm_url : unit -> string

(** Environment variable used by {!resolve_fallback_provider}. *)
val fallback_provider_env_var : string

(** Non-local default provider returned by {!resolve_fallback_provider} when
    {!fallback_provider_env_var} is unset or empty. Local providers remain opt-in via
    an explicit ["local"] provider value or [OAS_FALLBACK_PROVIDER=local]. *)
val default_fallback_provider : string

(** Fallback provider name.
    Reads {!fallback_provider_env_var} at call time, defaults to
    {!default_fallback_provider}. *)
val resolve_fallback_provider : unit -> string

(** Explicit gate for runtime-only test providers such as ["mock"] and ["echo"].
    Disabled by default; tests must opt in via [OAS_ALLOW_TEST_PROVIDERS]. *)
val allow_test_providers : unit -> bool

(** Default context reducer: repair dangling tool calls, prune old
    tool args, and drop thinking blocks. *)
val default_context_reducer : Context_reducer.t
