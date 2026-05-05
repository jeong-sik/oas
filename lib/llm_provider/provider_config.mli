(** Lightweight provider configuration for standalone LLM calls.

    Decoupled from agent_state/agent_config. Consumers can
    construct this directly and pass it to {!Complete.complete}.

    @since 0.46.0

    @stability Internal
    @since 0.93.1 *)

(** Provider kind determines request/response wire format.

    Re-exported from {!Provider_kind} — the underlying type now lives there so
    it can be shared with {!Types} without creating a dependency cycle. *)
type provider_kind = Provider_kind.t =
  | Anthropic
  | Kimi (** Kimi Code direct API: Anthropic-compatible [/v1/messages]. @since 0.169.0 *)
  | OpenAI_compat
  | Ollama
  (** Ollama: OpenAI compat wire format + reasoning_effort + no tool_choice. @since 0.112.0 *)
  | Gemini
  | Glm
  (** ZhipuAI GLM native: OpenAI wire format + JWT auth + GLM error parsing. @since 0.83.0 *)
  | DashScope
  | Claude_code (** Subprocess transport via [claude -p]. @since 0.78.0 *)
  | Gemini_cli (** Subprocess transport via [gemini -p]. @since 0.133.0 *)
  | Kimi_cli (** Subprocess transport via [kimi --print]. @since 0.169.0 *)
  | Codex_cli (** Subprocess transport via [codex exec]. @since 0.133.0 *)

(** Default HTTP request path for a given provider kind, returning
    [""] for CLI subprocess kinds that do not dispatch over a path.
    Single source of truth shared by [make] and direct record-literal
    callers; pin the [kind] and the [request_path] together via this
    helper to avoid the two fields drifting out of sync. *)
val request_path_default_for_kind : provider_kind -> string

type t =
  { kind : provider_kind
  ; model_id : string
  ; base_url : string
  ; api_key : string
  ; headers : (string * string) list
  ; request_path : string
  ; max_tokens : int option
    (** [None] = resolve from model capabilities at request time. @since 0.123.0 *)
  ; max_context : int option
    (** Provider's context window limit in tokens. When set, downstream callers may truncate messages to fit before dispatch. @since 0.120.0 *)
  ; temperature : float option
  ; top_p : float option
  ; top_k : int option
  ; min_p : float option
  ; system_prompt : string option
  ; enable_thinking : bool option
  ; thinking_budget : int option
  ; clear_thinking : bool option
  ; tool_stream : bool
  ; tool_choice : Types.tool_choice option
  ; disable_parallel_tool_use : bool
  ; response_format : Types.response_format
  ; output_schema : Yojson.Safe.t option
    (** Provider-native JSON schema output request. @since 0.163.0 *)
  ; cache_system_prompt : bool
  ; supports_tool_choice_override : bool option
    (** Override the registry default for [supports_tool_choice].
      [None] = use the per-kind default from {!Capabilities}.
      [Some b] = force [b].

      Kept on this low-level config so downstream callers (e.g. declaring
      per-entry capability facts in their own config file) can inject
      a verified model-side support flag without the SDK matching on
      [model_id]. The SDK stays model-agnostic; the consumer declares.

      Design principle: declaration-over-probing. The SDK does not run
      any capability probe against the provider endpoint (Ollama's
      [/api/show] exposes no authoritative tool_choice flag; LiteLLM
      encodes this in a static JSON table and has the same blind spot).
      Instead of guessing from model_id substrings, the consumer owns
      the policy and declares it.

      @since 0.150.0 *)
  ; keep_alive : string option
    (** Ollama [keep_alive] request field. Accepted values: integer
      seconds ({"-1"}, {"0"}, {"3600"}) or duration strings ({"5m"},
      {"30m"}, {"24h"}). [None] falls back to the
      [OAS_OLLAMA_KEEP_ALIVE] env var, then to the SDK default
      ({"-1"}, permanent). Honored only by the Ollama backend; ignored
      by other kinds. Cascade configs may surface this so a profile
      can declare its own residency policy without a global env
      variable.
      @since 0.171.0 *)
  ; internal_model_rotation_count : int option
    (** Number of model attempts the subprocess CLI is configured to
      cycle through internally before yielding a final response.
      [None] = SDK has no opinion (the default for non-CLI providers
      and CLI providers that do not expose rotation visibility).

      Originally surfaced for [Codex_cli], whose vendor binary cycles
      through 5 candidate models per [codex exec] invocation and
      returns only the final attempt's outcome. Without this hint a
      single CLI call appears as one provider attempt to the
      downstream cascade observer, even though it can take ~180s
      worst-case (5 model retries with internal backoff). Consumers
      that want to render the rotation in cascade traces or apply
      a per-attempt timeout budget can read this hint instead of
      hard-coding a Codex-specific constant.

      The SDK does not enforce or schedule the rotation; it remains
      the CLI binary's responsibility. This field is purely
      declarative metadata so the consumer can reason about the
      worst-case behaviour of one [Complete.complete] call.

      Honored only as an advisory hint; ignored for non-CLI kinds.
      @since 0.182.0 *)
  ; num_ctx : int option
    (** Ollama [num_ctx] option. Per-request context window allocation
      in tokens. Drives KV cache RAM allocation. [None] leaves the
      field unset so Ollama uses its own default (Modelfile or 4096).
      Honored only by the Ollama backend; ignored by other kinds.
      Cascade configs may surface this so a small-model profile can
      pick a smaller window than a long-context profile.
      @since 0.171.0 *)
  ; seed : int option
    (** Deterministic seed for providers that support it. When [Some n],
      injected into the request body as ["seed": n] if the model's
      {!Capabilities.t.supports_seed} is [true]. [None] = use the
      [OAS_DEFAULT_SEED] env var, then fallback to
      {!Constants.Deterministic.default_seed} (42).
      Anthropic (Claude) does not support seed — the field is silently
      ignored for that provider.
      @since 0.185.0 *)
  }

(** Default config for quick construction. Only [kind], [model_id],
    [base_url], and [request_path] are required; rest use safe defaults. *)
val make
  :  kind:provider_kind
  -> model_id:string
  -> base_url:string
  -> ?api_key:string
  -> ?headers:(string * string) list
  -> ?request_path:string
  -> ?max_tokens:int
  -> ?max_context:int
  -> ?temperature:float
  -> ?top_p:float
  -> ?top_k:int
  -> ?min_p:float
  -> ?system_prompt:string
  -> ?enable_thinking:bool
  -> ?thinking_budget:int
  -> ?clear_thinking:bool
  -> ?tool_stream:bool
  -> ?tool_choice:Types.tool_choice
  -> ?disable_parallel_tool_use:bool
  -> ?response_format:Types.response_format
  -> ?response_format_json:bool
  -> ?output_schema:Yojson.Safe.t
  -> ?cache_system_prompt:bool
  -> ?supports_tool_choice_override:bool
  -> ?keep_alive:string
  -> ?internal_model_rotation_count:int
  -> ?num_ctx:int
  -> ?seed:int
  -> unit
  -> t

(** Lowercase string representation of the wire-format kind.
    Returns the variant name in lowercase (e.g. [Anthropic] -> ["anthropic"]).
    Exhaustive match: adding a new variant triggers a compile error.
    @since 0.100.0 *)
val string_of_provider_kind : provider_kind -> string

(** All provider kinds in canonical order. Re-export of
    {!Provider_kind.all} — see that module's docs for intent.
    @since 0.166.0 *)
val all_provider_kinds : provider_kind list

(** Conventional API key env var name per kind. Re-export of
    {!Provider_kind.default_api_key_env}. Returns [None] for kinds
    that do not have a universally-agreed env var (local / transport-
    mediated / OpenAI-compatible spaces where the env name is
    consumer-specified).
    @since 0.166.0 *)
val default_api_key_env : provider_kind -> string option

(** Re-export of {!Provider_kind.is_subprocess_cli}. [true] for the
    four CLI-subprocess kinds (Claude_code, Gemini_cli, Kimi_cli,
    Codex_cli); [false] for direct-HTTP kinds.
    @since 0.170.0 *)
val is_subprocess_cli : provider_kind -> bool

(** Canonical inverse of {!string_of_provider_kind}.

    Accepts every lowercase form produced by {!string_of_provider_kind} plus
    the documented legacy aliases used by cascade configs and callers:
    - [claude]  -> [Anthropic]
    - [openai]  -> [OpenAI_compat]
    - [llama]   -> [Ollama]

    The match is case-insensitive; leading and trailing whitespace is
    trimmed. Returns [None] for any other input so callers fail fast
    rather than silently falling back to a default provider.

    Use this instead of scattered ad-hoc [match s with "claude" -> ...]
    ladders to keep all string-to-kind drift in one place.
    @since 0.165.0 *)
val provider_kind_of_string : string -> provider_kind option

(** {1 Serializers}

    Hand-written to emit the wire-format produced by
    {!string_of_provider_kind} (for example ["anthropic"]) rather than the
    capitalised constructor name that [\[@@deriving yojson\]] would default
    to (["Anthropic"]).

    Records that embed [provider_kind] (for example
    [Types.inference_telemetry]) can therefore add it to a derived-yojson
    record without breaking the current on-disk / over-the-wire format.
    @since 0.165.0 *)

val pp_provider_kind : Format.formatter -> provider_kind -> unit
val show_provider_kind : provider_kind -> string
val provider_kind_to_yojson : provider_kind -> Yojson.Safe.t

val provider_kind_of_yojson
  :  Yojson.Safe.t
  -> provider_kind Ppx_deriving_yojson_runtime.error_or

(** Provider-internal hard cap for subprocess/native turn budgets.
    Returns [None] when OAS has no provider-specific hard ceiling. *)
val max_turns_hard_cap : provider_kind -> int option

(** Clamp a requested per-provider turn budget to the provider's hard cap.
    Providers without a hard cap return the requested value unchanged. *)
val clamp_max_turns : provider_kind -> int -> int

(** Provider-specific wall-clock budget hint for one provider attempt.
    This is advisory metadata for cascade/orchestration layers; transports
    still apply their own lower-level connect/body/idle timeouts. *)
val default_attempt_timeout_s : provider_kind -> float option

(** Map thinking configuration fields to reasoning_effort string.
    Returns "none", "low", "medium", or "high".
    @since 0.114.0 *)
val effort_of_thinking_config
  :  enable_thinking:bool option
  -> thinking_budget:int option
  -> string

(** Compute reasoning_effort for a provider config.
    Returns [None] for non-Ollama providers.
    @since 0.114.0 *)
val reasoning_effort_of_config : t -> string option

(** Derive a provider-safe schema name for native structured-output APIs
    that require one (for example OpenAI's [json_schema.name]). *)
val structured_output_name_of_schema : Yojson.Safe.t -> string

(** Validate whether [output_schema] can be sent natively for this config.
    Returns [Ok ()] when no schema was requested or when the provider kind
    is wired for native schema output. Returns [Error reason] for
    unsupported provider/model combinations so callers can fail fast
    before making an HTTP request.

    Conservative policy:
    - [OpenAI_compat] is accepted only for official OpenAI hosts with a
      model capability record that reports [supports_structured_output].
    - [Gemini], [Anthropic], [Ollama], and [DashScope] are accepted.
      DashScope (Qwen) exposes [response_format.json_schema] on its
      OpenAI-compatible endpoint; the field is forwarded by
      [backend_openai.ml] without additional host validation.
    - [Kimi] is rejected until native json_schema support is verified.
    - [Glm] is rejected: Z.AI's current official docs document JSON mode
      ([json_object]) only; [response_format.json_schema] is not listed.
    - CLI kinds are rejected.

    @since 0.163.0 *)
val validate_output_schema_request : t -> (unit, string) result

(** Validate that sampling parameters unsupported by CLI subprocess
    transports ([min_p], [top_k]) are not set.
    Returns [Error] with the unsupported parameter names for CLI providers.
    @since 0.185.0 *)
val validate_cli_sampling_params : t -> (unit, string) result

(** Whether the provider config points at a local loopback endpoint.
    This is the SSOT for locality checks derived from runtime configuration. *)
val is_local : t -> bool
