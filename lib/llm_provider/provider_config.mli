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
  | Kimi (** Kimi direct API: Anthropic-compatible [/v1/messages]. @since 0.169.0 *)
  | OpenAI_compat
  | Ollama
  (** Ollama: OpenAI_compat wire format + reasoning_effort + no tool_choice. @since 0.112.0 *)
  | Gemini
  | Glm
  (** ZhipuAI GLM native: OpenAI_compat wire format + JWT auth + GLM error parsing. @since 0.83.0 *)
  | DashScope

(** Default HTTP request path for a given provider kind.
    Single source of truth shared by [make] and direct record-literal
    callers; pin the [kind] and the [request_path] together via this
    helper to avoid the two fields drifting out of sync. *)
val request_path_default_for_kind : provider_kind -> string

val default_connect_timeout_s : provider_kind -> float
val default_stream_idle_timeout_s : provider_kind -> float

(** Derive [output_schema] from a [response_format].
    Returns [Some schema] when [response_format = JsonSchema schema]
    and [None] otherwise. With [?override:(Some s)] the helper
    short-circuits to [Some s], preserving the legacy semantics of
    [make]'s explicit schema argument. Use this helper to keep the
    [response_format] / [output_schema] pair consistent in record
    literals built outside [make]. *)
val output_schema_of_response_format
  :  ?override:Yojson.Safe.t
  -> Types.response_format
  -> Yojson.Safe.t option

type t =
  { kind : provider_kind
  ; model_id : string
  ; base_url : string
  ; api_key : Secret.t
    (** API key / token as an abstract secret.  Never log or serialize
        this field directly; use {!auth_headers_for_config} at HTTP request
        time. *)
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
  ; preserve_thinking : bool option
    (** Request historical reasoning preservation when the selected provider
        exposes a preserve toggle. The concrete wire field is selected by
        [Capabilities.preserve_thinking_control_format]: chat-template kwargs,
        top-level [preserve_thinking], [thinking.keep], or no request field for
        always-preserved models.
        @since 0.205.12 *)
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
      by other kinds. Profiles may surface this field to declare
      their own residency policy without a global env variable.
      @since 0.171.0 *)
  ; internal_model_rotation_count : int option
    (** Number of model attempts the subprocess CLI is configured to
      cycle through internally before yielding a final response.
      [None] = SDK has no opinion (the default for non-CLI providers
      and CLI providers that do not expose rotation visibility).

      Originally surfaced for [Codex], whose vendor binary cycles
      through 5 candidate models per [codex exec] invocation and
      returns only the final attempt's outcome. Without this hint a
      single CLI call appears as one provider attempt to the
      downstream observer, even though it can take ~180s
      worst-case (5 model retries with internal backoff). Consumers
      that want to render the rotation in traces or apply
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
      Profiles may surface this field so small-model configurations
      can pick a smaller window than long-context configurations.
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
  ; connect_timeout_s : float option
    (** Per-config override for the connect + initial-response-headers
      wall-clock timeout. See {!default_connect_timeout_s} for the
      kind-based default this overrides. [None] keeps the kind default
      ([Ollama] -> 600.0s, other kinds -> 60.0s). [Some s] forces [s]
      seconds for the connect/headers phase only — it is independent of
      the body deadline ([body_timeout_s]) and the inter-chunk stream-idle
      deadline ([stream_idle_timeout_s]).

      Use case (oas#2163, RFC-OAS-026 I2): an OpenAI-compatible endpoint
      whose upstream behaves like a queued/cloud model (long cold-start or
      admission wait before the first response header) needs a larger
      connect budget than the 60s cloud default, without changing its
      wire-format [kind]. The consumer declares the budget; OAS owns
      enforcement and [phase=Http_operation] attribution. No URL/string
      matching is performed on the SDK side.
      @since 0.207.9 *)
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
  -> ?preserve_thinking:bool
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
  -> ?connect_timeout_s:float
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

(** Canonical inverse of {!string_of_provider_kind}.

    Accepts every lowercase form produced by {!string_of_provider_kind}.

    The match is case-insensitive; leading and trailing whitespace is
    trimmed. Returns [None] for any other input so callers fail fast
    rather than silently falling back to a default provider.

    Use this instead of scattered ad-hoc string-to-kind matches to keep all
    string drift in one place.
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
    This is advisory metadata for orchestration layers; transports
    still apply their own lower-level connect/body/idle timeouts. Ollama has
    no default hard attempt timeout because local model load/generation can
    legitimately exceed fixed cloud-style budgets. *)
val default_attempt_timeout_s : provider_kind -> float option

(** OpenAI-compatible reasoning effort levels accepted on the wire.
    [reasoning_effort_to_string] is the only string serialization surface for
    these values. *)
type reasoning_effort = Reasoning_effort.t =
  | Minimal
  | Low
  | Medium
  | High
  | XHigh

val all_reasoning_efforts : reasoning_effort list
val reasoning_effort_to_string : reasoning_effort -> string
val reasoning_effort_of_string : string -> reasoning_effort option

(** Typed default reasoning effort.
    [getenv] exists for deterministic tests; production callers use the
    default {!Cli_common_env.get} boundary. *)
val default_reasoning_effort_value
  :  ?getenv:(string -> string option)
  -> unit
  -> reasoning_effort

(** Typed form of {!effort_of_thinking_config}. [None] means the request must
    omit [reasoning_effort] or expose the legacy ["none"] sentinel through the
    string compatibility wrapper. *)
val effort_of_thinking_config_value
  :  ?getenv:(string -> string option)
  -> enable_thinking:bool option
  -> thinking_budget:int option
  -> unit
  -> reasoning_effort option

(** Map thinking configuration fields to reasoning_effort string.
    Returns ["none"], one of the budget-derived levels ["low" | "medium" |
    "high"], or a supported [OAS_DEFAULT_REASONING_EFFORT] override
    ["minimal" | "low" | "medium" | "high" | "xhigh"].
    @since 0.114.0 *)
val effort_of_thinking_config
  :  enable_thinking:bool option
  -> thinking_budget:int option
  -> string

(** Typed provider request body value for OpenAI-compatible
    [reasoning_effort]. [None] means callers omit the field. Production
    serializers should prefer this over the string compatibility wrapper. *)
val reasoning_effort_request_value_typed
  :  enable_thinking:bool option
  -> thinking_budget:int option
  -> reasoning_effort option

(** Provider request body value for OpenAI-compatible [reasoning_effort].
    Returns [None] when thinking is disabled, unset, or resolved to ["none"],
    so callers omit the field instead of sending a provider-rejected sentinel.
    @since 0.206.5 *)
val reasoning_effort_request_value
  :  enable_thinking:bool option
  -> thinking_budget:int option
  -> string option

(** Resolve GLM [clear_thinking]: the explicit field, else the inverse of
    [preserve_thinking], else the API default [true]. SSOT for both request
    builders' clear_thinking handling. *)
val glm_clear_thinking_value
  :  clear_thinking:bool option
  -> preserve_thinking:bool option
  -> bool

val glm_clear_thinking : t -> bool

(** [true] iff GLM should replay prior-turn [reasoning_content] into request
    history: thinking active AND [clear_thinking] false (Preserved Thinking).
    Under the default [clear_thinking=true] the server discards prior reasoning,
    so replaying it violates the GLM contract and bloats the request. SSOT for
    every GLM message-serializer routing site. *)
val glm_should_replay_reasoning_fields
  :  enable_thinking:bool option
  -> clear_thinking:bool option
  -> preserve_thinking:bool option
  -> bool

val glm_should_replay_reasoning : t -> bool

(** Compute reasoning_effort for a provider config.
    Returns [None] for non-Ollama providers.
    @since 0.114.0 *)
val reasoning_effort_of_config : t -> string option

(** Capability catalog provider namespace for [config].

    This is usually {!string_of_provider_kind}, except official Ollama Cloud
    endpoints are scoped as ["ollama_cloud"] even when they use the
    OpenAI-compatible wire kind. *)
val capability_provider_label : t -> string

(** Resolve model capabilities using provider-qualified catalog entries first,
    then bare model entries. *)
val capabilities_for_config_model : t -> Capabilities.capabilities option

(** True when [config] targets Z.AI GLM semantics, either through the native
    [Glm] kind or an OpenAI-compatible Z.AI endpoint with a GLM model id. *)
val is_zai_glm_config : t -> bool

(** Derive a provider-safe schema name for native structured-output APIs
    that require one (for example Openai's [json_schema.name]). *)
val structured_output_name_of_schema : Yojson.Safe.t -> string

(** Validate whether [output_schema] can be sent natively for this config.
    Returns [Ok ()] when no schema was requested or when the provider kind
    is wired for native schema output. Returns [Error reason] for
    unsupported provider/model combinations so callers can fail fast
    before making an HTTP request.

    Conservative policy:
    - [OpenAI_compat] is accepted only for official Openai hosts with a
      model capability record that reports [supports_structured_output].
    - [Gemini], [Anthropic], [Ollama], and [DashScope] are accepted.
      DashScope (DashScope) exposes [response_format.json_schema] on its
      OpenAI-compatible endpoint; the field is forwarded by
      [backend_openai.ml] without additional host validation.
    - [Kimi] is rejected until native json_schema support is verified.
    - [Glm] is rejected: Z.AI's current official docs document JSON mode
      ([json_object]) only; [response_format.json_schema] is not listed.
    - CLI kinds are rejected.

    @since 0.163.0 *)
val validate_output_schema_request : t -> (unit, string) result

(** True when [request_path] targets the OpenAI Responses API item-based wire
    format rather than Chat Completions. *)
val request_path_targets_responses_api : string -> bool

(** Validate that [request_path] names a wire format implemented by this
    provider kind. OpenAI Responses API paths require [OpenAI_compat] and use a
    Responses-specific sync serializer/parser. *)
val validate_request_path : t -> (unit, string) result

(** Validate that sampling parameters unsupported by CLI subprocess
    transports ([min_p], [top_k]) are not set.
    Returns [Error] with the unsupported parameter names for CLI providers.
    @since 0.185.0 *)
val validate_cli_sampling_params : t -> (unit, string) result

(** Validate provider-specific [tool_choice] constraints before request-body
    construction. This catches unsupported runtime/provider contracts at the
    typed config boundary instead of letting serializers raise exceptions after
    a turn has started. *)
type tool_choice_request_rejection =
  | Unsupported_named_tool_choice of
      { provider_kind : provider_kind
      ; model_id : string
      ; tool_name : string
      }

val tool_choice_request_rejection_to_message : tool_choice_request_rejection -> string
val validate_tool_choice_request_typed : t -> (unit, tool_choice_request_rejection) result

val validate_tool_choice_request_with_capabilities
  :  provider_kind:provider_kind
  -> model_id:string
  -> tool_choice:Types.tool_choice option
  -> Capabilities.capabilities
  -> (unit, tool_choice_request_rejection) result

val validate_tool_choice_request : t -> (unit, string) result

(** Whether the provider config points at a local loopback endpoint.
    This is the SSOT for locality checks derived from runtime configuration. *)
val is_local : t -> bool

(** Return only the auth-specific headers for a config.
    Callers merge this into [config.headers] at HTTP request time so that
    [Provider_config.t.headers] never carries sensitive tokens like API keys.
    Gemini keys are sent in the [x-goog-api-key] header and are never placed
    in the URL query string. *)
val auth_headers_for_config : t -> (string * string) list

(** Same as {!auth_headers_for_config} but takes the provider kind and raw key
    as separate arguments.  Used by the legacy {!Api.create_message} path so it
    does not need to construct a full [Provider_config.t] just to compute auth
    headers. *)
val auth_headers_for_kind_and_key
  :  kind:provider_kind
  -> api_key:string
  -> (string * string) list
