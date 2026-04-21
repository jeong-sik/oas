(** Lightweight provider configuration for standalone LLM calls.

    Decoupled from agent_state/agent_config. Consumers can
    construct this directly and pass it to {!Complete.complete}.

    @since 0.46.0

    @stability Internal
    @since 0.93.1 *)

(** Provider kind determines request/response wire format. *)
type provider_kind =
  | Anthropic
  | OpenAI_compat
  | Ollama  (** Ollama: OpenAI compat wire format + reasoning_effort + no tool_choice. @since 0.112.0 *)
  | Gemini
  | Glm  (** ZhipuAI GLM native: OpenAI wire format + JWT auth + GLM error parsing. @since 0.83.0 *)
  | Claude_code  (** Subprocess transport via [claude -p]. @since 0.78.0 *)
  | Gemini_cli  (** Subprocess transport via [gemini -p]. @since 0.133.0 *)
  | Codex_cli   (** Subprocess transport via [codex exec]. @since 0.133.0 *)

type t = {
  kind: provider_kind;
  model_id: string;
  base_url: string;
  api_key: string;
  headers: (string * string) list;
  request_path: string;
  max_tokens: int option;  (** [None] = resolve from model capabilities at request time. @since 0.123.0 *)
  max_context: int option;  (** Provider's context window limit in tokens. When set, downstream callers may truncate messages to fit before dispatch. @since 0.120.0 *)
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  system_prompt: string option;
  enable_thinking: bool option;
  thinking_budget: int option;
  clear_thinking: bool option;
  tool_stream: bool;
  tool_choice: Types.tool_choice option;
  disable_parallel_tool_use: bool;
  response_format: Types.response_format;
  output_schema: Yojson.Safe.t option;  (** Provider-native JSON schema output request. @since 0.163.0 *)
  cache_system_prompt: bool;
  supports_tool_choice_override: bool option;
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
}

(** Default config for quick construction. Only [kind], [model_id],
    [base_url], and [request_path] are required; rest use safe defaults. *)
val make :
  kind:provider_kind ->
  model_id:string ->
  base_url:string ->
  ?api_key:string ->
  ?headers:(string * string) list ->
  ?request_path:string ->
  ?max_tokens:int ->
  ?max_context:int ->
  ?temperature:float ->
  ?top_p:float ->
  ?top_k:int ->
  ?min_p:float ->
  ?system_prompt:string ->
  ?enable_thinking:bool ->
  ?thinking_budget:int ->
  ?clear_thinking:bool ->
  ?tool_stream:bool ->
  ?tool_choice:Types.tool_choice ->
  ?disable_parallel_tool_use:bool ->
  ?response_format:Types.response_format ->
  ?response_format_json:bool ->
  ?output_schema:Yojson.Safe.t ->
  ?cache_system_prompt:bool ->
  ?supports_tool_choice_override:bool ->
  unit -> t

(** Lowercase string representation of the wire-format kind.
    Returns the variant name in lowercase (e.g. [Anthropic] -> ["anthropic"]).
    Exhaustive match: adding a new variant triggers a compile error.
    @since 0.100.0 *)
val string_of_provider_kind : provider_kind -> string

(** Map thinking configuration fields to reasoning_effort string.
    Returns "none", "low", "medium", or "high".
    @since 0.114.0 *)
val effort_of_thinking_config :
  enable_thinking:bool option -> thinking_budget:int option -> string

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
    - [Gemini], [Anthropic], and [Ollama] are accepted.
    - [Glm] and CLI kinds are rejected.

    @since 0.163.0 *)
val validate_output_schema_request : t -> (unit, string) result

(** Whether the provider config points at a local loopback endpoint.
    This is the SSOT for locality checks derived from runtime configuration. *)
val is_local : t -> bool
