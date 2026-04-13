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

type t = {
  kind: provider_kind;
  model_id: string;
  base_url: string;
  api_key: string;
  headers: (string * string) list;
  request_path: string;
  max_tokens: int option;  (** [None] = resolve from model capabilities at request time. @since 0.123.0 *)
  max_context: int option;  (** Provider's context window limit in tokens. When set, cascade executor truncates messages to fit before dispatch. @since 0.120.0 *)
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
  response_format_json: bool;
  cache_system_prompt: bool;
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
  ?response_format_json:bool ->
  ?cache_system_prompt:bool ->
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

(** Whether the provider config points at a local loopback endpoint.
    This is the SSOT for locality checks derived from runtime configuration. *)
val is_local : t -> bool
