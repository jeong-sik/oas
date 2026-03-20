(** Lightweight provider configuration for standalone LLM calls.

    Decoupled from agent_state/agent_config. Both OAS and MASC can
    construct this directly and pass it to {!Complete.complete}.

    @since 0.46.0 *)

(** Provider kind determines request/response wire format. *)
type provider_kind =
  | Anthropic
  | OpenAI_compat
  | Gemini
  | Claude_code  (** Subprocess transport via [claude -p]. @since 0.78.0 *)

type t = {
  kind: provider_kind;
  model_id: string;
  base_url: string;
  api_key: string;
  headers: (string * string) list;
  request_path: string;
  max_tokens: int;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  system_prompt: string option;
  enable_thinking: bool option;
  thinking_budget: int option;
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
  ?temperature:float ->
  ?top_p:float ->
  ?top_k:int ->
  ?min_p:float ->
  ?system_prompt:string ->
  ?enable_thinking:bool ->
  ?thinking_budget:int ->
  ?tool_choice:Types.tool_choice ->
  ?disable_parallel_tool_use:bool ->
  ?response_format_json:bool ->
  ?cache_system_prompt:bool ->
  unit -> t
