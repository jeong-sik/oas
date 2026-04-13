(** Lightweight provider configuration for standalone LLM calls.
    @since 0.46.0 *)

type provider_kind =
  | Anthropic
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | Claude_code

type t = {
  kind: provider_kind;
  model_id: string;
  base_url: string;
  api_key: string;
  headers: (string * string) list;
  request_path: string;
  max_tokens: int option;
  max_context: int option;
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

let make ~kind ~model_id ~base_url
    ?(api_key="") ?(headers=[("Content-Type", "application/json")])
    ?request_path ?max_tokens ?max_context
    ?temperature ?top_p ?top_k ?min_p
    ?system_prompt ?enable_thinking ?thinking_budget
    ?clear_thinking ?(tool_stream=false)
    ?tool_choice ?(disable_parallel_tool_use=false)
    ?(response_format_json=false)
    ?(cache_system_prompt=false) () =
  let request_path = match request_path with
    | Some p -> p
    | None -> match kind with
      | Anthropic -> "/v1/messages"
      | OpenAI_compat -> "/v1/chat/completions"
      | Ollama -> "/api/chat"
      | Gemini -> ""
      | Glm -> "/chat/completions"
      | Claude_code -> ""
  in
  { kind; model_id; base_url; api_key; headers; request_path;
    max_tokens; max_context; temperature; top_p; top_k; min_p;
    system_prompt; enable_thinking; thinking_budget; clear_thinking;
    tool_stream;
    tool_choice; disable_parallel_tool_use; response_format_json;
    cache_system_prompt }

(** Lowercase string representation of the wire-format kind.
    Exhaustive match: adding a new variant triggers a compile error.
    @since 0.100.0 *)
let string_of_provider_kind = function
  | Anthropic -> "anthropic"
  | OpenAI_compat -> "openai_compat"
  | Ollama -> "ollama"
  | Gemini -> "gemini"
  | Glm -> "glm"
  | Claude_code -> "claude_code"

(** Map thinking configuration to reasoning_effort string.
    Four levels: "none", "low" (≤2048), "medium" (≤8192), "high" (>8192).
    Shared by Ollama backends and api_openai request building.
    @since 0.114.0 *)
let effort_of_thinking_config ~(enable_thinking : bool option)
    ~(thinking_budget : int option) : string =
  match enable_thinking with
  | Some false | None -> "none"
  | Some true ->
      match thinking_budget with
      | Some n when n <= 0 -> "none"
      | Some n when n <= 2048 -> "low"
      | Some n when n <= 8192 -> "medium"
      | Some _ -> "high"
      | None -> "medium"

(** Compute reasoning_effort for a provider config.
    Returns [None] for non-Ollama providers.
    @since 0.114.0 *)
let reasoning_effort_of_config (config : t) : string option =
  match config.kind with
  | Ollama -> Some (effort_of_thinking_config
                      ~enable_thinking:config.enable_thinking
                      ~thinking_budget:config.thinking_budget)
  | _ -> None

let has_host_prefix ~url ~prefix =
  let prefix_len = String.length prefix in
  String.length url >= prefix_len
  && String.sub url 0 prefix_len = prefix
  &&
  let next_index = prefix_len in
  String.length url = prefix_len
  || match url.[next_index] with
     | ':' | '/' | '?' | '#' -> true
     | _ -> false

let is_local (config : t) =
  let url = String.lowercase_ascii (String.trim config.base_url) in
  has_host_prefix ~url ~prefix:Constants.Endpoints.local_prefix
  || has_host_prefix ~url ~prefix:Constants.Endpoints.localhost_prefix
