(** Lightweight provider configuration for standalone LLM calls.
    @since 0.46.0 *)

type provider_kind =
  | Anthropic
  | OpenAI_compat
  | Gemini

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

let make ~kind ~model_id ~base_url
    ?(api_key="") ?(headers=[("Content-Type", "application/json")])
    ?request_path ?(max_tokens=4096)
    ?temperature ?top_p ?top_k ?min_p
    ?system_prompt ?enable_thinking ?thinking_budget
    ?tool_choice ?(disable_parallel_tool_use=false)
    ?(response_format_json=false)
    ?(cache_system_prompt=false) () =
  let request_path = match request_path with
    | Some p -> p
    | None -> match kind with
      | Anthropic -> "/v1/messages"
      | OpenAI_compat -> "/v1/chat/completions"
      | Gemini -> ""
  in
  { kind; model_id; base_url; api_key; headers; request_path;
    max_tokens; temperature; top_p; top_k; min_p;
    system_prompt; enable_thinking; thinking_budget;
    tool_choice; disable_parallel_tool_use; response_format_json;
    cache_system_prompt }
