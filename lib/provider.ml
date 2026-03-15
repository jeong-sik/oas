(** Provider abstraction for local and cloud LLM endpoints *)

type ollama_mode =
  | Chat
  | Generate

type provider =
  | Local of { base_url: string }
  | Anthropic
  | OpenAICompat of {
      base_url: string;
      auth_header: string option;
      path: string;
      static_token: string option;
    }
  | Ollama of { base_url: string; mode: ollama_mode }

type config = {
  provider: provider;
  model_id: string;
  api_key_env: string;
}

type request_kind =
  | Anthropic_messages
  | Openai_chat_completions
  | Ollama_chat
  | Ollama_generate

type capabilities = {
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_reasoning: bool;
  supports_response_format_json: bool;
  supports_multimodal_inputs: bool;
  supports_native_streaming: bool;
  supports_top_k: bool;
  supports_min_p: bool;
}

type model_spec = {
  provider: provider;
  model_id: string;
  api_key_env: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
}

let default_capabilities = {
  supports_tools = false;
  supports_tool_choice = false;
  supports_reasoning = false;
  supports_response_format_json = false;
  supports_multimodal_inputs = false;
  supports_native_streaming = false;
  supports_top_k = false;
  supports_min_p = false;
}

let anthropic_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_reasoning = true;
  supports_multimodal_inputs = true;
  supports_native_streaming = true;
}

let openai_chat_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_response_format_json = true;
  supports_multimodal_inputs = true;
}

let qwen_openai_chat_capabilities = {
  openai_chat_capabilities with
  supports_reasoning = true;
  supports_top_k = true;
  supports_min_p = true;
}

let string_contains = Util.string_contains

let is_qwen_family model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  string_contains ~needle:"qwen" normalized

let capabilities_for_model ~(provider : provider) ~(model_id : string) =
  match provider with
  | Anthropic | Local _ -> anthropic_capabilities
  | OpenAICompat _ ->
      if is_qwen_family model_id then qwen_openai_chat_capabilities
      else openai_chat_capabilities
  | Ollama { mode = Chat; _ } ->
      if is_qwen_family model_id then qwen_openai_chat_capabilities
      else openai_chat_capabilities
  | Ollama { mode = Generate; _ } -> default_capabilities

let request_kind = function
  | Local _ | Anthropic -> Anthropic_messages
  | OpenAICompat _ -> Openai_chat_completions
  | Ollama { mode = Chat; _ } -> Ollama_chat
  | Ollama { mode = Generate; _ } -> Ollama_generate

let request_path = function
  | Local _ | Anthropic -> "/v1/messages"
  | OpenAICompat { path; _ } -> path
  | Ollama { mode = Chat; _ } -> "/api/chat"
  | Ollama { mode = Generate; _ } -> "/api/generate"

let capabilities_for_config (cfg : config) =
  capabilities_for_model ~provider:cfg.provider ~model_id:cfg.model_id

let model_spec_of_config (cfg : config) =
  {
    provider = cfg.provider;
    model_id = cfg.model_id;
    api_key_env = cfg.api_key_env;
    request_kind = request_kind cfg.provider;
    request_path = request_path cfg.provider;
    capabilities = capabilities_for_config cfg;
  }

let resolve (cfg : config) =
  match cfg.provider with
  | Local { base_url } ->
    Ok (base_url, "dummy", [("Content-Type", "application/json")])
  | Anthropic ->
    (match Sys.getenv_opt cfg.api_key_env with
     | Some key -> Ok ("https://api.anthropic.com", key,
       [("x-api-key", key);
        ("anthropic-version", "2023-06-01");
        ("Content-Type", "application/json")])
     | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))
  | OpenAICompat { base_url; auth_header; static_token; _ } ->
    (match static_token with
     | Some key when String.trim key <> "" ->
         let headers =
           match auth_header with
           | Some header -> [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ]
           | None -> [ ("Content-Type", "application/json") ]
         in
         Ok (base_url, key, headers)
     | _ ->
         (match auth_header with
          | None ->
              Ok (base_url, "", [ ("Content-Type", "application/json") ])
          | Some header ->
              (match Sys.getenv_opt cfg.api_key_env with
               | Some key ->
                   Ok
                     ( base_url,
                       key,
                       [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ] )
               | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))))
  | Ollama { base_url; _ } ->
    Ok (base_url, "dummy", [("Content-Type", "application/json")])

let local_qwen () = {
  provider = Local { base_url = Defaults.local_qwen_url };
  model_id = "qwen3.5-35b-a3b-ud-q8-xl";
  api_key_env = "DUMMY_KEY";
}

let anthropic_sonnet () = {
  provider = Anthropic;
  model_id = "claude-sonnet-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_haiku () = {
  provider = Anthropic;
  model_id = "claude-haiku-4-5-20251001";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_opus () = {
  provider = Anthropic;
  model_id = "claude-opus-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let local_mlx () = {
  provider = Local { base_url = Defaults.local_mlx_url };
  model_id = "qwen3.5";
  api_key_env = "DUMMY_KEY";
}

let openrouter ?(model_id="anthropic/claude-sonnet-4-6") () = {
  provider = OpenAICompat {
    base_url = "https://openrouter.ai/api/v1";
    auth_header = Some "Authorization";
    path = "/chat/completions";
    static_token = None;
  };
  model_id;
  api_key_env = "OPENROUTER_API_KEY";
}

let ollama ?(base_url=Defaults.ollama_url) ?(model_id=Defaults.ollama_model)
    ?(mode=Chat) () = {
  provider = Ollama { base_url; mode };
  model_id;
  api_key_env = "DUMMY_KEY";
}

(* ── Cascade: multi-provider failover ──────────────────────────── *)

type cascade = {
  primary: config;
  fallbacks: config list;
}

let cascade ~primary ~fallbacks = { primary; fallbacks }

(* ── Pricing: per-model cost estimation ────────────────────────── *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
}

let pricing_for_model model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  if string_contains ~needle:"opus-4-6" normalized then
    { input_per_million = 15.0; output_per_million = 75.0 }
  else if string_contains ~needle:"opus-4-5" normalized then
    { input_per_million = 15.0; output_per_million = 75.0 }
  else if string_contains ~needle:"sonnet-4-6" normalized then
    { input_per_million = 3.0; output_per_million = 15.0 }
  else if string_contains ~needle:"sonnet-4" normalized then
    { input_per_million = 3.0; output_per_million = 15.0 }
  else if string_contains ~needle:"haiku-4-5" normalized then
    { input_per_million = 0.8; output_per_million = 4.0 }
  else if string_contains ~needle:"claude-3-7-sonnet" normalized then
    { input_per_million = 3.0; output_per_million = 15.0 }
  else
    { input_per_million = 0.0; output_per_million = 0.0 }

let estimate_cost ~(pricing : pricing) ~input_tokens ~output_tokens =
  let input_cost = Float.of_int input_tokens *. pricing.input_per_million /. 1_000_000.0 in
  let output_cost = Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0 in
  input_cost +. output_cost
