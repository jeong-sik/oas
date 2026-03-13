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

let resolve cfg =
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
  provider = Local { base_url = "http://127.0.0.1:8085" };
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
  provider = Local { base_url = "http://127.0.0.1:3033" };
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

let ollama ?(base_url="http://127.0.0.1:11434") ?(model_id="glm-4.7-flash")
    ?(mode=Chat) () = {
  provider = Ollama { base_url; mode };
  model_id;
  api_key_env = "DUMMY_KEY";
}
