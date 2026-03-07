(** Provider abstraction for local and cloud LLM endpoints *)

type provider =
  | Local of { base_url: string }
  | Anthropic
  | OpenAICompat of { base_url: string; auth_header: string }

type config = {
  provider: provider;
  model_id: string;
  api_key_env: string;
}

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
     | None -> Error (Printf.sprintf "Missing env var: %s" cfg.api_key_env))
  | OpenAICompat { base_url; auth_header } ->
    (match Sys.getenv_opt cfg.api_key_env with
     | Some key -> Ok (base_url, key,
       [(auth_header, "Bearer " ^ key);
        ("Content-Type", "application/json")])
     | None -> Error (Printf.sprintf "Missing env var: %s" cfg.api_key_env))

let local_qwen () = {
  provider = Local { base_url = "http://127.0.0.1:3034" };
  model_id = "qwen3.5";
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
    auth_header = "Authorization";
  };
  model_id;
  api_key_env = "OPENROUTER_API_KEY";
}
