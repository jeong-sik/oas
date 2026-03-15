(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding OAS_* environment variable is unset or empty. *)

let env_or default var =
  match Sys.getenv_opt var with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default

let local_qwen_url =
  env_or "http://127.0.0.1:8085" "OAS_LOCAL_QWEN_URL"

let local_mlx_url =
  env_or "http://127.0.0.1:3033" "OAS_LOCAL_MLX_URL"

let ollama_url =
  env_or "http://127.0.0.1:11434" "OAS_OLLAMA_URL"

let ollama_model =
  env_or "glm-4.7-flash" "OAS_OLLAMA_MODEL"

let fallback_provider =
  env_or "local-qwen" "OAS_FALLBACK_PROVIDER"
