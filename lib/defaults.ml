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

let fallback_provider =
  env_or "local-qwen" "OAS_FALLBACK_PROVIDER"

(** Default context reducer: repair dangling tool calls + prune old tool args.
    Applied automatically unless the user provides a custom reducer.
    Compose order: repair first (fix broken pairs), then prune (reduce tokens). *)
let default_context_reducer =
  Context_reducer.compose [
    Context_reducer.repair_dangling_tool_calls;
    Context_reducer.prune_tool_args ~max_arg_len:2000 ();
    Context_reducer.drop_thinking;
  ]
