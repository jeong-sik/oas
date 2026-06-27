(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding OAS_* environment variable is unset or empty. *)

let env_or = Util.env_or
let int_env_or default var = Llm_provider.Cli_common_env.int ~default var
let float_env_or default var = Llm_provider.Cli_common_env.float ~default var
let bool_env_or default var = Llm_provider.Cli_common_env.bool ~default var
let local_llm_url = Llm_provider.Discovery.default_endpoint
let fallback_provider = env_or "local" "OAS_FALLBACK_PROVIDER"
let allow_test_providers () = bool_env_or false "OAS_ALLOW_TEST_PROVIDERS"

(** Default context reducer: repair dangling tool calls + prune old tool args.
    Applied automatically unless the user provides a custom reducer.
    Compose order: repair first (fix broken pairs), then prune (reduce tokens). *)
let default_context_reducer =
  Context_reducer.compose
    [ Context_reducer.repair_dangling_tool_calls
    ; Context_reducer.repair_orphaned_tool_results
    ; Context_reducer.prune_tool_args ~max_arg_len:2000 ()
    ; Context_reducer.drop_thinking
    ]
;;
