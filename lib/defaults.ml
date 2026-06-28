(** Default configuration constants with environment variable overrides.

    Each value falls back to the compile-time default when the
    corresponding OAS_* environment variable is unset or empty. *)

let _log = Log.create ~module_name:"defaults" ()

let warn_invalid_env ~var ~raw ~expected =
  Log.warn
    _log
    "invalid environment override; using default"
    [ Log.S ("var", var); Log.S ("raw", raw); Log.S ("expected", expected) ]
;;

let env_or = Util.env_or

let int_env_or default var =
  Llm_provider.Cli_common_env.int
    ~on_invalid:(fun { var; raw; expected } -> warn_invalid_env ~var ~raw ~expected)
    ~default
    var
;;

let float_env_or default var =
  Llm_provider.Cli_common_env.float
    ~on_invalid:(fun { var; raw; expected } -> warn_invalid_env ~var ~raw ~expected)
    ~default
    var
;;

let bool_env_or default var =
  Llm_provider.Cli_common_env.bool
    ~on_invalid:(fun { var; raw; expected } -> warn_invalid_env ~var ~raw ~expected)
    ~default
    var
;;

let fallback_provider_env_var = "OAS_FALLBACK_PROVIDER"
let default_fallback_provider = "local"
let resolve_local_llm_url () = Llm_provider.Discovery.resolve_default_endpoint ()

let resolve_fallback_provider () =
  env_or default_fallback_provider fallback_provider_env_var
  |> String.trim
  |> String.lowercase_ascii
;;

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
