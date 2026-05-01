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
  match Sys.getenv_opt var with
  | Some raw ->
    let trimmed = String.trim raw in
    (match int_of_string_opt trimmed with
     | Some v when v > 0 -> v
     | _ ->
       if trimmed <> ""
       then warn_invalid_env ~var ~raw:trimmed ~expected:"positive integer";
       default)
  | None -> default
;;

let float_env_or default var =
  match Sys.getenv_opt var with
  | Some raw ->
    let trimmed = String.trim raw in
    (match float_of_string_opt trimmed with
     | Some v when v > 0.0 -> v
     | _ ->
       if trimmed <> "" then warn_invalid_env ~var ~raw:trimmed ~expected:"positive float";
       default)
  | None -> default
;;

let bool_env_or default var =
  match Sys.getenv_opt var with
  | Some raw ->
    let trimmed = String.trim raw in
    (match String.lowercase_ascii trimmed with
     | "1" | "true" | "yes" | "on" -> true
     | "0" | "false" | "no" | "off" -> false
     | _ ->
       if trimmed <> "" then warn_invalid_env ~var ~raw:trimmed ~expected:"boolean";
       default)
  | None -> default
;;

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
