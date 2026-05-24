(** Provider kind sum type, split from [Provider_config] so it can be shared
    by {!Types.inference_telemetry} without introducing a
    [Provider_config <-> Types] dependency cycle.

    [Provider_config] re-exports this type and its helpers so existing
    callers that refer to [Llm_provider.Provider_config.Provider_a] /
    [Provider_config.string_of_provider_kind] keep working unchanged.

    @since 0.165.0 *)

type t =
  | Provider_a
  | Provider_c
  | Provider_d_compat
  | Ollama
  | Provider_f
  | Provider_k
  | Provider_h
  | Cli_tool_d
  | Cli_tool_b
  | Cli_tool_c
  | Cli_tool_a

let to_string = function
  | Provider_a -> "provider_a"
  | Provider_c -> "provider_c"
  | Provider_d_compat -> "provider_d_compat"
  | Ollama -> "ollama"
  | Provider_f -> "provider_f"
  | Provider_k -> "provider_k"
  | Provider_h -> "provider_h"
  | Cli_tool_d -> "cli_tool_d"
  | Cli_tool_b -> "cli_tool_b"
  | Cli_tool_c -> "cli_tool_c"
  | Cli_tool_a -> "cli_tool_a"
;;

let all : t list =
  [ Provider_a
  ; Provider_c
  ; Provider_d_compat
  ; Ollama
  ; Provider_f
  ; Provider_k
  ; Provider_h
  ; Cli_tool_d
  ; Cli_tool_b
  ; Cli_tool_c
  ; Cli_tool_a
  ]
;;

let default_api_key_env = function
  | Provider_a -> Some "PROVIDER_A_API_KEY"
  | Provider_c -> Some "PROVIDER_C_API_KEY"
  | Provider_f -> Some "PROVIDER_F_API_KEY"
  | Provider_k -> Some "ZAI_API_KEY"
  | Provider_h -> Some "PROVIDER_H_API_KEY"
  | Provider_d_compat | Ollama | Cli_tool_d | Cli_tool_b | Cli_tool_c | Cli_tool_a ->
    (* Ollama Cloud uses the same wire kind. The named provider entry
       prefers OLLAMA_CLOUD_API_KEY and falls back to OLLAMA_API_KEY. *)
    None
;;

let is_subprocess_cli = function
  | Cli_tool_d | Cli_tool_b | Cli_tool_c | Cli_tool_a -> true
  | Provider_a
  | Provider_c
  | Provider_d_compat
  | Ollama
  | Provider_f
  | Provider_k
  | Provider_h -> false
;;

let of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "provider_a" | "agent_llm_a" -> Some Provider_a
  | "provider_c" -> Some Provider_c
  | "provider_d_compat" | "provider_d" -> Some Provider_d_compat
  | "ollama" | "provider_n" | "ollama_cloud" -> Some Ollama
  | "provider_f" -> Some Provider_f
  | "provider_k" -> Some Provider_k
  | "provider_h" -> Some Provider_h
  | "cli_tool_d" -> Some Cli_tool_d
  | "cli_tool_b" -> Some Cli_tool_b
  | "cli_tool_c" -> Some Cli_tool_c
  | "cli_tool_a" -> Some Cli_tool_a
  | _ -> None
;;

let pp fmt k = Format.pp_print_string fmt (to_string k)
let show = to_string
let to_yojson (k : t) : Yojson.Safe.t = `String (to_string k)

let of_yojson (json : Yojson.Safe.t) : t Ppx_deriving_yojson_runtime.error_or =
  match json with
  | `String s ->
    (match of_string s with
     | Some k -> Ok k
     | None -> Error (Printf.sprintf "provider_kind: unknown value %S" s))
  | _ -> Error "provider_kind: expected JSON string"
;;
