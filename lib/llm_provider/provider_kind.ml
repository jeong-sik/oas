(** Provider kind sum type, split from [Provider_config] so it can be shared
    by {!Types.inference_telemetry} without introducing a
    [Provider_config <-> Types] dependency cycle.

    [Provider_config] re-exports this type and its helpers so existing
    callers that refer to [Llm_provider.Provider_config.Anthropic] /
    [Provider_config.string_of_provider_kind] keep working unchanged.

    @since 0.165.0 *)

type t =
  | Anthropic
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | Claude_code
  | Gemini_cli
  | Codex_cli

let to_string = function
  | Anthropic -> "anthropic"
  | OpenAI_compat -> "openai_compat"
  | Ollama -> "ollama"
  | Gemini -> "gemini"
  | Glm -> "glm"
  | Claude_code -> "claude_code"
  | Gemini_cli -> "gemini_cli"
  | Codex_cli -> "codex_cli"

let of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "anthropic" | "claude" -> Some Anthropic
  | "openai_compat" | "openai" -> Some OpenAI_compat
  | "ollama" | "llama" -> Some Ollama
  | "gemini" -> Some Gemini
  | "glm" -> Some Glm
  | "claude_code" -> Some Claude_code
  | "gemini_cli" -> Some Gemini_cli
  | "codex_cli" -> Some Codex_cli
  | _ -> None

let pp fmt k = Format.pp_print_string fmt (to_string k)

let show = to_string

let to_yojson (k : t) : Yojson.Safe.t = `String (to_string k)

let of_yojson (json : Yojson.Safe.t) :
  t Ppx_deriving_yojson_runtime.error_or =
  match json with
  | `String s ->
    (match of_string s with
     | Some k -> Ok k
     | None -> Error (Printf.sprintf "provider_kind: unknown value %S" s))
  | _ -> Error "provider_kind: expected JSON string"
