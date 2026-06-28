(** Model registry: alias resolution and model ID normalization.

    The single source of truth for model alias → canonical API ID mapping.
    New models are added here only.

    @stability Evolving
    @since 0.93.1 *)

let default_model_id_env_var = "OAS_DEFAULT_MODEL"
let default_model_id_fallback = "claude-sonnet-4-6-20250514"

let default_model_id_value ?(getenv = Llm_provider.Cli_common_env.get) () =
  match getenv default_model_id_env_var with
  | Some v ->
    let v = String.trim v in
    if String.equal v "" then default_model_id_fallback else v
  | None -> default_model_id_fallback
;;

let default_model_id = default_model_id_value ()

(** Resolve a model alias or short name to its full API model ID.
    Unknown strings pass through unchanged — this allows custom models. *)
let resolve_model_id = function
  | "claude-opus-4-6" | "opus" -> "claude-opus-4-6-20250514"
  | "claude-sonnet-4-6" | "sonnet" -> "claude-sonnet-4-6-20250514"
  | "claude-opus-4-5" -> "claude-opus-4-5-20251101"
  | "claude-sonnet-4" -> "claude-sonnet-4-20250514"
  | "claude-haiku-4-5" | "haiku" -> "claude-haiku-4-5-20251001"
  | "claude-3-7-sonnet" -> "claude-3-7-sonnet-20250219"
  | other -> other
;;
