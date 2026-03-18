(** Model registry: alias resolution and model ID normalization.

    This is the single place where vendor-specific model names are mapped
    to canonical API model IDs. New models are added here only. *)

let env_or default var =
  match Sys.getenv_opt var with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default

let default_model_id =
  env_or "claude-sonnet-4-6-20250514" "OAS_DEFAULT_MODEL"

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
