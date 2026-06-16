(** Model registry: alias resolution and model ID normalization.

    The single source of truth for model alias → canonical API ID mapping.
    New models are added here only.

    @stability Evolving
    @since 0.93.1 *)

let default_model_id =
  match Sys.getenv_opt "OAS_DEFAULT_MODEL" with
  | Some v -> v
  | None -> "claude-sonnet-4-6-20250514"
;;

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
