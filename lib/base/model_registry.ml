(** Model registry: alias resolution and model ID normalization.

    The single source of truth for model alias → canonical API ID mapping.
    New models are added here only.

    @stability Evolving
    @since 0.93.1 *)

let default_model_id =
  match Sys.getenv_opt "OAS_DEFAULT_MODEL" with
  | Some v -> v
  | None -> "agent_llm_a-sonnet-4-6-20250514"
;;

(** Resolve a model alias or short name to its full API model ID.
    Unknown strings pass through unchanged — this allows custom models. *)
let resolve_model_id = function
  | "agent_llm_a-opus-4-6" | "opus" -> "agent_llm_a-opus-4-6-20250514"
  | "agent_llm_a-sonnet-4-6" | "sonnet" -> "agent_llm_a-sonnet-4-6-20250514"
  | "agent_llm_a-opus-4-5" -> "agent_llm_a-opus-4-5-20251101"
  | "agent_llm_a-sonnet-4" -> "agent_llm_a-sonnet-4-20250514"
  | "agent_llm_a-haiku-4-5" | "haiku" -> "agent_llm_a-haiku-4-5-20251001"
  | "agent_llm_a-3-7-sonnet" -> "agent_llm_a-3-7-sonnet-20250219"
  | other -> other
;;
