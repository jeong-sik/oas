(** Unified model metadata — consolidates capabilities, pricing, and
    locality into a single query point.

    Consumers use this instead of querying Capabilities, Pricing, and
    Provider_registry separately. Unknown models get conservative
    defaults (context_window=100K, is_local=false, zero cost).

    @since 0.100.0 *)

type t = {
  model_id : string;
  capabilities : Capabilities.capabilities;
  pricing : Pricing.pricing;
  is_local : bool;
    (** Whether the model likely runs locally (llama.cpp, Ollama, etc.).
        Determined by known local model name fragments (substring match)
        AND zero pricing (both input and output).
        Unknown models default to [false] (conservative). *)
  context_window : int;
    (** Effective context window in tokens. Resolved from
        [capabilities.max_context_tokens], falling back to 100_000. *)
  max_output_tokens : int;
    (** Max output tokens. Resolved from
        [capabilities.max_output_tokens], falling back to 4_096. *)
  cost_per_1k_input : float;
    (** USD per 1K input tokens. *)
  cost_per_1k_output : float;
    (** USD per 1K output tokens. *)
}

val for_model_id : string -> t
(** Resolve model metadata by model ID.
    Always succeeds — unknown models get conservative defaults. *)

val for_model_id_with_ctx : string -> ctx_size:int -> t
(** Like [for_model_id] but overrides [context_window] with a
    Discovery-provided [ctx_size] (e.g. from [GET /props]). *)

val is_free : t -> bool
(** [true] when both input and output cost are zero. *)

val context_window : t -> int
(** Shorthand for [t.context_window]. *)
