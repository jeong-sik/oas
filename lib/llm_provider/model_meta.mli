(** Unified model metadata — consolidates capabilities, pricing, and
    locality into a single query point.

    Consumers use this instead of querying Capabilities, Pricing, and
    Provider_registry separately. Unknown models get conservative
    defaults (context_window=100K, is_local=false, zero cost).

    @since 0.100.0 *)

type t =
  { model_id : string
  ; capabilities : Capabilities.capabilities
  ; pricing : Pricing.pricing
  ; is_local : bool
    (** Whether the caller explicitly identified this model as local.
        Model IDs alone do not imply locality. The default is [false]
        unless the caller passes [~locality:`Local]. *)
  ; context_window : int
    (** Effective context window in tokens. Resolved from
        [capabilities.max_context_tokens], falling back to 100_000. *)
  ; max_output_tokens : int
    (** Max output tokens. Resolved from
        [capabilities.max_output_tokens], falling back to 4_096. *)
  ; cost_per_1k_input : float (** USD per 1K input tokens. *)
  ; cost_per_1k_output : float (** USD per 1K output tokens. *)
  }

(** Resolve model metadata by model ID.
    Always succeeds — unknown models get conservative defaults.
    [locality] defaults to [`Remote] because model IDs alone are not a
    trustworthy source of deployment locality. *)
val for_model_id : ?locality:[ `Local | `Remote ] -> string -> t

(** Resolve model metadata using provider-config locality as the SSOT. *)
val for_provider_config : Provider_config.t -> t

(** Like [for_model_id] but overrides [context_window] with a
    Discovery-provided [ctx_size] (e.g. from [GET /props]). *)
val for_model_id_with_ctx : ?locality:[ `Local | `Remote ] -> string -> ctx_size:int -> t

(** [true] when both input and output cost are zero. *)
val is_free : t -> bool

(** Shorthand for [t.context_window]. *)
val context_window : t -> int
