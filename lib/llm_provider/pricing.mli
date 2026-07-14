(** Catalog-backed cost observation.

    Pricing is sourced exclusively from {!Model_catalog}.  Unknown models and
    catalog rows with incomplete pricing remain explicit [None]; they are never
    classified as free. *)

type pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

(** Return the complete price declared by the active model catalog. Missing
    input, output, or cache rates remain [None]; the SDK does not synthesize
    cache multipliers. *)
val pricing_for_model_opt : string -> pricing option

(** Estimate USD cost from an explicit pricing value and provider token usage. *)
val estimate_cost
  :  pricing:pricing
  -> input_tokens:int
  -> output_tokens:int
  -> ?cache_creation_input_tokens:int
  -> ?cache_read_input_tokens:int
  -> unit
  -> float

(** Fill [usage.cost_usd] only when both catalog pricing and provider usage are
    available.  An absent price remains absent. *)
val annotate_usage_cost : model_id:string -> Types.api_usage -> Types.api_usage

(** Apply {!annotate_usage_cost} to a response usage record. *)
val annotate_response_cost : Types.api_response -> Types.api_response
