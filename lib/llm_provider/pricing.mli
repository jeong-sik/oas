(** Per-model cost estimation. *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
  cache_write_multiplier: float;
  cache_read_multiplier: float;
}

(** Substring match helper. *)
val string_contains : needle:string -> string -> bool

(** Look up pricing for a model ID (case-insensitive, prefix-matched). *)
val pricing_for_model : string -> pricing

(** Estimate USD cost from token counts. *)
val estimate_cost :
  pricing:pricing ->
  input_tokens:int ->
  output_tokens:int ->
  ?cache_creation_input_tokens:int ->
  ?cache_read_input_tokens:int ->
  unit -> float

(** Estimate cost for a usage record from its model ID. *)
val estimate_usage_cost :
  model_id:string ->
  Types.api_usage -> float

(** Fill [usage.cost_usd] using the pricing table when absent. *)
val annotate_usage_cost :
  model_id:string ->
  Types.api_usage -> Types.api_usage

(** Fill [response.usage.cost_usd] using [response.model] when absent. *)
val annotate_response_cost :
  Types.api_response -> Types.api_response
