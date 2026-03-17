(** Per-model cost estimation.

    Extracted from OAS Provider module. Provides pricing lookup by model ID
    and USD cost calculation from token counts.

    @since 0.42.0 *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
  cache_write_multiplier: float;  (** cache creation tokens cost input_rate * this *)
  cache_read_multiplier: float;   (** cache read tokens cost input_rate * this *)
}

let string_contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    if index + needle_len > haystack_len then false
    else if String.sub haystack index needle_len = needle then true
    else loop (index + 1)
  in
  if needle_len = 0 then true else loop 0

let pricing_for_model model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Anthropic cache pricing: write = 1.25x input, read = 0.1x input.
     OpenAI/local: no cache pricing (multipliers are 1.0 for no-op). *)
  let anthropic_cache = (1.25, 0.1) in
  let no_cache = (1.0, 1.0) in
  let base, (cw, cr) =
    if string_contains ~needle:"opus-4-6" normalized then
      (15.0, 75.0), anthropic_cache
    else if string_contains ~needle:"opus-4-5" normalized then
      (15.0, 75.0), anthropic_cache
    else if string_contains ~needle:"sonnet-4-6" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"sonnet-4" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"haiku-4-5" normalized then
      (0.8, 4.0), anthropic_cache
    else if string_contains ~needle:"claude-3-7-sonnet" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"gpt-4o-mini" normalized then
      (0.15, 0.6), no_cache
    else if string_contains ~needle:"gpt-4o" normalized then
      (2.5, 10.0), no_cache
    else if string_contains ~needle:"gpt-4.1" normalized then
      (2.0, 8.0), no_cache
    else if string_contains ~needle:"o3-mini" normalized then
      (1.1, 4.4), no_cache
    else if string_contains ~needle:"ollama" normalized
         || string_contains ~needle:"qwen" normalized
         || string_contains ~needle:"llama" normalized then
      (0.0, 0.0), no_cache
    else
      (0.0, 0.0), no_cache
  in
  let input_per_million, output_per_million = base in
  { input_per_million; output_per_million;
    cache_write_multiplier = cw; cache_read_multiplier = cr }

let estimate_cost ~(pricing : pricing)
    ~input_tokens ~output_tokens
    ?(cache_creation_input_tokens=0) ?(cache_read_input_tokens=0) () =
  (* Regular input tokens (excluding cache tokens -- billed separately) *)
  let regular_input = input_tokens - cache_creation_input_tokens - cache_read_input_tokens in
  let regular_input = max 0 regular_input in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier in
  let output_cost = Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0 in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost
