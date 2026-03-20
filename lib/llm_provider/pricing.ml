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

[@@@coverage off]
(* === Inline tests === *)

let close_enough a b = Float.abs (a -. b) < 1e-9

(* --- string_contains --- *)

let%test "string_contains: empty needle matches anything" =
  string_contains ~needle:"" "hello"

let%test "string_contains: empty needle matches empty haystack" =
  string_contains ~needle:"" ""

let%test "string_contains: exact match" =
  string_contains ~needle:"hello" "hello"

let%test "string_contains: prefix match" =
  string_contains ~needle:"hel" "hello"

let%test "string_contains: suffix match" =
  string_contains ~needle:"llo" "hello"

let%test "string_contains: middle match" =
  string_contains ~needle:"ell" "hello"

let%test "string_contains: no match" =
  not (string_contains ~needle:"xyz" "hello")

let%test "string_contains: needle longer than haystack" =
  not (string_contains ~needle:"hello world" "hello")

let%test "string_contains: case sensitive" =
  not (string_contains ~needle:"HELLO" "hello")

(* --- pricing_for_model: Anthropic models --- *)

let%test "pricing opus-4-6" =
  let p = pricing_for_model "claude-opus-4-6-20250514" in
  close_enough p.input_per_million 15.0
  && close_enough p.output_per_million 75.0
  && close_enough p.cache_write_multiplier 1.25
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing opus-4-5" =
  let p = pricing_for_model "claude-opus-4-5-20251101" in
  close_enough p.input_per_million 15.0
  && close_enough p.output_per_million 75.0

let%test "pricing sonnet-4-6" =
  let p = pricing_for_model "claude-sonnet-4-6-20250514" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_write_multiplier 1.25
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing sonnet-4 (non-4-6)" =
  let p = pricing_for_model "claude-sonnet-4-20250514" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0

let%test "pricing haiku-4-5" =
  let p = pricing_for_model "claude-haiku-4-5-20251001" in
  close_enough p.input_per_million 0.8
  && close_enough p.output_per_million 4.0

let%test "pricing claude-3-7-sonnet" =
  let p = pricing_for_model "claude-3-7-sonnet-20250219" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_write_multiplier 1.25

(* --- pricing_for_model: OpenAI models --- *)

let%test "pricing gpt-4o-mini" =
  let p = pricing_for_model "gpt-4o-mini" in
  close_enough p.input_per_million 0.15
  && close_enough p.output_per_million 0.6
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 1.0

let%test "pricing gpt-4o (not mini)" =
  let p = pricing_for_model "gpt-4o" in
  close_enough p.input_per_million 2.5
  && close_enough p.output_per_million 10.0

let%test "pricing gpt-4.1" =
  let p = pricing_for_model "gpt-4.1-turbo" in
  close_enough p.input_per_million 2.0
  && close_enough p.output_per_million 8.0

let%test "pricing o3-mini" =
  let p = pricing_for_model "o3-mini" in
  close_enough p.input_per_million 1.1
  && close_enough p.output_per_million 4.4

(* --- pricing_for_model: local/free models --- *)

let%test "pricing ollama is free" =
  let p = pricing_for_model "ollama/llama3" in
  close_enough p.input_per_million 0.0
  && close_enough p.output_per_million 0.0

let%test "pricing qwen is free" =
  let p = pricing_for_model "qwen3.5-35b" in
  close_enough p.input_per_million 0.0

let%test "pricing llama is free" =
  let p = pricing_for_model "llama-3.1-70b" in
  close_enough p.input_per_million 0.0

let%test "pricing unknown model is free" =
  let p = pricing_for_model "some-random-model" in
  close_enough p.input_per_million 0.0
  && close_enough p.output_per_million 0.0

(* --- pricing_for_model: case insensitivity --- *)

let%test "pricing case insensitive" =
  let p = pricing_for_model "Claude-Opus-4-6" in
  close_enough p.input_per_million 15.0

let%test "pricing whitespace trimmed" =
  let p = pricing_for_model "  claude-sonnet-4-6  " in
  close_enough p.input_per_million 3.0

(* --- estimate_cost --- *)

let%test "estimate_cost: zero tokens is zero" =
  let p = pricing_for_model "claude-opus-4-6" in
  close_enough (estimate_cost ~pricing:p ~input_tokens:0 ~output_tokens:0 ()) 0.0

let%test "estimate_cost: 1M input tokens opus" =
  let p = pricing_for_model "claude-opus-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:1_000_000 ~output_tokens:0 () in
  close_enough cost 15.0

let%test "estimate_cost: 1M output tokens opus" =
  let p = pricing_for_model "claude-opus-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:0 ~output_tokens:1_000_000 () in
  close_enough cost 75.0

let%test "estimate_cost: mixed input and output" =
  let p = pricing_for_model "claude-sonnet-4-6" in
  let cost = estimate_cost ~pricing:p ~input_tokens:1000 ~output_tokens:500 () in
  (* 1000 * 3.0/1M + 500 * 15.0/1M = 0.003 + 0.0075 = 0.0105 *)
  close_enough cost 0.0105

let%test "estimate_cost: with cache write tokens" =
  let p = pricing_for_model "claude-opus-4-6" in
  (* 1000 input, 500 cache write, 0 cache read, 0 output *)
  (* regular_input = 1000 - 500 - 0 = 500 *)
  (* input_cost = 500 * 15/1M = 0.0075 *)
  (* cache_write = 500 * 15/1M * 1.25 = 0.009375 *)
  let cost = estimate_cost ~pricing:p ~input_tokens:1000 ~output_tokens:0
    ~cache_creation_input_tokens:500 () in
  close_enough cost (0.0075 +. 0.009375)

let%test "estimate_cost: with cache read tokens" =
  let p = pricing_for_model "claude-opus-4-6" in
  (* 1000 input, 0 cache write, 200 cache read, 0 output *)
  (* regular_input = 1000 - 0 - 200 = 800 *)
  (* input_cost = 800 * 15/1M = 0.012 *)
  (* cache_read = 200 * 15/1M * 0.1 = 0.0003 *)
  let cost = estimate_cost ~pricing:p ~input_tokens:1000 ~output_tokens:0
    ~cache_read_input_tokens:200 () in
  close_enough cost (0.012 +. 0.0003)

let%test "estimate_cost: regular_input clamped to zero when cache exceeds total" =
  let p = pricing_for_model "claude-sonnet-4-6" in
  (* input_tokens=100 but cache_creation=200: regular = max 0 (100-200) = 0 *)
  let cost = estimate_cost ~pricing:p ~input_tokens:100 ~output_tokens:0
    ~cache_creation_input_tokens:200 () in
  (* only cache_write cost: 200 * 3/1M * 1.25 = 0.00075 *)
  close_enough cost 0.00075

let%test "estimate_cost: free model is always zero" =
  let p = pricing_for_model "qwen3.5" in
  let cost = estimate_cost ~pricing:p ~input_tokens:1_000_000 ~output_tokens:1_000_000
    ~cache_creation_input_tokens:500_000 ~cache_read_input_tokens:500_000 () in
  close_enough cost 0.0
