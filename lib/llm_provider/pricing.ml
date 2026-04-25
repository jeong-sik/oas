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

let pricing_for_model_opt model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Anthropic cache pricing: write = 1.25x input, read = 0.1x input.
     Newer OpenAI text models expose cached input at 0.1x input.
     Local/free models keep no-op cache multipliers. *)
  let anthropic_cache = (1.25, 0.1) in
  let openai_cached_input = (1.0, 0.1) in
  let no_cache = (1.0, 1.0) in
  let result =
    if string_contains ~needle:"opus-4-6" normalized then
      Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"opus-4-5" normalized then
      Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4-6" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"haiku-4-5" normalized then
      Some ((0.8, 4.0), anthropic_cache)
    else if string_contains ~needle:"claude-3-7-sonnet" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    (* claude_code provider alias fallback. The Claude Code transport
       surfaces telemetry.model_used as the alias (e.g. "claude_code:auto",
       "cc:default") instead of the canonical model id returned by the
       Anthropic response, so substring matches against opus/sonnet/haiku
       above never fire. Estimate at sonnet-4-6 rates as the modal
       Anthropic backend; per-call accuracy is a follow-up that should
       resolve the canonical id from the API response. *)
    else if string_contains ~needle:"claude_code" normalized
         || string_contains ~needle:"cc:" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    (* OpenAI API text-token pricing, confirmed from official model docs
       2026-04-25. GPT-5.3-Codex-Spark is intentionally not covered here:
       its Codex rate card labels it research preview with non-final rates. *)
    else if string_contains ~needle:"gpt-5.3-codex-spark" normalized then
      None
    else if string_contains ~needle:"gpt-5.5" normalized then
      Some ((5.0, 30.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.4-mini" normalized then
      Some ((0.75, 4.5), openai_cached_input)
    else if string_contains ~needle:"gpt-5.4" normalized then
      Some ((2.5, 15.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.3-codex" normalized then
      Some ((1.75, 14.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.2" normalized then
      Some ((1.75, 14.0), openai_cached_input)
    else if string_contains ~needle:"gpt-4o-mini" normalized then
      Some ((0.15, 0.6), no_cache)
    else if string_contains ~needle:"gpt-4o" normalized then
      Some ((2.5, 10.0), no_cache)
    else if string_contains ~needle:"gpt-4.1" normalized then
      Some ((2.0, 8.0), no_cache)
    else if string_contains ~needle:"o3-mini" normalized then
      Some ((1.1, 4.4), no_cache)
    (* Gemini 3-계 preview. Source: ai.google.dev/gemini-api/docs/pricing,
       confirmed 2026-04-16. Google also exposes context caching with a
       per-hour storage surcharge ($1.00/h flash, $4.50/h pro); the
       pricing record cannot represent time-based storage, so we keep
       cache multipliers at no_cache and rely on provider-reported
       cost_usd for exact billing. Estimates here are an upper bound on
       input/output token cost only. *)
    else if string_contains ~needle:"gemini-3-flash-preview" normalized then
      Some ((0.50, 3.0), no_cache)
    else if string_contains ~needle:"gemini-3.1-pro-preview" normalized
         || string_contains ~needle:"gemini-3.1-pro" normalized then
      (* Standard tier (input <= 200k tokens). Above 200k Google charges
         2x ($4 input / $18 output). The pricing record has no context
         window size field, so cost for >200k inputs is underestimated
         by 2x. Follow-up: extend the record with tiered pricing. *)
      Some ((2.0, 12.0), no_cache)
    else if string_contains ~needle:"gemini-3.1-flash-lite-preview" normalized
         || string_contains ~needle:"gemini-3.1-flash-lite" normalized then
      Some ((0.25, 1.5), no_cache)
    else if string_contains ~needle:"ollama" normalized
         || string_contains ~needle:"qwen" normalized
         || string_contains ~needle:"llama" normalized then
      Some ((0.0, 0.0), no_cache)
    else
      None
  in
  match result with
  | Some ((input_per_million, output_per_million), (cw, cr)) ->
    Some { input_per_million; output_per_million;
           cache_write_multiplier = cw; cache_read_multiplier = cr }
  | None -> None

let zero_pricing =
  { input_per_million = 0.0; output_per_million = 0.0;
    cache_write_multiplier = 1.0; cache_read_multiplier = 1.0 }

let pricing_for_model model_id =
  Option.value ~default:zero_pricing (pricing_for_model_opt model_id)

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

let estimate_usage_cost ~model_id (usage : Types.api_usage) =
  let pricing = pricing_for_model model_id in
  estimate_cost ~pricing
    ~input_tokens:usage.input_tokens
    ~output_tokens:usage.output_tokens
    ~cache_creation_input_tokens:usage.cache_creation_input_tokens
    ~cache_read_input_tokens:usage.cache_read_input_tokens
    ()

let annotate_usage_cost ~model_id (usage : Types.api_usage) =
  match usage.cost_usd with
  | Some _ -> usage
  | None ->
    match pricing_for_model_opt model_id with
    | Some pricing ->
      let cost = estimate_cost ~pricing
        ~input_tokens:usage.input_tokens
        ~output_tokens:usage.output_tokens
        ~cache_creation_input_tokens:usage.cache_creation_input_tokens
        ~cache_read_input_tokens:usage.cache_read_input_tokens () in
      { usage with cost_usd = Some cost }
    | None ->
      usage  (* unknown model: leave cost_usd as None *)

let annotate_response_cost (response : Types.api_response) =
  let usage =
    Option.map (annotate_usage_cost ~model_id:response.model) response.usage
  in
  match usage with
  | None -> response
  | Some usage -> { response with usage = Some usage }

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

let%test "pricing gpt-5.5" =
  let p = pricing_for_model "gpt-5.5" in
  close_enough p.input_per_million 5.0
  && close_enough p.output_per_million 30.0
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing gpt-5.4-mini" =
  let p = pricing_for_model "gpt-5.4-mini" in
  close_enough p.input_per_million 0.75
  && close_enough p.output_per_million 4.5
  && close_enough p.cache_write_multiplier 1.0
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing gpt-5.4" =
  let p = pricing_for_model "gpt-5.4" in
  close_enough p.input_per_million 2.5
  && close_enough p.output_per_million 15.0
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing gpt-5.3-codex" =
  let p = pricing_for_model "gpt-5.3-codex" in
  close_enough p.input_per_million 1.75
  && close_enough p.output_per_million 14.0
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing gpt-5.2" =
  let p = pricing_for_model "gpt-5.2" in
  close_enough p.input_per_million 1.75
  && close_enough p.output_per_million 14.0
  && close_enough p.cache_read_multiplier 0.1

let%test "pricing gpt-5.3-codex-spark remains unknown" =
  pricing_for_model_opt "gpt-5.3-codex-spark" = None

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

(* --- pricing_for_model: Gemini 3-계 preview (2026-04-16) --- *)

let%test "pricing gemini-3-flash-preview" =
  let p = pricing_for_model "gemini-3-flash-preview" in
  close_enough p.input_per_million 0.50
  && close_enough p.output_per_million 3.0

let%test "pricing gemini-3.1-pro-preview" =
  let p = pricing_for_model "gemini-3.1-pro-preview" in
  close_enough p.input_per_million 2.0
  && close_enough p.output_per_million 12.0

let%test "pricing gemini-3.1-pro (bare id)" =
  let p = pricing_for_model "gemini-3.1-pro" in
  close_enough p.input_per_million 2.0
  && close_enough p.output_per_million 12.0

let%test "pricing gemini-3.1-flash-lite-preview" =
  let p = pricing_for_model "gemini-3.1-flash-lite-preview" in
  close_enough p.input_per_million 0.25
  && close_enough p.output_per_million 1.5

(* --- pricing_for_model: claude_code alias fallback --- *)

let%test "pricing claude_code:auto falls back to sonnet-4-6 rates" =
  let p = pricing_for_model "claude_code:auto" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0

let%test "pricing claude_code (bare alias)" =
  let p = pricing_for_model "claude_code" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0

let%test "pricing cc: short alias falls back to sonnet-4-6 rates" =
  let p = pricing_for_model "cc:default" in
  close_enough p.input_per_million 3.0
  && close_enough p.output_per_million 15.0

let%test "pricing_for_model_opt returns Some for gemini-3-flash-preview" =
  match pricing_for_model_opt "gemini-3-flash-preview" with
  | Some p -> p.input_per_million > 0.0
  | None -> false

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

let%test "pricing_for_model: unknown model falls back to zero" =
  let p = pricing_for_model "some-random-model" in
  close_enough p.input_per_million 0.0
  && close_enough p.output_per_million 0.0

(* --- pricing_for_model_opt: distinguishes unknown from free --- *)

let%test "pricing_for_model_opt: known cloud model returns Some" =
  match pricing_for_model_opt "claude-opus-4-6" with
  | Some p -> p.input_per_million > 0.0
  | None -> false

let%test "pricing_for_model_opt: known local model returns Some with zero pricing" =
  match pricing_for_model_opt "ollama/llama3" with
  | Some p -> close_enough p.input_per_million 0.0
  | None -> false

let%test "pricing_for_model_opt: qwen returns Some" =
  match pricing_for_model_opt "qwen3.5-35b" with
  | Some _ -> true
  | None -> false

let%test "pricing_for_model_opt: unknown model returns None" =
  match pricing_for_model_opt "some-random-model" with
  | Some _ -> false
  | None -> true

let%test "pricing_for_model_opt: cloud-style unknown returns None" =
  match pricing_for_model_opt "future-cloud-provider/fancy-model-v9" with
  | Some _ -> false
  | None -> true

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

let%test "annotate_usage_cost fills missing cost for known model" =
  let usage : Types.api_usage = {
    input_tokens = 1_000;
    output_tokens = 500;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
    cost_usd = None;
  } in
  match annotate_usage_cost ~model_id:"claude-sonnet-4-6" usage with
  | { cost_usd = Some cost; _ } -> cost > 0.0
  | _ -> false

let%test "annotate_usage_cost leaves cost_usd None for unknown model" =
  let usage : Types.api_usage = {
    input_tokens = 1_000;
    output_tokens = 500;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
    cost_usd = None;
  } in
  match annotate_usage_cost ~model_id:"totally-unknown-cloud-model" usage with
  | { cost_usd = None; _ } -> true
  | _ -> false

let%test "annotate_usage_cost fills zero cost for known free model" =
  let usage : Types.api_usage = {
    input_tokens = 1_000;
    output_tokens = 500;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
    cost_usd = None;
  } in
  match annotate_usage_cost ~model_id:"qwen3.5-35b" usage with
  | { cost_usd = Some cost; _ } -> close_enough cost 0.0
  | _ -> false

let%test "annotate_response_cost preserves measured cost" =
  let response : Types.api_response = {
    id = "resp-1";
    model = "claude-sonnet-4-6";
    stop_reason = Types.EndTurn;
    content = [Types.Text "ok"];
    usage = Some {
      input_tokens = 100;
      output_tokens = 20;
      cache_creation_input_tokens = 0;
      cache_read_input_tokens = 0;
      cost_usd = Some 0.1234;
    };
    telemetry = None;
  } in
  match annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } -> close_enough cost 0.1234
  | _ -> false
