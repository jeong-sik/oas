open Alcotest
open Agent_sdk

(* ── Provider cascade type tests ─────────────────────────────── *)

let test_cascade_type () =
  let primary = Provider.anthropic_sonnet () in
  let fallback = { Provider.provider = Local { base_url = "http://127.0.0.1:8085" }; model_id = "qwen3.5-35b-a3b"; api_key_env = "DUMMY_KEY" } in
  let casc = Provider.cascade ~primary ~fallbacks:[fallback] in
  check string "primary model" "claude-sonnet-4-6" casc.primary.model_id;
  check int "fallback count" 1 (List.length casc.fallbacks);
  check string "fallback model" "qwen3.5-35b-a3b"
    (List.hd casc.fallbacks).model_id

let test_cascade_empty_fallbacks () =
  let primary = Provider.anthropic_sonnet () in
  let casc = Provider.cascade ~primary ~fallbacks:[] in
  check int "no fallbacks" 0 (List.length casc.fallbacks)

let test_cascade_multiple_fallbacks () =
  let primary = Provider.anthropic_opus () in
  let fb1 = Provider.anthropic_sonnet () in
  let fb2 = { Provider.provider = Local { base_url = "http://127.0.0.1:8085" }; model_id = "qwen3.5-35b-a3b"; api_key_env = "DUMMY_KEY" } in
  let casc = Provider.cascade ~primary ~fallbacks:[fb1; fb2] in
  check int "two fallbacks" 2 (List.length casc.fallbacks)

(* ── Pricing tests ───────────────────────────────────────────── *)

let test_pricing_opus () =
  let p = Provider.pricing_for_model "claude-opus-4-6-20250514" in
  check (float 0.01) "opus input" 15.0 p.input_per_million;
  check (float 0.01) "opus output" 75.0 p.output_per_million

let test_pricing_sonnet () =
  let p = Provider.pricing_for_model "claude-sonnet-4-6" in
  check (float 0.01) "sonnet input" 3.0 p.input_per_million;
  check (float 0.01) "sonnet output" 15.0 p.output_per_million

let test_pricing_haiku () =
  let p = Provider.pricing_for_model "claude-haiku-4-5-20251001" in
  check (float 0.01) "haiku input" 0.8 p.input_per_million;
  check (float 0.01) "haiku output" 4.0 p.output_per_million

let test_pricing_unknown () =
  let p = Provider.pricing_for_model "some-unknown-model" in
  check (float 0.01) "unknown input" 0.0 p.input_per_million;
  check (float 0.01) "unknown output" 0.0 p.output_per_million

let test_estimate_cost () =
  let pricing = Provider.pricing_for_model "claude-sonnet-4-6" in
  let cost = Provider.estimate_cost ~pricing ~input_tokens:1_000_000 ~output_tokens:100_000 () in
  (* 1M input * 3.0/1M + 100K output * 15.0/1M = 3.0 + 1.5 = 4.5 *)
  check (float 0.001) "cost" 4.5 cost

(* ── Retry cascade tests ─────────────────────────────────────── *)

let test_cascade_retry_primary_succeeds () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let primary () = Ok "primary_result" in
  let fallback () = Ok "fallback_result" in
  let config = { Retry.max_retries = 1; initial_delay = 0.01; max_delay = 0.1; backoff_factor = 2.0 } in
  match Retry.with_cascade ~clock ~config ~primary ~fallbacks:[fallback] () with
  | Ok v -> check string "uses primary" "primary_result" v
  | Error _ -> fail "expected success"

let test_cascade_retry_falls_through () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let primary () = Error (Retry.RateLimited { retry_after = Some 0.01; message = "rate limited" }) in
  let fallback () = Ok "fallback_result" in
  let config = { Retry.max_retries = 0; initial_delay = 0.01; max_delay = 0.1; backoff_factor = 2.0 } in
  match Retry.with_cascade ~clock ~config ~primary ~fallbacks:[fallback] () with
  | Ok v -> check string "uses fallback" "fallback_result" v
  | Error _ -> fail "expected fallback success"

let test_cascade_non_retryable_tries_fallback () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let primary () = Error (Retry.AuthError { message = "bad key" }) in
  let fallback () = Ok "fallback_result" in
  let config = { Retry.max_retries = 1; initial_delay = 0.01; max_delay = 0.1; backoff_factor = 2.0 } in
  match Retry.with_cascade ~clock ~config ~primary ~fallbacks:[fallback] () with
  | Ok v -> check string "fallback used on non-retryable primary" "fallback_result" v
  | Error _ -> fail "expected fallback to succeed"

let test_cascade_all_fail_returns_primary_error () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let primary () = Error (Retry.AuthError { message = "bad key" }) in
  let fallback () = Error (Retry.ServerError { status = 500; message = "down" }) in
  let config = { Retry.max_retries = 0; initial_delay = 0.01; max_delay = 0.1; backoff_factor = 2.0 } in
  match Retry.with_cascade ~clock ~config ~primary ~fallbacks:[fallback] () with
  | Ok _ -> fail "expected error"
  | Error (Retry.AuthError { message }) ->
      check string "primary error preserved" "bad key" message
  | Error _ -> fail "expected primary AuthError"

let test_cascade_invalid_request_tries_all_fallbacks () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let call_log = ref [] in
  let make_provider name result () =
    call_log := name :: !call_log;
    result
  in
  let primary = make_provider "primary"
    (Error (Retry.InvalidRequest { message = "bad param" })) in
  let fb1 = make_provider "fb1"
    (Error (Retry.AuthError { message = "no key" })) in
  let fb2 = make_provider "fb2" (Ok "fb2_result") in
  let config = { Retry.max_retries = 0; initial_delay = 0.01; max_delay = 0.1; backoff_factor = 2.0 } in
  match Retry.with_cascade ~clock ~config ~primary ~fallbacks:[fb1; fb2] () with
  | Ok v ->
    check string "reached fb2" "fb2_result" v;
    (* All three providers were attempted *)
    check int "all providers called" 3 (List.length !call_log)
  | Error _ -> fail "expected fb2 to succeed"

(* ── Builder cascade tests ───────────────────────────────────── *)

let test_builder_with_fallback () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent =
    Builder.create ~net ~model:"claude-sonnet-4-6"
    |> Builder.with_provider (Provider.anthropic_sonnet ())
    |> Builder.with_fallback ({ Provider.provider = Local { base_url = "http://127.0.0.1:8085" }; model_id = "qwen3.5-35b-a3b"; api_key_env = "DUMMY_KEY" })
    |> Builder.build_safe
  in
  let agent = match agent with
    | Ok a -> a
    | Error e -> fail (Error.to_string e)
  in
  let opts = Agent.options agent in
  match opts.cascade with
  | Some casc ->
    check string "primary" "claude-sonnet-4-6" casc.primary.model_id;
    check int "fallbacks" 1 (List.length casc.fallbacks)
  | None -> fail "expected cascade to be set"

(* Test apply_provider_filter directly using mock Provider_config.t values.
   Avoids parse_model_strings dependency on API key env vars in CI. *)
let mock_provider kind model_id : Llm_provider.Provider_config.t =
  { kind; model_id; base_url = ""; api_key = ""; headers = [];
    request_path = ""; max_tokens = Some 4096; max_context = None;
    temperature = None;
    top_p = None; top_k = None; min_p = None; system_prompt = None;
    enable_thinking = None; thinking_budget = None;
    clear_thinking = None; tool_stream = false; tool_choice = None;
    disable_parallel_tool_use = false; response_format_json = false;
    cache_system_prompt = false }

let test_provider_filter_ollama_only () =
  let providers = [
    mock_provider Llm_provider.Provider_config.Glm "glm-5.1";
    mock_provider Llm_provider.Provider_config.Glm "glm-5-turbo";
    mock_provider Llm_provider.Provider_config.Ollama "qwen3.5";
  ] in
  let filtered = Llm_provider.Cascade_config.apply_provider_filter
    ~provider_filter:(Some ["ollama"]) ~label:"test" providers in
  check int "one ollama" 1 (List.length filtered);
  check string "kind" "ollama"
    (Llm_provider.Provider_config.string_of_provider_kind (List.hd filtered).kind)

let test_provider_filter_none_passes_all () =
  let providers = [
    mock_provider Llm_provider.Provider_config.Glm "glm-5.1";
    mock_provider Llm_provider.Provider_config.Ollama "qwen3.5";
  ] in
  let result = Llm_provider.Cascade_config.apply_provider_filter
    ~provider_filter:None ~label:"test" providers in
  check int "all pass" 2 (List.length result)

let test_provider_filter_no_match_fallback () =
  let providers = [
    mock_provider Llm_provider.Provider_config.Glm "glm-5.1";
    mock_provider Llm_provider.Provider_config.Ollama "qwen3.5";
  ] in
  let result = Llm_provider.Cascade_config.apply_provider_filter
    ~provider_filter:(Some ["nonexistent"]) ~label:"test" providers in
  check int "fallback to all" 2 (List.length result)

let () =
  run "cascade" [
    "type", [
      test_case "basic cascade" `Quick test_cascade_type;
      test_case "empty fallbacks" `Quick test_cascade_empty_fallbacks;
      test_case "multiple fallbacks" `Quick test_cascade_multiple_fallbacks;
    ];
    "pricing", [
      test_case "opus pricing" `Quick test_pricing_opus;
      test_case "sonnet pricing" `Quick test_pricing_sonnet;
      test_case "haiku pricing" `Quick test_pricing_haiku;
      test_case "unknown pricing" `Quick test_pricing_unknown;
      test_case "estimate cost" `Quick test_estimate_cost;
    ];
    "retry", [
      test_case "primary succeeds" `Quick test_cascade_retry_primary_succeeds;
      test_case "falls through to fallback" `Quick test_cascade_retry_falls_through;
      test_case "non-retryable tries fallback" `Quick test_cascade_non_retryable_tries_fallback;
      test_case "all fail returns primary error" `Quick test_cascade_all_fail_returns_primary_error;
      test_case "InvalidRequest cascades through all fallbacks" `Quick test_cascade_invalid_request_tries_all_fallbacks;
    ];
    "builder", [
      test_case "with_fallback" `Quick test_builder_with_fallback;
    ];
    "provider_filter", [
      test_case "ollama only" `Quick test_provider_filter_ollama_only;
      test_case "none passes all" `Quick test_provider_filter_none_passes_all;
      test_case "no match empty" `Quick test_provider_filter_no_match_fallback;
    ];
  ]
