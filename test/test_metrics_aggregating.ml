(** Tests for Llm_provider.Metrics.Aggregating — per-provider counter accumulation.

    @since 0.188.0 *)

open Alcotest
open Llm_provider
module M = Metrics
module Agg = M.Aggregating

let test_aggregating_create_empty () =
  let agg = Agg.create () in
  let snap = Agg.snapshot agg in
  check int "empty snapshot" 0 (List.length snap)
;;

let test_aggregating_on_request_start () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_request_start ~model_id:"test-model";
  let snap = Agg.snapshot agg in
  check int "one entry" 1 (List.length snap);
  let entry = List.hd snap in
  check int "request_total" 1 entry.M.request_total;
  check string "provider" "unknown" entry.M.provider;
  check string "model_id" "test-model" entry.M.model_id
;;

let test_aggregating_on_retry () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_retry ~provider:"openai" ~model_id:"gpt-4" ~attempt:1;
  hooks.on_retry ~provider:"openai" ~model_id:"gpt-4" ~attempt:2;
  let snap = Agg.snapshot agg in
  check int "one entry" 1 (List.length snap);
  let entry = List.hd snap in
  check int "retry_total" 2 entry.M.retry_total;
  check string "provider" "openai" entry.M.provider;
  check string "model_id" "gpt-4" entry.M.model_id
;;

let test_aggregating_on_token_usage () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_token_usage
    ~provider:"anthropic"
    ~model_id:"claude-3"
    ~input_tokens:100
    ~output_tokens:50;
  hooks.on_token_usage
    ~provider:"anthropic"
    ~model_id:"claude-3"
    ~input_tokens:200
    ~output_tokens:75;
  let snap = Agg.snapshot agg in
  let entry = List.hd snap in
  check int "input_tokens_total" 300 entry.M.input_tokens_total;
  check int "output_tokens_total" 125 entry.M.output_tokens_total
;;

let test_aggregating_on_error () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_error ~model_id:"bad-model" ~error:"timeout";
  let snap = Agg.snapshot agg in
  let entry = List.hd snap in
  check int "error_total" 1 entry.M.error_total
;;

let test_aggregating_on_request_end_latency () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_request_start ~model_id:"lat-model";
  hooks.on_request_end ~model_id:"lat-model" ~latency_ms:100;
  hooks.on_request_end ~model_id:"lat-model" ~latency_ms:200;
  let snap = Agg.snapshot agg in
  let entry = List.hd snap in
  check int "latency_ms_sum" 300 entry.M.latency_ms_sum;
  check int "latency_ms_count" 2 entry.M.latency_ms_count
;;

let test_aggregating_reset () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_request_start ~model_id:"x";
  hooks.on_error ~model_id:"x" ~error:"err";
  Agg.reset agg;
  let snap = Agg.snapshot agg in
  check int "empty after reset" 0 (List.length snap)
;;

let test_aggregating_key () =
  check string "key format" "prov/model" (Agg.key ~provider:"prov" ~model_id:"model")
;;

let test_aggregating_multiple_providers () =
  let agg = Agg.create () in
  let hooks = Agg.to_hooks agg in
  hooks.on_retry ~provider:"openai" ~model_id:"gpt-4" ~attempt:1;
  hooks.on_token_usage
    ~provider:"anthropic"
    ~model_id:"claude"
    ~input_tokens:50
    ~output_tokens:25;
  hooks.on_request_start ~model_id:"local";
  let snap = Agg.snapshot agg in
  check int "three entries" 3 (List.length snap)
;;

let test_aggregating_inner_delegation () =
  let inner_request_start_called = ref false in
  let inner : M.t =
    { M.noop with
      on_request_start = (fun ~model_id:_ -> inner_request_start_called := true)
    }
  in
  let agg = Agg.create ~inner () in
  let hooks = Agg.to_hooks agg in
  hooks.on_request_start ~model_id:"test";
  check bool "inner on_request_start called" true !inner_request_start_called;
  let snap = Agg.snapshot agg in
  check int "aggregator also counted" 1 (List.length snap)
;;

let () =
  run
    "Metrics.Aggregating"
    [ "create", [ test_case "empty snapshot" `Quick test_aggregating_create_empty ]
    ; ( "counters"
      , [ test_case "on_request_start" `Quick test_aggregating_on_request_start
        ; test_case "on_retry" `Quick test_aggregating_on_retry
        ; test_case "on_token_usage" `Quick test_aggregating_on_token_usage
        ; test_case "on_error" `Quick test_aggregating_on_error
        ; test_case
            "on_request_end latency"
            `Quick
            test_aggregating_on_request_end_latency
        ] )
    ; ( "lifecycle"
      , [ test_case "reset clears all" `Quick test_aggregating_reset
        ; test_case "key format" `Quick test_aggregating_key
        ; test_case "multiple providers" `Quick test_aggregating_multiple_providers
        ; test_case "inner delegation" `Quick test_aggregating_inner_delegation
        ] )
    ]
;;
