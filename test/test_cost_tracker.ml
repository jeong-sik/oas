(** Unit tests for Cost_tracker. *)

open Alcotest
open Agent_sdk

(* ── Cost Tracker ──────────────────────────────────── *)

let make_usage
      ?(cost = 0.0)
      ?(calls = 0)
      ?(inp = 0)
      ?(out = 0)
      ?(cache_creation = 0)
      ?(cache_read = 0)
      ?(pricing_gap = None)
      ()
  : Types.usage_stats
  =
  { total_input_tokens = inp
  ; total_output_tokens = out
  ; total_cache_creation_input_tokens = cache_creation
  ; total_cache_read_input_tokens = cache_read
  ; api_calls = calls
  ; estimated_cost_usd = cost
  ; pricing_gap
  }
;;

(* ── Cost Report ───────────────────────────────────── *)

let test_report_basic () =
  let usage = make_usage ~cost:0.05 ~calls:10 ~inp:1000 ~out:500 () in
  let r = Cost_tracker.report usage in
  check (float 0.001) "total" 0.05 r.total_usd;
  check int "calls" 10 r.api_calls;
  check (float 0.001) "avg" 0.005 r.avg_cost_per_call;
  check int "input" 1000 r.input_tokens;
  check int "output" 500 r.output_tokens
;;

let test_report_cache_miss_input_tokens () =
  let usage =
    make_usage
      ~cost:0.05
      ~calls:2
      ~inp:1000
      ~out:100
      ~cache_creation:150
      ~cache_read:650
      ()
  in
  let r = Cost_tracker.report usage in
  check int "cache write" 150 r.cache_creation_tokens;
  check int "cache read" 650 r.cache_read_tokens;
  check int "cache miss" 200 r.cache_miss_input_tokens
;;

let test_report_zero_calls () =
  let usage = make_usage () in
  let r = Cost_tracker.report usage in
  check (float 0.001) "avg zero" 0.0 r.avg_cost_per_call
;;

let test_report_to_string () =
  let usage = make_usage ~cost:0.123456 ~calls:5 ~inp:500 ~out:200 ~cache_read:300 () in
  let r = Cost_tracker.report usage in
  let s = Cost_tracker.report_to_string r in
  check
    bool
    "contains cost"
    true
    (String.length s > 0
     &&
     try
       let _ = Str.search_forward (Str.regexp_string "0.123456") s 0 in
       true
     with
     | Not_found -> false);
  check
    bool
    "contains cache miss"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "200 miss") s 0 in
       true
     with
     | Not_found -> false)
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "cost_tracker"
    [ ( "cost_report"
      , [ test_case "basic report" `Quick test_report_basic
        ; test_case "cache miss input tokens" `Quick test_report_cache_miss_input_tokens
        ; test_case "zero calls" `Quick test_report_zero_calls
        ; test_case "to_string" `Quick test_report_to_string
        ] )
    ]
;;
