(** Unit tests for Cost_tracker and Context_offload (v0.62.0). *)

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
      ?(unpriced_model = None)
      ()
  : Types.usage_stats
  =
  { total_input_tokens = inp
  ; total_output_tokens = out
  ; total_cache_creation_input_tokens = cache_creation
  ; total_cache_read_input_tokens = cache_read
  ; api_calls = calls
  ; estimated_cost_usd = cost
  ; unpriced_model
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

(* ── Context Offload ───────────────────────────────── *)

let test_offload_small_content () =
  let config = Context_offload.default_config in
  let result = Context_offload.maybe_offload ~config ~tool_name:"test" "small" in
  match result with
  | Context_offload.Kept s -> check string "kept" "small" s
  | Context_offload.Offloaded _ -> fail "should keep small content"
;;

let test_offload_large_content () =
  let config = { Context_offload.default_config with threshold_bytes = 10 } in
  let content = String.make 100 'x' in
  let result = Context_offload.maybe_offload ~config ~tool_name:"big" content in
  match result with
  | Context_offload.Offloaded { path; preview; original_bytes } ->
    check int "original bytes" 100 original_bytes;
    check bool "preview shorter" true (String.length preview <= config.preview_len);
    check bool "file exists" true (Sys.file_exists path);
    (* Cleanup *)
    (try Sys.remove path with
     | _ -> ())
  | Context_offload.Kept _ -> fail "should offload large content"
;;

let test_offload_exact_threshold () =
  let config = { Context_offload.default_config with threshold_bytes = 10 } in
  let content = String.make 10 'y' in
  match Context_offload.maybe_offload ~config ~tool_name:"exact" content with
  | Context_offload.Kept _ -> () (* At threshold, kept *)
  | Context_offload.Offloaded _ -> fail "at threshold should be kept"
;;

let test_offload_to_context_string_kept () =
  let s = Context_offload.to_context_string (Kept "hello") in
  check string "kept passthrough" "hello" s
;;

let test_offload_to_context_string_offloaded () =
  let s =
    Context_offload.to_context_string
      (Offloaded { path = "/tmp/test.txt"; preview = "first..."; original_bytes = 1000 })
  in
  check
    bool
    "contains path"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "/tmp/test.txt") s 0 in
       true
     with
     | Not_found -> false);
  check
    bool
    "contains bytes"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "1000") s 0 in
       true
     with
     | Not_found -> false)
;;

let test_offload_convenience () =
  let config = { Context_offload.default_config with threshold_bytes = 5 } in
  let result =
    Context_offload.offload_tool_result ~config ~tool_name:"conv" "this is longer than 5"
  in
  check
    bool
    "contains Offloaded"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "Offloaded") result 0 in
       true
     with
     | Not_found -> false)
;;

let test_offload_special_chars_in_name () =
  let config = { Context_offload.default_config with threshold_bytes = 5 } in
  let content = String.make 20 'z' in
  let result = Context_offload.maybe_offload ~config ~tool_name:"my/tool name" content in
  match result with
  | Context_offload.Offloaded { path; _ } ->
    check
      bool
      "no slash in filename"
      true
      (not (String.contains (Filename.basename path) '/'));
    (try Sys.remove path with
     | _ -> ())
  | Context_offload.Kept _ -> fail "should offload"
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "cost_and_offload"
    [ ( "cost_report"
      , [ test_case "basic report" `Quick test_report_basic
        ; test_case "cache miss input tokens" `Quick test_report_cache_miss_input_tokens
        ; test_case "zero calls" `Quick test_report_zero_calls
        ; test_case "to_string" `Quick test_report_to_string
        ] )
    ; ( "context_offload"
      , [ test_case "small kept" `Quick test_offload_small_content
        ; test_case "large offloaded" `Quick test_offload_large_content
        ; test_case "exact threshold" `Quick test_offload_exact_threshold
        ; test_case "to_context_string kept" `Quick test_offload_to_context_string_kept
        ; test_case
            "to_context_string offloaded"
            `Quick
            test_offload_to_context_string_offloaded
        ; test_case "convenience function" `Quick test_offload_convenience
        ; test_case "special chars in name" `Quick test_offload_special_chars_in_name
        ] )
    ]
;;
