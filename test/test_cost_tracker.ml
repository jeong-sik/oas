(** Unit tests for Cost_tracker and Context_offload (v0.62.0). *)

open Alcotest
open Agent_sdk

(* ── Cost Tracker ──────────────────────────────────── *)

let make_usage ?(cost = 0.0) ?(calls = 0) ?(inp = 0) ?(out = 0) () : Types.usage_stats =
  { total_input_tokens = inp
  ; total_output_tokens = out
  ; total_cache_creation_input_tokens = 0
  ; total_cache_read_input_tokens = 0
  ; api_calls = calls
  ; estimated_cost_usd = cost
  }
;;

let make_config ?max_cost_usd () : Types.agent_config =
  { Types.default_config with max_cost_usd }
;;

let test_check_budget_no_limit () =
  let config = make_config () in
  let usage = make_usage ~cost:100.0 () in
  check
    bool
    "no limit = no error"
    true
    (Option.is_none (Cost_tracker.check_budget config usage))
;;

let test_check_budget_under () =
  let config = make_config ~max_cost_usd:1.0 () in
  let usage = make_usage ~cost:0.5 () in
  check
    bool
    "under budget = no error"
    true
    (Option.is_none (Cost_tracker.check_budget config usage))
;;

let test_check_budget_at_limit () =
  let config = make_config ~max_cost_usd:1.0 () in
  let usage = make_usage ~cost:1.0 () in
  check
    bool
    "at limit = no error"
    true
    (Option.is_none (Cost_tracker.check_budget config usage))
;;

let test_check_budget_exceeded () =
  let config = make_config ~max_cost_usd:1.0 () in
  let usage = make_usage ~cost:1.001 () in
  match Cost_tracker.check_budget config usage with
  | Some (Error.Agent (CostBudgetExceeded r)) ->
    check (float 0.001) "spent" 1.001 r.spent_usd;
    check (float 0.001) "limit" 1.0 r.limit_usd
  | Some _ -> fail "expected CostBudgetExceeded"
  | None -> fail "expected budget exceeded error"
;;

let test_check_budget_zero_limit () =
  let config = make_config ~max_cost_usd:0.0 () in
  let usage = make_usage ~cost:0.001 () in
  check
    bool
    "zero limit + any cost = exceeded"
    true
    (Option.is_some (Cost_tracker.check_budget config usage))
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

let test_report_zero_calls () =
  let usage = make_usage () in
  let r = Cost_tracker.report usage in
  check (float 0.001) "avg zero" 0.0 r.avg_cost_per_call
;;

let test_report_to_string () =
  let usage = make_usage ~cost:0.123456 ~calls:5 ~inp:500 ~out:200 () in
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
    [ ( "cost_budget"
      , [ test_case "no limit" `Quick test_check_budget_no_limit
        ; test_case "under budget" `Quick test_check_budget_under
        ; test_case "at limit" `Quick test_check_budget_at_limit
        ; test_case "exceeded" `Quick test_check_budget_exceeded
        ; test_case "zero limit" `Quick test_check_budget_zero_limit
        ] )
    ; ( "cost_report"
      , [ test_case "basic report" `Quick test_report_basic
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
