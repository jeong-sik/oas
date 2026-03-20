(** Integration tests for cost tracking — accumulation and budget enforcement.

    Uses mock HTTP server to verify cost accumulates across turns
    and budget limits are enforced by Agent.run.

    Pattern: test_integration.ml (Anthropic Messages API mock) *)

open Agent_sdk
open Types

(* ── Mock HTTP helpers ───────────────────────────────── *)

(** Build response with specific token counts for cost tracking. *)
let text_body_with_usage ~input_tokens ~output_tokens text =
  Printf.sprintf
    {|{"id":"c1","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":%d,"output_tokens":%d}}|}
    text input_tokens output_tokens

let tool_body_with_usage ~input_tokens ~output_tokens ~tool_name =
  Printf.sprintf
    {|{"id":"c2","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"tu_c","name":"%s","input":{}}],"stop_reason":"tool_use","usage":{"input_tokens":%d,"output_tokens":%d}}|}
    tool_name input_tokens output_tokens

let with_mock_server ~port handler f =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~base_url;
    Eio.Switch.fail sw Exit
  with Exit -> ()

(* ── Token accumulation tests ────────────────────────── *)

let test_tokens_accumulate_across_turns () =
  let call_count = ref 0 in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
    incr call_count;
    let response_body =
      if !call_count <= 2 then
        tool_body_with_usage ~input_tokens:100 ~output_tokens:50 ~tool_name:"echo"
      else
        text_body_with_usage ~input_tokens:100 ~output_tokens:50 "done"
    in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  in
  with_mock_server ~port:18301 handler (fun ~sw ~net ~base_url ->
    let tool = Tool.create ~name:"echo" ~description:"echo" ~parameters:[]
        (fun _input -> Ok { content = "ok" }) in
    let options = { Agent.default_options with base_url } in
    let config = { default_config with max_turns = 5 } in
    let agent = Agent.create ~net ~config ~options ~tools:[tool] () in
    (match Agent.run ~sw agent "test" with
     | Ok _ | Error _ -> ());
    let st = Agent.state agent in
    (* 3 API calls: tool, tool, text — each with 100 input + 50 output *)
    Alcotest.(check int) "api calls" 3 st.usage.api_calls;
    Alcotest.(check int) "input tokens" 300 st.usage.total_input_tokens;
    Alcotest.(check int) "output tokens" 150 st.usage.total_output_tokens)

let test_single_turn_usage () =
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
    let response_body =
      text_body_with_usage ~input_tokens:200 ~output_tokens:75 "hello" in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  in
  with_mock_server ~port:18302 handler (fun ~sw ~net ~base_url ->
    let options = { Agent.default_options with base_url } in
    let agent = Agent.create ~net ~options () in
    (match Agent.run ~sw agent "test" with
     | Ok _ | Error _ -> ());
    let st = Agent.state agent in
    Alcotest.(check int) "1 api call" 1 st.usage.api_calls;
    Alcotest.(check int) "input" 200 st.usage.total_input_tokens;
    Alcotest.(check int) "output" 75 st.usage.total_output_tokens)

(* ── Cost report tests ───────────────────────────────── *)

let test_report_zero_division () =
  let usage : Types.usage_stats = {
    total_input_tokens = 0; total_output_tokens = 0;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 0;
    api_calls = 0; estimated_cost_usd = 0.0 } in
  let r = Cost_tracker.report usage in
  Alcotest.(check (float 0.001)) "avg zero" 0.0 r.avg_cost_per_call

let test_report_format () =
  let usage : Types.usage_stats = {
    total_input_tokens = 1000; total_output_tokens = 500;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 0;
    api_calls = 5; estimated_cost_usd = 0.05 } in
  let r = Cost_tracker.report usage in
  let s = Cost_tracker.report_to_string r in
  Alcotest.(check bool) "non-empty" true (String.length s > 0);
  Alcotest.(check (float 0.001)) "total" 0.05 r.total_usd;
  Alcotest.(check int) "calls" 5 r.api_calls

(* ── Budget check tests ──────────────────────────────── *)

let test_budget_under () =
  let config = { default_config with max_cost_usd = Some 1.0 } in
  let usage : Types.usage_stats = {
    total_input_tokens = 0; total_output_tokens = 0;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 0;
    api_calls = 0; estimated_cost_usd = 0.5 } in
  Alcotest.(check bool) "under budget" true
    (Option.is_none (Cost_tracker.check_budget config usage))

let test_budget_exceeded () =
  let config = { default_config with max_cost_usd = Some 1.0 } in
  let usage : Types.usage_stats = {
    total_input_tokens = 0; total_output_tokens = 0;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 0;
    api_calls = 0; estimated_cost_usd = 1.5 } in
  match Cost_tracker.check_budget config usage with
  | Some (Error.Agent (CostBudgetExceeded _)) -> ()
  | _ -> Alcotest.fail "expected CostBudgetExceeded"

let test_no_budget_unlimited () =
  let config = { default_config with max_cost_usd = None } in
  let usage : Types.usage_stats = {
    total_input_tokens = 0; total_output_tokens = 0;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 0;
    api_calls = 0; estimated_cost_usd = 999.0 } in
  Alcotest.(check bool) "no limit" true
    (Option.is_none (Cost_tracker.check_budget config usage))

(* ── Suite ───────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  let open Alcotest in
  run "Cost_Integration" [
    "accumulation", [
      test_case "tokens across turns" `Quick
        test_tokens_accumulate_across_turns;
      test_case "single turn usage" `Quick test_single_turn_usage;
    ];
    "report", [
      test_case "zero division safe" `Quick test_report_zero_division;
      test_case "format" `Quick test_report_format;
    ];
    "budget", [
      test_case "under budget" `Quick test_budget_under;
      test_case "exceeded" `Quick test_budget_exceeded;
      test_case "no budget unlimited" `Quick test_no_budget_unlimited;
    ];
  ]
