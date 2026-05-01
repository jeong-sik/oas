open Agent_sdk

let default_attrs
      ?(name = "test")
      ?(agent = "agent")
      ?(turn = 1)
      ?(kind = Tracing.Agent_run)
      ()
  : Tracing.span_attrs
  =
  { kind; name; agent_name = agent; turn; extra = [] }
;;

let with_reset f =
  Otel_tracer.reset ();
  Fun.protect ~finally:(fun () -> Otel_tracer.reset ()) f
;;

let test_summarize_counts () =
  with_reset
  @@ fun () ->
  let a = Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"run" ()) in
  Otel_tracer.end_span a ~ok:true;
  let api =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"chat" ())
  in
  Otel_tracer.end_span api ~ok:false;
  let tool =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"grep" ())
  in
  Otel_tracer.add_event tool "called";
  Otel_tracer.end_span tool ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "total spans" 3 summary.total_spans;
  Alcotest.(check int) "agent runs" 1 summary.agent_runs;
  Alcotest.(check int) "api calls" 1 summary.api_calls;
  Alcotest.(check int) "tool execs" 1 summary.tool_execs;
  Alcotest.(check int) "failed spans" 1 summary.failed_spans;
  Alcotest.(check int) "failed api calls" 1 summary.failed_api_calls;
  Alcotest.(check int) "events" 1 summary.total_events
;;

let test_evaluate_detects_failed_api () =
  with_reset
  @@ fun () ->
  let api =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"chat" ())
  in
  Otel_tracer.end_span api ~ok:false;
  let spans = Otel_tracer.flush () in
  let evaluation = Trace_eval.evaluate spans in
  Alcotest.(check bool) "not ok" false evaluation.ok;
  Alcotest.(check bool)
    "api failure check present"
    true
    (List.exists
       (fun (check : Trace_eval.check) ->
          check.name = "api_failures_within_budget" && not check.passed)
       evaluation.checks)
;;

let test_evaluate_allows_custom_budgets () =
  with_reset
  @@ fun () ->
  let tool =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"grep" ())
  in
  Otel_tracer.end_span tool ~ok:false;
  let spans = Otel_tracer.flush () in
  let evaluation = Trace_eval.evaluate ~max_failed_tool_execs:1 spans in
  Alcotest.(check bool) "ok with budget" true evaluation.ok
;;

let test_evaluate_flushed_clears_tracer () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"run" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let evaluation = Trace_eval.evaluate_flushed () in
  Alcotest.(check bool) "ok" true evaluation.ok;
  Alcotest.(check int) "flush cleared tracer" 0 (Otel_tracer.completed_count ())
;;

(* ── classify_span ────────────────────────────────────────── *)

let test_classify_agent_run () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"my_agent" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let s = List.hd spans in
  Alcotest.(check string) "span name" "agent_run/my_agent" s.name;
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "agent_runs" 1 summary.agent_runs
;;

let test_classify_tool_exec () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"grep" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "tool_execs" 1 summary.tool_execs
;;

let test_classify_hook_invoke () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span
      (default_attrs ~kind:Tracing.Hook_invoke ~name:"before_turn" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "hook_invokes" 1 summary.hook_invokes
;;

(* ── summarize: empty spans ─────────────────────────────── *)

let test_summarize_empty () =
  let summary = Trace_eval.summarize [] in
  Alcotest.(check int) "total 0" 0 summary.total_spans;
  Alcotest.(check int) "agent_runs 0" 0 summary.agent_runs;
  Alcotest.(check int) "api_calls 0" 0 summary.api_calls;
  Alcotest.(check int) "tool_execs 0" 0 summary.tool_execs;
  Alcotest.(check int) "hook_invokes 0" 0 summary.hook_invokes;
  Alcotest.(check int) "failed 0" 0 summary.failed_spans;
  Alcotest.(check int) "events 0" 0 summary.total_events;
  (match summary.average_duration_ms with
   | None -> ()
   | Some _ -> Alcotest.fail "expected None for empty");
  match summary.longest_span_name with
  | None -> ()
  | Some _ -> Alcotest.fail "expected None for empty"
;;

(* ── summarize: failed tool_execs ────────────────────────── *)

let test_summarize_failed_tool () =
  with_reset
  @@ fun () ->
  let tool =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"fail_tool" ())
  in
  Otel_tracer.end_span tool ~ok:false;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "failed_tool_execs" 1 summary.failed_tool_execs;
  Alcotest.(check int) "failed_spans" 1 summary.failed_spans
;;

(* ── summarize: average_duration_ms ──────────────────────── *)

let test_summarize_avg_duration () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"r1" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  match summary.average_duration_ms with
  | Some avg -> Alcotest.(check bool) "avg >= 0" true (avg >= 0.0)
  | None -> Alcotest.fail "expected Some for non-empty spans"
;;

(* ── summarize: longest_span_name ────────────────────────── *)

let test_summarize_longest_span () =
  with_reset
  @@ fun () ->
  let s1 =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"short" ())
  in
  Otel_tracer.end_span s1 ~ok:true;
  let s2 =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"long" ())
  in
  for _ = 0 to 100 do
    ignore (Unix.gettimeofday ())
  done;
  Otel_tracer.end_span s2 ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check bool) "longest is some" true (Option.is_some summary.longest_span_name)
;;

(* ── summarize: multiple events ──────────────────────────── *)

let test_summarize_multiple_events () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"multi" ())
  in
  Otel_tracer.add_event span "event1";
  Otel_tracer.add_event span "event2";
  Otel_tracer.add_event span "event3";
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "total_events" 3 summary.total_events
;;

(* ── evaluate: max_span_duration_ms ──────────────────────── *)

let test_evaluate_max_span_duration_pass () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"fast" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate ~max_span_duration_ms:10000.0 spans in
  Alcotest.(check bool) "ok (fast span)" true eval.ok;
  Alcotest.(check bool)
    "has duration check"
    true
    (List.exists
       (fun (c : Trace_eval.check) -> c.name = "longest_span_within_budget" && c.passed)
       eval.checks)
;;

let test_evaluate_max_span_duration_fail () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"slow" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate ~max_span_duration_ms:(-1.0) spans in
  Alcotest.(check bool) "not ok (budget exceeded)" false eval.ok;
  Alcotest.(check bool)
    "duration check failed"
    true
    (List.exists
       (fun (c : Trace_eval.check) ->
          c.name = "longest_span_within_budget" && not c.passed)
       eval.checks)
;;

(* ── evaluate: has_spans check ───────────────────────────── *)

let test_evaluate_empty_spans () =
  let eval = Trace_eval.evaluate [] in
  Alcotest.(check bool) "not ok (no spans)" false eval.ok;
  Alcotest.(check bool)
    "has_spans failed"
    true
    (List.exists
       (fun (c : Trace_eval.check) -> c.name = "has_spans" && not c.passed)
       eval.checks)
;;

(* ── evaluate: tool failure budget ───────────────────────── *)

let test_evaluate_tool_failure_exceeds_budget () =
  with_reset
  @@ fun () ->
  let t1 = Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"t1" ()) in
  Otel_tracer.end_span t1 ~ok:false;
  let t2 = Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"t2" ()) in
  Otel_tracer.end_span t2 ~ok:false;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate ~max_failed_tool_execs:1 spans in
  Alcotest.(check bool) "not ok (2 tool failures > budget 1)" false eval.ok
;;

let test_evaluate_tool_failure_within_budget () =
  with_reset
  @@ fun () ->
  let t1 = Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"t1" ()) in
  Otel_tracer.end_span t1 ~ok:false;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate ~max_failed_tool_execs:1 spans in
  Alcotest.(check bool) "ok (1 tool failure within budget 1)" true eval.ok
;;

(* ── evaluate: api failure budget ────────────────────────── *)

let test_evaluate_api_failure_budget () =
  with_reset
  @@ fun () ->
  let a1 = Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"c1" ()) in
  Otel_tracer.end_span a1 ~ok:false;
  let a2 = Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"c2" ()) in
  Otel_tracer.end_span a2 ~ok:false;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate ~max_failed_api_calls:2 spans in
  Alcotest.(check bool) "ok (2 api failures = budget 2)" true eval.ok
;;

(* ── evaluate: check detail field ────────────────────────── *)

let test_check_detail_present () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"det" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let spans = Otel_tracer.flush () in
  let eval = Trace_eval.evaluate spans in
  List.iter
    (fun (c : Trace_eval.check) ->
       Alcotest.(check bool) ("detail present: " ^ c.name) true (Option.is_some c.detail))
    eval.checks
;;

(* ── evaluate_flushed: with budgets ──────────────────────── *)

let test_evaluate_flushed_with_budgets () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"api" ())
  in
  Otel_tracer.end_span span ~ok:false;
  let eval = Trace_eval.evaluate_flushed ~max_failed_api_calls:1 () in
  Alcotest.(check bool) "ok with budget" true eval.ok
;;

let test_evaluate_flushed_with_duration_budget () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"r" ())
  in
  Otel_tracer.end_span span ~ok:true;
  let eval = Trace_eval.evaluate_flushed ~max_span_duration_ms:100000.0 () in
  Alcotest.(check bool) "ok with duration budget" true eval.ok
;;

(* ── duration_ms: span without end_time ─────────────────── *)

let test_duration_unfinished_span () =
  with_reset
  @@ fun () ->
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"open" ())
  in
  let d = Trace_eval.duration_ms span in
  Alcotest.(check bool) "unfinished is 0" true (d = 0.0);
  Otel_tracer.end_span span ~ok:true
;;

(* ── Mixed span types in a single summarize call ────────── *)

let test_summarize_mixed_types () =
  with_reset
  @@ fun () ->
  let a = Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"ag" ()) in
  Otel_tracer.end_span a ~ok:true;
  let b = Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"api" ()) in
  Otel_tracer.end_span b ~ok:true;
  let c =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"tool" ())
  in
  Otel_tracer.end_span c ~ok:false;
  let d =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Hook_invoke ~name:"hook" ())
  in
  Otel_tracer.end_span d ~ok:true;
  let spans = Otel_tracer.flush () in
  let summary = Trace_eval.summarize spans in
  Alcotest.(check int) "total" 4 summary.total_spans;
  Alcotest.(check int) "agent_runs" 1 summary.agent_runs;
  Alcotest.(check int) "api_calls" 1 summary.api_calls;
  Alcotest.(check int) "tool_execs" 1 summary.tool_execs;
  Alcotest.(check int) "hook_invokes" 1 summary.hook_invokes;
  Alcotest.(check int) "failed_spans" 1 summary.failed_spans;
  Alcotest.(check int) "failed_api_calls" 0 summary.failed_api_calls;
  Alcotest.(check int) "failed_tool_execs" 1 summary.failed_tool_execs
;;

let () =
  let open Alcotest in
  run
    "Trace_eval"
    [ ( "summary"
      , [ test_case "summarize counts" `Quick test_summarize_counts
        ; test_case "detects failed api" `Quick test_evaluate_detects_failed_api
        ; test_case "allows custom budgets" `Quick test_evaluate_allows_custom_budgets
        ; test_case
            "evaluate_flushed clears tracer"
            `Quick
            test_evaluate_flushed_clears_tracer
        ] )
    ; ( "classify"
      , [ test_case "agent_run" `Quick test_classify_agent_run
        ; test_case "tool_exec" `Quick test_classify_tool_exec
        ; test_case "hook_invoke" `Quick test_classify_hook_invoke
        ] )
    ; ( "summarize_detail"
      , [ test_case "empty spans" `Quick test_summarize_empty
        ; test_case "failed tool" `Quick test_summarize_failed_tool
        ; test_case "avg duration" `Quick test_summarize_avg_duration
        ; test_case "longest span" `Quick test_summarize_longest_span
        ; test_case "multiple events" `Quick test_summarize_multiple_events
        ; test_case "mixed types" `Quick test_summarize_mixed_types
        ] )
    ; ( "evaluate_budget"
      , [ test_case "max_span_duration pass" `Quick test_evaluate_max_span_duration_pass
        ; test_case "max_span_duration fail" `Quick test_evaluate_max_span_duration_fail
        ; test_case "empty spans" `Quick test_evaluate_empty_spans
        ; test_case
            "tool failure exceeds"
            `Quick
            test_evaluate_tool_failure_exceeds_budget
        ; test_case "tool failure within" `Quick test_evaluate_tool_failure_within_budget
        ; test_case "api failure budget" `Quick test_evaluate_api_failure_budget
        ; test_case "check detail" `Quick test_check_detail_present
        ] )
    ; ( "evaluate_flushed"
      , [ test_case "with api budget" `Quick test_evaluate_flushed_with_budgets
        ; test_case
            "with duration budget"
            `Quick
            test_evaluate_flushed_with_duration_budget
        ] )
    ; "duration", [ test_case "unfinished span" `Quick test_duration_unfinished_span ]
    ]
;;
