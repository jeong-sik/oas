open Agent_sdk

let default_attrs ?(name = "test") ?(agent = "agent") ?(turn = 1)
    ?(kind = Tracing.Agent_run) () : Tracing.span_attrs =
  { kind; name; agent_name = agent; turn; extra = [] }

let with_reset f =
  Otel_tracer.reset ();
  Fun.protect ~finally:(fun () -> Otel_tracer.reset ()) f

let test_summarize_counts () =
  with_reset @@ fun () ->
  let a = Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"run" ()) in
  Otel_tracer.end_span a ~ok:true;
  let api = Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"chat" ()) in
  Otel_tracer.end_span api ~ok:false;
  let tool = Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"grep" ()) in
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

let test_evaluate_detects_failed_api () =
  with_reset @@ fun () ->
  let api = Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"chat" ()) in
  Otel_tracer.end_span api ~ok:false;
  let spans = Otel_tracer.flush () in
  let evaluation = Trace_eval.evaluate spans in
  Alcotest.(check bool) "not ok" false evaluation.ok;
  Alcotest.(check bool) "api failure check present" true
    (List.exists
       (fun (check : Trace_eval.check) ->
         check.name = "api_failures_within_budget" && not check.passed)
       evaluation.checks)

let test_evaluate_allows_custom_budgets () =
  with_reset @@ fun () ->
  let tool = Otel_tracer.start_span (default_attrs ~kind:Tracing.Tool_exec ~name:"grep" ()) in
  Otel_tracer.end_span tool ~ok:false;
  let spans = Otel_tracer.flush () in
  let evaluation = Trace_eval.evaluate ~max_failed_tool_execs:1 spans in
  Alcotest.(check bool) "ok with budget" true evaluation.ok

let test_evaluate_flushed_clears_tracer () =
  with_reset @@ fun () ->
  let span = Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ~name:"run" ()) in
  Otel_tracer.end_span span ~ok:true;
  let evaluation = Trace_eval.evaluate_flushed () in
  Alcotest.(check bool) "ok" true evaluation.ok;
  Alcotest.(check int) "flush cleared tracer" 0 (Otel_tracer.completed_count ())

let () =
  let open Alcotest in
  run "Trace_eval"
    [
      ( "summary",
        [
          test_case "summarize counts" `Quick test_summarize_counts;
          test_case "detects failed api" `Quick test_evaluate_detects_failed_api;
          test_case "allows custom budgets" `Quick
            test_evaluate_allows_custom_budgets;
          test_case "evaluate_flushed clears tracer" `Quick
            test_evaluate_flushed_clears_tracer;
        ] );
    ]
