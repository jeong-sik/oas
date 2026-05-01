type summary =
  { total_spans : int
  ; agent_runs : int
  ; api_calls : int
  ; tool_execs : int
  ; hook_invokes : int
  ; failed_spans : int
  ; failed_api_calls : int
  ; failed_tool_execs : int
  ; total_events : int
  ; average_duration_ms : float option
  ; longest_span_name : string option
  }

type check =
  { name : string
  ; passed : bool
  ; detail : string option
  }

type evaluation =
  { ok : bool
  ; summary : summary
  ; checks : check list
  }

let classify_span (span : Otel_tracer.span) =
  if String.starts_with ~prefix:"agent_run/" span.name
  then `Agent_run
  else if String.starts_with ~prefix:"api_call/" span.name
  then `Api_call
  else if String.starts_with ~prefix:"tool_exec/" span.name
  then `Tool_exec
  else if String.starts_with ~prefix:"hook_invoke/" span.name
  then `Hook_invoke
  else `Other
;;

let duration_ms (span : Otel_tracer.span) =
  match span.end_time_ns with
  | Some end_ns ->
    Int64.sub end_ns span.start_time_ns |> Int64.to_float |> fun ns -> ns /. 1_000_000.0
  | None -> 0.0
;;

let summarize spans =
  let total_spans = List.length spans in
  let ( agent_runs
      , api_calls
      , tool_execs
      , hook_invokes
      , failed_spans
      , failed_api_calls
      , failed_tool_execs
      , total_events
      , total_duration_ms
      , longest_span_name
      , _longest_duration )
    =
    List.fold_left
      (fun ( agent_runs
           , api_calls
           , tool_execs
           , hook_invokes
           , failed_spans
           , failed_api_calls
           , failed_tool_execs
           , total_events
           , total_duration_ms
           , longest_span_name
           , longest_duration )
        (span : Otel_tracer.span) ->
         let failed = span.status = Some false in
         let duration = duration_ms span in
         let longest_span_name, longest_duration =
           if duration > longest_duration
           then Some span.name, duration
           else longest_span_name, longest_duration
         in
         let ( agent_runs
             , api_calls
             , tool_execs
             , hook_invokes
             , failed_api_calls
             , failed_tool_execs )
           =
           match classify_span span with
           | `Agent_run ->
             ( agent_runs + 1
             , api_calls
             , tool_execs
             , hook_invokes
             , failed_api_calls
             , failed_tool_execs )
           | `Api_call ->
             ( agent_runs
             , api_calls + 1
             , tool_execs
             , hook_invokes
             , (failed_api_calls + if failed then 1 else 0)
             , failed_tool_execs )
           | `Tool_exec ->
             ( agent_runs
             , api_calls
             , tool_execs + 1
             , hook_invokes
             , failed_api_calls
             , failed_tool_execs + if failed then 1 else 0 )
           | `Hook_invoke ->
             ( agent_runs
             , api_calls
             , tool_execs
             , hook_invokes + 1
             , failed_api_calls
             , failed_tool_execs )
           | `Other ->
             ( agent_runs
             , api_calls
             , tool_execs
             , hook_invokes
             , failed_api_calls
             , failed_tool_execs )
         in
         ( agent_runs
         , api_calls
         , tool_execs
         , hook_invokes
         , (failed_spans + if failed then 1 else 0)
         , failed_api_calls
         , failed_tool_execs
         , total_events + List.length span.events
         , total_duration_ms +. duration
         , longest_span_name
         , longest_duration ))
      (0, 0, 0, 0, 0, 0, 0, 0, 0.0, None, 0.0)
      spans
  in
  { total_spans
  ; agent_runs
  ; api_calls
  ; tool_execs
  ; hook_invokes
  ; failed_spans
  ; failed_api_calls
  ; failed_tool_execs
  ; total_events
  ; average_duration_ms =
      (if total_spans = 0
       then None
       else Some (total_duration_ms /. float_of_int total_spans))
  ; longest_span_name
  }
;;

let evaluate
      ?(max_failed_api_calls = 0)
      ?(max_failed_tool_execs = 0)
      ?max_span_duration_ms
      spans
  =
  let summary = summarize spans in
  let checks =
    [ { name = "has_spans"
      ; passed = summary.total_spans > 0
      ; detail = Some (Printf.sprintf "total_spans=%d" summary.total_spans)
      }
    ; { name = "api_failures_within_budget"
      ; passed = summary.failed_api_calls <= max_failed_api_calls
      ; detail =
          Some
            (Printf.sprintf
               "failed_api_calls=%d limit=%d"
               summary.failed_api_calls
               max_failed_api_calls)
      }
    ; { name = "tool_failures_within_budget"
      ; passed = summary.failed_tool_execs <= max_failed_tool_execs
      ; detail =
          Some
            (Printf.sprintf
               "failed_tool_execs=%d limit=%d"
               summary.failed_tool_execs
               max_failed_tool_execs)
      }
    ]
  in
  let checks =
    match max_span_duration_ms, summary.average_duration_ms with
    | Some max_ms, _ ->
      let longest =
        match
          List.fold_left
            (fun acc (span : Otel_tracer.span) -> max acc (duration_ms span))
            0.0
            spans
        with
        | value -> value
      in
      checks
      @ [ { name = "longest_span_within_budget"
          ; passed = longest <= max_ms
          ; detail =
              Some (Printf.sprintf "longest_span_ms=%.3f limit=%.3f" longest max_ms)
          }
        ]
    | None, _ -> checks
  in
  let ok = List.for_all (fun check -> check.passed) checks in
  { ok; summary; checks }
;;

let evaluate_flushed ?max_failed_api_calls ?max_failed_tool_execs ?max_span_duration_ms ()
  =
  let spans = Otel_tracer.flush () in
  evaluate ?max_failed_api_calls ?max_failed_tool_execs ?max_span_duration_ms spans
;;
