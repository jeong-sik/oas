(** Eval Metrics OTel Bridge.

    Bridges [Eval.run_metrics] into OTel-compatible span attributes.

    OAS's Otel_tracer provides span/event recording only (no native
    counter/gauge metric API).  This module records eval metrics as
    attributes on a dedicated span so OTel collectors can derive
    counters and gauges from attribute values.

    Metric namespace: [oas.eval.*], [oas.agent.*], [oas.tool.*]
    per RFC-OAS-002. *)

(* ── Types ─────────────────────────────────────────────────────── *)

type otel_metric =
  { name : string
  ; value : float
  ; metric_type : string
  }

type metrics_snapshot =
  { agent_name : string
  ; run_id : string
  ; verdict_passed_total : int
  ; verdict_failed_total : int
  ; coverage : float
  ; turns_total : int option
  ; tool_calls_total : int option
  ; tool_errors_total : int option
  ; api_calls_total : int option
  ; failed_api_calls_total : int option
  ; cost_usd : float option
  }

(* ── Extraction ────────────────────────────────────────────────── *)

let extract (rm : Eval.run_metrics) : metrics_snapshot =
  let passed =
    List.length (List.filter (fun (v : Harness.verdict) -> v.passed) rm.harness_verdicts)
  in
  let failed =
    List.length
      (List.filter (fun (v : Harness.verdict) -> not v.passed) rm.harness_verdicts)
  in
  let total = List.length rm.harness_verdicts in
  let coverage = if total = 0 then 1.0 else Float.of_int passed /. Float.of_int total in
  let ( turns_total
      , tool_calls_total
      , tool_errors_total
      , api_calls_total
      , failed_api_calls_total )
    =
    match rm.trace_summary with
    | None -> None, None, None, None, None
    | Some (s : Trace_eval.summary) ->
      ( Some s.agent_runs
      , Some s.tool_execs
      , Some s.failed_tool_execs
      , Some s.api_calls
      , Some s.failed_api_calls )
  in
  let cost_usd =
    match List.find_opt (fun (m : Eval.metric) -> m.name = "cost_usd") rm.metrics with
    | Some m -> Eval.metric_value_to_float m.value
    | None -> None
  in
  { agent_name = rm.agent_name
  ; run_id = rm.run_id
  ; verdict_passed_total = passed
  ; verdict_failed_total = failed
  ; coverage
  ; turns_total
  ; tool_calls_total
  ; tool_errors_total
  ; api_calls_total
  ; failed_api_calls_total
  ; cost_usd
  }
;;

(* ── OTel metric list ──────────────────────────────────────────── *)

let to_metric_list (snap : metrics_snapshot) : otel_metric list =
  let required =
    [ { name = "oas.eval.verdict_passed_total"
      ; value = Float.of_int snap.verdict_passed_total
      ; metric_type = "counter"
      }
    ; { name = "oas.eval.verdict_failed_total"
      ; value = Float.of_int snap.verdict_failed_total
      ; metric_type = "counter"
      }
    ; { name = "oas.eval.coverage"; value = snap.coverage; metric_type = "gauge" }
    ]
  in
  let optional =
    List.filter_map
      Fun.id
      [ Option.map
          (fun v ->
             { name = "oas.agent.turns_total"
             ; value = Float.of_int v
             ; metric_type = "counter"
             })
          snap.turns_total
      ; Option.map
          (fun v ->
             { name = "oas.tool.calls_total"
             ; value = Float.of_int v
             ; metric_type = "counter"
             })
          snap.tool_calls_total
      ; Option.map
          (fun v ->
             { name = "oas.tool.errors_total"
             ; value = Float.of_int v
             ; metric_type = "counter"
             })
          snap.tool_errors_total
      ; Option.map
          (fun v ->
             { name = "oas.llm.requests_total"
             ; value = Float.of_int v
             ; metric_type = "counter"
             })
          snap.api_calls_total
      ; Option.map
          (fun v ->
             { name = "oas.llm.failed_requests_total"
             ; value = Float.of_int v
             ; metric_type = "counter"
             })
          snap.failed_api_calls_total
      ; Option.map
          (fun v -> { name = "oas.eval.cost_usd"; value = v; metric_type = "gauge" })
          snap.cost_usd
      ]
  in
  required @ optional
;;

(* ── Metric type mapping ──────────────────────────────────────── *)

let otel_metric_type_to_tracer (mt : string) : Otel_tracer.metric_type =
  match mt with
  | "counter" -> Otel_tracer.Counter
  | "gauge" -> Otel_tracer.Gauge
  | "histogram" -> Otel_tracer.Histogram
  | _ -> Otel_tracer.Gauge
;;

(* ── OTel metric emission ────────────────────────────────────── *)

let emit_run_metrics (inst : Otel_tracer.instance) (rm : Eval.run_metrics) : unit =
  let snap = extract rm in
  let metrics = to_metric_list snap in
  (* Record each metric via the native metric API *)
  List.iter
    (fun (m : otel_metric) ->
       Otel_tracer.inst_record_metric
         inst
         ~name:m.name
         ~value:m.value
         ~metric_type:(otel_metric_type_to_tracer m.metric_type))
    metrics;
  (* Create a summary span for correlation and event context *)
  let span_attrs : Tracing.span_attrs =
    { kind = Tracing.Agent_run
    ; name = "eval_metrics"
    ; agent_name = snap.agent_name
    ; turn = 0
    ; extra = [ "oas.run_id", snap.run_id ]
    ; links = []
    }
  in
  let span = Otel_tracer.inst_start_span inst span_attrs in
  (* Add a summary event *)
  Otel_tracer.inst_add_event
    inst
    span
    (Printf.sprintf
       "eval: %d passed, %d failed, coverage=%.2f"
       snap.verdict_passed_total
       snap.verdict_failed_total
       snap.coverage);
  (* Close the span *)
  Otel_tracer.inst_end_span inst span ~ok:(snap.verdict_failed_total = 0)
;;

(* Shared stdlib-mutex instance construction.  Kept as a single helper so
   tests and the shared default instance use the same config source. *)
let _mk_stdlib_inst () : Otel_tracer.instance =
  { config = Otel_tracer.default_config_from_env ()
  ; mu = Otel_tracer.Stdlib_mu (Mutex.create ())
  ; fiber_key = None
  ; current_spans = []
  ; completed_spans = []
  ; metrics = []
  }
;;

(* Lazy so default_config_from_env runs on first use, not module load. *)
let default_otel_instance : Otel_tracer.instance lazy_t = lazy (_mk_stdlib_inst ())
let default_instance () = Lazy.force default_otel_instance
let emit_run_metrics_default rm = emit_run_metrics (default_instance ()) rm

(* ── JSON export ──────────────────────────────────────────────── *)

let to_metrics_json (snap : metrics_snapshot) : Yojson.Safe.t =
  let metrics = to_metric_list snap in
  let tags =
    `Assoc [ "agent_name", `String snap.agent_name; "run_id", `String snap.run_id ]
  in
  `List
    (List.map
       (fun (m : otel_metric) ->
          `Assoc
            [ "name", `String m.name
            ; "value", `Float m.value
            ; "type", `String m.metric_type
            ; "tags", tags
            ])
       metrics)
;;

(* ── Inline tests ─────────────────────────────────────────── *)

let%test "extract: empty verdicts yield coverage 1.0" =
  let rm : Eval.run_metrics =
    { run_id = "r1"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = None
    }
  in
  let s = extract rm in
  s.verdict_passed_total = 0 && s.verdict_failed_total = 0 && s.coverage = 1.0
;;

let%test "extract: mixed verdicts" =
  let pass : Harness.verdict =
    { passed = true; score = None; evidence = []; detail = None }
  in
  let fail_ : Harness.verdict =
    { passed = false; score = None; evidence = []; detail = None }
  in
  let rm : Eval.run_metrics =
    { run_id = "r2"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = [ pass; pass; fail_ ]
    ; trace_summary = None
    }
  in
  let s = extract rm in
  s.verdict_passed_total = 2
  && s.verdict_failed_total = 1
  && Float.abs (s.coverage -. (2.0 /. 3.0)) < 0.001
;;

let%test "metric_list: 3 required when no trace" =
  let rm : Eval.run_metrics =
    { run_id = "r3"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = None
    }
  in
  let ms = to_metric_list (extract rm) in
  List.length ms = 3
;;

let%test "metric_list: 8 with trace summary" =
  let summary : Trace_eval.summary =
    { total_spans = 10
    ; agent_runs = 2
    ; api_calls = 5
    ; tool_execs = 4
    ; hook_invokes = 1
    ; failed_spans = 1
    ; failed_api_calls = 1
    ; failed_tool_execs = 0
    ; total_events = 8
    ; average_duration_ms = Some 50.0
    ; longest_span_name = None
    }
  in
  let rm : Eval.run_metrics =
    { run_id = "r4"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = Some summary
    }
  in
  let ms = to_metric_list (extract rm) in
  List.length ms = 8
;;

let%test "all metric names start with oas." =
  let summary : Trace_eval.summary =
    { total_spans = 10
    ; agent_runs = 2
    ; api_calls = 5
    ; tool_execs = 4
    ; hook_invokes = 1
    ; failed_spans = 1
    ; failed_api_calls = 1
    ; failed_tool_execs = 0
    ; total_events = 8
    ; average_duration_ms = Some 50.0
    ; longest_span_name = None
    }
  in
  let pass : Harness.verdict =
    { passed = true; score = None; evidence = []; detail = None }
  in
  let rm : Eval.run_metrics =
    { run_id = "r5"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = [ pass ]
    ; trace_summary = Some summary
    }
  in
  let ms = to_metric_list (extract rm) in
  List.for_all (fun m -> String.length m.name > 4 && String.sub m.name 0 4 = "oas.") ms
;;

let%test "emit_run_metrics creates one span" =
  let inst = _mk_stdlib_inst () in
  let rm : Eval.run_metrics =
    { run_id = "r6"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = None
    }
  in
  emit_run_metrics inst rm;
  Otel_tracer.inst_completed_count inst = 1
;;

let%test "emit_run_metrics_default records on shared instance" =
  let inst = default_instance () in
  Otel_tracer.inst_clear_metrics inst;
  ignore (Otel_tracer.inst_flush inst : Otel_tracer.span list);
  let pass : Harness.verdict =
    { passed = true; score = None; evidence = []; detail = None }
  in
  let rm : Eval.run_metrics =
    { run_id = "shared"
    ; agent_name = "eval-agent"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = [ pass ]
    ; trace_summary = None
    }
  in
  emit_run_metrics_default rm;
  Otel_tracer.inst_get_metrics inst
  |> List.exists (fun (name, _, _) -> String.equal name "oas.eval.verdict_passed_total")
;;

let%test "emit span ok=true when no failures" =
  let inst = _mk_stdlib_inst () in
  let pass : Harness.verdict =
    { passed = true; score = None; evidence = []; detail = None }
  in
  let rm : Eval.run_metrics =
    { run_id = "r7"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = [ pass ]
    ; trace_summary = None
    }
  in
  emit_run_metrics inst rm;
  let spans = Otel_tracer.inst_flush inst in
  match spans with
  | [ s ] -> s.status = Some true
  | _ -> false
;;

let%test "emit span ok=false when failures" =
  let inst = _mk_stdlib_inst () in
  let fail_ : Harness.verdict =
    { passed = false; score = None; evidence = []; detail = None }
  in
  let rm : Eval.run_metrics =
    { run_id = "r8"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = [ fail_ ]
    ; trace_summary = None
    }
  in
  emit_run_metrics inst rm;
  let spans = Otel_tracer.inst_flush inst in
  match spans with
  | [ s ] -> s.status = Some false
  | _ -> false
;;

let%test "to_metrics_json produces valid JSON array" =
  let rm : Eval.run_metrics =
    { run_id = "r9"
    ; agent_name = "a"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = None
    }
  in
  let json = to_metrics_json (extract rm) in
  match json with
  | `List items -> List.length items = 3
  | _ -> false
;;

let%test "to_metrics_json carries run correlation tags" =
  let rm : Eval.run_metrics =
    { run_id = "run-json"
    ; agent_name = "agent-json"
    ; timestamp = 0.0
    ; metrics = []
    ; harness_verdicts = []
    ; trace_summary = None
    }
  in
  match to_metrics_json (extract rm) with
  | `List (`Assoc fields :: _) ->
    (match List.assoc_opt "tags" fields with
     | Some (`Assoc tags) ->
       List.assoc_opt "run_id" tags = Some (`String "run-json")
       && List.assoc_opt "agent_name" tags = Some (`String "agent-json")
     | _ -> false)
  | _ -> false
;;
