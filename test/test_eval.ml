(** Tests for Eval module — evaluation framework. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let mk_metric name value = { Eval.name; value; unit_ = None; tags = [] }

let mk_run_metrics ?(run_id = "r1") ?(agent_name = "test") metrics =
  { Eval.run_id
  ; agent_name
  ; timestamp = 0.0
  ; metrics
  ; harness_verdicts = []
  ; trace_summary = None
  }
;;

(* ── metric_value tests ───────────────────────────────────────── *)

let test_metric_value_yojson_roundtrip () =
  let values = [ Eval.Int_val 42; Float_val 3.14; Bool_val true; String_val "hello" ] in
  List.iter
    (fun v ->
       let json = Eval.metric_value_to_yojson v in
       match Eval.metric_value_of_yojson json with
       | Ok v2 ->
         Alcotest.(check string)
           "roundtrip"
           (Eval.show_metric_value v)
           (Eval.show_metric_value v2)
       | Error e -> Alcotest.fail e)
    values
;;

let test_metric_value_to_float () =
  Alcotest.(check (option (float 0.001)))
    "int"
    (Some 42.0)
    (Eval.metric_value_to_float (Int_val 42));
  Alcotest.(check (option (float 0.001)))
    "float"
    (Some 3.14)
    (Eval.metric_value_to_float (Float_val 3.14));
  Alcotest.(check (option (float 0.001)))
    "bool true"
    (Some 1.0)
    (Eval.metric_value_to_float (Bool_val true));
  Alcotest.(check (option (float 0.001)))
    "string"
    None
    (Eval.metric_value_to_float (String_val "x"))
;;

(* ── metric tests ─────────────────────────────────────────────── *)

let test_metric_yojson_roundtrip () =
  let m =
    { Eval.name = "latency"
    ; value = Float_val 1.5
    ; unit_ = Some "ms"
    ; tags = [ "env", "test" ]
    }
  in
  let json = Eval.metric_to_yojson m in
  match Eval.metric_of_yojson json with
  | Ok m2 ->
    Alcotest.(check string) "name" m.name m2.name;
    Alcotest.(check string)
      "value"
      (Eval.show_metric_value m.value)
      (Eval.show_metric_value m2.value)
  | Error e -> Alcotest.fail e
;;

(* ── collector tests ──────────────────────────────────────────── *)

let test_collector_basic () =
  let c = Eval.create_collector ~agent_name:"a" ~run_id:"r" in
  Eval.record c (mk_metric "x" (Int_val 1));
  Eval.record c (mk_metric "y" (Float_val 2.0));
  let rm = Eval.finalize c in
  Alcotest.(check int) "metrics count" 2 (List.length rm.metrics);
  Alcotest.(check string) "agent" "a" rm.agent_name;
  Alcotest.(check string) "run_id" "r" rm.run_id
;;

let test_collector_verdict () =
  let c = Eval.create_collector ~agent_name:"a" ~run_id:"r" in
  let v = { Harness.passed = true; score = Some 1.0; evidence = []; detail = None } in
  Eval.add_verdict c v;
  let rm = Eval.finalize c in
  Alcotest.(check int) "verdicts" 1 (List.length rm.harness_verdicts)
;;

(* ── comparison tests ─────────────────────────────────────────── *)

let test_compare_regression () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 100.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 120.0) ] in
  let cmp = Eval.compare ~baseline ~candidate in
  Alcotest.(check int) "regressions" 1 (List.length cmp.regressions);
  Alcotest.(check int) "improvements" 0 (List.length cmp.improvements)
;;

let test_compare_improvement () =
  let baseline = mk_run_metrics [ mk_metric "latency" (Float_val 100.0) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "latency" (Float_val 80.0) ] in
  let cmp = Eval.compare ~baseline ~candidate in
  Alcotest.(check int) "regressions" 0 (List.length cmp.regressions);
  Alcotest.(check int) "improvements" 1 (List.length cmp.improvements)
;;

let test_compare_unchanged () =
  let baseline = mk_run_metrics [ mk_metric "score" (Float_val 0.95) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "score" (Float_val 0.96) ] in
  let cmp = Eval.compare ~baseline ~candidate in
  Alcotest.(check int) "unchanged" 1 (List.length cmp.unchanged)
;;

let test_compare_with_specs_higher_is_better () =
  let baseline = mk_run_metrics [ mk_metric "accuracy" (Float_val 0.90) ] in
  let candidate = mk_run_metrics ~run_id:"r2" [ mk_metric "accuracy" (Float_val 0.95) ] in
  let specs =
    [ { Eval.name = "accuracy"; goal = Eval.Higher; tolerance_pct = Some 1.0 } ]
  in
  let cmp = Eval.compare_with_specs ~specs ~baseline ~candidate in
  Alcotest.(check int) "improvements" 1 (List.length cmp.improvements);
  Alcotest.(check int) "regressions" 0 (List.length cmp.regressions)
;;

(* ── threshold tests ──────────────────────────────────────────── *)

let test_threshold_pass () =
  let rm = mk_run_metrics [ mk_metric "latency" (Float_val 50.0) ] in
  let ths =
    [ { Eval.metric_name = "latency"
      ; max_value = Some (Float_val 100.0)
      ; min_value = None
      }
    ]
  in
  let v = Eval.check_thresholds rm ths in
  Alcotest.(check bool) "passed" true v.passed
;;

let test_threshold_fail_max () =
  let rm = mk_run_metrics [ mk_metric "latency" (Float_val 150.0) ] in
  let ths =
    [ { Eval.metric_name = "latency"
      ; max_value = Some (Float_val 100.0)
      ; min_value = None
      }
    ]
  in
  let v = Eval.check_thresholds rm ths in
  Alcotest.(check bool) "failed" false v.passed;
  Alcotest.(check int) "evidence" 1 (List.length v.evidence)
;;

let test_threshold_fail_min () =
  let rm = mk_run_metrics [ mk_metric "score" (Float_val 0.3) ] in
  let ths =
    [ { Eval.metric_name = "score"; max_value = None; min_value = Some (Float_val 0.5) } ]
  in
  let v = Eval.check_thresholds rm ths in
  Alcotest.(check bool) "failed" false v.passed
;;

(* ── lookup tests ─────────────────────────────────────────────── *)

let test_find_metric () =
  let rm = mk_run_metrics [ mk_metric "a" (Int_val 1); mk_metric "b" (Int_val 2) ] in
  (match Eval.find_metric rm "a" with
   | Some m -> Alcotest.(check string) "found" "a" m.name
   | None -> Alcotest.fail "not found");
  Alcotest.(check bool) "missing" true (Option.is_none (Eval.find_metric rm "c"))
;;

(* ── run_metrics serialization ────────────────────────────────── *)

let test_run_metrics_yojson () =
  let rm = mk_run_metrics [ mk_metric "x" (Int_val 42) ] in
  let json = Eval.run_metrics_to_yojson rm in
  match Eval.run_metrics_of_yojson json with
  | Ok rm2 ->
    Alcotest.(check string) "run_id" rm.run_id rm2.run_id;
    Alcotest.(check int) "metrics" 1 (List.length rm2.metrics)
  | Error e -> Alcotest.fail e
;;

(* ── eval_collector tests ─────────────────────────────────────── *)

let test_eval_collector_basic () =
  Eio_main.run
  @@ fun _env ->
  let bus = Event_bus.create () in
  let ec = Eval_collector.wrap_run ~bus ~agent_name:"test" ~run_id:"r1" () in
  Event_bus.publish
    bus
    (Event_bus.mk_event (TurnStarted { agent_name = "test"; turn = 0 }));
  Event_bus.publish
    bus
    (Event_bus.mk_event (TurnStarted { agent_name = "test"; turn = 1 }));
  Event_bus.publish
    bus
    (Event_bus.mk_event
       (ToolCalled { agent_name = "test"; tool_name = "t1"; input = `Null }));
  Event_bus.publish
    bus
    (Event_bus.mk_event
       (ToolCompleted
          { agent_name = "test"
          ; tool_name = "t1"
          ; output = Ok { Types.content = "done" }
          }));
  let rm = Eval_collector.finalize ec in
  (* Check auto-collected metrics *)
  (match Eval.find_metric_value rm "turn_count" with
   | Some (Int_val 2) -> ()
   | _ -> Alcotest.fail "expected turn_count=2");
  (match Eval.find_metric_value rm "tool_calls" with
   | Some (Int_val 1) -> ()
   | _ -> Alcotest.fail "expected tool_calls=1");
  match Eval.find_metric_value rm "tool_completions" with
  | Some (Int_val 1) -> ()
  | _ -> Alcotest.fail "expected tool_completions=1"
;;

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Eval"
    [ ( "metric_value"
      , [ Alcotest.test_case "yojson roundtrip" `Quick test_metric_value_yojson_roundtrip
        ; Alcotest.test_case "to_float" `Quick test_metric_value_to_float
        ] )
    ; ( "metric"
      , [ Alcotest.test_case "yojson roundtrip" `Quick test_metric_yojson_roundtrip ] )
    ; ( "collector"
      , [ Alcotest.test_case "basic" `Quick test_collector_basic
        ; Alcotest.test_case "verdict" `Quick test_collector_verdict
        ] )
    ; ( "comparison"
      , [ Alcotest.test_case "regression" `Quick test_compare_regression
        ; Alcotest.test_case "improvement" `Quick test_compare_improvement
        ; Alcotest.test_case "unchanged" `Quick test_compare_unchanged
        ; Alcotest.test_case
            "spec higher better"
            `Quick
            test_compare_with_specs_higher_is_better
        ] )
    ; ( "threshold"
      , [ Alcotest.test_case "pass" `Quick test_threshold_pass
        ; Alcotest.test_case "fail max" `Quick test_threshold_fail_max
        ; Alcotest.test_case "fail min" `Quick test_threshold_fail_min
        ] )
    ; "lookup", [ Alcotest.test_case "find_metric" `Quick test_find_metric ]
    ; ( "serialization"
      , [ Alcotest.test_case "run_metrics yojson" `Quick test_run_metrics_yojson ] )
    ; ( "eval_collector"
      , [ Alcotest.test_case "basic auto-collect" `Quick test_eval_collector_basic ] )
    ]
;;
