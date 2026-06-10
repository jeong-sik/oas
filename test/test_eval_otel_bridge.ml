(** External test placeholder for Eval_otel_bridge.

    Core tests are inline (%test) in lib/eval_otel_bridge.ml and run
    via [dune runtest lib/].  This file verifies the module is
    accessible from external consumers and exercises the JSON export
    path end-to-end. *)

open Agent_sdk

let () =
  let open Alcotest in
  let make_verdict ?(passed = true) () : Harness.verdict =
    { passed; score = None; evidence = []; detail = None }
  in
  let make_run_metrics ?(verdicts = []) () : Eval.run_metrics =
    { run_id = "test-run-1"
    ; agent_name = "test-agent"
    ; timestamp = 1000.0
    ; metrics = []
    ; harness_verdicts = verdicts
    ; trace_summary = None
    }
  in
  let test_extract_accessible () =
    let rm = make_run_metrics () in
    let snap = Eval_otel_bridge.extract rm in
    check int "passed" 0 snap.verdict_passed_total;
    check (float 0.01) "coverage" 1.0 snap.coverage
  in
  let test_metric_list_count () =
    let rm =
      make_run_metrics
        ~verdicts:[ make_verdict ~passed:true (); make_verdict ~passed:false () ]
        ()
    in
    let snap = Eval_otel_bridge.extract rm in
    let metrics = Eval_otel_bridge.to_metric_list snap in
    check int "3 required metrics" 3 (List.length metrics)
  in
  let test_json_export () =
    let rm = make_run_metrics ~verdicts:[ make_verdict ~passed:true () ] () in
    let snap = Eval_otel_bridge.extract rm in
    let json = Eval_otel_bridge.to_metrics_json snap in
    match json with
    | `List items ->
      check int "3 metrics" 3 (List.length items);
      let first = List.hd items in
      let name = Yojson.Safe.Util.(first |> member "name" |> to_string) in
      check string "first name" "oas.eval.verdict_passed_total" name;
      let run_id =
        Yojson.Safe.Util.(first |> member "tags" |> member "run_id" |> to_string)
      in
      let agent_name =
        Yojson.Safe.Util.(first |> member "tags" |> member "agent_name" |> to_string)
      in
      check string "run id tag" "test-run-1" run_id;
      check string "agent name tag" "test-agent" agent_name
    | _ -> fail "expected JSON list"
  in
  let test_default_instance_records_metrics () =
    let inst = Eval_otel_bridge.default_instance () in
    Otel_tracer.inst_clear_metrics inst;
    ignore (Otel_tracer.inst_flush inst : Otel_tracer.span list);
    let rm = make_run_metrics ~verdicts:[ make_verdict ~passed:true () ] () in
    Eval_otel_bridge.emit_run_metrics_default rm;
    let metrics = Otel_tracer.inst_get_metrics inst in
    check bool
      "shared instance has eval metric"
      true
      (List.exists
         (fun (name, _, _) -> String.equal name "oas.eval.verdict_passed_total")
         metrics)
  in
  run
    "Eval_otel_bridge (external)"
    [ ( "module access"
      , [ test_case "extract accessible" `Quick test_extract_accessible
        ; test_case "metric list count" `Quick test_metric_list_count
        ; test_case "json export" `Quick test_json_export
        ; test_case
            "default instance records metrics"
            `Quick
            test_default_instance_records_metrics
        ] )
    ]
;;
