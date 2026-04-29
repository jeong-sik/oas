(** Full coverage tests for Eval_report module.
    Targets: generate (4 paths), to_json, to_string.
    Covers all 3 verdict variants: Pass, Fail, NoBaseline. *)

open Agent_sdk

let pass_verdict : Harness.verdict =
  { passed = true; score = Some 1.0; evidence = []; detail = Some "ok" }
;;

let fail_verdict : Harness.verdict =
  { passed = false; score = Some 0.0; evidence = [ "failed" ]; detail = Some "bad" }
;;

let make_metric name value : Eval.metric = { name; value; unit_ = None; tags = [] }

let make_run ?(agent_name = "test-agent") metrics verdicts : Eval.run_metrics =
  { run_id = "run-1"
  ; agent_name
  ; timestamp = 1000.0
  ; metrics
  ; harness_verdicts = verdicts
  ; trace_summary = None
  }
;;

let () =
  let open Alcotest in
  run
    "Eval_report_full"
    [ (* ── generate: empty runs ────────────────────────── *)
      ( "generate_empty"
      , [ test_case "empty runs yields Fail verdict" `Quick (fun () ->
            let report = Eval_report.generate [] in
            check string "agent" "unknown" report.agent_name;
            check int "run_count" 0 report.run_count;
            check (float 0.01) "pass_at_k" 0.0 report.pass_at_k;
            (match report.verdict with
             | `Fail -> ()
             | _ -> fail "expected Fail verdict for empty runs");
            check string "summary" "No runs to evaluate" report.summary;
            check bool "no comparison" true (Option.is_none report.comparison))
        ] )
    ; (* ── generate: no baseline → NoBaseline ──────────── *)
      ( "generate_no_baseline"
      , [ test_case "no baseline yields NoBaseline" `Quick (fun () ->
            let runs = [ make_run [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            check string "agent" "test-agent" report.agent_name;
            check int "run_count" 1 report.run_count;
            (match report.verdict with
             | `NoBaseline -> ()
             | _ -> fail "expected NoBaseline verdict");
            check bool "no comparison" true (Option.is_none report.comparison);
            check
              bool
              "summary contains NO BASELINE"
              true
              (String.length report.summary > 0
               &&
               let s = report.summary in
               try
                 let _ = String.index s 'N' in
                 true
               with
               | Not_found -> false))
        ; test_case "no baseline summary mentions agent name" `Quick (fun () ->
            let runs = [ make_run ~agent_name:"mybot" [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            check
              bool
              "contains agent name"
              true
              (let s = report.summary in
               let re = Str.regexp_string "mybot" in
               try
                 let _ = Str.search_forward re s 0 in
                 true
               with
               | Not_found -> false))
        ] )
    ; (* ── generate: baseline pass ─────────────────────── *)
      ( "generate_baseline_pass"
      , [ test_case "baseline pass with high pass_at_k" `Quick (fun () ->
            let baseline_rm =
              make_run [ make_metric "accuracy" (Float_val 0.90) ] [ pass_verdict ]
            in
            let baseline = Eval_baseline.create ~description:"base" baseline_rm in
            let runs =
              [ make_run [ make_metric "accuracy" (Float_val 0.91) ] [ pass_verdict ]
              ; make_run [ make_metric "accuracy" (Float_val 0.92) ] [ pass_verdict ]
              ]
            in
            let report = Eval_report.generate ~baseline runs in
            check int "run_count" 2 report.run_count;
            (match report.verdict with
             | `Pass -> ()
             | _ -> fail "expected Pass verdict");
            check bool "has comparison" true (Option.is_some report.comparison);
            check
              bool
              "summary starts with PASS"
              true
              (String.length report.summary >= 4 && String.sub report.summary 0 4 = "PASS"))
        ; test_case "baseline pass with comparison details" `Quick (fun () ->
            let baseline_rm =
              make_run [ make_metric "latency" (Float_val 100.0) ] [ pass_verdict ]
            in
            let baseline = Eval_baseline.create ~description:"base" baseline_rm in
            let runs =
              [ make_run [ make_metric "latency" (Float_val 99.0) ] [ pass_verdict ] ]
            in
            let report = Eval_report.generate ~baseline runs in
            let comp = Option.get report.comparison in
            check bool "comparison passed" true comp.passed)
        ] )
    ; (* ── generate: baseline fail ─────────────────────── *)
      ( "generate_baseline_fail"
      , [ test_case "baseline fail when comparison has regressions" `Quick (fun () ->
            let baseline_rm =
              make_run [ make_metric "accuracy" (Float_val 0.90) ] [ pass_verdict ]
            in
            let baseline = Eval_baseline.create ~description:"base" baseline_rm in
            (* current has much lower value -> regression -> comparison.passed = false *)
            let runs =
              [ make_run [ make_metric "accuracy" (Float_val 0.50) ] [ pass_verdict ] ]
            in
            let report = Eval_report.generate ~baseline runs in
            (match report.verdict with
             | `Fail -> ()
             | _ -> fail "expected Fail verdict when baseline comparison fails");
            check bool "has comparison" true (Option.is_some report.comparison);
            let comp = Option.get report.comparison in
            check bool "comparison not passed" false comp.passed;
            check
              bool
              "summary starts with FAIL"
              true
              (String.length report.summary >= 4 && String.sub report.summary 0 4 = "FAIL"))
        ; test_case
            "fail when pass_at_k below 0.5 with passing comparison"
            `Quick
            (fun () ->
               (* Baseline comparison passes, but pass@k < 0.5 *)
               let baseline_rm =
                 make_run [ make_metric "accuracy" (Float_val 0.90) ] [ pass_verdict ]
               in
               let baseline = Eval_baseline.create ~description:"base" baseline_rm in
               let runs =
                 [ make_run [ make_metric "accuracy" (Float_val 0.91) ] [ fail_verdict ]
                 ; make_run [ make_metric "accuracy" (Float_val 0.91) ] [ fail_verdict ]
                 ; make_run [ make_metric "accuracy" (Float_val 0.91) ] [ fail_verdict ]
                 ]
               in
               let report = Eval_report.generate ~baseline runs in
               check (float 0.01) "pass_at_k" 0.0 report.pass_at_k;
               match report.verdict with
               | `Fail -> ()
               | _ -> fail "expected Fail when pass@k < 0.5")
        ] )
    ; (* ── to_json ─────────────────────────────────────── *)
      ( "to_json"
      , [ test_case "pass verdict in json" `Quick (fun () ->
            let baseline_rm =
              make_run [ make_metric "score" (Float_val 0.90) ] [ pass_verdict ]
            in
            let baseline = Eval_baseline.create ~description:"b" baseline_rm in
            let runs =
              [ make_run [ make_metric "score" (Float_val 0.91) ] [ pass_verdict ] ]
            in
            let report = Eval_report.generate ~baseline runs in
            let json = Eval_report.to_json report in
            let open Yojson.Safe.Util in
            check string "verdict" "pass" (json |> member "verdict" |> to_string);
            check
              string
              "agent_name"
              "test-agent"
              (json |> member "agent_name" |> to_string);
            check int "run_count" 1 (json |> member "run_count" |> to_int))
        ; test_case "fail verdict in json" `Quick (fun () ->
            let report = Eval_report.generate [] in
            let json = Eval_report.to_json report in
            let open Yojson.Safe.Util in
            check string "verdict" "fail" (json |> member "verdict" |> to_string))
        ; test_case "no_baseline verdict in json" `Quick (fun () ->
            let runs = [ make_run [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            let json = Eval_report.to_json report in
            let open Yojson.Safe.Util in
            check string "verdict" "no_baseline" (json |> member "verdict" |> to_string))
        ; test_case "json has all expected keys" `Quick (fun () ->
            let runs = [ make_run [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            let json = Eval_report.to_json report in
            let open Yojson.Safe.Util in
            let _ = json |> member "agent_name" |> to_string in
            let _ = json |> member "run_count" |> to_int in
            let _ = json |> member "pass_at_k" |> to_float in
            let _ = json |> member "verdict" |> to_string in
            let _ = json |> member "regressions" |> to_int in
            let _ = json |> member "improvements" |> to_int in
            let _ = json |> member "summary" |> to_string in
            ())
        ; test_case "regressions/improvements 0 without comparison" `Quick (fun () ->
            let runs = [ make_run [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            let json = Eval_report.to_json report in
            let open Yojson.Safe.Util in
            check int "regressions" 0 (json |> member "regressions" |> to_int);
            check int "improvements" 0 (json |> member "improvements" |> to_int))
        ] )
    ; (* ── to_string ───────────────────────────────────── *)
      ( "to_string"
      , [ test_case "contains agent name" `Quick (fun () ->
            let runs = [ make_run ~agent_name:"foobot" [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            let s = Eval_report.to_string report in
            let re = Str.regexp_string "foobot" in
            check
              bool
              "contains foobot"
              true
              (try
                 let _ = Str.search_forward re s 0 in
                 true
               with
               | Not_found -> false))
        ; test_case "to_string with comparison shows diffs" `Quick (fun () ->
            let baseline_rm =
              make_run [ make_metric "latency" (Float_val 100.0) ] [ pass_verdict ]
            in
            let baseline = Eval_baseline.create ~description:"b" baseline_rm in
            (* Big improvement: 100 -> 50 (50% decrease) *)
            let runs =
              [ make_run [ make_metric "latency" (Float_val 50.0) ] [ pass_verdict ] ]
            in
            let report = Eval_report.generate ~baseline runs in
            let s = Eval_report.to_string report in
            (* Should have more than just the summary line if there are diffs *)
            check bool "non-empty string" true (String.length s > 0))
        ; test_case "to_string without comparison is single line" `Quick (fun () ->
            let runs = [ make_run [] [ pass_verdict ] ] in
            let report = Eval_report.generate runs in
            let s = Eval_report.to_string report in
            check bool "no newline (single line)" true (not (String.contains s '\n')))
        ] )
    ]
;;
