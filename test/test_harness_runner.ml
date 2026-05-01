open Base
(** Tests for Harness_runner and Harness_report. *)

open Agent_sdk
open Alcotest

let ok_response text : Types.api_response =
  { id = "resp-1"
  ; model = "mock-model"
  ; stop_reason = EndTurn
  ; content = [ Text text ]
  ; usage =
      Some
        { input_tokens = 10
        ; output_tokens = 5
        ; cache_creation_input_tokens = 0
        ; cache_read_input_tokens = 0
        ; cost_usd = None
        }
  ; telemetry = None
  }
;;

let mk_observation ?(tools_called = []) ?(turn_count = 1) ?(final_response = "done") ()
  : Harness.Behavioral.observation
  =
  { tools_called; turn_count; final_response; messages = [] }
;;

let mk_trajectory ?(success = true) ?(tool_names = []) ?(response_text = "done") ()
  : Trajectory.trajectory
  =
  let steps =
    (tool_names
     |> List.mapi (fun index tool_name ->
       Trajectory.Act
         { tool_call =
             { tool_use_id = Printf.sprintf "tool-%d" index
             ; tool_name
             ; tool_input = `Null
             ; tool_result = None
             ; is_error = false
             ; started_at = Float.of_int index
             ; finished_at = Some (Float.of_int (index + 1))
             }
         ; ts = Float.of_int index
         }))
    @ [ Trajectory.Respond { content = response_text; ts = 10.0 } ]
  in
  { agent_name = "runner-agent"
  ; model = "mock-model"
  ; prompt = "test"
  ; steps
  ; started_at = 0.0
  ; finished_at = Some 1.0
  ; success
  ; metrics = None
  ; error = None
  }
;;

let with_trace_file name body =
  let trace_path = Printf.sprintf "/tmp/oas_harness_%s_%d.ndjson" name (Unix.getpid ()) in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove trace_path with
      | _ -> ())
    (fun () ->
       Out_channel.with_open_text trace_path (fun oc ->
         output_string
           oc
           {|{"trace_version":1,"worker_run_id":"wr-file","seq":1,"ts":1.0,"agent_name":"runner-agent","session_id":null,"record_type":"run_started","prompt":"Replay this","block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":null,"stop_reason":null,"error":null}|};
         output_char oc '\n';
         output_string
           oc
           {|{"trace_version":1,"worker_run_id":"wr-file","seq":2,"ts":2.0,"agent_name":"runner-agent","session_id":null,"record_type":"run_finished","prompt":null,"block_index":null,"block_kind":null,"assistant_block":null,"tool_use_id":null,"tool_name":null,"tool_input":null,"tool_result":null,"tool_error":null,"hook_name":null,"hook_decision":null,"hook_detail":null,"final_text":"done","stop_reason":"end_turn","error":null}|};
         output_char oc '\n');
       body trace_path)
;;

let trace_case ~id ~trace_path =
  Harness_case.make_trace_replay
    ~assertions:
      [ Harness_case.Response (Harness_case.Exact_text "done")
      ; Harness_case.Trace Harness_case.Succeeds
      ]
    ~id
    ~prompt:"Replay this"
    ~source_trace_path:trace_path
    ()
;;

let test_grade_case_fixture () =
  let case_ =
    Harness_case.make_fixture
      ~assertions:
        [ Harness_case.Response (Harness_case.Exact_text "hello world")
        ; Harness_case.Trace Harness_case.Succeeds
        ; Harness_case.Metric
            { name = "elapsed_s"
            ; goal = Eval.Lower
            ; target = Eval.Float_val 5.0
            ; tolerance_pct = None
            }
        ]
      ~id:"fixture-pass"
      ~prompt:"Say hello"
      ()
  in
  let result =
    Harness_runner.grade_case
      ~agent_name:"runner-agent"
      ~elapsed:0.3
      ~response:(Ok (ok_response "hello world"))
      ~observation:(mk_observation ~final_response:"hello world" ())
      case_
  in
  check bool "passed" true (result.status = Harness_report.Pass);
  check bool "has metrics" true (Option.is_some result.metrics)
;;

let test_grade_case_trace_replay () =
  let case_ =
    match
      Harness_case.trace_replay_of_records
        ~id:"trace-replay"
        ~source_trace_path:"/tmp/trace-replay.ndjson"
        [ { Raw_trace.trace_version = 1
          ; worker_run_id = "wr-1"
          ; seq = 1
          ; ts = 1.0
          ; agent_name = "runner-agent"
          ; session_id = None
          ; record_type = Raw_trace.Run_started
          ; prompt = Some "Replay this"
          ; model = None
          ; tool_choice = None
          ; enable_thinking = None
          ; thinking_budget = None
          ; block_index = None
          ; block_kind = None
          ; assistant_block = None
          ; tool_use_id = None
          ; tool_name = None
          ; tool_input = None
          ; tool_planned_index = None
          ; tool_batch_index = None
          ; tool_batch_size = None
          ; tool_concurrency_class = None
          ; tool_result = None
          ; tool_error = None
          ; hook_name = None
          ; hook_decision = None
          ; hook_detail = None
          ; final_text = None
          ; stop_reason = None
          ; error = None
          }
        ; { Raw_trace.trace_version = 1
          ; worker_run_id = "wr-1"
          ; seq = 2
          ; ts = 2.0
          ; agent_name = "runner-agent"
          ; session_id = None
          ; record_type = Raw_trace.Run_finished
          ; prompt = None
          ; model = None
          ; tool_choice = None
          ; enable_thinking = None
          ; thinking_budget = None
          ; block_index = None
          ; block_kind = None
          ; assistant_block = None
          ; tool_use_id = None
          ; tool_name = None
          ; tool_input = None
          ; tool_planned_index = None
          ; tool_batch_index = None
          ; tool_batch_size = None
          ; tool_concurrency_class = None
          ; tool_result = None
          ; tool_error = None
          ; hook_name = None
          ; hook_decision = None
          ; hook_detail = None
          ; final_text = Some "done"
          ; stop_reason = Some "end_turn"
          ; error = None
          }
        ]
    with
    | Ok case_ -> case_
    | Error e -> fail (Error.to_string e)
  in
  let result =
    Harness_runner.grade_case
      ~agent_name:"runner-agent"
      ~elapsed:0.1
      ~response:(Ok (ok_response "done"))
      ~observation:(mk_observation ~final_response:"done" ())
      ~trajectory:(mk_trajectory ())
      case_
  in
  let report = Harness_report.of_results [ result ] in
  check int "total" 1 report.summary.total;
  check int "passed" 1 report.summary.passed;
  check
    bool
    "markdown non-empty"
    true
    (String.length (Harness_report.to_markdown report) > 0)
;;

let test_grade_case_from_trace_file () =
  with_trace_file "trace" (fun trace_path ->
    let case_ = trace_case ~id:"trace-file" ~trace_path in
    match Harness_runner.grade_case_from_trace case_ with
    | Error e -> fail (Error.to_string e)
    | Ok result ->
      check bool "passed" true (result.status = Harness_report.Pass);
      check bool "raw trace path kept" true (result.raw_trace_path = Some trace_path))
;;

let test_grade_case_from_trace_rejects_fixture_kind () =
  let case_ =
    Harness_case.make_fixture
      ~assertions:[ Harness_case.Response (Harness_case.Exact_text "done") ]
      ~id:"fixture-kind"
      ~prompt:"noop"
      ()
  in
  match Harness_runner.grade_case_from_trace case_ with
  | Ok _ -> fail "expected fixture kind to be rejected"
  | Error _ -> ()
;;

let test_grade_case_negative_metric_tolerance () =
  let case_ =
    Harness_case.make_fixture
      ~assertions:
        [ Harness_case.Metric
            { name = "elapsed_s"
            ; goal = Eval.Lower
            ; target = Eval.Float_val 1.0
            ; tolerance_pct = Some (-1.0)
            }
        ]
      ~id:"negative-tolerance"
      ~prompt:"noop"
      ()
  in
  let result =
    Harness_runner.grade_case
      ~agent_name:"runner-agent"
      ~elapsed:0.1
      ~response:(Ok (ok_response "done"))
      ~observation:(mk_observation ())
      case_
  in
  check bool "failed" true (result.status = Harness_report.Fail)
;;

let test_run_dataset_mixed_dispatches_by_kind () =
  with_trace_file "mixed" (fun trace_path ->
    let fixture_case =
      Harness_case.make_fixture
        ~assertions:[ Harness_case.Response (Harness_case.Exact_text "fixture ok") ]
        ~id:"fixture-live"
        ~prompt:"noop"
        ()
    in
    let run_fixture case_ =
      Harness_runner.grade_case
        ~agent_name:"runner-agent"
        ~elapsed:0.1
        ~response:(Ok (ok_response "fixture ok"))
        ~observation:(mk_observation ~final_response:"fixture ok" ())
        case_
    in
    let report =
      Harness_runner.run_dataset_mixed
        ~run_fixture
        [ fixture_case; trace_case ~id:"trace-offline" ~trace_path ]
    in
    check int "total" 2 report.summary.total;
    check int "passed" 2 report.summary.passed;
    check int "failed" 0 report.summary.failed)
;;

let test_run_dataset_mixed_without_fixture_runner_fails_fixture_only () =
  with_trace_file "mixed-no-config" (fun trace_path ->
    let fixture_case =
      Harness_case.make_fixture
        ~assertions:[ Harness_case.Trace Harness_case.Succeeds ]
        ~id:"fixture-needs-config"
        ~prompt:"noop"
        ()
    in
    let report =
      Harness_runner.run_dataset_mixed
        [ fixture_case; trace_case ~id:"trace-offline" ~trace_path ]
    in
    check int "total" 2 report.summary.total;
    check int "passed" 1 report.summary.passed;
    check int "failed" 1 report.summary.failed;
    let fixture_result =
      match
        List.find_opt
          (fun (result : Harness_report.case_result) ->
             result.case_id = "fixture-needs-config")
          report.results
      with
      | Some result -> result
      | None -> fail "missing fixture result"
    in
    check bool "fixture failed" true (fixture_result.status = Harness_report.Fail);
    check
      bool
      "detail mentions config"
      true
      (match fixture_result.detail with
       | Some detail -> Util.string_contains ~needle:"--config" detail
       | None -> false))
;;

let () =
  run
    "harness_runner"
    [ ( "grade_case"
      , [ test_case "fixture" `Quick test_grade_case_fixture
        ; test_case "trace replay" `Quick test_grade_case_trace_replay
        ; test_case "from trace file" `Quick test_grade_case_from_trace_file
        ; test_case
            "rejects fixture in trace mode"
            `Quick
            test_grade_case_from_trace_rejects_fixture_kind
        ; test_case
            "rejects negative tolerance"
            `Quick
            test_grade_case_negative_metric_tolerance
        ; test_case
            "mixed dataset dispatches by kind"
            `Quick
            test_run_dataset_mixed_dispatches_by_kind
        ; test_case
            "mixed dataset without fixture runner fails fixture only"
            `Quick
            test_run_dataset_mixed_without_fixture_runner_fails_fixture_only
        ] )
    ]
;;
