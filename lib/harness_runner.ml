(** Execute harness cases against fresh agents and emit structured reports. *)

let mk_verdict ?score ~detail passed evidence : Harness.verdict =
  { passed; score; evidence; detail }

let case_failure ?detail ?response_text ?metrics ?raw_trace_path
    (case_ : Harness_case.t) =
  {
    Harness_report.case_id = case_.id;
    kind = case_.kind;
    status = Harness_report.Fail;
    verdicts = [];
    evidence = [];
    detail;
    response_text;
    raw_trace_path = Util.first_some raw_trace_path case_.source_trace_path;
    metrics;
  }

let response_text_of_result = function
  | Ok (response : Types.api_response) ->
    response.content
    |> List.filter_map (function Types.Text text -> Some text | _ -> None)
    |> String.concat "\n"
  | Error _ -> ""

let final_response_of_trajectory (trajectory : Trajectory.trajectory) =
  trajectory.steps
  |> List.rev
  |> List.find_opt (function Trajectory.Respond _ -> true | _ -> false)
  |> Option.map (function
       | Trajectory.Respond { content; _ } -> content
       | _ -> "")
  |> Option.value ~default:""

let result_usage = function
  | Ok (response : Types.api_response) -> response.usage
  | Error _ -> None

let trajectory_of_trace_ref = function
  | None -> None
  | Some (trace_ref : Raw_trace.run_ref) ->
    (match Raw_trace.read_all ~path:trace_ref.path () with
     | Ok records -> Some (Trajectory.of_raw_trace_records records)
     | Error _ -> None)

let observation_of_trajectory (trajectory : Trajectory.trajectory) =
  let tools_called =
    trajectory.steps
    |> List.filter_map (function
         | Trajectory.Act { tool_call; _ } -> Some tool_call.tool_name
         | _ -> None)
  in
  let _, _, _, respond_count = Trajectory.count_steps trajectory in
  {
    Harness.Behavioral.tools_called;
    turn_count = max 1 respond_count;
    final_response = final_response_of_trajectory trajectory;
    messages = [];
  }

let response_of_trajectory (trajectory : Trajectory.trajectory) =
  let response_text = final_response_of_trajectory trajectory in
  if trajectory.success then
    Ok {
      Types.id = "trace-replay";
      model = trajectory.model;
      stop_reason = Types.EndTurn;
      content = [Types.Text response_text];
      usage = None;
      telemetry = None;
    }
  else
    Error (Error.Internal (Option.value ~default:"trace replay failed" trajectory.error))

let tool_sequence ?trajectory (obs : Harness.Behavioral.observation) =
  match trajectory with
  | Some (trajectory : Trajectory.trajectory) ->
    trajectory.steps
    |> List.filter_map (function
         | Trajectory.Act { tool_call; _ } -> Some tool_call.tool_name
         | _ -> None)
  | None -> obs.tools_called

let response_verdict response_text = function
  | Harness_case.Exact_text expected ->
    mk_verdict
      (response_text = expected)
      [
        Printf.sprintf "expected=%s" expected;
        Printf.sprintf "actual=%s" response_text;
      ]
      ~detail:(if response_text = expected then None else Some "response text mismatch")
  | Harness_case.Contains_text needle ->
    let passed = Util.string_contains ~needle response_text in
    mk_verdict passed
      [
        Printf.sprintf "needle=%s" needle;
        Printf.sprintf "response=%s" response_text;
      ]
      ~detail:(if passed then None else Some "response text did not contain expected substring")
  | Harness_case.Structural_json expected_json ->
    (match try Ok (Yojson.Safe.from_string response_text)
     with Yojson.Json_error msg -> Error msg with
     | Error msg ->
       mk_verdict false
         [
           Printf.sprintf "expected_json=%s" (Yojson.Safe.to_string expected_json);
           Printf.sprintf "parse_error=%s" msg;
         ]
         ~detail:(Some "response text was not valid JSON")
     | Ok actual_json ->
       let passed = actual_json = expected_json in
       mk_verdict passed
         [
           Printf.sprintf "expected_json=%s" (Yojson.Safe.to_string expected_json);
           Printf.sprintf "actual_json=%s" (Yojson.Safe.to_string actual_json);
         ]
         ~detail:(if passed then None else Some "response JSON structure mismatch"))
  | Harness_case.Fuzzy_text { expected; threshold } ->
    Harness.Regression.evaluate
      ~mode:(Harness.Regression.FuzzyMatch { threshold })
      { output_json = `Null; output_text = response_text }
      expected

let trace_verdict ?trajectory (obs : Harness.Behavioral.observation)
    (response : (Types.api_response, Error.sdk_error) result) = function
  | Harness_case.Succeeds ->
    let passed = Result.is_ok response in
    mk_verdict passed
      [Printf.sprintf "success=%b" passed]
      ~detail:(if passed then None else Some "agent run failed")
  | Harness_case.Tool_called name ->
    let passed = List.mem name obs.tools_called in
    mk_verdict passed
      [
        Printf.sprintf "expected_tool=%s" name;
        Printf.sprintf "actual_tools=%s" (String.concat "," obs.tools_called);
      ]
      ~detail:(if passed then None else Some "expected tool was not called")
  | Harness_case.Tool_sequence expected ->
    let actual = tool_sequence ?trajectory obs in
    let passed = actual = expected in
    mk_verdict passed
      [
        Printf.sprintf "expected_sequence=%s" (String.concat "," expected);
        Printf.sprintf "actual_sequence=%s" (String.concat "," actual);
      ]
      ~detail:(if passed then None else Some "tool sequence mismatch")
  | Harness_case.Tool_call_count expected ->
    let actual = List.length obs.tools_called in
    let passed = actual = expected in
    mk_verdict passed
      [
        Printf.sprintf "expected_tool_calls=%d" expected;
        Printf.sprintf "actual_tool_calls=%d" actual;
      ]
      ~detail:(if passed then None else Some "tool call count mismatch")
  | Harness_case.Max_turns expected ->
    let passed = obs.turn_count <= expected in
    mk_verdict ~score:(if passed then 1.0 else 0.0) passed
      [
        Printf.sprintf "max_turns=%d" expected;
        Printf.sprintf "actual_turns=%d" obs.turn_count;
      ]
      ~detail:(if passed then None else Some "turn limit exceeded")

let compare_numeric_threshold ~goal ~target ~tolerance_pct current =
  let tolerance = tolerance_pct /. 100.0 in
  match goal with
  | Eval.Higher ->
    let floor =
      if Float.abs target < 1e-10 then target
      else target *. (1.0 -. tolerance)
    in
    (current >= floor, floor)
  | Eval.Lower ->
    let ceil =
      if Float.abs target < 1e-10 then target
      else target *. (1.0 +. tolerance)
    in
    (current <= ceil, ceil)
  | Eval.Exact ->
    let allowed =
      if Float.abs target < 1e-10 then tolerance
      else Float.abs target *. tolerance
    in
    (Float.abs (current -. target) <= allowed, target)

let metric_verdict (metrics : Eval.run_metrics) (assertion : Harness_case.metric_assertion) =
  let tolerance_pct =
    Option.value assertion.tolerance_pct ~default:0.0
  in
  if tolerance_pct < 0.0 then
    mk_verdict false
      [Printf.sprintf "metric=%s" assertion.name]
      ~detail:(Some "metric tolerance_pct must be non-negative")
  else
    match Eval.find_metric metrics assertion.name with
    | None ->
      mk_verdict false
        [Printf.sprintf "missing_metric=%s" assertion.name]
        ~detail:(Some "metric was not collected")
    | Some metric ->
      (match Eval.metric_value_to_float metric.value, Eval.metric_value_to_float assertion.target with
       | Some current, Some target ->
         let passed, bound =
           compare_numeric_threshold ~goal:assertion.goal ~target ~tolerance_pct current
         in
         mk_verdict passed
           [
             Printf.sprintf "metric=%s" assertion.name;
             Printf.sprintf "goal=%s"
               (match assertion.goal with
                | Eval.Higher -> "higher"
                | Eval.Lower -> "lower"
                | Eval.Exact -> "exact");
             Printf.sprintf "current=%.4f" current;
             Printf.sprintf "target=%.4f" target;
             Printf.sprintf "bound=%.4f" bound;
           ]
           ~detail:(if passed then None else Some "metric threshold not met")
       | _ ->
         let passed =
           match assertion.goal with
           | Eval.Exact -> metric.value = assertion.target
           | _ -> false
         in
         mk_verdict passed
           [
             Printf.sprintf "metric=%s" assertion.name;
             Printf.sprintf "current=%s" (Eval.show_metric_value metric.value);
             Printf.sprintf "target=%s" (Eval.show_metric_value assertion.target);
           ]
           ~detail:(if passed then None else Some "non-numeric metric requires exact match"))

let collect_metrics ~agent_name ~run_id ~(obs : Harness.Behavioral.observation)
    ~(response : (Types.api_response, Error.sdk_error) result) ~elapsed =
  let collector = Eval.create_collector ~agent_name ~run_id in
  Eval.record collector {
    name = "turn_count";
    value = Eval.Int_val obs.turn_count;
    unit_ = None;
    tags = [];
  };
  Eval.record collector {
    name = "tool_calls";
    value = Eval.Int_val (List.length obs.tools_called);
    unit_ = None;
    tags = [];
  };
  Eval.record collector {
    name = "elapsed_s";
    value = Eval.Float_val elapsed;
    unit_ = Some "seconds";
    tags = [];
  };
  Eval.record collector {
    name = "success";
    value = Eval.Bool_val (Result.is_ok response);
    unit_ = None;
    tags = [];
  };
  (match result_usage response with
   | None -> ()
   | Some usage ->
     Eval.record collector {
       name = "input_tokens";
       value = Eval.Int_val usage.input_tokens;
       unit_ = None;
       tags = [];
     };
     Eval.record collector {
       name = "output_tokens";
       value = Eval.Int_val usage.output_tokens;
       unit_ = None;
       tags = [];
     });
  Eval.finalize collector

let evaluate_assertion ?trajectory obs response response_text metrics = function
  | Harness_case.Response assertion ->
    response_verdict response_text assertion
  | Harness_case.Trace assertion ->
    trace_verdict ?trajectory obs response assertion
  | Harness_case.Metric assertion ->
    metric_verdict metrics assertion

let grade_case ~agent_name ~elapsed ~response ~observation ?trajectory ?raw_trace_path
    (case_ : Harness_case.t) =
  let response_text = response_text_of_result response in
  let metrics =
    collect_metrics
      ~agent_name
      ~run_id:case_.id
      ~obs:observation
      ~response
      ~elapsed
  in
  let verdicts =
    List.map (evaluate_assertion ?trajectory observation response response_text metrics)
      case_.assertions
  in
  let collector = Eval.create_collector
    ~agent_name:metrics.agent_name ~run_id:metrics.run_id
  in
  List.iter (Eval.record collector) metrics.metrics;
  List.iter (Eval.add_verdict collector) verdicts;
  let metrics = Eval.finalize collector in
  let status, detail =
    match case_.assertions with
    | [] -> (Harness_report.Skip, Some "case had no assertions")
    | _ ->
      if List.for_all (fun (verdict : Harness.verdict) -> verdict.passed) verdicts
      then (Harness_report.Pass, None)
      else
        let first_failure =
          match List.find_opt (fun (verdict : Harness.verdict) -> not verdict.passed) verdicts with
          | Some verdict -> verdict.detail
          | None -> None
        in
        (Harness_report.Fail, first_failure)
  in
  {
    Harness_report.case_id = case_.id;
    kind = case_.kind;
    status;
    verdicts;
    evidence = List.concat_map (fun (verdict : Harness.verdict) -> verdict.evidence) verdicts;
    detail;
    response_text = Some response_text;
    raw_trace_path = Util.first_some raw_trace_path case_.source_trace_path;
    metrics = Some metrics;
  }

let grade_case_from_trace (case_ : Harness_case.t) =
  match case_.kind, case_.source_trace_path with
  | Harness_case.Fixture, _ ->
    Error (Error.Io (Error.ValidationFailed {
      detail = Printf.sprintf "case '%s' is not a trace_replay case" case_.id;
    }))
  | _, None ->
    Error (Error.Io (Error.ValidationFailed {
      detail = Printf.sprintf "case '%s' does not define source_trace_path" case_.id;
    }))
  | Harness_case.Trace_replay, Some path ->
    (match Raw_trace.read_all ~path () with
     | Error _ as err -> err
     | Ok records ->
       let trajectory = Trajectory.of_raw_trace_records records in
       let observation = observation_of_trajectory trajectory in
       let response = response_of_trajectory trajectory in
       let elapsed = Option.value ~default:0.0 (Trajectory.elapsed_s trajectory) in
       Ok (grade_case
         ~agent_name:trajectory.agent_name
         ~elapsed
         ~response
         ~observation
         ~trajectory
         ~raw_trace_path:path
         case_))

let execute_case ?run_fixture (case_ : Harness_case.t) =
  match case_.kind with
  | Harness_case.Trace_replay ->
    (match grade_case_from_trace case_ with
     | Ok result -> result
     | Error e -> case_failure case_ ~detail:(Error.to_string e))
  | Harness_case.Fixture ->
    (match run_fixture with
     | Some run_fixture -> run_fixture case_
     | None ->
       case_failure case_
         ~detail:(
           (Printf.sprintf
              "case '%s' requires live fixture execution; rerun with --config"
              case_.id)))

let run_case ~sw ~clock ~build_agent (case_ : Harness_case.t) =
  match build_agent case_ with
  | Error err ->
    case_failure case_ ~detail:(Error.to_string err)
  | Ok agent ->
    let run_result = Consumer.run_agent ~sw ~clock agent case_.prompt in
    let obs = Harness.Behavioral.observe agent run_result.response in
    let trajectory = trajectory_of_trace_ref run_result.trace_ref in
    let raw_trace_path =
      match run_result.trace_ref with
      | Some trace_ref -> Some trace_ref.path
      | None -> case_.source_trace_path
    in
    grade_case
      ~agent_name:(Agent.card agent).Agent_card.name
      ~elapsed:run_result.elapsed
      ~response:run_result.response
      ~observation:obs
      ?trajectory
      ?raw_trace_path
      case_

let run_dataset_mixed ?run_fixture cases =
  cases
  |> List.map (execute_case ?run_fixture)
  |> Harness_report.of_results

let run_dataset ~sw ~clock ~build_agent cases =
  let run_fixture = run_case ~sw ~clock ~build_agent in
  run_dataset_mixed ~run_fixture cases
