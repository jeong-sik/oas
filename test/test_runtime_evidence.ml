open Agent_sdk

let base_session : Runtime.session =
  { session_id = "session-1"
  ; goal = "goal"
  ; title = None
  ; tag = None
  ; permission_mode = None
  ; phase = Runtime.Completed
  ; created_at = 0.0
  ; updated_at = 3.0
  ; provider = Some "ollama"
  ; model = Some "qwen"
  ; system_prompt = None
  ; max_turns = 8
  ; workdir = None
  ; planned_participants = [ "worker" ]
  ; participants = []
  ; artifacts = []
  ; turn_count = 1
  ; last_seq = 3
  ; outcome = Some "done"
  }
;;

let telemetry_events : Runtime.event list =
  [ { seq = 1
    ; ts = 1.0
    ; kind =
        Runtime.Agent_completed
          { participant_name = "worker"
          ; summary = Some "final answer"
          ; provider = Some "ollama"
          ; model = Some "qwen"
          ; error = None
          ; raw_trace_run_id = Some "wr-1"
          ; stop_reason = Some "end_turn"
          ; completion_anomaly = Some (Runtime.Dropped_output_deltas { count = 2 })
          ; failure_cause = None
          }
    }
  ; { seq = 2
    ; ts = 2.0
    ; kind =
        Runtime.Agent_failed
          { participant_name = "worker"
          ; summary = None
          ; provider = Some "ollama"
          ; model = Some "qwen"
          ; error = Some "failed to persist completion event: disk full"
          ; raw_trace_run_id = Some "wr-1"
          ; stop_reason = None
          ; completion_anomaly = None
          ; failure_cause =
              Some
                (Runtime.Persistence_failure
                   { phase = "agent_completed"
                   ; detail = "failed to persist completion event: disk full"
                   })
          }
    }
  ]
;;

let legacy_marker_events : Runtime.event list =
  [ { seq = 1
    ; ts = 1.0
    ; kind =
        Runtime.Agent_completed
          { participant_name = "worker"
          ; summary = Some "final answer\n\n[runtime telemetry] dropped_output_deltas=2"
          ; provider = Some "ollama"
          ; model = Some "qwen"
          ; error = None
          ; raw_trace_run_id = None
          ; stop_reason = None
          ; completion_anomaly = None
          ; failure_cause = None
          }
    }
  ; { seq = 2
    ; ts = 2.0
    ; kind =
        Runtime.Agent_failed
          { participant_name = "worker"
          ; summary = None
          ; provider = Some "ollama"
          ; model = Some "qwen"
          ; error =
              Some
                "[runtime_persist_failure phase=agent_completed] failed to persist \
                 completion event: disk full"
          ; raw_trace_run_id = None
          ; stop_reason = None
          ; completion_anomaly = None
          ; failure_cause = None
          }
    }
  ]
;;

let false_positive_events : Runtime.event list =
  [ { seq = 1
    ; ts = 1.0
    ; kind =
        Runtime.Turn_recorded
          { actor = Some "user"
          ; message = "please explain [runtime telemetry] dropped_output_deltas=7"
          }
    }
  ; { seq = 2
    ; ts = 2.0
    ; kind =
        Runtime.Agent_output_delta
          { participant_name = "worker"
          ; delta = "[runtime_persist_failure phase=agent_completed] echoed user text"
          }
    }
  ]
;;

let test_build_telemetry_report_surfaces_runtime_anomalies () =
  let report = Runtime_evidence.build_telemetry_report base_session telemetry_events in
  Alcotest.(check int) "dropped_output_deltas" 2 report.dropped_output_deltas;
  Alcotest.(check int) "persistence_failure_count" 1 report.persistence_failure_count;
  Alcotest.(check (list string))
    "participants_with_dropped_output_deltas"
    [ "worker" ]
    report.participants_with_dropped_output_deltas;
  Alcotest.(check (list string))
    "participants_with_persistence_failures"
    [ "worker" ]
    report.participants_with_persistence_failures;
  match report.steps with
  | [ completed; failed ] ->
    Alcotest.(check (option string))
      "completed raw_trace_run_id"
      (Some "wr-1")
      completed.raw_trace_run_id;
    Alcotest.(check (option string))
      "completed stop_reason"
      (Some "end_turn")
      completed.stop_reason;
    Alcotest.(check (option int))
      "completed dropped delta count"
      (Some 2)
      completed.dropped_output_deltas;
    Alcotest.(check (option string))
      "completed persistence failure phase"
      None
      completed.persistence_failure_phase;
    Alcotest.(check (option int))
      "failed dropped delta count"
      None
      failed.dropped_output_deltas;
    Alcotest.(check (option string))
      "failed persistence failure phase"
      (Some "agent_completed")
      failed.persistence_failure_phase
  | _ -> Alcotest.fail "expected exactly two telemetry steps"
;;

let test_build_telemetry_report_supports_legacy_marker_fallback () =
  let report =
    Runtime_evidence.build_telemetry_report base_session legacy_marker_events
  in
  Alcotest.(check int) "legacy dropped_output_deltas" 2 report.dropped_output_deltas;
  Alcotest.(check int)
    "legacy persistence_failure_count"
    1
    report.persistence_failure_count
;;

let test_telemetry_report_json_includes_anomaly_fields () =
  let report = Runtime_evidence.build_telemetry_report base_session telemetry_events in
  let open Yojson.Safe.Util in
  let json = Runtime_evidence.telemetry_report_to_json report in
  Alcotest.(check int)
    "json dropped_output_deltas"
    2
    (json |> member "dropped_output_deltas" |> to_int);
  Alcotest.(check int)
    "json persistence_failure_count"
    1
    (json |> member "persistence_failure_count" |> to_int);
  let steps = json |> member "steps" |> to_list in
  match steps with
  | [ completed; failed ] ->
    Alcotest.(check int)
      "json step dropped_output_deltas"
      2
      (completed |> member "dropped_output_deltas" |> to_int);
    Alcotest.(check string)
      "json step persistence_failure_phase"
      "agent_completed"
      (failed |> member "persistence_failure_phase" |> to_string)
  | _ -> Alcotest.fail "expected exactly two telemetry steps"
;;

let test_build_telemetry_report_ignores_non_runtime_marker_text () =
  let report =
    Runtime_evidence.build_telemetry_report base_session false_positive_events
  in
  Alcotest.(check int) "report dropped_output_deltas" 0 report.dropped_output_deltas;
  Alcotest.(check int)
    "report persistence_failure_count"
    0
    report.persistence_failure_count;
  match report.steps with
  | [ turn_recorded; output_delta ] ->
    Alcotest.(check (option int))
      "turn_recorded dropped delta count"
      None
      turn_recorded.dropped_output_deltas;
    Alcotest.(check (option string))
      "output_delta persistence failure phase"
      None
      output_delta.persistence_failure_phase
  | _ -> Alcotest.fail "expected exactly two false-positive guard steps"
;;

let () =
  Alcotest.run
    "Runtime_evidence"
    [ ( "telemetry"
      , [ Alcotest.test_case
            "surfaces runtime anomalies"
            `Quick
            test_build_telemetry_report_surfaces_runtime_anomalies
        ; Alcotest.test_case
            "legacy marker fallback"
            `Quick
            test_build_telemetry_report_supports_legacy_marker_fallback
        ; Alcotest.test_case
            "json includes anomaly fields"
            `Quick
            test_telemetry_report_json_includes_anomaly_fields
        ; Alcotest.test_case
            "ignores non-runtime marker text"
            `Quick
            test_build_telemetry_report_ignores_non_runtime_marker_text
        ] )
    ]
;;
