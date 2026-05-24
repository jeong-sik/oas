open Agent_sdk
open Alcotest
module R = Response_harness

let check_metric label expected_name expected_value actual =
  check string (label ^ " name") expected_name actual.Metric_contract.name;
  check (float 0.001) (label ^ " value") expected_value actual.value
;;

let test_extract_first_float_patterns () =
  check (option (float 0.001)) "plain" (Some 0.75) (R.extract_first_float "0.75");
  check (option (float 0.001)) "prefix" (Some 0.85) (R.extract_first_float "Score: 0.85");
  check (option (float 0.001)) "leading dot" (Some 0.5) (R.extract_first_float ".5 ok");
  check
    (option (float 0.001))
    "negative"
    (Some (-0.25))
    (R.extract_first_float "loss=-0.25");
  check (option (float 0.001)) "percent" (Some 0.75) (R.extract_first_float "75%");
  check (option (float 0.001)) "none" None (R.extract_first_float "no number")
;;

let test_extract_score_tracks_telemetry () =
  R.reset_telemetry ();
  check (option (float 0.001)) "clamped high" (Some 1.0) (R.extract_score_from_text "2.5");
  check (option (float 0.001)) "clamped low" (Some 0.0) (R.extract_score_from_text "-0.5");
  check (option (float 0.001)) "missing" None (R.extract_score_from_text "none");
  let snap = R.snapshot () in
  check int "successes" 2 snap.parse_success;
  check int "failures" 1 snap.parse_failure;
  check int "text extractions" 2 snap.text_extraction;
  check (float 0.001) "success rate" (2.0 /. 3.0) snap.success_rate
;;

let test_metric_schema_parse_paths () =
  let schema = R.metric_schema ~metric_name:"quality" () in
  check string "schema name" "report_metric" schema.name;
  check int "params" 2 (List.length schema.params);
  (match schema.parse (`Assoc [ "name", `String "quality"; "value", `Float 0.91 ]) with
   | Ok parsed -> check_metric "parsed" "quality" 0.91 parsed
   | Error err -> fail err);
  (match
     schema.parse (`Assoc [ "name", `String "quality"; "value", `Float Float.nan ])
   with
   | Error err -> check bool "finite error" true (String.length err > 0)
   | Ok _ -> fail "expected nan rejection");
  match schema.parse (`Assoc [ "name", `Int 1; "value", `String "bad" ]) with
  | Error err -> check bool "type error" true (String.length err > 0)
  | Ok _ -> fail "expected type error"
;;

let test_parse_metric_from_text_direct_recovered_and_failure () =
  R.reset_telemetry ();
  (match
     R.parse_metric_from_text ~expected_name:"score" "<metric name=\"score\">0.7</metric>"
   with
   | Ok parsed -> check_metric "direct" "score" 0.7 parsed
   | Error err -> fail err);
  (match
     R.parse_metric_from_text
       ~expected_name:"score"
       "```xml\n<metric name=\"score\">0.8</metric>\n```"
   with
   | Ok parsed -> check_metric "recovered" "score" 0.8 parsed
   | Error err -> fail err);
  (match
     R.parse_metric_from_text ~expected_name:"score" "<metric name=\"loss\">0.2</metric>"
   with
   | Error err -> check bool "mismatch" true (String.length err > 0)
   | Ok _ -> fail "expected mismatch");
  let snap = R.snapshot () in
  check int "parse success" 2 snap.parse_success;
  check int "parse failure" 1 snap.parse_failure;
  check int "recovery" 0 snap.recovery_applied;
  check int "text extraction" 2 snap.text_extraction
;;

let test_extract_metric_from_response_tool_use () =
  R.reset_telemetry ();
  let content =
    [ Types.Text "using tool"
    ; Types.ToolUse
        { id = "toolu-1"
        ; name = "report_metric"
        ; input = `Assoc [ "name", `String "score"; "value", `Float 0.66 ]
        }
    ]
  in
  (match R.extract_metric_from_response ~metric_name:"score" content with
   | Ok parsed -> check_metric "tool metric" "score" 0.66 parsed
   | Error err -> fail err);
  (match R.extract_metric_from_response ~metric_name:"score" [ Types.Text "none" ] with
   | Error err -> check bool "missing tool error" true (String.length err > 0)
   | Ok _ -> fail "expected missing tool error");
  let snap = R.snapshot () in
  check int "success" 1 snap.parse_success;
  check int "failure" 1 snap.parse_failure;
  check int "tool extraction" 1 snap.tool_use_extraction
;;

let () =
  run
    "response-harness"
    [ ( "float extraction"
      , [ test_case "patterns" `Quick test_extract_first_float_patterns
        ; test_case "score telemetry" `Quick test_extract_score_tracks_telemetry
        ] )
    ; ( "metric extraction"
      , [ test_case "schema parse paths" `Quick test_metric_schema_parse_paths
        ; test_case
            "text direct recovered failure"
            `Quick
            test_parse_metric_from_text_direct_recovered_and_failure
        ; test_case "tool use response" `Quick test_extract_metric_from_response_tool_use
        ] )
    ]
;;
