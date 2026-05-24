open Agent_sdk
open Alcotest
module H = Runtime_health

let status =
  testable (fun fmt s -> Format.pp_print_string fmt (H.status_to_string s)) ( = )
;;

let probe_name =
  testable (fun fmt n -> Format.pp_print_string fmt (H.probe_name_to_string n)) ( = )
;;

let test_status_roundtrip () =
  List.iter
    (fun expected ->
       let encoded = H.status_to_string expected in
       match H.status_of_string encoded with
       | Ok actual -> check status encoded expected actual
       | Error err -> fail err)
    [ H.Status_ok; Degraded; Failed; Unknown ];
  match H.status_of_string "paused" with
  | Error err -> check string "invalid status error" "unknown health status: paused" err
  | Ok _ -> fail "expected invalid status to fail"
;;

let test_probe_name_roundtrip () =
  let cases =
    [ H.Provider, "provider"
    ; Transport, "transport"
    ; Checkpoint, "checkpoint"
    ; Context, "context"
    ; Event_bus, "event_bus"
    ; Custom "scheduler", "scheduler"
    ]
  in
  List.iter
    (fun (expected, encoded) ->
       check string "encode" encoded (H.probe_name_to_string expected);
       check probe_name "decode" expected (H.probe_name_of_string encoded))
    cases
;;

let test_make_probe_and_overall_status () =
  let ok =
    H.make_probe
      ~name:H.Provider
      ~status:H.Status_ok
      ~detail:"ready"
      ~checked_at:10.0
      ~latency_ms:2.5
      ()
  in
  check probe_name "probe name" H.Provider ok.name;
  check status "probe status" H.Status_ok ok.status;
  check (option string) "detail" (Some "ready") ok.detail;
  check (float 0.001) "checked_at" 10.0 ok.checked_at;
  check (option (float 0.001)) "latency_ms" (Some 2.5) ok.latency_ms;
  check status "empty overall" H.Unknown (H.overall_status []);
  check status "ok overall" H.Status_ok (H.overall_status [ ok ]);
  check
    status
    "failed wins"
    H.Failed
    (H.overall_status
       [ ok
       ; H.make_probe ~name:H.Transport ~status:H.Degraded ~checked_at:11.0 ()
       ; H.make_probe ~name:H.Context ~status:H.Failed ~checked_at:12.0 ()
       ]);
  check
    status
    "unknown beats ok"
    H.Unknown
    (H.overall_status [ ok; H.make_probe ~name:H.Event_bus ~status:H.Unknown () ])
;;

let test_report_json_roundtrip () =
  let probes =
    [ H.make_probe
        ~name:H.Provider
        ~status:H.Status_ok
        ~detail:"provider reachable"
        ~checked_at:1.0
        ~latency_ms:3.0
        ()
    ; H.make_probe ~name:(H.Custom "queue") ~status:H.Degraded ~checked_at:2.0 ()
    ]
  in
  let report = H.make ~generated_at:42.0 probes in
  check status "overall" H.Degraded report.overall;
  let json = H.to_json report in
  match H.of_json json with
  | Error err -> fail err
  | Ok decoded ->
    check (float 0.001) "generated_at" 42.0 decoded.generated_at;
    check status "decoded overall" H.Degraded decoded.overall;
    check int "probe count" 2 (List.length decoded.probes);
    let first = List.hd decoded.probes in
    check probe_name "first name" H.Provider first.name;
    check (option string) "first detail" (Some "provider reachable") first.detail;
    check (option (float 0.001)) "first latency" (Some 3.0) first.latency_ms
;;

let expect_error label expected = function
  | Ok _ -> fail (label ^ ": expected error")
  | Error actual -> check string label expected actual
;;

let test_json_error_paths () =
  expect_error
    "probe non-object"
    "health probe must be a JSON object"
    (H.probe_of_json `Null);
  expect_error
    "probe missing field"
    "missing field: status"
    (H.probe_of_json (`Assoc [ "name", `String "provider" ]));
  expect_error
    "probe bad status"
    "unknown health status: weird"
    (H.probe_of_json
       (`Assoc
           [ "name", `String "provider"
           ; "status", `String "weird"
           ; "detail", `Null
           ; "checked_at", `Float 1.0
           ; "latency_ms", `Null
           ]));
  expect_error
    "report non-object"
    "runtime health report must be a JSON object"
    (H.of_json `Null);
  expect_error
    "report probes not list"
    "field probes must be a list"
    (H.of_json
       (`Assoc
           [ "generated_at", `Float 1.0; "overall", `String "ok"; "probes", `Assoc [] ]))
;;

let () =
  run
    "runtime-health"
    [ ( "status"
      , [ test_case "status roundtrip" `Quick test_status_roundtrip
        ; test_case "probe name roundtrip" `Quick test_probe_name_roundtrip
        ; test_case "overall severity" `Quick test_make_probe_and_overall_status
        ] )
    ; ( "json"
      , [ test_case "report roundtrip" `Quick test_report_json_roundtrip
        ; test_case "error paths" `Quick test_json_error_paths
        ] )
    ]
;;
