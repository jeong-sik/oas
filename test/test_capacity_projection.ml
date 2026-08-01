let test_request_body_projection () =
  let measure value =
    Ok
      { Llm_provider.Capacity_projection.serialized_body_bytes =
          Some (String.length value)
      ; input_tokens = None
      }
  in
  match
    Llm_provider.Capacity_projection.project
      ~pressure:
        (Llm_provider.Capacity_projection.Request_body
           { actual_bytes = 9; limit_bytes = 4 })
      ~current:"123456789"
      ~candidates:[ "12345"; "1234" ]
      ~measure
  with
  | Ok (Llm_provider.Capacity_projection.Candidate { value = "1234"; _ }) -> ()
  | Ok _ -> Alcotest.fail "expected the first fitting candidate"
  | Error _ -> Alcotest.fail "projection unexpectedly failed"
;;

let test_context_exhaustion_is_typed () =
  let measure value =
    Ok
      { Llm_provider.Capacity_projection.serialized_body_bytes = None
      ; input_tokens = Some value
      }
  in
  match
    Llm_provider.Capacity_projection.project
      ~pressure:
        (Llm_provider.Capacity_projection.Context_window
           { input_tokens = 10; reserved_output_tokens = 3; max_context_tokens = 8 })
      ~current:10
      ~candidates:[ 9; 8 ]
      ~measure
  with
  | Ok
      (Llm_provider.Capacity_projection.Exhausted
         (Llm_provider.Capacity_projection.Context_window _)) -> ()
  | Ok _ -> Alcotest.fail "expected typed exhaustion"
  | Error _ -> Alcotest.fail "exhaustion must not be a retry error"
;;

let test_missing_current_measurement_is_typed () =
  let measure _ =
    Ok
      { Llm_provider.Capacity_projection.serialized_body_bytes = None
      ; input_tokens = None
      }
  in
  match
    Llm_provider.Capacity_projection.project
      ~pressure:
        (Llm_provider.Capacity_projection.Request_body
           { actual_bytes = 9; limit_bytes = 4 })
      ~current:"current"
      ~candidates:[]
      ~measure
  with
  | Error (Llm_provider.Capacity_projection.Missing_measurement _) -> ()
  | Error _ -> Alcotest.fail "expected Missing_measurement for the current value"
  | Ok _ -> Alcotest.fail "missing current measurement unexpectedly projected"
;;

let test_candidate_must_strictly_decrease () =
  let measure value =
    Ok
      { Llm_provider.Capacity_projection.serialized_body_bytes = Some value
      ; input_tokens = None
      }
  in
  match
    Llm_provider.Capacity_projection.project
      ~pressure:
        (Llm_provider.Capacity_projection.Request_body
           { actual_bytes = 9; limit_bytes = 4 })
      ~current:9
      ~candidates:[ 9 ]
      ~measure
  with
  | Error
      (Llm_provider.Capacity_projection.Candidate_not_smaller
         { previous = 9; candidate = 9 }) -> ()
  | Error _ -> Alcotest.fail "expected Candidate_not_smaller"
  | Ok _ -> Alcotest.fail "non-decreasing candidate unexpectedly projected"
;;

let () =
  Alcotest.run
    "capacity projection"
    [ ( "finite projection"
      , [ Alcotest.test_case "request body" `Quick test_request_body_projection
        ; Alcotest.test_case "context exhaustion" `Quick test_context_exhaustion_is_typed
        ; Alcotest.test_case
            "missing current measurement"
            `Quick
            test_missing_current_measurement_is_typed
        ; Alcotest.test_case
            "candidate decrease"
            `Quick
            test_candidate_must_strictly_decrease
        ] )
    ]
;;
