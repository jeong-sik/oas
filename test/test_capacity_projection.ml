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

let () =
  Alcotest.run
    "capacity projection"
    [ ( "finite projection"
      , [ Alcotest.test_case "request body" `Quick test_request_body_projection
        ; Alcotest.test_case "context exhaustion" `Quick test_context_exhaustion_is_typed
        ] )
    ]
;;
