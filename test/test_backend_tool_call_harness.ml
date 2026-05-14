open Alcotest
module H = Llm_provider.Backend_tool_call_harness

let malformed_openai_response = `Assoc [ "choices", `String "not-a-list" ]

let test_openai_parse_error_is_typed () =
  match
    H.validate_openai_response_result
      ~declared_tools:[ "read_file" ]
      malformed_openai_response
  with
  | Ok _ -> fail "expected typed parse error"
  | Error err ->
    check string "backend" "openai" err.response_backend;
    check
      bool
      "parse error detail is surfaced"
      true
      (String.length err.response_parse_error > 0)
;;

let test_openai_parse_error_fails_closed_for_legacy_wrapper () =
  let result =
    H.validate_openai_response ~declared_tools:[ "read_file" ] malformed_openai_response
  in
  check bool "stop reason not accepted" false result.stop_reason_correct;
  check bool "declared tools not accepted" false result.all_tools_declared;
  check int "no tool calls fabricated" 0 (List.length result.tool_calls_found)
;;

let () =
  run
    "backend_tool_call_harness"
    [ ( "openai_parse_errors"
      , [ test_case
            "typed result surfaces parse error"
            `Quick
            test_openai_parse_error_is_typed
        ; test_case
            "legacy wrapper fails closed"
            `Quick
            test_openai_parse_error_fails_closed_for_legacy_wrapper
        ] )
    ]
;;
