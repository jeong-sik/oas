open Alcotest
module H = Llm_provider.Backend_tool_call_harness

let malformed_openai_response = `Assoc [ "choices", `String "not-a-list" ]

let test_openai_parse_error_is_typed () =
  match
    H.validate_openai_response ~declared_tools:[ "read_file" ] malformed_openai_response
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

let test_openai_text_response_is_ok_empty_validation () =
  let json =
    `Assoc
      [ "id", `String "chatcmpl-text"
      ; "model", `String "gpt-4o"
      ; ( "choices"
        , `List
            [ `Assoc
                [ ( "message"
                  , `Assoc
                      [ "role", `String "assistant"; "content", `String "plain response" ]
                  )
                ; "finish_reason", `String "stop"
                ]
            ] )
      ]
  in
  match H.validate_openai_response ~declared_tools:[ "read_file" ] json with
  | Error err -> fail ("unexpected parse error: " ^ err.response_parse_error)
  | Ok result ->
    check bool "stop reason accepted" true result.stop_reason_correct;
    check bool "declared tools accepted" true result.all_tools_declared;
    check int "no tool calls found" 0 (List.length result.tool_calls_found)
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
            "valid text response stays Ok with no tool calls"
            `Quick
            test_openai_text_response_is_ok_empty_validation
        ] )
    ]
;;
