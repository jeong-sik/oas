open Alcotest
module H = Llm_provider.Backend_tool_call_harness
module T = Llm_provider.Types

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

let mk_response ?(stop_reason = T.StopToolUse) content : T.api_response =
  { id = "resp"; model = "model"; stop_reason; content; usage = None; telemetry = None }
;;

let test_schema_validation_reports_nested_violations () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "required", `List [ `String "path"; `String "limit"; `String "tags" ]
      ; ( "properties"
        , `Assoc
            [ "path", `Assoc [ "type", `String "string" ]
            ; "limit", `Assoc [ "type", `String "integer" ]
            ; ( "tags"
              , `Assoc
                  [ "type", `String "array"
                  ; "items", `Assoc [ "type", `String "string" ]
                  ] )
            ; "mode", `Assoc [ "enum", `List [ `String "fast"; `String "safe" ] ]
            ] )
      ]
  in
  let response =
    mk_response
      [ T.ToolUse
          { id = "call-1"
          ; name = "read_file"
          ; input =
              `Assoc
                [ "limit", `Float 1.5
                ; "tags", `List [ `String "src"; `Int 1 ]
                ; "mode", `String "slow"
                ]
          }
      ; T.Text ""
      ]
  in
  let result =
    H.validate_response_with_schemas
      ~declared_tools:[ "read_file" ]
      ~tool_schemas:[ "read_file", schema ]
      response
  in
  check bool "stop reason ok" true result.stop_reason_correct;
  check bool "declared" true result.all_tools_declared;
  check int "dropped empty text" 1 result.dropped_content_blocks;
  match result.tool_calls_found with
  | [ call ] ->
    check bool "invalid arguments" false call.arguments_valid;
    check int "violations" 4 (List.length call.violations);
    check
      (list string)
      "paths"
      [ "$.limit"; "$.mode"; "$.path"; "$.tags[1]" ]
      (List.map (fun (v : H.schema_violation) -> v.path) call.violations
       |> List.sort String.compare)
  | _ -> fail "expected one tool call"
;;

let test_build_schema_map_accepts_provider_shapes () =
  let anthropic_tool =
    `Assoc
      [ "name", `String "search"; "input_schema", `Assoc [ "type", `String "object" ] ]
  in
  let openai_tool =
    `Assoc
      [ ( "function"
        , `Assoc
            [ "name", `String "write_file"
            ; "parameters", `Assoc [ "type", `String "object" ]
            ] )
      ]
  in
  let ignored = `Assoc [ "name", `String "noop"; "input_schema", `Null ] in
  let schemas = H.build_schema_map [ anthropic_tool; openai_tool; ignored ] in
  check (list string) "names" [ "search"; "write_file" ] (List.map fst schemas)
;;

let test_validate_response_flags_unknown_tool_and_stop_reason () =
  let response =
    mk_response
      ~stop_reason:T.EndTurn
      [ T.ToolUse { id = "call-2"; name = "undeclared"; input = `Assoc [] }
      ; T.Thinking { thinking_type = "visible"; content = "reason" }
      ; T.RedactedThinking "redacted"
      ; T.ToolResult
          { tool_use_id = "call-2"; content = "ok"; is_error = false; json = None }
      ; T.Image { media_type = "image/png"; data = "abc"; source_type = "base64" }
      ; T.Document { media_type = "text/plain"; data = "doc"; source_type = "base64" }
      ; T.Audio { media_type = "audio/wav"; data = "audio"; source_type = "base64" }
      ]
  in
  let result = H.validate_response ~declared_tools:[ "read_file" ] response in
  check bool "wrong stop reason" false result.stop_reason_correct;
  check bool "unknown tool" false result.all_tools_declared;
  check int "tool calls" 1 (List.length result.tool_calls_found);
  check int "no dropped blocks" 0 result.dropped_content_blocks
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
    ; ( "schema_validation"
      , [ test_case
            "nested violations"
            `Quick
            test_schema_validation_reports_nested_violations
        ; test_case "tool schema map" `Quick test_build_schema_map_accepts_provider_shapes
        ; test_case
            "unknown tool and stop reason"
            `Quick
            test_validate_response_flags_unknown_tool_and_stop_reason
        ] )
    ]
;;
