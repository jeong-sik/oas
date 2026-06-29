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
      ; "model", `String "gpt"
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
          { tool_use_id = "call-2"
          ; content = "ok"
          ; is_error = false
          ; json = None
          ; content_blocks = None
          }
      ; T.Image { media_type = "image/png"; data = "abc"; source_type = T.Base64 }
      ; T.Document { media_type = "text/plain"; data = "doc"; source_type = T.Base64 }
      ; T.Audio { media_type = "audio/wav"; data = "audio"; source_type = T.Base64 }
      ]
  in
  let result = H.validate_response ~declared_tools:[ "read_file" ] response in
  check bool "wrong stop reason" false result.stop_reason_correct;
  check bool "unknown tool" false result.all_tools_declared;
  check int "tool calls" 1 (List.length result.tool_calls_found);
  check int "no dropped blocks" 0 result.dropped_content_blocks
;;

let contains ~needle text =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop idx =
    if idx + needle_len > text_len
    then false
    else if String.sub text idx needle_len = needle
    then true
    else loop (idx + 1)
  in
  needle_len = 0 || loop 0
;;

let test_schema_validation_covers_scalar_actual_types_and_feedback () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ "as_array", `Assoc [ "type", `String "object" ]
            ; "as_object", `Assoc [ "type", `String "array" ]
            ; "as_null", `Assoc [ "type", `String "string" ]
            ; "as_bool", `Assoc [ "type", `String "number" ]
            ; "as_string", `Assoc [ "type", `String "boolean" ]
            ; "as_float", `Assoc [ "type", `String "integer" ]
            ] )
      ]
  in
  let response =
    mk_response
      [ T.ToolUse
          { id = "call-3"
          ; name = "shape_check"
          ; input =
              `Assoc
                [ "as_array", `List []
                ; "as_object", `Assoc []
                ; "as_null", `Null
                ; "as_bool", `Bool true
                ; "as_string", `String "yes"
                ; "as_float", `Float 1.5
                ]
          }
      ]
  in
  let result =
    H.validate_response_with_schemas
      ~declared_tools:[ "shape_check" ]
      ~tool_schemas:[ "shape_check", schema ]
      response
  in
  match result.tool_calls_found with
  | [ call ] ->
    check bool "invalid arguments" false call.arguments_valid;
    check
      (list string)
      "paths"
      [ "$.as_array"
      ; "$.as_bool"
      ; "$.as_float"
      ; "$.as_null"
      ; "$.as_object"
      ; "$.as_string"
      ]
      (List.map (fun (v : H.schema_violation) -> v.path) call.violations
       |> List.sort String.compare);
    let feedback = H.format_violations_feedback call in
    check bool "feedback names tool" true (contains ~needle:"shape_check" feedback);
    check bool "feedback includes path" true (contains ~needle:"$.as_null" feedback);
    check bool "feedback asks retry" true (contains ~needle:"Please retry" feedback)
  | _ -> fail "expected one tool call"
;;

let test_build_schema_map_rejects_missing_name_and_schema () =
  let direct_parameters =
    `Assoc [ "name", `String "direct"; "parameters", `Assoc [ "type", `String "object" ] ]
  in
  let no_name = `Assoc [ "parameters", `Assoc [ "type", `String "object" ] ] in
  let no_schema = `Assoc [ "name", `String "no_schema" ] in
  let function_without_parameters =
    `Assoc [ "function", `Assoc [ "name", `String "wrapped_no_schema" ] ]
  in
  let function_not_object =
    `Assoc
      [ "function", `String "bad"; "parameters", `Assoc [ "type", `String "object" ] ]
  in
  check
    bool
    "null schema ignored"
    true
    (H.extract_tool_schema (`Assoc [ "input_schema", `Null ]) = None);
  let schemas =
    H.build_schema_map
      [ direct_parameters
      ; no_name
      ; no_schema
      ; function_without_parameters
      ; function_not_object
      ]
  in
  check (list string) "only valid named schema" [ "direct" ] (List.map fst schemas)
;;

let test_provider_convenience_validators_cover_tool_responses () =
  let anthropic_json =
    `Assoc
      [ "id", `String "msg_123"
      ; "model", `String "claude-sonnet"
      ; "stop_reason", `String "tool_use"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "toolu_1"
                ; "name", `String "lookup"
                ; "input", `Assoc [ "q", `String "test" ]
                ]
            ] )
      ]
  in
  let anthropic =
    H.validate_anthropic_response ~declared_tools:[ "lookup" ] anthropic_json
  in
  check bool "anthropic stop reason" true anthropic.stop_reason_correct;
  check bool "anthropic declared tool" true anthropic.all_tools_declared;
  check int "anthropic tool calls" 1 (List.length anthropic.tool_calls_found);
  let gemini_json =
    Yojson.Safe.from_string
      {|{
        "candidates": [{
          "content": {
            "parts": [{
              "functionCall": {"name": "lookup", "args": {"q": "test"}}
            }],
            "role": "model"
          },
          "finishReason": "STOP"
        }]
      }|}
  in
  let gemini = H.validate_gemini_response ~declared_tools:[ "lookup" ] gemini_json in
  check bool "gemini stop reason" true gemini.stop_reason_correct;
  check bool "gemini declared tool" true gemini.all_tools_declared;
  check int "gemini tool calls" 1 (List.length gemini.tool_calls_found)
;;

let test_anthropic_tool_use_stop_without_tool_block_fails_closed () =
  let json =
    `Assoc
      [ "id", `String "msg_no_tool"
      ; "model", `String "claude-sonnet"
      ; "stop_reason", `String "tool_use"
      ; "content", `List [ `Assoc [ "type", `String "text"; "text", `String "" ] ]
      ; "usage", `Assoc [ "input_tokens", `Int 1; "output_tokens", `Int 1 ]
      ]
  in
  let response = Llm_provider.Backend_anthropic.parse_response json in
  check
    bool
    "tool_use stop without a ToolUse block is not executable"
    true
    (response.stop_reason = T.Unknown "tool_calls");
  let result = H.validate_response ~declared_tools:[ "lookup" ] response in
  check bool "validation sees non-tool stop" true result.stop_reason_correct;
  check int "no tool calls found" 0 (List.length result.tool_calls_found)
;;

let test_unknown_stop_reason_without_tool_block_still_fails_validation () =
  let response =
    mk_response ~stop_reason:(T.Unknown "provider_specific") [ T.Text "ok" ]
  in
  let result = H.validate_response ~declared_tools:[ "lookup" ] response in
  check bool "arbitrary unknown stop reason rejected" false result.stop_reason_correct;
  check int "no tool calls found" 0 (List.length result.tool_calls_found)
;;

let test_schema_validation_unknown_type_fails_closed () =
  (* RFC-OAS-029 S8.1: a non-standard schema [type] must surface as a violation
     instead of silently accepting any value (regression: the prior
     [unsupported_type <> ""] catch-all passed every value). A valid "null"
     type still validates a null value. *)
  let schema =
    `Assoc
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ "weird", `Assoc [ "type", `String "frobnicate" ]
            ; "nullable", `Assoc [ "type", `String "null" ]
            ] )
      ]
  in
  let response =
    mk_response
      [ T.ToolUse
          { id = "call-unknown-type"
          ; name = "shape_check"
          ; input = `Assoc [ "weird", `String "anything"; "nullable", `Null ]
          }
      ]
  in
  let result =
    H.validate_response_with_schemas
      ~declared_tools:[ "shape_check" ]
      ~tool_schemas:[ "shape_check", schema ]
      response
  in
  match result.tool_calls_found with
  | [ call ] ->
    check bool "unknown type makes args invalid" false call.arguments_valid;
    check
      (list string)
      "only the unknown-type path violates (null type accepts null)"
      [ "$.weird" ]
      (List.map (fun (v : H.schema_violation) -> v.path) call.violations
       |> List.sort String.compare);
    (match call.violations with
     | [ violation ] ->
       check string "unknown expected type is reported" "frobnicate" violation.expected;
       check string "actual json type is reported" "string" violation.actual
     | _ -> fail "expected exactly one unknown-type violation")
  | _ -> fail "expected one tool call"
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
        ; test_case
            "scalar actual types and feedback"
            `Quick
            test_schema_validation_covers_scalar_actual_types_and_feedback
        ; test_case
            "invalid schema map entries"
            `Quick
            test_build_schema_map_rejects_missing_name_and_schema
        ; test_case
            "provider convenience validators"
            `Quick
            test_provider_convenience_validators_cover_tool_responses
        ; test_case
            "anthropic tool_use stop without tool block fails closed"
            `Quick
            test_anthropic_tool_use_stop_without_tool_block_fails_closed
        ; test_case
            "unknown stop without tool block still fails validation"
            `Quick
            test_unknown_stop_reason_without_tool_block_still_fails_validation
        ; test_case
            "unknown schema type fails closed"
            `Quick
            test_schema_validation_unknown_type_fails_closed
        ] )
    ]
;;
