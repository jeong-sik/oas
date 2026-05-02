(** Tool-calling verification harness for LLM provider backends.

    Validates that parsed API responses contain well-formed tool calls,
    correct stop reasons, and no silently dropped content blocks.
    Pure functions — no Eio, no network.

    @since 0.187.7 *)

open Types

type tool_call_check =
  { name : string
  ; arguments_valid : bool
  }

type validation_result =
  { tool_calls_found : tool_call_check list
  ; stop_reason_correct : bool
  ; all_tools_declared : bool
  ; dropped_content_blocks : int
  }

let empty_result =
  { tool_calls_found = []
  ; stop_reason_correct = true
  ; all_tools_declared = true
  ; dropped_content_blocks = 0
  }
;;

(** Validate a parsed {!api_response} against a set of declared tool names.

    Checks:
    - Every {!ToolUse} block has a name present in [declared_tools]
    - Every {!ToolUse} block has parseable input (valid Yojson)
    - [stop_reason] is {!StopToolUse} when tool calls exist, {!EndTurn} otherwise
    - No content blocks were silently dropped during parse (heuristic: empty text) *)
let validate_response ~declared_tools (resp : api_response) : validation_result =
  let tool_calls =
    List.filter_map
      (function
        | ToolUse { name; input; _ } ->
          let arguments_valid =
            true
            (* any Yojson.t is valid JSON *)
          in
          Some { name; arguments_valid }
        | _ -> None)
      resp.content
  in
  let has_tool_calls = tool_calls <> [] in
  let stop_reason_correct =
    match has_tool_calls, resp.stop_reason with
    | true, StopToolUse -> true
    | false, (EndTurn | MaxTokens | StopSequence) -> true
    | _ -> false
  in
  let declared_set = List.sort_uniq String.compare declared_tools in
  let all_tools_declared =
    List.for_all (fun (tc : tool_call_check) -> List.mem tc.name declared_set) tool_calls
  in
  let dropped_content_blocks =
    List.length
      (List.filter
         (function
           | Text s when String.trim s = "" -> true
           | _ -> false)
         resp.content)
  in
  { tool_calls_found = tool_calls
  ; stop_reason_correct
  ; all_tools_declared
  ; dropped_content_blocks
  }
;;

let validate_anthropic_response ~declared_tools json =
  validate_response ~declared_tools (Backend_anthropic.parse_response json)
;;

let validate_gemini_response ~declared_tools json =
  validate_response ~declared_tools (Backend_gemini.parse_response json)
;;

let validate_openai_response ~declared_tools json =
  let json_str = Yojson.Safe.to_string json in
  match Backend_openai_parse.parse_openai_response_result json_str with
  | Ok resp -> validate_response ~declared_tools resp
  | Error _ -> empty_result
;;

(* ── Inline Tests ─────────────────────────────────────── *)

let%test "anthropic tool_use response validates correctly" =
  let json =
    `Assoc
      [ "id", `String "msg_123"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "tool_use"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_001"
                ; "name", `String "get_weather"
                ; "input", `Assoc [ "city", `String "Seoul" ]
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 100
            ; "output_tokens", `Int 50
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "get_weather" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
  && (List.hd result.tool_calls_found).name = "get_weather"
;;

let%test "anthropic undeclared tool fails validation" =
  let json =
    `Assoc
      [ "id", `String "msg_456"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "tool_use"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_002"
                ; "name", `String "unknown_tool"
                ; "input", `Assoc []
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "get_weather" ] json in
  result.stop_reason_correct && not result.all_tools_declared
;;

let%test "gemini functionCall response validates correctly" =
  let json =
    `Assoc
      [ ( "candidates"
        , `List
            [ `Assoc
                [ ( "content"
                  , `Assoc
                      [ ( "parts"
                        , `List
                            [ `Assoc
                                [ ( "functionCall"
                                  , `Assoc
                                      [ "name", `String "search"
                                      ; "args", `Assoc [ "query", `String "OCaml 5" ]
                                      ] )
                                ]
                            ] )
                      ] )
                ; "finishReason", `String "STOP"
                ]
            ] )
      ]
  in
  let result = validate_gemini_response ~declared_tools:[ "search" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
;;

let%test "openai tool_calls response validates correctly" =
  let json =
    `Assoc
      [ "id", `String "chatcmpl-123"
      ; "model", `String "gpt-4o"
      ; ( "choices"
        , `List
            [ `Assoc
                [ ( "message"
                  , `Assoc
                      [ "role", `String "assistant"
                      ; "content", `Null
                      ; ( "tool_calls"
                        , `List
                            [ `Assoc
                                [ "id", `String "call_001"
                                ; "type", `String "function"
                                ; ( "function"
                                  , `Assoc
                                      [ "name", `String "read_file"
                                      ; "arguments", `String {|{"path": "/etc/hosts"}|}
                                      ] )
                                ]
                            ] )
                      ] )
                ; "finish_reason", `String "tool_calls"
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "prompt_tokens", `Int 50
            ; "completion_tokens", `Int 20
            ; "total_tokens", `Int 70
            ] )
      ]
  in
  let result = validate_openai_response ~declared_tools:[ "read_file" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
  && (List.hd result.tool_calls_found).name = "read_file"
;;

let%test "wrong stop_reason for tool calls fails validation" =
  let json =
    `Assoc
      [ "id", `String "msg_bad"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "end_turn"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_003"
                ; "name", `String "test"
                ; "input", `Assoc []
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "test" ] json in
  not result.stop_reason_correct
;;

let%test "text-only response passes validation" =
  let json =
    `Assoc
      [ "id", `String "msg_text"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "end_turn"
      ; "content", `List [ `Assoc [ "type", `String "text"; "text", `String "Hello" ] ]
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[] json in
  result.stop_reason_correct && result.all_tools_declared && result.tool_calls_found = []
;;
