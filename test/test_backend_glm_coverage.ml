open Alcotest
module K = Llm_provider.Backend_glm
module PC = Llm_provider.Provider_config
module T = Llm_provider.Types

let member key json = Yojson.Safe.Util.member key json
let to_bool json = Yojson.Safe.Util.to_bool json
let to_string json = Yojson.Safe.Util.to_string json

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop i =
    i + sub_len <= text_len && (String.sub text i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0
;;

let source_path path =
  let rec find_up dir remaining =
    let candidate = Filename.concat dir path in
    if Sys.file_exists candidate
    then Some candidate
    else if remaining = 0
    then None
    else (
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else find_up parent (remaining - 1))
  in
  match find_up (Sys.getcwd ()) 12 with
  | Some candidate -> candidate
  | None -> path
;;

let read_source path = In_channel.with_open_text (source_path path) In_channel.input_all

let source_before_inline_tests source =
  source
  |> String.split_on_char '\n'
  |> List.fold_left
       (fun (keep, lines) line ->
          if (not keep) || String.equal line "[@@@coverage off]"
          then false, lines
          else true, line :: lines)
       (true, [])
  |> snd
  |> List.rev
  |> String.concat "\n"
;;

let glm_config ?enable_thinking ?clear_thinking ?(tool_stream = false) () =
  PC.make
    ~kind:PC.Glm
    ~model_id:"glm-5.1"
    ~base_url:"https://api.z.ai/api/paas/v4"
    ?enable_thinking
    ?clear_thinking
    ~tool_stream
    ()
;;

let check_class label expected actual = check bool label true (actual = expected)

let check_classification label code message expected retryable =
  let actual, retry = K.classify_glm_error ~code ~message in
  check_class (label ^ " class") expected actual;
  check bool (label ^ " retryable") retryable retry
;;

let response_json ?(reasoning = Some "step by step") ?(content = "answer") () =
  let message_fields =
    [ "role", `String "assistant"; "content", `String content ]
    @
    match reasoning with
    | Some r -> [ "reasoning_content", `String r ]
    | None -> []
  in
  Yojson.Safe.to_string
    (`Assoc
        [ "id", `String "chatcmpl-provider-k"
        ; "model", `String "glm-5.1"
        ; ( "choices"
          , `List
              [ `Assoc
                  [ "finish_reason", `String "stop"; "message", `Assoc message_fields ]
              ] )
        ; ( "usage"
          , `Assoc
              [ "prompt_tokens", `Int 7
              ; "completion_tokens", `Int 3
              ; "total_tokens", `Int 10
              ] )
        ])
;;

let test_classification_matrix () =
  check_classification "auth" "1001" "bad api key" K.Glm_auth_error false;
  check_classification "rate" "1305" "service overloaded" K.Glm_rate_limited true;
  check_classification "quota" "1308" "quota exhausted" K.Glm_quota_exceeded false;
  check_classification "server" "1234" "backend timeout" K.Glm_server_error true;
  check_classification "invalid" "1210" "bad parameter" K.Glm_invalid_request false;
  check_classification
    "message quota fallback"
    "unknown"
    "usage limit exceeded"
    K.Glm_quota_exceeded
    false;
  check_classification
    "message rate fallback"
    "unknown"
    "rate limit hit"
    K.Glm_rate_limited
    true;
  check_classification
    "unknown fallback"
    "9999"
    "unclassified"
    K.Glm_invalid_request
    false
;;

let test_http_status_mapping () =
  check int "quota" 429 (K.http_code_of_glm_error_class K.Glm_quota_exceeded);
  check int "rate" 429 (K.http_code_of_glm_error_class K.Glm_rate_limited);
  check int "auth" 401 (K.http_code_of_glm_error_class K.Glm_auth_error);
  check int "server" 500 (K.http_code_of_glm_error_class K.Glm_server_error);
  check int "invalid" 400 (K.http_code_of_glm_error_class K.Glm_invalid_request)
;;

let test_build_request_thinking_modes_and_tool_stream () =
  let messages = [ T.user_msg "calculate" ] in
  let enabled =
    K.build_request
      ~stream:true
      ~config:
        (glm_config ~enable_thinking:true ~clear_thinking:false ~tool_stream:true ())
      ~messages
      ~tools:[ `Assoc [ "name", `String "calc" ] ]
      ()
    |> Yojson.Safe.from_string
  in
  check
    string
    "thinking enabled"
    "enabled"
    (enabled |> member "thinking" |> member "type" |> to_string);
  check
    bool
    "clear thinking false"
    false
    (enabled |> member "thinking" |> member "clear_thinking" |> to_bool);
  check bool "tool stream true" true (enabled |> member "tool_stream" |> to_bool);
  let disabled =
    K.build_request
      ~config:(glm_config ~enable_thinking:false ~tool_stream:true ())
      ~messages
      ()
    |> Yojson.Safe.from_string
  in
  check
    string
    "thinking disabled"
    "disabled"
    (disabled |> member "thinking" |> member "type" |> to_string);
  check
    bool
    "tool stream gated by stream flag"
    true
    (disabled |> member "tool_stream" = `Null);
  let passthrough =
    K.build_request ~config:(glm_config ()) ~messages () |> Yojson.Safe.from_string
  in
  check bool "thinking omitted" true (passthrough |> member "thinking" = `Null)
;;

let test_backend_glm_delegates_thinking_builder_to_shared_request_builder () =
  let source =
    read_source "lib/llm_provider/backend_glm.ml" |> source_before_inline_tests
  in
  [ "clear_thinking"; "chat_template_kwargs"; "reasoning_effort"; "enable_thinking" ]
  |> List.iter (fun field ->
    check
      bool
      (field ^ " not emitted directly in backend_glm.ml")
      false
      (contains_substring ~sub:("\"" ^ field ^ "\"") source))
;;

let test_parse_response_extracts_reasoning_and_usage () =
  let resp = K.parse_response (response_json ()) in
  check string "id" "chatcmpl-provider-k" resp.id;
  check string "model" "glm-5.1" resp.model;
  (match resp.content with
   | [ T.Thinking { content = "step by step"; _ }; T.Text "answer" ] -> ()
   | _ -> fail "expected reasoning block followed by answer text");
  match resp.usage with
  | Some usage ->
    check int "input tokens" 7 usage.input_tokens;
    check int "output tokens" 3 usage.output_tokens
  | None -> fail "expected usage"
;;

let test_parse_response_classifies_glm_errors () =
  let cases =
    [ ( {|{"error":{"code":"1305","message":"service overloaded"}}|}
      , "1305"
      , K.Glm_rate_limited
      , true )
    ; ( {|{"error":{"code":1234,"message":"server unavailable"}}|}
      , "1234"
      , K.Glm_server_error
      , true )
    ; ( {|{"error":{"code":{"nested":true},"message":"quota exceeded"}}|}
      , "unknown"
      , K.Glm_quota_exceeded
      , false )
    ; {|{"error":{"code":1200}}|}, "1200", K.Glm_invalid_request, false
    ]
  in
  List.iter
    (fun (body, code, expected_class, expected_retryable) ->
       match K.parse_response body with
       | exception K.Glm_api_error err ->
         check string "error code" code err.code;
         check_class "error class" expected_class err.error_class;
         check bool "retryable" expected_retryable err.is_retryable
       | _ -> fail "expected Glm_api_error")
    cases
;;

let test_parse_response_wraps_openai_parse_errors () =
  match K.parse_response {|{"choices":"not-a-list"}|} with
  | exception K.Glm_api_error err ->
    check string "parse code" "parse" err.code;
    check_class "parse class" K.Glm_invalid_request err.error_class;
    check bool "parse is not retryable" false err.is_retryable;
    check bool "parse message surfaced" true (String.length err.message > 0)
  | _ -> fail "expected Glm_api_error"
;;

let test_extract_reasoning_handles_empty_and_malformed_bodies () =
  let resp =
    { T.id = "resp"
    ; model = "glm-5.1"
    ; stop_reason = T.EndTurn
    ; content = [ T.Text "answer" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let unchanged label body =
    match (K.extract_reasoning_content resp body).content with
    | [ T.Text "answer" ] -> ()
    | _ -> fail (label ^ ": expected unchanged response")
  in
  unchanged "blank reasoning" (response_json ~reasoning:(Some "   ") ());
  unchanged "no choices" {|{"choices":[]}|};
  unchanged "malformed json" {|not json|}
;;

let test_parse_stream_chunk_delegates_reasoning_delta () =
  let data =
    {|{"id":"chunk-1","model":"glm-5.1","choices":[{"delta":{"reasoning_content":"thinking","content":"token"},"index":0}]}|}
  in
  match K.parse_stream_chunk data with
  | Some chunk ->
    check (option string) "content" (Some "token") chunk.delta_content;
    check (option string) "reasoning" (Some "thinking") chunk.delta_reasoning
  | None -> fail "expected stream chunk"
;;

let () =
  run
    "backend_glm_coverage"
    [ ( "classification"
      , [ test_case "error classification matrix" `Quick test_classification_matrix
        ; test_case "http status mapping" `Quick test_http_status_mapping
        ] )
    ; ( "request"
      , [ test_case
            "thinking modes and tool_stream"
            `Quick
            test_build_request_thinking_modes_and_tool_stream
        ; test_case
            "thinking builder delegated"
            `Quick
            test_backend_glm_delegates_thinking_builder_to_shared_request_builder
        ] )
    ; ( "response"
      , [ test_case
            "reasoning content and usage"
            `Quick
            test_parse_response_extracts_reasoning_and_usage
        ; test_case "glm error classes" `Quick test_parse_response_classifies_glm_errors
        ; test_case
            "openai parse errors"
            `Quick
            test_parse_response_wraps_openai_parse_errors
        ; test_case
            "empty and malformed reasoning bodies"
            `Quick
            test_extract_reasoning_handles_empty_and_malformed_bodies
        ; test_case
            "stream reasoning delta"
            `Quick
            test_parse_stream_chunk_delegates_reasoning_delta
        ] )
    ]
;;
