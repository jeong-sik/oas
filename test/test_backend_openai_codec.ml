open Llm_provider
open Types
module Parse = Backend_openai_parse
module Serialize = Backend_openai_serialize

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let check_float = Alcotest.(check (float 0.0001))

let msg role content : message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

let only label = function
  | [ x ] -> x
  | xs ->
    Alcotest.fail (Printf.sprintf "expected one %s item, got %d" label (List.length xs))
;;

let as_list label = function
  | `List xs -> xs
  | json ->
    Alcotest.fail
      (Printf.sprintf "expected %s list, got %s" label (Yojson.Safe.to_string json))
;;

let member key json = Yojson.Safe.Util.member key json
let to_string json = Yojson.Safe.Util.to_string json
let to_int json = Yojson.Safe.Util.to_int json

let response_json ?(content = `String "ok") ?(finish_reason = "stop") ?message_fields () =
  let message_fields =
    match message_fields with
    | Some fields -> ("content", content) :: fields
    | None -> [ "content", content ]
  in
  `Assoc
    [ "id", `String "chatcmpl-test"
    ; "model", `String "provider-d-test"
    ; ( "choices"
      , `List
          [ `Assoc
              [ "finish_reason", `String finish_reason; "message", `Assoc message_fields ]
          ] )
    ]
;;

let parse_ok json =
  match Parse.parse_openai_response_result (Yojson.Safe.to_string json) with
  | Ok response -> response
  | Error msg -> Alcotest.fail ("unexpected parse error: " ^ msg)
;;

let test_parse_reasoning_content_and_tool_calls_coexist () =
  (* 2025-2026 providers (DeepSeek, Kimi, Qwen, MiMo) return reasoning_content
     alongside tool_calls. Both must survive parsing into [content]: the
     reasoning as a Thinking block and the call as a ToolUse block. *)
  let json =
    response_json
      ~content:(`String "")
      ~finish_reason:"tool_calls"
      ~message_fields:
        [ "reasoning_content", `String "think step by step"
        ; ( "tool_calls"
          , `List
              [ `Assoc
                  [ "id", `String "call-1"
                  ; "type", `String "function"
                  ; ( "function"
                    , `Assoc
                        [ "name", `String "search"; "arguments", `String {|{"q":"x"}|} ] )
                  ]
              ] )
        ]
      ()
  in
  let response = parse_ok json in
  let has_thinking =
    List.exists
      (function
        | Thinking { content; _ } -> content = "think step by step"
        | _ -> false)
      response.content
  in
  let has_tool =
    List.exists
      (function
        | ToolUse { name; _ } -> name = "search"
        | _ -> false)
      response.content
  in
  check_bool "reasoning_content -> Thinking" true has_thinking;
  check_bool "tool_calls -> ToolUse" true has_tool;
  check_bool "stop_reason is StopToolUse" true (response.stop_reason = StopToolUse)
;;

let test_content_parts_cover_modalities () =
  let parts =
    Serialize.openai_content_parts_of_blocks
      [ Text "hello"
      ; Image { media_type = "image/png"; data = "img"; source_type = "base64" }
      ; Document { media_type = "application/pdf"; data = "doc"; source_type = "base64" }
      ; Audio { media_type = "wav"; data = "aud"; source_type = "base64" }
      ; Thinking { thinking_type = "reasoning"; content = "hidden" }
      ; RedactedThinking "redacted"
      ; ToolUse { id = "tc"; name = "tool"; input = `Assoc [] }
      ]
  in
  check_int "parts" 4 (List.length parts);
  let text = List.nth parts 0 in
  check_string "text type" "text" (member "type" text |> to_string);
  check_string "text value" "hello" (member "text" text |> to_string);
  let image = List.nth parts 1 in
  check_string "image type" "image_url" (member "type" image |> to_string);
  check_string
    "image url"
    "data:image/png;base64,img"
    (member "image_url" image |> member "url" |> to_string);
  let document = List.nth parts 2 in
  check_string
    "document url"
    "data:application/pdf;base64,doc"
    (member "image_url" document |> member "url" |> to_string);
  let audio = List.nth parts 3 in
  check_string "audio type" "input_audio" (member "type" audio |> to_string);
  check_string
    "audio format"
    "wav"
    (member "input_audio" audio |> member "format" |> to_string)
;;

let test_provider_d_user_messages_text_tool_and_empty () =
  let user =
    msg
      User
      [ Text "first"
      ; Text "second"
      ; ToolResult
          { tool_use_id = "call-1"
          ; content = "42"
          ; is_error = false
          ; json = None
          ; content_blocks = None
          }
      ]
  in
  let messages = Serialize.openai_messages_of_message user in
  check_int "user + tool messages" 2 (List.length messages);
  let user_json = List.nth messages 0 in
  check_string "user role" "user" (member "role" user_json |> to_string);
  check_string "user content" "first\nsecond" (member "content" user_json |> to_string);
  let tool_json = List.nth messages 1 in
  check_string "tool role" "tool" (member "role" tool_json |> to_string);
  check_string "tool id" "call-1" (member "tool_call_id" tool_json |> to_string);
  check_string "tool content" "42" (member "content" tool_json |> to_string);
  let empty_user =
    Serialize.openai_messages_of_message
      (msg User [ Thinking { thinking_type = ""; content = "x" } ])
  in
  check_int "empty user drops message" 0 (List.length empty_user)
;;

let test_user_multimodal_preserve_and_visual_first () =
  let content =
    [ Text "caption"
    ; Image { media_type = "image/jpeg"; data = "jpeg"; source_type = "base64" }
    ]
  in
  let openai =
    Serialize.openai_messages_of_message (msg User content) |> only "openai"
  in
  let provider_d_parts = member "content" openai |> as_list "openai content" in
  check_string
    "openai preserves text first"
    "text"
    (List.nth provider_d_parts 0 |> member "type" |> to_string);
  let visual_first =
    Serialize.ollama_messages_of_message
      ~model_id:"model-f-gemma-4-27b-it"
      (msg User content)
    |> only "ollama"
  in
  let visual_parts = member "content" visual_first |> as_list "ollama content" in
  check_string
    "visual model moves image first"
    "image_url"
    (List.nth visual_parts 0 |> member "type" |> to_string)
;;

let test_assistant_tool_calls_provider_d_ollama_and_provider_k () =
  let assistant =
    msg
      Assistant
      [ ToolUse { id = "call-1"; name = "lookup"; input = `Assoc [ "q", `String "x" ] } ]
  in
  let openai = Serialize.openai_messages_of_message assistant |> only "openai" in
  check_string "assistant role" "assistant" (member "role" openai |> to_string);
  Alcotest.(check bool)
    "openai content null"
    true
    (member "content" openai = `Null);
  let call = member "tool_calls" openai |> as_list "tool_calls" |> only "tool_call" in
  check_string
    "openai arguments string"
    {|{"q":"x"}|}
    (member "function" call |> member "arguments" |> to_string);
  let ollama = Serialize.ollama_messages_of_message assistant |> only "ollama" in
  let ollama_call =
    member "tool_calls" ollama |> as_list "ollama tool_calls" |> only "tool_call"
  in
  check_string
    "ollama arguments raw json"
    "x"
    (member "function" ollama_call |> member "arguments" |> member "q" |> to_string);
  let glm =
    Serialize.provider_k_messages_of_message
      (msg
         Assistant
         [ Text "answer"; Thinking { thinking_type = "reasoning"; content = "because" } ])
    |> only "glm"
  in
  check_string "glm content" "answer" (member "content" glm |> to_string);
  check_string
    "glm reasoning"
    "because"
    (member "reasoning_content" glm |> to_string)
;;

let test_system_and_tool_role_messages () =
  let system =
    Serialize.openai_messages_of_message (msg System [ Text "sys" ]) |> only "system"
  in
  check_string "system role" "system" (member "role" system |> to_string);
  check_string "system content" "sys" (member "content" system |> to_string);
  let tool =
    Serialize.openai_messages_of_message
      (msg
         Tool
         [ ToolResult
             { tool_use_id = "call-2"
             ; content = "ok"
             ; is_error = false
             ; json = None
             ; content_blocks = None
             }
         ])
    |> only "tool"
  in
  check_string "tool role" "tool" (member "role" tool |> to_string);
  check_string "tool call id" "call-2" (member "tool_call_id" tool |> to_string);
  let fallback =
    Serialize.openai_messages_of_message (msg Tool [ Text "plain fallback" ])
    |> only "fallback"
  in
  check_string "fallback role" "user" (member "role" fallback |> to_string);
  check_string "fallback content" "plain fallback" (member "content" fallback |> to_string)
;;

let test_strip_orphaned_tool_results_dedupes_and_drops_empty () =
  let messages =
    [ msg
        Assistant
        [ ToolUse { id = "call-1"; name = "a"; input = `Null }
        ; ToolUse { id = "call-2"; name = "b"; input = `Null }
        ]
    ; msg
        User
        [ ToolResult
            { tool_use_id = "call-1"
            ; content = "first"
            ; is_error = false
            ; json = None
            ; content_blocks = None
            }
        ; ToolResult
            { tool_use_id = "call-1"
            ; content = "dupe"
            ; is_error = false
            ; json = None
            ; content_blocks = None
            }
        ; ToolResult
            { tool_use_id = "orphan"
            ; content = "bad"
            ; is_error = true
            ; json = None
            ; content_blocks = None
            }
        ; Text "kept"
        ]
    ; msg
        User
        [ ToolResult
            { tool_use_id = "orphan-2"
            ; content = "drop"
            ; is_error = false
            ; json = None
            ; content_blocks = None
            }
        ]
    ; msg Assistant [ Text "done" ]
    ]
  in
  let stripped = Serialize.strip_orphaned_tool_results messages in
  check_int "drops empty orphan-only message" 3 (List.length stripped);
  let user = List.nth stripped 1 in
  check_int "keeps one matching result and text" 2 (List.length user.content);
  match user.content with
  | ToolResult { tool_use_id; content; _ } :: Text text :: _ ->
    check_string "tool id" "call-1" tool_use_id;
    check_string "tool content" "first" content;
    check_string "text" "kept" text
  | _ -> Alcotest.fail "unexpected stripped user content"
;;

let test_strip_thinking_blocks () =
  let messages =
    [ msg
        Assistant
        [ Thinking { thinking_type = "reasoning"; content = "x" }; Text "visible" ]
    ; msg User [ Text "same" ]
    ]
  in
  let stripped = Serialize.strip_thinking_blocks messages in
  (match (List.hd stripped).content with
   | [ Text "visible" ] -> ()
   | _ -> Alcotest.fail "thinking block should be stripped");
  match (List.nth stripped 1).content with
  | [ Text "same" ] -> ()
  | _ -> Alcotest.fail "unchanged text block should remain"
;;

let test_tool_choice_and_tool_schema_conversion () =
  check_string
    "auto"
    "\"auto\""
    (Serialize.tool_choice_to_provider_d_json Auto |> Yojson.Safe.to_string);
  check_string
    "required"
    "\"required\""
    (Serialize.tool_choice_to_provider_d_json Any |> Yojson.Safe.to_string);
  check_string
    "none"
    "\"none\""
    (Serialize.tool_choice_to_provider_d_json None_ |> Yojson.Safe.to_string);
  let tool_choice = Serialize.tool_choice_to_provider_d_json (Tool "lookup") in
  check_string
    "tool choice name"
    "lookup"
    (member "function" tool_choice |> member "name" |> to_string);
  let schema = `Assoc [ "type", `String "object" ] in
  let with_input_schema =
    Serialize.build_provider_d_tool_json
      (`Assoc
          [ "name", `String "direct"; "description", `String "d"; "input_schema", schema ])
  in
  check_string
    "input_schema passes through"
    "object"
    (member "function" with_input_schema
     |> member "parameters"
     |> member "type"
     |> to_string);
  let legacy =
    Serialize.build_provider_d_tool_json
      (`Assoc
          [ "name", `String "legacy"
          ; ( "parameters"
            , `List
                [ `Assoc
                    [ "name", `String "city"
                    ; "description", `String "City name"
                    ; "param_type", `String "string"
                    ; "required", `Bool true
                    ]
                ; `Assoc [ "description", `String "missing name" ]
                ] )
          ])
  in
  let params = member "function" legacy |> member "parameters" in
  check_string "legacy type" "object" (member "type" params |> to_string);
  check_string
    "legacy property type"
    "string"
    (member "properties" params |> member "city" |> member "type" |> to_string);
  check_string
    "legacy required"
    "city"
    (member "required" params |> as_list "required" |> only "required" |> to_string);
  let passthrough = `String "raw" in
  Alcotest.(check bool)
    "non-object passthrough"
    true
    (Serialize.build_provider_d_tool_json passthrough = passthrough)
;;

let ignored_blocks : content_block list =
  [ Thinking { thinking_type = "reasoning"; content = "   " }
  ; RedactedThinking "hidden"
  ; ToolResult
      { tool_use_id = "call-x"
      ; content = "ignored"
      ; is_error = false
      ; json = None
      ; content_blocks = None
      }
  ; Image { media_type = "image/png"; data = "img"; source_type = "base64" }
  ; Document { media_type = "application/pdf"; data = "doc"; source_type = "base64" }
  ; Audio { media_type = "wav"; data = "aud"; source_type = "base64" }
  ]
;;

let test_serializer_ignored_block_variants () =
  check_int
    "openai tool_calls ignores non-tool blocks"
    0
    (Serialize.tool_calls_to_openai_json ignored_blocks |> List.length);
  let ollama =
    Serialize.ollama_messages_of_message (msg Assistant ignored_blocks) |> only "ollama"
  in
  Alcotest.(check bool)
    "ollama has no tool_calls"
    true
    (member "tool_calls" ollama = `Null);
  let assistant =
    Serialize.openai_messages_of_message (msg Assistant ignored_blocks)
    |> only "assistant"
  in
  check_string "assistant content is empty" "" (member "content" assistant |> to_string);
  Alcotest.(check bool)
    "assistant has no tool_calls"
    true
    (member "tool_calls" assistant = `Null);
  let glm =
    Serialize.provider_k_messages_of_message (msg Assistant ignored_blocks)
    |> only "glm"
  in
  Alcotest.(check bool)
    "blank reasoning omitted"
    true
    (member "reasoning_content" glm = `Null);
  let tool_fallback_blocks =
    [ Thinking { thinking_type = "reasoning"; content = "   " }
    ; RedactedThinking "hidden"
    ; ToolUse { id = "local-call"; name = "local"; input = `Null }
    ; Image { media_type = "image/png"; data = "img"; source_type = "base64" }
    ; Document { media_type = "application/pdf"; data = "doc"; source_type = "base64" }
    ; Audio { media_type = "wav"; data = "aud"; source_type = "base64" }
    ]
  in
  let tool_fallback =
    Serialize.openai_messages_of_message (msg Tool tool_fallback_blocks)
    |> only "tool fallback"
  in
  check_string "tool fallback role" "user" (member "role" tool_fallback |> to_string)
;;

let test_strip_helpers_cover_non_tool_variants () =
  let messages =
    [ msg Assistant ignored_blocks
    ; msg
        User
        (Text "kept"
         :: ToolUse { id = "local-call"; name = "local"; input = `Null }
         :: ignored_blocks)
    ]
  in
  let stripped = Serialize.strip_orphaned_tool_results messages in
  check_int "both messages remain" 2 (List.length stripped);
  let user = List.nth stripped 1 in
  Alcotest.(check bool)
    "orphan tool result dropped"
    false
    (List.exists
       (function
         | ToolResult _ -> true
         | _ -> false)
       user.content);
  let no_thinking =
    Serialize.strip_thinking_blocks
      [ msg
          Assistant
          [ RedactedThinking "hidden"
          ; ToolUse { id = "call"; name = "lookup"; input = `Null }
          ; ToolResult
              { tool_use_id = "call"
              ; content = "ok"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; Image { media_type = "image/png"; data = "img"; source_type = "base64" }
          ; Document
              { media_type = "application/pdf"; data = "doc"; source_type = "base64" }
          ; Audio { media_type = "wav"; data = "aud"; source_type = "base64" }
          ]
      ]
  in
  check_int "non-thinking blocks preserved" 6 (List.length (List.hd no_thinking).content)
;;

let test_parallel_tool_calls_fields () =
  (* SSOT for the parallel-tool-call wire field: emitted only to disable, and
     only when tools are present. *)
  let fields ~disable ~tools =
    Serialize.parallel_tool_calls_fields ~disable_parallel:disable ~tools_present:tools
  in
  check_int
    "disable + tools -> one field"
    1
    (List.length (fields ~disable:true ~tools:true));
  check_int
    "disable + no tools -> empty"
    0
    (List.length (fields ~disable:true ~tools:false));
  check_int "not disabled -> empty" 0 (List.length (fields ~disable:false ~tools:true));
  match fields ~disable:true ~tools:true with
  | [ ("parallel_tool_calls", `Bool false) ] -> ()
  | _ -> Alcotest.fail "expected parallel_tool_calls:false singleton"
;;

let test_tool_schema_defaults_and_legacy_edge_params () =
  let defaulted =
    Serialize.build_provider_d_tool_json
      (`Assoc [ "name", `Int 1; "description", `Bool true ])
  in
  check_string
    "default name"
    "tool"
    (member "function" defaulted |> member "name" |> to_string);
  check_string
    "default description"
    ""
    (member "function" defaulted |> member "description" |> to_string);
  let legacy =
    Serialize.build_provider_d_tool_json
      (`Assoc
          [ "name", `String "legacy-edge"
          ; ( "parameters"
            , `List
                [ `Assoc [ "name", `Int 1; "description", `String "bad name" ]
                ; `Assoc
                    [ "name", `String "flag"
                    ; "description", `Bool true
                    ; "type", `Bool true
                    ; "required", `String "yes"
                    ]
                ; `String "ignored"
                ] )
          ])
  in
  let params = member "function" legacy |> member "parameters" in
  check_string
    "default legacy param type"
    "string"
    (member "properties" params |> member "flag" |> member "type" |> to_string);
  check_string
    "default legacy param description"
    ""
    (member "properties" params |> member "flag" |> member "description" |> to_string);
  check_int
    "non-bool required omitted"
    0
    (member "required" params |> as_list "required" |> List.length)
;;

let test_strip_json_markdown_fences_variants () =
  check_string "plain" "plain" (Parse.strip_json_markdown_fences " plain ");
  check_string
    "json fence"
    {|{"a":1}|}
    (Parse.strip_json_markdown_fences "```json\n{\"a\":1}\n```");
  check_string
    "unterminated fence"
    "```\n{\"a\":1}"
    (Parse.strip_json_markdown_fences "```\n{\"a\":1}")
;;

let test_usage_provider_d_fallbacks () =
  let usage =
    Parse.usage_of_provider_d_json
      (`Assoc
          [ ( "usage"
            , `Assoc
                [ "input_tokens", `Int 11
                ; "output_tokens", `Int 5
                ; "prompt_cache_hit_tokens", `Int 4
                ] )
          ])
  in
  (match usage with
   | Some u ->
     check_int "input" 11 u.input_tokens;
     check_int "output" 5 u.output_tokens;
     check_int "cache read" 4 u.cache_read_input_tokens
   | None -> Alcotest.fail "expected usage");
  let usage =
    Parse.usage_of_provider_d_json
      (`Assoc
          [ ( "usage"
            , `Assoc
                [ "prompt_tokens", `Int 7
                ; "completion_tokens", `Int 3
                ; "prompt_tokens_details", `Assoc [ "cached_tokens", `Int 2 ]
                ] )
          ])
  in
  match usage with
  | Some u ->
    check_int "prompt" 7 u.input_tokens;
    check_int "completion" 3 u.output_tokens;
    check_int "details cache" 2 u.cache_read_input_tokens
  | None -> Alcotest.fail "expected usage with details"
;;

let test_parse_text_list_reasoning_and_reported_telemetry () =
  let json =
    response_json
      ~content:
        (`List
            [ `Assoc [ "type", `String "text"; "text", `String "hello" ]
            ; `Assoc [ "type", `String "ignored" ]
            ; `String " raw"
            ])
      ~message_fields:[ "reasoning_content", `String "reported reasoning" ]
      ()
  in
  let json =
    match json with
    | `Assoc fields ->
      `Assoc
        (fields
         @ [ ( "usage"
             , `Assoc
                 [ "prompt_tokens", `Int 12
                 ; "completion_tokens", `Int 7
                 ; "completion_tokens_details", `Assoc [ "reasoning_tokens", `Int 5 ]
                 ] )
           ; ( "timings"
             , `Assoc
                 [ "prompt_n", `Int 12
                 ; "prompt_ms", `Float 20.5
                 ; "predicted_n", `Int 7
                 ; "predicted_per_second", `Float 14.0
                 ; "cache_n", `Int 2
                 ] )
           ; "system_fingerprint", `String "fp-test"
           ; "peak_memory_gb", `Float 1.25
           ])
    | _ -> json
  in
  let response = parse_ok json in
  (match response.content with
   | [ Thinking { content = "reported reasoning"; _ }; Text "hello raw" ] -> ()
   | _ -> Alcotest.fail "expected thinking + concatenated text");
  match response.telemetry with
  | Some
      { system_fingerprint = Some fp
      ; timings = Some timings
      ; reasoning_tokens = Some rt
      ; reasoning_tokens_estimated
      ; peak_memory_gb = Some peak
      ; _
      } ->
    check_string "fingerprint" "fp-test" fp;
    check_int "reasoning tokens" 5 rt;
    check_bool "reported not estimated" false reasoning_tokens_estimated;
    check_float "prompt ms" 20.5 (Option.get timings.prompt_ms);
    check_float "predicted per second" 14.0 (Option.get timings.predicted_per_second);
    check_int "cache_n" 2 (Option.get timings.cache_n);
    check_float "peak" 1.25 peak
  | _ -> Alcotest.fail "expected telemetry"
;;

let test_parse_reasoning_estimate_and_fenced_json () =
  let json =
    response_json
      ~content:(`String "```json\n{\"ok\":true}\n```")
      ~message_fields:[ "reasoning", `String "abcdefghij" ]
      ()
  in
  let response = parse_ok json in
  (match response.content with
   | [ Thinking { content = "abcdefghij"; _ }; Text {|{"ok":true}|} ] -> ()
   | _ -> Alcotest.fail "expected estimated reasoning and stripped JSON text");
  match response.telemetry with
  | Some
      { timings = None; reasoning_tokens = Some n; reasoning_tokens_estimated = true; _ }
    -> check_bool "estimated at least one token" true (n >= 1)
  | _ -> Alcotest.fail "expected estimated reasoning telemetry"
;;

let test_parse_tool_calls_filters_malformed_and_sets_stop_reason () =
  let json =
    response_json
      ~content:`Null
      ~finish_reason:"unexpected-finish"
      ~message_fields:
        [ ( "tool_calls"
          , `List
              [ `Assoc
                  [ "id", `String "call-ok"
                  ; ( "function"
                    , `Assoc
                        [ "name", `String "lookup"
                        ; "arguments", `String {|{"city":"Seoul"}|}
                        ] )
                  ]
              ; `Assoc [ "id", `String "broken"; "function", `Assoc [] ]
              ] )
        ]
      ()
  in
  let response = parse_ok json in
  match response.stop_reason, response.content with
  | StopToolUse, [ ToolUse { id; name; input } ] ->
    check_string "tool id" "call-ok" id;
    check_string "tool name" "lookup" name;
    check_string "tool arg" "Seoul" (member "city" input |> to_string)
  | _ -> Alcotest.fail "expected valid tool call and StopToolUse"
;;

let test_parse_error_default_message () =
  let json = `Assoc [ "error", `Assoc [] ] |> Yojson.Safe.to_string in
  match Parse.parse_openai_response_result json with
  | Error msg -> check_string "default error" "Unknown API error" msg
  | Ok _ -> Alcotest.fail "expected API error"
;;

let test_parse_edge_shapes_for_text_and_telemetry () =
  let invalid_fence =
    parse_ok (response_json ~content:(`String "```json\nnot-json\n```") ())
  in
  (match invalid_fence.content with
   | [ Text "```json\nnot-json\n```" ] -> ()
   | _ -> Alcotest.fail "invalid JSON fence should keep original text");
  let no_text =
    parse_ok
      (response_json
         ~content:
           (`List
               [ `Assoc [ "text", `Int 1 ]; `Assoc [ "type", `String "ignored" ]; `Int 7 ])
         ~message_fields:[ "reasoning_content", `String "  " ]
         ())
  in
  check_int "invalid content blocks produce no content" 0 (List.length no_text.content);
  let telemetry =
    parse_ok
      (`Assoc
          [ "id", `String "chatcmpl-telemetry"
          ; "model", `String "provider-d-test"
          ; ( "choices"
            , `List
                [ `Assoc
                    [ "finish_reason", `String "length"
                    ; "message", `Assoc [ "content", `Assoc [ "unexpected", `Bool true ] ]
                    ]
                ] )
          ; ( "usage"
            , `Assoc
                [ "input_tokens", `Int 11
                ; "output_tokens", `Int 5
                ; "prompt_tps", `Int 22
                ; "generation_tps", `Int 50
                ; "peak_memory", `Int 3
                ] )
          ])
  in
  (match telemetry.stop_reason with
   | MaxTokens -> ()
   | _ -> Alcotest.fail "length should map to MaxTokens");
  match telemetry.telemetry with
  | Some { timings = Some t; peak_memory_gb = Some peak; _ } ->
    check_float "int prompt tps" 22.0 (Option.get t.prompt_per_second);
    check_float "int peak memory" 3.0 peak
  | _ -> Alcotest.fail "expected telemetry from integer rates"
;;

let () =
  Alcotest.run
    "backend_openai_codec"
    [ ( "serialize"
      , [ Alcotest.test_case
            "content parts cover modalities"
            `Quick
            test_content_parts_cover_modalities
        ; Alcotest.test_case
            "openai user text/tool/empty"
            `Quick
            test_provider_d_user_messages_text_tool_and_empty
        ; Alcotest.test_case
            "multimodal ordering policies"
            `Quick
            test_user_multimodal_preserve_and_visual_first
        ; Alcotest.test_case
            "assistant tool calls provider variants"
            `Quick
            test_assistant_tool_calls_provider_d_ollama_and_provider_k
        ; Alcotest.test_case
            "system and tool roles"
            `Quick
            test_system_and_tool_role_messages
        ; Alcotest.test_case
            "strip orphaned tool results"
            `Quick
            test_strip_orphaned_tool_results_dedupes_and_drops_empty
        ; Alcotest.test_case "strip thinking blocks" `Quick test_strip_thinking_blocks
        ; Alcotest.test_case
            "tool choice and schema conversion"
            `Quick
            test_tool_choice_and_tool_schema_conversion
        ; Alcotest.test_case
            "ignored block variants"
            `Quick
            test_serializer_ignored_block_variants
        ; Alcotest.test_case
            "strip helpers non-tool variants"
            `Quick
            test_strip_helpers_cover_non_tool_variants
        ; Alcotest.test_case
            "tool schema defaults and legacy edge params"
            `Quick
            test_tool_schema_defaults_and_legacy_edge_params
        ; Alcotest.test_case
            "parallel_tool_calls fields SSOT"
            `Quick
            test_parallel_tool_calls_fields
        ] )
    ; ( "parse"
      , [ Alcotest.test_case
            "strip markdown fences variants"
            `Quick
            test_strip_json_markdown_fences_variants
        ; Alcotest.test_case "usage fallbacks" `Quick test_usage_provider_d_fallbacks
        ; Alcotest.test_case
            "text list reasoning and reported telemetry"
            `Quick
            test_parse_text_list_reasoning_and_reported_telemetry
        ; Alcotest.test_case
            "reasoning estimate and fenced JSON"
            `Quick
            test_parse_reasoning_estimate_and_fenced_json
        ; Alcotest.test_case
            "tool calls filter malformed"
            `Quick
            test_parse_tool_calls_filters_malformed_and_sets_stop_reason
        ; Alcotest.test_case
            "reasoning_content and tool_calls coexist"
            `Quick
            test_parse_reasoning_content_and_tool_calls_coexist
        ; Alcotest.test_case
            "error default message"
            `Quick
            test_parse_error_default_message
        ; Alcotest.test_case
            "edge text shapes and telemetry"
            `Quick
            test_parse_edge_shapes_for_text_and_telemetry
        ] )
    ]
;;
