open Llm_provider
open Types

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

let contract replay_policy : Reasoning_replay_contract.t =
  { replay_policy; streaming = No_streaming_reasoning; output_wire = No_output_control }
;;

let source
      ?(kind = Provider_config.OpenAI_compat)
      ?(base_url = "https://provider.example")
      ?(request_path = "/v1/chat/completions")
      ?(model_id = "model-a")
      replay_policy
  =
  match
    Types.Reasoning_source.create
      ~provider_kind:kind
      ~provider_instance:
        (Types.Reasoning_source.provider_instance ~base_url ~request_path)
      ~canonical_model_id:model_id
      ~replay_contract:(contract replay_policy)
  with
  | Ok source -> source
  | Error detail -> Alcotest.fail detail
;;

let message ?(metadata = []) role content : message =
  { role; content; name = None; tool_call_id = None; metadata }
;;

let with_source source role content =
  message ~metadata:(Types.Reasoning_source.metadata source) role content
;;

let reasoning_supported = function
  | Thinking _ | ReasoningDetails _ | RedactedThinking _ -> true
  | Text _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> false
;;

let project ~target ~policy messages =
  Reasoning_history_projection.project
    ~assistant_has_payload:(fun content -> content <> [])
    ~reasoning_block_supported:reasoning_supported
    ~reasoning_target:target
    ~replay_policy:policy
    messages
;;

let content_has_reasoning =
  List.exists (function
    | Thinking _ | ReasoningDetails _ | RedactedThinking _ -> true
    | Text _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> false)
;;

let test_source_exactness_and_classification () =
  let left = source All_assistant_messages in
  let same = source All_assistant_messages in
  let other_endpoint = source ~base_url:"https://other.example" All_assistant_messages in
  let other_model = source ~model_id:"model-b" All_assistant_messages in
  check_bool "same exact source" true (Types.Reasoning_source.equal left same);
  check_bool
    "endpoint identity differs"
    false
    (Types.Reasoning_source.equal left other_endpoint);
  check_bool
    "model identity differs"
    false
    (Types.Reasoning_source.equal left other_model);
  (match Types.Reasoning_source.classify (Types.Reasoning_source.metadata left) with
   | Present actual ->
     check_bool "metadata round-trip" true (Reasoning_source.equal left actual)
   | Absent | Invalid | Duplicate -> Alcotest.fail "expected present source");
  let entry = Types.Reasoning_source.entry left in
  match Types.Reasoning_source.classify [ entry; entry ] with
  | Duplicate -> ()
  | Absent | Present _ | Invalid -> Alcotest.fail "expected duplicate source"
;;

let test_whole_history_source_filtering () =
  let target = source All_assistant_messages in
  let foreign = source ~model_id:"foreign" All_assistant_messages in
  let messages =
    [ with_source target Assistant [ Thinking { content = "keep"; signature = None } ]
    ; with_source foreign Assistant [ Thinking { content = "foreign"; signature = None } ]
    ; message Assistant [ Thinking { content = "unsourced"; signature = None } ]
    ; message User [ Text "next" ]
    ]
  in
  match project ~target ~policy:All_assistant_messages messages with
  | Error error -> Alcotest.fail (Reasoning_history_projection.error_to_string error)
  | Ok projection ->
    check_int
      "foreign and unsourced drops"
      2
      (List.length projection.reasoning_replay_drops);
    check_int
      "reasoning-only empty assistants removed"
      2
      (List.length projection.removed_empty_assistant_indices);
    check_int "kept assistant plus user" 2 (List.length projection.messages);
    let first = List.hd projection.messages in
    check_bool "exact-source reasoning remains" true (content_has_reasoning first.content)
;;

let test_tool_call_policy_applies_across_history () =
  let target = source Tool_call_assistant_messages_all_history in
  let messages =
    [ with_source
        target
        Assistant
        [ Thinking { content = "tool reasoning"; signature = None }
        ; ToolUse { id = "call"; name = "lookup"; input = `Assoc [] }
        ]
    ; message
        Tool
        [ ToolResult
            { tool_use_id = "call"
            ; content = "ok"
            ; outcome = Tool_succeeded
            ; json = None
            ; content_blocks = None
            }
        ]
    ; with_source
        target
        Assistant
        [ Thinking { content = "plain reasoning"; signature = None }; Text "answer" ]
    ]
  in
  match project ~target ~policy:Tool_call_assistant_messages_all_history messages with
  | Error error -> Alcotest.fail (Reasoning_history_projection.error_to_string error)
  | Ok projection ->
    let first = List.nth projection.messages 0 in
    let last = List.nth projection.messages 2 in
    check_bool
      "tool-call assistant keeps reasoning"
      true
      (content_has_reasoning first.content);
    check_bool
      "plain assistant drops reasoning"
      false
      (content_has_reasoning last.content)
;;

let test_selected_unsupported_block_fails_closed () =
  let target = source All_assistant_messages in
  let result =
    Reasoning_history_projection.project
      ~assistant_has_payload:(fun content -> content <> [])
      ~reasoning_block_supported:(fun _ -> false)
      ~reasoning_target:target
      ~replay_policy:All_assistant_messages
      [ with_source target Assistant [ Thinking { content = "x"; signature = None } ] ]
  in
  match result with
  | Error (Unsupported_reasoning_block _) -> ()
  | Error error -> Alcotest.fail (Reasoning_history_projection.error_to_string error)
  | Ok _ -> Alcotest.fail "unsupported selected reasoning must fail"
;;

let test_assistant_message_requires_live_source () =
  let response : api_response =
    { id = "response"
    ; model = "model-a"
    ; stop_reason = EndTurn
    ; content = [ Thinking { content = "reasoning"; signature = None } ]
    ; usage = None
    ; telemetry = None
    }
  in
  (match Types.assistant_message_of_response response with
   | Error Reasoning_source_telemetry_missing -> ()
   | Error _ | Ok _ -> Alcotest.fail "missing telemetry must be explicit");
  let source = source All_assistant_messages in
  let telemetry =
    { Types.default_inference_telemetry with reasoning_source = Some source }
  in
  match
    Types.assistant_message_of_response { response with telemetry = Some telemetry }
  with
  | Error error -> Alcotest.fail (Types.show_assistant_message_error error)
  | Ok assistant ->
    (match Types.Reasoning_source.classify assistant.metadata with
     | Present actual ->
       check_bool "response source persisted" true (Reasoning_source.equal source actual)
     | Absent | Invalid | Duplicate -> Alcotest.fail "expected persisted source")
;;

let preserving_capabilities =
  { Capabilities.default_capabilities with
    reasoning_replay_override = Force_preserve_always
  }
;;

let source_for_config config =
  match Reasoning_dialect.reasoning_source_for_provider_config config with
  | Ok source -> source
  | Error detail -> Alcotest.fail detail
;;

let test_openai_chat_request_uses_whole_history_projection () =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"model-a"
      ~base_url:"https://provider.example"
      ~model_capabilities_override:preserving_capabilities
      ()
  in
  let source = source_for_config config in
  let body =
    Backend_openai.build_request
      ~config
      ~messages:
        [ with_source
            source
            Assistant
            [ Thinking { content = "reasoning"; signature = None }; Text "answer" ]
        ]
      ()
    |> Yojson.Safe.from_string
  in
  let assistant = Yojson.Safe.Util.(body |> member "messages" |> to_list |> List.hd) in
  check_bool
    "reasoning stays in dedicated field"
    true
    Yojson.Safe.Util.(assistant |> member "reasoning_content" |> to_string = "reasoning");
  check_bool
    "visible answer remains distinct"
    true
    Yojson.Safe.Util.(assistant |> member "content" |> to_string = "answer")
;;

let test_ollama_request_replays_native_thinking_field () =
  let capabilities =
    { preserving_capabilities with thinking_control_format = Ollama_think }
  in
  let config =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"model-a"
      ~base_url:"http://localhost:11434"
      ~model_capabilities_override:capabilities
      ()
  in
  let source = source_for_config config in
  let body =
    Backend_ollama.build_request
      ~config
      ~messages:
        [ with_source
            source
            Assistant
            [ Thinking { content = "native reasoning"; signature = None }; Text "answer" ]
        ]
      ()
    |> Yojson.Safe.from_string
  in
  let assistant = Yojson.Safe.Util.(body |> member "messages" |> to_list |> List.hd) in
  check_bool
    "native thinking field"
    true
    Yojson.Safe.Util.(assistant |> member "thinking" |> to_string = "native reasoning")
;;

let test_openai_responses_replays_only_opaque_item () =
  let config =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"model-a"
      ~base_url:"https://api.example"
      ~request_path:"/v1/responses"
      ()
  in
  let source = source_for_config config in
  let opaque =
    Yojson.Safe.to_string
      (`Assoc
          [ "type", `String "reasoning"
          ; "id", `String "reasoning-item"
          ; "summary", `List []
          ; "encrypted_content", `String "opaque"
          ])
  in
  let body =
    Backend_openai_responses.build_request
      ~config
      ~messages:
        [ with_source
            source
            Assistant
            [ Thinking { content = "generic"; signature = None }
            ; RedactedThinking opaque
            ]
        ]
      ()
    |> Yojson.Safe.from_string
  in
  let input = Yojson.Safe.Util.(body |> member "input" |> to_list) in
  check_int "only opaque reasoning item remains" 1 (List.length input);
  check_bool
    "opaque item preserved"
    true
    Yojson.Safe.Util.(List.hd input |> member "type" |> to_string = "reasoning")
;;

let () =
  Alcotest.run
    "provider_agnostic_reasoning_replay"
    [ ( "typed whole-history replay"
      , [ Alcotest.test_case
            "source exactness"
            `Quick
            test_source_exactness_and_classification
        ; Alcotest.test_case
            "whole-history filtering"
            `Quick
            test_whole_history_source_filtering
        ; Alcotest.test_case
            "tool-call policy"
            `Quick
            test_tool_call_policy_applies_across_history
        ; Alcotest.test_case
            "unsupported codec fails"
            `Quick
            test_selected_unsupported_block_fails_closed
        ; Alcotest.test_case
            "response source required"
            `Quick
            test_assistant_message_requires_live_source
        ; Alcotest.test_case
            "OpenAI chat boundary"
            `Quick
            test_openai_chat_request_uses_whole_history_projection
        ; Alcotest.test_case
            "Ollama native boundary"
            `Quick
            test_ollama_request_replays_native_thinking_field
        ; Alcotest.test_case
            "OpenAI Responses opaque boundary"
            `Quick
            test_openai_responses_replays_only_opaque_item
        ] )
    ]
;;
