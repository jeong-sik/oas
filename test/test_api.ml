(** Tests for api.ml -- content block JSON, build_body_assoc, parse_response *)

open Alcotest
open Agent_sdk

(* Helper: compare content_block via show string *)
let check_block msg expected actual =
  check string msg
    (Types.show_content_block expected)
    (Types.show_content_block actual)

(* ------------------------------------------------------------------ *)
(* content_block_to_json / content_block_of_json round-trips            *)
(* ------------------------------------------------------------------ *)

let test_text_round_trip () =
  let block = Types.Text "hello world" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "text" block parsed
  | None -> fail "returned None"

let test_thinking_round_trip () =
  let block = Types.Thinking { thinking_type = "sig123"; content = "I think therefore I am" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "thinking" block parsed
  | None -> fail "returned None"

let test_redacted_thinking_round_trip () =
  let block = Types.RedactedThinking "redacted_data_blob" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "redacted_thinking" block parsed
  | None -> fail "returned None"

let test_tool_use_round_trip () =
  let block = Types.ToolUse { id = "tu_001"; name = "calculator"; input = `Assoc [("expr", `String "2+2")] } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_use" block parsed
  | None -> fail "returned None"

let test_tool_result_round_trip () =
  let block = Types.ToolResult { tool_use_id = "tu_001"; content = "4"; is_error = false; json = Types.try_parse_json "4" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result" block parsed
  | None -> fail "returned None"

let test_tool_result_error_round_trip () =
  let block = Types.ToolResult { tool_use_id = "tu_002"; content = "failed"; is_error = true; json = None } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result_error" block parsed
  | None -> fail "returned None"

let test_image_round_trip () =
  let block = Types.Image { media_type = "image/png"; data = "abc"; source_type = "base64" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "image" block parsed
  | None -> fail "returned None"

let test_document_round_trip () =
  let block = Types.Document { media_type = "application/pdf"; data = "pdf"; source_type = "base64" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "document" block parsed
  | None -> fail "returned None"

let test_unknown_type_returns_none () =
  let json = `Assoc [("type", `String "future_block"); ("data", `String "x")] in
  match Api.content_block_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for unknown type"

(* ------------------------------------------------------------------ *)
(* build_body_assoc                                                     *)
(* ------------------------------------------------------------------ *)

let make_state ?thinking_budget ?tool_choice () =
  let config = { Types.default_config with
    system_prompt = Some "You are helpful.";
    thinking_budget;
    tool_choice;
  } in
  { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }

let test_build_body_basic () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check string "model present" "claude-sonnet-4-6-20250514"
    (json |> member "model" |> to_string);
  check bool "stream false" false
    (json |> member "stream" |> to_bool);
  check string "system prompt" "You are helpful."
    (json |> member "system" |> to_string)

let test_build_body_with_thinking_budget () =
  let config = make_state ~thinking_budget:1024 () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  check string "thinking type" "enabled"
    (thinking |> member "type" |> to_string);
  check int "budget_tokens" 1024
    (thinking |> member "budget_tokens" |> to_int)

let test_build_body_without_thinking () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let has_thinking = List.exists (fun (k, _) -> k = "thinking") assoc in
  check bool "no thinking key" false has_thinking

let test_build_body_with_tool_choice () =
  let config = make_state ~tool_choice:(Types.Tool "calculator") () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tc = json |> member "tool_choice" in
  check string "tool_choice type" "tool"
    (tc |> member "type" |> to_string);
  check string "tool_choice name" "calculator"
    (tc |> member "name" |> to_string)

let test_build_body_with_tools () =
  let tool_json = `Assoc [("name", `String "calc"); ("description", `String "calc")] in
  let assoc = Api.build_body_assoc ~config:(make_state ()) ~messages:[]
    ~tools:[tool_json] ~stream:true () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check bool "stream true" true (json |> member "stream" |> to_bool);
  let tools = json |> member "tools" |> to_list in
  check int "1 tool" 1 (List.length tools)

let test_build_openai_body_with_qwen_sampling () =
  let state = {
    Types.config = {
      Types.default_config with
      model = "qwen3.5-35b-a3b-ud-q8-xl";
      temperature = Some 0.6;
      top_p = Some 0.95;
      top_k = Some 20;
      min_p = Some 0.01;
      enable_thinking = Some false;
    };
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in
  let json =
    Api.build_openai_body ~config:state ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  check (option (float 0.001)) "top_p" (Some 0.95) (json |> member "top_p" |> to_float_option);
  check (option int) "top_k" (Some 20) (json |> member "top_k" |> to_int_option);
  check (option (float 0.001)) "min_p" (Some 0.01) (json |> member "min_p" |> to_float_option);
  check bool "enable_thinking false"
    false
    (json |> member "chat_template_kwargs" |> member "enable_thinking" |> to_bool)

let test_build_openai_body_omits_qwen_only_fields_for_generic_compat () =
  let provider_config =
    Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" ()
  in
  let state = {
    Types.config = {
      Types.default_config with
      model = provider_config.model_id;
      top_p = Some 0.9;
      top_k = Some 40;
      min_p = Some 0.05;
      enable_thinking = Some false;
      response_format_json = true;
      tool_choice = Some Types.Any;
    };
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in
  let tool_json =
    `Assoc
      [
        ("name", `String "calculator");
        ("description", `String "math");
        ("input_schema", `Assoc [("type", `String "object")]);
      ]
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[]
      ~tools:[tool_json] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check (option (float 0.001)) "top_p kept" (Some 0.9)
    (json |> member "top_p" |> to_float_option);
  check (option int) "top_k omitted" None
    (json |> member "top_k" |> to_int_option);
  check (option (float 0.001)) "min_p omitted" None
    (json |> member "min_p" |> to_float_option);
  check bool "chat_template_kwargs omitted" false
    (List.mem_assoc "chat_template_kwargs" assoc);
  check bool "response_format preserved" true
    (List.mem_assoc "response_format" assoc);
  check bool "tool_choice preserved" true
    (List.mem_assoc "tool_choice" assoc);
  check bool "tools preserved" true
    (List.mem_assoc "tools" assoc)

let test_build_openai_body_uses_glm_thinking_and_auto_tool_choice () =
  let provider_config = {
    Provider.provider = Provider.OpenAICompat {
      base_url = Llm_provider.Zai_catalog.general_base_url;
      auth_header = None;
      path = "/chat/completions";
      static_token = None;
    };
    model_id = "glm-5";
    api_key_env = "";
  } in
  let state = {
    Types.config = {
      Types.default_config with
      model = provider_config.model_id;
      enable_thinking = Some true;
      tool_choice = Some (Types.Tool "calculator");
    };
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in
  let tool_json =
    `Assoc [
      ("name", `String "calculator");
      ("description", `String "math");
      ("input_schema", `Assoc [("type", `String "object")]);
    ]
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[]
      ~tools:[tool_json] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  check string "thinking enabled" "enabled"
    (thinking |> member "type" |> to_string);
  check bool "clear_thinking default true" true
    (thinking |> member "clear_thinking" |> to_bool);
  check string "glm tool choice coerced" "auto"
    (json |> member "tool_choice" |> to_string)

let test_build_openai_body_does_not_treat_non_zai_glm_as_glm () =
  let provider_config = {
    Provider.provider = Provider.OpenAICompat {
      base_url = "https://openrouter.ai/api/v1";
      auth_header = None;
      path = "/chat/completions";
      static_token = None;
    };
    model_id = "glm-5";
    api_key_env = "";
  } in
  let state = {
    Types.config = {
      Types.default_config with
      model = provider_config.model_id;
      enable_thinking = Some true;
      tool_choice = Some (Types.Tool "calculator");
    };
    messages = [];
    turn_count = 0;
    usage = Types.empty_usage;
  } in
  let tool_json =
    `Assoc [
      ("name", `String "calculator");
      ("description", `String "math");
      ("input_schema", `Assoc [("type", `String "object")]);
    ]
  in
  let json =
    Api.build_openai_body ~provider_config ~config:state ~messages:[]
      ~tools:[tool_json] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let assoc = to_assoc json in
  check bool "thinking omitted for non-zai glm" false
    (List.mem_assoc "thinking" assoc);
  match json |> member "tool_choice" with
  | `Assoc _ -> ()
  | _ -> fail "non-zai glm tool_choice should preserve named function form"

(* ------------------------------------------------------------------ *)
(* parse_response                                                       *)
(* ------------------------------------------------------------------ *)

let test_parse_response_complete () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_test",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."},
      {"type": "thinking", "signature": "sig", "thinking": "Let me think..."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|} in
  let resp = Api.parse_response json in
  check string "id" "msg_test" resp.id;
  check string "model" "claude-sonnet-4-6-20250514" resp.model;
  check int "content count" 2 (List.length resp.content);
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | sr -> fail (Printf.sprintf "expected EndTurn, got %s" (Types.show_stop_reason sr)));
  (match resp.usage with
   | Some u when u.Types.input_tokens = 100 && u.output_tokens = 50 -> ()
   | _ -> fail "expected usage input=100 output=50")

let test_parse_response_tool_use () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_tu",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "tool_use",
    "content": [
      {"type": "tool_use", "id": "tu_1", "name": "calc", "input": {"x": 1}}
    ],
    "usage": null
  }|} in
  let resp = Api.parse_response json in
  (match resp.stop_reason with
   | Types.StopToolUse -> ()
   | sr -> fail (Printf.sprintf "expected StopToolUse, got %s" (Types.show_stop_reason sr)));
  check bool "usage is None" true (resp.usage = None);
  (match resp.content with
   | [Types.ToolUse { id = "tu_1"; name = "calc"; _ }] -> ()
   | _ -> fail "expected single ToolUse")

let test_parse_response_unknown_stop () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_unk",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "new_future_reason",
    "content": [],
    "usage": null
  }|} in
  let resp = Api.parse_response json in
  (match resp.stop_reason with
   | Types.Unknown "new_future_reason" -> ()
   | sr -> fail (Printf.sprintf "expected Unknown, got %s" (Types.show_stop_reason sr)))

let test_parse_openai_response_strips_fenced_json () =
  let json_str = {|{
    "id": "chatcmpl_test",
    "model": "qwen",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "```json\n{\"engine\":\"default\",\"tool_calling\":false}\n```"
      }
    }]
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    match resp.content with
    | [Types.Text text] ->
        Alcotest.(check string) "stripped json"
          "{\"engine\":\"default\",\"tool_calling\":false}" text
    | _ -> Alcotest.fail "expected stripped text block"

let test_parse_openai_response_reasoning_content () =
  let json_str = {|{
    "id": "chatcmpl_think",
    "model": "qwen3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "The answer is 42.",
        "reasoning_content": "Let me think step by step about the meaning of life."
      }
    }],
    "usage": {"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    check int "2 content blocks" 2 (List.length resp.content);
    (match resp.content with
     | [Types.Thinking { thinking_type; content }; Types.Text text] ->
         check string "thinking_type" "reasoning" thinking_type;
         check string "thinking content"
           "Let me think step by step about the meaning of life." content;
         check string "text" "The answer is 42." text
     | _ -> Alcotest.fail "expected [Thinking; Text]")

let test_parse_openai_response_reasoning_with_tools () =
  let json_str = {|{
    "id": "chatcmpl_think_tool",
    "model": "qwen3.5-35b",
    "choices": [{
      "finish_reason": "tool_calls",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": null,
        "reasoning_content": "I need to call the calculator.",
        "tool_calls": [{
          "id": "call_1",
          "type": "function",
          "function": { "name": "calc", "arguments": "{\"expr\":\"2+2\"}" }
        }]
      }
    }]
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    check int "2 content blocks" 2 (List.length resp.content);
    (match resp.content with
     | [Types.Thinking { content; _ }; Types.ToolUse { name; _ }] ->
         check string "thinking" "I need to call the calculator." content;
         check string "tool name" "calc" name
     | _ -> Alcotest.fail "expected [Thinking; ToolUse]");
    (match resp.stop_reason with
     | Types.StopToolUse -> ()
     | sr -> Alcotest.fail (Printf.sprintf "expected StopToolUse, got %s" (Types.show_stop_reason sr)))

let test_parse_openai_response_blank_reasoning () =
  let json_str = {|{
    "id": "chatcmpl_blank",
    "model": "qwen3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Just text",
        "reasoning_content": "   "
      }
    }]
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    check int "1 content block (blank reasoning filtered)" 1 (List.length resp.content);
    (match resp.content with
     | [Types.Text "Just text"] -> ()
     | _ -> Alcotest.fail "expected [Text] only, blank reasoning should be filtered")

let test_parse_openai_response_no_reasoning () =
  let json_str = {|{
    "id": "chatcmpl_no_think",
    "model": "qwen3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Hello world"
      }
    }]
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    check int "1 content block" 1 (List.length resp.content);
    (match resp.content with
     | [Types.Text "Hello world"] -> ()
     | _ -> Alcotest.fail "expected [Text]")

let test_parse_openai_response_ollama_reasoning () =
  let json_str = {|{
    "id": "chatcmpl_ollama",
    "model": "qwen3.5:35b-a3b-nvfp4",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "The answer is 42.",
        "reasoning": "Ollama uses reasoning field instead of reasoning_content."
      }
    }],
    "usage": {"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30}
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    check int "2 content blocks (thinking + text)" 2 (List.length resp.content);
    (match resp.content with
     | [Types.Thinking { content = t; _ }; Types.Text text] ->
       check string "reasoning text"
         "Ollama uses reasoning field instead of reasoning_content." t;
       check string "content text" "The answer is 42." text;
       (* Verify telemetry estimates reasoning_tokens from content length *)
       (match resp.telemetry with
        | Some tel ->
          (match tel.reasoning_tokens with
           | Some n ->
             check bool "estimated reasoning_tokens > 0" true (n > 0)
           | None -> Alcotest.fail "reasoning_tokens should be estimated from reasoning text")
        | None -> Alcotest.fail "telemetry should be present")
     | _ -> Alcotest.fail "expected [Thinking; Text]")

let test_parse_openai_response_reasoning_content_preferred () =
  let json_str = {|{
    "id": "chatcmpl_both",
    "model": "qwen3.5-35b",
    "choices": [{
      "finish_reason": "stop",
      "index": 0,
      "message": {
        "role": "assistant",
        "content": "Answer.",
        "reasoning_content": "preferred field",
        "reasoning": "fallback field"
      }
    }],
    "usage": {"prompt_tokens": 5, "completion_tokens": 5, "total_tokens": 10}
  }|} in
  match Api.parse_openai_response_result json_str with
  | Error msg -> Alcotest.fail ("unexpected error: " ^ msg)
  | Ok resp ->
    (match resp.content with
     | [Types.Thinking { content = t; _ }; Types.Text text] ->
       check string "reasoning_content wins" "preferred field" t;
       check string "content text" "Answer." text;
       (* Verify telemetry picks up reasoning_tokens from preferred field *)
       (match resp.telemetry with
        | Some tel ->
          (match tel.reasoning_tokens with
           | Some n ->
             (* "preferred field" = 15 chars → ~3-4 tokens *)
             check bool "estimated reasoning_tokens > 0" true (n > 0)
           | None -> Alcotest.fail "reasoning_tokens should be estimated")
        | None -> Alcotest.fail "telemetry should be present")
     | _ -> Alcotest.fail "expected [Thinking; Text]")

(* ------------------------------------------------------------------ *)
(* message_to_json                                                      *)
(* ------------------------------------------------------------------ *)

let test_message_to_json () =
  let msg = { Types.role = Types.User; content = [Types.Text "hi"]; name = None; tool_call_id = None } in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "user" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  check int "1 block" 1 (List.length content)

(* ------------------------------------------------------------------ *)
(* build_body_assoc: cache_system_prompt                                *)
(* ------------------------------------------------------------------ *)

let test_build_body_with_cache () =
  (* Prompt must be >= 4096 chars for cache_control (min token threshold) *)
  let long_prompt = "You are a cached helper. " ^ String.make 4100 'x' in
  let config = { Types.default_config with
    system_prompt = Some long_prompt;
    cache_system_prompt = true;
  } in
  let state = { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage } in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let system = json |> member "system" in
  let blocks = system |> to_list in
  check int "1 system block" 1 (List.length blocks);
  let block = List.hd blocks in
  check string "block type" "text" (block |> member "type" |> to_string);
  let cc = block |> member "cache_control" in
  check string "cache_control type" "ephemeral" (cc |> member "type" |> to_string)

let test_build_body_tools_cache_control () =
  (* When cache_system_prompt is true, last tool gets cache_control *)
  let config = { Types.default_config with
    system_prompt = Some "You are helpful.";
    cache_system_prompt = true;
  } in
  let state = { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage } in
  let tool1 = `Assoc [("name", `String "t1"); ("description", `String "first")] in
  let tool2 = `Assoc [("name", `String "t2"); ("description", `String "second")] in
  let assoc = Api.build_body_assoc ~config:state ~messages:[]
    ~tools:[tool1; tool2] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  check int "2 tools" 2 (List.length tools);
  (* First tool has no cache_control *)
  let first = List.hd tools in
  (match first |> member "cache_control" with
   | `Null -> ()
   | _ -> fail "first tool should not have cache_control");
  (* Last tool has cache_control *)
  let last = List.nth tools 1 in
  let cc = last |> member "cache_control" in
  check string "cache_control type" "ephemeral" (cc |> member "type" |> to_string)

let test_build_body_tools_no_cache_without_flag () =
  (* When cache_system_prompt is false, tools have no cache_control *)
  let state = make_state () in
  let tool1 = `Assoc [("name", `String "t1"); ("description", `String "first")] in
  let assoc = Api.build_body_assoc ~config:state ~messages:[]
    ~tools:[tool1] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  let last = List.hd tools in
  (match last |> member "cache_control" with
   | `Null -> ()
   | _ -> fail "tools should not have cache_control without flag")

let test_build_body_no_system_prompt () =
  let state = { Types.config = Types.default_config;
                messages = []; turn_count = 0; usage = Types.empty_usage } in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let has_system = List.exists (fun (k, _) -> k = "system") assoc in
  check bool "no system key" false has_system

(* ------------------------------------------------------------------ *)
(* parse_response: cache tokens in usage                               *)
(* ------------------------------------------------------------------ *)

let test_parse_response_with_cache_tokens () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_cache",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [{"type": "text", "text": "cached"}],
    "usage": {
      "input_tokens": 50,
      "output_tokens": 25,
      "cache_creation_input_tokens": 1000,
      "cache_read_input_tokens": 800
    }
  }|} in
  let resp = Api.parse_response json in
  match resp.usage with
  | Some u ->
    check int "input" 50 u.Types.input_tokens;
    check int "output" 25 u.output_tokens;
    check int "cache_creation" 1000 u.cache_creation_input_tokens;
    check int "cache_read" 800 u.cache_read_input_tokens
  | None -> fail "expected usage"

(* ------------------------------------------------------------------ *)
(* parse_sse_event                                                      *)
(* ------------------------------------------------------------------ *)

let test_parse_sse_message_start () =
  let data = {|{"message":{"id":"msg_1","model":"claude-sonnet-4-6","usage":{"input_tokens":10}}}|} in
  match Streaming.parse_sse_event (Some "message_start") data with
  | Some (Types.MessageStart { id; model; usage }) ->
    check string "id" "msg_1" id;
    check string "model" "claude-sonnet-4-6" model;
    (match usage with
     | Some u -> check int "input" 10 u.Types.input_tokens
     | None -> fail "expected usage")
  | _ -> fail "expected MessageStart"

let test_parse_sse_content_block_delta_text () =
  let data = {|{"index":0,"delta":{"type":"text_delta","text":"hello"}}|} in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { index; delta = Types.TextDelta t }) ->
    check int "index" 0 index;
    check string "text" "hello" t
  | _ -> fail "expected ContentBlockDelta TextDelta"

let test_parse_sse_content_block_delta_thinking () =
  let data = {|{"index":0,"delta":{"type":"thinking_delta","thinking":"hmm"}}|} in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { delta = Types.ThinkingDelta t; _ }) ->
    check string "thinking" "hmm" t
  | _ -> fail "expected ThinkingDelta"

let test_parse_sse_content_block_delta_input_json () =
  let data = {|{"index":1,"delta":{"type":"input_json_delta","partial_json":"{\"x\":1"}}|} in
  match Streaming.parse_sse_event (Some "content_block_delta") data with
  | Some (Types.ContentBlockDelta { delta = Types.InputJsonDelta j; _ }) ->
    check string "partial json" {|{"x":1|} j
  | _ -> fail "expected InputJsonDelta"

let test_parse_sse_content_block_start () =
  let data = {|{"index":0,"content_block":{"type":"tool_use","id":"tu_1","name":"calc"}}|} in
  match Streaming.parse_sse_event (Some "content_block_start") data with
  | Some (Types.ContentBlockStart { index; content_type; tool_id; tool_name }) ->
    check int "index" 0 index;
    check string "type" "tool_use" content_type;
    check (option string) "tool_id" (Some "tu_1") tool_id;
    check (option string) "tool_name" (Some "calc") tool_name
  | _ -> fail "expected ContentBlockStart"

let test_parse_sse_message_delta () =
  let data = {|{"delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":42}}|} in
  match Streaming.parse_sse_event (Some "message_delta") data with
  | Some (Types.MessageDelta { stop_reason; usage }) ->
    (match stop_reason with
     | Some Types.EndTurn -> ()
     | _ -> fail "expected EndTurn");
    (match usage with
     | Some u -> check int "output" 42 u.Types.output_tokens
     | None -> fail "expected usage")
  | _ -> fail "expected MessageDelta"

let test_parse_sse_message_stop () =
  match Streaming.parse_sse_event (Some "message_stop") "{}" with
  | Some Types.MessageStop -> ()
  | _ -> fail "expected MessageStop"

let test_parse_sse_ping () =
  match Streaming.parse_sse_event (Some "ping") "{}" with
  | Some Types.Ping -> ()
  | _ -> fail "expected Ping"

let test_parse_sse_error () =
  let data = {|{"error":{"message":"rate limited"}}|} in
  match Streaming.parse_sse_event (Some "error") data with
  | Some (Types.SSEError msg) -> check string "error msg" "rate limited" msg
  | _ -> fail "expected SSEError"

let test_parse_sse_unknown_type () =
  match Streaming.parse_sse_event (Some "future_event") "{}" with
  | None -> ()
  | Some _ -> fail "expected None for unknown type"

let test_parse_sse_malformed_json () =
  match Streaming.parse_sse_event (Some "message_start") "not json" with
  | None -> ()
  | Some _ -> fail "expected None for malformed JSON"

(* ------------------------------------------------------------------ *)
(* message_to_json: assistant + mixed content                           *)
(* ------------------------------------------------------------------ *)

let test_message_to_json_assistant () =
  let msg = { Types.role = Types.Assistant;
              content = [Types.Text "hi"; Types.ToolUse { id = "t1"; name = "calc"; input = `Null }];
              name = None; tool_call_id = None } in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "assistant" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  check int "2 blocks" 2 (List.length content)

(* ------------------------------------------------------------------ *)
(* openai_messages_of_message: multimodal user content                  *)
(* ------------------------------------------------------------------ *)

let test_openai_messages_text_only () =
  let msg = { Types.role = Types.User; content = [Types.Text "hello"]; name = None; tool_call_id = None } in
  let msgs = Api.openai_messages_of_message msg in
  check int "1 message" 1 (List.length msgs);
  let open Yojson.Safe.Util in
  let content = List.hd msgs |> member "content" in
  (* text-only should be a plain string, not an array *)
  check string "plain string" "hello" (to_string content)

let test_openai_messages_with_image () =
  let msg = { Types.role = Types.User; content = [
    Types.Text "describe this";
    Types.Image { media_type = "image/png"; data = "abc123"; source_type = "base64" };
  ]; name = None; tool_call_id = None } in
  let msgs = Api.openai_messages_of_message msg in
  check int "1 message" 1 (List.length msgs);
  let open Yojson.Safe.Util in
  let content = List.hd msgs |> member "content" |> to_list in
  check int "2 content parts" 2 (List.length content);
  let first_type = List.nth content 0 |> member "type" |> to_string in
  let second_type = List.nth content 1 |> member "type" |> to_string in
  check string "text part" "text" first_type;
  check string "image part" "image_url" second_type

(* ------------------------------------------------------------------ *)
(* F1: OpenAI API error → Openai_api_error exception                    *)
(* ------------------------------------------------------------------ *)

let test_openai_api_error_returns_error () =
  let error_json = {|{"error":{"message":"Invalid API key","type":"invalid_request_error"}}|} in
  match Api.parse_openai_response_result error_json with
  | Error msg -> check string "error message" "Invalid API key" msg
  | Ok _ -> Alcotest.fail "expected Error on API error"

let test_openai_api_error_unknown_message () =
  let error_json = {|{"error":{}}|} in
  match Api.parse_openai_response_result error_json with
  | Error msg -> check string "unknown error" "Unknown API error" msg
  | Ok _ -> Alcotest.fail "expected Error on empty error"

(* ------------------------------------------------------------------ *)
(* F2: OpenAI error returns structured Error, not exception              *)
(* ------------------------------------------------------------------ *)

let test_openai_error_returns_result () =
  let error_json = {|{"error":{"message":"bad request"}}|} in
  match Api.parse_openai_response_result error_json with
  | Error msg -> check string "error msg" "bad request" msg
  | Ok _ -> Alcotest.fail "expected Error result"

(* ------------------------------------------------------------------ *)
(* Phase 6: additional api_common helpers                               *)
(* ------------------------------------------------------------------ *)

let test_text_blocks_to_string () =
  let blocks = [
    Types.Text "hello";
    Types.Thinking { thinking_type = "s"; content = "hmm" };
    Types.RedactedThinking "r";
    Types.ToolUse { id = "t"; name = "n"; input = `Null };
    Types.ToolResult { tool_use_id = "t"; content = "ok"; is_error = false; json = None };
    Types.Image { media_type = "image/png"; data = ""; source_type = "base64" };
    Types.Text "world";
  ] in
  let result = Api.text_blocks_to_string blocks in
  check string "text+thinking" "hello\nhmm\nworld" result

let test_string_is_blank () =
  check bool "empty is blank" true (Api.string_is_blank "");
  check bool "spaces is blank" true (Api.string_is_blank "   ");
  check bool "text not blank" false (Api.string_is_blank "hi");
  check bool "tabs blank" true (Api.string_is_blank "\t\n ")

let test_json_of_string_or_raw_valid () =
  let result = Api.json_of_string_or_raw {|{"key":"value"}|} in
  let open Yojson.Safe.Util in
  check string "parsed" "value" (result |> member "key" |> to_string)

let test_json_of_string_or_raw_invalid () =
  let result = Api.json_of_string_or_raw "not json" in
  let open Yojson.Safe.Util in
  check string "fallback raw" "not json" (result |> member "raw" |> to_string)

let test_build_body_disable_parallel () =
  let config = { Types.default_config with
    tool_choice = Some Types.Auto;
    disable_parallel_tool_use = true;
  } in
  let state = { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage } in
  let assoc = Api.build_body_assoc ~config:state ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tc = json |> member "tool_choice" in
  check bool "disable_parallel" true
    (tc |> member "disable_parallel_tool_use" |> to_bool)

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  run "Api" [
    "content_block_round_trip", [
      test_case "text" `Quick test_text_round_trip;
      test_case "thinking" `Quick test_thinking_round_trip;
      test_case "redacted_thinking" `Quick test_redacted_thinking_round_trip;
      test_case "tool_use" `Quick test_tool_use_round_trip;
      test_case "tool_result" `Quick test_tool_result_round_trip;
      test_case "tool_result_error" `Quick test_tool_result_error_round_trip;
      test_case "image" `Quick test_image_round_trip;
      test_case "document" `Quick test_document_round_trip;
      test_case "unknown type" `Quick test_unknown_type_returns_none;
    ];
    "build_body_assoc", [
      test_case "basic" `Quick test_build_body_basic;
      test_case "with thinking_budget" `Quick test_build_body_with_thinking_budget;
      test_case "without thinking" `Quick test_build_body_without_thinking;
      test_case "with tool_choice" `Quick test_build_body_with_tool_choice;
      test_case "with tools" `Quick test_build_body_with_tools;
      test_case "with qwen sampling" `Quick test_build_openai_body_with_qwen_sampling;
      test_case "generic compat omits qwen-only fields" `Quick
        test_build_openai_body_omits_qwen_only_fields_for_generic_compat;
      test_case "glm thinking + auto tool choice" `Quick
        test_build_openai_body_uses_glm_thinking_and_auto_tool_choice;
      test_case "non-zai glm avoids glm path" `Quick
        test_build_openai_body_does_not_treat_non_zai_glm_as_glm;
      test_case "with cache_system_prompt" `Quick test_build_body_with_cache;
      test_case "tools cache_control with flag" `Quick test_build_body_tools_cache_control;
      test_case "tools no cache_control without flag" `Quick test_build_body_tools_no_cache_without_flag;
      test_case "no system prompt" `Quick test_build_body_no_system_prompt;
    ];
    "parse_response", [
      test_case "complete response" `Quick test_parse_response_complete;
      test_case "tool_use response" `Quick test_parse_response_tool_use;
      test_case "unknown stop_reason" `Quick test_parse_response_unknown_stop;
      test_case "strip fenced json" `Quick test_parse_openai_response_strips_fenced_json;
      test_case "cache tokens in usage" `Quick test_parse_response_with_cache_tokens;
      test_case "reasoning_content" `Quick test_parse_openai_response_reasoning_content;
      test_case "reasoning_content with tools" `Quick test_parse_openai_response_reasoning_with_tools;
      test_case "blank reasoning_content" `Quick test_parse_openai_response_blank_reasoning;
      test_case "no reasoning_content" `Quick test_parse_openai_response_no_reasoning;
      test_case "ollama reasoning field" `Quick test_parse_openai_response_ollama_reasoning;
      test_case "reasoning_content preferred over reasoning" `Quick test_parse_openai_response_reasoning_content_preferred;
    ];
    "error_handling", [
      test_case "openai api error returns Error" `Quick test_openai_api_error_returns_error;
      test_case "openai api error unknown message" `Quick test_openai_api_error_unknown_message;
      test_case "openai error returns result" `Quick test_openai_error_returns_result;
    ];
    "parse_sse_event", [
      test_case "message_start" `Quick test_parse_sse_message_start;
      test_case "content_block_delta text" `Quick test_parse_sse_content_block_delta_text;
      test_case "content_block_delta thinking" `Quick test_parse_sse_content_block_delta_thinking;
      test_case "content_block_delta input_json" `Quick test_parse_sse_content_block_delta_input_json;
      test_case "content_block_start" `Quick test_parse_sse_content_block_start;
      test_case "message_delta" `Quick test_parse_sse_message_delta;
      test_case "message_stop" `Quick test_parse_sse_message_stop;
      test_case "ping" `Quick test_parse_sse_ping;
      test_case "error" `Quick test_parse_sse_error;
      test_case "unknown event type" `Quick test_parse_sse_unknown_type;
      test_case "malformed JSON" `Quick test_parse_sse_malformed_json;
    ];
    "message_to_json", [
      test_case "user message" `Quick test_message_to_json;
      test_case "assistant mixed content" `Quick test_message_to_json_assistant;
    ];
    "openai_messages", [
      test_case "text only user" `Quick test_openai_messages_text_only;
      test_case "user with image" `Quick test_openai_messages_with_image;
    ];
    "api_common_helpers", [
      test_case "text_blocks_to_string" `Quick test_text_blocks_to_string;
      test_case "string_is_blank" `Quick test_string_is_blank;
      test_case "json_of_string_or_raw valid" `Quick test_json_of_string_or_raw_valid;
      test_case "json_of_string_or_raw invalid" `Quick test_json_of_string_or_raw_invalid;
      test_case "disable_parallel_tool_use" `Quick test_build_body_disable_parallel;
    ];
  ]
