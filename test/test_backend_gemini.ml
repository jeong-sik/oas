open Alcotest
open Llm_provider

(* ── Helpers ────────────────────────────────────────── *)

let gemini_config ?(thinking=false) ?(budget=10000) ?(tools=[])
    ?(json_mode=false) ?(system="") () =
  ignore tools;
  Provider_config.make
    ~kind:Gemini
    ~model_id:"gemini-2.5-flash"
    ~base_url:"https://generativelanguage.googleapis.com/v1beta"
    ~api_key:"test-key"
    ~request_path:""
    ~max_tokens:4096
    ~temperature:0.7
    ?enable_thinking:(if thinking then Some true else None)
    ?thinking_budget:(if thinking then Some budget else None)
    ~response_format_json:json_mode
    ?system_prompt:(if system = "" then None else Some system)
    ()

let parse_body str =
  Yojson.Safe.from_string str

let member k json =
  Yojson.Safe.Util.member k json

let to_string json =
  Yojson.Safe.Util.to_string json

let to_int json =
  Yojson.Safe.Util.to_int json

let to_list json =
  Yojson.Safe.Util.to_list json

let to_bool json =
  Yojson.Safe.Util.to_bool json

(* ── build_request tests ────────────────────────────── *)

let test_basic_request () =
  let config = gemini_config () in
  let messages = [Types.user_msg "Hello"] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let contents = json |> member "contents" |> to_list in
  check int "one content" 1 (List.length contents);
  let first = List.hd contents in
  check string "role" "user" (first |> member "role" |> to_string);
  let parts = first |> member "parts" |> to_list in
  check int "one part" 1 (List.length parts);
  check string "text" "Hello" (List.hd parts |> member "text" |> to_string);
  (* generationConfig *)
  let gen = json |> member "generationConfig" in
  check int "maxOutputTokens" 4096 (gen |> member "maxOutputTokens" |> to_int);
  check (float 0.01) "temperature" 0.7
    (gen |> member "temperature" |> Yojson.Safe.Util.to_float)

let test_system_instruction () =
  let config = gemini_config ~system:"You are helpful." () in
  let messages = [Types.user_msg "Hi"] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let si = json |> member "systemInstruction" in
  check bool "has systemInstruction" true (si <> `Null);
  let parts = si |> member "parts" |> to_list in
  check bool "non-empty parts" true (List.length parts > 0);
  check string "system text" "You are helpful."
    (List.hd parts |> member "text" |> to_string)

let test_system_from_messages () =
  let config = gemini_config () in
  let messages = [
    Types.system_msg "Be concise.";
    Types.user_msg "Hello";
  ] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  (* System message should be in systemInstruction, not in contents *)
  let si = json |> member "systemInstruction" in
  check bool "has systemInstruction" true (si <> `Null);
  let contents = json |> member "contents" |> to_list in
  (* Only the user message should be in contents *)
  check int "one content (no system)" 1 (List.length contents)

let test_thinking_config () =
  let config = gemini_config ~thinking:true ~budget:8000 () in
  let messages = [Types.user_msg "Think about this."] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let gen = json |> member "generationConfig" in
  let tc = gen |> member "thinkingConfig" in
  check bool "has thinkingConfig" true (tc <> `Null);
  check int "thinkingBudget" 8000 (tc |> member "thinkingBudget" |> to_int);
  check bool "includeThoughts" true (tc |> member "includeThoughts" |> to_bool)

let test_tools () =
  let config = gemini_config () in
  let messages = [Types.user_msg "What's the weather?"] in
  let tools = [`Assoc [
    ("name", `String "get_weather");
    ("description", `String "Get weather for a city");
    ("input_schema", `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("city", `Assoc [("type", `String "string")])
      ]);
    ]);
  ]] in
  let body = Backend_gemini.build_request ~config ~messages ~tools () in
  let json = parse_body body in
  let tools_arr = json |> member "tools" |> to_list in
  check int "one tool group" 1 (List.length tools_arr);
  let func_decls =
    List.hd tools_arr |> member "functionDeclarations" |> to_list
  in
  check int "one declaration" 1 (List.length func_decls);
  check string "tool name" "get_weather"
    (List.hd func_decls |> member "name" |> to_string)

let test_tool_result () =
  let config = gemini_config () in
  let messages = [
    Types.user_msg "What's the weather?";
    { role = Assistant; content = [
        ToolUse { id = "call_123"; name = "get_weather";
                  input = `Assoc [("city", `String "Seoul")] }
      ]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "call_123"; content = "Sunny, 25C";
                     is_error = false }
      ]; name = None; tool_call_id = None };
  ] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let contents = json |> member "contents" |> to_list in
  check int "three contents" 3 (List.length contents);
  (* Third content should have functionResponse *)
  let third = List.nth contents 2 in
  let parts = third |> member "parts" |> to_list in
  let fr = List.hd parts |> member "functionResponse" in
  check bool "has functionResponse" true (fr <> `Null);
  (* Name should be resolved from tool_use_id to tool name *)
  check string "function name" "get_weather" (fr |> member "name" |> to_string)

let test_json_mode () =
  let config = gemini_config ~json_mode:true () in
  let messages = [Types.user_msg "Return JSON."] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let gen = json |> member "generationConfig" in
  check string "responseMimeType" "application/json"
    (gen |> member "responseMimeType" |> to_string)

let test_role_mapping () =
  let config = gemini_config () in
  let messages = [
    Types.user_msg "Hi";
    Types.assistant_msg "Hello";
    Types.user_msg "How are you?";
  ] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let contents = json |> member "contents" |> to_list in
  check int "three contents" 3 (List.length contents);
  check string "first role" "user"
    (List.nth contents 0 |> member "role" |> to_string);
  check string "second role" "model"
    (List.nth contents 1 |> member "role" |> to_string);
  check string "third role" "user"
    (List.nth contents 2 |> member "role" |> to_string)

(* ── parse_response tests ───────────────────────────── *)

let test_parse_text_response () =
  let json = Yojson.Safe.from_string {|{
    "candidates": [{
      "content": {
        "parts": [{"text": "Hello, world!"}],
        "role": "model"
      },
      "finishReason": "STOP"
    }],
    "usageMetadata": {
      "promptTokenCount": 10,
      "candidatesTokenCount": 5
    },
    "modelVersion": "gemini-2.5-flash"
  }|} in
  let resp = Backend_gemini.parse_response json in
  check string "model" "gemini-2.5-flash" resp.model;
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | _ -> fail "expected EndTurn");
  check int "one content block" 1 (List.length resp.content);
  (match List.hd resp.content with
   | Types.Text s -> check string "text" "Hello, world!" s
   | _ -> fail "expected Text");
  (match resp.usage with
   | Some u ->
       check int "input tokens" 10 u.input_tokens;
       check int "output tokens" 5 u.output_tokens
   | None -> fail "expected usage")

let test_parse_thinking_response () =
  let json = Yojson.Safe.from_string {|{
    "candidates": [{
      "content": {
        "parts": [
          {"thought": true, "text": "Let me think..."},
          {"text": "The answer is 42."}
        ],
        "role": "model"
      },
      "finishReason": "STOP"
    }],
    "usageMetadata": {"promptTokenCount": 5, "candidatesTokenCount": 20}
  }|} in
  let resp = Backend_gemini.parse_response json in
  check int "two content blocks" 2 (List.length resp.content);
  (match List.hd resp.content with
   | Types.Thinking { content; _ } ->
       check string "thinking" "Let me think..." content
   | _ -> fail "expected Thinking");
  (match List.nth resp.content 1 with
   | Types.Text s -> check string "text" "The answer is 42." s
   | _ -> fail "expected Text")

let test_parse_function_call () =
  let json = Yojson.Safe.from_string {|{
    "candidates": [{
      "content": {
        "parts": [{
          "functionCall": {
            "name": "get_weather",
            "args": {"city": "Tokyo"}
          }
        }],
        "role": "model"
      },
      "finishReason": "STOP"
    }],
    "usageMetadata": {"promptTokenCount": 15, "candidatesTokenCount": 8}
  }|} in
  let resp = Backend_gemini.parse_response json in
  check int "one content block" 1 (List.length resp.content);
  (match List.hd resp.content with
   | Types.ToolUse { name; input; _ } ->
       check string "name" "get_weather" name;
       check string "city arg" "Tokyo"
         (Yojson.Safe.Util.member "city" input |> Yojson.Safe.Util.to_string)
   | _ -> fail "expected ToolUse");
  (match resp.stop_reason with
   | Types.StopToolUse -> ()
   | _ -> fail "expected StopToolUse")

let test_parse_usage () =
  let json = Yojson.Safe.from_string {|{
    "candidates": [{
      "content": {"parts": [{"text": "hi"}], "role": "model"},
      "finishReason": "STOP"
    }],
    "usageMetadata": {
      "promptTokenCount": 100,
      "candidatesTokenCount": 50,
      "cachedContentTokenCount": 30
    }
  }|} in
  let resp = Backend_gemini.parse_response json in
  (match resp.usage with
   | Some u ->
       check int "input" 100 u.input_tokens;
       check int "output" 50 u.output_tokens;
       check int "cache read" 30 u.cache_read_input_tokens
   | None -> fail "expected usage")

let test_parse_stop_reasons () =
  let test_reason finish expected =
    let json = Yojson.Safe.from_string (Printf.sprintf {|{
      "candidates": [{
        "content": {"parts": [{"text": "x"}], "role": "model"},
        "finishReason": "%s"
      }]
    }|} finish) in
    let resp = Backend_gemini.parse_response json in
    check bool (Printf.sprintf "stop_reason for %s" finish)
      true (resp.stop_reason = expected)
  in
  test_reason "STOP" Types.EndTurn;
  test_reason "MAX_TOKENS" Types.MaxTokens;
  test_reason "SAFETY" (Types.Unknown "safety")

let test_parse_error () =
  let json = Yojson.Safe.from_string {|{
    "error": {
      "code": 400,
      "message": "Invalid API key",
      "status": "INVALID_ARGUMENT"
    }
  }|} in
  match Backend_gemini.parse_response json with
  | exception Backend_gemini.Gemini_api_error msg ->
      check bool "error contains message" true
        (String.length msg > 0)
  | _ -> fail "expected Gemini_api_error"

(* ── contents_of_messages tests ─────────────────────── *)

let test_contents_system_extraction () =
  let messages = [
    Types.system_msg "Be brief.";
    Types.user_msg "Hi";
  ] in
  let (contents, sys_instr) = Backend_gemini.contents_of_messages messages in
  check int "one content" 1 (List.length contents);
  check bool "has system" true (Option.is_some sys_instr)

let test_contents_role_mapping () =
  let messages = [
    Types.user_msg "A";
    Types.assistant_msg "B";
    Types.user_msg "C";
  ] in
  let (contents, sys_instr) = Backend_gemini.contents_of_messages messages in
  check int "three contents" 3 (List.length contents);
  check bool "no system" true (Option.is_none sys_instr);
  check string "first role" "user"
    (List.nth contents 0 |> member "role" |> to_string);
  check string "second role" "model"
    (List.nth contents 1 |> member "role" |> to_string)

let test_contents_multimodal () =
  let messages = [
    { Types.role = User; content = [
        Text "Describe this image:";
        Image { media_type = "image/png"; data = "base64data"; source_type = "base64" };
      ]; name = None; tool_call_id = None };
  ] in
  let (contents, _) = Backend_gemini.contents_of_messages messages in
  check int "one content" 1 (List.length contents);
  let parts = List.hd contents |> member "parts" |> to_list in
  check int "two parts" 2 (List.length parts);
  let img_part = List.nth parts 1 in
  let inline = img_part |> member "inlineData" in
  check string "mimeType" "image/png" (inline |> member "mimeType" |> to_string)

(* ── Streaming tests ────────────────────────────────── *)

let test_gemini_stream_text () =
  let data = {|{
    "candidates": [{
      "content": {
        "parts": [{"text": "Hello "}],
        "role": "model"
      }
    }]
  }|} in
  match Streaming.parse_gemini_sse_chunk data with
  | Some chunk ->
      check int "one part" 1 (List.length chunk.gem_parts);
      let state = Streaming.create_openai_stream_state () in
      let events = Streaming.gemini_chunk_to_events state chunk in
      check bool "has events" true (List.length events > 0)
  | None -> fail "expected Some chunk"

let test_gemini_stream_thinking () =
  let data = {|{
    "candidates": [{
      "content": {
        "parts": [{"thought": true, "text": "reasoning..."}],
        "role": "model"
      }
    }]
  }|} in
  match Streaming.parse_gemini_sse_chunk data with
  | Some chunk ->
      let state = Streaming.create_openai_stream_state () in
      let events = Streaming.gemini_chunk_to_events state chunk in
      let has_thinking = List.exists (function
        | Types.ContentBlockStart { content_type = "thinking"; _ } -> true
        | _ -> false
      ) events in
      check bool "has thinking block start" true has_thinking
  | None -> fail "expected Some chunk"

let test_gemini_stream_function_call () =
  let data = {|{
    "candidates": [{
      "content": {
        "parts": [{
          "functionCall": {"name": "search", "args": {"q": "test"}}
        }],
        "role": "model"
      }
    }]
  }|} in
  match Streaming.parse_gemini_sse_chunk data with
  | Some chunk ->
      let state = Streaming.create_openai_stream_state () in
      let events = Streaming.gemini_chunk_to_events state chunk in
      let has_tool = List.exists (function
        | Types.ContentBlockStart { content_type = "tool_use"; _ } -> true
        | _ -> false
      ) events in
      check bool "has tool_use block" true has_tool
  | None -> fail "expected Some chunk"

let test_gemini_stream_finish () =
  let data = {|{
    "candidates": [{
      "content": {"parts": [{"text": "done"}], "role": "model"},
      "finishReason": "STOP"
    }],
    "usageMetadata": {"promptTokenCount": 10, "candidatesTokenCount": 3}
  }|} in
  match Streaming.parse_gemini_sse_chunk data with
  | Some chunk ->
      let state = Streaming.create_openai_stream_state () in
      let events = Streaming.gemini_chunk_to_events state chunk in
      let has_delta = List.exists (function
        | Types.MessageDelta { stop_reason = Some Types.EndTurn; _ } -> true
        | _ -> false
      ) events in
      check bool "has EndTurn delta" true has_delta
  | None -> fail "expected Some chunk"

(* ── Cascade config tests ──────────────────────────── *)

let test_cascade_gemini_kind () =
  (* Gemini parse should produce kind=Gemini if GEMINI_API_KEY is set *)
  match Sys.getenv_opt "GEMINI_API_KEY" with
  | Some _ ->
      (match Cascade_config.parse_model_string "gemini:gemini-2.5-flash" with
       | Some cfg ->
           check bool "kind is Gemini" true
             (cfg.kind = Provider_config.Gemini)
       | None -> fail "expected Some for gemini")
  | None ->
      (* No key, should return None *)
      let result = Cascade_config.parse_model_string "gemini:gemini-2.5-flash" in
      check bool "gemini without key" true (Option.is_none result)

let test_gemini_capabilities_named () =
  let caps = Capabilities.gemini_capabilities in
  check bool "tools" true caps.supports_tools;
  check bool "thinking" true caps.supports_extended_thinking;
  check bool "audio" true caps.supports_audio_input;
  check bool "video" true caps.supports_video_input;
  check bool "code_execution" true caps.supports_code_execution;
  check bool "caching" true caps.supports_caching;
  (match caps.max_context_tokens with
   | Some n -> check int "1M context" 1_000_000 n
   | None -> fail "expected max_context_tokens")

let test_tool_choice_mapping () =
  let test_choice choice expected_mode =
    let config = { (gemini_config ()) with tool_choice = Some choice } in
    let tools = [`Assoc [
      ("name", `String "test_tool");
      ("description", `String "A test tool");
    ]] in
    let messages = [Types.user_msg "test"] in
    let body = Backend_gemini.build_request ~config ~messages ~tools () in
    let json = parse_body body in
    let tc = json |> member "toolConfig" in
    let fcc = tc |> member "functionCallingConfig" in
    let mode = fcc |> member "mode" |> to_string in
    check string (Printf.sprintf "tool_choice mode for %s" expected_mode)
      expected_mode mode
  in
  test_choice Types.Auto "AUTO";
  test_choice Types.Any "ANY";
  test_choice Types.None_ "NONE"

let test_thinking_part_roundtrip () =
  (* Test that Thinking content blocks become thought:true parts *)
  let config = gemini_config () in
  let messages = [
    { Types.role = Assistant; content = [
        Thinking { thinking_type = "thinking"; content = "Let me consider..." };
        Text "The answer is 42.";
      ]; name = None; tool_call_id = None };
    Types.user_msg "Thanks";
  ] in
  let body = Backend_gemini.build_request ~config ~messages () in
  let json = parse_body body in
  let contents = json |> member "contents" |> to_list in
  let first = List.hd contents in
  let parts = first |> member "parts" |> to_list in
  check int "two parts" 2 (List.length parts);
  let thought_part = List.hd parts in
  check bool "thought flag" true (thought_part |> member "thought" |> to_bool);
  check string "thought text" "Let me consider..."
    (thought_part |> member "text" |> to_string)

(* ── Suite ────────────────────────────────────────── *)

let () =
  run "backend_gemini" [
    "build_request", [
      test_case "basic" `Quick test_basic_request;
      test_case "system instruction from config" `Quick test_system_instruction;
      test_case "system from messages" `Quick test_system_from_messages;
      test_case "thinking config" `Quick test_thinking_config;
      test_case "tools" `Quick test_tools;
      test_case "tool result" `Quick test_tool_result;
      test_case "json mode" `Quick test_json_mode;
      test_case "role mapping" `Quick test_role_mapping;
      test_case "tool choice" `Quick test_tool_choice_mapping;
      test_case "thinking part roundtrip" `Quick test_thinking_part_roundtrip;
    ];
    "parse_response", [
      test_case "text" `Quick test_parse_text_response;
      test_case "thinking parts" `Quick test_parse_thinking_response;
      test_case "function call" `Quick test_parse_function_call;
      test_case "usage" `Quick test_parse_usage;
      test_case "stop reasons" `Quick test_parse_stop_reasons;
      test_case "error" `Quick test_parse_error;
    ];
    "contents_of_messages", [
      test_case "system extraction" `Quick test_contents_system_extraction;
      test_case "role mapping" `Quick test_contents_role_mapping;
      test_case "multimodal" `Quick test_contents_multimodal;
    ];
    "streaming", [
      test_case "text chunk" `Quick test_gemini_stream_text;
      test_case "thinking chunk" `Quick test_gemini_stream_thinking;
      test_case "function call chunk" `Quick test_gemini_stream_function_call;
      test_case "finish reason" `Quick test_gemini_stream_finish;
    ];
    "cascade_config", [
      test_case "gemini kind" `Quick test_cascade_gemini_kind;
      test_case "capabilities named" `Quick test_gemini_capabilities_named;
    ];
  ]
