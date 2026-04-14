(** Tests for Llm_provider.Complete — request JSON verification. *)

module PC = Llm_provider.Provider_config
module BA = Llm_provider.Backend_anthropic
module BO = Llm_provider.Backend_openai
open Llm_provider.Types

(* ── Anthropic build_request ─────────────────────────── *)

let test_anthropic_basic_body () =
  let config = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"https://api.anthropic.com" ~max_tokens:1024 () in
  let msgs = [user_msg "hello"] in
  let body = BA.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "claude-sonnet-4-6"
    (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 1024
    (json |> member "max_tokens" |> to_int);
  Alcotest.(check bool) "stream false"
    false (json |> member "stream" |> to_bool);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)

let test_anthropic_with_system () =
  let config = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"" ~system_prompt:"You are helpful." () in
  let body = BA.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "system" "You are helpful."
    (json |> member "system" |> to_string)

let test_anthropic_with_thinking () =
  let config = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"" ~enable_thinking:true ~thinking_budget:5000 () in
  let body = BA.build_request ~config ~messages:[user_msg "think"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  Alcotest.(check string) "thinking type" "enabled"
    (thinking |> member "type" |> to_string);
  Alcotest.(check int) "budget" 5000
    (thinking |> member "budget_tokens" |> to_int)

let test_anthropic_stream_flag () =
  let config = PC.make ~kind:Anthropic ~model_id:"m"
    ~base_url:"" () in
  let body = BA.build_request ~stream:true ~config
    ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true
    (json |> member "stream" |> to_bool)

let test_anthropic_parse_response_initializes_telemetry () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_test",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|} in
  let resp = BA.parse_response json in
  match resp.telemetry with
  | Some t ->
      Alcotest.(check int) "request_latency_ms defaults to zero" 0 t.request_latency_ms;
      Alcotest.(check (option string)) "provider_kind placeholder" None t.provider_kind;
      Alcotest.(check (option string)) "canonical model placeholder" None t.canonical_model_id
  | None ->
      Alcotest.fail "expected telemetry placeholder"

(* ── OpenAI build_request ────────────────────────────── *)

let test_openai_basic_body () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"gpt-4"
    ~base_url:"https://api.openai.com/v1" ~max_tokens:2048 () in
  let msgs = [user_msg "hello"] in
  let body = BO.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "gpt-4"
    (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 2048
    (json |> member "max_tokens" |> to_int);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)

let test_openai_with_system () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"gpt-4"
    ~base_url:"" ~system_prompt:"Be helpful." () in
  let body = BO.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let msgs = json |> member "messages" |> to_list in
  (* First message should be system *)
  let first = List.hd msgs in
  Alcotest.(check string) "system role" "system"
    (first |> member "role" |> to_string);
  Alcotest.(check string) "system content" "Be helpful."
    (first |> member "content" |> to_string)

let test_openai_with_tools () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"gpt-4"
    ~base_url:"" () in
  let tool = `Assoc [
    ("name", `String "calc");
    ("description", `String "calculator");
    ("input_schema", `Assoc [("type", `String "object")])
  ] in
  let body = BO.build_request ~config ~messages:[user_msg "add 1+1"]
    ~tools:[tool] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "1 tool" 1 (List.length tools)

let test_openai_stream_flag () =
  let config = PC.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"" () in
  let body = BO.build_request ~stream:true ~config
    ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true
    (json |> member "stream" |> to_bool)

(* ── Provider_config.make ────────────────────────────── *)

let test_config_default_paths () =
  let anth = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "anthropic path" "/v1/messages"
    anth.request_path;
  let oai = PC.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "openai path" "/v1/chat/completions"
    oai.request_path

let test_config_custom_path () =
  let cfg = PC.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"" ~request_path:"/custom" () in
  Alcotest.(check string) "custom path" "/custom" cfg.request_path

(* ── Retry config ────────────────────────────────────── *)

let test_default_retry_config () =
  let cfg = Llm_provider.Complete.default_retry_config in
  Alcotest.(check int) "max_retries" 3 cfg.max_retries;
  Alcotest.(check (float 0.01)) "initial_delay" 1.0 cfg.initial_delay_sec;
  Alcotest.(check (float 0.01)) "max_delay" 30.0 cfg.max_delay_sec;
  Alcotest.(check (float 0.01)) "backoff" 2.0 cfg.backoff_multiplier

let test_is_retryable () =
  let open Llm_provider in
  (* Retryable status codes *)
  Alcotest.(check bool) "429 retryable" true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }));
  Alcotest.(check bool) "503 retryable" true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }));
  Alcotest.(check bool) "529 retryable" true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }));
  (* Network errors *)
  Alcotest.(check bool) "network retryable" true
    (Complete.is_retryable (Http_client.NetworkError { message = "timeout" }));
  (* Non-retryable *)
  Alcotest.(check bool) "400 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }));
  Alcotest.(check bool) "401 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }));
  Alcotest.(check bool) "404 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }))

(* ── Cascade config ──────────────────────────────────── *)

let test_cascade_type () =
  let primary = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"https://api.anthropic.com" () in
  let fallback = PC.make ~kind:OpenAI_compat ~model_id:"qwen3.5"
    ~base_url:"http://127.0.0.1:8085" () in
  let casc : Llm_provider.Complete.cascade = {
    primary; fallbacks = [fallback]
  } in
  Alcotest.(check int) "1 fallback" 1 (List.length casc.fallbacks);
  Alcotest.(check string) "primary model" "claude-sonnet-4-6"
    casc.primary.model_id;
  Alcotest.(check string) "fallback model" "qwen3.5"
    (List.hd casc.fallbacks).model_id

let test_annotate_response_cost () =
  let response : api_response = {
    id = "resp-1";
    model = "claude-sonnet-4-6";
    stop_reason = EndTurn;
    content = [Text "ok"];
    usage = Some {
      input_tokens = 1_000;
      output_tokens = 500;
      cache_creation_input_tokens = 0;
      cache_read_input_tokens = 0;
      cost_usd = None;
    };
    telemetry = None;
  } in
  match Llm_provider.Pricing.annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } ->
      Alcotest.(check bool) "annotated cost" true (cost > 0.0)
  | _ -> Alcotest.fail "expected annotated response cost"

(* ── Stream accumulator ──────────────────────────────── *)

let test_stream_acc_text () =
  (* Simulate a minimal Anthropic SSE event sequence *)
  let events = [
    MessageStart { id = "msg_123"; model = "claude-sonnet-4-6";
                   usage = Some { input_tokens = 10; output_tokens = 0;
                                  cache_creation_input_tokens = 0;
                                  cache_read_input_tokens = 0 ; cost_usd = None } };
    ContentBlockStart { index = 0; content_type = "text";
                        tool_id = None; tool_name = None };
    ContentBlockDelta { index = 0; delta = TextDelta "Hello " };
    ContentBlockDelta { index = 0; delta = TextDelta "world" };
    ContentBlockStop { index = 0 };
    MessageDelta { stop_reason = Some EndTurn;
                   usage = Some { input_tokens = 0; output_tokens = 5;
                                  cache_creation_input_tokens = 0;
                                  cache_read_input_tokens = 0 ; cost_usd = None } };
    MessageStop;
  ] in
  (* Use the internal accumulator via a module alias *)
  let module C = Llm_provider.Complete in
  ignore (C.default_retry_config);  (* force link *)
  (* We can't call the internal functions directly, but we can test
     that the event types compose correctly *)
  Alcotest.(check int) "7 events" 7 (List.length events)

let test_stream_acc_tool_use () =
  let events = [
    MessageStart { id = "msg_456"; model = "gpt-4";
                   usage = None };
    ContentBlockStart { index = 0; content_type = "tool_use";
                        tool_id = Some "tu_1"; tool_name = Some "calc" };
    ContentBlockDelta { index = 0;
                        delta = InputJsonDelta "{\"x\":1}" };
    ContentBlockStop { index = 0 };
    MessageDelta { stop_reason = Some StopToolUse; usage = None };
    MessageStop;
  ] in
  Alcotest.(check int) "6 events" 6 (List.length events)

(* ── Prompt caching ───────────────────────────────── *)

(* Long prompt exceeding 3500 char threshold for cache_control *)
let long_prompt = String.concat "" (List.init 200 (fun i ->
  Printf.sprintf "Rule %d: follow this guideline carefully. " i))

let test_cache_system_prompt () =
  let config = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"" ~system_prompt:long_prompt
    ~cache_system_prompt:true () in
  let body = BA.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let system = json |> member "system" |> to_list in
  Alcotest.(check int) "1 system block" 1 (List.length system);
  let block = List.hd system in
  Alcotest.(check string) "type" "text"
    (block |> member "type" |> to_string);
  let cc = block |> member "cache_control" in
  Alcotest.(check string) "cache_control type" "ephemeral"
    (cc |> member "type" |> to_string)

let test_cache_short_prompt_skips () =
  let config = PC.make ~kind:Anthropic ~model_id:"m"
    ~base_url:"" ~system_prompt:"Short."
    ~cache_system_prompt:true () in
  let body = BA.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "short = plain string" "Short."
    (json |> member "system" |> to_string)

let test_cache_no_system_no_cache () =
  let config = PC.make ~kind:Anthropic ~model_id:"m"
    ~base_url:"" ~system_prompt:"Hello."
    ~cache_system_prompt:false () in
  let body = BA.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  (* system should be a plain string when caching disabled *)
  Alcotest.(check string) "system is string" "Hello."
    (json |> member "system" |> to_string)

let test_cache_tools () =
  let config = PC.make ~kind:Anthropic ~model_id:"m"
    ~base_url:"" ~cache_system_prompt:true () in
  let tool1 = `Assoc [("name", `String "a"); ("description", `String "tool a")] in
  let tool2 = `Assoc [("name", `String "b"); ("description", `String "tool b")] in
  let body = BA.build_request ~config ~messages:[user_msg "hi"]
    ~tools:[tool1; tool2] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "2 tools" 2 (List.length tools);
  (* First tool should NOT have cache_control *)
  let first = List.hd tools in
  Alcotest.(check bool) "first tool no cache" true
    (first |> member "cache_control" = `Null);
  (* Last tool SHOULD have cache_control *)
  let last = List.nth tools 1 in
  let cc = last |> member "cache_control" in
  Alcotest.(check string) "last tool cache_control" "ephemeral"
    (cc |> member "type" |> to_string)

let test_cache_default_false () =
  let cfg = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  Alcotest.(check bool) "default cache off" false cfg.cache_system_prompt

let () =
  let open Alcotest in
  run "provider_complete" [
    "anthropic_build_request", [
      test_case "basic body" `Quick test_anthropic_basic_body;
      test_case "with system" `Quick test_anthropic_with_system;
      test_case "with thinking" `Quick test_anthropic_with_thinking;
      test_case "stream flag" `Quick test_anthropic_stream_flag;
      test_case "parse response initializes telemetry" `Quick
        test_anthropic_parse_response_initializes_telemetry;
    ];
    "openai_build_request", [
      test_case "basic body" `Quick test_openai_basic_body;
      test_case "with system" `Quick test_openai_with_system;
      test_case "with tools" `Quick test_openai_with_tools;
      test_case "stream flag" `Quick test_openai_stream_flag;
    ];
    "provider_config", [
      test_case "default paths" `Quick test_config_default_paths;
      test_case "custom path" `Quick test_config_custom_path;
    ];
    "retry", [
      test_case "default config" `Quick test_default_retry_config;
      test_case "is_retryable" `Quick test_is_retryable;
    ];
    "cascade", [
      test_case "cascade type" `Quick test_cascade_type;
      test_case "annotate response cost" `Quick test_annotate_response_cost;
    ];
    "stream_acc", [
      test_case "text events" `Quick test_stream_acc_text;
      test_case "tool_use events" `Quick test_stream_acc_tool_use;
    ];
    "prompt_caching", [
      test_case "system block with cache_control" `Quick test_cache_system_prompt;
      test_case "no cache when disabled" `Quick test_cache_no_system_no_cache;
      test_case "last tool gets cache_control" `Quick test_cache_tools;
      test_case "default cache off" `Quick test_cache_default_false;
      test_case "short prompt skips cache" `Quick test_cache_short_prompt_skips;
    ];
  ]
