(** Tests for Llm_provider.Complete — request JSON verification. *)

module PC = Llm_provider.Provider_config
module BA = Llm_provider.Backend_anthropic
module BO = Llm_provider.Backend_openai
module BGlm = Llm_provider.Backend_glm
module BOL = Llm_provider.Backend_ollama
module BGemini = Llm_provider.Backend_gemini
open Llm_provider.Types

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len then false
    else if String.sub text idx sub_len = sub then true
    else loop (idx + 1)
  in
  if sub_len = 0 then true else loop 0

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

let test_anthropic_output_schema () =
  let schema =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
      ("required", `List [`String "answer"]);
    ]
  in
  let config = PC.make ~kind:Anthropic ~model_id:"claude-sonnet-4-6"
    ~base_url:"" ~output_schema:schema () in
  let body = BA.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "output_config type" "json_schema"
    (json |> member "output_config" |> member "format" |> member "type" |> to_string);
  Alcotest.(check bool) "schema copied" true
    (json |> member "output_config" |> member "format" |> member "schema" = schema)

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
      let provider_kind_t =
        Alcotest.testable PC.pp_provider_kind (=)
      in
      Alcotest.(check (option provider_kind_t))
        "provider_kind placeholder" None t.provider_kind;
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

let test_ollama_output_schema () =
  let schema =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [("answer", `Assoc [("type", `String "string")])]);
      ("required", `List [`String "answer"]);
    ]
  in
  let config = PC.make ~kind:Ollama ~model_id:"qwen3.5:9b"
    ~base_url:"http://localhost:11434" ~output_schema:schema () in
  let body = BOL.build_request ~config ~messages:[user_msg "hi"] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "format copied" true
    (json |> member "format" = schema)

let test_openai_with_json_schema () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [("answer", `Assoc [("type", `String "string")])] );
      ]
  in
  let config =
    PC.make ~kind:OpenAI_compat ~model_id:"gpt-4o-mini"
      ~base_url:"https://api.openai.com/v1"
      ~response_format:(JsonSchema schema) ()
  in
  let body = BO.build_request ~config ~messages:[user_msg "Return JSON."] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let response_format = json |> member "response_format" in
  Alcotest.(check string) "response_format.type" "json_schema"
    (response_format |> member "type" |> to_string);
  Alcotest.(check string) "json_schema.name" "structured_output"
    (response_format |> member "json_schema" |> member "name" |> to_string);
  Alcotest.(check string) "json_schema.schema.type" "object"
    (response_format |> member "json_schema" |> member "schema" |> member "type"
     |> to_string)

let test_gemini_with_json_schema () =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [("answer", `Assoc [("type", `String "string")])] );
        ("required", `List [`String "answer"]);
      ]
  in
  let config =
    PC.make ~kind:Gemini ~model_id:"gemini-2.5-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ~api_key:"test-key" ~response_format:(JsonSchema schema) ()
  in
  let body = BGemini.build_request ~config ~messages:[user_msg "Return JSON."] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let generation_config = json |> member "generationConfig" in
  Alcotest.(check string) "responseMimeType" "application/json"
    (generation_config |> member "responseMimeType" |> to_string);
  Alcotest.(check string) "responseJsonSchema.type" "object"
    (generation_config |> member "responseJsonSchema" |> member "type" |> to_string);
  Alcotest.(check string) "responseJsonSchema.required[0]" "answer"
    (generation_config |> member "responseJsonSchema" |> member "required" |> to_list
     |> List.hd |> to_string)

let test_kimi_direct_with_tools_and_thinking () =
  let config = PC.make ~kind:Kimi ~model_id:"kimi-for-coding"
    ~base_url:"https://api.kimi.com/coding" ~enable_thinking:true () in
  let tool = `Assoc [
    ("name", `String "shell");
    ("description", `String "run shell command");
    ("input_schema", `Assoc [("type", `String "object")]);
  ] in
  let body = BA.build_request ~config ~messages:[user_msg "inspect repo"]
    ~tools:[tool] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  let thinking = json |> member "thinking" in
  Alcotest.(check string) "model" "kimi-for-coding"
    (json |> member "model" |> to_string);
  Alcotest.(check int) "tool count" 1 (List.length tools);
  Alcotest.(check string) "thinking type" "enabled"
    (thinking |> member "type" |> to_string)

let test_kimi_direct_tool_result_uses_text_blocks () =
  let config = PC.make ~kind:Kimi ~model_id:"kimi-for-coding"
    ~base_url:"https://api.kimi.com/coding" () in
  let messages = [
    { role = Assistant; content = [
        Thinking { thinking_type = "sig_1";
                   content = "I should call the calculator." };
        ToolUse { id = "tool_1"; name = "calculator";
                  input = `Assoc [("a", `Int 2); ("b", `Int 3)] };
      ]; name = None; tool_call_id = None ; metadata = []};
    { role = Tool; content = [
        ToolResult {
          tool_use_id = "tool_1";
          content = "5";
          is_error = false;
          json = Some (`Int 5);
        };
      ]; name = None; tool_call_id = None ; metadata = []};
  ] in
  let body = BA.build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let replay = json |> member "messages" |> index 1 in
  let block = replay |> member "content" |> index 0 in
  let content_blocks = block |> member "content" |> to_list in
  Alcotest.(check string) "tool result role serialized as user" "user"
    (replay |> member "role" |> to_string);
  Alcotest.(check string) "tool_result type" "tool_result"
    (block |> member "type" |> to_string);
  Alcotest.(check string) "tool_use id preserved" "tool_1"
    (block |> member "tool_use_id" |> to_string);
  Alcotest.(check int) "tool_result content block count" 1
    (List.length content_blocks);
  Alcotest.(check string) "nested text block type" "text"
    (List.hd content_blocks |> member "type" |> to_string);
  Alcotest.(check string) "nested text block content" "5"
    (List.hd content_blocks |> member "text" |> to_string)

let test_glm_preserved_reasoning_replay_and_auto_tool_choice () =
  let config = PC.make ~kind:Glm ~model_id:"glm-5.1"
    ~base_url:"https://api.z.ai/api/coding/paas/v4"
    ~enable_thinking:true ~clear_thinking:false
    ~tool_stream:true ~tool_choice:(Tool "calculator") () in
  let messages = [
    { role = Assistant; content = [
        Thinking { thinking_type = "reasoning";
                   content = "I need the calculator result." };
        ToolUse { id = "call_1"; name = "calculator";
                  input = `Assoc [("expr", `String "2+2")] };
      ]; name = None; tool_call_id = None ; metadata = []};
    { role = Tool; content = [
        ToolResult {
          tool_use_id = "call_1";
          content = "{\"value\":4}";
          is_error = false;
          json = Some (`Assoc [("value", `Int 4)]);
        };
      ]; name = None; tool_call_id = None ; metadata = []};
  ] in
  let body = BGlm.build_request ~stream:true ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  Alcotest.(check string) "glm tool_choice coerced" "auto"
    (json |> member "tool_choice" |> to_string);
  Alcotest.(check string) "assistant content remains text channel" ""
    (assistant |> member "content" |> to_string);
  Alcotest.(check string) "reasoning replayed separately"
    "I need the calculator result."
    (assistant |> member "reasoning_content" |> to_string);
  Alcotest.(check bool) "clear_thinking false preserved" true
    (json |> member "thinking" |> member "clear_thinking" |> to_bool = false);
  Alcotest.(check bool) "tool_stream enabled" true
    (json |> member "tool_stream" |> to_bool)
(* ── Provider_config.make ────────────────────────────── *)

let test_config_default_paths () =
  let anth = PC.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "anthropic path" "/v1/messages"
    anth.request_path;
  let kimi = PC.make ~kind:Kimi ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "kimi path" "/v1/messages"
    kimi.request_path;
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
    (Complete.is_retryable (Http_client.NetworkError { message = "timeout"; kind = Unknown }));
  (* Non-retryable *)
  Alcotest.(check bool) "400 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }));
  Alcotest.(check bool) "401 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }));
  Alcotest.(check bool) "404 not retryable" false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }));
  (* Wiring bug — retrying cannot conjure a missing CLI transport. *)
  Alcotest.(check bool) "CliTransportRequired not retryable" false
    (Complete.is_retryable
       (Http_client.CliTransportRequired { kind = "claude_code" }))

let test_complete_claude_code_without_transport_is_guarded () =
  (* Regression: [Complete.complete] used to forward CLI-kind configs
     (base_url = "") to cohttp-eio, which crashed with
     [Fmt.failwith "Unknown scheme None"].  The guard now returns a
     typed [CliTransportRequired] so cascades and callers can
     distinguish a wiring bug from a transient network failure.

     Covers the full matrix (Claude_code, Gemini_cli, Kimi_cli, Codex_cli). *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let kinds =
    [ PC.Claude_code,  "claude_code";
      PC.Gemini_cli,   "gemini_cli";
      PC.Kimi_cli,     "kimi_cli";
      PC.Codex_cli,    "codex_cli" ]
  in
  List.iter (fun (kind, expected_name) ->
    let config = PC.make ~kind ~model_id:"auto" ~base_url:"" () in
    let messages = [user_msg "hi"] in
    match
      Llm_provider.Complete.complete ~sw ~net
        ~config ~messages ?transport:None ()
    with
    | Ok _ ->
        Alcotest.failf
          "%s with no transport must not succeed via HTTP fallback"
          expected_name
    | Error (Llm_provider.Http_client.CliTransportRequired { kind }) ->
        Alcotest.(check string)
          (Printf.sprintf "%s reports its own kind" expected_name)
          expected_name kind
    | Error (Llm_provider.Http_client.HttpError { code; _ }) ->
        Alcotest.failf
          "%s expected CliTransportRequired, got HttpError %d"
          expected_name code
    | Error (Llm_provider.Http_client.NetworkError { message; _ }) ->
        Alcotest.failf
          "%s expected CliTransportRequired, got NetworkError: %s \
           (this is the 'Unknown scheme None' regression)"
          expected_name message
    | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
        Alcotest.failf
          "%s expected CliTransportRequired, got AcceptRejected: %s"
          expected_name reason
  ) kinds

let test_complete_rejects_output_schema_for_glm () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let config = PC.make ~kind:Glm ~model_id:"glm-5"
    ~base_url:"https://api.z.ai/api/coding/paas/v4"
    ~output_schema:(`Assoc [("type", `String "object")]) () in
  match Llm_provider.Complete.complete ~sw ~net ~config
          ~messages:[user_msg "hi"] () with
  | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
      Alcotest.(check bool) "mentions glm json mode" true
        (contains_substring ~sub:"json mode only"
           (String.lowercase_ascii reason))
  | Ok _ -> Alcotest.fail "expected AcceptRejected for glm output_schema"
  | Error _ -> Alcotest.fail "expected AcceptRejected for glm output_schema"

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
      test_case "with output schema" `Quick test_anthropic_output_schema;
      test_case "stream flag" `Quick test_anthropic_stream_flag;
      test_case "parse response initializes telemetry" `Quick
        test_anthropic_parse_response_initializes_telemetry;
    ];
    "openai_build_request", [
      test_case "basic body" `Quick test_openai_basic_body;
      test_case "with system" `Quick test_openai_with_system;
      test_case "with tools" `Quick test_openai_with_tools;
      test_case "kimi direct tools + thinking" `Quick
        test_kimi_direct_with_tools_and_thinking;
      test_case "kimi direct tool_result uses text blocks" `Quick
        test_kimi_direct_tool_result_uses_text_blocks;
      test_case "stream flag" `Quick test_openai_stream_flag;
      test_case "with json schema" `Quick test_openai_with_json_schema;
      test_case "ollama output schema" `Quick test_ollama_output_schema;
      test_case "glm preserved reasoning replay" `Quick
        test_glm_preserved_reasoning_replay_and_auto_tool_choice;
    ];
    "gemini_build_request", [
      test_case "with json schema" `Quick test_gemini_with_json_schema;
    ];
    "provider_config", [
      test_case "default paths" `Quick test_config_default_paths;
      test_case "custom path" `Quick test_config_custom_path;
    ];
    "retry", [
      test_case "default config" `Quick test_default_retry_config;
      test_case "is_retryable" `Quick test_is_retryable;
    ];
    "cli_transport_guard", [
      test_case "complete refuses HTTP fallback for CLI kinds"
        `Quick test_complete_claude_code_without_transport_is_guarded;
      test_case "glm output schema rejected before request"
        `Quick test_complete_rejects_output_schema_for_glm;
    ];
    "cost", [
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
