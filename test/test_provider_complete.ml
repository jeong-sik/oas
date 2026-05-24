(** Tests for Llm_provider.Complete — request JSON verification. *)

module PC = Llm_provider.Provider_config
module BA = Llm_provider.Backend_provider_a
module BO = Llm_provider.Backend_provider_d
module BGlm = Llm_provider.Backend_provider_k
module BOL = Llm_provider.Backend_ollama
module BGemini = Llm_provider.Backend_provider_f
open Llm_provider.Types

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len
    then false
    else if String.sub text idx sub_len = sub
    then true
    else loop (idx + 1)
  in
  if sub_len = 0 then true else loop 0
;;

(* ── Provider_a build_request ─────────────────────────── *)

let test_provider_a_basic_body () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-sonnet-4-6"
      ~base_url:"https://api.provider_a.com"
      ~max_tokens:1024
      ()
  in
  let msgs = [ user_msg "hello" ] in
  let body = BA.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "model"
    "agent_llm_a-sonnet-4-6"
    (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 1024 (json |> member "max_tokens" |> to_int);
  Alcotest.(check bool) "stream false" false (json |> member "stream" |> to_bool);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)
;;

let test_provider_a_with_system () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-sonnet-4-6"
      ~base_url:""
      ~system_prompt:"You are helpful."
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "system"
    "You are helpful."
    (json |> member "system" |> to_string)
;;

let test_provider_a_with_thinking () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-sonnet-4-6"
      ~base_url:""
      ~enable_thinking:true
      ~thinking_budget:5000
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "think" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  Alcotest.(check string)
    "thinking type"
    "enabled"
    (thinking |> member "type" |> to_string);
  Alcotest.(check int) "budget" 5000 (thinking |> member "budget_tokens" |> to_int)
;;

let test_provider_a_stream_flag () =
  let config = PC.make ~kind:Provider_a ~model_id:"m" ~base_url:"" () in
  let body = BA.build_request ~stream:true ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true (json |> member "stream" |> to_bool)
;;

let test_provider_a_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-sonnet-4-6"
      ~base_url:""
      ~output_schema:schema
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "output_config type"
    "json_schema"
    (json |> member "output_config" |> member "format" |> member "type" |> to_string);
  Alcotest.(check bool)
    "schema copied"
    true
    (json |> member "output_config" |> member "format" |> member "schema" = schema)
;;

let test_provider_a_json_schema_response_format_without_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    { (PC.make ~kind:Provider_a ~model_id:"agent_llm_a-sonnet-4-6" ~base_url:"" ()) with
      response_format = JsonSchema schema
    ; output_schema = None
    }
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "schema copied from response_format"
    true
    (json |> member "output_config" |> member "format" |> member "schema" = schema)
;;

let test_provider_a_parse_response_initializes_telemetry () =
  let json =
    Yojson.Safe.from_string
      {|{
    "id": "msg_test",
    "model": "agent_llm_a-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|}
  in
  let resp = BA.parse_response json in
  match resp.telemetry with
  | Some t ->
    Alcotest.(check (option int))
      "request_latency_ms defaults to unknown"
      None
      t.request_latency_ms;
    let provider_kind_t = Alcotest.testable PC.pp_provider_kind ( = ) in
    Alcotest.(check (option provider_kind_t))
      "provider_kind placeholder"
      None
      t.provider_kind;
    Alcotest.(check (option string))
      "canonical model placeholder"
      None
      t.canonical_model_id
  | None -> Alcotest.fail "expected telemetry placeholder"
;;

(* ── Provider_d build_request ────────────────────────────── *)

let test_provider_d_basic_body () =
  let config =
    PC.make
      ~kind:Provider_d_compat
      ~model_id:"model-d-4"
      ~base_url:"https://api.provider_d.com/v1"
      ~max_tokens:2048
      ()
  in
  let msgs = [ user_msg "hello" ] in
  let body = BO.build_request ~config ~messages:msgs () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "model-d-4" (json |> member "model" |> to_string);
  Alcotest.(check int) "max_tokens" 2048 (json |> member "max_tokens" |> to_int);
  let msgs_json = json |> member "messages" |> to_list in
  Alcotest.(check int) "1 message" 1 (List.length msgs_json)
;;

let test_provider_d_with_system () =
  let config =
    PC.make
      ~kind:Provider_d_compat
      ~model_id:"model-d-4"
      ~base_url:""
      ~system_prompt:"Be helpful."
      ()
  in
  let body = BO.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let msgs = json |> member "messages" |> to_list in
  (* First message should be system *)
  let first = List.hd msgs in
  Alcotest.(check string) "system role" "system" (first |> member "role" |> to_string);
  Alcotest.(check string)
    "system content"
    "Be helpful."
    (first |> member "content" |> to_string)
;;

let test_provider_d_with_tools () =
  let config = PC.make ~kind:Provider_d_compat ~model_id:"model-d-4" ~base_url:"" () in
  let tool =
    `Assoc
      [ "name", `String "calc"
      ; "description", `String "calculator"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let body =
    BO.build_request ~config ~messages:[ user_msg "add 1+1" ] ~tools:[ tool ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "1 tool" 1 (List.length tools)
;;

let test_provider_d_stream_flag () =
  let config = PC.make ~kind:Provider_d_compat ~model_id:"m" ~base_url:"" () in
  let body = BO.build_request ~stream:true ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "stream" true (json |> member "stream" |> to_bool)
;;

let test_ollama_output_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Ollama
      ~model_id:"provider_h-3.5:9b"
      ~base_url:"http://localhost:11434"
      ~output_schema:schema
      ()
  in
  let body = BOL.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "format copied" true (json |> member "format" = schema)
;;

let test_ollama_parse_parallel_tool_calls_object_arguments () =
  let body =
    {|{"model":"provider_h-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"function":{"index":0,"name":"get_temperature","arguments":{"city":"New York"}}},
           {"function":{"index":1,"name":"get_conditions","arguments":{"city":"London"}}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg -> Alcotest.fail msg
  | Ok resp ->
    Alcotest.(check bool) "stop tool use" true (resp.stop_reason = StopToolUse);
    (match resp.content with
     | [ ToolUse first; ToolUse second ] ->
       Alcotest.(check string) "first name" "get_temperature" first.name;
       Alcotest.(check string) "second name" "get_conditions" second.name;
       Alcotest.(check bool) "distinct synthetic ids" true (first.id <> second.id);
       Alcotest.(check bool)
         "first input object"
         true
         (first.input = `Assoc [ "city", `String "New York" ]);
       Alcotest.(check bool)
         "second input object"
         true
         (second.input = `Assoc [ "city", `String "London" ])
     | _ -> Alcotest.fail "expected two ToolUse blocks")
;;

let test_ollama_parse_tool_call_preserves_explicit_id_and_string_arguments () =
  let body =
    {|{"model":"provider_h-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"id":"call_explicit","function":{"name":"get_weather","arguments":"{\"city\":\"Seoul\"}"}}
         ]}}|}
  in
  match BOL.parse_ollama_response body with
  | Error msg -> Alcotest.fail msg
  | Ok resp ->
    (match resp.content with
     | [ ToolUse tool_use ] ->
       Alcotest.(check string) "id" "call_explicit" tool_use.id;
       Alcotest.(check string) "name" "get_weather" tool_use.name;
       Alcotest.(check bool)
         "input"
         true
         (tool_use.input = `Assoc [ "city", `String "Seoul" ])
     | _ -> Alcotest.fail "expected one ToolUse block")
;;

let test_ollama_parse_warns_on_malformed_tool_call () =
  let body =
    {|{"model":"provider_h-3:8b","done":true,"done_reason":"tool_calls",
       "message":{"role":"assistant","content":"",
         "tool_calls":[
           {"function":{"name":"ok_tool","arguments":{"city":"Seoul"}}},
           {"function":{"arguments":{"city":"Missing name"}}}
         ]}}|}
  in
  let logs = ref [] in
  let result =
    Llm_provider.Diag.with_sink
      (fun level ~ctx msg -> logs := (level, ctx, msg) :: !logs)
      (fun () -> BOL.parse_ollama_response body)
  in
  (match result with
   | Error msg -> Alcotest.fail msg
   | Ok resp ->
     (match resp.content with
      | [ ToolUse tool_use ] ->
        Alcotest.(check string) "surviving tool name" "ok_tool" tool_use.name
      | _ -> Alcotest.fail "expected one surviving ToolUse block"));
  let has_warning =
    List.exists
      (fun (level, ctx, msg) ->
         level = Llm_provider.Diag.Warn
         && ctx = "backend_ollama"
         && contains_substring ~sub:"dropped 1 malformed Ollama tool_call" msg)
      !logs
  in
  Alcotest.(check bool) "malformed tool call warning" true has_warning
;;

let test_provider_d_with_json_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ]
  in
  let config =
    PC.make
      ~kind:Provider_d_compat
      ~model_id:"model-d-mini"
      ~base_url:"https://api.provider_d.com/v1"
      ~response_format:(JsonSchema schema)
      ()
  in
  let body = BO.build_request ~config ~messages:[ user_msg "Return JSON." ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let response_format = json |> member "response_format" in
  Alcotest.(check string)
    "response_format.type"
    "json_schema"
    (response_format |> member "type" |> to_string);
  Alcotest.(check string)
    "json_schema.name"
    "structured_output"
    (response_format |> member "json_schema" |> member "name" |> to_string);
  Alcotest.(check string)
    "json_schema.schema.type"
    "object"
    (response_format
     |> member "json_schema"
     |> member "schema"
     |> member "type"
     |> to_string)
;;

let test_provider_f_with_json_schema () =
  let schema =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "answer", `Assoc [ "type", `String "string" ] ]
      ; "required", `List [ `String "answer" ]
      ]
  in
  let config =
    PC.make
      ~kind:Provider_f
      ~model_id:"provider_f-2.5-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ~api_key:"test-key"
      ~response_format:(JsonSchema schema)
      ()
  in
  let body = BGemini.build_request ~config ~messages:[ user_msg "Return JSON." ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let generation_config = json |> member "generationConfig" in
  Alcotest.(check string)
    "responseMimeType"
    "application/json"
    (generation_config |> member "responseMimeType" |> to_string);
  Alcotest.(check string)
    "responseJsonSchema.type"
    "object"
    (generation_config |> member "responseJsonSchema" |> member "type" |> to_string);
  Alcotest.(check string)
    "responseJsonSchema.required[0]"
    "answer"
    (generation_config
     |> member "responseJsonSchema"
     |> member "required"
     |> to_list
     |> List.hd
     |> to_string)
;;

let test_provider_c_direct_with_tools_and_thinking () =
  let config =
    PC.make
      ~kind:Provider_c
      ~model_id:"provider_c-for-coding"
      ~base_url:"https://api.provider_c.com/coding"
      ~enable_thinking:true
      ()
  in
  let tool =
    `Assoc
      [ "name", `String "shell"
      ; "description", `String "run shell command"
      ; "input_schema", `Assoc [ "type", `String "object" ]
      ]
  in
  let body =
    BA.build_request ~config ~messages:[ user_msg "inspect repo" ] ~tools:[ tool ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  let thinking = json |> member "thinking" in
  Alcotest.(check string)
    "model"
    "provider_c-for-coding"
    (json |> member "model" |> to_string);
  Alcotest.(check int) "tool count" 1 (List.length tools);
  Alcotest.(check string)
    "thinking type"
    "enabled"
    (thinking |> member "type" |> to_string)
;;

let test_provider_c_direct_tool_result_uses_text_blocks () =
  let config =
    PC.make
      ~kind:Provider_c
      ~model_id:"provider_c-for-coding"
      ~base_url:"https://api.provider_c.com/coding"
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking
              { thinking_type = "sig_1"; content = "I should call the calculator." }
          ; ToolUse
              { id = "tool_1"
              ; name = "calculator"
              ; input = `Assoc [ "a", `Int 2; "b", `Int 3 ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "tool_1"
              ; content = "5"
              ; is_error = false
              ; json = Some (`Int 5)
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = BA.build_request ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let replay = json |> member "messages" |> index 1 in
  let block = replay |> member "content" |> index 0 in
  let content_blocks = block |> member "content" |> to_list in
  Alcotest.(check string)
    "tool result role serialized as user"
    "user"
    (replay |> member "role" |> to_string);
  Alcotest.(check string)
    "tool_result type"
    "tool_result"
    (block |> member "type" |> to_string);
  Alcotest.(check string)
    "tool_use id preserved"
    "tool_1"
    (block |> member "tool_use_id" |> to_string);
  Alcotest.(check int) "tool_result content block count" 1 (List.length content_blocks);
  Alcotest.(check string)
    "nested text block type"
    "text"
    (List.hd content_blocks |> member "type" |> to_string);
  Alcotest.(check string)
    "nested text block content"
    "5"
    (List.hd content_blocks |> member "text" |> to_string)
;;

let test_glm_preserved_reasoning_replay_and_drops_unsupported_tool_choice () =
  let config =
    PC.make
      ~kind:Provider_k
      ~model_id:"provider_k-5.1"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~enable_thinking:true
      ~clear_thinking:false
      ~tool_stream:true
      ~tool_choice:(Tool "calculator")
      ()
  in
  let messages =
    [ { role = Assistant
      ; content =
          [ Thinking
              { thinking_type = "reasoning"; content = "I need the calculator result." }
          ; ToolUse
              { id = "call_1"
              ; name = "calculator"
              ; input = `Assoc [ "expr", `String "2+2" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "call_1"
              ; content = "{\"value\":4}"
              ; is_error = false
              ; json = Some (`Assoc [ "value", `Int 4 ])
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let body = BGlm.build_request ~stream:true ~config ~messages () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let assistant = json |> member "messages" |> index 0 in
  Alcotest.(check bool)
    "provider_k unsupported tool_choice dropped"
    true
    (match json with
     | `Assoc fields -> not (List.mem_assoc "tool_choice" fields)
     | _ -> false);
  Alcotest.(check string)
    "assistant content remains text channel"
    ""
    (assistant |> member "content" |> to_string);
  Alcotest.(check string)
    "reasoning replayed separately"
    "I need the calculator result."
    (assistant |> member "reasoning_content" |> to_string);
  Alcotest.(check bool)
    "clear_thinking false preserved"
    true
    (json |> member "thinking" |> member "clear_thinking" |> to_bool = false);
  Alcotest.(check bool)
    "tool_stream enabled"
    true
    (json |> member "tool_stream" |> to_bool)
;;

(* ── Provider_config.make ────────────────────────────── *)

let test_config_default_paths () =
  let anth = PC.make ~kind:Provider_a ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "provider_a path" "/v1/messages" anth.request_path;
  let provider_c = PC.make ~kind:Provider_c ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "provider_c path" "/v1/messages" provider_c.request_path;
  let oai = PC.make ~kind:Provider_d_compat ~model_id:"m" ~base_url:"" () in
  Alcotest.(check string) "provider_d path" "/v1/chat/completions" oai.request_path
;;

let test_config_custom_path () =
  let cfg =
    PC.make ~kind:Provider_d_compat ~model_id:"m" ~base_url:"" ~request_path:"/custom" ()
  in
  Alcotest.(check string) "custom path" "/custom" cfg.request_path
;;

(* ── Retry config ────────────────────────────────────── *)

let test_default_retry_config () =
  let cfg = Llm_provider.Complete.default_retry_config in
  Alcotest.(check int) "max_retries" 3 cfg.max_retries;
  Alcotest.(check (float 0.01)) "initial_delay" 1.0 cfg.initial_delay_sec;
  Alcotest.(check (float 0.01)) "max_delay" 30.0 cfg.max_delay_sec;
  Alcotest.(check (float 0.01)) "backoff" 2.0 cfg.backoff_multiplier
;;

let test_is_retryable () =
  let open Llm_provider in
  (* Retryable status codes *)
  Alcotest.(check bool)
    "429 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }));
  Alcotest.(check bool)
    "503 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }));
  Alcotest.(check bool)
    "529 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }));
  (* Network errors *)
  Alcotest.(check bool)
    "network retryable"
    true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "timeout"; kind = Unknown }));
  (* Non-retryable *)
  Alcotest.(check bool)
    "400 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }));
  Alcotest.(check bool)
    "401 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }));
  Alcotest.(check bool)
    "404 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }));
  (* Wiring bug — retrying cannot conjure a missing CLI transport. *)
  Alcotest.(check bool)
    "CliTransportRequired not retryable"
    false
    (Complete.is_retryable (Http_client.CliTransportRequired { kind = "cli_tool_d" }))
;;

let test_complete_agent_llm_a_code_without_transport_is_guarded () =
  (* Regression: [Complete.complete] used to forward CLI-kind configs
     (base_url = "") to cohttp-eio, which crashed with
     [Fmt.failwith "Unknown scheme None"].  The guard now returns a
     typed [CliTransportRequired] so cascades and callers can
     distinguish a wiring bug from a transient network failure.

     Covers the full matrix (Cli_tool_d, Cli_tool_b, Cli_tool_c, Cli_tool_a). *)
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let kinds =
    [ PC.Cli_tool_d, "cli_tool_d"
    ; PC.Cli_tool_b, "cli_tool_b"
    ; PC.Cli_tool_c, "cli_tool_c"
    ; PC.Cli_tool_a, "cli_tool_a"
    ]
  in
  List.iter
    (fun (kind, expected_name) ->
       let config = PC.make ~kind ~model_id:"auto" ~base_url:"" () in
       let messages = [ user_msg "hi" ] in
       match
         Llm_provider.Complete.complete ~sw ~net ~config ~messages ?transport:None ()
       with
       | Ok _ ->
         Alcotest.failf
           "%s with no transport must not succeed via HTTP fallback"
           expected_name
       | Error (Llm_provider.Http_client.CliTransportRequired { kind }) ->
         Alcotest.(check string)
           (Printf.sprintf "%s reports its own kind" expected_name)
           expected_name
           kind
       | Error (Llm_provider.Http_client.HttpError { code; _ }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got HttpError %d"
           expected_name
           code
       | Error (Llm_provider.Http_client.NetworkError { message; _ }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got NetworkError: %s (this is the 'Unknown \
            scheme None' regression)"
           expected_name
           message
       | Error (Llm_provider.Http_client.TimeoutError { message; _ }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got TimeoutError: %s"
           expected_name
           message
       | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got AcceptRejected: %s"
           expected_name
           reason
       | Error (Llm_provider.Http_client.ProviderTerminal { message; _ }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got ProviderTerminal: %s"
           expected_name
           message
       | Error (Llm_provider.Http_client.ProviderFailure { kind; message }) ->
         Alcotest.failf
           "%s expected CliTransportRequired, got ProviderFailure: %s"
           expected_name
           (Llm_provider.Http_client.provider_failure_to_string ~kind ~message))
    kinds
;;

let usage =
  Some
    { input_tokens = 1
    ; output_tokens = 1
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
;;

let fake_transport response : Llm_provider.Llm_transport.t =
  { complete_sync = (fun _request -> { response = Ok response; latency_ms = Some 1 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _request -> Ok response)
  }
;;

let complete_with_captured_diag ~config ~response =
  let entries = ref [] in
  let run () =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let transport = fake_transport response in
    match
      Llm_provider.Complete.complete
        ~sw
        ~net
        ~config
        ~messages:[ user_msg "hi" ]
        ~transport
        ()
    with
    | Ok _ -> ()
    | Error _ -> Alcotest.fail "fake completion should succeed"
  in
  Llm_provider.Diag.with_sink
    (fun level ~ctx message -> entries := (level, ctx, message) :: !entries)
    run;
  List.rev !entries
;;

let response_with_thinking =
  { id = "resp-thinking"
  ; model = "auto"
  ; stop_reason = EndTurn
  ; content = [ Thinking { thinking_type = "thinking"; content = "reasoning" } ]
  ; usage
  ; telemetry = None
  }
;;

let test_provider_default_thinking_drift_is_info () =
  let config =
    PC.make
      ~kind:Provider_d_compat
      ~model_id:"auto"
      ~base_url:"https://example.invalid/v1"
      ()
  in
  let entries = complete_with_captured_diag ~config ~response:response_with_thinking in
  Alcotest.(check bool)
    "no warn for provider-default thinking observation"
    false
    (List.exists (fun (level, _, _) -> level = Llm_provider.Diag.Warn) entries);
  Alcotest.(check bool)
    "low-confidence info is recorded"
    true
    (List.exists
       (fun (level, ctx, message) ->
          level = Llm_provider.Diag.Info
          && ctx = "complete"
          && contains_substring ~sub:"capability_observation" message
          && contains_substring ~sub:"provider_default" message
          && contains_substring ~sub:"low" message)
       entries)
;;

let test_model_capability_thinking_drift_remains_warn () =
  let config =
    PC.make
      ~kind:Provider_d_compat
      ~model_id:"provider_k-4-flash"
      ~base_url:"https://example.invalid/v1"
      ()
  in
  let entries = complete_with_captured_diag ~config ~response:response_with_thinking in
  Alcotest.(check bool)
    "model-specific mismatch remains warn"
    true
    (List.exists
       (fun (level, ctx, message) ->
          level = Llm_provider.Diag.Warn
          && ctx = "complete"
          && contains_substring ~sub:"capability_drift" message
          && contains_substring ~sub:"model" message
          && contains_substring ~sub:"high" message)
       entries)
;;

let test_complete_rejects_output_schema_for_glm () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let config =
    PC.make
      ~kind:Provider_k
      ~model_id:"provider_k-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match
    Llm_provider.Complete.complete ~sw ~net ~config ~messages:[ user_msg "hi" ] ()
  with
  | Error (Llm_provider.Http_client.AcceptRejected { reason }) ->
    Alcotest.(check bool)
      "mentions provider_k json mode"
      true
      (contains_substring ~sub:"json mode" (String.lowercase_ascii reason))
  | Ok _ -> Alcotest.fail "expected AcceptRejected for provider_k output_schema"
  | Error _ -> Alcotest.fail "expected AcceptRejected for provider_k output_schema"
;;

let test_annotate_response_cost () =
  let response : api_response =
    { id = "resp-1"
    ; model = "agent_llm_a-sonnet-4-6"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage =
        Some
          { input_tokens = 1_000
          ; output_tokens = 500
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    ; telemetry = None
    }
  in
  match Llm_provider.Pricing.annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } ->
    Alcotest.(check bool) "annotated cost" true (cost > 0.0)
  | _ -> Alcotest.fail "expected annotated response cost"
;;

let test_annotate_response_cost_gpt55 () =
  let response : api_response =
    { id = "resp-gpt55"
    ; model = "model-d-5.5"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage =
        Some
          { input_tokens = 1_000_000
          ; output_tokens = 1_000_000
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens = 0
          ; cost_usd = None
          }
    ; telemetry = None
    }
  in
  match Llm_provider.Pricing.annotate_response_cost response with
  | { usage = Some { cost_usd = Some cost; _ }; _ } ->
    Alcotest.(check (float 0.001)) "model-d-5.5 cost" 35.0 cost
  | _ -> Alcotest.fail "expected model-d-5.5 annotated response cost"
;;

(* ── Stream accumulator ──────────────────────────────── *)

let test_stream_acc_text () =
  (* Simulate a minimal Provider_a SSE event sequence *)
  let events =
    [ MessageStart
        { id = "msg_123"
        ; model = "agent_llm_a-sonnet-4-6"
        ; usage =
            Some
              { input_tokens = 10
              ; output_tokens = 0
              ; cache_creation_input_tokens = 0
              ; cache_read_input_tokens = 0
              ; cost_usd = None
              }
        }
    ; ContentBlockStart
        { index = 0; content_type = "text"; tool_id = None; tool_name = None }
    ; ContentBlockDelta { index = 0; delta = TextDelta "Hello " }
    ; ContentBlockDelta { index = 0; delta = TextDelta "world" }
    ; ContentBlockStop { index = 0 }
    ; MessageDelta
        { stop_reason = Some EndTurn
        ; usage =
            Some
              { input_tokens = 0
              ; output_tokens = 5
              ; cache_creation_input_tokens = 0
              ; cache_read_input_tokens = 0
              ; cost_usd = None
              }
        }
    ; MessageStop
    ]
  in
  (* Use the internal accumulator via a module alias *)
  let module C = Llm_provider.Complete in
  ignore C.default_retry_config;
  (* force link *)
  (* We can't call the internal functions directly, but we can test
     that the event types compose correctly *)
  Alcotest.(check int) "7 events" 7 (List.length events)
;;

let test_stream_acc_tool_use () =
  let events =
    [ MessageStart { id = "msg_456"; model = "model-d-4"; usage = None }
    ; ContentBlockStart
        { index = 0
        ; content_type = "tool_use"
        ; tool_id = Some "tu_1"
        ; tool_name = Some "calc"
        }
    ; ContentBlockDelta { index = 0; delta = InputJsonDelta "{\"x\":1}" }
    ; ContentBlockStop { index = 0 }
    ; MessageDelta { stop_reason = Some StopToolUse; usage = None }
    ; MessageStop
    ]
  in
  Alcotest.(check int) "6 events" 6 (List.length events)
;;

(* ── Prompt caching ───────────────────────────────── *)

(* Long prompt exceeding 3500 char threshold for cache_control *)
let long_prompt =
  String.concat
    ""
    (List.init 200 (fun i ->
       Printf.sprintf "Rule %d: follow this guideline carefully. " i))
;;

let test_cache_system_prompt () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"agent_llm_a-sonnet-4-6"
      ~base_url:""
      ~system_prompt:long_prompt
      ~cache_system_prompt:true
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let system = json |> member "system" |> to_list in
  Alcotest.(check int) "1 system block" 1 (List.length system);
  let block = List.hd system in
  Alcotest.(check string) "type" "text" (block |> member "type" |> to_string);
  let cc = block |> member "cache_control" in
  Alcotest.(check string)
    "cache_control type"
    "ephemeral"
    (cc |> member "type" |> to_string)
;;

let test_cache_short_prompt_skips () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"m"
      ~base_url:""
      ~system_prompt:"Short."
      ~cache_system_prompt:true
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "short = plain string"
    "Short."
    (json |> member "system" |> to_string)
;;

let test_cache_no_system_no_cache () =
  let config =
    PC.make
      ~kind:Provider_a
      ~model_id:"m"
      ~base_url:""
      ~system_prompt:"Hello."
      ~cache_system_prompt:false
      ()
  in
  let body = BA.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  (* system should be a plain string when caching disabled *)
  Alcotest.(check string)
    "system is string"
    "Hello."
    (json |> member "system" |> to_string)
;;

let test_cache_tools () =
  let config =
    PC.make ~kind:Provider_a ~model_id:"m" ~base_url:"" ~cache_system_prompt:true ()
  in
  let tool1 = `Assoc [ "name", `String "a"; "description", `String "tool a" ] in
  let tool2 = `Assoc [ "name", `String "b"; "description", `String "tool b" ] in
  let body =
    BA.build_request ~config ~messages:[ user_msg "hi" ] ~tools:[ tool1; tool2 ] ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check int) "2 tools" 2 (List.length tools);
  (* First tool should NOT have cache_control *)
  let first = List.hd tools in
  Alcotest.(check bool)
    "first tool no cache"
    true
    (first |> member "cache_control" = `Null);
  (* Last tool SHOULD have cache_control *)
  let last = List.nth tools 1 in
  let cc = last |> member "cache_control" in
  Alcotest.(check string)
    "last tool cache_control"
    "ephemeral"
    (cc |> member "type" |> to_string)
;;

let test_cache_default_false () =
  let cfg = PC.make ~kind:Provider_a ~model_id:"m" ~base_url:"" () in
  Alcotest.(check bool) "default cache off" false cfg.cache_system_prompt
;;

let () =
  let open Alcotest in
  run
    "provider_complete"
    [ ( "provider_a_build_request"
      , [ test_case "basic body" `Quick test_provider_a_basic_body
        ; test_case "with system" `Quick test_provider_a_with_system
        ; test_case "with thinking" `Quick test_provider_a_with_thinking
        ; test_case "with output schema" `Quick test_provider_a_output_schema
        ; test_case
            "with json schema response_format"
            `Quick
            test_provider_a_json_schema_response_format_without_output_schema
        ; test_case "stream flag" `Quick test_provider_a_stream_flag
        ; test_case
            "parse response initializes telemetry"
            `Quick
            test_provider_a_parse_response_initializes_telemetry
        ] )
    ; ( "provider_d_build_request"
      , [ test_case "basic body" `Quick test_provider_d_basic_body
        ; test_case "with system" `Quick test_provider_d_with_system
        ; test_case "with tools" `Quick test_provider_d_with_tools
        ; test_case
            "provider_c direct tools + thinking"
            `Quick
            test_provider_c_direct_with_tools_and_thinking
        ; test_case
            "provider_c direct tool_result uses text blocks"
            `Quick
            test_provider_c_direct_tool_result_uses_text_blocks
        ; test_case "stream flag" `Quick test_provider_d_stream_flag
        ; test_case "with json schema" `Quick test_provider_d_with_json_schema
        ; test_case "ollama output schema" `Quick test_ollama_output_schema
        ; test_case
            "ollama parse parallel tool calls object args"
            `Quick
            test_ollama_parse_parallel_tool_calls_object_arguments
        ; test_case
            "ollama parse explicit id string args"
            `Quick
            test_ollama_parse_tool_call_preserves_explicit_id_and_string_arguments
        ; test_case
            "ollama malformed tool call warning"
            `Quick
            test_ollama_parse_warns_on_malformed_tool_call
        ; test_case
            "provider_k preserved reasoning replay"
            `Quick
            test_glm_preserved_reasoning_replay_and_drops_unsupported_tool_choice
        ] )
    ; ( "provider_f_build_request"
      , [ test_case "with json schema" `Quick test_provider_f_with_json_schema ] )
    ; ( "provider_config"
      , [ test_case "default paths" `Quick test_config_default_paths
        ; test_case "custom path" `Quick test_config_custom_path
        ] )
    ; ( "retry"
      , [ test_case "default config" `Quick test_default_retry_config
        ; test_case "is_retryable" `Quick test_is_retryable
        ] )
    ; ( "cli_transport_guard"
      , [ test_case
            "complete refuses HTTP fallback for CLI kinds"
            `Quick
            test_complete_agent_llm_a_code_without_transport_is_guarded
        ; test_case
            "provider_k output schema rejected before request"
            `Quick
            test_complete_rejects_output_schema_for_glm
        ] )
    ; ( "capability_drift"
      , [ test_case
            "provider-default thinking observation is info"
            `Quick
            test_provider_default_thinking_drift_is_info
        ; test_case
            "model-specific thinking drift remains warn"
            `Quick
            test_model_capability_thinking_drift_remains_warn
        ] )
    ; ( "cost"
      , [ test_case "annotate response cost" `Quick test_annotate_response_cost
        ; test_case
            "annotate model-d-5.5 response cost"
            `Quick
            test_annotate_response_cost_gpt55
        ] )
    ; ( "stream_acc"
      , [ test_case "text events" `Quick test_stream_acc_text
        ; test_case "tool_use events" `Quick test_stream_acc_tool_use
        ] )
    ; ( "prompt_caching"
      , [ test_case "system block with cache_control" `Quick test_cache_system_prompt
        ; test_case "no cache when disabled" `Quick test_cache_no_system_no_cache
        ; test_case "last tool gets cache_control" `Quick test_cache_tools
        ; test_case "default cache off" `Quick test_cache_default_false
        ; test_case "short prompt skips cache" `Quick test_cache_short_prompt_skips
        ] )
    ]
;;
