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

let () =
  let open Alcotest in
  run "provider_complete" [
    "anthropic_build_request", [
      test_case "basic body" `Quick test_anthropic_basic_body;
      test_case "with system" `Quick test_anthropic_with_system;
      test_case "with thinking" `Quick test_anthropic_with_thinking;
      test_case "stream flag" `Quick test_anthropic_stream_flag;
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
  ]
