(** Tests for Provider_config — lightweight provider configuration. *)

open Llm_provider

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

(* ── make: defaults ───────────────────────────────────── *)

let test_make_defaults () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"test" ~base_url:"http://localhost:8080" () in
  check_string "model_id" "test" cfg.model_id;
  check_string "base_url" "http://localhost:8080" cfg.base_url;
  check_string "api_key default empty" "" cfg.api_key;
  check_bool "max_tokens default None" true (cfg.max_tokens = None);
  check_bool "temperature None" true (cfg.temperature = None);
  check_bool "top_p None" true (cfg.top_p = None);
  check_bool "top_k None" true (cfg.top_k = None);
  check_bool "min_p None" true (cfg.min_p = None);
  check_bool "system_prompt None" true (cfg.system_prompt = None);
  check_bool "enable_thinking None" true (cfg.enable_thinking = None);
  check_bool "thinking_budget None" true (cfg.thinking_budget = None);
  check_bool "clear_thinking None" true (cfg.clear_thinking = None);
  check_bool "tool_stream false" false cfg.tool_stream;
  check_bool "tool_choice None" true (cfg.tool_choice = None);
  check_bool "no parallel tool use" false cfg.disable_parallel_tool_use;
  check_bool "response format off" true (cfg.response_format = Types.Off);
  check_bool "no cache system prompt" false cfg.cache_system_prompt

(* ── make: request_path per kind ──────────────────────── *)

let test_request_path_anthropic () =
  let cfg = Provider_config.make ~kind:Anthropic
    ~model_id:"m" ~base_url:"" () in
  check_string "anthropic path" "/v1/messages" cfg.request_path

let test_request_path_openai () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"" () in
  check_string "openai path" "/v1/chat/completions" cfg.request_path

let test_request_path_gemini () =
  let cfg = Provider_config.make ~kind:Gemini
    ~model_id:"m" ~base_url:"" () in
  check_string "gemini path" "" cfg.request_path

let test_request_path_glm () =
  let cfg = Provider_config.make ~kind:Glm
    ~model_id:"m" ~base_url:"" () in
  check_string "glm path" "/chat/completions" cfg.request_path

let test_request_path_claude_code () =
  let cfg = Provider_config.make ~kind:Claude_code
    ~model_id:"m" ~base_url:"" () in
  check_string "claude_code path" "" cfg.request_path

let test_request_path_override () =
  let cfg = Provider_config.make ~kind:Anthropic
    ~model_id:"m" ~base_url:"" ~request_path:"/custom/path" () in
  check_string "custom path" "/custom/path" cfg.request_path

(* ── make: explicit values ────────────────────────────── *)

let test_make_with_all_options () =
  let cfg = Provider_config.make ~kind:Anthropic
    ~model_id:"claude-opus" ~base_url:"https://api.anthropic.com"
    ~api_key:"sk-test"
    ~headers:[("X-Custom", "val")]
    ~max_tokens:2048
    ~temperature:0.7
    ~top_p:0.9
    ~top_k:40
    ~min_p:0.05
    ~system_prompt:"system"
    ~enable_thinking:true
    ~thinking_budget:1000
    ~clear_thinking:false
    ~tool_stream:true
    ~disable_parallel_tool_use:true
    ~response_format_json:true
    ~cache_system_prompt:true () in
  check_string "api_key" "sk-test" cfg.api_key;
  check_bool "max_tokens" true (cfg.max_tokens = Some 2048);
  check_bool "temperature" true (cfg.temperature = Some 0.7);
  check_bool "top_p" true (cfg.top_p = Some 0.9);
  check_bool "top_k" true (cfg.top_k = Some 40);
  check_bool "min_p" true (cfg.min_p = Some 0.05);
  check_bool "system_prompt" true (cfg.system_prompt = Some "system");
  check_bool "enable_thinking" true (cfg.enable_thinking = Some true);
  check_bool "thinking_budget" true (cfg.thinking_budget = Some 1000);
  check_bool "clear_thinking" true (cfg.clear_thinking = Some false);
  check_bool "tool_stream" true cfg.tool_stream;
  check_bool "disable_parallel" true cfg.disable_parallel_tool_use;
  check_bool "json mode" true (cfg.response_format = Types.JsonMode);
  check_bool "cache prompt" true cfg.cache_system_prompt

(* ── make: headers default ────────────────────────────── *)

let test_default_headers () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"" () in
  check_int "1 default header" 1 (List.length cfg.headers);
  let (k, v) = List.hd cfg.headers in
  check_string "Content-Type key" "Content-Type" k;
  check_string "Content-Type val" "application/json" v

let test_custom_headers () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:""
    ~headers:[("Auth", "Bearer x"); ("X-Custom", "val")] () in
  check_int "2 custom headers" 2 (List.length cfg.headers)

(* ── locality ────────────────────────────────────────── *)

let test_is_local_loopback_ip () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"http://127.0.0.1:8085" () in
  check_bool "loopback ip is local" true (Provider_config.is_local cfg)

let test_is_local_localhost () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"http://localhost/v1" () in
  check_bool "localhost is local" true (Provider_config.is_local cfg)

let test_is_local_remote_false () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"https://api.example.com" () in
  check_bool "remote is not local" false (Provider_config.is_local cfg)

let test_is_local_host_boundary_false () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"http://localhostevil.com" () in
  check_bool "hostname boundary respected" false (Provider_config.is_local cfg)

let test_is_local_localhost_query_true () =
  let cfg = Provider_config.make ~kind:OpenAI_compat
    ~model_id:"m" ~base_url:"http://localhost?foo=bar" () in
  check_bool "localhost query is local" true (Provider_config.is_local cfg)

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run "provider_config" [
    "defaults", [
      Alcotest.test_case "make defaults" `Quick test_make_defaults;
      Alcotest.test_case "default headers" `Quick test_default_headers;
    ];
    "request_path", [
      Alcotest.test_case "anthropic" `Quick test_request_path_anthropic;
      Alcotest.test_case "openai" `Quick test_request_path_openai;
      Alcotest.test_case "gemini" `Quick test_request_path_gemini;
      Alcotest.test_case "glm" `Quick test_request_path_glm;
      Alcotest.test_case "claude_code" `Quick test_request_path_claude_code;
      Alcotest.test_case "override" `Quick test_request_path_override;
    ];
    "explicit_values", [
      Alcotest.test_case "all options" `Quick test_make_with_all_options;
      Alcotest.test_case "custom headers" `Quick test_custom_headers;
    ];
    "locality", [
      Alcotest.test_case "loopback ip" `Quick test_is_local_loopback_ip;
      Alcotest.test_case "localhost" `Quick test_is_local_localhost;
      Alcotest.test_case "remote false" `Quick test_is_local_remote_false;
      Alcotest.test_case "host boundary false" `Quick test_is_local_host_boundary_false;
      Alcotest.test_case "localhost query true" `Quick
        test_is_local_localhost_query_true;
    ];
  ]
