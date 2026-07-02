(** Tests for Provider_config — lightweight provider configuration. *)

open Llm_provider

let check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)
let getenv_from pairs name = List.assoc_opt name pairs

let reasoning_effort_option_to_string =
  Option.map Provider_config.reasoning_effort_to_string
;;

(* ── make: defaults ───────────────────────────────────── *)

let test_make_defaults () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"test"
      ~base_url:"http://localhost:8080"
      ()
  in
  check_string "model_id" "test" cfg.model_id;
  check_string "base_url" "http://localhost:8080" cfg.base_url;
  check_string "api_key default empty" "" (cfg.api_key :> string);
  check_bool "max_tokens default None" true (cfg.max_tokens = None);
  check_bool "temperature None" true (cfg.temperature = None);
  check_bool "top_p None" true (cfg.top_p = None);
  check_bool "top_k None" true (cfg.top_k = None);
  check_bool "min_p None" true (cfg.min_p = None);
  check_bool "system_prompt None" true (cfg.system_prompt = None);
  check_bool "enable_thinking None" true (cfg.enable_thinking = None);
  check_bool "preserve_thinking None" true (cfg.preserve_thinking = None);
  check_bool "thinking_budget None" true (cfg.thinking_budget = None);
  check_bool "clear_thinking None" true (cfg.clear_thinking = None);
  check_bool "tool_stream false" false cfg.tool_stream;
  check_bool "tool_choice None" true (cfg.tool_choice = None);
  check_bool "no parallel tool use" false cfg.disable_parallel_tool_use;
  check_bool "response format off" true (cfg.response_format = Types.Off);
  check_bool "no output schema" true (Option.is_none cfg.output_schema);
  check_bool "no cache system prompt" false cfg.cache_system_prompt
;;

(* ── make: request_path per kind ──────────────────────── *)

let test_request_path_anthropic () =
  let cfg = Provider_config.make ~kind:Anthropic ~model_id:"m" ~base_url:"" () in
  check_string "anthropic path" "/v1/messages" cfg.request_path
;;

let test_request_path_provider_c () =
  let cfg = Provider_config.make ~kind:Kimi ~model_id:"m" ~base_url:"" () in
  check_string "kimi path" "/v1/chat/completions" cfg.request_path
;;

let test_request_path_openai () =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  check_string "openai path" "/v1/chat/completions" cfg.request_path
;;

let test_request_path_gemini () =
  let cfg = Provider_config.make ~kind:Gemini ~model_id:"m" ~base_url:"" () in
  check_string "gemini path" "" cfg.request_path
;;

let test_request_path_glm () =
  let cfg = Provider_config.make ~kind:Glm ~model_id:"m" ~base_url:"" () in
  check_string "glm path" "/chat/completions" cfg.request_path
;;

let test_request_path_ollama () =
  let cfg = Provider_config.make ~kind:Ollama ~model_id:"m" ~base_url:"" () in
  check_string "ollama path" "/api/chat" cfg.request_path
;;

let test_request_path_dashscope () =
  let cfg = Provider_config.make ~kind:DashScope ~model_id:"m" ~base_url:"" () in
  check_string "dashscope path" "/chat/completions" cfg.request_path
;;

let test_request_path_override () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"m"
      ~base_url:""
      ~request_path:"/custom/path"
      ()
  in
  check_string "custom path" "/custom/path" cfg.request_path
;;

(* ── auth headers ────────────────────────────────────── *)

let check_headers = Alcotest.(check (list (pair string string)))

let test_auth_headers_for_kind_and_key_matches_config () =
  List.iter
    (fun kind ->
       let cfg =
         Provider_config.make
           ~kind
           ~model_id:"auth-model"
           ~base_url:"https://provider.example"
           ~api_key:"provider-key"
           ()
       in
       check_headers
         (Provider_config.string_of_provider_kind kind)
         (Provider_config.auth_headers_for_config cfg)
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:"provider-key"))
    Provider_config.all_provider_kinds
;;

let expected_auth_headers_for_kind = function
  | Provider_config.Anthropic | Provider_config.Kimi -> [ "x-api-key", "provider-key" ]
  | Provider_config.Gemini -> [ "x-goog-api-key", "provider-key" ]
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Glm
  | Provider_config.DashScope -> [ "Authorization", "Bearer provider-key" ]
;;

let test_auth_headers_for_kind_and_key_wire_headers () =
  List.iter
    (fun kind ->
       check_headers
         (Provider_config.string_of_provider_kind kind)
         (expected_auth_headers_for_kind kind)
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:"provider-key"))
    Provider_config.all_provider_kinds
;;

let test_auth_headers_for_kind_and_key_omits_empty_secret () =
  List.iter
    (fun kind ->
       check_headers
         (Provider_config.string_of_provider_kind kind)
         []
         (Provider_config.auth_headers_for_kind_and_key ~kind ~api_key:""))
    Provider_config.all_provider_kinds
;;

(* ── make: explicit values ────────────────────────────── *)

let test_make_with_all_options () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-opus"
      ~base_url:"https://api.anthropic.com"
      ~api_key:"sk-test"
      ~headers:[ "X-Custom", "val" ]
      ~max_tokens:2048
      ~temperature:0.7
      ~top_p:0.9
      ~top_k:40
      ~min_p:0.05
      ~system_prompt:"system"
      ~enable_thinking:true
      ~preserve_thinking:true
      ~thinking_budget:1000
      ~clear_thinking:false
      ~tool_stream:true
      ~disable_parallel_tool_use:true
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~cache_system_prompt:true
      ~supports_structured_output_override:true
      ()
  in
  check_string "api_key" "sk-test" (cfg.api_key :> string);
  check_bool "max_tokens" true (cfg.max_tokens = Some 2048);
  check_bool "temperature" true (cfg.temperature = Some 0.7);
  check_bool "top_p" true (cfg.top_p = Some 0.9);
  check_bool "top_k" true (cfg.top_k = Some 40);
  check_bool "min_p" true (cfg.min_p = Some 0.05);
  check_bool "system_prompt" true (cfg.system_prompt = Some "system");
  check_bool "enable_thinking" true (cfg.enable_thinking = Some true);
  check_bool "preserve_thinking" true (cfg.preserve_thinking = Some true);
  check_bool "thinking_budget" true (cfg.thinking_budget = Some 1000);
  check_bool "clear_thinking" true (cfg.clear_thinking = Some false);
  check_bool "tool_stream" true cfg.tool_stream;
  check_bool "disable_parallel" true cfg.disable_parallel_tool_use;
  let expected_schema = `Assoc [ "type", `String "object" ] in
  check_bool
    "json schema mode"
    true
    (cfg.response_format = Types.JsonSchema expected_schema);
  check_bool "has output schema" true (Option.is_some cfg.output_schema);
  check_bool "cache prompt" true cfg.cache_system_prompt;
  check_bool
    "structured output override"
    true
    (cfg.supports_structured_output_override = Some true)
;;

let test_make_response_format_json_mode () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt"
      ~base_url:"https://api.openai.com/v1"
      ~response_format_json:true
      ()
  in
  check_bool "json mode" true (cfg.response_format = Types.JsonMode);
  check_bool "no json schema" true (Option.is_none cfg.output_schema)
;;

let test_output_schema_of_response_format () =
  let schema = `Assoc [ "type", `String "object" ] in
  check_bool
    "schema derived"
    true
    (Option.equal
       Yojson.Safe.equal
       (Some schema)
       (Provider_config.output_schema_of_response_format (Types.JsonSchema schema)));
  check_bool
    "json mode has no schema"
    true
    (Option.is_none (Provider_config.output_schema_of_response_format Types.JsonMode));
  check_bool
    "off has no schema"
    true
    (Option.is_none (Provider_config.output_schema_of_response_format Types.Off));
  check_bool
    "override wins"
    true
    (Option.equal
       Yojson.Safe.equal
       (Some schema)
       (Provider_config.output_schema_of_response_format ~override:schema Types.JsonMode))
;;

let test_validate_output_schema_openai_official () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4o"
      ~base_url:"https://api.openai.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "official openai accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_openai_official_catalog_model () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-4o"
      ~base_url:"https://api.openai.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "official OpenAI catalog model accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_openai_compat_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt"
      ~base_url:"https://openrouter.ai/api/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "generic compat rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_unknown_openai_compat_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"generic"
      ~base_url:"https://openai-compatible.example.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "unknown OpenAI-compatible host rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_ollama_cloud_minimax_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"minimax-m3"
      ~base_url:"https://ollama.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "rejection"
      "model minimax-m3 does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected Ollama Cloud minimax-m3 capability rejection"
;;

let test_validate_output_schema_ollama_cloud_mistral_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"mistral-large-3:675b"
      ~base_url:"https://ollama.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "rejection"
      "model mistral-large-3:675b does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected Ollama Cloud model capability rejection"
;;

let test_validate_output_schema_native_ollama_ministral_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"ministral-3:8b"
      ~base_url:"http://localhost:11434"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "rejection"
      "model ministral-3:8b does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected native Ollama model capability rejection"
;;

let test_validate_output_schema_openai_compat_declared_endpoint_accepted () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"qwen/qwen3.6-35b-a3b"
      ~base_url:"https://ma8xbr1kgbclkl-64411be1.proxy.runpod.net/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~supports_structured_output_override:true
      ~model_capabilities_override:Capabilities.openai_compat_chat_capabilities
      ()
  in
  check_bool
    "declared self-hosted OpenAI-compatible endpoint accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_ollama_cloud_minimax_output_schema_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"minimax-m3"
      ~base_url:"https://ollama.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "rejection"
      "model minimax-m3 does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected Ollama Cloud minimax-m3 capability rejection"
;;

let test_validate_output_schema_ollama_cloud_rejects_unverified_model () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"mistral-large-3:675b"
      ~base_url:"https://ollama.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "reason"
      "model mistral-large-3:675b does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected Ollama Cloud model capability rejection"
;;

let test_validate_output_schema_native_ollama_rejects_unverified_model () =
  let cfg =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"ministral-3:8b"
      ~base_url:"http://localhost:11434"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg ->
    check_string
      "reason"
      "model ministral-3:8b does not advertise native structured output"
      msg
  | Ok () -> Alcotest.fail "expected native Ollama model capability rejection"
;;

let test_validate_output_schema_declared_endpoint_still_requires_model_capability () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"unknown-model-without-schema-capability"
      ~base_url:"https://schema-capable.example.test/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~supports_structured_output_override:true
      ()
  in
  check_bool
    "endpoint declaration does not invent model capability"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_endpoint_override_can_fail_closed () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.5"
      ~base_url:"https://api.openai.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ~supports_structured_output_override:false
      ()
  in
  check_bool
    "explicit endpoint override false rejects even official host"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_glm_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5"
      ~base_url:"https://api.z.ai/api/coding/paas/v4"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "glm rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_dashscope_accepted () =
  let cfg =
    Provider_config.make
      ~kind:DashScope
      ~model_id:"dashscope-max"
      ~base_url:"https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "dashscope accepted"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_kimi_rejected () =
  let cfg =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "kimi rejected"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_unrequested_ok () =
  let cfg =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ()
  in
  check_bool
    "no schema request bypasses provider restriction"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_direct_response_format_record () =
  let schema = `Assoc [ "type", `String "object" ] in
  let cfg =
    { (Provider_config.make
         ~kind:OpenAI_compat
         ~model_id:"gpt"
         ~base_url:"https://openrouter.ai/api/v1"
         ())
      with
      response_format = Types.JsonSchema schema
    ; output_schema = None
    }
  in
  check_bool
    "response_format JsonSchema is validated even without output_schema"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_output_schema_supported_non_openai () =
  let schema = `Assoc [ "type", `String "object" ] in
  List.iter
    (fun kind ->
       let cfg =
         Provider_config.make
           ~kind
           ~model_id:"m"
           ~base_url:"https://api.example.test"
           ~output_schema:schema
           ()
       in
       check_bool
         (Provider_config.string_of_provider_kind kind ^ " accepts schema")
         true
         (Result.is_ok (Provider_config.validate_output_schema_request cfg)))
    [ Anthropic; Gemini; DashScope ];
  let ollama_cfg =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"devstral-2:123b"
      ~base_url:"http://localhost:11434"
      ~output_schema:schema
      ~model_capabilities_override:
        { Capabilities.ollama_capabilities with supports_structured_output = true }
      ()
  in
  check_bool
    "ollama accepts schema only for models with a native SO guarantee"
    true
    (Result.is_ok (Provider_config.validate_output_schema_request ollama_cfg))
;;

let test_validate_output_schema_capability_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"unknown-model-without-schema-capability"
      ~base_url:"https://api.openai.com/v1"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  match Provider_config.validate_output_schema_request cfg with
  | Error msg -> check_bool "returns explanatory error" true (String.length msg > 0)
  | Ok () -> Alcotest.fail "expected model capability rejection"
;;

let test_openai_compat_raw_qwen_does_not_inherit_bare_capability () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"qwen/qwen3.6-35b-a3b"
      ~base_url:"https://unknown-openai-compatible.example/v1"
      ()
  in
  check_bool
    "raw OpenAI-compatible endpoint does not inherit bare qwen capability"
    true
    (Option.is_none (Provider_config.capabilities_for_config_model cfg))
;;

let test_openai_compat_raw_minimax_does_not_inherit_bare_reasoning_dialect () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"minimax-m3"
      ~base_url:"https://unknown-openai-compatible.example/v1"
      ()
  in
  check_bool
    "raw OpenAI-compatible endpoint does not inherit bare reasoning dialect"
    true
    (Option.is_none (Provider_config.capabilities_for_config_model cfg))
;;

let with_model_catalog_toml contents f =
  let previous_catalog = Model_catalog.global () in
  let restore () =
    match previous_catalog with
    | Some catalog -> Model_catalog.set_global catalog
    | None -> Model_catalog.clear_global ()
  in
  let path = Filename.temp_file "oas-provider-config-models" ".toml" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () ->
       Out_channel.with_open_text path (fun oc -> output_string oc contents);
       match Model_catalog.load_file path with
       | Error msg -> Alcotest.failf "failed to load model catalog: %s" msg
       | Ok catalog ->
         Model_catalog.set_global catalog;
         Fun.protect f ~finally:restore)
;;

let test_openai_compat_raw_tool_capability_requires_endpoint_declaration () =
  with_model_catalog_toml
    {|
[[models]]
id_prefix = "raw-tool-only-model"
base = "openai_chat"
supports_tools = true
supports_tool_choice = true
|}
    (fun () ->
       let cfg =
         Provider_config.make
           ~kind:OpenAI_compat
           ~model_id:"raw-tool-only-model"
           ~base_url:"https://unknown-openai-compatible.example/v1"
           ()
       in
       check_bool
         "raw OpenAI-compatible endpoint does not inherit bare tool wire capability"
         true
         (Option.is_none (Provider_config.capabilities_for_config_model cfg)))
;;

let test_openai_compat_raw_template_dialect_requires_endpoint_declaration () =
  with_model_catalog_toml
    {|
[[models]]
id_prefix = "raw-template-model"
base = "openai_chat"
supports_tools = true
supports_tool_choice = true
supports_reasoning = true
supports_extended_thinking = true
thinking_control_format = "chat_template_kwargs"
|}
    (fun () ->
       let cfg =
         Provider_config.make
           ~kind:OpenAI_compat
           ~model_id:"raw-template-model"
           ~base_url:"https://unknown-openai-compatible.example/v1"
           ()
       in
       check_bool
         "raw OpenAI-compatible endpoint does not inherit template thinking dialect"
         true
         (Option.is_none (Provider_config.capabilities_for_config_model cfg)))
;;

let test_openai_compat_explicit_provider_qualified_model_id_resolves_catalog_row () =
  with_model_catalog_toml
    {|
[[models]]
id_prefix = "runpod_mtp/qwen36-35b-a3b-mtp"
base = "openai_chat"
provider_name = "runpod_mtp"
supports_tools = true
supports_tool_choice = true
supports_reasoning = true
supports_extended_thinking = true
thinking_control_format = "chat_template_kwargs"
|}
    (fun () ->
       let cfg =
         Provider_config.make
           ~kind:OpenAI_compat
           ~model_id:"runpod_mtp.qwen36-35b-a3b-mtp"
           ~base_url:"https://unknown-openai-compatible.example/v1"
           ()
       in
       match Provider_config.capabilities_for_config_model cfg with
       | Some caps ->
         check_bool
           "explicit provider-qualified model keeps tools"
           true
           caps.supports_tools;
         check_bool
           "explicit provider-qualified model keeps reasoning"
           true
           caps.supports_reasoning;
         check_bool
           "explicit provider-qualified model uses chat template kwargs"
           true
           (caps.thinking_control_format = Capabilities.Chat_template_kwargs)
       | None ->
         Alcotest.fail
           "explicit provider-qualified model id should resolve its catalog row")
;;

let test_openai_compat_bare_model_id_does_not_resolve_provider_qualified_row () =
  with_model_catalog_toml
    {|
[[models]]
id_prefix = "runpod_mtp/qwen36-35b-a3b-mtp"
base = "openai_chat"
provider_name = "runpod_mtp"
supports_tools = true
supports_tool_choice = true
supports_reasoning = true
supports_extended_thinking = true
thinking_control_format = "chat_template_kwargs"
|}
    (fun () ->
       let cfg =
         Provider_config.make
           ~kind:OpenAI_compat
           ~model_id:"qwen36-35b-a3b-mtp"
           ~base_url:"https://unknown-openai-compatible.example/v1"
           ()
       in
       check_bool
         "bare raw model id does not inherit provider-qualified row"
         true
         (Option.is_none (Provider_config.capabilities_for_config_model cfg)))
;;

let test_validate_responses_request_path_allows_structured_output () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.5"
      ~base_url:"https://api.openai.com/v1"
      ~request_path:"/v1/responses"
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "responses structured output accepted at path layer"
    true
    (Result.is_ok (Provider_config.validate_request_path cfg))
;;

let test_validate_responses_request_path_allows_json_mode () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"gpt-5.5"
      ~base_url:"https://api.openai.com/v1"
      ~request_path:"/v1/responses"
      ~response_format_json:true
      ()
  in
  check_bool
    "responses json mode accepted at path layer"
    true
    (Result.is_ok (Provider_config.validate_request_path cfg))
;;

let test_validate_kimi_k27_rejects_forced_tool_choice () =
  let cfg tool_choice =
    Provider_config.make
      ~kind:Kimi
      ~model_id:"kimi-k2.7-code"
      ~base_url:"https://api.moonshot.ai/v1"
      ~tool_choice
      ()
  in
  check_bool
    "named forced tool_choice rejects"
    true
    (Result.is_error
       (Provider_config.validate_tool_choice_request (cfg (Types.Tool "lookup"))));
  check_bool
    "required forced tool_choice rejects"
    true
    (Result.is_error (Provider_config.validate_tool_choice_request (cfg Types.Any)));
  check_bool
    "auto tool_choice accepted"
    true
    (Result.is_ok (Provider_config.validate_tool_choice_request (cfg Types.Auto)));
  check_bool
    "none tool_choice accepted"
    true
    (Result.is_ok (Provider_config.validate_tool_choice_request (cfg Types.None_)))
;;

let test_validate_anthropic_thinking_rejects_forced_tool_choice () =
  let cfg ?(enable_thinking = true) tool_choice =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet-4-6"
      ~base_url:"https://api.anthropic.com"
      ~enable_thinking
      ~tool_choice
      ~model_capabilities_override:Capabilities.anthropic_capabilities
      ()
  in
  check_bool
    "thinking + named forced tool_choice rejects"
    true
    (Result.is_error
       (Provider_config.validate_tool_choice_request (cfg (Types.Tool "lookup"))));
  check_bool
    "thinking + required forced tool_choice rejects"
    true
    (Result.is_error (Provider_config.validate_tool_choice_request (cfg Types.Any)));
  check_bool
    "thinking + auto tool_choice accepted"
    true
    (Result.is_ok (Provider_config.validate_tool_choice_request (cfg Types.Auto)));
  check_bool
    "thinking + none tool_choice accepted"
    true
    (Result.is_ok (Provider_config.validate_tool_choice_request (cfg Types.None_)));
  check_bool
    "non-thinking forced tool_choice remains accepted"
    true
    (Result.is_ok
       (Provider_config.validate_tool_choice_request
          (cfg ~enable_thinking:false (Types.Tool "lookup"))))
;;

let test_validate_cli_sampling_params_allows_min_p () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-4"
      ~base_url:"https://api.anthropic.com"
      ~min_p:0.05
      ()
  in
  check_bool
    "sampling params currently accepted"
    true
    (Result.is_ok (Provider_config.validate_cli_sampling_params cfg))
;;

let test_connect_timeout_none_by_default () =
  let cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"https://x" ()
  in
  Alcotest.(check (option (float 0.001))) "default" None cfg.connect_timeout_s
;;

let test_connect_timeout_explicit_override_preserved () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"https://x"
      ~connect_timeout_s:600.0
      ()
  in
  Alcotest.(check (option (float 0.001))) "override" (Some 600.0) cfg.connect_timeout_s
;;

(* ── make: headers default ────────────────────────────── *)

let test_default_headers () =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"" () in
  check_int "1 default header" 1 (List.length cfg.headers);
  let k, v = List.hd cfg.headers in
  check_string "Content-Type key" "Content-Type" k;
  check_string "Content-Type val" "application/json" v
;;

let test_custom_headers () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:""
      ~headers:[ "Auth", "Bearer x"; "X-Custom", "val" ]
      ()
  in
  check_int "2 custom headers" 2 (List.length cfg.headers)
;;

(* ── locality ────────────────────────────────────────── *)

let test_is_local_loopback_ip () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_bool "loopback ip is local" true (Provider_config.is_local cfg)
;;

let test_is_local_localhost () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhost/v1"
      ()
  in
  check_bool "localhost is local" true (Provider_config.is_local cfg)
;;

let test_is_local_remote_false () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"https://api.example.com"
      ()
  in
  check_bool "remote is not local" false (Provider_config.is_local cfg)
;;

let test_is_local_host_boundary_false () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhostevil.com"
      ()
  in
  check_bool "hostname boundary respected" false (Provider_config.is_local cfg)
;;

let test_is_local_localhost_query_true () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"http://localhost?foo=bar"
      ()
  in
  check_bool "localhost query is local" true (Provider_config.is_local cfg)
;;

let test_default_attempt_timeout_s () =
  let check_timeout label expected kind =
    Alcotest.(check (option (float 0.001)))
      label
      expected
      (Provider_config.default_attempt_timeout_s kind)
  in
  check_timeout "ollama has no default hard attempt timeout" None Ollama;
  check_timeout "openai_compat has no default hard attempt timeout" None OpenAI_compat
;;

let test_connect_timeout_s_default_and_override () =
  let default_cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"https://x" ()
  in
  Alcotest.(check (option (float 0.001)))
    "default defers to downstream kind default"
    None
    default_cfg.connect_timeout_s;
  let explicit_cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"https://x"
      ~connect_timeout_s:600.0
      ()
  in
  Alcotest.(check (option (float 0.001)))
    "explicit override preserved"
    (Some 600.0)
    explicit_cfg.connect_timeout_s
;;

let test_validate_cli_sampling_params_anthropic_min_p_ok () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-4"
      ~base_url:"https://api.anthropic.com"
      ~min_p:0.05
      ()
  in
  check_bool
    "Anthropic min_p is accepted by current validator"
    true
    (Result.is_ok (Provider_config.validate_cli_sampling_params cfg))
;;

let test_max_turns_hard_cap_and_clamp () =
  Alcotest.(check (option int))
    "anthropic no hard cap"
    None
    (Provider_config.max_turns_hard_cap Anthropic);
  check_int
    "anthropic preserves request"
    99
    (Provider_config.clamp_max_turns Anthropic 99)
;;

let test_reasoning_effort_of_thinking_config () =
  let check_effort label expected enable_thinking thinking_budget =
    check_string
      label
      expected
      (Provider_config.effort_of_thinking_config ~enable_thinking ~thinking_budget)
  in
  check_effort "disabled" "none" (Some false) (Some 4096);
  check_effort "missing flag" "none" None (Some 4096);
  check_effort "zero budget" "none" (Some true) (Some 0);
  check_effort
    "low budget"
    "low"
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_effort
    "medium budget"
    "medium"
    (Some true)
    (Some Reasoning_effort.medium_budget_max_tokens);
  check_effort
    "high budget"
    "high"
    (Some true)
    (Some Reasoning_effort.high_budget_max_tokens);
  check_effort
    "xhigh budget"
    "xhigh"
    (Some true)
    (Some (Reasoning_effort.high_budget_max_tokens + 1))
;;

let test_reasoning_effort_top_tier_budget_mapping () =
  let check_effort label expected budget =
    Alcotest.(check (option string))
      label
      (Some expected)
      (reasoning_effort_option_to_string (Reasoning_effort.of_budget_with_xhigh budget))
  in
  check_effort "low top-tier mapping" "low" Reasoning_effort.low_budget_max_tokens;
  check_effort
    "medium top-tier mapping"
    "medium"
    Reasoning_effort.medium_budget_max_tokens;
  check_effort "high top-tier mapping" "high" Reasoning_effort.high_budget_max_tokens;
  check_effort
    "xhigh top-tier mapping"
    "xhigh"
    (Reasoning_effort.high_budget_max_tokens + 1);
  Alcotest.(check (option string))
    "non-positive budget omits effort"
    None
    (reasoning_effort_option_to_string (Reasoning_effort.of_budget_with_xhigh 0))
;;

let test_reasoning_effort_typed_roundtrip () =
  let cases =
    [ Provider_config.None_, "none"
    ; Provider_config.Minimal, "minimal"
    ; Provider_config.Low, "low"
    ; Provider_config.Medium, "medium"
    ; Provider_config.High, "high"
    ; Provider_config.XHigh, "xhigh"
    ]
  in
  List.iter
    (fun (value, wire) ->
       check_string "to wire" wire (Provider_config.reasoning_effort_to_string value);
       Alcotest.(check (option string))
         "from wire"
         (Some wire)
         (reasoning_effort_option_to_string
            (Provider_config.reasoning_effort_of_string wire)))
    cases;
  Alcotest.(check (option string))
    "unknown wire"
    None
    (reasoning_effort_option_to_string
       (Provider_config.reasoning_effort_of_string "urgent"));
  Alcotest.(check (option string))
    "trimmed case-insensitive wire"
    (Some "low")
    (reasoning_effort_option_to_string
       (Provider_config.reasoning_effort_of_string " LOW "))
;;

let test_reasoning_effort_typed_config_value () =
  let check_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (reasoning_effort_option_to_string
         (Provider_config.effort_of_thinking_config_value
            ~enable_thinking
            ~thinking_budget
            ()))
  in
  check_value "disabled typed" None (Some false) (Some 4096);
  check_value "missing flag typed" None None (Some 4096);
  check_value "zero budget typed" None (Some true) (Some 0);
  check_value
    "low typed"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_value
    "medium typed"
    (Some "medium")
    (Some true)
    (Some Reasoning_effort.medium_budget_max_tokens);
  check_value
    "high typed"
    (Some "high")
    (Some true)
    (Some Reasoning_effort.high_budget_max_tokens);
  check_value
    "xhigh typed"
    (Some "xhigh")
    (Some true)
    (Some (Reasoning_effort.high_budget_max_tokens + 1));
  let getenv = getenv_from [ "OAS_DEFAULT_REASONING_EFFORT", "xhigh" ] in
  Alcotest.(check (option string))
    "env default typed"
    (Some "xhigh")
    (reasoning_effort_option_to_string
       (Provider_config.effort_of_thinking_config_value
          ~getenv
          ~enable_thinking:(Some true)
          ~thinking_budget:None
          ()));
  let none_getenv = getenv_from [ "OAS_DEFAULT_REASONING_EFFORT", "none" ] in
  Alcotest.(check (option string))
    "env none typed"
    (Some "none")
    (reasoning_effort_option_to_string
       (Provider_config.effort_of_thinking_config_value
          ~getenv:none_getenv
          ~enable_thinking:(Some true)
          ~thinking_budget:None
          ()));
  let invalid_getenv = getenv_from [ "OAS_DEFAULT_REASONING_EFFORT", "urgent" ] in
  Alcotest.(check string)
    "invalid env defaults medium"
    "medium"
    (Provider_config.reasoning_effort_to_string
       (Provider_config.default_reasoning_effort_value ~getenv:invalid_getenv ()))
;;

let test_validate_reasoning_effort_subset_rejects_unsupported () =
  let manifest =
    Yojson.Safe.from_string
      {|{"schema_version":1,"models":[{"id_prefix":"effort-subset-model","base":"openai_chat_extended","accepted_reasoning_efforts":["low"]}]}|}
    |> Capability_manifest.of_json
    |> Result.get_ok
  in
  Fun.protect ~finally:Capability_manifest.clear_global (fun () ->
    Capability_manifest.set_global manifest;
    let cfg thinking_budget =
      Provider_config.make
        ~kind:OpenAI_compat
        ~model_id:"effort-subset-model"
        ~base_url:"https://api.openai.com/v1"
        ~enable_thinking:true
        ~thinking_budget
        ()
    in
    Alcotest.(check bool)
      "low accepted"
      true
      (Result.is_ok
         (Provider_config.validate_reasoning_effort_request_typed
            (cfg Reasoning_effort.low_budget_max_tokens)));
    match
      Provider_config.validate_reasoning_effort_request_typed
        (cfg Reasoning_effort.high_budget_max_tokens)
    with
    | Error
        (Provider_config.Unsupported_reasoning_effort
           { effort = Provider_config.High; accepted = [ Provider_config.Low ]; _ }) -> ()
    | Error rejection ->
      Alcotest.failf
        "unexpected rejection: %s"
        (Provider_config.reasoning_effort_request_rejection_to_message rejection)
    | Ok () -> Alcotest.fail "high effort should be rejected by accepted subset")
;;

let test_zai_glm_clear_thinking_request_field () =
  let resolve
        ?(thinking_control_format = Capabilities.No_thinking_control)
        ?(is_zai_glm = true)
        ?clear_thinking
        ?preserve_thinking
        ()
    =
    Provider_config.zai_glm_clear_thinking_request_field
      ~thinking_control_format
      ~is_zai_glm
      ~clear_thinking
      ~preserve_thinking
  in
  Alcotest.(check (option bool)) "default GLM clears" (Some true) (resolve ());
  Alcotest.(check (option bool))
    "preserve disables clear"
    (Some false)
    (resolve ~preserve_thinking:true ());
  Alcotest.(check (option bool))
    "explicit clear wins"
    (Some true)
    (resolve ~clear_thinking:true ~preserve_thinking:true ());
  Alcotest.(check (option bool)) "non-GLM omits" None (resolve ~is_zai_glm:false ());
  Alcotest.(check (option bool))
    "typed thinking control omits"
    None
    (resolve ~thinking_control_format:Capabilities.Thinking_object ())
;;

let test_reasoning_effort_of_config () =
  let ollama =
    Provider_config.make
      ~kind:Ollama
      ~model_id:"llama"
      ~base_url:"http://127.0.0.1:11434"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  let anthropic =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-sonnet"
      ~base_url:"https://api.anthropic.com"
      ~enable_thinking:true
      ~thinking_budget:2048
      ()
  in
  Alcotest.(check (option string))
    "ollama exposes effort"
    (Some "low")
    (Provider_config.reasoning_effort_of_config ollama);
  Alcotest.(check (option string))
    "non-ollama has no effort"
    None
    (Provider_config.reasoning_effort_of_config anthropic)
;;

let test_reasoning_effort_request_value () =
  let check_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (Provider_config.reasoning_effort_request_value ~enable_thinking ~thinking_budget)
  in
  let check_typed_value label expected enable_thinking thinking_budget =
    Alcotest.(check (option string))
      label
      expected
      (reasoning_effort_option_to_string
         (Provider_config.reasoning_effort_request_value_typed
            ~enable_thinking
            ~thinking_budget))
  in
  check_value "unset omits field" None None (Some 4096);
  check_value "disabled omits field" None (Some false) (Some 4096);
  check_value "zero budget omits field" None (Some true) (Some 0);
  check_value
    "enabled maps effort"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens);
  check_typed_value
    "enabled maps typed effort"
    (Some "low")
    (Some true)
    (Some Reasoning_effort.low_budget_max_tokens)
;;

let test_structured_output_name_of_schema () =
  let check_name label expected schema =
    check_string label expected (Provider_config.structured_output_name_of_schema schema)
  in
  check_name "normalizes title" "invoice_v2" (`Assoc [ "title", `String " Invoice V2! " ]);
  check_name
    "keeps hyphen underscore"
    "my-schema_v2"
    (`Assoc [ "title", `String "My-Schema_v2" ]);
  check_name
    "blank title uses default"
    "structured_output"
    (`Assoc [ "title", `String "   " ]);
  check_name "missing title uses default" "structured_output" (`Assoc []);
  check_name "non-object uses default" "structured_output" (`List [])
;;

(* ── provider_name_of_config ─────────────────────────── *)

let with_repository_model_catalog f =
  let previous_catalog = Model_catalog.global () in
  let restore () =
    match previous_catalog with
    | Some catalog -> Model_catalog.set_global catalog
    | None -> Model_catalog.clear_global ()
  in
  let candidates = [ "models.toml"; "../models.toml" ] in
  match List.find_opt Sys.file_exists candidates with
  | None -> Alcotest.fail "models.toml not found for provider_name tests"
  | Some path ->
    (match Model_catalog.load_file path with
     | Error msg -> Alcotest.failf "failed to load %s: %s" path msg
     | Ok catalog ->
       Model_catalog.set_global catalog;
       Fun.protect f ~finally:restore)
;;

let test_validate_output_schema_openai_official_catalog () =
  with_repository_model_catalog (fun () ->
    let cfg =
      Provider_config.make
        ~kind:OpenAI_compat
        ~model_id:"gpt-4o"
        ~base_url:"https://api.openai.com/v1"
        ~response_format_json:true
        ~output_schema:(`Assoc [ "type", `String "object" ])
        ()
    in
    check_bool
      "catalog OpenAI official host accepts json_schema"
      true
      (Result.is_ok (Provider_config.validate_output_schema_request cfg)))
;;

let test_validate_output_schema_ollama_cloud_catalog_minimax_rejected () =
  with_repository_model_catalog (fun () ->
    let cfg =
      Provider_config.make
        ~kind:OpenAI_compat
        ~model_id:"minimax-m3"
        ~base_url:"https://ollama.com/v1"
        ~response_format_json:true
        ~output_schema:(`Assoc [ "type", `String "object" ])
        ()
    in
    match Provider_config.validate_output_schema_request cfg with
    | Error msg ->
      check_string
        "rejection reason"
        "model minimax-m3 does not advertise native structured output"
        msg
    | Ok () -> Alcotest.fail "expected Ollama Cloud minimax-m3 capability rejection")
;;

let test_validate_output_schema_ollama_cloud_catalog_rejects_model_without_so () =
  with_repository_model_catalog (fun () ->
    let cfg =
      Provider_config.make
        ~kind:OpenAI_compat
        ~model_id:"mistral-large-3:675b"
        ~base_url:"https://ollama.com/v1"
        ~response_format_json:true
        ~output_schema:(`Assoc [ "type", `String "object" ])
        ()
    in
    match Provider_config.validate_output_schema_request cfg with
    | Error msg ->
      check_string
        "rejection reason"
        "model mistral-large-3:675b does not advertise native structured output"
        msg
    | Ok () -> Alcotest.fail "expected Ollama Cloud model capability rejection")
;;

let test_validate_output_schema_native_ollama_catalog_rejects_model_without_so () =
  with_repository_model_catalog (fun () ->
    let cfg =
      Provider_config.make
        ~kind:Ollama
        ~model_id:"ministral-3:8b"
        ~base_url:"http://localhost:11434"
        ~response_format_json:true
        ~output_schema:(`Assoc [ "type", `String "object" ])
        ()
    in
    match Provider_config.validate_output_schema_request cfg with
    | Error msg ->
      check_string
        "rejection reason"
        "model ministral-3:8b does not advertise native structured output"
        msg
    | Ok () -> Alcotest.fail "expected native Ollama model capability rejection")
;;

let test_validate_output_schema_unknown_openai_compat_host_rejected () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"generic"
      ~base_url:"https://openai-compatible.example.com/v1"
      ~response_format_json:true
      ~output_schema:(`Assoc [ "type", `String "object" ])
      ()
  in
  check_bool
    "unknown OpenAI-compatible host rejects json_schema"
    true
    (Result.is_error (Provider_config.validate_output_schema_request cfg))
;;

let test_validate_cli_sampling_params_accepts_anthropic_min_p () =
  let cfg =
    Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-4"
      ~base_url:"https://api.anthropic.com"
      ~min_p:0.05
      ()
  in
  check_bool
    "anthropic min_p validation is currently accepted"
    true
    (Result.is_ok (Provider_config.validate_cli_sampling_params cfg))
;;

let test_connect_timeout_s_default_none () =
  let cfg =
    Provider_config.make ~kind:OpenAI_compat ~model_id:"m" ~base_url:"https://x" ()
  in
  Alcotest.(check (option (float 0.001)))
    "default connect timeout"
    None
    cfg.connect_timeout_s
;;

let test_connect_timeout_s_explicit_override () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"m"
      ~base_url:"https://x"
      ~connect_timeout_s:600.0
      ()
  in
  Alcotest.(check (option (float 0.001)))
    "explicit connect timeout"
    (Some 600.0)
    cfg.connect_timeout_s
;;

let test_provider_name_of_config_glm_general () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:Zai_catalog.general_base_url
      ()
  in
  check_string "glm general" "glm" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_glm_coding () =
  let cfg =
    Provider_config.make
      ~kind:Glm
      ~model_id:"glm-5.1"
      ~base_url:Zai_catalog.coding_base_url
      ()
  in
  check_string "glm coding" "glm-coding" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_local_openai_compat () =
  (* A local OpenAI-compatible endpoint resolves to the neutral kind label, not
     the "nous" vendor entry: locality is transport, not vendor identity
     (RFC-OAS-034). Capabilities for the canonical local llama endpoint come from
     its explicit endpoint binding, not from this name. *)
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"local-model"
      ~base_url:"http://127.0.0.1:8085"
      ()
  in
  check_string
    "local openai compat resolves to neutral kind label"
    "openai_compat"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_openrouter () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"openai/gpt-oss-20b"
      ~base_url:"https://openrouter.ai/api/v1"
      ~request_path:"/chat/completions"
      ()
  in
  check_string "openrouter" "openrouter" (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_unmatched_openai_compat () =
  let cfg =
    Provider_config.make
      ~kind:OpenAI_compat
      ~model_id:"unlisted-model"
      ~base_url:"https://unlisted.example/v1"
      ~request_path:"/chat/completions"
      ()
  in
  check_string
    "unmatched openai compat"
    "openai_compat"
    (Provider_registry.provider_name_of_config cfg)
;;

let test_capability_provider_label_deepseek_exact_host () =
  let label base_url =
    Provider_config.capability_provider_label
      (Provider_config.make ~kind:OpenAI_compat ~model_id:"deepseek-v4-pro" ~base_url ())
  in
  (* RFC-OAS-034 rule 2: api.deepseek.com is DeepSeek's canonical vendor host, so
     its endpoint carries the vendor identity regardless of scheme. *)
  check_string
    "https canonical host is deepseek"
    "deepseek"
    (label "https://api.deepseek.com/v1");
  check_string
    "http canonical host is deepseek"
    "deepseek"
    (label "http://api.deepseek.com");
  (* Exact [Uri.host] equality must reject look-alikes so a hostile or accidental
     host cannot inherit the deepseek vendor identity. Falls back to the transport
     kind label ("openai_compat") rather than "deepseek". *)
  check_string
    "subdomain lookalike is not deepseek"
    "openai_compat"
    (label "https://api.deepseek.com.evil.example/v1");
  check_string
    "userinfo lookalike is not deepseek"
    "openai_compat"
    (label "https://api.deepseek.com@evil.example/v1")
;;

let check_unmatched_provider_name_ignores_catalog_model ~label ~model_id =
  with_repository_model_catalog (fun () ->
    let cfg =
      Provider_config.make
        ~kind:OpenAI_compat
        ~model_id
        ~base_url:"https://unlisted.example/v1"
        ~request_path:"/chat/completions"
        ()
    in
    check_string label "openai_compat" (Provider_registry.provider_name_of_config cfg))
;;

let check_provider_name_from_registered_endpoint ~label ~provider ~model_id =
  match Provider_registry.find (Provider_registry.default ()) provider with
  | None -> Alcotest.failf "provider %S not registered" provider
  | Some (entry : Provider_registry.entry) ->
    let cfg =
      Provider_config.make
        ~kind:entry.defaults.kind
        ~model_id
        ~base_url:entry.defaults.base_url
        ~request_path:entry.defaults.request_path
        ()
    in
    check_string label provider (Provider_registry.provider_name_of_config cfg)
;;

let test_provider_name_of_config_ignores_xai_catalog_model () =
  check_unmatched_provider_name_ignores_catalog_model
    ~label:"unmatched endpoint ignores xai catalog provider"
    ~model_id:"grok-4.3"
;;

let test_provider_name_of_config_ignores_mistral_catalog_model () =
  check_unmatched_provider_name_ignores_catalog_model
    ~label:"unmatched endpoint ignores mistral catalog provider"
    ~model_id:"mistral-large"
;;

let test_provider_name_of_config_ignores_cohere_catalog_model () =
  check_unmatched_provider_name_ignores_catalog_model
    ~label:"unmatched endpoint ignores cohere catalog provider"
    ~model_id:"command-r-plus"
;;

let test_provider_name_of_config_ignores_mimo_catalog_model () =
  check_unmatched_provider_name_ignores_catalog_model
    ~label:"unmatched endpoint ignores mimo catalog provider"
    ~model_id:"mimo-v2.5-pro"
;;

let test_provider_name_of_config_xai_registered_endpoint () =
  check_provider_name_from_registered_endpoint
    ~label:"xai registered endpoint"
    ~provider:"xai"
    ~model_id:"grok-4.3"
;;

let test_provider_name_of_config_mistral_registered_endpoint () =
  check_provider_name_from_registered_endpoint
    ~label:"mistral registered endpoint"
    ~provider:"mistral"
    ~model_id:"mistral-large"
;;

let test_provider_name_of_config_cohere_registered_endpoint () =
  check_provider_name_from_registered_endpoint
    ~label:"cohere registered endpoint"
    ~provider:"cohere"
    ~model_id:"command-r-plus"
;;

let test_provider_name_of_config_mimo_registered_endpoint () =
  check_provider_name_from_registered_endpoint
    ~label:"mimo registered endpoint"
    ~provider:"mimo"
    ~model_id:"mimo-v2.5-pro"
;;

(* ── provider_kind_of_string ─────────────────────────── *)

(** Check a raw string parses to the expected variant. Compared via
    [string_of_provider_kind] to avoid needing a derived [equal_provider_kind]. *)
let check_parse label input expected =
  match Provider_config.provider_kind_of_string input with
  | None -> Alcotest.failf "%s: expected Some _, got None for %S" label input
  | Some k ->
    let got = Provider_config.string_of_provider_kind k in
    let want = Provider_config.string_of_provider_kind expected in
    check_string label want got
;;

(* SSOT: pull the canonical list from the type's own module so adding a
   new variant without updating [Provider_kind.all] is caught by the
   [test_all_is_exhaustive] property below rather than silently skipping
   the new variant in every iterative test. *)
let all_kinds : Provider_config.provider_kind list = Provider_config.all_provider_kinds

let test_kind_roundtrip () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       check_parse ("roundtrip " ^ s) s k)
    all_kinds
;;

let test_kind_aliases_rejected () =
  List.iter
    (fun input ->
       check_bool
         ("alias rejected " ^ input)
         true
         (Option.is_none (Provider_config.provider_kind_of_string input)))
    [ "claude"; "openai"; "nous"; "claude"; "openai"; "llama"; "zhipu" ]
;;

let test_kind_case_insensitive () =
  check_parse "ANTHROPIC" "ANTHROPIC" Anthropic;
  check_parse "OpenAI_Compat" "OpenAI_Compat" OpenAI_compat;
  check_parse "Glm" "Glm" Glm
;;

let test_kind_whitespace () =
  check_parse "leading ws" "  anthropic" Anthropic;
  check_parse "trailing ws" "ollama  " Ollama;
  check_parse "both ws" "\topenai_compat\n" OpenAI_compat
;;

let test_kind_unknown_returns_none () =
  check_bool
    "empty string"
    true
    (Option.is_none (Provider_config.provider_kind_of_string ""));
  check_bool
    "misspelling"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "anthrpic"));
  check_bool
    "bare openrouter"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "openrouter"));
  check_bool
    "json-ish"
    true
    (Option.is_none (Provider_config.provider_kind_of_string "\"claude\""))
;;

(* ── provider_kind serializers ───────────────────────── *)

let test_show_matches_string_of () =
  List.iter
    (fun k ->
       check_string
         "show = string_of"
         (Provider_config.string_of_provider_kind k)
         (Provider_config.show_provider_kind k))
    all_kinds
;;

let test_pp_uses_lowercase () =
  let buf = Buffer.create 32 in
  let fmt = Format.formatter_of_buffer buf in
  Provider_config.pp_provider_kind fmt Anthropic;
  Format.pp_print_flush fmt ();
  check_string "pp Anthropic" "anthropic" (Buffer.contents buf)
;;

let test_to_yojson_roundtrip () =
  List.iter
    (fun k ->
       let json = Provider_config.provider_kind_to_yojson k in
       match json with
       | `String s ->
         check_string "to_yojson wire form" (Provider_config.string_of_provider_kind k) s
       | _ -> Alcotest.fail "to_yojson must produce `String")
    all_kinds
;;

let test_of_yojson_accepts_canonical () =
  List.iter
    (fun k ->
       let s = Provider_config.string_of_provider_kind k in
       let json : Yojson.Safe.t = `String s in
       match Provider_config.provider_kind_of_yojson json with
       | Ok k' ->
         check_string "of_yojson roundtrip" s (Provider_config.string_of_provider_kind k')
       | Error msg -> Alcotest.failf "of_yojson failed for %s: %s" s msg)
    all_kinds
;;

let test_of_yojson_rejects_aliases () =
  List.iter
    (fun input ->
       let json : Yojson.Safe.t = `String input in
       match Provider_config.provider_kind_of_yojson json with
       | Ok _ -> Alcotest.failf "of_yojson alias %S should fail" input
       | Error _ -> ())
    [ "claude"; "openai"; "nous" ]
;;

let test_of_yojson_rejects_unknown_string () =
  let json : Yojson.Safe.t = `String "nopenope" in
  match Provider_config.provider_kind_of_yojson json with
  | Ok _ -> Alcotest.fail "expected Error for unknown string"
  | Error _ -> ()
;;

let test_of_yojson_rejects_non_string () =
  let cases : (string * Yojson.Safe.t) list =
    [ "null", `Null; "int", `Int 1; "assoc", `Assoc [ "kind", `String "anthropic" ] ]
  in
  List.iter
    (fun (label, json) ->
       match Provider_config.provider_kind_of_yojson json with
       | Ok _ -> Alcotest.failf "expected Error for non-string %s" label
       | Error _ -> ())
    cases
;;

(* ── telemetry wire-format regression ─────────────────── *)

(** Build a throwaway inference_telemetry with only provider_kind varying.
    Other fields carry placeholder values so the serialised payload is stable. *)
let telemetry_with_kind (pk : Provider_config.provider_kind option)
  : Types.inference_telemetry
  =
  { Types.default_inference_telemetry with provider_kind = pk }
;;

(** Substring search helper local to this module. *)
let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop i =
    if i + sub_len > text_len
    then false
    else if String.sub text i sub_len = sub
    then true
    else loop (i + 1)
  in
  sub_len = 0 || loop 0
;;

let test_wire_kind_lowercase () =
  let cases =
    [ Provider_config.Anthropic, "\"provider_kind\":\"anthropic\""
    ; Provider_config.OpenAI_compat, "\"provider_kind\":\"openai_compat\""
    ; Provider_config.Ollama, "\"provider_kind\":\"ollama\""
    ; Provider_config.Gemini, "\"provider_kind\":\"gemini\""
    ; Provider_config.Glm, "\"provider_kind\":\"glm\""
    ]
  in
  List.iter
    (fun (kind, expected_substring) ->
       let json = Types.inference_telemetry_to_yojson (telemetry_with_kind (Some kind)) in
       let encoded = Yojson.Safe.to_string json in
       Alcotest.(check bool)
         (Printf.sprintf
            "wire for %s contains %s"
            (Provider_config.string_of_provider_kind kind)
            expected_substring)
         true
         (contains_substring ~sub:expected_substring encoded))
    cases
;;

let test_wire_kind_none_roundtrip () =
  let t = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson t) in
  (* None should not produce "anthropic" / "ollama" / any kind string. *)
  List.iter
    (fun s ->
       Alcotest.(check bool)
         (Printf.sprintf "None telemetry must not contain %S" s)
         false
         (contains_substring ~sub:s encoded))
    [ "\"anthropic\""; "\"ollama\""; "\"openai_compat\"" ]
;;

let test_wire_unknown_latency_is_null () =
  let original = telemetry_with_kind None in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "unknown latency encoded as JSON null"
    true
    (contains_substring ~sub:"\"request_latency_ms\":null" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "unknown latency roundtrips"
    None
    decoded.request_latency_ms
;;

let test_wire_measured_zero_latency_is_distinct () =
  let original = { (telemetry_with_kind None) with request_latency_ms = Some 0 } in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  Alcotest.(check bool)
    "measured zero encoded as JSON zero"
    true
    (contains_substring ~sub:"\"request_latency_ms\":0" encoded);
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  Alcotest.(check (option int))
    "measured zero roundtrips"
    (Some 0)
    decoded.request_latency_ms
;;

(* ── enumeration & default_api_key_env ────────────────── *)

(** [all_provider_kinds] must contain every variant exactly once. The
    property guards against adding a variant to the sum type without
    extending {!Provider_kind.all}; subsequent iterative tests would
    silently skip the new kind otherwise. *)
let test_all_is_exhaustive () =
  let xs = Provider_config.all_provider_kinds in
  Alcotest.(check int) "seven canonical variants" 7 (List.length xs);
  Alcotest.(check bool)
    "no duplicate canonical strings"
    true
    (let strs = List.map Provider_config.string_of_provider_kind xs in
     List.length strs = List.length (List.sort_uniq compare strs));
  (* Exhaustive match: any missing or extra variant produces a compile
     error here — the check is the compiler, not the runtime. *)
  List.iter
    (fun k ->
       match (k : Provider_config.provider_kind) with
       | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | DashScope | Glm -> ())
    xs
;;

let test_all_drives_parse_roundtrip () =
  (* Property: [of_string (to_string k) = Some k] for every variant in
     [all_provider_kinds]. Stronger than the spot-check roundtrip
     because the driver is the canonical enumeration — new variants
     are tested automatically. *)
  List.iter
    (fun k ->
       let encoded = Provider_config.string_of_provider_kind k in
       match Provider_config.provider_kind_of_string encoded with
       | Some k' ->
         Alcotest.(check string)
           ("parse " ^ encoded)
           encoded
           (Provider_config.string_of_provider_kind k')
       | None -> Alcotest.failf "of_string %S returned None for a canonical form" encoded)
    Provider_config.all_provider_kinds
;;

let test_default_api_key_env_known () =
  Alcotest.(check (option string))
    "anthropic"
    (Some "ANTHROPIC_API_KEY")
    (Provider_config.default_api_key_env Anthropic);
  Alcotest.(check (option string))
    "gemini"
    (Some "GEMINI_API_KEY")
    (Provider_config.default_api_key_env Gemini);
  Alcotest.(check (option string))
    "glm"
    (Some "ZAI_API_KEY")
    (Provider_config.default_api_key_env Glm);
  Alcotest.(check (option string))
    "kimi"
    (Some "KIMI_API_KEY")
    (Provider_config.default_api_key_env Kimi)
;;

let test_default_api_key_env_none_for_others () =
  (* Local / transport-mediated / OpenAI-compatible share: OAS does not
     dictate a single env var; callers supply their own. *)
  List.iter
    (fun (label, k) ->
       Alcotest.(check (option string)) label None (Provider_config.default_api_key_env k))
    [ "openai_compat", Provider_config.OpenAI_compat; "ollama", Provider_config.Ollama ]
;;

let test_wire_kind_roundtrip_via_yojson () =
  (* End-to-end: record -> JSON string -> JSON tree -> record; the
     provider_kind survives as the same typed constructor. *)
  let original = telemetry_with_kind (Some Provider_config.Ollama) in
  let encoded = Yojson.Safe.to_string (Types.inference_telemetry_to_yojson original) in
  let decoded =
    match Types.inference_telemetry_of_yojson (Yojson.Safe.from_string encoded) with
    | Ok t -> t
    | Error msg -> Alcotest.failf "roundtrip decode failed: %s" msg
  in
  match decoded.provider_kind with
  | Some Ollama -> ()
  | Some other ->
    Alcotest.failf
      "roundtrip produced wrong variant: %s"
      (Provider_config.string_of_provider_kind other)
  | None -> Alcotest.fail "roundtrip produced None"
;;

let test_capability_provider_label_ollama_cloud_exact_host () =
  let label base_url =
    Provider_config.capability_provider_label
      (Provider_config.make ~kind:Ollama ~model_id:"m" ~base_url ())
  in
  (* Apex ollama.com resolves to the cloud vendor label regardless of scheme. *)
  check_string "https apex is cloud" "ollama_cloud" (label "https://ollama.com/v1");
  check_string "http apex is cloud" "ollama_cloud" (label "http://ollama.com");
  (* RFC-OAS-034 B4: a raw URL-prefix match ([starts_with "https://ollama.com"])
     wrongly accepted these lookalike hosts because the prefix ends inside a
     longer hostname. Exact [Uri.host] equality must reject them so a hostile or
     accidental lookalike cannot inherit the ollama-cloud identity. *)
  Alcotest.(check bool)
    "subdomain lookalike rejected"
    false
    (String.equal "ollama_cloud" (label "https://ollama.company.com/v1"));
  Alcotest.(check bool)
    "suffix lookalike rejected"
    false
    (String.equal "ollama_cloud" (label "https://ollama.com.evil.example/v1"));
  (* A prefix matcher also accepts a userinfo-based lookalike: the authority
     [ollama.com@evil.example] makes [starts_with "https://ollama.com"] true
     while the real [Uri.host] is [evil.example]. Exact host equality rejects it. *)
  Alcotest.(check bool)
    "userinfo lookalike rejected"
    false
    (String.equal "ollama_cloud" (label "https://ollama.com@evil.example/v1"))
;;

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "provider_config"
    [ ( "defaults"
      , [ Alcotest.test_case "make defaults" `Quick test_make_defaults
        ; Alcotest.test_case "default headers" `Quick test_default_headers
        ; Alcotest.test_case
            "connect timeout default"
            `Quick
            test_connect_timeout_s_default_none
        ; Alcotest.test_case
            "connect timeout override"
            `Quick
            test_connect_timeout_s_explicit_override
        ] )
    ; ( "request_path"
      , [ Alcotest.test_case "anthropic" `Quick test_request_path_anthropic
        ; Alcotest.test_case "kimi" `Quick test_request_path_provider_c
        ; Alcotest.test_case "openai" `Quick test_request_path_openai
        ; Alcotest.test_case "gemini" `Quick test_request_path_gemini
        ; Alcotest.test_case "glm" `Quick test_request_path_glm
        ; Alcotest.test_case "ollama" `Quick test_request_path_ollama
        ; Alcotest.test_case "dashscope" `Quick test_request_path_dashscope
        ; Alcotest.test_case "override" `Quick test_request_path_override
        ] )
    ; ( "auth_headers"
      , [ Alcotest.test_case
            "kind/key API matches config API"
            `Quick
            test_auth_headers_for_kind_and_key_matches_config
        ; Alcotest.test_case
            "kind/key API emits provider wire headers"
            `Quick
            test_auth_headers_for_kind_and_key_wire_headers
        ; Alcotest.test_case
            "empty secret omits headers"
            `Quick
            test_auth_headers_for_kind_and_key_omits_empty_secret
        ] )
    ; ( "explicit_values"
      , [ Alcotest.test_case "all options" `Quick test_make_with_all_options
        ; Alcotest.test_case "custom headers" `Quick test_custom_headers
        ; Alcotest.test_case
            "connect timeout default and override"
            `Quick
            test_connect_timeout_s_default_and_override
        ; Alcotest.test_case
            "cli sampling params accepted"
            `Quick
            test_validate_cli_sampling_params_anthropic_min_p_ok
        ; Alcotest.test_case
            "response_format_json mode"
            `Quick
            test_make_response_format_json_mode
        ; Alcotest.test_case
            "output schema derivation"
            `Quick
            test_output_schema_of_response_format
        ; Alcotest.test_case
            "sampling params allow min_p"
            `Quick
            test_validate_cli_sampling_params_allows_min_p
        ; Alcotest.test_case
            "connect timeout none by default"
            `Quick
            test_connect_timeout_none_by_default
        ; Alcotest.test_case
            "connect timeout explicit override"
            `Quick
            test_connect_timeout_explicit_override_preserved
        ] )
    ; ( "output_schema"
      , [ Alcotest.test_case
            "official openai"
            `Quick
            test_validate_output_schema_openai_official
        ; Alcotest.test_case
            "official openai catalog model"
            `Quick
            test_validate_output_schema_openai_official_catalog_model
        ; Alcotest.test_case
            "official openai catalog"
            `Quick
            test_validate_output_schema_openai_official_catalog
        ; Alcotest.test_case
            "generic compat rejected"
            `Quick
            test_validate_output_schema_openai_compat_rejected
        ; Alcotest.test_case
            "unknown compat rejected"
            `Quick
            test_validate_output_schema_unknown_openai_compat_rejected
        ; Alcotest.test_case
            "ollama cloud minimax rejected"
            `Quick
            test_validate_output_schema_ollama_cloud_minimax_rejected
        ; Alcotest.test_case
            "ollama cloud mistral rejected"
            `Quick
            test_validate_output_schema_ollama_cloud_mistral_rejected
        ; Alcotest.test_case
            "native ollama ministral rejected"
            `Quick
            test_validate_output_schema_native_ollama_ministral_rejected
        ; Alcotest.test_case
            "declared compat endpoint accepted"
            `Quick
            test_validate_output_schema_openai_compat_declared_endpoint_accepted
        ; Alcotest.test_case
            "ollama cloud minimax output_schema rejected"
            `Quick
            test_validate_output_schema_ollama_cloud_minimax_output_schema_rejected
        ; Alcotest.test_case
            "ollama cloud rejects unverified model"
            `Quick
            test_validate_output_schema_ollama_cloud_rejects_unverified_model
        ; Alcotest.test_case
            "native ollama rejects unverified model"
            `Quick
            test_validate_output_schema_native_ollama_rejects_unverified_model
        ; Alcotest.test_case
            "declared endpoint still requires model capability"
            `Quick
            test_validate_output_schema_declared_endpoint_still_requires_model_capability
        ; Alcotest.test_case
            "endpoint override can fail closed"
            `Quick
            test_validate_output_schema_endpoint_override_can_fail_closed
        ; Alcotest.test_case
            "glm rejected"
            `Quick
            test_validate_output_schema_glm_rejected
        ; Alcotest.test_case
            "kimi rejected"
            `Quick
            test_validate_output_schema_kimi_rejected
        ; Alcotest.test_case
            "dashscope accepted"
            `Quick
            test_validate_output_schema_dashscope_accepted
        ; Alcotest.test_case
            "unrequested schema bypasses restrictions"
            `Quick
            test_validate_output_schema_unrequested_ok
        ; Alcotest.test_case
            "direct JsonSchema record is validated"
            `Quick
            test_validate_output_schema_direct_response_format_record
        ; Alcotest.test_case
            "supported non-openai providers"
            `Quick
            test_validate_output_schema_supported_non_openai
        ; Alcotest.test_case
            "openai capability rejection"
            `Quick
            test_validate_output_schema_capability_rejected
        ; Alcotest.test_case
            "ollama cloud catalog minimax rejected"
            `Quick
            test_validate_output_schema_ollama_cloud_catalog_minimax_rejected
        ; Alcotest.test_case
            "ollama cloud catalog rejects model without SO"
            `Quick
            test_validate_output_schema_ollama_cloud_catalog_rejects_model_without_so
        ; Alcotest.test_case
            "native ollama catalog rejects model without SO"
            `Quick
            test_validate_output_schema_native_ollama_catalog_rejects_model_without_so
        ; Alcotest.test_case
            "unknown openai compat host rejected"
            `Quick
            test_validate_output_schema_unknown_openai_compat_host_rejected
        ; Alcotest.test_case
            "raw compat qwen does not inherit bare capability"
            `Quick
            test_openai_compat_raw_qwen_does_not_inherit_bare_capability
        ; Alcotest.test_case
            "raw compat minimax does not inherit reasoning dialect"
            `Quick
            test_openai_compat_raw_minimax_does_not_inherit_bare_reasoning_dialect
        ; Alcotest.test_case
            "raw compat tool capability requires endpoint declaration"
            `Quick
            test_openai_compat_raw_tool_capability_requires_endpoint_declaration
        ; Alcotest.test_case
            "raw compat template dialect requires endpoint declaration"
            `Quick
            test_openai_compat_raw_template_dialect_requires_endpoint_declaration
        ; Alcotest.test_case
            "explicit provider-qualified model id resolves catalog row"
            `Quick
            test_openai_compat_explicit_provider_qualified_model_id_resolves_catalog_row
        ; Alcotest.test_case
            "bare model id does not resolve provider-qualified row"
            `Quick
            test_openai_compat_bare_model_id_does_not_resolve_provider_qualified_row
        ; Alcotest.test_case
            "responses structured path accepted"
            `Quick
            test_validate_responses_request_path_allows_structured_output
        ; Alcotest.test_case
            "responses json mode path accepted"
            `Quick
            test_validate_responses_request_path_allows_json_mode
        ; Alcotest.test_case
            "kimi k2.7 forced tool_choice rejected"
            `Quick
            test_validate_kimi_k27_rejects_forced_tool_choice
        ; Alcotest.test_case
            "anthropic thinking forced tool_choice rejected"
            `Quick
            test_validate_anthropic_thinking_rejects_forced_tool_choice
        ] )
    ; ( "locality"
      , [ Alcotest.test_case "loopback ip" `Quick test_is_local_loopback_ip
        ; Alcotest.test_case "localhost" `Quick test_is_local_localhost
        ; Alcotest.test_case "remote false" `Quick test_is_local_remote_false
        ; Alcotest.test_case
            "host boundary false"
            `Quick
            test_is_local_host_boundary_false
        ; Alcotest.test_case
            "localhost query true"
            `Quick
            test_is_local_localhost_query_true
        ; Alcotest.test_case
            "default attempt timeout hints"
            `Quick
            test_default_attempt_timeout_s
        ; Alcotest.test_case
            "cli sampling params"
            `Quick
            test_validate_cli_sampling_params_accepts_anthropic_min_p
        ; Alcotest.test_case
            "turn hard caps and clamp"
            `Quick
            test_max_turns_hard_cap_and_clamp
        ; Alcotest.test_case
            "reasoning effort typed roundtrip"
            `Quick
            test_reasoning_effort_typed_roundtrip
        ; Alcotest.test_case
            "reasoning effort typed config value"
            `Quick
            test_reasoning_effort_typed_config_value
        ; Alcotest.test_case
            "thinking effort thresholds"
            `Quick
            test_reasoning_effort_of_thinking_config
        ; Alcotest.test_case
            "thinking effort top-tier thresholds"
            `Quick
            test_reasoning_effort_top_tier_budget_mapping
        ; Alcotest.test_case
            "reasoning effort by config"
            `Quick
            test_reasoning_effort_of_config
        ; Alcotest.test_case
            "reasoning effort request value"
            `Quick
            test_reasoning_effort_request_value
        ; Alcotest.test_case
            "reasoning effort accepted subset"
            `Quick
            test_validate_reasoning_effort_subset_rejects_unsupported
        ; Alcotest.test_case
            "zai glm clear_thinking request field"
            `Quick
            test_zai_glm_clear_thinking_request_field
        ; Alcotest.test_case
            "structured output names"
            `Quick
            test_structured_output_name_of_schema
        ] )
    ; ( "provider_name"
      , [ Alcotest.test_case "glm general" `Quick test_provider_name_of_config_glm_general
        ; Alcotest.test_case "glm coding" `Quick test_provider_name_of_config_glm_coding
        ; Alcotest.test_case
            "local openai compat"
            `Quick
            test_provider_name_of_config_local_openai_compat
        ; Alcotest.test_case "openrouter" `Quick test_provider_name_of_config_openrouter
        ; Alcotest.test_case
            "deepseek vendor host label (exact Uri.host, RFC-OAS-034)"
            `Quick
            test_capability_provider_label_deepseek_exact_host
        ; Alcotest.test_case
            "unmatched openai_compat"
            `Quick
            test_provider_name_of_config_unmatched_openai_compat
        ; Alcotest.test_case
            "ignores xai catalog model"
            `Quick
            test_provider_name_of_config_ignores_xai_catalog_model
        ; Alcotest.test_case
            "ignores mistral catalog model"
            `Quick
            test_provider_name_of_config_ignores_mistral_catalog_model
        ; Alcotest.test_case
            "ignores cohere catalog model"
            `Quick
            test_provider_name_of_config_ignores_cohere_catalog_model
        ; Alcotest.test_case
            "ignores mimo catalog model"
            `Quick
            test_provider_name_of_config_ignores_mimo_catalog_model
        ; Alcotest.test_case
            "xai registered endpoint"
            `Quick
            test_provider_name_of_config_xai_registered_endpoint
        ; Alcotest.test_case
            "mistral registered endpoint"
            `Quick
            test_provider_name_of_config_mistral_registered_endpoint
        ; Alcotest.test_case
            "cohere registered endpoint"
            `Quick
            test_provider_name_of_config_cohere_registered_endpoint
        ; Alcotest.test_case
            "mimo registered endpoint"
            `Quick
            test_provider_name_of_config_mimo_registered_endpoint
        ] )
    ; ( "kind_of_string"
      , [ Alcotest.test_case "roundtrip all variants" `Quick test_kind_roundtrip
        ; Alcotest.test_case "aliases rejected" `Quick test_kind_aliases_rejected
        ; Alcotest.test_case "case insensitive" `Quick test_kind_case_insensitive
        ; Alcotest.test_case "whitespace trimmed" `Quick test_kind_whitespace
        ; Alcotest.test_case "unknown returns None" `Quick test_kind_unknown_returns_none
        ] )
    ; ( "kind_serializers"
      , [ Alcotest.test_case "show matches string_of" `Quick test_show_matches_string_of
        ; Alcotest.test_case "pp uses lowercase" `Quick test_pp_uses_lowercase
        ; Alcotest.test_case "to_yojson roundtrip" `Quick test_to_yojson_roundtrip
        ; Alcotest.test_case "of_yojson canonical" `Quick test_of_yojson_accepts_canonical
        ; Alcotest.test_case
            "of_yojson aliases rejected"
            `Quick
            test_of_yojson_rejects_aliases
        ; Alcotest.test_case
            "of_yojson unknown rejected"
            `Quick
            test_of_yojson_rejects_unknown_string
        ; Alcotest.test_case
            "of_yojson non-string rejected"
            `Quick
            test_of_yojson_rejects_non_string
        ] )
    ; ( "kind_enumeration"
      , [ Alcotest.test_case
            "all_provider_kinds is exhaustive"
            `Quick
            test_all_is_exhaustive
        ; Alcotest.test_case
            "all drives parse roundtrip"
            `Quick
            test_all_drives_parse_roundtrip
        ; Alcotest.test_case
            "default_api_key_env known"
            `Quick
            test_default_api_key_env_known
        ; Alcotest.test_case
            "default_api_key_env None for others"
            `Quick
            test_default_api_key_env_none_for_others
        ] )
    ; ( "telemetry_wire_format"
      , [ Alcotest.test_case
            "kind emitted as lowercase canonical string"
            `Quick
            test_wire_kind_lowercase
        ; Alcotest.test_case
            "None kind stays absent / no kind leaks"
            `Quick
            test_wire_kind_none_roundtrip
        ; Alcotest.test_case
            "unknown latency is encoded as null"
            `Quick
            test_wire_unknown_latency_is_null
        ; Alcotest.test_case
            "measured zero latency remains zero"
            `Quick
            test_wire_measured_zero_latency_is_distinct
        ; Alcotest.test_case
            "record JSON roundtrip preserves variant"
            `Quick
            test_wire_kind_roundtrip_via_yojson
        ] )
    ; ( "capability_provider_label"
      , [ Alcotest.test_case
            "ollama cloud matched by exact host, lookalikes rejected"
            `Quick
            test_capability_provider_label_ollama_cloud_exact_host
        ] )
    ]
;;
