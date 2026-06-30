(** Test Provider.resolve returns Error when env var is missing
    and Ok when present. *)

open Agent_sdk

let with_env name value f =
  let previous = Sys.getenv_opt name in
  (match value with
   | Some v -> Unix.putenv name v
   | None -> Unix.putenv name "");
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f
;;

let check_no_header label name headers =
  Alcotest.(check (option string)) label None (List.assoc_opt name headers)
;;

let check_auth_headers label expected (pc : Llm_provider.Provider_config.t) =
  Alcotest.(check (list (pair string string)))
    label
    expected
    (Provider.auth_headers_only_for_kind
       ~kind:pc.kind
       ~api_key:((pc.api_key :> string) :> string))
;;

let with_provider_catalog json f =
  match Llm_provider.Provider_catalog.of_json (Yojson.Safe.from_string json) with
  | Error msg -> Alcotest.fail msg
  | Ok catalog ->
    Llm_provider.Provider_catalog.set_global catalog;
    Fun.protect ~finally:Llm_provider.Provider_catalog.clear_global f
;;

let install_repo_model_catalog () =
  Model_catalog_test_support.install_repo_model_catalog ~suite:"provider"
;;

let with_empty_capability_sources f =
  let original_catalog = Llm_provider.Model_catalog.global () in
  let original_manifest = Llm_provider.Capability_manifest.global () in
  Llm_provider.Model_catalog.set_global [];
  Llm_provider.Capability_manifest.set_global [];
  Fun.protect
    ~finally:(fun () ->
      (match original_catalog with
       | Some catalog -> Llm_provider.Model_catalog.set_global catalog
       | None -> Llm_provider.Model_catalog.clear_global ());
      match original_manifest with
      | Some manifest -> Llm_provider.Capability_manifest.set_global manifest
      | None -> Llm_provider.Capability_manifest.clear_global ())
    f
;;

let test_missing_env_var () =
  (* Anthropic provider checks env var; nonexistent key -> Error *)
  let cfg : Provider.config =
    { provider = Anthropic
    ; model_id = "test-model"
    ; api_key_env = "AGENT_SDK_TEST_NONEXISTENT_KEY_39f7b2"
    }
  in
  match Provider.resolve cfg with
  | Error (Error.Config (MissingEnvVar { var_name })) ->
    Alcotest.(check string)
      "error mentions env var"
      "AGENT_SDK_TEST_NONEXISTENT_KEY_39f7b2"
      var_name
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error variant: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "should fail when env var is missing"
;;

let test_present_env_var () =
  (* Anthropic provider resolves env var to api_key *)
  let env_var = "AGENT_SDK_TEST_KEY_PRESENT_a1b2c3" in
  Unix.putenv env_var "test-api-key-value";
  let cfg : Provider.config =
    { provider = Anthropic; model_id = "test-model"; api_key_env = env_var }
  in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, _headers) ->
    Alcotest.(check string) "base_url" "https://api.anthropic.com" base_url;
    Alcotest.(check string) "api_key" "test-api-key-value" api_key
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed but got: %s" (Error.to_string e))
;;

let test_local_skips_env_var () =
  (* Local provider always succeeds without env var lookup *)
  let cfg : Provider.config =
    { provider = Local { base_url = "http://localhost:9999" }
    ; model_id = "test-model"
    ; api_key_env = "DOES_NOT_EXIST"
    }
  in
  match Provider.resolve cfg with
  | Ok (base_url, _api_key, _headers) ->
    Alcotest.(check string) "base_url" "http://localhost:9999" base_url
  | Error e ->
    Alcotest.fail (Printf.sprintf "Local should always succeed: %s" (Error.to_string e))
;;

let test_anthropic_provider () =
  let env_var = "AGENT_SDK_TEST_PROVIDER_A_KEY_x9y8z7" in
  Unix.putenv env_var "sk-ant-test-key";
  let cfg : Provider.config =
    { provider = Anthropic; model_id = "claude-sonnet-4-20250514"; api_key_env = env_var }
  in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, _headers) ->
    Alcotest.(check string) "anthropic base_url" "https://api.anthropic.com" base_url;
    Alcotest.(check string) "api_key" "sk-ant-test-key" api_key
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed but got: %s" (Error.to_string e))
;;

let test_openai_compat_resolve_success () =
  let env_var = "AGENT_SDK_TEST_OPENROUTER_KEY_q1w2e3" in
  Unix.putenv env_var "or-test-key";
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "https://openrouter.ai/api/v1"
          ; auth_header = Some "Authorization"
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "anthropic/claude-sonnet-4-6"
    ; api_key_env = env_var
    }
  in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, headers) ->
    Alcotest.(check string) "base_url" "https://openrouter.ai/api/v1" base_url;
    Alcotest.(check string) "api_key" "or-test-key" api_key;
    check_no_header "auth omitted from resolve headers" "Authorization" headers;
    Alcotest.(check (list (pair string string)))
      "auth header derived"
      [ "Authorization", "Bearer or-test-key" ]
      (Provider.auth_headers_only_for_kind
         ~kind:Llm_provider.Provider_config.OpenAI_compat
         ~api_key)
  | Error e -> Alcotest.fail (Printf.sprintf "should succeed: %s" (Error.to_string e))
;;

let test_openai_compat_resolve_missing_key () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "https://example.com"
          ; auth_header = Some "Authorization"
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "test"
    ; api_key_env = "AGENT_SDK_TEST_NONEXISTENT_COMPAT_KEY_z0z0"
    }
  in
  match Provider.resolve cfg with
  | Ok (_base_url, api_key, headers) ->
    Alcotest.(check string) "empty key" "" api_key;
    Alcotest.(check (list (pair string string)))
      "non-auth headers"
      [ "Content-Type", "application/json" ]
      headers
  | Error e ->
    Alcotest.fail
      (Printf.sprintf "missing OpenAI-compatible key is allowed: %s" (Error.to_string e))
;;

let test_anthropic_headers () =
  let env_var = "AGENT_SDK_TEST_HDR_KEY_h3h3" in
  Unix.putenv env_var "sk-ant-hdr-test";
  let cfg : Provider.config =
    { provider = Anthropic; model_id = "test"; api_key_env = env_var }
  in
  match Provider.resolve cfg with
  | Ok (_, api_key, headers) ->
    check_no_header "x-api-key omitted from resolve headers" "x-api-key" headers;
    Alcotest.(check (list (pair string string)))
      "auth header derived"
      [ "x-api-key", "sk-ant-hdr-test" ]
      (Provider.auth_headers_only_for_kind
         ~kind:Llm_provider.Provider_config.Anthropic
         ~api_key);
    let version = List.assoc "anthropic-version" headers in
    Alcotest.(check string) "anthropic-version" "2023-06-01" version;
    let ct = List.assoc "Content-Type" headers in
    Alcotest.(check string) "content-type" "application/json" ct
  | Error e -> Alcotest.fail (Printf.sprintf "should succeed: %s" (Error.to_string e))
;;

let test_model_spec_local_llm_capabilities () =
  let local_llm : Provider.config =
    { provider = Local { base_url = "http://127.0.0.1:8085" }
    ; model_id = "default"
    ; api_key_env = "DUMMY_KEY"
    }
  in
  let spec = Provider.model_spec_of_config local_llm in
  Alcotest.(check string) "request path" "/v1/chat/completions" spec.request_path;
  Alcotest.(check string)
    "request kind"
    "Openai_chat_completions"
    (match spec.request_kind with
     | Provider.Openai_chat_completions -> "Openai_chat_completions"
     | Provider.Anthropic_messages -> "Anthropic_messages"
     | Provider.Custom n -> "Custom:" ^ n);
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools
;;

let test_model_spec_openrouter_capabilities () =
  let cfg = Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" () in
  let spec = Provider.model_spec_of_config cfg in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check string) "request path" "/chat/completions" spec.request_path;
  Alcotest.(check string)
    "contract modality"
    "multimodal"
    (Provider.modality_to_string contract.modality);
  Alcotest.(check (option string)) "contract task" None contract.task;
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools;
  Alcotest.(check bool) "supports reasoning" false spec.capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" false spec.capabilities.supports_top_k;
  Alcotest.(check bool)
    "supports json response"
    true
    spec.capabilities.supports_response_format_json
;;

let test_inference_contract_anthropic_multimodal () =
  let contract = Provider.inference_contract_of_config (Provider.anthropic_sonnet ()) in
  Alcotest.(check string)
    "modality"
    "multimodal"
    (Provider.modality_to_string contract.modality)
;;

let test_inference_contract_task_transcription () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "https://api.openai.com/v1"
          ; auth_header = Some "Authorization"
          ; path = "/audio/transcriptions"
          ; static_token = None
          }
    ; model_id = "whisper-1"
    ; api_key_env = "OPENAI_API_KEY"
    }
  in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check (option string)) "task" (Some "transcription") contract.task
;;

let test_inference_contract_task_image_generation () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = Llm_provider.Zai_catalog.general_base_url
          ; auth_header = None
          ; path = "/images/generations"
          ; static_token = None
          }
    ; model_id = "glm-image"
    ; api_key_env = ""
    }
  in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check (option string)) "task" (Some "image_generation") contract.task
;;

let test_inference_contract_task_video_generation () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = Llm_provider.Zai_catalog.general_base_url
          ; auth_header = None
          ; path = "/videos/generations"
          ; static_token = None
          }
    ; model_id = "cogvideox-2"
    ; api_key_env = ""
    }
  in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check (option string)) "task" (Some "video_generation") contract.task
;;

let test_zai_glm5v_capabilities_include_image_input () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = Llm_provider.Zai_catalog.general_base_url
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "glm-5v-turbo"
    ; api_key_env = ""
    }
  in
  let capabilities = Provider.capabilities_for_config cfg in
  Alcotest.(check bool) "supports image input" true capabilities.supports_image_input;
  Alcotest.(check bool)
    "supports multimodal inputs"
    true
    capabilities.supports_multimodal_inputs
;;

let test_non_zai_glm_capabilities_stay_openai_compat () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "https://openrouter.ai/api/v1"
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "glm-5"
    ; api_key_env = ""
    }
  in
  let capabilities = Provider.capabilities_for_config cfg in
  Alcotest.(check bool) "reasoning disabled" false capabilities.supports_reasoning;
  Alcotest.(check bool)
    "extended thinking disabled"
    false
    capabilities.supports_extended_thinking
;;

let test_validate_inference_contract_rejects_unsupported_modality () =
  let cfg : Provider.config =
    { provider = Custom_registered { name = "text-only" }
    ; model_id = "text-only-model"
    ; api_key_env = ""
    }
  in
  let contract : Provider.inference_contract =
    { provider = cfg.provider
    ; model_id = cfg.model_id
    ; modality = Provider.Image
    ; task = None
    }
  in
  match
    Provider.validate_inference_contract
      ~capabilities:(Provider.capabilities_for_config cfg)
      contract
  with
  | Error (Error.Config (InvalidConfig { field; detail })) ->
    Alcotest.(check string) "field" "modality" field;
    Alcotest.(check string)
      "detail"
      "Model 'text-only-model' for provider 'custom:text-only' does not support modality \
       'image'"
      detail
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error variant: %s" (Error.to_string e))
  | Ok () -> Alcotest.fail "expected unsupported modality validation to fail"
;;

let test_raw_openai_compat_does_not_infer_extended_capabilities () =
  let capabilities =
    Provider.capabilities_for_model
      ~provider:
        (Provider.OpenAICompat
           { base_url = "http://localhost:8080"
           ; auth_header = None
           ; path = "/chat/completions"
           ; static_token = None
           })
      ~model_id:"dashscope-3.5-35b-a3b-ud-q8-xl"
  in
  Alcotest.(check bool) "supports reasoning" false capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" false capabilities.supports_top_k;
  Alcotest.(check bool) "supports min_p" false capabilities.supports_min_p
;;

let test_raw_openai_compat_does_not_infer_dashscope_from_model_id () =
  with_empty_capability_sources (fun () ->
    let capabilities =
      Provider.capabilities_for_model
        ~provider:
          (Provider.OpenAICompat
             { base_url = "https://compat.example.invalid/v1"
             ; auth_header = None
             ; path = "/chat/completions"
             ; static_token = None
             })
        ~model_id:"dashscope-compatible-unknown"
    in
    Alcotest.(check bool) "supports reasoning" false capabilities.supports_reasoning;
    Alcotest.(check bool) "supports top_k" false capabilities.supports_top_k;
    Alcotest.(check bool) "supports min_p" false capabilities.supports_min_p)
;;

let test_raw_openai_compat_does_not_infer_minimax_from_model_id () =
  let capabilities =
    Provider.capabilities_for_model
      ~provider:
        (Provider.OpenAICompat
           { base_url = "https://compat.example.invalid/v1"
           ; auth_header = None
           ; path = "/chat/completions"
           ; static_token = None
           })
      ~model_id:"minimax-m3"
  in
  Alcotest.(check bool) "supports reasoning" false capabilities.supports_reasoning;
  Alcotest.(check bool)
    "supports extended thinking"
    false
    capabilities.supports_extended_thinking;
  Alcotest.(check bool)
    "supports reasoning budget"
    false
    capabilities.supports_reasoning_budget
;;

let test_local_compat_does_not_infer_dialect_from_model_id () =
  let capabilities =
    Provider.capabilities_for_model
      ~provider:(Provider.Local { base_url = "http://127.0.0.1:8085" })
      ~model_id:"dashscope-3.5-35b"
  in
  Alcotest.(check bool) "supports reasoning" false capabilities.supports_reasoning;
  Alcotest.(check bool)
    "supports extended thinking"
    false
    capabilities.supports_extended_thinking;
  Alcotest.(check bool)
    "supports reasoning budget"
    false
    capabilities.supports_reasoning_budget;
  Alcotest.(check bool)
    "no thinking control"
    true
    (capabilities.thinking_control_format = Llm_provider.Capabilities.No_thinking_control);
  Alcotest.(check bool) "supports top_k" false capabilities.supports_top_k;
  Alcotest.(check bool) "supports min_p" false capabilities.supports_min_p
;;

let test_anthropic_capabilities_consults_for_model_id () =
  (* Regression for #824: the Anthropic branch of capabilities_for_model
     was returning the base anthropic_capabilities (200K window)
     regardless of model_id, bypassing the per-model overrides in
     Llm_provider.Capabilities.for_model_id. Opus 4 / Sonnet 4 advertise
     a 1M window in that table; this test pins that the config path
     now picks them up. *)
  let opus = Provider.anthropic_opus () in
  let sonnet = Provider.anthropic_sonnet () in
  let haiku = Provider.anthropic_haiku () in
  let opus_caps = Provider.capabilities_for_config opus in
  let sonnet_caps = Provider.capabilities_for_config sonnet in
  let haiku_caps = Provider.capabilities_for_config haiku in
  Alcotest.(check (option int))
    "opus-4-6 context = 1M"
    (Some 1_000_000)
    opus_caps.max_context_tokens;
  Alcotest.(check (option int))
    "opus-4-6 output = 128K"
    (Some 128_000)
    opus_caps.max_output_tokens;
  Alcotest.(check (option int))
    "sonnet-4-6 context = 1M"
    (Some 1_000_000)
    sonnet_caps.max_context_tokens;
  Alcotest.(check (option int))
    "sonnet-4-6 output = 64K"
    (Some 64_000)
    sonnet_caps.max_output_tokens;
  (* haiku-4 explicitly stays at 200K in for_model_id *)
  Alcotest.(check (option int))
    "haiku-4-5 context = 200K"
    (Some 200_000)
    haiku_caps.max_context_tokens
;;

let test_anthropic_capabilities_unknown_model_id_falls_back () =
  (* Unknown Anthropic model_ids still fall back to the conservative
     base anthropic_capabilities rather than failing hard. *)
  let cfg : Provider.config =
    { provider = Anthropic
    ; model_id = "claude-nonexistent-future-model"
    ; api_key_env = "ANTHROPIC_API_KEY"
    }
  in
  let caps = Provider.capabilities_for_config cfg in
  (* Base anthropic_capabilities has max_context_tokens = Some 200_000 *)
  Alcotest.(check (option int))
    "unknown anthropic model falls back to base 200K"
    (Some 200_000)
    caps.max_context_tokens
;;

(* ── Phase 6: pricing, ollama, static_token ─────────────────────── *)

let test_pricing_sonnet () =
  let p = Provider.pricing_for_model "claude-sonnet-4-6-20250514" in
  Alcotest.(check (float 0.001)) "input/M" 3.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "output/M" 15.0 p.output_per_million;
  Alcotest.(check (float 0.001)) "cache_write" 1.25 p.cache_write_multiplier;
  Alcotest.(check (float 0.001)) "cache_read" 0.1 p.cache_read_multiplier
;;

let test_pricing_gpt55 () =
  let p = Provider.pricing_for_model "gpt-5.5" in
  Alcotest.(check (float 0.001)) "input/M" 5.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "output/M" 30.0 p.output_per_million;
  Alcotest.(check (float 0.001)) "cache_write" 1.0 p.cache_write_multiplier;
  Alcotest.(check (float 0.001)) "cache_read" 0.1 p.cache_read_multiplier
;;

let test_pricing_local () =
  let p =
    Provider.pricing_for_provider
      ~provider:(Local { base_url = "http://127.0.0.1:8085" })
      ~model_id:"dashscope-3.5-35b-a3b"
  in
  Alcotest.(check (float 0.001)) "free" 0.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "free output" 0.0 p.output_per_million
;;

let test_pricing_unknown () =
  let p = Provider.pricing_for_model "future-model-xyz" in
  Alcotest.(check (float 0.001)) "zero" 0.0 p.input_per_million
;;

let test_estimate_cost () =
  let p = Provider.pricing_for_model "claude-sonnet-4-6" in
  let cost =
    Provider.estimate_cost
      ~pricing:p
      ~input_tokens:1_000_000
      ~output_tokens:500_000
      ~cache_creation_input_tokens:100_000
      ~cache_read_input_tokens:200_000
      ()
  in
  Alcotest.(check bool) "cost > 0" true (cost > 0.0)
;;

let test_config_of_provider_config_localhost_boundary () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"test-model"
      ~base_url:"http://localhostevil.com:8080"
      ()
  in
  match Provider.config_of_provider_config cfg with
  | { provider = Provider.OpenAICompat _; _ } -> ()
  | { provider = Provider.Local _; _ } ->
    Alcotest.fail "localhostevil.com must not be treated as local"
  | _ -> Alcotest.fail "unexpected provider kind"
;;

let test_config_of_provider_config_local_ollama_delegates_to_ssot () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.Ollama
      ~model_id:"test-model"
      ~base_url:"http://localhost:11434"
      ()
  in
  match Provider.config_of_provider_config cfg with
  | { provider = Provider.Local { base_url }; _ } ->
    Alcotest.(check string) "base_url" "http://localhost:11434" base_url
  | _ -> Alcotest.fail "expected localhost ollama config to resolve as local"
;;

let test_config_of_provider_config_uppercase_localhost_delegates_to_ssot () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"test-model"
      ~base_url:"  HTTP://LOCALHOST:11434/v1  "
      ()
  in
  match Provider.config_of_provider_config cfg with
  | { provider = Provider.Local { base_url }; _ } ->
    Alcotest.(check string) "base_url preserved" "  HTTP://LOCALHOST:11434/v1  " base_url
  | _ -> Alcotest.fail "expected uppercase localhost config to resolve as local"
;;

let test_config_of_provider_config_localhost_query_delegates_to_ssot () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"test-model"
      ~base_url:"http://localhost?foo=bar"
      ()
  in
  match Provider.config_of_provider_config cfg with
  | { provider = Provider.Local { base_url }; _ } ->
    Alcotest.(check string) "base_url preserved" "http://localhost?foo=bar" base_url
  | _ -> Alcotest.fail "expected localhost query config to resolve as local"
;;

let test_config_of_provider_config_kimi_uses_custom_provider () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.Kimi
      ~model_id:"kimi-for-coding"
      ~base_url:"https://api.kimi.com/coding"
      ()
  in
  match Provider.config_of_provider_config cfg with
  | { provider = Provider.Custom_registered { name }; api_key_env; _ } ->
    Alcotest.(check string) "provider name" "kimi" name;
    Alcotest.(check string) "api_key_env" "KIMI_API_KEY" api_key_env
  | _ -> Alcotest.fail "expected kimi config to round-trip through Custom_registered"
;;

let test_openai_compat_static_token () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "http://localhost:8080"
          ; auth_header = Some "Authorization"
          ; path = "/v1/chat/completions"
          ; static_token = Some "static-key-123"
          }
    ; model_id = "test"
    ; api_key_env = "NONEXISTENT"
    }
  in
  match Provider.resolve cfg with
  | Ok (_, key, headers) ->
    Alcotest.(check string) "static key" "static-key-123" key;
    check_no_header "auth omitted from resolve headers" "Authorization" headers;
    Alcotest.(check (list (pair string string)))
      "auth header derived"
      [ "Authorization", "Bearer static-key-123" ]
      (Provider.auth_headers_only_for_kind
         ~kind:Llm_provider.Provider_config.OpenAI_compat
         ~api_key:key)
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_openai_compat_no_auth () =
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "http://localhost:8080"
          ; auth_header = None
          ; path = "/v1/chat/completions"
          ; static_token = None
          }
    ; model_id = "test"
    ; api_key_env = "NONEXISTENT"
    }
  in
  match Provider.resolve cfg with
  | Ok (_, key, _) -> Alcotest.(check string) "empty key" "" key
  | Error e -> Alcotest.fail (Error.to_string e)
;;

(** Forward adapter — Provider.config + agent_state → Provider_config.t *)

let agent_state_with_params () : Types.agent_state =
  let cfg =
    { Types.default_config with
      model = "claude-test"
    ; max_tokens = Some 4096
    ; temperature = Some 0.7
    ; top_p = Some 0.9
    ; top_k = Some 40
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; cache_system_prompt = true
    ; system_prompt = Some "you are a tester"
    }
  in
  { config = cfg; usage = Types.empty_usage; turn_count = 0; messages = [] }
;;

let test_provider_config_of_agent_anthropic () =
  let env_var = "AGENT_SDK_TEST_ADAPTER_KEY_anth" in
  Unix.putenv env_var "sk-ant-adapter-test";
  let cfg : Provider.config =
    { provider = Anthropic; model_id = "claude-sonnet-4-20250514"; api_key_env = env_var }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent
      ~state
      ~base_url:"https://api.anthropic.com"
      (Some cfg)
  with
  | Ok pc ->
    Alcotest.(check string)
      "kind"
      "anthropic"
      (Llm_provider.Provider_config.string_of_provider_kind pc.kind);
    Alcotest.(check string) "model_id" "claude-sonnet-4-20250514" pc.model_id;
    Alcotest.(check string) "api_key" "sk-ant-adapter-test" (pc.api_key :> string);
    Alcotest.(check string) "request_path" "/v1/messages" pc.request_path;
    check_no_header "x-api-key omitted from config headers" "x-api-key" pc.headers;
    check_auth_headers
      "x-api-key auth header derived"
      [ "x-api-key", "sk-ant-adapter-test" ]
      pc;
    Alcotest.(check bool)
      "anthropic-version header present"
      true
      (List.mem ("anthropic-version", "2023-06-01") pc.headers);
    Alcotest.(check (option int)) "max_tokens" (Some 4096) pc.max_tokens;
    Alcotest.(check (option (float 0.001))) "temperature" (Some 0.7) pc.temperature;
    Alcotest.(check (option (float 0.001))) "top_p" (Some 0.9) pc.top_p;
    Alcotest.(check (option int)) "top_k" (Some 40) pc.top_k;
    Alcotest.(check bool) "cache_system_prompt" true pc.cache_system_prompt;
    Alcotest.(check (option string))
      "system_prompt"
      (Some "you are a tester")
      pc.system_prompt
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_provider_config_of_agent_openai_compat_collapses () =
  (* OpenAICompat collapses to OpenAI_compat (lossy — documented limitation) *)
  let env_var = "AGENT_SDK_TEST_ADAPTER_KEY_oai" in
  Unix.putenv env_var "sk-oai-adapter-test";
  let cfg : Provider.config =
    { provider =
        OpenAICompat
          { base_url = "https://generativelanguage.googleapis.com/v1beta/openai"
          ; auth_header = Some "Authorization"
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "gemini-2.5-flash"
    ; api_key_env = env_var
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Ok pc ->
    Alcotest.(check string)
      "kind collapses to openai_compat"
      "openai_compat"
      (Llm_provider.Provider_config.string_of_provider_kind pc.kind);
    Alcotest.(check string)
      "base_url from resolve"
      "https://generativelanguage.googleapis.com/v1beta/openai"
      pc.base_url;
    Alcotest.(check string) "request_path preserved" "/chat/completions" pc.request_path;
    check_no_header "authorization omitted from config headers" "Authorization" pc.headers;
    check_auth_headers
      "authorization auth header derived"
      [ "Authorization", "Bearer sk-oai-adapter-test" ]
      pc;
    Alcotest.(check string) "model_id" "gemini-2.5-flash" pc.model_id
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_provider_config_of_agent_missing_env () =
  let cfg : Provider.config =
    { provider = Anthropic
    ; model_id = "claude-test"
    ; api_key_env = "AGENT_SDK_TEST_ADAPTER_NONEXISTENT_zzz"
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent
      ~state
      ~base_url:"https://api.anthropic.com"
      (Some cfg)
  with
  | Error (Error.Config (MissingEnvVar { var_name })) ->
    Alcotest.(check string)
      "propagates env var name"
      "AGENT_SDK_TEST_ADAPTER_NONEXISTENT_zzz"
      var_name
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "should fail when env var missing"
;;

let test_provider_config_of_agent_none_fallback () =
  (* None provider with ANTHROPIC_API_KEY present = Anthropic default *)
  Unix.putenv "ANTHROPIC_API_KEY" "sk-ant-default-fallback";
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"https://api.anthropic.com" None
  with
  | Ok pc ->
    Alcotest.(check string)
      "defaults to anthropic"
      "anthropic"
      (Llm_provider.Provider_config.string_of_provider_kind pc.kind);
    Alcotest.(check string)
      "uses fallback key"
      "sk-ant-default-fallback"
      (pc.api_key :> string);
    Alcotest.(check string)
      "preserves caller base_url"
      "https://api.anthropic.com"
      pc.base_url;
    Alcotest.(check string) "request_path" "/v1/messages" pc.request_path;
    check_no_header "x-api-key omitted from config headers" "x-api-key" pc.headers;
    check_auth_headers
      "x-api-key auth header derived"
      [ "x-api-key", "sk-ant-default-fallback" ]
      pc
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_provider_config_of_agent_local_strips_dummy_key () =
  let cfg : Provider.config =
    { provider = Local { base_url = "http://127.0.0.1:11434" }
    ; model_id = "dashscope-3.5"
    ; api_key_env = "IGNORED"
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Ok pc ->
    Alcotest.(check string)
      "kind"
      "openai_compat"
      (Llm_provider.Provider_config.string_of_provider_kind pc.kind);
    Alcotest.(check string) "request_path" "/v1/chat/completions" pc.request_path;
    Alcotest.(check string) "local strips dummy api_key" "" (pc.api_key :> string);
    Alcotest.(check (list (pair string string)))
      "headers"
      [ "Content-Type", "application/json" ]
      pc.headers
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_provider_config_of_agent_custom_registered_preserves_kind () =
  (* Regression for #1003: Custom_registered must preserve the
     registry-declared provider_kind (e.g. Gemini) rather than
     flattening to OpenAI_compat, which would route Gemini requests
     through the Openai wire format and produce 404 against the
     Gemini base URL. *)
  let cfg : Provider.config =
    { provider = Custom_registered { name = "gemini" }
    ; model_id = "gemini-2.5-flash"
    ; api_key_env = "GEMINI_API_KEY"
    }
  in
  let state = agent_state_with_params () in
  Unix.putenv "GEMINI_API_KEY" "fake-gemini-key";
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Ok pc ->
    Alcotest.(check bool)
      "kind preserves Gemini"
      true
      (pc.kind = Llm_provider.Provider_config.Gemini);
    Alcotest.(check string) "model_id" "gemini-2.5-flash" pc.model_id
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
;;

let test_provider_config_of_agent_custom_registered_kimi_preserves_headers () =
  let env_var = "KIMI_PROVIDER_TEST_KEY" in
  Unix.putenv env_var "kimi-provider-test-key";
  let cfg : Provider.config =
    { provider = Custom_registered { name = "kimi" }
    ; model_id = "kimi-for-coding"
    ; api_key_env = env_var
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Ok pc ->
    Alcotest.(check bool)
      "kind preserves Kimi"
      true
      (pc.kind = Llm_provider.Provider_config.Kimi);
    Alcotest.(check string) "request_path" "/v1/messages" pc.request_path;
    check_no_header "x-api-key omitted from config headers" "x-api-key" pc.headers;
    check_auth_headers
      "x-api-key auth header derived"
      [ "x-api-key", "kimi-provider-test-key" ]
      pc;
    Alcotest.(check bool)
      "anthropic-version header present"
      true
      (List.mem ("anthropic-version", "2023-06-01") pc.headers)
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
;;

let test_provider_config_of_agent_custom_registered_nous_uses_calltime_default () =
  with_env "LLM_ENDPOINTS" (Some "") (fun () ->
    with_env
      Llm_provider.Discovery.local_llm_url_env_var
      (Some "http://127.0.0.1:19013")
      (fun () ->
         let cfg : Provider.config =
           { provider = Custom_registered { name = "nous" }
           ; model_id = "local-model"
           ; api_key_env = ""
           }
         in
         let state = agent_state_with_params () in
         match
           Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
         with
         | Ok pc ->
           Alcotest.(check string)
             "custom nous base_url"
             "http://127.0.0.1:19013"
             pc.base_url
         | Error e ->
           Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))))
;;

let test_provider_config_of_agent_custom_registered_ollama_cloud_headers () =
  with_env "OLLAMA_CLOUD_API_KEY" (Some "ollama-cloud-provider-test-key") (fun () ->
    with_env "OLLAMA_API_KEY" (Some "fallback-ollama-api-key") (fun () ->
      let cfg : Provider.config =
        { provider = Custom_registered { name = "ollama_cloud" }
        ; model_id = "glm-5.1:cloud"
        ; api_key_env = "OLLAMA_CLOUD_API_KEY"
        }
      in
      let state = agent_state_with_params () in
      match
        Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
      with
      | Ok pc ->
        Alcotest.(check bool)
          "kind preserves Ollama"
          true
          (pc.kind = Llm_provider.Provider_config.Ollama);
        Alcotest.(check string) "base_url" "https://ollama.com" pc.base_url;
        Alcotest.(check string) "request_path" "/api/chat" pc.request_path;
        Alcotest.(check string)
          "uses cloud-specific key first"
          "ollama-cloud-provider-test-key"
          (pc.api_key :> string);
        check_no_header
          "authorization omitted from config headers"
          "Authorization"
          pc.headers;
        check_auth_headers
          "authorization auth header derived"
          [ "Authorization", "Bearer ollama-cloud-provider-test-key" ]
          pc
      | Error e ->
        Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))))
;;

let test_provider_config_of_agent_custom_registered_ollama_cloud_api_key_fallback () =
  with_env "OLLAMA_CLOUD_API_KEY" None (fun () ->
    with_env "OLLAMA_API_KEY" (Some "ollama-api-fallback-key") (fun () ->
      let cfg : Provider.config =
        { provider = Custom_registered { name = "ollama_cloud" }
        ; model_id = "deepseek-v4-pro:cloud"
        ; api_key_env = "OLLAMA_CLOUD_API_KEY"
        }
      in
      let state = agent_state_with_params () in
      match
        Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
      with
      | Ok pc ->
        Alcotest.(check string)
          "uses OLLAMA_API_KEY fallback"
          "ollama-api-fallback-key"
          (pc.api_key :> string);
        check_no_header
          "authorization omitted from config headers"
          "Authorization"
          pc.headers;
        check_auth_headers
          "authorization auth header derived"
          [ "Authorization", "Bearer ollama-api-fallback-key" ]
          pc
      | Error e ->
        Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))))
;;

let test_provider_config_of_agent_catalog_structured_output_endpoint_declaration () =
  with_provider_catalog
    {|
{
  "schema_version": 1,
  "providers": [
    {
      "id": "runpod-qwen36",
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "https://ma8xbr1kgbclkl-64411be1.proxy.runpod.net/v1",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat",
      "capabilities": {
        "supports_structured_output": true,
        "supports_reasoning": true,
        "supports_extended_thinking": true,
        "supports_reasoning_budget": true,
        "thinking_control_format": "chat_template_kwargs",
        "preserve_thinking_control_format": "chat_template_kwargs_preserve_thinking"
      }
    }
  ]
}
|}
    (fun () ->
       let model_id = "qwen/qwen3.6-35b-a3b" in
       let schema = `Assoc [ "type", `String "object" ] in
       let state = agent_state_with_params () in
       let declared_cfg : Provider.config =
         { provider = Custom_registered { name = "runpod-qwen36" }
         ; model_id
         ; api_key_env = ""
         }
       in
       let declared_pc =
         match
           Provider.provider_config_of_agent
             ~state
             ~base_url:"unused-fallback"
             (Some declared_cfg)
         with
         | Ok pc ->
           { pc with
             Llm_provider.Provider_config.response_format = Types.JsonSchema schema
           ; output_schema = Some schema
           }
         | Error e ->
           Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
       in
       Alcotest.(check (option bool))
         "catalog endpoint declaration projected"
         (Some true)
         declared_pc.supports_structured_output_override;
       Alcotest.(check bool)
         "catalog capability declaration projected"
         true
         (Option.is_some declared_pc.model_capabilities_override);
       let declared_caps =
         Llm_provider.Provider_config.capabilities_for_config_model declared_pc
       in
       Alcotest.(check bool)
         "catalog capability uses chat_template_kwargs"
         true
         (match declared_caps with
          | Some caps ->
            caps.Llm_provider.Capabilities.thinking_control_format
            = Llm_provider.Capabilities.Chat_template_kwargs
            && caps.preserve_thinking_control_format
               = Llm_provider.Capabilities.Chat_template_kwargs_preserve_thinking
          | None -> false);
       Alcotest.(check bool)
         "catalog-declared endpoint validates schema output"
         true
         (Result.is_ok
            (Llm_provider.Provider_config.validate_output_schema_request declared_pc));
       let raw_cfg : Provider.config =
         { provider =
             OpenAICompat
               { base_url = "https://ma8xbr1kgbclkl-64411be1.proxy.runpod.net/v1"
               ; auth_header = None
               ; path = "/v1/chat/completions"
               ; static_token = None
               }
         ; model_id
         ; api_key_env = ""
         }
       in
       let raw_pc =
         match
           Provider.provider_config_of_agent
             ~state
             ~base_url:"unused-fallback"
             (Some raw_cfg)
         with
         | Ok pc ->
           { pc with
             Llm_provider.Provider_config.response_format = Types.JsonSchema schema
           ; output_schema = Some schema
           }
         | Error e ->
           Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
       in
       Alcotest.(check (option bool))
         "raw OpenAICompat does not invent endpoint declaration"
         None
         raw_pc.supports_structured_output_override;
       Alcotest.(check bool)
         "raw OpenAICompat has no endpoint capability override"
         true
         (Option.is_none raw_pc.model_capabilities_override);
       Alcotest.(check bool)
         "raw OpenAICompat does not inherit qwen capability fallback"
         true
         (Option.is_none
            (Llm_provider.Provider_config.capabilities_for_config_model raw_pc));
       Alcotest.(check bool)
         "raw OpenAICompat endpoint remains fail-closed"
         true
         (Result.is_error
            (Llm_provider.Provider_config.validate_output_schema_request raw_pc)))
;;

let test_provider_config_of_agent_custom_registered_unknown_name () =
  let cfg : Provider.config =
    { provider = Custom_registered { name = "nonexistent-provider-xyz" }
    ; model_id = "m"
    ; api_key_env = "IGNORED"
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Error (Error.Config (InvalidConfig { field; detail })) ->
    Alcotest.(check string) "field" "provider" field;
    Alcotest.(check bool)
      "detail mentions not found"
      true
      (Util.contains_substring_ci ~haystack:detail ~needle:"not found")
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "should error on unregistered name"
;;

let () =
  install_repo_model_catalog ();
  Alcotest.run
    "Provider"
    [ ( "resolve"
      , [ Alcotest.test_case "missing env var returns Error" `Quick test_missing_env_var
        ; Alcotest.test_case "present env var returns Ok" `Quick test_present_env_var
        ; Alcotest.test_case "local skips env var" `Quick test_local_skips_env_var
        ; Alcotest.test_case "anthropic provider" `Quick test_anthropic_provider
        ; Alcotest.test_case
            "openai compat success"
            `Quick
            test_openai_compat_resolve_success
        ; Alcotest.test_case
            "openai compat missing key"
            `Quick
            test_openai_compat_resolve_missing_key
        ; Alcotest.test_case "anthropic headers" `Quick test_anthropic_headers
        ; Alcotest.test_case
            "local llm model spec capabilities"
            `Quick
            test_model_spec_local_llm_capabilities
        ; Alcotest.test_case
            "openrouter model spec capabilities"
            `Quick
            test_model_spec_openrouter_capabilities
        ; Alcotest.test_case
            "inference contract anthropic multimodal"
            `Quick
            test_inference_contract_anthropic_multimodal
        ; Alcotest.test_case
            "task inference transcription"
            `Quick
            test_inference_contract_task_transcription
        ; Alcotest.test_case
            "task inference image generation"
            `Quick
            test_inference_contract_task_image_generation
        ; Alcotest.test_case
            "task inference video generation"
            `Quick
            test_inference_contract_task_video_generation
        ; Alcotest.test_case
            "zai glm-5v image capabilities"
            `Quick
            test_zai_glm5v_capabilities_include_image_input
        ; Alcotest.test_case
            "non-zai glm stays openai compat"
            `Quick
            test_non_zai_glm_capabilities_stay_openai_compat
        ; Alcotest.test_case
            "invalid modality gets actionable error"
            `Quick
            test_validate_inference_contract_rejects_unsupported_modality
        ; Alcotest.test_case
            "raw OpenAI-compatible does not infer extended capabilities"
            `Quick
            test_raw_openai_compat_does_not_infer_extended_capabilities
        ; Alcotest.test_case
            "raw openai_compat does not infer dashscope"
            `Quick
            test_raw_openai_compat_does_not_infer_dashscope_from_model_id
        ; Alcotest.test_case
            "raw openai_compat does not infer minimax"
            `Quick
            test_raw_openai_compat_does_not_infer_minimax_from_model_id
        ; Alcotest.test_case
            "local compat does not infer dialect"
            `Quick
            test_local_compat_does_not_infer_dialect_from_model_id
        ; Alcotest.test_case
            "anthropic consults for_model_id (#824)"
            `Quick
            test_anthropic_capabilities_consults_for_model_id
        ; Alcotest.test_case
            "anthropic unknown model falls back to base"
            `Quick
            test_anthropic_capabilities_unknown_model_id_falls_back
        ] )
    ; ( "pricing"
      , [ Alcotest.test_case "sonnet pricing" `Quick test_pricing_sonnet
        ; Alcotest.test_case "gpt-5.5 pricing" `Quick test_pricing_gpt55
        ; Alcotest.test_case "local free" `Quick test_pricing_local
        ; Alcotest.test_case "unknown model" `Quick test_pricing_unknown
        ; Alcotest.test_case "estimate cost" `Quick test_estimate_cost
        ; Alcotest.test_case
            "provider_config localhost boundary"
            `Quick
            test_config_of_provider_config_localhost_boundary
        ; Alcotest.test_case
            "provider_config local ollama"
            `Quick
            test_config_of_provider_config_local_ollama_delegates_to_ssot
        ; Alcotest.test_case
            "provider_config uppercase localhost"
            `Quick
            test_config_of_provider_config_uppercase_localhost_delegates_to_ssot
        ; Alcotest.test_case
            "provider_config localhost query"
            `Quick
            test_config_of_provider_config_localhost_query_delegates_to_ssot
        ; Alcotest.test_case
            "provider_config kimi custom"
            `Quick
            test_config_of_provider_config_kimi_uses_custom_provider
        ] )
    ; ( "openai_compat"
      , [ Alcotest.test_case "static token" `Quick test_openai_compat_static_token
        ; Alcotest.test_case "no auth" `Quick test_openai_compat_no_auth
        ] )
    ; ( "provider_config_of_agent"
      , [ Alcotest.test_case
            "anthropic maps fields"
            `Quick
            test_provider_config_of_agent_anthropic
        ; Alcotest.test_case
            "openai_compat kind collapses"
            `Quick
            test_provider_config_of_agent_openai_compat_collapses
        ; Alcotest.test_case
            "missing env propagates"
            `Quick
            test_provider_config_of_agent_missing_env
        ; Alcotest.test_case
            "none falls back to ANTHROPIC_API_KEY"
            `Quick
            test_provider_config_of_agent_none_fallback
        ; Alcotest.test_case
            "local strips dummy key"
            `Quick
            test_provider_config_of_agent_local_strips_dummy_key
        ; Alcotest.test_case
            "custom registered preserves kind (#1003)"
            `Quick
            test_provider_config_of_agent_custom_registered_preserves_kind
        ; Alcotest.test_case
            "custom registered kimi preserves headers"
            `Quick
            test_provider_config_of_agent_custom_registered_kimi_preserves_headers
        ; Alcotest.test_case
            "custom registered nous uses call-time default"
            `Quick
            test_provider_config_of_agent_custom_registered_nous_uses_calltime_default
        ; Alcotest.test_case
            "custom registered ollama_cloud adds auth header"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_headers
        ; Alcotest.test_case
            "custom registered ollama_cloud falls back to OLLAMA_API_KEY"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_api_key_fallback
        ; Alcotest.test_case
            "catalog structured output endpoint declaration"
            `Quick
            test_provider_config_of_agent_catalog_structured_output_endpoint_declaration
        ; Alcotest.test_case
            "custom registered unknown name errors"
            `Quick
            test_provider_config_of_agent_custom_registered_unknown_name
        ] )
    ]
;;
