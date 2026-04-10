(** Test Provider.resolve returns Error when env var is missing
    and Ok when present. *)

open Agent_sdk

let test_missing_env_var () =
  (* Anthropic provider checks env var; nonexistent key -> Error *)
  let cfg : Provider.config = {
    provider = Anthropic;
    model_id = "test-model";
    api_key_env = "AGENT_SDK_TEST_NONEXISTENT_KEY_39f7b2";
  } in
  match Provider.resolve cfg with
  | Error (Error.Config (MissingEnvVar { var_name })) ->
    Alcotest.(check string) "error mentions env var"
      "AGENT_SDK_TEST_NONEXISTENT_KEY_39f7b2" var_name
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error variant: %s" (Error.to_string e))
  | Ok _ ->
    Alcotest.fail "should fail when env var is missing"

let test_present_env_var () =
  (* Anthropic provider resolves env var to api_key *)
  let env_var = "AGENT_SDK_TEST_KEY_PRESENT_a1b2c3" in
  Unix.putenv env_var "test-api-key-value";
  let cfg : Provider.config = {
    provider = Anthropic;
    model_id = "test-model";
    api_key_env = env_var;
  } in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, _headers) ->
    Alcotest.(check string) "base_url" "https://api.anthropic.com" base_url;
    Alcotest.(check string) "api_key" "test-api-key-value" api_key
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed but got: %s" (Error.to_string e))

let test_local_skips_env_var () =
  (* Local provider always succeeds without env var lookup *)
  let cfg : Provider.config = {
    provider = Local { base_url = "http://localhost:9999" };
    model_id = "test-model";
    api_key_env = "DOES_NOT_EXIST";
  } in
  match Provider.resolve cfg with
  | Ok (base_url, _api_key, _headers) ->
    Alcotest.(check string) "base_url" "http://localhost:9999" base_url
  | Error e ->
    Alcotest.fail (Printf.sprintf "Local should always succeed: %s" (Error.to_string e))

let test_anthropic_provider () =
  let env_var = "AGENT_SDK_TEST_ANTHROPIC_KEY_x9y8z7" in
  Unix.putenv env_var "sk-ant-test-key";
  let cfg : Provider.config = {
    provider = Anthropic;
    model_id = "claude-sonnet-4-20250514";
    api_key_env = env_var;
  } in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, _headers) ->
    Alcotest.(check string) "anthropic base_url" "https://api.anthropic.com" base_url;
    Alcotest.(check string) "api_key" "sk-ant-test-key" api_key
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed but got: %s" (Error.to_string e))

let test_openai_compat_resolve_success () =
  let env_var = "AGENT_SDK_TEST_OPENROUTER_KEY_q1w2e3" in
  Unix.putenv env_var "or-test-key";
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "https://openrouter.ai/api/v1";
      auth_header = Some "Authorization";
      path = "/chat/completions";
      static_token = None;
    };
    model_id = "anthropic/claude-sonnet-4-6";
    api_key_env = env_var;
  } in
  match Provider.resolve cfg with
  | Ok (base_url, api_key, headers) ->
    Alcotest.(check string) "base_url" "https://openrouter.ai/api/v1" base_url;
    Alcotest.(check string) "api_key" "or-test-key" api_key;
    let auth = List.assoc "Authorization" headers in
    Alcotest.(check string) "bearer token" "Bearer or-test-key" auth
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed: %s" (Error.to_string e))

let test_openai_compat_resolve_missing_key () =
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "https://example.com";
      auth_header = Some "Authorization";
      path = "/chat/completions";
      static_token = None;
    };
    model_id = "test";
    api_key_env = "AGENT_SDK_TEST_NONEXISTENT_COMPAT_KEY_z0z0";
  } in
  match Provider.resolve cfg with
  | Error (Error.Config (MissingEnvVar { var_name })) ->
    Alcotest.(check string) "error mentions env var"
      "AGENT_SDK_TEST_NONEXISTENT_COMPAT_KEY_z0z0" var_name
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error variant: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "should fail when env var missing"

let test_anthropic_headers () =
  let env_var = "AGENT_SDK_TEST_HDR_KEY_h3h3" in
  Unix.putenv env_var "sk-ant-hdr-test";
  let cfg : Provider.config = {
    provider = Anthropic;
    model_id = "test";
    api_key_env = env_var;
  } in
  match Provider.resolve cfg with
  | Ok (_, _, headers) ->
    let xkey = List.assoc "x-api-key" headers in
    Alcotest.(check string) "x-api-key header" "sk-ant-hdr-test" xkey;
    let version = List.assoc "anthropic-version" headers in
    Alcotest.(check string) "anthropic-version" "2023-06-01" version;
    let ct = List.assoc "Content-Type" headers in
    Alcotest.(check string) "content-type" "application/json" ct
  | Error e ->
    Alcotest.fail (Printf.sprintf "should succeed: %s" (Error.to_string e))

let test_model_spec_local_llm_capabilities () =
  let local_llm : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:8085" };
    model_id = "default";
    api_key_env = "DUMMY_KEY";
  } in
  let spec = Provider.model_spec_of_config local_llm in
  Alcotest.(check string) "request path" "/v1/chat/completions" spec.request_path;
  Alcotest.(check string) "request kind" "Openai_chat_completions"
    (match spec.request_kind with
     | Provider.Openai_chat_completions -> "Openai_chat_completions"
     | Provider.Anthropic_messages -> "Anthropic_messages"
     | Provider.Custom n -> "Custom:" ^ n);
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools

let test_model_spec_openrouter_capabilities () =
  let cfg = Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" () in
  let spec =
    Provider.model_spec_of_config cfg
  in
  let contract =
    Provider.inference_contract_of_config cfg
  in
  Alcotest.(check string) "request path" "/chat/completions" spec.request_path;
  Alcotest.(check string) "contract modality" "multimodal"
    (Provider.modality_to_string contract.modality);
  Alcotest.(check (option string)) "contract task" None
    contract.task;
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools;
  Alcotest.(check bool) "supports reasoning" false
    spec.capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" false spec.capabilities.supports_top_k;
  Alcotest.(check bool) "supports json response" true
    spec.capabilities.supports_response_format_json

let test_inference_contract_local_qwen () =
  let cfg : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:8085" };
    model_id = "qwen3.5-35b-a3b-ud-q8-xl";
    api_key_env = "DUMMY_KEY";
  } in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check string) "model_id" "qwen3.5-35b-a3b-ud-q8-xl" contract.model_id;
  Alcotest.(check string) "modality" "text"
    (Provider.modality_to_string contract.modality);
  Alcotest.(check (option string)) "task" None contract.task

let test_inference_contract_anthropic_multimodal () =
  let contract =
    Provider.inference_contract_of_config (Provider.anthropic_sonnet ())
  in
  Alcotest.(check string) "modality" "multimodal"
    (Provider.modality_to_string contract.modality)

let test_inference_contract_task_transcription () =
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "https://api.openai.com/v1";
      auth_header = Some "Authorization";
      path = "/audio/transcriptions";
      static_token = None;
    };
    model_id = "whisper-1";
    api_key_env = "OPENAI_API_KEY";
  } in
  let contract = Provider.inference_contract_of_config cfg in
  Alcotest.(check (option string)) "task" (Some "transcription") contract.task

let test_validate_inference_contract_rejects_unsupported_modality () =
  let cfg : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:8085" };
    model_id = "qwen3.5-35b-a3b-ud-q8-xl";
    api_key_env = "DUMMY_KEY";
  } in
  let contract : Provider.inference_contract = {
    provider = cfg.provider;
    model_id = cfg.model_id;
    modality = Provider.Image;
    task = None;
  } in
  match
    Provider.validate_inference_contract
      ~capabilities:(Provider.capabilities_for_config cfg)
      contract
  with
  | Error (Error.Config (InvalidConfig { field; detail })) ->
    Alcotest.(check string) "field" "modality" field;
    Alcotest.(check string) "detail"
      "Model 'qwen3.5-35b-a3b-ud-q8-xl' for provider 'local' does not support modality 'image'"
      detail
  | Error e ->
    Alcotest.fail (Printf.sprintf "unexpected error variant: %s" (Error.to_string e))
  | Ok () ->
    Alcotest.fail "expected unsupported modality validation to fail"

let test_extended_openai_capabilities () =
  let capabilities =
    Provider.capabilities_for_model
      ~provider:
        (Provider.OpenAICompat
           {
             base_url = "http://localhost:8080";
             auth_header = None;
             path = "/chat/completions";
             static_token = None;
           })
      ~model_id:"qwen3.5-35b-a3b-ud-q8-xl"
  in
  Alcotest.(check bool) "supports reasoning" true capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" true capabilities.supports_top_k;
  Alcotest.(check bool) "supports min_p" true capabilities.supports_min_p

(* ── Phase 6: pricing, ollama, static_token ─────────────────────── *)

let test_pricing_sonnet () =
  let p = Provider.pricing_for_model "claude-sonnet-4-6-20250514" in
  Alcotest.(check (float 0.001)) "input/M" 3.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "output/M" 15.0 p.output_per_million;
  Alcotest.(check (float 0.001)) "cache_write" 1.25 p.cache_write_multiplier;
  Alcotest.(check (float 0.001)) "cache_read" 0.1 p.cache_read_multiplier

let test_pricing_local () =
  let p = Provider.pricing_for_provider
    ~provider:(Local { base_url = "http://127.0.0.1:8085" })
    ~model_id:"qwen3.5-35b-a3b" in
  Alcotest.(check (float 0.001)) "free" 0.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "free output" 0.0 p.output_per_million

let test_pricing_unknown () =
  let p = Provider.pricing_for_model "future-model-xyz" in
  Alcotest.(check (float 0.001)) "zero" 0.0 p.input_per_million

let test_estimate_cost () =
  let p = Provider.pricing_for_model "claude-sonnet-4-6" in
  let cost = Provider.estimate_cost ~pricing:p
    ~input_tokens:1_000_000 ~output_tokens:500_000
    ~cache_creation_input_tokens:100_000
    ~cache_read_input_tokens:200_000 () in
  Alcotest.(check bool) "cost > 0" true (cost > 0.0)

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
  | _ ->
      Alcotest.fail "unexpected provider kind"

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
  | _ ->
      Alcotest.fail "expected localhost ollama config to resolve as local"

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
      Alcotest.(check string) "base_url preserved" "  HTTP://LOCALHOST:11434/v1  "
        base_url
  | _ ->
      Alcotest.fail "expected uppercase localhost config to resolve as local"

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
      Alcotest.(check string) "base_url preserved" "http://localhost?foo=bar"
        base_url
  | _ ->
      Alcotest.fail "expected localhost query config to resolve as local"

let test_openai_compat_static_token () =
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "http://localhost:8080";
      auth_header = Some "Authorization";
      path = "/v1/chat/completions";
      static_token = Some "static-key-123";
    };
    model_id = "test"; api_key_env = "NONEXISTENT";
  } in
  match Provider.resolve cfg with
  | Ok (_, key, headers) ->
    Alcotest.(check string) "static key" "static-key-123" key;
    let auth = List.assoc "Authorization" headers in
    Alcotest.(check string) "bearer" "Bearer static-key-123" auth
  | Error e -> Alcotest.fail (Error.to_string e)

let test_openai_compat_no_auth () =
  let cfg : Provider.config = {
    provider = OpenAICompat {
      base_url = "http://localhost:8080";
      auth_header = None;
      path = "/v1/chat/completions";
      static_token = None;
    };
    model_id = "test"; api_key_env = "NONEXISTENT";
  } in
  match Provider.resolve cfg with
  | Ok (_, key, _) ->
    Alcotest.(check string) "empty key" "" key
  | Error e -> Alcotest.fail (Error.to_string e)

let test_cascade_create () =
  let primary = Provider.anthropic_sonnet () in
  let fallback : Provider.config = {
    provider = Local { base_url = "http://127.0.0.1:8085" };
    model_id = "qwen3.5"; api_key_env = "DUMMY_KEY";
  } in
  let c = Provider.cascade ~primary ~fallbacks:[fallback] in
  Alcotest.(check int) "1 fallback" 1 (List.length c.fallbacks)

let () =
  Alcotest.run "Provider" [
    "resolve", [
      Alcotest.test_case "missing env var returns Error" `Quick test_missing_env_var;
      Alcotest.test_case "present env var returns Ok" `Quick test_present_env_var;
      Alcotest.test_case "local skips env var" `Quick test_local_skips_env_var;
      Alcotest.test_case "anthropic provider" `Quick test_anthropic_provider;
      Alcotest.test_case "openai compat success" `Quick test_openai_compat_resolve_success;
      Alcotest.test_case "openai compat missing key" `Quick test_openai_compat_resolve_missing_key;
      Alcotest.test_case "anthropic headers" `Quick test_anthropic_headers;
      Alcotest.test_case "local llm model spec capabilities" `Quick
        test_model_spec_local_llm_capabilities;
      Alcotest.test_case "openrouter model spec capabilities" `Quick
        test_model_spec_openrouter_capabilities;
      Alcotest.test_case "inference contract local qwen" `Quick
        test_inference_contract_local_qwen;
      Alcotest.test_case "inference contract anthropic multimodal" `Quick
        test_inference_contract_anthropic_multimodal;
      Alcotest.test_case "task inference transcription" `Quick
        test_inference_contract_task_transcription;
      Alcotest.test_case "invalid modality gets actionable error" `Quick
        test_validate_inference_contract_rejects_unsupported_modality;
      Alcotest.test_case "extended openai capabilities" `Quick
        test_extended_openai_capabilities;
    ];
    "pricing", [
      Alcotest.test_case "sonnet pricing" `Quick test_pricing_sonnet;
      Alcotest.test_case "local free" `Quick test_pricing_local;
      Alcotest.test_case "unknown model" `Quick test_pricing_unknown;
      Alcotest.test_case "estimate cost" `Quick test_estimate_cost;
      Alcotest.test_case "provider_config localhost boundary" `Quick
        test_config_of_provider_config_localhost_boundary;
      Alcotest.test_case "provider_config local ollama" `Quick
        test_config_of_provider_config_local_ollama_delegates_to_ssot;
      Alcotest.test_case "provider_config uppercase localhost" `Quick
        test_config_of_provider_config_uppercase_localhost_delegates_to_ssot;
      Alcotest.test_case "provider_config localhost query" `Quick
        test_config_of_provider_config_localhost_query_delegates_to_ssot;
    ];
    "openai_compat", [
      Alcotest.test_case "static token" `Quick test_openai_compat_static_token;
      Alcotest.test_case "no auth" `Quick test_openai_compat_no_auth;
    ];
    "cascade", [
      Alcotest.test_case "create" `Quick test_cascade_create;
    ];
  ]
