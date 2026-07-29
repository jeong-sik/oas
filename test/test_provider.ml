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

let check_provider_id label expected (pc : Llm_provider.Provider_config.t) =
  Alcotest.(check (option string)) label (Some expected) pc.provider_id
;;

let with_provider_catalog json f =
  match Llm_provider.Provider_catalog.of_json (Yojson.Safe.from_string json) with
  | Error msg -> Alcotest.fail msg
  | Ok catalog ->
    Llm_provider.Provider_catalog.set_global catalog;
    Fun.protect ~finally:Llm_provider.Provider_catalog.clear_global f
;;

let install_embedded_model_catalog () =
  Model_catalog_test_support.install_embedded_model_catalog ~suite:"provider"
;;

let declared_pricing model_id =
  match Provider.pricing_for_model_opt model_id with
  | Some pricing -> pricing
  | None -> Alcotest.failf "expected catalog pricing for %S" model_id
;;

let require_estimated_cost = function
  | Provider.Estimated cost -> cost
  | Provider.Incomplete _ -> Alcotest.fail "expected an exact cost estimate"
;;

let with_empty_capability_sources f =
  let original_catalog = Llm_provider.Model_catalog.global () in
  let original_manifest = Llm_provider.Capability_manifest.global () in
  Llm_provider.Model_catalog.set_global Llm_provider.Model_catalog.empty;
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

let task_testable =
  Alcotest.testable
    (fun ppf task -> Format.pp_print_string ppf (Provider.task_to_string task))
    ( = )
;;

(* Install a synthetic catalog from inline TOML for the duration of [f], then
   restore the original. Goes through [Model_catalog.load_file] so the tests
   exercise the real fail-closed parse path. *)
let with_catalog_toml content f =
  let path = Filename.temp_file "oas_provider_task_catalog" ".toml" in
  let original = Llm_provider.Model_catalog.global () in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove path with
       | Sys_error _ -> ());
      match original with
      | Some c -> Llm_provider.Model_catalog.set_global c
      | None -> Llm_provider.Model_catalog.clear_global ())
    (fun () ->
       let oc = open_out path in
       output_string oc content;
       close_out oc;
       match Llm_provider.Model_catalog.load_file path with
       | Ok catalog ->
         Llm_provider.Model_catalog.set_global catalog;
         f ()
       | Error e -> Alcotest.fail ("test catalog load failed: " ^ e))
;;

let task_catalog_toml =
  {|
[[models]]
id_prefix = "acme-transcribe"
supports_audio_input = true
task = "transcription"

[[models]]
id_prefix = "acme-tts"
task = "speech"

[[models]]
id_prefix = "acme-image"
supports_image_input = true
task = "image_generation"

[[models]]
id_prefix = "acme-video"
supports_video_input = true
task = "video_generation"

[[models]]
id_prefix = "acme-chat"
|}
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
  | Ok (base_url, api_key, _headers) ->
    Alcotest.(check string) "base_url" "http://localhost:9999" base_url;
    Alcotest.(check string) "local api_key" "" api_key
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
  Alcotest.(check (option task_testable)) "contract task" None contract.task;
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools;
  Alcotest.(check bool) "supports reasoning" false spec.capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" false spec.capabilities.supports_top_k;
  Alcotest.(check bool)
    "supports json response"
    true
    spec.capabilities.supports_response_format_json
;;

let test_inference_contract_anthropic_multimodal () =
  let contract =
    Provider.inference_contract_of_config
      (Provider.anthropic ~model_id:"claude-sonnet-4-6" ())
  in
  Alcotest.(check string)
    "modality"
    "multimodal"
    (Provider.modality_to_string contract.modality)
;;

let test_capabilities_task_catalog_declared () =
  with_catalog_toml task_catalog_toml (fun () ->
    let task_of model_id =
      match Llm_provider.Capabilities.for_model_id model_id with
      | Some (caps : Provider.capabilities) -> caps.task
      | None -> Alcotest.failf "catalog entry expected for %s" model_id
    in
    Alcotest.(check (option task_testable))
      "transcription"
      (Some Provider.Transcription)
      (task_of "acme-transcribe-1");
    Alcotest.(check (option task_testable))
      "speech"
      (Some Provider.Speech)
      (task_of "acme-tts-1");
    Alcotest.(check (option task_testable))
      "image generation"
      (Some Provider.Image_generation)
      (task_of "acme-image-1");
    Alcotest.(check (option task_testable))
      "video generation"
      (Some Provider.Video_generation)
      (task_of "acme-video-1");
    Alcotest.(check (option task_testable))
      "entry without a task field declares no task"
      None
      (task_of "acme-chat-1"))
;;

let test_inference_contract_task_catalog_declared () =
  with_catalog_toml task_catalog_toml (fun () ->
    (* The [Anthropic] branch of [capabilities_for_model] consults the model
       catalog, so this exercises the full config -> capabilities -> contract
       threading. *)
    let cfg : Provider.config =
      { provider = Anthropic
      ; model_id = "acme-transcribe-1"
      ; api_key_env = "ANTHROPIC_API_KEY"
      }
    in
    let contract = Provider.inference_contract_of_config cfg in
    Alcotest.(check (option task_testable))
      "catalog-declared task reaches the contract"
      (Some Provider.Transcription)
      contract.task)
;;

(* Regression for the deleted model-id substring classifier: these ids used to
   be classified as transcription/image_generation/video_generation purely by
   substring. Without a catalog-declared [task] they must stay [None]. *)
let test_inference_contract_task_never_inferred_from_model_id () =
  with_catalog_toml task_catalog_toml (fun () ->
    let contract_task provider model_id =
      let cfg : Provider.config = { provider; model_id; api_key_env = "" } in
      (Provider.inference_contract_of_config cfg).task
    in
    Alcotest.(check (option task_testable))
      "whisper-style id declares no task"
      None
      (contract_task
         (OpenAICompat
            { base_url = "https://api.openai.com/v1"
            ; auth_header = Some "Authorization"
            ; path = "/audio/transcriptions"
            ; static_token = None
            })
         "whisper-1");
    Alcotest.(check (option task_testable))
      "glm-image id declares no task"
      None
      (contract_task
         (OpenAICompat
            { base_url = Llm_provider.Zai_catalog.general_base_url
            ; auth_header = None
            ; path = "/images/generations"
            ; static_token = None
            })
         "glm-image");
    Alcotest.(check (option task_testable))
      "cogvideox id declares no task"
      None
      (contract_task
         (OpenAICompat
            { base_url = Llm_provider.Zai_catalog.general_base_url
            ; auth_header = None
            ; path = "/videos/generations"
            ; static_token = None
            })
         "cogvideox-2"))
;;

let test_raw_zai_like_openai_compat_stays_generic () =
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
    capabilities.supports_multimodal_inputs;
  Alcotest.(check bool)
    "does not infer GLM reasoning"
    false
    capabilities.supports_reasoning;
  Alcotest.(check bool)
    "does not infer GLM thinking dialect"
    true
    (capabilities.thinking_control_format = Llm_provider.Capabilities.No_thinking_control)
;;

let non_glm_prefixed_glm_catalog_toml =
  {|
[[models]]
id_prefix = "fake-glm-model"
base = "glm"
max_context_tokens = 999999
|}
;;

let test_raw_openai_compat_does_not_infer_glm_from_model_or_host () =
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
  Alcotest.(check bool) "reasoning not inferred" false capabilities.supports_reasoning;
  Alcotest.(check bool)
    "extended thinking not inferred"
    false
    capabilities.supports_extended_thinking
;;

(* A raw compatibility provider has no vendor identity, so an unscoped model
   row cannot replace its generic wire capability contract. *)
let test_raw_openai_compat_ignores_provider_independent_catalog_row () =
  with_catalog_toml non_glm_prefixed_glm_catalog_toml (fun () ->
    let cfg : Provider.config =
      { provider =
          OpenAICompat
            { base_url = "https://openrouter.ai/api/v1"
            ; auth_header = None
            ; path = "/chat/completions"
            ; static_token = None
            }
      ; model_id = "fake-glm-model"
      ; api_key_env = ""
      }
    in
    let capabilities = Provider.capabilities_for_config cfg in
    Alcotest.(check bool)
      "unscoped model row does not replace generic capabilities"
      true
      (capabilities.max_context_tokens <> Some 999_999))
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

let test_raw_openai_compat_does_not_infer_dashscope_capabilities () =
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

let test_raw_openai_compat_does_not_infer_minimax_model_contract () =
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

let test_local_compat_does_not_infer_model_capabilities () =
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
    "declared thinking control"
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
  let opus = Provider.anthropic ~model_id:"claude-opus-4-6" () in
  let sonnet = Provider.anthropic ~model_id:"claude-sonnet-4-6" () in
  let haiku = Provider.anthropic ~model_id:"claude-haiku-4-5-20251001" () in
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
  let p = declared_pricing "claude-sonnet-4-6-20250514" in
  Alcotest.(check (float 0.001)) "input/M" 3.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "output/M" 15.0 p.output_per_million;
  Alcotest.(check (option (float 0.001)))
    "cache_write"
    (Some 1.25)
    p.cache_write_multiplier;
  Alcotest.(check (option (float 0.001))) "cache_read" (Some 0.1) p.cache_read_multiplier
;;

let test_pricing_gpt55 () =
  let p = declared_pricing "gpt-5.5" in
  Alcotest.(check (float 0.001)) "input/M" 5.0 p.input_per_million;
  Alcotest.(check (float 0.001)) "output/M" 30.0 p.output_per_million;
  Alcotest.(check (option (float 0.001)))
    "cache_write"
    (Some 1.0)
    p.cache_write_multiplier;
  Alcotest.(check (option (float 0.001))) "cache_read" (Some 0.1) p.cache_read_multiplier
;;

let test_incomplete_cache_pricing_remains_declared () =
  Alcotest.(check bool)
    "base price remains observable without inventing cache multipliers"
    true
    (match Provider.pricing_for_model_opt "dashscope-3.5-35b-a3b" with
     | Some
         { input_per_million = 0.0
         ; output_per_million = 0.0
         ; cache_write_multiplier = None
         ; cache_read_multiplier = None
         } -> true
     | Some _ | None -> false)
;;

let test_pricing_unknown () =
  Alcotest.(check bool)
    "unpriced"
    true
    (Option.is_none (Provider.pricing_for_model_opt "future-model-xyz"))
;;

let test_estimate_cost () =
  let p = declared_pricing "claude-sonnet-4-6" in
  let cost =
    Provider.estimate_cost
      ~pricing:p
      ~input_tokens:1_000_000
      ~output_tokens:500_000
      ~cache_creation_input_tokens:100_000
      ~cache_read_input_tokens:200_000
      ()
    |> require_estimated_cost
  in
  Alcotest.(check bool) "cost > 0" true (cost > 0.0)
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

let test_provider_config_rebinds_model_specific_context () =
  let parent_capabilities =
    { Llm_provider.Capabilities.anthropic_capabilities with
      max_context_tokens = Some 12_345
    }
  in
  let parent =
    Llm_provider.Provider_config.make
      ~kind:Anthropic
      ~model_id:"claude-opus-4-1"
      ~base_url:"https://api.anthropic.com"
      ~max_context:12_345
      ~model_capabilities_override:parent_capabilities
      ()
  in
  let target_config = Types.default_config ~model:"claude-sonnet-4-5" in
  let target = Provider.provider_config_with_agent_config ~config:target_config parent in
  let expected =
    let clean_target =
      { parent with
        model_id = "claude-sonnet-4-5"
      ; max_context = None
      ; model_capabilities_override = None
      ; supports_structured_output_override = None
      }
    in
    Option.bind
      (Llm_provider.Provider_config.capabilities_for_config_model clean_target)
      (fun capabilities -> capabilities.max_context_tokens)
  in
  Alcotest.(check string) "target model" "claude-sonnet-4-5" target.model_id;
  Alcotest.(check (option int)) "target context SSOT" expected target.max_context;
  Alcotest.(check bool)
    "parent model capability override is not inherited"
    true
    (Option.is_none target.model_capabilities_override)
;;

let () =
  install_embedded_model_catalog ();
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
            "task catalog declared capabilities"
            `Quick
            test_capabilities_task_catalog_declared
        ; Alcotest.test_case
            "task catalog declared contract"
            `Quick
            test_inference_contract_task_catalog_declared
        ; Alcotest.test_case
            "task never inferred from model id"
            `Quick
            test_inference_contract_task_never_inferred_from_model_id
        ; Alcotest.test_case
            "raw ZAI-like OpenAI compatibility stays generic"
            `Quick
            test_raw_zai_like_openai_compat_stays_generic
        ; Alcotest.test_case
            "raw OpenAI compatibility does not infer GLM"
            `Quick
            test_raw_openai_compat_does_not_infer_glm_from_model_or_host
        ; Alcotest.test_case
            "raw OpenAI compatibility ignores provider-independent model row"
            `Quick
            test_raw_openai_compat_ignores_provider_independent_catalog_row
        ; Alcotest.test_case
            "invalid modality gets actionable error"
            `Quick
            test_validate_inference_contract_rejects_unsupported_modality
        ; Alcotest.test_case
            "raw OpenAI-compatible does not infer DashScope capabilities"
            `Quick
            test_raw_openai_compat_does_not_infer_dashscope_capabilities
        ; Alcotest.test_case
            "raw openai_compat does not infer dashscope"
            `Quick
            test_raw_openai_compat_does_not_infer_dashscope_from_model_id
        ; Alcotest.test_case
            "raw openai_compat does not infer MiniMax contract"
            `Quick
            test_raw_openai_compat_does_not_infer_minimax_model_contract
        ; Alcotest.test_case
            "local compat does not infer model capabilities"
            `Quick
            test_local_compat_does_not_infer_model_capabilities
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
        ; Alcotest.test_case
            "incomplete cache pricing remains declared"
            `Quick
            test_incomplete_cache_pricing_remains_declared
        ; Alcotest.test_case "unknown model" `Quick test_pricing_unknown
        ; Alcotest.test_case "estimate cost" `Quick test_estimate_cost
        ] )
    ; ( "openai_compat"
      , [ Alcotest.test_case "static token" `Quick test_openai_compat_static_token
        ; Alcotest.test_case "no auth" `Quick test_openai_compat_no_auth
        ] )
    ; ( "provider_config"
      , [ Alcotest.test_case
            "model rebind clears parent context"
            `Quick
            test_provider_config_rebinds_model_specific_context
        ] )
    ]
;;
