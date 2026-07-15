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

(** Forward adapter — Provider.config + agent_state → Provider_config.t *)

let agent_state_with_params () : Types.agent_state =
  let cfg =
    { (Types.default_config ~model:"test-model") with
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

let test_provider_config_of_agent_none_is_rejected () =
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"https://api.anthropic.com" None
  with
  | Error (Error.Config (InvalidConfig { field; detail })) ->
    Alcotest.(check string) "field" "provider" field;
    Alcotest.(check string)
      "explicit error"
      "An explicit provider configuration is required"
      detail
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "missing provider must not be reinterpreted as Anthropic"
;;

let test_provider_config_of_agent_local_keeps_empty_api_key () =
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
    Alcotest.(check string) "local api_key remains empty" "" (pc.api_key :> string);
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
    check_provider_id "registered provider id" "gemini" pc;
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
    check_provider_id "registered provider id" "kimi" pc;
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

let test_provider_config_of_agent_custom_registered_nous_uses_declaration () =
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
           check_provider_id "registered provider id" "nous" pc;
           Alcotest.(check string)
             "custom nous base_url"
             Llm_provider.Discovery.default_endpoint
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
        check_provider_id "registered provider id" "ollama_cloud" pc;
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

let test_provider_config_of_agent_custom_registered_ollama_cloud_does_not_infer_api_key ()
  =
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
      | Error (Error.Config (MissingEnvVar { var_name })) ->
        Alcotest.(check string)
          "only the declared credential is accepted"
          "OLLAMA_CLOUD_API_KEY"
          var_name
      | Error e ->
        Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
      | Ok _ ->
        Alcotest.fail "undeclared OLLAMA_API_KEY must not satisfy OLLAMA_CLOUD_API_KEY"))
;;

let agent_state_with_schema schema : Types.agent_state =
  let base = agent_state_with_params () in
  let cfg = { base.config with response_format = Types.JsonSchema schema } in
  { base with config = cfg }
;;

let test_provider_config_of_agent_custom_registered_ollama_cloud_row_separates_json_mode
      ()
  =
  (* Custom_registered ollama_cloud must resolve the explicit
     (provider_name, model_id) catalog tuple. devstral-2:123b advertises JSON
     mode, but #2499 correctly removed the stronger native-schema guarantee
     from the Ollama Cloud /v1 boundary. *)
  with_env "OLLAMA_CLOUD_API_KEY" (Some "ollama-cloud-test-key") (fun () ->
    let schema = `Assoc [ "type", `String "object" ] in
    let cfg : Provider.config =
      { provider = Custom_registered { name = "ollama_cloud" }
      ; model_id = "devstral-2:123b"
      ; api_key_env = "OLLAMA_CLOUD_API_KEY"
      }
    in
    let state = agent_state_with_schema schema in
    match
      Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
    with
    | Ok pc ->
      check_provider_id "registered provider id" "ollama_cloud" pc;
      Alcotest.(check bool)
        "row is authoritative: no model_capabilities_override"
        true
        (Option.is_none pc.model_capabilities_override);
      Alcotest.(check bool)
        "row is authoritative: no supports_structured_output_override"
        true
        (Option.is_none pc.supports_structured_output_override);
      Alcotest.(check bool)
        "provider kind stays Ollama"
        true
        (pc.kind = Llm_provider.Provider_config.Ollama);
      (match Llm_provider.Provider_config.capabilities_for_config_model pc with
       | Some caps ->
         Alcotest.(check bool)
           "row advertises JSON mode"
           true
           caps.supports_response_format_json;
         Alcotest.(check bool)
           "row does not advertise native structured output"
           false
           caps.supports_structured_output
       | None -> Alcotest.fail "provider/model catalog tuple was not resolved");
      Alcotest.(check bool)
        "native schema rejected because JSON mode is not structured output"
        true
        (Result.is_error (Llm_provider.Provider_config.validate_output_schema_request pc))
    | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e)))
;;

let test_provider_config_of_agent_custom_registered_ollama_cloud_row_rejects_so () =
  (* minimax-m3 catalog row explicitly does not guarantee schema-shaped output.
     The named-provider path must reject the request with the model capability
     error, not silently inherit the provider default. *)
  with_env "OLLAMA_CLOUD_API_KEY" (Some "ollama-cloud-test-key") (fun () ->
    let schema = `Assoc [ "type", `String "object" ] in
    let cfg : Provider.config =
      { provider = Custom_registered { name = "ollama_cloud" }
      ; model_id = "minimax-m3"
      ; api_key_env = "OLLAMA_CLOUD_API_KEY"
      }
    in
    let state = agent_state_with_schema schema in
    match
      Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
    with
    | Ok pc ->
      check_provider_id "registered provider id" "ollama_cloud" pc;
      Alcotest.(check bool)
        "row is authoritative: no model_capabilities_override"
        true
        (Option.is_none pc.model_capabilities_override);
      (match Llm_provider.Provider_config.validate_output_schema_request pc with
       | Error msg ->
         Alcotest.(check bool)
           "rejected with model capability reason"
           true
           (Util.contains_substring_ci
              ~haystack:msg
              ~needle:"does not advertise native structured output")
       | Ok () -> Alcotest.fail "expected rejection for ollama_cloud/minimax-m3")
    | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e)))
;;

let test_provider_config_of_agent_custom_registered_ollama_cloud_unknown_uses_default () =
  (* When no provider/model catalog tuple exists, the named provider path should
     still project the registry default capabilities (the pre-#2440 behavior
     for explicit Provider_catalog endpoints), not leave capabilities
     unconstrained. *)
  with_env "OLLAMA_CLOUD_API_KEY" (Some "ollama-cloud-test-key") (fun () ->
    let schema = `Assoc [ "type", `String "object" ] in
    let cfg : Provider.config =
      { provider = Custom_registered { name = "ollama_cloud" }
      ; model_id = "totally-unknown-model-with-no-row"
      ; api_key_env = "OLLAMA_CLOUD_API_KEY"
      }
    in
    let state = agent_state_with_schema schema in
    match
      Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
    with
    | Ok pc ->
      check_provider_id "registered provider id" "ollama_cloud" pc;
      Alcotest.(check bool)
        "registry default override is present when no row exists"
        true
        (Option.is_some pc.model_capabilities_override);
      Alcotest.(check bool)
        "schema request rejected by provider default"
        true
        (Result.is_error (Llm_provider.Provider_config.validate_output_schema_request pc))
    | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e)))
;;

let test_provider_config_of_agent_model_row_precedes_provider_capabilities () =
  with_provider_catalog
    {|
{
  "schema_version": 1,
  "providers": [
    {
      "id": "runpod-qwen36",
      "kind": "openai_compat",
      "base_url": "https://ma8xbr1kgbclkl-64411be1.proxy.runpod.net/v1",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat",
      "capabilities": {
        "supports_structured_output": false,
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
       with_catalog_toml
         {|
[[models]]
id_prefix = "qwen/qwen3.6-35b-a3b"
base = "openai_chat"
provider_name = "runpod-qwen36"
supports_structured_output = true
supports_reasoning = true
supports_extended_thinking = true
supports_reasoning_budget = true
thinking_control_format = "chat_template_kwargs"
preserve_thinking_control_format = "chat_template_kwargs_preserve_thinking"
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
            check_provider_id "canonical provider id" "runpod-qwen36" declared_pc;
            Alcotest.(check (option bool))
              "provider-level structured-output flag does not mask model row"
              None
              declared_pc.supports_structured_output_override;
            Alcotest.(check bool)
              "provider-level capability record does not mask model row"
              true
              (Option.is_none declared_pc.model_capabilities_override);
            let declared_caps =
              Llm_provider.Provider_config.capabilities_for_config_model declared_pc
            in
            Alcotest.(check bool)
              "model row uses chat_template_kwargs"
              true
              (match declared_caps with
               | Some caps ->
                 caps.Llm_provider.Capabilities.thinking_control_format
                 = Llm_provider.Capabilities.Chat_template_kwargs
                 && caps.preserve_thinking_control_format
                    = Llm_provider.Capabilities.Chat_template_kwargs_preserve_thinking
               | None -> false);
            Alcotest.(check bool)
              "model row validates schema output despite provider default"
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
              "raw OpenAICompat does not resolve the provider-independent model row"
              true
              (Option.is_none
                 (Llm_provider.Provider_config.capabilities_for_config_model raw_pc));
            Alcotest.(check bool)
              "raw compatibility schema request fails closed"
              true
              (Result.is_error
                 (Llm_provider.Provider_config.validate_output_schema_request raw_pc))))
;;

let test_provider_config_of_agent_runtime_only_registration () =
  let provider_name = "runtime-only-provider-contract-2590-7f3b1" in
  let model_id = "runtime-only-model-contract-2590-7f3b1" in
  let capabilities : Provider.capabilities =
    { Provider.default_capabilities with
      supports_tools = true
    ; supports_response_format_json = true
    ; supports_structured_output = true
    }
  in
  let impl : Provider.provider_impl =
    { name = provider_name
    ; provider_kind = Llm_provider.Provider_config.Anthropic
    ; request_kind = Provider.Anthropic_messages
    ; request_path = "/runtime-only/messages"
    ; capabilities
    ; build_body = (fun ~config:_ ~messages:_ ?tools:_ () -> "{}")
    ; parse_response =
        (fun _body ->
          { Types.id = "runtime-only-response"
          ; model = model_id
          ; stop_reason = Types.EndTurn
          ; content = [ Types.Text "runtime-only" ]
          ; usage = None
          ; telemetry = None
          })
    ; resolve =
        (fun _cfg ->
          Ok
            ( "https://runtime-only-provider.invalid/api"
            , "runtime-only-secret"
            , [ "Content-Type", "application/json"; "x-runtime-provider", "2590" ] ))
    }
  in
  Provider.register_provider impl;
  let cfg : Provider.config =
    { provider = Custom_registered { name = provider_name }
    ; model_id
    ; api_key_env = "RUNTIME_ONLY_PROVIDER_UNUSED_KEY"
    }
  in
  let state = agent_state_with_params () in
  match
    Provider.provider_config_of_agent ~state ~base_url:"unused-fallback" (Some cfg)
  with
  | Ok pc ->
    check_provider_id "runtime-only provider id" provider_name pc;
    Alcotest.(check bool)
      "runtime request kind maps to Anthropic"
      true
      (pc.kind = Llm_provider.Provider_config.Anthropic);
    Alcotest.(check string)
      "runtime endpoint"
      "https://runtime-only-provider.invalid/api"
      pc.base_url;
    Alcotest.(check string)
      "runtime request path"
      "/runtime-only/messages"
      pc.request_path;
    Alcotest.(check string)
      "runtime resolver credential"
      "runtime-only-secret"
      (pc.api_key :> string);
    Alcotest.(check (list (pair string string)))
      "runtime resolver headers"
      [ "Content-Type", "application/json"; "x-runtime-provider", "2590" ]
      pc.headers;
    (match pc.model_capabilities_override with
     | Some projected ->
       Alcotest.(check bool)
         "runtime capabilities preserve tools"
         true
         projected.supports_tools;
       Alcotest.(check bool)
         "runtime capabilities preserve structured output"
         true
         projected.supports_structured_output
     | None -> Alcotest.fail "runtime-only capabilities were not projected")
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
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
    Alcotest.(check string)
      "explicit declaration/runtime registration diagnostic"
      "Custom_registered provider 'nonexistent-provider-xyz' is neither declared nor \
       registered at runtime"
      detail
  | Error e -> Alcotest.fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ -> Alcotest.fail "should error on unregistered name"
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
            "none is rejected explicitly"
            `Quick
            test_provider_config_of_agent_none_is_rejected
        ; Alcotest.test_case
            "local keeps empty api key"
            `Quick
            test_provider_config_of_agent_local_keeps_empty_api_key
        ; Alcotest.test_case
            "custom registered preserves kind (#1003)"
            `Quick
            test_provider_config_of_agent_custom_registered_preserves_kind
        ; Alcotest.test_case
            "custom registered kimi preserves headers"
            `Quick
            test_provider_config_of_agent_custom_registered_kimi_preserves_headers
        ; Alcotest.test_case
            "custom registered nous uses declaration"
            `Quick
            test_provider_config_of_agent_custom_registered_nous_uses_declaration
        ; Alcotest.test_case
            "custom registered ollama_cloud adds auth header"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_headers
        ; Alcotest.test_case
            "custom registered ollama_cloud uses only its declared key"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_does_not_infer_api_key
        ; Alcotest.test_case
            "custom registered ollama_cloud row separates JSON mode from schema"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_row_separates_json_mode
        ; Alcotest.test_case
            "custom registered ollama_cloud row rejects structured output"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_row_rejects_so
        ; Alcotest.test_case
            "custom registered ollama_cloud unknown uses provider default"
            `Quick
            test_provider_config_of_agent_custom_registered_ollama_cloud_unknown_uses_default
        ; Alcotest.test_case
            "model row precedes provider capabilities"
            `Quick
            test_provider_config_of_agent_model_row_precedes_provider_capabilities
        ; Alcotest.test_case
            "runtime-only registration maps exact provider contract"
            `Quick
            test_provider_config_of_agent_runtime_only_registration
        ; Alcotest.test_case
            "custom registered unknown name errors"
            `Quick
            test_provider_config_of_agent_custom_registered_unknown_name
        ] )
    ]
;;
