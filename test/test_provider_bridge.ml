open Base
(** Tests for Provider_bridge: legacy Provider.config -> Provider_config.t *)

let with_env key value f =
  let previous = Sys.getenv_opt key in
  let restore () =
    match previous with
    | Some current -> Unix.putenv key current
    | None -> Unix.putenv key ""
  in
  Fun.protect ~finally:restore (fun () ->
    Unix.putenv key value;
    f ())
;;

let test_anthropic_bridge () =
  let legacy = Agent_sdk.Provider.anthropic_sonnet () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
    (* Expected in test env without ANTHROPIC_API_KEY *)
    Alcotest.(check pass) "missing key = expected in test" () ()
  | Ok cfg ->
    Alcotest.(check string) "model" "claude-sonnet-4-6" cfg.model_id;
    Alcotest.(check string) "path" "/v1/messages" cfg.request_path
;;

let test_openai_compat_bridge () =
  let legacy = Agent_sdk.Provider.openrouter () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.(check pass) "missing key = expected in test" () ()
  | Ok cfg -> Alcotest.(check string) "path" "/chat/completions" cfg.request_path
;;

let test_local_provider_bridge () =
  let legacy = Agent_sdk.Provider.local_llm () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.fail "local provider should not need env var"
  | Ok cfg ->
    Alcotest.(check string) "model" "default" cfg.model_id;
    Alcotest.(check string) "path" "/v1/chat/completions" cfg.request_path
;;

let test_non_zai_glm_stays_openai_compat () =
  let legacy =
    { Agent_sdk.Provider.provider =
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
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.fail "custom openai compat provider should not need env var"
  | Ok cfg ->
    Alcotest.(check string)
      "kind remains openai compat"
      "openai_compat"
      (match cfg.kind with
       | Llm_provider.Provider_config.OpenAI_compat -> "openai_compat"
       | Anthropic -> "anthropic"
       | Kimi -> "kimi"
       | Gemini -> "gemini"
       | Glm -> "glm"
       | Ollama -> "ollama"
       | DashScope -> "dashscope"
       | Claude_code -> "claude_code"
       | Gemini_cli -> "gemini_cli"
       | Kimi_cli -> "kimi_cli"
       | Codex_cli -> "codex_cli")
;;

let test_zai_glm_becomes_glm_provider_config () =
  let legacy =
    { Agent_sdk.Provider.provider =
        OpenAICompat
          { base_url = Llm_provider.Zai_catalog.general_base_url
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "glm-5"
    ; api_key_env = ""
    }
  in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.fail "z.ai glm provider should resolve without env var"
  | Ok cfg ->
    Alcotest.(check string)
      "kind becomes glm"
      "glm"
      (match cfg.kind with
       | Llm_provider.Provider_config.OpenAI_compat -> "openai_compat"
       | Anthropic -> "anthropic"
       | Kimi -> "kimi"
       | Gemini -> "gemini"
       | Glm -> "glm"
       | Ollama -> "ollama"
       | DashScope -> "dashscope"
       | Claude_code -> "claude_code"
       | Gemini_cli -> "gemini_cli"
       | Kimi_cli -> "kimi_cli"
       | Codex_cli -> "codex_cli")
;;

let test_zai_coding_auto_uses_coding_default_model () =
  with_env "ZAI_DEFAULT_MODEL" "glm-5.1" (fun () ->
    with_env "ZAI_CODING_DEFAULT_MODEL" "glm-4.5-air" (fun () ->
      let legacy =
        { Agent_sdk.Provider.provider =
            OpenAICompat
              { base_url = Llm_provider.Zai_catalog.coding_base_url
              ; auth_header = None
              ; path = "/chat/completions"
              ; static_token = None
              }
        ; model_id = "auto"
        ; api_key_env = ""
        }
      in
      match Agent_sdk.Provider_bridge.to_provider_config legacy with
      | Error _ -> Alcotest.fail "z.ai coding provider should resolve without env var"
      | Ok cfg -> Alcotest.(check string) "coding auto model" "glm-4.5-air" cfg.model_id))
;;

let test_kimi_custom_registered_becomes_kimi_provider_config () =
  let env_var = "KIMI_PROVIDER_BRIDGE_TEST_KEY" in
  with_env env_var "kimi-test-key" (fun () ->
    let legacy =
      { Agent_sdk.Provider.provider = Custom_registered { name = "kimi" }
      ; model_id = "auto"
      ; api_key_env = env_var
      }
    in
    match Agent_sdk.Provider_bridge.to_provider_config legacy with
    | Error e ->
      Alcotest.fail
        (Printf.sprintf
           "kimi custom provider should resolve: %s"
           (Agent_sdk.Error.to_string e))
    | Ok cfg ->
      Alcotest.(check string)
        "kind becomes kimi"
        "kimi"
        (Llm_provider.Provider_config.string_of_provider_kind cfg.kind);
      Alcotest.(check string) "auto model" "kimi-for-coding" cfg.model_id;
      Alcotest.(check string) "path" "/v1/messages" cfg.request_path)
;;

let test_zai_coding_auto_models_default_order () =
  with_env "ZAI_CODING_AUTO_MODELS" "" (fun () ->
    Alcotest.(check (list string))
      "coding auto order"
      [ "glm-5.1"; "glm-5"; "glm-5-turbo"; "glm-4.7"; "glm-4.5-air" ]
      (Llm_provider.Zai_catalog.glm_coding_auto_models ()))
;;

let () =
  let open Alcotest in
  run
    "provider_bridge"
    [ ( "to_provider_config"
      , [ test_case "anthropic" `Quick test_anthropic_bridge
        ; test_case "openai compat" `Quick test_openai_compat_bridge
        ; test_case "local" `Quick test_local_provider_bridge
        ; test_case
            "non-zai glm stays openai compat"
            `Quick
            test_non_zai_glm_stays_openai_compat
        ; test_case "zai glm becomes glm" `Quick test_zai_glm_becomes_glm_provider_config
        ; test_case
            "zai coding auto uses coding default model"
            `Quick
            test_zai_coding_auto_uses_coding_default_model
        ; test_case
            "kimi custom provider becomes kimi"
            `Quick
            test_kimi_custom_registered_becomes_kimi_provider_config
        ; test_case
            "zai coding auto models default order"
            `Quick
            test_zai_coding_auto_models_default_order
        ] )
    ]
;;
