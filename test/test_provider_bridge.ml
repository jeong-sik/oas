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

let test_provider_a_bridge () =
  let legacy = Agent_sdk.Provider.provider_a_sonnet () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
    (* Expected in test env without PROVIDER_A_API_KEY *)
    Alcotest.(check pass) "missing key = expected in test" () ()
  | Ok cfg ->
    Alcotest.(check string) "model" "agent_llm_a-sonnet-4-6" cfg.model_id;
    Alcotest.(check string) "path" "/v1/messages" cfg.request_path
;;

let test_provider_d_compat_bridge () =
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

let test_non_zai_glm_stays_provider_d_compat () =
  let legacy =
    { Agent_sdk.Provider.provider =
        OpenAICompat
          { base_url = "https://openrouter.ai/api/v1"
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "provider_k-5"
    ; api_key_env = ""
    }
  in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.fail "custom provider_d compat provider should not need env var"
  | Ok cfg ->
    Alcotest.(check string)
      "kind remains provider_d compat"
      "provider_d_compat"
      (match cfg.kind with
       | Llm_provider.Provider_config.Provider_d_compat -> "provider_d_compat"
       | Provider_a -> "provider_a"
       | Provider_c -> "provider_c"
       | Provider_f -> "provider_f"
       | Provider_k -> "provider_k"
       | Ollama -> "ollama"
       | Provider_h -> "provider_h"
       | Cli_tool_d -> "cli_tool_d"
       | Cli_tool_b -> "cli_tool_b"
       | Cli_tool_c -> "cli_tool_c"
       | Cli_tool_a -> "cli_tool_a")
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
    ; model_id = "provider_k-5"
    ; api_key_env = ""
    }
  in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ -> Alcotest.fail "z.ai provider_k provider should resolve without env var"
  | Ok cfg ->
    Alcotest.(check string)
      "kind becomes provider_k"
      "provider_k"
      (match cfg.kind with
       | Llm_provider.Provider_config.Provider_d_compat -> "provider_d_compat"
       | Provider_a -> "provider_a"
       | Provider_c -> "provider_c"
       | Provider_f -> "provider_f"
       | Provider_k -> "provider_k"
       | Ollama -> "ollama"
       | Provider_h -> "provider_h"
       | Cli_tool_d -> "cli_tool_d"
       | Cli_tool_b -> "cli_tool_b"
       | Cli_tool_c -> "cli_tool_c"
       | Cli_tool_a -> "cli_tool_a")
;;

let test_zai_coding_auto_uses_coding_default_model () =
  with_env "ZAI_DEFAULT_MODEL" "provider_k-5.1" (fun () ->
    with_env "ZAI_CODING_DEFAULT_MODEL" "provider_k-4.5-air" (fun () ->
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
      | Ok cfg -> Alcotest.(check string) "coding auto model" "provider_k-4.5-air" cfg.model_id))
;;

let test_provider_c_custom_registered_becomes_provider_c_provider_config () =
  let env_var = "KIMI_PROVIDER_BRIDGE_TEST_KEY" in
  with_env env_var "provider_c-test-key" (fun () ->
    let legacy =
      { Agent_sdk.Provider.provider = Custom_registered { name = "provider_c" }
      ; model_id = "auto"
      ; api_key_env = env_var
      }
    in
    match Agent_sdk.Provider_bridge.to_provider_config legacy with
    | Error e ->
      Alcotest.fail
        (Printf.sprintf
           "provider_c custom provider should resolve: %s"
           (Agent_sdk.Error.to_string e))
    | Ok cfg ->
      Alcotest.(check string)
        "kind becomes provider_c"
        "provider_c"
        (Llm_provider.Provider_config.string_of_provider_kind cfg.kind);
      Alcotest.(check string) "auto model" "provider_c-for-coding" cfg.model_id;
      Alcotest.(check string) "path" "/v1/messages" cfg.request_path)
;;

let test_zai_coding_auto_models_default_order () =
  with_env "ZAI_CODING_AUTO_MODELS" "" (fun () ->
    Alcotest.(check (list string))
      "coding auto order"
      [ "provider_k-5.1"; "provider_k-5"; "provider_k-5-turbo"; "provider_k-4.7"; "provider_k-4.5-air" ]
      (Llm_provider.Zai_catalog.provider_k_coding_auto_models ()))
;;

let () =
  let open Alcotest in
  run
    "provider_bridge"
    [ ( "to_provider_config"
      , [ test_case "provider_a" `Quick test_provider_a_bridge
        ; test_case "provider_d compat" `Quick test_provider_d_compat_bridge
        ; test_case "local" `Quick test_local_provider_bridge
        ; test_case
            "non-zai provider_k stays provider_d compat"
            `Quick
            test_non_zai_glm_stays_provider_d_compat
        ; test_case "zai provider_k becomes provider_k" `Quick test_zai_glm_becomes_glm_provider_config
        ; test_case
            "zai coding auto uses coding default model"
            `Quick
            test_zai_coding_auto_uses_coding_default_model
        ; test_case
            "provider_c custom provider becomes provider_c"
            `Quick
            test_provider_c_custom_registered_becomes_provider_c_provider_config
        ; test_case
            "zai coding auto models default order"
            `Quick
            test_zai_coding_auto_models_default_order
        ] )
    ]
;;
