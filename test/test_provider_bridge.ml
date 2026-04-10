(** Tests for Provider_bridge: legacy Provider.config -> Provider_config.t *)

let test_anthropic_bridge () =
  let legacy = Agent_sdk.Provider.anthropic_sonnet () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
      (* Expected in test env without ANTHROPIC_API_KEY *)
      Alcotest.(check pass) "missing key = expected in test" () ()
  | Ok cfg ->
      Alcotest.(check string) "model" "claude-sonnet-4-6" cfg.model_id;
      Alcotest.(check string) "path" "/v1/messages" cfg.request_path

let test_openai_compat_bridge () =
  let legacy = Agent_sdk.Provider.openrouter () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
      Alcotest.(check pass) "missing key = expected in test" () ()
  | Ok cfg ->
      Alcotest.(check string) "path" "/chat/completions" cfg.request_path

let test_local_provider_bridge () =
  let legacy = Agent_sdk.Provider.local_llm () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
      Alcotest.fail "local provider should not need env var"
  | Ok cfg ->
      Alcotest.(check string) "model" "default"
        cfg.model_id;
      Alcotest.(check string) "path" "/v1/chat/completions" cfg.request_path

let test_cascade_bridge () =
  let primary = Agent_sdk.Provider.local_llm () in
  let fallback = Agent_sdk.Provider.local_llm () in
  let legacy = Agent_sdk.Provider.cascade ~primary ~fallbacks:[fallback] in
  match Agent_sdk.Provider_bridge.cascade_to_provider_config legacy with
  | Error _ ->
      Alcotest.fail "local cascade should not need env var"
  | Ok casc ->
      Alcotest.(check int) "1 fallback" 1
        (List.length casc.fallbacks)

let test_non_zai_glm_stays_openai_compat () =
  let legacy = {
    Agent_sdk.Provider.provider = OpenAICompat {
      base_url = "https://openrouter.ai/api/v1";
      auth_header = None;
      path = "/chat/completions";
      static_token = None;
    };
    model_id = "glm-5";
    api_key_env = "";
  } in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
      Alcotest.fail "custom openai compat provider should not need env var"
  | Ok cfg ->
      Alcotest.(check string) "kind remains openai compat" "openai_compat"
        (match cfg.kind with
         | Llm_provider.Provider_config.OpenAI_compat -> "openai_compat"
         | Anthropic -> "anthropic"
         | Gemini -> "gemini"
         | Glm -> "glm"
         | Ollama -> "ollama"
         | Claude_code -> "claude_code")

let () =
  let open Alcotest in
  run "provider_bridge" [
    "to_provider_config", [
      test_case "anthropic" `Quick test_anthropic_bridge;
      test_case "openai compat" `Quick test_openai_compat_bridge;
      test_case "local" `Quick test_local_provider_bridge;
      test_case "non-zai glm stays openai compat" `Quick
        test_non_zai_glm_stays_openai_compat;
    ];
    "cascade", [
      test_case "cascade bridge" `Quick test_cascade_bridge;
    ];
  ]
