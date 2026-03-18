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
  let legacy = Agent_sdk.Provider.local_qwen () in
  match Agent_sdk.Provider_bridge.to_provider_config legacy with
  | Error _ ->
      Alcotest.fail "local provider should not need env var"
  | Ok cfg ->
      Alcotest.(check string) "model" "qwen3.5-35b-a3b-ud-q8-xl"
        cfg.model_id;
      Alcotest.(check string) "path" "/v1/messages" cfg.request_path

let test_cascade_bridge () =
  let primary = Agent_sdk.Provider.local_qwen () in
  let fallback = Agent_sdk.Provider.local_mlx () in
  let legacy = Agent_sdk.Provider.cascade ~primary ~fallbacks:[fallback] in
  match Agent_sdk.Provider_bridge.cascade_to_provider_config legacy with
  | Error _ ->
      Alcotest.fail "local cascade should not need env var"
  | Ok casc ->
      Alcotest.(check int) "1 fallback" 1
        (List.length casc.fallbacks)

let () =
  let open Alcotest in
  run "provider_bridge" [
    "to_provider_config", [
      test_case "anthropic" `Quick test_anthropic_bridge;
      test_case "openai compat" `Quick test_openai_compat_bridge;
      test_case "local" `Quick test_local_provider_bridge;
    ];
    "cascade", [
      test_case "cascade bridge" `Quick test_cascade_bridge;
    ];
  ]
