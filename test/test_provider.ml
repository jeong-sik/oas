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

let test_model_spec_local_qwen_capabilities () =
  let spec = Provider.model_spec_of_config (Provider.local_qwen ()) in
  Alcotest.(check string) "request path" "/v1/messages" spec.request_path;
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools;
  Alcotest.(check bool) "supports reasoning" true
    spec.capabilities.supports_reasoning;
  Alcotest.(check bool) "supports native streaming" true
    spec.capabilities.supports_native_streaming

let test_model_spec_openrouter_capabilities () =
  let spec =
    Provider.model_spec_of_config
      (Provider.openrouter ~model_id:"anthropic/claude-sonnet-4-6" ())
  in
  Alcotest.(check string) "request path" "/chat/completions" spec.request_path;
  Alcotest.(check bool) "supports tools" true spec.capabilities.supports_tools;
  Alcotest.(check bool) "supports reasoning" false
    spec.capabilities.supports_reasoning;
  Alcotest.(check bool) "supports top_k" false spec.capabilities.supports_top_k;
  Alcotest.(check bool) "supports json response" true
    spec.capabilities.supports_response_format_json

let test_qwen_family_openai_capabilities () =
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
      Alcotest.test_case "local qwen model spec capabilities" `Quick
        test_model_spec_local_qwen_capabilities;
      Alcotest.test_case "openrouter model spec capabilities" `Quick
        test_model_spec_openrouter_capabilities;
      Alcotest.test_case "qwen family openai capabilities" `Quick
        test_qwen_family_openai_capabilities;
    ];
  ]
