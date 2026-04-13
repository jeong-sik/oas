open Agent_sdk

let with_test_providers_enabled f =
  let previous = Sys.getenv_opt "OAS_ALLOW_TEST_PROVIDERS" in
  Unix.putenv "OAS_ALLOW_TEST_PROVIDERS" "1";
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some value -> Unix.putenv "OAS_ALLOW_TEST_PROVIDERS" value
      | None -> Unix.putenv "OAS_ALLOW_TEST_PROVIDERS" "")
    f

let dummy_session : Runtime.session =
  {
    session_id = "test-sess";
    goal = "test";
    title = None;
    tag = None;
    permission_mode = None;
    phase = Runtime.Running;
    created_at = 0.0;
    updated_at = 0.0;
    provider = None;
    model = None;
    system_prompt = None;
    max_turns = 10;
    workdir = None;
    planned_participants = [];
    participants = [];
    artifacts = [];
    turn_count = 0;
    last_seq = 0;
    outcome = None;
  }

let dummy_spawn : Runtime.spawn_agent_request =
  {
    participant_name = "agent-1";
    role = Some "execute";
    prompt = "do something";
    provider = None;
    model = None;
    system_prompt = None;
    max_turns = None;
  }

let test_provider_runtime_name_none_cfg () =
  Alcotest.(check string) "selected" "my-provider"
    (Runtime_server_resolve.provider_runtime_name "my-provider" None)

let test_provider_runtime_name_local () =
  let cfg =
    Some
      {
        Provider.provider =
          Provider.Local { base_url = Llm_provider.Constants.Endpoints.default_url_localhost };
        model_id = "test";
        api_key_env = "K";
      }
  in
  Alcotest.(check string) "local" "local"
    (Runtime_server_resolve.provider_runtime_name "local" cfg)

let test_provider_runtime_name_anthropic () =
  let cfg =
    Some
      {
        Provider.provider = Provider.Anthropic;
        model_id = "claude-sonnet-4-6";
        api_key_env = "K";
      }
  in
  Alcotest.(check string) "anthropic" "anthropic"
    (Runtime_server_resolve.provider_runtime_name "anthropic" cfg)

let test_provider_runtime_name_openai_compat () =
  let cfg =
    Some
      {
        Provider.provider =
          Provider.OpenAICompat
            {
              base_url = "https://api.openai.com";
              auth_header = None;
              path = "/v1/chat/completions";
              static_token = None;
            };
        model_id = "gpt-4o";
        api_key_env = "K";
      }
  in
  Alcotest.(check string) "openai-compat" "openai-compat"
    (Runtime_server_resolve.provider_runtime_name "openai" cfg)

let test_provider_runtime_name_custom_registered () =
  let cfg =
    Some
      {
        Provider.provider = Provider.Custom_registered { name = "myvendor" };
        model_id = "test";
        api_key_env = "K";
      }
  in
  Alcotest.(check string) "custom:myvendor" "custom:myvendor"
    (Runtime_server_resolve.provider_runtime_name "custom" cfg)

let test_resolve_provider_mock () =
  with_test_providers_enabled (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"mock" () with
    | Ok None -> ()
    | _ -> Alcotest.fail "expected Ok None")

let test_resolve_provider_echo () =
  with_test_providers_enabled (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"echo" () with
    | Ok None -> ()
    | _ -> Alcotest.fail "expected Ok None")

let test_resolve_provider_mock_rejected_by_default () =
  match Runtime_server_resolve.resolve_provider ~provider:"mock" () with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> ()
  | _ -> Alcotest.fail "expected test-only provider rejection"

let test_resolve_provider_local () =
  match Runtime_server_resolve.resolve_provider ~provider:"local" () with
  | Ok (Some cfg) -> (
      match cfg.Provider.provider with
      | Provider.Local _ -> ()
      | _ -> Alcotest.fail "expected local provider")
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_local_qwen () =
  match Runtime_server_resolve.resolve_provider ~provider:"local-qwen" () with
  | Ok (Some cfg) -> (
      match cfg.Provider.provider with
      | Provider.Local _ -> ()
      | _ -> Alcotest.fail "expected local provider")
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_sonnet () =
  match Runtime_server_resolve.resolve_provider ~provider:"sonnet" () with
  | Ok (Some cfg) -> Alcotest.(check bool) "anthropic" true (cfg.Provider.provider = Provider.Anthropic)
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_haiku () =
  match Runtime_server_resolve.resolve_provider ~provider:"haiku" () with
  | Ok (Some cfg) -> Alcotest.(check bool) "anthropic" true (cfg.Provider.provider = Provider.Anthropic)
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_opus () =
  match Runtime_server_resolve.resolve_provider ~provider:"opus" () with
  | Ok (Some cfg) -> Alcotest.(check bool) "anthropic" true (cfg.Provider.provider = Provider.Anthropic)
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_openrouter () =
  match Runtime_server_resolve.resolve_provider ~provider:"openrouter" () with
  | Ok (Some cfg) -> (
      match cfg.Provider.provider with
      | Provider.OpenAICompat _ -> ()
      | _ -> Alcotest.fail "expected OpenAICompat")
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_unknown () =
  match Runtime_server_resolve.resolve_provider ~provider:"unknown-provider" () with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> ()
  | _ -> Alcotest.fail "expected UnsupportedProvider"

let test_resolve_provider_typoed () =
  match Runtime_server_resolve.resolve_provider ~provider:"openriauter" () with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> ()
  | _ -> Alcotest.fail "expected UnsupportedProvider"

let test_resolve_provider_empty_uses_fallback () =
  match Runtime_server_resolve.resolve_provider ~provider:"  " () with
  | Ok (Some _) -> ()
  | _ -> Alcotest.fail "expected fallback provider"

let test_resolve_provider_model_override () =
  match Runtime_server_resolve.resolve_provider ~provider:"sonnet" ~model:"my-custom-model" () with
  | Ok (Some cfg) ->
      Alcotest.(check string) "model override" "my-custom-model" cfg.Provider.model_id
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_empty_model_uses_default () =
  match Runtime_server_resolve.resolve_provider ~provider:"sonnet" ~model:"  " () with
  | Ok (Some cfg) ->
      Alcotest.(check bool) "non-empty" true (String.length cfg.Provider.model_id > 0)
  | _ -> Alcotest.fail "expected Ok (Some cfg)"

let test_resolve_provider_trimmed_lowercased () =
  with_test_providers_enabled (fun () ->
    match Runtime_server_resolve.resolve_provider ~provider:"  MOCK  " () with
    | Ok None -> ()
    | _ -> Alcotest.fail "expected mock alias to resolve to None")

let test_resolve_execution_mock_provider () =
  with_test_providers_enabled (fun () ->
    let detail = { dummy_spawn with provider = Some "mock" } in
    match Runtime_server_resolve.resolve_execution dummy_session detail with
    | Ok res ->
        Alcotest.(check string) "selected" "mock" res.selected_provider;
        Alcotest.(check bool) "no provider cfg" true (Option.is_none res.provider_cfg);
        Alcotest.(check (option string)) "resolved provider" (Some "mock") res.resolved_provider
    | Error _ -> Alcotest.fail "expected Ok")

let test_resolve_execution_requested_model_from_detail () =
  let detail = { dummy_spawn with model = Some "my-model" } in
  match Runtime_server_resolve.resolve_execution dummy_session detail with
  | Ok res -> Alcotest.(check (option string)) "requested model" (Some "my-model") res.requested_model
  | Error _ -> Alcotest.fail "expected Ok"

let test_resolve_execution_requested_model_none_when_detail_empty () =
  let detail = { dummy_spawn with model = Some "  " } in
  match Runtime_server_resolve.resolve_execution dummy_session detail with
  | Ok res -> Alcotest.(check bool) "requested model none" true (Option.is_none res.requested_model)
  | Error _ -> Alcotest.fail "expected Ok"

let test_resolve_execution_non_mock_has_provider_cfg () =
  let detail = { dummy_spawn with provider = Some "sonnet" } in
  match Runtime_server_resolve.resolve_execution dummy_session detail with
  | Ok res ->
      Alcotest.(check bool) "has provider cfg" true (Option.is_some res.provider_cfg);
      Alcotest.(check (option string)) "resolved provider" (Some "anthropic")
        res.resolved_provider
  | Error _ -> Alcotest.fail "expected Ok"

let test_resolve_execution_unknown_provider_returns_error () =
  let detail = { dummy_spawn with provider = Some "bogus-provider" } in
  match Runtime_server_resolve.resolve_execution dummy_session detail with
  | Error (Error.Config (Error.UnsupportedProvider _)) -> ()
  | _ -> Alcotest.fail "expected UnsupportedProvider"

let test_resolve_execution_mock_resolved_model_uses_session_model () =
  with_test_providers_enabled (fun () ->
    let session = { dummy_session with model = Some "sess-model" } in
    let detail = { dummy_spawn with provider = Some "mock" } in
    match Runtime_server_resolve.resolve_execution session detail with
    | Ok res -> Alcotest.(check (option string)) "session model" (Some "sess-model") res.resolved_model
    | Error _ -> Alcotest.fail "expected Ok")

let test_resolve_execution_mock_requested_model_takes_priority () =
  with_test_providers_enabled (fun () ->
    let session = { dummy_session with model = Some "sess-model" } in
    let detail = { dummy_spawn with provider = Some "mock"; model = Some "req-model" } in
    match Runtime_server_resolve.resolve_execution session detail with
    | Ok res -> Alcotest.(check (option string)) "requested model" (Some "req-model") res.resolved_model
    | Error _ -> Alcotest.fail "expected Ok")

let test_resolve_execution_echo_provider () =
  with_test_providers_enabled (fun () ->
    let detail = { dummy_spawn with provider = Some "echo" } in
    match Runtime_server_resolve.resolve_execution dummy_session detail with
    | Ok res ->
        Alcotest.(check string) "selected" "echo" res.selected_provider;
        Alcotest.(check bool) "no provider cfg" true (Option.is_none res.provider_cfg)
    | Error _ -> Alcotest.fail "expected Ok")

let test_resolve_execution_detail_priority () =
  with_test_providers_enabled (fun () ->
    let session = { dummy_session with provider = Some "sonnet" } in
    let detail = { dummy_spawn with provider = Some "mock" } in
    match Runtime_server_resolve.resolve_execution session detail with
    | Ok res -> Alcotest.(check string) "priority" "mock" res.selected_provider
    | Error _ -> Alcotest.fail "expected Ok")

let test_resolve_execution_session_provider () =
  let session = { dummy_session with provider = Some "haiku" } in
  match Runtime_server_resolve.resolve_execution session dummy_spawn with
  | Ok res -> Alcotest.(check string) "session provider" "haiku" res.selected_provider
  | Error _ -> Alcotest.fail "expected Ok"

let test_resolve_execution_fallback () =
  match Runtime_server_resolve.resolve_execution dummy_session dummy_spawn with
  | Ok res ->
      Alcotest.(check string) "fallback" Defaults.fallback_provider res.selected_provider
  | Error _ -> Alcotest.fail "expected Ok"

let () =
  Alcotest.run "Runtime_server_resolve"
    [
      ( "provider_runtime_name",
        [
          Alcotest.test_case "None cfg returns selected" `Quick
            test_provider_runtime_name_none_cfg;
          Alcotest.test_case "Local provider" `Quick
            test_provider_runtime_name_local;
          Alcotest.test_case "Anthropic provider" `Quick
            test_provider_runtime_name_anthropic;
          Alcotest.test_case "OpenAICompat provider" `Quick
            test_provider_runtime_name_openai_compat;
          Alcotest.test_case "Custom_registered provider" `Quick
            test_provider_runtime_name_custom_registered;
        ] );
      ( "resolve_provider",
        [
          Alcotest.test_case "mock returns Ok None" `Quick
            test_resolve_provider_mock;
          Alcotest.test_case "echo returns Ok None" `Quick
            test_resolve_provider_echo;
          Alcotest.test_case "mock rejected by default" `Quick
            test_resolve_provider_mock_rejected_by_default;
          Alcotest.test_case "local returns Local provider" `Quick
            test_resolve_provider_local;
          Alcotest.test_case "local-qwen returns Local provider" `Quick
            test_resolve_provider_local_qwen;
          Alcotest.test_case "sonnet returns Anthropic" `Quick
            test_resolve_provider_sonnet;
          Alcotest.test_case "haiku returns Anthropic" `Quick
            test_resolve_provider_haiku;
          Alcotest.test_case "opus returns Anthropic" `Quick
            test_resolve_provider_opus;
          Alcotest.test_case "openrouter returns OpenAICompat" `Quick
            test_resolve_provider_openrouter;
          Alcotest.test_case "unknown returns UnsupportedProvider error" `Quick
            test_resolve_provider_unknown;
          Alcotest.test_case "typoed provider returns error" `Quick
            test_resolve_provider_typoed;
          Alcotest.test_case "empty provider uses fallback" `Quick
            test_resolve_provider_empty_uses_fallback;
          Alcotest.test_case "model override applied" `Quick
            test_resolve_provider_model_override;
          Alcotest.test_case "empty model uses default" `Quick
            test_resolve_provider_empty_model_uses_default;
          Alcotest.test_case "trimmed and lowercased" `Quick
            test_resolve_provider_trimmed_lowercased;
        ] );
      ( "resolve_execution",
        [
          Alcotest.test_case "mock provider has no provider_cfg" `Quick
            test_resolve_execution_mock_provider;
          Alcotest.test_case "requested_model from detail" `Quick
            test_resolve_execution_requested_model_from_detail;
          Alcotest.test_case "requested_model None when detail empty" `Quick
            test_resolve_execution_requested_model_none_when_detail_empty;
          Alcotest.test_case "non-mock has provider_cfg" `Quick
            test_resolve_execution_non_mock_has_provider_cfg;
          Alcotest.test_case "unknown provider returns error" `Quick
            test_resolve_execution_unknown_provider_returns_error;
          Alcotest.test_case "mock resolved_model uses session model" `Quick
            test_resolve_execution_mock_resolved_model_uses_session_model;
          Alcotest.test_case "mock requested_model takes priority" `Quick
            test_resolve_execution_mock_requested_model_takes_priority;
          Alcotest.test_case "echo provider has no provider_cfg" `Quick
            test_resolve_execution_echo_provider;
          Alcotest.test_case "detail provider takes priority over session" `Quick
            test_resolve_execution_detail_priority;
          Alcotest.test_case "session provider used when detail is None" `Quick
            test_resolve_execution_session_provider;
          Alcotest.test_case "fallback when both None" `Quick
            test_resolve_execution_fallback;
        ] );
    ]
