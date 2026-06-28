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

let with_provider_catalog json f =
  match Llm_provider.Provider_catalog.of_json (Yojson.Safe.from_string json) with
  | Error msg -> Alcotest.fail msg
  | Ok catalog ->
    Llm_provider.Provider_catalog.set_global catalog;
    Fun.protect ~finally:Llm_provider.Provider_catalog.clear_global f
;;

let catalog_json =
  {|
{
  "schema_version": 1,
  "providers": [
    {
      "id": "subscriber-local",
      "aliases": ["Subscriber-Alias"],
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "http://127.0.0.1:8123",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "default_model": "local-model",
      "capabilities_base": "openai_chat",
      "capabilities": {"supports_tools": true},
      "credential_scope": "test runtime"
    }
  ]
}
|}
;;

let catalog_variants_json =
  {|
{
  "schema_version": 1,
  "providers": [
    {
      "id": "custom-rich",
      "aliases": ["Rich-Alias"],
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "https://rich.example/v1/",
      "request_path": "/chat/completions",
      "auth": {"type": "setup_token_env", "env": "RICH_SETUP_TOKEN"},
      "default_model": "rich-default",
      "capabilities_base": "openai_chat"
    },
    {
      "id": "managed-oauth",
      "kind": "openai_compat",
      "transport": "managed",
      "auth": {"type": "oauth_cached_login"},
      "capabilities_base": "openai_chat"
    },
    {
      "id": "api-key-auth",
      "kind": "openai_compat",
      "auth": {"type": "api_key_env", "env": "API_KEY_AUTH"},
      "capabilities_base": "openai_chat"
    }
  ]
}
|}
;;

let transport_to_string = function
  | Provider_runtime_binding.Http -> "http"
  | Provider_runtime_binding.Managed -> "managed"
;;

let auth_to_string = function
  | Provider_runtime_binding.No_auth -> "none"
  | Provider_runtime_binding.Api_key_env env -> "api_key_env:" ^ env
  | Provider_runtime_binding.Oauth_cached_login -> "oauth"
  | Provider_runtime_binding.Setup_token_env env -> "setup:" ^ env
;;

let expect_binding label =
  match Provider_runtime_binding.find label with
  | Some binding -> binding
  | None -> Alcotest.failf "expected provider binding for %S" label
;;

let test_catalog_alias_default_and_capabilities () =
  with_provider_catalog catalog_json (fun () ->
    let binding = expect_binding " subscriber-alias " in
    Alcotest.(check string) "canonical id" "subscriber-local" binding.id;
    Alcotest.(check (list string)) "aliases" [ "subscriber-alias" ] binding.aliases;
    Alcotest.(check (option string))
      "default model"
      (Some "local-model")
      binding.default_model;
    Alcotest.(check bool) "supports tools" true binding.capabilities.supports_tools;
    Alcotest.(check (option string))
      "credential scope"
      (Some "test runtime")
      binding.credential_scope;
    Alcotest.(check string)
      "resolved default"
      "local-model"
      (Provider_runtime_binding.resolve_model binding ~requested_model:None);
    Alcotest.(check string)
      "requested model wins"
      "explicit-model"
      (Provider_runtime_binding.resolve_model
         binding
         ~requested_model:(Some " explicit-model ")))
;;

let test_catalog_to_provider_config () =
  with_provider_catalog catalog_json (fun () ->
    let binding = expect_binding "subscriber-local" in
    let cfg = Provider_runtime_binding.to_provider_config binding in
    Alcotest.(check string) "model id" "local-model" cfg.model_id;
    Alcotest.(check string) "base url" "http://127.0.0.1:8123" cfg.base_url;
    Alcotest.(check string) "request path" "/v1/chat/completions" cfg.request_path;
    Alcotest.(check bool)
      "kind"
      true
      (cfg.kind = Llm_provider.Provider_config.OpenAI_compat))
;;

let test_binding_for_provider_config_uses_catalog_endpoint () =
  with_provider_catalog catalog_json (fun () ->
    let cfg =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.OpenAI_compat
        ~model_id:"local-model"
        ~base_url:"http://127.0.0.1:8123"
        ~request_path:"/v1/chat/completions"
        ()
    in
    match Provider_runtime_binding.binding_for_provider_config cfg with
    | Some binding ->
      Alcotest.(check string) "catalog binding id" "subscriber-local" binding.id
    | None -> Alcotest.fail "expected catalog binding for provider config")
;;

let test_capabilities_for_provider_config_uses_catalog_capabilities () =
  with_provider_catalog catalog_json (fun () ->
    let cfg =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.OpenAI_compat
        ~model_id:"unlisted-local-model"
        ~base_url:"http://127.0.0.1:8123"
        ~request_path:"/v1/chat/completions"
        ()
    in
    let caps = Provider_runtime_binding.capabilities_for_provider_config cfg in
    Alcotest.(check bool) "catalog supports tools" true caps.supports_tools;
    Alcotest.(check bool)
      "catalog support tool_choice default"
      true
      caps.supports_tool_choice)
;;

let test_capabilities_for_provider_config_honors_override () =
  with_provider_catalog catalog_json (fun () ->
    let cfg =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.OpenAI_compat
        ~model_id:"unlisted-local-model"
        ~base_url:"http://127.0.0.1:8123"
        ~request_path:"/v1/chat/completions"
        ~supports_tool_choice_override:false
        ()
    in
    let caps = Provider_runtime_binding.capabilities_for_provider_config cfg in
    Alcotest.(check bool) "override disables tool choice" false caps.supports_tool_choice)
;;

let test_all_includes_catalog_entry_once () =
  with_provider_catalog catalog_json (fun () ->
    let matches =
      Provider_runtime_binding.all ()
      |> List.filter (fun (binding : Provider_runtime_binding.t) ->
        String.equal binding.id "subscriber-local")
    in
    Alcotest.(check int) "catalog entry count" 1 (List.length matches))
;;

let test_catalog_transport_and_auth_variants () =
  with_provider_catalog catalog_variants_json (fun () ->
    let cases =
      [ "rich-alias", "http", "setup:RICH_SETUP_TOKEN", Some "rich-default", None
      ; "managed-oauth", "managed", "oauth", None, None
      ; "api-key-auth", "http", "api_key_env:API_KEY_AUTH", None, None
      ]
    in
    List.iter
      (fun (label, transport, auth, default_model, command) ->
         let binding = expect_binding label in
         Alcotest.(check string)
           (label ^ " transport")
           transport
           (transport_to_string binding.transport);
         Alcotest.(check string) (label ^ " auth") auth (auth_to_string binding.auth);
         Alcotest.(check (option string))
           (label ^ " default model")
           default_model
           binding.default_model;
         Alcotest.(check (option string)) (label ^ " command") command binding.command)
      cases)
;;

let test_find_empty_missing_and_provider_config_fallbacks () =
  Alcotest.(check bool)
    "empty missing"
    true
    (Option.is_none (Provider_runtime_binding.find " "));
  Alcotest.(check bool)
    "unknown missing"
    true
    (Option.is_none (Provider_runtime_binding.find "not-a-provider"));
  with_provider_catalog catalog_variants_json (fun () ->
    let cfg =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.OpenAI_compat
        ~model_id:"rich-default"
        ~base_url:" https://rich.example/v1/// "
        ~request_path:" /chat/completions "
        ()
    in
    match Provider_runtime_binding.binding_for_provider_config cfg with
    | Some binding ->
      Alcotest.(check string) "normalized endpoint match" "custom-rich" binding.id
    | None -> Alcotest.fail "expected normalized endpoint binding")
;;

let test_builtin_binding_resolves () =
  let binding = expect_binding "claude" in
  Alcotest.(check string) "builtin id" "claude" binding.id;
  Alcotest.(check bool)
    "builtin kind"
    true
    (binding.kind = Llm_provider.Provider_config.Anthropic);
  Alcotest.(check string)
    "fallback model"
    Model_registry.default_model_id
    (Provider_runtime_binding.resolve_model binding ~requested_model:None)
;;

let test_builtin_nous_binding_uses_calltime_default_endpoint () =
  with_env "LLM_ENDPOINTS" (Some "") (fun () ->
    with_env
      Llm_provider.Discovery.local_llm_url_env_var
      (Some "http://127.0.0.1:19014")
      (fun () ->
         let binding = expect_binding "nous" in
         Alcotest.(check string)
           "runtime binding base_url"
           "http://127.0.0.1:19014"
           binding.base_url))
;;

let test_non_claude_builtin_defaults_do_not_use_claude_model () =
  [ "gemini"; "glm"; "kimi"; "dashscope"; "deepseek"; "groq"; "openrouter" ]
  |> List.iter (fun provider_id ->
    let binding = expect_binding provider_id in
    let model = Provider_runtime_binding.resolve_model binding ~requested_model:None in
    Alcotest.(check bool)
      (provider_id ^ " default is not the Claude default")
      true
      (not (String.equal model Model_registry.default_model_id));
    Alcotest.(check bool)
      (provider_id ^ " default is not a Claude model id")
      true
      (not (String.starts_with ~prefix:"claude-" model)))
;;

let test_builtin_aliases_are_canonicalized () =
  let cases =
    [ "anthropic", "claude"
    ; "anthropic", "claude"
    ; "kimi", "kimi"
    ; "gemini", "gemini"
    ; "glm", "glm"
    ; "dashscope", "dashscope"
    ]
  in
  List.iter
    (fun (input, expected_id) ->
       let binding = expect_binding input in
       Alcotest.(check string) input expected_id binding.id)
    cases;
  Alcotest.(check bool)
    "openai_compat is a kind, not a provider selector"
    true
    (Option.is_none (Provider_runtime_binding.find "openai_compat"))
;;

let test_provider_id_fallbacks_do_not_invent_openai () =
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"unlisted-model"
      ~base_url:"https://unlisted.example/v1"
      ~request_path:"/chat/completions"
      ()
  in
  Alcotest.(check string)
    "unmatched openai compat"
    "openai_compat"
    (Provider_runtime_binding.provider_id_of_provider_config cfg);
  let config : Provider.config =
    { provider =
        Provider.OpenAICompat
          { base_url = "https://unlisted.example/v1"
          ; auth_header = None
          ; path = "/chat/completions"
          ; static_token = None
          }
    ; model_id = "unlisted-model"
    ; api_key_env = "UNLISTED_API_KEY"
    }
  in
  Alcotest.(check string)
    "config unmatched openai compat"
    "openai_compat"
    (Provider_runtime_binding.provider_id_of_config config)
;;

let () =
  Alcotest.run
    "Provider_runtime_binding"
    [ ( "catalog"
      , [ Alcotest.test_case
            "alias default capabilities"
            `Quick
            test_catalog_alias_default_and_capabilities
        ; Alcotest.test_case "to provider config" `Quick test_catalog_to_provider_config
        ; Alcotest.test_case
            "binding for provider config"
            `Quick
            test_binding_for_provider_config_uses_catalog_endpoint
        ; Alcotest.test_case
            "capabilities for provider config"
            `Quick
            test_capabilities_for_provider_config_uses_catalog_capabilities
        ; Alcotest.test_case
            "capabilities honor tool_choice override"
            `Quick
            test_capabilities_for_provider_config_honors_override
        ; Alcotest.test_case
            "all includes catalog once"
            `Quick
            test_all_includes_catalog_entry_once
        ; Alcotest.test_case
            "transport and auth variants"
            `Quick
            test_catalog_transport_and_auth_variants
        ; Alcotest.test_case
            "find and provider config fallbacks"
            `Quick
            test_find_empty_missing_and_provider_config_fallbacks
        ] )
    ; ( "builtins"
      , [ Alcotest.test_case "builtin resolves" `Quick test_builtin_binding_resolves
        ; Alcotest.test_case
            "nous binding uses call-time default endpoint"
            `Quick
            test_builtin_nous_binding_uses_calltime_default_endpoint
        ; Alcotest.test_case
            "non-Claude builtin defaults do not use Claude model"
            `Quick
            test_non_claude_builtin_defaults_do_not_use_claude_model
        ; Alcotest.test_case
            "builtin aliases canonicalize"
            `Quick
            test_builtin_aliases_are_canonicalized
        ; Alcotest.test_case
            "provider id fallback"
            `Quick
            test_provider_id_fallbacks_do_not_invent_openai
        ] )
    ]
;;
