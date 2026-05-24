open Agent_sdk

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
      "kind": "provider_d_compat",
      "transport": "http",
      "base_url": "http://127.0.0.1:8123",
      "request_path": "/v1/chat/completions",
      "auth": {"type": "none"},
      "default_model": "local-model",
      "capabilities_base": "provider_d_chat",
      "capabilities": {"supports_tools": true},
      "non_interactive": true,
      "interactive_required": false,
      "daemon_safe": true,
      "credential_scope": "test runtime"
    }
  ]
}
|}
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
    Alcotest.(check bool) "non interactive" true binding.non_interactive;
    Alcotest.(check bool) "daemon safe" true binding.daemon_safe;
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
      (cfg.kind = Llm_provider.Provider_config.Provider_d_compat))
;;

let test_binding_for_provider_config_uses_catalog_endpoint () =
  with_provider_catalog catalog_json (fun () ->
    let cfg =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.Provider_d_compat
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
        ~kind:Llm_provider.Provider_config.Provider_d_compat
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
        ~kind:Llm_provider.Provider_config.Provider_d_compat
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

let test_builtin_binding_resolves () =
  let binding = expect_binding "agent_llm_a" in
  Alcotest.(check string) "builtin id" "agent_llm_a" binding.id;
  Alcotest.(check bool)
    "builtin kind"
    true
    (binding.kind = Llm_provider.Provider_config.Provider_a);
  Alcotest.(check string)
    "fallback model"
    Model_registry.default_model_id
    (Provider_runtime_binding.resolve_model binding ~requested_model:None)
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
        ] )
    ; ( "builtins"
      , [ Alcotest.test_case "builtin resolves" `Quick test_builtin_binding_resolves ] )
    ]
;;
