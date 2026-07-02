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

let with_model_catalog toml f =
  let path = Filename.temp_file "oas-provider-runtime-binding-models" ".toml" in
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> output_string oc toml);
  Fun.protect
    ~finally:(fun () ->
      Llm_provider.Model_catalog.clear_global ();
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () ->
       match Llm_provider.Model_catalog.load_file path with
       | Error msg -> Alcotest.fail msg
       | Ok catalog ->
         Llm_provider.Model_catalog.set_global catalog;
         f ())
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
    Alcotest.(check bool) "override disables tool choice" false caps.supports_tool_choice;
    Alcotest.(check bool)
      "override disables required tool choice"
      false
      caps.supports_required_tool_choice;
    Alcotest.(check bool)
      "override disables named tool choice"
      false
      caps.supports_named_tool_choice)
;;

let test_local_openai_compat_capabilities_not_inflated_by_locality () =
  (* RFC-OAS-034: host locality must not grant the extended capability preset.
     A local OpenAI-compatible endpoint on a non-default port serving an
     uncatalogued model resolves to base openai_compat capabilities — reasoning
     and extended thinking are NOT inferred from the endpoint being local. Before
     the fix, [is_local -> "nous"] routed this to the extended preset. *)
  let cfg =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"uncatalogued-local-model"
      ~base_url:"http://127.0.0.1:8199"
      ~request_path:"/v1/chat/completions"
      ()
  in
  let caps = Provider_runtime_binding.capabilities_for_provider_config cfg in
  Alcotest.(check bool) "no reasoning from locality" false caps.supports_reasoning;
  Alcotest.(check bool)
    "no extended thinking from locality"
    false
    caps.supports_extended_thinking;
  Alcotest.(check bool)
    "no reasoning budget from locality"
    false
    caps.supports_reasoning_budget
;;

let test_capabilities_host_invariant_local_vs_remote () =
  (* RFC-OAS-034: capability = f(runtime x model), not f(host). The same kind +
     uncatalogued model resolves to the same reasoning capability whether the
     endpoint is local (non-default port) or remote. *)
  let make base_url =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_config.OpenAI_compat
      ~model_id:"uncatalogued-model"
      ~base_url
      ~request_path:"/v1/chat/completions"
      ()
  in
  let local =
    Provider_runtime_binding.capabilities_for_provider_config
      (make "http://127.0.0.1:8199")
  in
  let remote =
    Provider_runtime_binding.capabilities_for_provider_config
      (make "https://remote.example/v1")
  in
  Alcotest.(check bool)
    "reasoning capability is host-invariant"
    remote.supports_reasoning
    local.supports_reasoning;
  Alcotest.(check bool)
    "both resolve to base (no reasoning)"
    false
    local.supports_reasoning
;;

let test_catalog_structured_output_capability_projects_to_provider_config () =
  with_provider_catalog catalog_json (fun () ->
    let binding = expect_binding "subscriber-local" in
    let cfg =
      Provider_runtime_binding.to_provider_config ~model:"qwen/qwen3.6-35b-a3b" binding
    in
    let schema = `Assoc [ "type", `String "object" ] in
    let cfg =
      { cfg with
        Llm_provider.Provider_config.response_format = Types.JsonSchema schema
      ; output_schema = Some schema
      }
    in
    Alcotest.(check bool)
      "catalog endpoint accepts declared structured output"
      true
      (Result.is_ok (Llm_provider.Provider_config.validate_output_schema_request cfg)))
;;

let test_capabilities_for_provider_config_uses_provider_qualified_model_catalog () =
  with_model_catalog
    {|
[[models]]
id_prefix = "kimi-k2.6"
base = "kimi"
supports_tools = true
supports_tool_choice = true
thinking_control_format = "none"

[[models]]
id_prefix = "ollama_cloud/kimi-k2.6"
base = "ollama_cloud"
supports_tools = true
supports_tool_choice = false
supports_reasoning = true
supports_extended_thinking = true
supports_reasoning_budget = true
thinking_control_format = "reasoning_effort"
reasoning_replay = "preserve_always"
|}
    (fun () ->
       let cfg =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"kimi-k2.6"
           ~base_url:"https://ollama.com/v1"
           ~request_path:"/chat/completions"
           ()
       in
       let caps = Provider_runtime_binding.capabilities_for_provider_config cfg in
       Alcotest.(check bool)
         "ollama cloud row disables forced tool_choice"
         false
         caps.supports_tool_choice;
       Alcotest.(check bool)
         "ollama cloud row disables named forced tool_choice"
         false
         caps.supports_named_tool_choice;
       Alcotest.(check bool)
         "ollama cloud row uses reasoning_effort"
         true
         (caps.thinking_control_format = Llm_provider.Capabilities.Reasoning_effort);
       Alcotest.(check bool)
         "ollama cloud row preserves reasoning replay"
         true
         (caps.reasoning_replay_override = Llm_provider.Capabilities.Force_preserve_always))
;;

let test_capabilities_for_provider_config_uses_dot_qualified_model_catalog () =
  with_provider_catalog
    {|
{
  "schema_version": 1,
  "providers": [
    {
      "id": "vllm-qwen3-mtp",
      "kind": "openai_compat",
      "transport": "http",
      "base_url": "https://runpod.example.invalid/v1",
      "request_path": "/chat/completions",
      "auth": {"type": "none"},
      "capabilities_base": "openai_chat"
    }
  ]
}
|}
    (fun () ->
       with_model_catalog
         {|
[[models]]
id_prefix = "vllm-qwen3-mtp/qwen36-35b-a3b-mtp"
base = "openai_chat"
provider_name = "vllm-qwen3-mtp"
supports_tools = true
supports_tool_choice = true
supports_parallel_tool_calls = true
supports_reasoning = true
supports_extended_thinking = true
supports_reasoning_budget = true
thinking_control_format = "chat_template_kwargs"
preserve_thinking_control_format = "chat_template_kwargs_preserve_thinking"
|}
         (fun () ->
            let cfg =
              Llm_provider.Provider_config.make
                ~kind:Llm_provider.Provider_config.OpenAI_compat
                ~model_id:"vllm-qwen3-mtp.qwen36-35b-a3b-mtp"
                ~base_url:"https://runpod.example.invalid/v1"
                ~request_path:"/chat/completions"
                ()
            in
            let caps = Provider_runtime_binding.capabilities_for_provider_config cfg in
            Alcotest.(check bool)
              "vllm-qwen3-mtp row keeps tools"
              true
              caps.supports_tools;
            Alcotest.(check bool)
              "vllm-qwen3-mtp row keeps parallel tools"
              true
              caps.supports_parallel_tool_calls;
            Alcotest.(check bool)
              "vllm-qwen3-mtp row keeps reasoning"
              true
              caps.supports_reasoning;
            Alcotest.(check bool)
              "vllm-qwen3-mtp row uses chat_template_kwargs"
              true
              (caps.thinking_control_format
               = Llm_provider.Capabilities.Chat_template_kwargs)))
;;

let expect_tool_choice_ok label cfg =
  match Llm_provider.Provider_config.validate_tool_choice_request_typed cfg with
  | Ok () -> ()
  | Error rejection ->
    Alcotest.failf
      "%s unexpectedly rejected tool_choice: %s"
      label
      (Llm_provider.Provider_config.tool_choice_request_rejection_to_message rejection)
;;

let expect_named_tool_choice_rejected label cfg =
  match Llm_provider.Provider_config.validate_tool_choice_request_typed cfg with
  | Error (Llm_provider.Provider_config.Unsupported_named_tool_choice { tool_name; _ }) ->
    Alcotest.(check string) (label ^ " tool") "calc" tool_name
  | Error rejection ->
    Alcotest.failf
      "%s rejected named forced tool_choice with unexpected reason: %s"
      label
      (Llm_provider.Provider_config.tool_choice_request_rejection_to_message rejection)
  | Ok () -> Alcotest.failf "%s unexpectedly accepted named forced tool_choice" label
;;

let expect_required_tool_choice_rejected label cfg =
  match Llm_provider.Provider_config.validate_tool_choice_request_typed cfg with
  | Error (Llm_provider.Provider_config.Unsupported_required_tool_choice _) -> ()
  | Error rejection ->
    Alcotest.failf
      "%s rejected required forced tool_choice with unexpected reason: %s"
      label
      (Llm_provider.Provider_config.tool_choice_request_rejection_to_message rejection)
  | Ok () -> Alcotest.failf "%s unexpectedly accepted required forced tool_choice" label
;;

let request_tool_choice_field cfg =
  let body =
    Llm_provider.Backend_openai.build_request ~config:cfg ~messages:[] ()
    |> Yojson.Safe.from_string
  in
  match body with
  | `Assoc fields -> List.assoc_opt "tool_choice" fields
  | _ -> Alcotest.fail "expected request body object"
;;

let expect_named_tool_choice_serialized label cfg =
  match request_tool_choice_field cfg with
  | Some (`Assoc fields) ->
    Alcotest.(check string)
      (label ^ " type")
      "function"
      (Yojson.Safe.Util.to_string (List.assoc "type" fields));
    let function_json = List.assoc "function" fields in
    Alcotest.(check string)
      (label ^ " name")
      "calc"
      Yojson.Safe.Util.(function_json |> member "name" |> to_string)
  | Some json ->
    Alcotest.failf
      "%s expected named tool_choice object, got %s"
      label
      (Yojson.Safe.to_string json)
  | None -> Alcotest.failf "%s expected tool_choice in request body" label
;;

let expect_no_tool_choice_field label cfg =
  match request_tool_choice_field cfg with
  | None -> ()
  | Some json ->
    Alcotest.failf
      "%s unexpectedly serialized tool_choice: %s"
      label
      (Yojson.Safe.to_string json)
;;

let test_forced_tool_choice_provider_invariants () =
  with_model_catalog
    {|
[[models]]
id_prefix = "claude-opus-4-6"
base = "anthropic"
supports_tools = true
supports_tool_choice = true
supports_required_tool_choice = true
supports_named_tool_choice = true

[[models]]
id_prefix = "glm-5.1"
base = "glm"
supports_tools = true
supports_tool_choice = true
supports_required_tool_choice = false
supports_named_tool_choice = false

[[models]]
id_prefix = "minimax-m3"
base = "openai_chat"
supports_tools = true
supports_tool_choice = false
supports_required_tool_choice = false
supports_named_tool_choice = false

[[models]]
id_prefix = "ollama_cloud/minimax-m3"
base = "ollama_cloud"
supports_tools = true
supports_tool_choice = false
supports_required_tool_choice = false
supports_named_tool_choice = false
|}
    (fun () ->
       let named = Types.Tool "calc" in
       let anthropic =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.Anthropic
           ~model_id:"claude-opus-4-6"
           ~base_url:"https://api.anthropic.com"
           ~tool_choice:named
           ()
       in
       expect_tool_choice_ok "anthropic named" anthropic;
       let minimax =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"minimax-m3"
           ~base_url:"https://api.minimax.chat/v1"
           ~tool_choice:named
           ()
       in
       expect_named_tool_choice_rejected "minimax named" minimax;
       let minimax_any =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"minimax-m3"
           ~base_url:"https://api.minimax.chat/v1"
           ~tool_choice:Types.Any
           ()
       in
       expect_required_tool_choice_rejected "minimax any" minimax_any;
       let minimax_auto =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"minimax-m3"
           ~base_url:"https://api.minimax.chat/v1"
           ~tool_choice:Types.Auto
           ()
       in
       expect_tool_choice_ok "minimax auto" minimax_auto;
       expect_no_tool_choice_field "minimax auto" minimax_auto;
       let glm =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.Glm
           ~model_id:"glm-5.1"
           ~base_url:Llm_provider.Zai_catalog.coding_base_url
           ~tool_choice:named
           ()
       in
       expect_named_tool_choice_rejected "glm named" glm;
       let glm_any =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.Glm
           ~model_id:"glm-5.1"
           ~base_url:Llm_provider.Zai_catalog.coding_base_url
           ~tool_choice:Types.Any
           ()
       in
       expect_required_tool_choice_rejected "glm any" glm_any;
       let bare_zai_glm =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"glm-5.1"
           ~base_url:Llm_provider.Zai_catalog.coding_base_url
           ~tool_choice:named
           ()
       in
       expect_named_tool_choice_rejected "bare zai glm named" bare_zai_glm;
       let bare_zai_glm_any =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"glm-5.1"
           ~base_url:Llm_provider.Zai_catalog.coding_base_url
           ~tool_choice:Types.Any
           ()
       in
       expect_required_tool_choice_rejected "bare zai glm any" bare_zai_glm_any;
       let hosted_minimax_named =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"minimax-m3"
           ~base_url:"https://ollama.com/v1"
           ~request_path:"/chat/completions"
           ~tool_choice:named
           ()
       in
       expect_named_tool_choice_rejected "ollama cloud minimax named" hosted_minimax_named;
       let hosted_minimax_any =
         Llm_provider.Provider_config.make
           ~kind:Llm_provider.Provider_config.OpenAI_compat
           ~model_id:"minimax-m3"
           ~base_url:"https://ollama.com/v1"
           ~request_path:"/chat/completions"
           ~tool_choice:Types.Any
           ()
       in
       expect_required_tool_choice_rejected "ollama cloud minimax any" hosted_minimax_any)
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
            "structured output projects to provider config"
            `Quick
            test_catalog_structured_output_capability_projects_to_provider_config
        ; Alcotest.test_case
            "provider-qualified model catalog capabilities"
            `Quick
            test_capabilities_for_provider_config_uses_provider_qualified_model_catalog
        ; Alcotest.test_case
            "dot-qualified model catalog capabilities"
            `Quick
            test_capabilities_for_provider_config_uses_dot_qualified_model_catalog
        ; Alcotest.test_case
            "forced tool_choice provider invariants"
            `Quick
            test_forced_tool_choice_provider_invariants
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
        ; Alcotest.test_case
            "local capabilities not inflated by locality"
            `Quick
            test_local_openai_compat_capabilities_not_inflated_by_locality
        ; Alcotest.test_case
            "capabilities host-invariant local vs remote"
            `Quick
            test_capabilities_host_invariant_local_vs_remote
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
