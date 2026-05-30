open Alcotest
open Llm_provider

let contains ~needle text =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop idx =
    if idx + needle_len > text_len
    then false
    else if String.sub text idx needle_len = needle
    then true
    else loop (idx + 1)
  in
  needle_len = 0 || loop 0
;;

let catalog_of_string text =
  match Provider_catalog.of_json (Yojson.Safe.from_string text) with
  | Ok catalog -> catalog
  | Error msg -> fail msg
;;

let require_lookup catalog id =
  match Provider_catalog.lookup catalog id with
  | Some entry -> entry
  | None -> failf "expected catalog entry %S" id
;;

let test_full_entry_parses_auth_transport_and_capabilities () =
  let catalog =
    catalog_of_string
      {|{
        "schema_version": 1,
        "providers": [
          {
            "id": "rich-http",
            "aliases": [" Alias ", "", 7, "second"],
            "kind": "openai_compat",
            "transport": "custom-openai-compat",
            "base_url": "https://rich.example/v1",
            "request_path": "/chat/completions",
            "default_model": "rich-model",
            "credential_scope": "workspace",
            "auth": {"type": "setup-token-env", "key": "SETUP_TOKEN"},
            "max_context": 32000,
            "capabilities_base": "provider_d_chat",
            "capabilities": {
              "max_context_tokens": 64000,
              "max_output_tokens": 4096,
              "supports_tools": true,
              "supports_tool_choice": true,
              "supports_parallel_tool_calls": true,
              "supports_runtime_mcp_tools": true,
              "supports_runtime_tool_events": true,
              "supports_reasoning": true,
              "supports_extended_thinking": true,
              "supports_reasoning_budget": true,
              "supports_response_format_json": true,
              "supports_structured_output": true,
              "supports_multimodal_inputs": true,
              "supports_image_input": true,
              "supports_audio_input": true,
              "supports_video_input": true,
              "supports_native_streaming": true,
              "supports_system_prompt": false,
              "supports_caching": true,
              "supports_prompt_caching": true,
              "prompt_cache_alignment": 128,
              "supports_top_k": true,
              "supports_min_p": true,
              "supports_seed": true,
              "supports_seed_with_images": true,
              "supports_computer_use": true,
              "supports_code_execution": true,
              "emits_usage_tokens": true,
              "thinking_control_format": "chat-template-kwargs",
              "supported_models": [" rich-model ", "", 3, "rich-fast"]
            }
          },
          {
            "id": "cli-cached",
            "kind": "openai_compat",
            "transport": "",
            "command": "tool-a",
            "auth": {"type": "oauth_cached_login"}
          },
          {
            "id": "oauth",
            "kind": "openai_compat",
            "transport": "managed",
            "auth": {"type": "oauth_cached_login"}
          },
          {
            "id": "file-auth",
            "kind": "openai_compat",
            "auth": {"type": "file", "path": "/tmp/provider-token"}
          },
          {
            "id": "exec-auth",
            "kind": "openai_compat",
            "auth": {"type": "exec", "command": "op read token"}
          },
          {
            "id": "legacy-env",
            "kind": "openai_compat",
            "api_key_env": "LEGACY_KEY"
          }
        ]
      }|}
  in
  let rich = require_lookup catalog "alias" in
  check string "id" "rich-http" rich.id;
  check (list string) "aliases" [ "Alias"; "second" ] rich.aliases;
  check
    bool
    "legacy custom transport alias"
    true
    (rich.transport = Provider_catalog.Http);
  check bool "setup auth" true (rich.auth = Provider_catalog.Setup_token_env "SETUP_TOKEN");
  check string "api key from setup auth" "SETUP_TOKEN" rich.api_key_env;
  check (option string) "default model" (Some "rich-model") rich.default_model;
  check (option int) "explicit max context" (Some 32000) rich.max_context;
  check (option string) "credential scope" (Some "workspace") rich.credential_scope;
  let caps = rich.capabilities in
  check (option int) "cap max context" (Some 64000) caps.max_context_tokens;
  check (option int) "cap max output" (Some 4096) caps.max_output_tokens;
  check bool "supports tools" true caps.supports_tools;
  check bool "supports tool choice" true caps.supports_tool_choice;
  check bool "supports parallel calls" true caps.supports_parallel_tool_calls;
  check bool "runtime mcp tools" true caps.supports_runtime_mcp_tools;
  check bool "runtime tool events" true caps.supports_runtime_tool_events;
  check bool "reasoning" true caps.supports_reasoning;
  check bool "extended thinking" true caps.supports_extended_thinking;
  check bool "reasoning budget" true caps.supports_reasoning_budget;
  check bool "json response" true caps.supports_response_format_json;
  check bool "structured output" true caps.supports_structured_output;
  check bool "multimodal" true caps.supports_multimodal_inputs;
  check bool "image" true caps.supports_image_input;
  check bool "audio" true caps.supports_audio_input;
  check bool "video" true caps.supports_video_input;
  check bool "native streaming" true caps.supports_native_streaming;
  check bool "system prompt override" false caps.supports_system_prompt;
  check bool "caching" true caps.supports_caching;
  check bool "prompt caching" true caps.supports_prompt_caching;
  check (option int) "cache alignment" (Some 128) caps.prompt_cache_alignment;
  check bool "top k" true caps.supports_top_k;
  check bool "min p" true caps.supports_min_p;
  check bool "seed" true caps.supports_seed;
  check bool "seed images" true caps.supports_seed_with_images;
  check bool "computer use" true caps.supports_computer_use;
  check bool "code execution" true caps.supports_code_execution;
  check bool "usage tokens" true caps.emits_usage_tokens;
  check
    bool
    "thinking format"
    true
    (caps.thinking_control_format = Capabilities.Chat_template_kwargs);
  check
    (option (list string))
    "supported models"
    (Some [ "rich-model"; "rich-fast" ])
    caps.supported_models;
  check
    (option string)
    "default model helper"
    (Some "rich-model")
    (Provider_catalog.default_model_for_provider catalog "rich-http");
  let cli = require_lookup catalog "cli-cached" in
  check bool "cli default transport" true (cli.transport = Provider_catalog.Http);
  check bool "cli cached auth" true (cli.auth = Provider_catalog.Oauth_cached_login);
  let oauth = require_lookup catalog "oauth" in
  check bool "managed transport" true (oauth.transport = Provider_catalog.Managed);
  check bool "oauth auth" true (oauth.auth = Provider_catalog.Oauth_cached_login);
  let file_auth = require_lookup catalog "file-auth" in
  check
    bool
    "file auth"
    true
    (file_auth.auth = Provider_catalog.File "/tmp/provider-token");
  let exec_auth = require_lookup catalog "exec-auth" in
  check bool "exec auth" true (exec_auth.auth = Provider_catalog.Exec "op read token");
  let legacy = require_lookup catalog "legacy-env" in
  check
    bool
    "legacy api_key_env auth"
    true
    (legacy.auth = Provider_catalog.Api_key_env "LEGACY_KEY")
;;

let test_type_mismatches_fall_back_without_rejecting_entry () =
  let catalog =
    catalog_of_string
      {|{
        "schema_version": 1,
        "providers": [
          {
            "id": "typed",
            "kind": 42,
            "aliases": "not-array",
            "transport": false,
            "auth": {"type": "env", "env": 17},
            "capabilities": {
              "supports_tools": "yes",
              "max_output_tokens": "many",
              "supported_models": "single"
            }
          }
        ]
      }|}
  in
  let typed = require_lookup catalog "typed" in
  check bool "default kind" true (typed.kind = Provider_config.OpenAI_compat);
  check bool "default transport" true (typed.transport = Provider_catalog.Http);
  check (list string) "aliases default empty" [] typed.aliases;
  check bool "env auth with empty env" true (typed.auth = Provider_catalog.Api_key_env "");
  check bool "supports tools remains default" false typed.capabilities.supports_tools;
  check
    (option int)
    "max output remains default"
    None
    typed.capabilities.max_output_tokens;
  check
    (option (list string))
    "models remain default"
    None
    typed.capabilities.supported_models
;;

let test_transport_auth_and_thinking_alias_matrix () =
  let catalog =
    catalog_of_string
      {|{
        "schema_version": 1,
        "providers": [
          {
            "id": "http-none",
            "kind": "openai_compat",
            "transport": "http",
            "auth": {"type": "none"},
            "capabilities": {"thinking_control_format": "none"}
          },
          {
            "id": "cli-env",
            "kind": "openai_compat",
            "transport": "http",
            "auth": {"type": "api-key-env", "env": "ENV_KEY"},
            "capabilities": {"thinking_control_format": "thinking-object"}
          },
      {
            "id": "compat-alias",
            "kind": "openai_compat",
            "transport": "custom-openai-compat",
            "auth": {"type": "env", "env": "SHORT_ENV"},
            "capabilities": {"thinking_control_format": "thinking-object-plain"}
          },
          {
            "id": "reasoning",
            "kind": "openai_compat",
            "auth": {"type": "api_key_env", "env": "REASON_KEY"},
            "capabilities": {"thinking_control_format": "reasoning-effort"}
          },
          {
            "id": "enable",
            "kind": "openai_compat",
            "auth": {"type": "api-key-env", "env": "ENABLE_KEY"},
            "capabilities": {"thinking_control_format": "enable-thinking"}
          },
          {
            "id": "base-fallback",
            "kind": "openai_compat",
            "base": "provider_d_chat",
            "max_context": 9223372036854775807999,
            "capabilities": {
              "prompt_cache_alignment": 9223372036854775807999
            }
          }
        ]
      }|}
  in
  let http_none = require_lookup catalog "http-none" in
  check bool "http transport" true (http_none.transport = Provider_catalog.Http);
  check bool "none auth" true (http_none.auth = Provider_catalog.No_auth);
  check
    bool
    "none thinking format"
    true
    (http_none.capabilities.thinking_control_format = Capabilities.No_thinking_control);
  let cli_env = require_lookup catalog "cli-env" in
  check bool "cli transport" true (cli_env.transport = Provider_catalog.Http);
  check
    bool
    "api key auth alias"
    true
    (cli_env.auth = Provider_catalog.Api_key_env "ENV_KEY");
  check
    bool
    "thinking object alias"
    true
    (cli_env.capabilities.thinking_control_format = Capabilities.Thinking_object);
  let compat = require_lookup catalog "compat-alias" in
  check
    bool
    "custom-openai-compat transport alias"
    true
    (compat.transport = Provider_catalog.Http);
  check bool "env auth alias" true (compat.auth = Provider_catalog.Api_key_env "SHORT_ENV");
  check
    bool
    "thinking object plain alias"
    true
    (compat.capabilities.thinking_control_format = Capabilities.Thinking_object_only);
  let reasoning = require_lookup catalog "reasoning" in
  check
    bool
    "reasoning effort alias"
    true
    (reasoning.capabilities.thinking_control_format = Capabilities.Reasoning_effort);
  let enable = require_lookup catalog "enable" in
  check bool "enable env" true (enable.auth = Provider_catalog.Api_key_env "ENABLE_KEY");
  check
    bool
    "enable thinking alias"
    true
    (enable.capabilities.thinking_control_format = Capabilities.Enable_thinking);
  let base = require_lookup catalog "base-fallback" in
  check bool "base fallback supports tools" true base.capabilities.supports_tools;
  check
    (option int)
    "oversized prompt alignment ignored"
    None
    base.capabilities.prompt_cache_alignment;
  check int "catalog size" 6 (List.length catalog)
;;

let test_non_list_providers_is_empty_catalog () =
  let catalog = catalog_of_string {|{"schema_version": 1, "providers": {}}|} in
  check int "empty catalog" 0 (List.length catalog);
  check (option reject) "lookup none" None (Provider_catalog.lookup catalog "missing")
;;

let test_rejects_schema_version_and_accumulates_entry_errors () =
  (match Provider_catalog.of_json (Yojson.Safe.from_string {|{"schema_version": 2}|}) with
   | Error msg ->
     check bool "schema version message" true (contains ~needle:"schema_version" msg)
   | Ok _ -> fail "unsupported schema_version should be rejected");
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"kind": "openai_compat"},
             {"id": "bad-kind", "kind": "missing_kind"},
             {"id": "bad-auth", "auth": {"type": "unknown"}}
           ]
         }|})
  with
  | Error msg ->
    check bool "missing id surfaced" true (contains ~needle:"missing required" msg);
    check bool "unknown kind surfaced" true (contains ~needle:"unknown kind" msg);
    check bool "unknown auth surfaced" true (contains ~needle:"unknown auth type" msg)
  | Ok _ -> fail "entry errors should be accumulated"
;;

let test_load_file_and_runtime_file_edges () =
  let valid_path = Filename.temp_file "provider-catalog-coverage" ".json" in
  let invalid_path = Filename.temp_file "provider-catalog-invalid" ".json" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove valid_path with
       | Sys_error _ -> ());
      try Sys.remove invalid_path with
      | Sys_error _ -> ())
    (fun () ->
       let write_file path contents =
         let oc = open_out path in
         Fun.protect
           ~finally:(fun () -> close_out_noerr oc)
           (fun () -> output_string oc contents)
       in
       write_file
         valid_path
         {|{"schema_version":1,"providers":[{"id":"runtime","kind":"openai_compat"}]}|};
       write_file invalid_path {|{"schema_version":|};
       (match Provider_catalog.load_file valid_path with
        | Ok catalog ->
          check
            bool
            "load_file lookup"
            true
            (Option.is_some (Provider_catalog.lookup catalog "runtime"))
        | Error msg -> fail msg);
       (match Provider_catalog.load_file invalid_path with
        | Error msg ->
          check bool "parse error mentions path" true (contains ~needle:invalid_path msg)
        | Ok _ -> fail "invalid JSON should fail");
       (match Provider_catalog.load_file (valid_path ^ ".missing") with
        | Error msg ->
          check bool "missing file mentions path" true (contains ~needle:valid_path msg)
        | Ok _ -> fail "missing file should fail");
       (match Provider_catalog.load_runtime_file valid_path with
        | Some catalog ->
          check
            bool
            "runtime load"
            true
            (Option.is_some (Provider_catalog.lookup catalog "runtime"))
        | None -> fail "valid runtime catalog should load");
       check
         (option reject)
         "runtime invalid file returns none"
         None
         (Provider_catalog.load_runtime_file invalid_path))
;;

let test_global_override_lifecycle () =
  Provider_catalog.clear_global ();
  let catalog =
    catalog_of_string
      {|{"schema_version":1,"providers":[{"id":"override","kind":"openai_compat"}]}|}
  in
  Provider_catalog.set_global catalog;
  (match Provider_catalog.global () with
   | Some loaded ->
     check
       bool
       "override visible"
       true
       (Option.is_some (Provider_catalog.lookup loaded "override"))
   | None -> fail "expected global override");
  Provider_catalog.clear_global ()
;;

let () =
  run
    "provider_catalog_coverage"
    [ ( "parse"
      , [ test_case
            "full entry auth transport capabilities"
            `Quick
            test_full_entry_parses_auth_transport_and_capabilities
        ; test_case
            "type mismatches fall back"
            `Quick
            test_type_mismatches_fall_back_without_rejecting_entry
        ; test_case
            "transport auth thinking aliases"
            `Quick
            test_transport_auth_and_thinking_alias_matrix
        ; test_case
            "non-list providers is empty"
            `Quick
            test_non_list_providers_is_empty_catalog
        ; test_case
            "schema and entry errors"
            `Quick
            test_rejects_schema_version_and_accumulates_entry_errors
        ] )
    ; ( "load"
      , [ test_case
            "load_file and load_runtime_file edges"
            `Quick
            test_load_file_and_runtime_file_edges
        ; test_case "global override lifecycle" `Quick test_global_override_lifecycle
        ] )
    ]
;;
