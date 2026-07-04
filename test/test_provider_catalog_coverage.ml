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
            "transport": "http",
            "base_url": "https://rich.example/v1",
            "request_path": "/chat/completions",
            "default_model": "rich-model",
            "credential_scope": "workspace",
            "auth": {"type": "setup_token_env", "env": "SETUP_TOKEN"},
            "max_context": 32000,
            "capabilities_base": "openai_chat",
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
              "ignored_sampling_parameters": ["temperature", "top_p"],
              "supports_computer_use": true,
              "supports_code_execution": true,
              "emits_usage_tokens": true,
              "thinking_control_format": "chat_template_kwargs",
              "accepted_reasoning_efforts": ["low", "high"],
              "modality_priority": "visual_first",
              "reasoning_replay": "preserve_always",
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
          }
        ]
      }|}
  in
  let rich = require_lookup catalog "alias" in
  check string "id" "rich-http" rich.id;
  check (list string) "aliases" [ "Alias"; "second" ] rich.aliases;
  check bool "http transport" true (rich.transport = Provider_catalog.Http);
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
  check
    (list string)
    "ignored sampling"
    [ "temperature"; "top_p" ]
    (List.map Capabilities.sampling_parameter_to_string caps.ignored_sampling_parameters);
  check bool "computer use" true caps.supports_computer_use;
  check bool "code execution" true caps.supports_code_execution;
  check bool "usage tokens" true caps.emits_usage_tokens;
  check
    (option (list string))
    "accepted reasoning efforts"
    (Some [ "low"; "high" ])
    (Option.map (List.map Reasoning_effort.to_string) caps.accepted_reasoning_efforts);
  check bool "visual first" true (caps.modality_priority = Modality.Visual_first);
  check
    bool
    "thinking format"
    true
    (caps.thinking_control_format = Capabilities.Chat_template_kwargs);
  check
    bool
    "reasoning replay"
    true
    (caps.reasoning_replay_override = Capabilities.Force_preserve_always);
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
  check bool "oauth auth" true (oauth.auth = Provider_catalog.Oauth_cached_login)
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
            "auth": 17,
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
  check
    bool
    "invalid auth defaults to no_auth"
    true
    (typed.auth = Provider_catalog.No_auth);
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

let test_transport_auth_and_thinking_canonical_matrix () =
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
            "capabilities": {
              "thinking_control_format": "none",
              "preserve_thinking_control_format": "always_preserved"
            }
          },
          {
            "id": "cli-env",
            "kind": "openai_compat",
            "transport": "http",
            "auth": {"type": "api_key_env", "env": "ENV_KEY"},
            "capabilities": {"thinking_control_format": "thinking_object"}
          },
      {
            "id": "http-direct",
            "kind": "openai_compat",
            "transport": "http",
            "auth": {"type": "api_key_env", "env": "SHORT_ENV"},
            "capabilities": {"thinking_control_format": "thinking_object_only"}
          },
          {
            "id": "reasoning",
            "kind": "openai_compat",
            "auth": {"type": "api_key_env", "env": "REASON_KEY"},
            "capabilities": {"thinking_control_format": "reasoning_effort"}
          },
          {
            "id": "ollama-think",
            "kind": "ollama",
            "capabilities": {"thinking_control_format": "ollama_think"}
          },
          {
            "id": "enable",
            "kind": "openai_compat",
            "auth": {"type": "api_key_env", "env": "ENABLE_KEY"},
            "capabilities": {"thinking_control_format": "enable_thinking"}
          },
          {
            "id": "template-token",
            "kind": "ollama",
            "capabilities": {"thinking_control_format": "chat_template_token"}
          },
          {
            "id": "base-entry",
            "kind": "openai_compat",
            "capabilities_base": "openai_chat",
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
  check
    bool
    "always preserved format"
    true
    (http_none.capabilities.preserve_thinking_control_format
     = Capabilities.Always_preserved_thinking);
  let cli_env = require_lookup catalog "cli-env" in
  check bool "cli transport" true (cli_env.transport = Provider_catalog.Http);
  check bool "api key auth" true (cli_env.auth = Provider_catalog.Api_key_env "ENV_KEY");
  check
    bool
    "thinking object"
    true
    (cli_env.capabilities.thinking_control_format = Capabilities.Thinking_object);
  let compat = require_lookup catalog "http-direct" in
  check bool "http transport" true (compat.transport = Provider_catalog.Http);
  check bool "env auth" true (compat.auth = Provider_catalog.Api_key_env "SHORT_ENV");
  check
    bool
    "thinking object only"
    true
    (compat.capabilities.thinking_control_format = Capabilities.Thinking_object_only);
  let reasoning = require_lookup catalog "reasoning" in
  check
    bool
    "reasoning effort"
    true
    (reasoning.capabilities.thinking_control_format = Capabilities.Reasoning_effort);
  let ollama_think = require_lookup catalog "ollama-think" in
  check
    bool
    "ollama think"
    true
    (ollama_think.capabilities.thinking_control_format = Capabilities.Ollama_think);
  let enable = require_lookup catalog "enable" in
  check bool "enable env" true (enable.auth = Provider_catalog.Api_key_env "ENABLE_KEY");
  check
    bool
    "enable thinking"
    true
    (enable.capabilities.thinking_control_format = Capabilities.Enable_thinking);
  let template_token = require_lookup catalog "template-token" in
  check
    bool
    "template token thinking"
    true
    (template_token.capabilities.thinking_control_format
     = Capabilities.Chat_template_token);
  let base = require_lookup catalog "base-entry" in
  check bool "capabilities_base supports tools" true base.capabilities.supports_tools;
  check
    (option int)
    "oversized prompt alignment ignored"
    None
    base.capabilities.prompt_cache_alignment;
  check int "catalog size" 8 (List.length catalog)
;;

let test_removed_catalog_aliases_are_rejected () =
  let assert_reject label json_text needle =
    match Provider_catalog.of_json (Yojson.Safe.from_string json_text) with
    | Error msg -> check bool label true (contains ~needle msg)
    | Ok _ -> failf "%s should be rejected" label
  in
  assert_reject
    "transport alias rejected"
    {|{"schema_version":1,"providers":[{"id":"p","transport":"custom-openai-compat"}]}|}
    "unknown transport";
  assert_reject
    "top-level api_key_env rejected"
    {|{"schema_version":1,"providers":[{"id":"p","api_key_env":"LEGACY_KEY"}]}|}
    "removed provider catalog field";
  assert_reject
    "top-level api_key_env null rejected"
    {|{"schema_version":1,"providers":[{"id":"p","api_key_env":null}]}|}
    "removed provider catalog field";
  assert_reject
    "auth key rejected"
    {|{"schema_version":1,"providers":[{"id":"p","auth":{"type":"api_key_env","key":"LEGACY_KEY"}}]}|}
    "removed provider catalog auth field";
  assert_reject
    "capability base alias rejected"
    {|{"schema_version":1,"providers":[{"id":"p","base":"openai_chat"}]}|}
    "removed provider catalog field \"base\"";
  assert_reject
    "auth alias rejected"
    {|{"schema_version":1,"providers":[{"id":"p","auth":{"type":"api-key-env","env":"K"}}]}|}
    "unknown auth type";
  assert_reject
    "thinking alias rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"thinking_control_format":"thinking-object"}}]}|}
    "unknown thinking_control_format";
  assert_reject
    "reasoning replay alias rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"reasoning_replay":"preserve-allways"}}]}|}
    "unknown reasoning_replay";
  assert_reject
    "reasoning replay type rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"reasoning_replay":true}}]}|}
    "expected string";
  assert_reject
    "accepted reasoning effort rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"accepted_reasoning_efforts":["low","turbo"]}}]}|}
    "unknown accepted_reasoning_efforts";
  assert_reject
    "ignored sampling parameter rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"ignored_sampling_parameters":["temp"]}}]}|}
    "unknown ignored_sampling_parameters";
  assert_reject
    "modality priority rejected"
    {|{"schema_version":1,"providers":[{"id":"p","capabilities":{"modality_priority":"image_only"}}]}|}
    "unknown modality_priority"
;;

let test_non_list_providers_is_empty_catalog () =
  let catalog = catalog_of_string {|{"schema_version": 1, "providers": {}}|} in
  check int "empty catalog" 0 (List.length catalog);
  check (option reject) "lookup none" None (Provider_catalog.lookup catalog "missing")
;;

let test_removed_auth_types_are_rejected () =
  let catalog_json auth_type extra_fields =
    Printf.sprintf
      {|{
        "schema_version": 1,
        "providers": [
          {
            "id": "legacy-auth",
            "kind": "openai_compat",
            "auth": {"type": "%s"%s}
          }
        ]
      }|}
      auth_type
      extra_fields
  in
  let check_rejected auth_type extrafields =
    match
      Provider_catalog.of_json
        (Yojson.Safe.from_string (catalog_json auth_type extrafields))
    with
    | Ok _ -> failf "expected auth type %S to be rejected" auth_type
    | Error msg ->
      check
        bool
        (Printf.sprintf "%S rejected" auth_type)
        true
        (contains ~needle:"removed" msg || contains ~needle:"unknown" msg)
  in
  check_rejected "file" ", \"path\": \"/tmp/token\"";
  check_rejected "exec" ", \"command\": \"op read token\""
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
            "transport auth thinking canonical"
            `Quick
            test_transport_auth_and_thinking_canonical_matrix
        ; test_case
            "removed catalog aliases rejected"
            `Quick
            test_removed_catalog_aliases_are_rejected
        ; test_case
            "non-list providers is empty"
            `Quick
            test_non_list_providers_is_empty_catalog
        ; test_case
            "schema and entry errors"
            `Quick
            test_rejects_schema_version_and_accumulates_entry_errors
        ; test_case
            "removed auth types rejected"
            `Quick
            test_removed_auth_types_are_rejected
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
