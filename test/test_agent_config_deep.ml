(** Deep tests for Agent_config — JSON parsing, provider resolution, tool/MCP config.

    Targets 55 uncovered points in lib/agent/agent_config.ml. *)

open Agent_sdk

let () = Printexc.record_backtrace true
let tc name f = Alcotest.test_case name `Quick f

let with_temp_file content f =
  let path = Filename.temp_file "agent_cfg_" ".json" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

let expect_invalid_config_field label field json =
  match Agent_config.of_json json with
  | Error (Error.Config (InvalidConfig { field = actual; _ })) ->
    Alcotest.(check string) label field actual
  | Error e -> Alcotest.fail (label ^ ": unexpected error: " ^ Error.to_string e)
  | Ok _ -> Alcotest.fail (label ^ ": expected InvalidConfig")
;;

(* ── of_json: minimal / defaults ─────────────────────────────── *)

let test_of_json_minimal () =
  let json = Yojson.Safe.from_string {|{}|} in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("minimal: " ^ Error.to_string e)
  | Ok cfg ->
    Alcotest.(check string) "default name" "agent" cfg.name;
    Alcotest.(check string) "default model" "claude-sonnet-4-6" cfg.model;
    Alcotest.(check (option string)) "no system_prompt" None cfg.system_prompt;
    Alcotest.(check (option int)) "no max_tokens" None cfg.max_tokens;
    Alcotest.(check (option int)) "no max_turns" None cfg.max_turns;
    Alcotest.(check (option bool)) "no enable_thinking" None cfg.enable_thinking;
    Alcotest.(check (option int)) "no thinking_budget" None cfg.thinking_budget;
    Alcotest.(check (option string)) "no provider" None cfg.provider;
    Alcotest.(check (option string)) "no base_url" None cfg.base_url;
    Alcotest.(check int) "no tools" 0 (List.length cfg.tools);
    Alcotest.(check int) "no mcp" 0 (List.length cfg.mcp_servers)
;;

let test_of_json_full () =
  let json =
    Yojson.Safe.from_string
      {|{
    "name": "my-agent",
    "model": "gpt-4",
    "system_prompt": "Be concise.",
    "max_tokens": 2048,
    "max_turns": 5,
    "enable_thinking": true,
    "thinking_budget": 1024,
    "provider": "local",
    "base_url": "http://127.0.0.1:9000"
  }|}
  in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("full: " ^ Error.to_string e)
  | Ok cfg ->
    Alcotest.(check string) "name" "my-agent" cfg.name;
    Alcotest.(check string) "model" "gpt-4" cfg.model;
    Alcotest.(check (option string))
      "system_prompt"
      (Some "Be concise.")
      cfg.system_prompt;
    Alcotest.(check (option int)) "max_tokens" (Some 2048) cfg.max_tokens;
    Alcotest.(check (option int)) "max_turns" (Some 5) cfg.max_turns;
    Alcotest.(check (option bool)) "thinking" (Some true) cfg.enable_thinking;
    Alcotest.(check (option int)) "thinking_budget" (Some 1024) cfg.thinking_budget;
    Alcotest.(check (option string)) "provider" (Some "local") cfg.provider;
    Alcotest.(check (option string))
      "base_url"
      (Some "http://127.0.0.1:9000")
      cfg.base_url
;;

(* ── Tool parsing ─────────────────────────────────────────────── *)

let test_parse_tool_full () =
  let json =
    Yojson.Safe.from_string
      {|{
    "name": "get_weather",
    "description": "Get weather info",
    "parameters": [
      {"name": "city", "type": "string", "required": true, "description": "City name"},
      {"name": "units", "type": "number", "required": false}
    ]
  }|}
  in
  match Agent_config.parse_tool json with
  | Error e -> Alcotest.fail ("tool: " ^ Error.to_string e)
  | Ok t ->
    Alcotest.(check string) "tool name" "get_weather" t.name;
    Alcotest.(check string) "tool desc" "Get weather info" t.description;
    Alcotest.(check int) "2 params" 2 (List.length t.parameters);
    let p1 = List.nth t.parameters 0 in
    Alcotest.(check string) "param name" "city" p1.name;
    Alcotest.(check bool) "param required" true p1.required
;;

let test_tool_no_parameters_key () =
  let json = Yojson.Safe.from_string {|{"name": "ping", "description": "ping"}|} in
  match Agent_config.parse_tool json with
  | Error e -> Alcotest.fail ("no params key: " ^ Error.to_string e)
  | Ok t -> Alcotest.(check int) "empty params" 0 (List.length t.parameters)
;;

let test_tool_parameters_not_list () =
  let json = Yojson.Safe.from_string {|{"name": "ping", "parameters": "not-a-list"}|} in
  match Agent_config.parse_tool json with
  | Error (Error.Config (InvalidConfig { field; _ })) ->
    Alcotest.(check string) "field" "parameters" field
  | Error e -> Alcotest.fail ("unexpected error: " ^ Error.to_string e)
  | Ok _ -> Alcotest.fail "expected invalid parameters"
;;

let test_param_defaults () =
  let json =
    Yojson.Safe.from_string
      {|{"name": "t1", "parameters": [{"name": "x", "type": "string"}]}|}
  in
  match Agent_config.parse_tool json with
  | Error e -> Alcotest.fail ("param defaults: " ^ Error.to_string e)
  | Ok t ->
    let p = List.nth t.parameters 0 in
    Alcotest.(check string) "default desc" "" p.description;
    Alcotest.(check bool) "default required" false p.required
;;

let test_param_missing_type_rejected () =
  let json = Yojson.Safe.from_string {|{"name": "t1", "parameters": [{"name": "x"}]}|} in
  match Agent_config.parse_tool json with
  | Error (Error.Config (InvalidConfig { field; detail })) ->
    Alcotest.(check string) "field" "parameter.type" field;
    Alcotest.(check string) "detail" "missing required field" detail
  | Error e -> Alcotest.fail ("missing type: unexpected error: " ^ Error.to_string e)
  | Ok _ -> Alcotest.fail "missing type: expected InvalidConfig"
;;

(* ── MCP server parsing ──────────────────────────────────────── *)

let test_mcp_stdio () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{
      "command": "npx",
      "args": ["-y", "server"],
      "name": "my-server",
      "env": ["FOO=bar", "BAZ=qux"]
    }]
  }|}
  in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("mcp stdio: " ^ Error.to_string e)
  | Ok cfg ->
    Alcotest.(check int) "1 mcp" 1 (List.length cfg.mcp_servers);
    (match List.nth cfg.mcp_servers 0 with
     | Agent_config.Stdio_mcp { command; args; name; env } ->
       Alcotest.(check string) "command" "npx" command;
       Alcotest.(check int) "args" 2 (List.length args);
       Alcotest.(check string) "name" "my-server" name;
       Alcotest.(check int) "env" 2 (List.length env)
     | Agent_config.Http_mcp _ -> Alcotest.fail "expected Stdio_mcp")
;;

let test_mcp_http () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{
      "url": "http://localhost:3000/mcp",
      "name": "http-server",
      "headers": {"Authorization": "Bearer token123", "X-Custom": "value"}
    }]
  }|}
  in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("mcp http: " ^ Error.to_string e)
  | Ok cfg ->
    (match List.nth cfg.mcp_servers 0 with
     | Agent_config.Http_mcp { url; name; headers } ->
       Alcotest.(check string) "url" "http://localhost:3000/mcp" url;
       Alcotest.(check string) "name" "http-server" name;
       Alcotest.(check int) "headers" 2 (List.length headers)
     | Agent_config.Stdio_mcp _ -> Alcotest.fail "expected Http_mcp")
;;

let test_mcp_http_defaults () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"url": "http://x.com/mcp"}]
  }|}
  in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("mcp http defaults: " ^ Error.to_string e)
  | Ok cfg ->
    (match List.nth cfg.mcp_servers 0 with
     | Agent_config.Http_mcp { url; name; headers } ->
       Alcotest.(check string) "url" "http://x.com/mcp" url;
       Alcotest.(check string) "name defaults to url" "http://x.com/mcp" name;
       Alcotest.(check int) "no headers" 0 (List.length headers)
     | Agent_config.Stdio_mcp _ -> Alcotest.fail "expected Http_mcp")
;;

let test_mcp_stdio_defaults () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"command": "my-cmd"}]
  }|}
  in
  match Agent_config.of_json json with
  | Error e -> Alcotest.fail ("mcp stdio defaults: " ^ Error.to_string e)
  | Ok cfg ->
    (match List.nth cfg.mcp_servers 0 with
     | Agent_config.Stdio_mcp { command; args; name; env } ->
       Alcotest.(check string) "command" "my-cmd" command;
       Alcotest.(check string) "name defaults to cmd" "my-cmd" name;
       Alcotest.(check int) "no args" 0 (List.length args);
       Alcotest.(check int) "no env" 0 (List.length env)
     | Agent_config.Http_mcp _ -> Alcotest.fail "expected Stdio_mcp")
;;

let test_mcp_http_headers_not_assoc () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"url": "http://x.com", "headers": "not-obj"}]
  }|}
  in
  expect_invalid_config_field "headers not assoc" "headers" json
;;

let test_mcp_http_non_string_header_value () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"url": "http://x.com", "headers": {"a": "b", "c": 123}}]
  }|}
  in
  expect_invalid_config_field "non-string header" "/headers/c" json
;;

let test_mcp_stdio_non_string_env_value () =
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"command": "node", "env": ["FOO=bar", 123]}]
  }|}
  in
  expect_invalid_config_field "non-string env" "env" json
;;

(* ── load from file ──────────────────────────────────────────── *)

let test_load_valid_file () =
  with_temp_file {|{"name": "file-agent", "model": "test-model"}|} (fun path ->
    match Agent_config.load path with
    | Error e -> Alcotest.fail ("load: " ^ Error.to_string e)
    | Ok cfg ->
      Alcotest.(check string) "name" "file-agent" cfg.name;
      Alcotest.(check string) "model" "test-model" cfg.model)
;;

let test_load_nonexistent () =
  match Agent_config.load "/tmp/nonexistent_agent_config_99999.json" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for nonexistent file"
;;

let test_load_invalid_json () =
  with_temp_file "{{not json}" (fun path ->
    match Agent_config.load path with
    | Error _ -> ()
    | Ok _ -> Alcotest.fail "expected error for invalid json")
;;

(* ── resolve_provider ────────────────────────────────────────── *)

let test_resolve_local () =
  let cfg = Agent_config.resolve_provider ~model_id:"m1" "local" None in
  match cfg.provider with
  | Provider.Local { base_url } ->
    Alcotest.(check bool) "has url" true (String.length base_url > 0)
  | _ -> Alcotest.fail "expected Local"
;;

let test_resolve_local_custom_url () =
  let cfg =
    Agent_config.resolve_provider ~model_id:"m1" "local" (Some "http://my-server:1234")
  in
  match cfg.provider with
  | Provider.Local { base_url } ->
    Alcotest.(check string) "custom url" "http://my-server:1234" base_url
  | _ -> Alcotest.fail "expected Local"
;;

let test_resolve_anthropic () =
  let cfg = Agent_config.resolve_provider ~model_id:"claude-sonnet" "anthropic" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "provider id" "claude" name;
    Alcotest.(check string) "model_id" "claude-sonnet" cfg.model_id;
    Alcotest.(check string) "api_key_env" "ANTHROPIC_API_KEY" cfg.api_key_env
  | _ -> Alcotest.fail "expected registered provider"
;;

let test_resolve_openai () =
  let cfg = Agent_config.resolve_provider ~model_id:"gpt-4" "openai" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "not a built-in provider id" "openai" name;
    Alcotest.(check string) "api_key_env preserves selector" "openai" cfg.api_key_env
  | _ -> Alcotest.fail "expected unresolved registered-provider adapter"
;;

let test_resolve_openai_custom_url () =
  let cfg =
    Agent_config.resolve_provider ~model_id:"gpt-4" "openai" (Some "http://custom:4000")
  in
  match cfg.provider with
  | Provider.OpenAICompat { base_url; auth_header; _ } ->
    Alcotest.(check string) "custom url" "http://custom:4000" base_url;
    Alcotest.(check (option string)) "auth header" (Some "Authorization") auth_header
  | _ -> Alcotest.fail "expected OpenAICompat"
;;

let test_resolve_other () =
  let cfg = Agent_config.resolve_provider ~model_id:"m1" "MY_CUSTOM_KEY" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "provider id" "my_custom_key" name;
    Alcotest.(check string) "api_key_env" "MY_CUSTOM_KEY" cfg.api_key_env
  | _ -> Alcotest.fail "expected unresolved registered-provider adapter"
;;

let test_resolve_other_custom_url () =
  let cfg =
    Agent_config.resolve_provider ~model_id:"m1" "MY_KEY" (Some "http://custom:5000")
  in
  match cfg.provider with
  | Provider.OpenAICompat { base_url; auth_header; _ } ->
    Alcotest.(check string) "custom url" "http://custom:5000" base_url;
    Alcotest.(check (option string)) "auth header" (Some "Authorization") auth_header
  | _ -> Alcotest.fail "expected OpenAICompat"
;;

let test_resolve_other_openai_v1_base_url () =
  let cfg =
    Agent_config.resolve_provider
      ~model_id:"mimo-v2.5-pro"
      "MIMO_API_KEY"
      (Some "https://token-plan-sgp.xiaomimimo.com/v1")
  in
  match cfg.provider with
  | Provider.OpenAICompat { base_url; auth_header; path; _ } ->
    Alcotest.(check string) "base_url" "https://token-plan-sgp.xiaomimimo.com/v1" base_url;
    Alcotest.(check string) "path" "/chat/completions" path;
    Alcotest.(check (option string)) "auth header" (Some "Authorization") auth_header;
    Alcotest.(check string) "api_key_env" "MIMO_API_KEY" cfg.api_key_env
  | _ -> Alcotest.fail "expected OpenAICompat"
;;

let test_resolve_provider_i () =
  (* With base_url=None, resolve_provider now returns Custom_registered
     so that downstream can look up the registry-declared kind.
     entry.defaults (url, path, api_key_env) are carried via the
     registry, not embedded in the Provider.config variant. *)
  let cfg = Agent_config.resolve_provider ~model_id:"dashscope/qwen3-32b" "groq" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "groq name" "groq" name;
    Alcotest.(check string) "groq api_key_env" "GROQ_API_KEY" cfg.api_key_env;
    Alcotest.(check string) "groq model_id" "dashscope/qwen3-32b" cfg.model_id
  | _ -> Alcotest.fail "expected Custom_registered for groq (registered)"
;;

let test_resolve_provider_i_custom_url () =
  let cfg =
    Agent_config.resolve_provider
      ~model_id:"dashscope/qwen3-32b"
      "groq"
      (Some "http://proxy:8080")
  in
  match cfg.provider with
  | Provider.OpenAICompat { base_url; auth_header; _ } ->
    Alcotest.(check string) "overridden url" "http://proxy:8080" base_url;
    Alcotest.(check (option string)) "auth header" (Some "Authorization") auth_header
  | _ -> Alcotest.fail "expected OpenAICompat for groq custom url"
;;

let test_resolve_deepseek () =
  (* base_url=None: Custom_registered preserves kind lookup via registry. *)
  let cfg = Agent_config.resolve_provider ~model_id:"deepseek-chat" "deepseek" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "deepseek name" "deepseek" name;
    Alcotest.(check string) "deepseek api_key_env" "DEEPSEEK_API_KEY" cfg.api_key_env
  | _ -> Alcotest.fail "expected Custom_registered for deepseek (registered)"
;;

let test_resolve_gemini_preserves_kind () =
  (* Regression for #1003: registered providers with non-Openai kind
     (e.g. Gemini) must route through Custom_registered so downstream
     preserves entry.defaults.kind. Previously resolve_provider
     returned OpenAICompat, flattening kind to OpenAI_compat and
     producing 404 against the Gemini endpoint. *)
  let cfg = Agent_config.resolve_provider ~model_id:"gemini-2.5-flash" "gemini" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "gemini name" "gemini" name;
    Alcotest.(check string) "gemini api_key_env" "GEMINI_API_KEY" cfg.api_key_env;
    Alcotest.(check string) "gemini model_id" "gemini-2.5-flash" cfg.model_id
  | _ -> Alcotest.fail "expected Custom_registered for gemini (registered)"
;;

(* ── Provider_kind dispatch (drift-fix regressions) ───────────── *)

let test_resolve_openai_compat_ssot () =
  let cfg = Agent_config.resolve_provider ~model_id:"gpt-4" "openai_compat" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "kind string is not provider id" "openai_compat" name;
    Alcotest.(check string) "api_key_env preserves input" "openai_compat" cfg.api_key_env
  | _ -> Alcotest.fail "expected unresolved registered-provider adapter"
;;

let test_resolve_anthropic_case_insensitive () =
  (* The parser trims and lowercases; ["Anthropic"] and [" ANTHROPIC "]
     both land on the Anthropic branch, not the registry fallback. *)
  List.iter
    (fun input ->
       let cfg = Agent_config.resolve_provider ~model_id:"claude-sonnet" input None in
       match cfg.provider with
       | Provider.Custom_registered { name } ->
         Alcotest.(check string) (Printf.sprintf "provider id for %S" input) "claude" name;
         Alcotest.(check string)
           (Printf.sprintf "api_key_env for %S" input)
           "ANTHROPIC_API_KEY"
           cfg.api_key_env
       | _ -> Alcotest.failf "expected registered provider for %S" input)
    [ "Anthropic"; " ANTHROPIC "; "anthropic" ]
;;

let test_resolve_claude_alias_routes_to_anthropic () =
  (* ["claude"] is a documented Provider_kind alias for Anthropic. Prior
     to this fix it fell to the registry fallback (Provider_registry
     has no "claude" entry) and ended up as OpenAICompat with
     api_key_env = "claude" — broken. *)
  let cfg = Agent_config.resolve_provider ~model_id:"claude-sonnet" "claude" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "provider id" "claude" name;
    Alcotest.(check string)
      "api_key_env routed to registered provider"
      "ANTHROPIC_API_KEY"
      cfg.api_key_env
  | _ -> Alcotest.fail "expected registered provider for claude alias"
;;

let test_resolve_unknown_still_goes_to_registry_fallback () =
  let cfg = Agent_config.resolve_provider ~model_id:"m1" "TOTALLY_BOGUS_KEY" None in
  match cfg.provider with
  | Provider.Custom_registered { name } ->
    Alcotest.(check string) "provider id" "totally_bogus_key" name;
    Alcotest.(check string)
      "api_key_env preserves input"
      "TOTALLY_BOGUS_KEY"
      cfg.api_key_env
  | _ -> Alcotest.fail "expected unresolved registered-provider adapter"
;;

let test_resolve_ollama_custom_url_stays_no_auth () =
  let cfg =
    Agent_config.resolve_provider
      ~model_id:"llama3.2"
      "ollama"
      (Some "http://127.0.0.1:11434")
  in
  match cfg.provider with
  | Provider.OpenAICompat { base_url; auth_header; path; _ } ->
    Alcotest.(check string) "base_url" "http://127.0.0.1:11434" base_url;
    Alcotest.(check string) "path" "/api/chat" path;
    Alcotest.(check (option string)) "auth header" None auth_header
  | _ -> Alcotest.fail "expected OpenAICompat for ollama custom url"
;;

(* ── of_json error cases ─────────────────────────────────────── *)

let test_of_json_bad_param () =
  let json =
    Yojson.Safe.from_string
      {|{
    "tools": [{"name": "t", "parameters": [{"name": 123}]}]
  }|}
  in
  expect_invalid_config_field "inline tools rejected before bad param" "tools" json
;;

let test_of_json_bad_tool () =
  (* tool name is required as string *)
  let json =
    Yojson.Safe.from_string
      {|{
    "tools": [{"name": 999}]
  }|}
  in
  expect_invalid_config_field "inline tools rejected before bad tool" "tools" json
;;

let test_of_json_non_object_tool () =
  let json = Yojson.Safe.from_string {|{"tools": ["not-object"]}|} in
  expect_invalid_config_field "inline tools rejected before non-object tool" "tools" json
;;

let test_of_json_bad_mcp () =
  (* mcp without command or url *)
  let json =
    Yojson.Safe.from_string
      {|{
    "mcp_servers": [{"not_command": "x"}]
  }|}
  in
  match Agent_config.of_json json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bad mcp"
;;

let test_of_json_non_object_mcp () =
  let json = Yojson.Safe.from_string {|{"mcp_servers": ["not-object"]}|} in
  expect_invalid_config_field "non-object mcp" "mcp_server" json
;;

let test_of_json_tools_not_list () =
  let json = Yojson.Safe.from_string {|{"tools": "not-list"}|} in
  expect_invalid_config_field "tools not list" "tools" json
;;

let test_of_json_mcp_not_list () =
  let json = Yojson.Safe.from_string {|{"mcp_servers": "not-list"}|} in
  expect_invalid_config_field "mcp not list" "mcp_servers" json
;;

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Agent_config deep"
    [ ( "of_json"
      , [ tc "minimal" test_of_json_minimal
        ; tc "full" test_of_json_full
        ; tc "parse tool full" test_parse_tool_full
        ; tc "tool no params key" test_tool_no_parameters_key
        ; tc "tool params not list" test_tool_parameters_not_list
        ; tc "param defaults" test_param_defaults
        ; tc "param missing type rejected" test_param_missing_type_rejected
        ; tc "tools not list" test_of_json_tools_not_list
        ; tc "mcp not list" test_of_json_mcp_not_list
        ; tc "bad param" test_of_json_bad_param
        ; tc "bad tool" test_of_json_bad_tool
        ; tc "non-object tool" test_of_json_non_object_tool
        ; tc "bad mcp" test_of_json_bad_mcp
        ; tc "non-object mcp" test_of_json_non_object_mcp
        ] )
    ; ( "mcp_servers"
      , [ tc "stdio" test_mcp_stdio
        ; tc "http" test_mcp_http
        ; tc "http defaults" test_mcp_http_defaults
        ; tc "stdio defaults" test_mcp_stdio_defaults
        ; tc "headers not assoc" test_mcp_http_headers_not_assoc
        ; tc "non-string header" test_mcp_http_non_string_header_value
        ; tc "non-string env" test_mcp_stdio_non_string_env_value
        ] )
    ; ( "load"
      , [ tc "valid file" test_load_valid_file
        ; tc "nonexistent" test_load_nonexistent
        ; tc "invalid json" test_load_invalid_json
        ] )
    ; ( "resolve_provider"
      , [ tc "local" test_resolve_local
        ; tc "local custom url" test_resolve_local_custom_url
        ; tc "anthropic" test_resolve_anthropic
        ; tc "openai is not built in" test_resolve_openai
        ; tc "explicit openai custom url" test_resolve_openai_custom_url
        ; tc "other" test_resolve_other
        ; tc "other custom url" test_resolve_other_custom_url
        ; tc "other OpenAI /v1 base url" test_resolve_other_openai_v1_base_url
        ; tc "groq" test_resolve_provider_i
        ; tc "groq custom url" test_resolve_provider_i_custom_url
        ; tc "deepseek" test_resolve_deepseek
        ; tc "gemini preserves kind (#1003)" test_resolve_gemini_preserves_kind
        ; tc "openai_compat is kind string" test_resolve_openai_compat_ssot
        ; tc "anthropic case-insensitive" test_resolve_anthropic_case_insensitive
        ; tc
            "claude alias routes to Anthropic"
            test_resolve_claude_alias_routes_to_anthropic
        ; tc
            "unknown stays unresolved provider id"
            test_resolve_unknown_still_goes_to_registry_fallback
        ; tc
            "ollama custom url stays no-auth"
            test_resolve_ollama_custom_url_stays_no_auth
        ] )
    ]
;;
