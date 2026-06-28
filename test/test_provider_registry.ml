(** Tests for Provider_registry and Capability_filter (v0.69.0). *)

open Alcotest
open Llm_provider

let with_env key value f =
  let previous = Sys.getenv_opt key in
  Unix.putenv key value;
  Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some previous -> Unix.putenv key previous
      | None -> Unix.putenv key "")
    f
;;

(* ── Registry CRUD ──────────────────────────────────── *)

let test_empty_registry () =
  let reg = Provider_registry.create () in
  check int "empty has 0 entries" 0 (List.length (Provider_registry.all reg));
  check (option reject) "find on empty is None" None (Provider_registry.find reg "nope")
;;

let test_register_and_find () =
  let reg = Provider_registry.create () in
  let entry : Provider_registry.entry =
    { name = "test-provider"
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = "http://localhost:9999"
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = Capabilities.default_capabilities
    ; is_available = (fun () -> true)
    }
  in
  Provider_registry.register reg entry;
  (match Provider_registry.find reg "test-provider" with
   | Some e -> check string "name" "test-provider" e.name
   | None -> fail "should find registered provider");
  check int "1 entry" 1 (List.length (Provider_registry.all reg))
;;

let test_overwrite () =
  let reg = Provider_registry.create () in
  let mk url : Provider_registry.entry =
    { name = "p"
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = url
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = Capabilities.default_capabilities
    ; is_available = (fun () -> true)
    }
  in
  Provider_registry.register reg (mk "http://old");
  Provider_registry.register reg (mk "http://new");
  (match Provider_registry.find reg "p" with
   | Some e -> check string "updated url" "http://new" e.defaults.base_url
   | None -> fail "should exist");
  check int "still 1 entry" 1 (List.length (Provider_registry.all reg))
;;

let test_unregister () =
  let reg = Provider_registry.create () in
  let entry : Provider_registry.entry =
    { name = "temp"
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = "http://x"
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = Capabilities.default_capabilities
    ; is_available = (fun () -> true)
    }
  in
  Provider_registry.register reg entry;
  Provider_registry.unregister reg "temp";
  check (option reject) "gone" None (Provider_registry.find reg "temp");
  check int "0 entries" 0 (List.length (Provider_registry.all reg))
;;

(* ── Availability ───────────────────────────────────── *)

let test_available_filter () =
  let reg = Provider_registry.create () in
  let mk name avail : Provider_registry.entry =
    { name
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = "http://x"
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = Capabilities.default_capabilities
    ; is_available = (fun () -> avail)
    }
  in
  Provider_registry.register reg (mk "up" true);
  Provider_registry.register reg (mk "down" false);
  let avail = Provider_registry.available reg in
  check int "only 1 available" 1 (List.length avail);
  check string "the up one" "up" (List.hd avail).name
;;

let test_command_in_path_finds_binary () =
  let tmp = Filename.temp_file "provider-registry" ".bin" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove tmp with
      | Sys_error _ -> ())
    (fun () ->
       let dir = Filename.dirname tmp in
       let name = Filename.basename tmp in
       check bool "binary found" true (Provider_registry.command_in_path ~path:dir name))
;;

let test_command_in_path_rejects_directory () =
  let dir = Filename.temp_file "provider-registry" ".dir" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      try Unix.rmdir dir with
      | Unix.Unix_error _ -> ())
    (fun () ->
       let parent = Filename.dirname dir in
       let name = Filename.basename dir in
       check
         bool
         "directory is not runnable"
         false
         (Provider_registry.command_in_path ~path:parent name))
;;

let test_command_in_path_misses_unknown_binary () =
  let dir = Filename.get_temp_dir_name () in
  check
    bool
    "missing binary"
    false
    (Provider_registry.command_in_path ~path:dir "provider-registry-missing-binary")
;;

(* ── Capability queries ─────────────────────────────── *)

let test_find_capable_tools () =
  let reg = Provider_registry.create () in
  let mk name caps : Provider_registry.entry =
    { name
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = "http://x"
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = caps
    ; is_available = (fun () -> true)
    }
  in
  Provider_registry.register
    reg
    (mk "with-tools" { Capabilities.default_capabilities with supports_tools = true });
  Provider_registry.register reg (mk "no-tools" Capabilities.default_capabilities);
  let capable = Provider_registry.find_capable reg Capability_filter.requires_tools in
  check int "1 with tools" 1 (List.length capable);
  check string "correct one" "with-tools" (List.hd capable).name
;;

let test_find_capable_composite () =
  let reg = Provider_registry.create () in
  let mk name caps : Provider_registry.entry =
    { name
    ; defaults =
        { kind = OpenAI_compat
        ; base_url = "http://x"
        ; api_key_env = ""
        ; request_path = "/v1/chat/completions"
        }
    ; max_context = 128_000
    ; capabilities = caps
    ; is_available = (fun () -> true)
    }
  in
  Provider_registry.register
    reg
    (mk
       "full"
       { Capabilities.default_capabilities with
         supports_tools = true
       ; supports_reasoning = true
       });
  Provider_registry.register
    reg
    (mk "tools-only" { Capabilities.default_capabilities with supports_tools = true });
  Provider_registry.register reg (mk "none" Capabilities.default_capabilities);
  let need_both =
    Capability_filter.requires_all
      [ Capability_filter.requires_tools; Capability_filter.requires_reasoning ]
  in
  let capable = Provider_registry.find_capable reg need_both in
  check int "only full matches" 1 (List.length capable);
  check string "the full one" "full" (List.hd capable).name
;;

(* ── Default registry ───────────────────────────────── *)

let test_default_has_14 () =
  let reg = Provider_registry.default () in
  let all = Provider_registry.all reg in
  check int "14 known providers" 14 (List.length all);
  check bool "llama exists" true (Option.is_some (Provider_registry.find reg "nous"));
  check bool "ollama exists" true (Option.is_some (Provider_registry.find reg "ollama"));
  check
    bool
    "ollama_cloud exists"
    true
    (Option.is_some (Provider_registry.find reg "ollama_cloud"));
  check bool "claude exists" true (Option.is_some (Provider_registry.find reg "claude"));
  check bool "gemini exists" true (Option.is_some (Provider_registry.find reg "gemini"));
  check bool "glm exists" true (Option.is_some (Provider_registry.find reg "glm"));
  check
    bool
    "glm-coding exists"
    true
    (Option.is_some (Provider_registry.find reg "glm-coding"));
  check bool "kimi exists" true (Option.is_some (Provider_registry.find reg "kimi"));
  check
    bool
    "openrouter exists"
    true
    (Option.is_some (Provider_registry.find reg "openrouter"));
  check bool "groq exists" true (Option.is_some (Provider_registry.find reg "groq"));
  check
    bool
    "deepseek exists"
    true
    (Option.is_some (Provider_registry.find reg "deepseek"));
  check bool "alibaba exists" true (Option.is_some (Provider_registry.find reg "alibaba"));
  check
    bool
    "dashscope exists"
    true
    (Option.is_some (Provider_registry.find reg "dashscope"));
  check
    bool
    "siliconflow exists"
    true
    (Option.is_some (Provider_registry.find reg "siliconflow"))
;;

let test_default_capabilities () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "claude" with
   | Some e ->
     check bool "claude has tools" true e.capabilities.supports_tools;
     check bool "claude has reasoning" true e.capabilities.supports_reasoning
   | None -> fail "claude should exist");
  match Provider_registry.find reg "nous" with
  | Some e ->
    check bool "llama has tools" true e.capabilities.supports_tools;
    check bool "llama has top_k" true e.capabilities.supports_top_k
  | None -> fail "llama should exist"
;;

let test_default_ollama_cloud_entry () =
  let reg = Provider_registry.default () in
  match Provider_registry.find reg "ollama_cloud" with
  | Some e ->
    check bool "kind is Ollama" true (e.defaults.kind = Provider_config.Ollama);
    check string "base_url" "https://ollama.com" e.defaults.base_url;
    check string "api_key_env" "OLLAMA_CLOUD_API_KEY" e.defaults.api_key_env;
    check string "request_path" "/api/chat" e.defaults.request_path
  | None -> fail "ollama_cloud should exist"
;;

let test_default_deepseek_entry () =
  let reg = Provider_registry.default () in
  match Provider_registry.find reg "deepseek" with
  | Some e ->
    check
      bool
      "kind is OpenAI_compat"
      true
      (e.defaults.kind = Provider_config.OpenAI_compat);
    check string "base_url" "https://api.deepseek.com" e.defaults.base_url;
    check string "api_key_env" "DEEPSEEK_API_KEY" e.defaults.api_key_env;
    check string "request_path" "/chat/completions" e.defaults.request_path
  | None -> fail "deepseek should exist"
;;

let test_default_deepseek_api_key_env () =
  let availability ~deepseek =
    with_env "DEEPSEEK_API_KEY" deepseek (fun () ->
      let reg = Provider_registry.default () in
      match Provider_registry.find reg "deepseek" with
      | Some e -> e.is_available ()
      | None -> fail "deepseek should exist")
  in
  check bool "no key unavailable" false (availability ~deepseek:"");
  check bool "documented key available" true (availability ~deepseek:"deepseek-secret")
;;

let test_provider_name_of_ollama_cloud_config () =
  let cfg =
    Provider_config.make
      ~kind:Provider_config.Ollama
      ~model_id:"glm-5.1:cloud"
      ~base_url:"https://ollama.com"
      ~request_path:"/api/chat"
      ()
  in
  check
    string
    "provider name"
    "ollama_cloud"
    (Provider_registry.provider_name_of_config cfg)
;;

let default_entry_base_url name =
  let reg = Provider_registry.default () in
  match Provider_registry.find reg name with
  | Some e -> e.defaults.base_url
  | None -> failf "%s should exist" name
;;

let test_default_nous_base_url_reads_llm_endpoints_at_call_time () =
  let first = "http://127.0.0.1:18085" in
  let second = "http://127.0.0.1:18086" in
  with_env "LLM_ENDPOINTS" first (fun () ->
    check string "first registry" first (default_entry_base_url "nous");
    Unix.putenv "LLM_ENDPOINTS" second;
    check string "second registry" second (default_entry_base_url "nous"))
;;

let test_default_ollama_base_url_reads_ollama_host_at_call_time () =
  let first = "http://127.0.0.1:19134" in
  let second = "http://127.0.0.1:19135" in
  with_env "OLLAMA_HOST" first (fun () ->
    check string "first registry" first (default_entry_base_url "ollama");
    Unix.putenv "OLLAMA_HOST" second;
    check string "second registry" second (default_entry_base_url "ollama"))
;;

let test_default_max_context () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "nous" with
   | Some e -> check int "llama 128K" 128_000 e.max_context
   | None -> fail "llama should exist");
  (match Provider_registry.find reg "claude" with
   | Some e -> check int "claude 200K" 200_000 e.max_context
   | None -> fail "claude should exist");
  (match Provider_registry.find reg "gemini" with
   | Some e -> check int "gemini 1M" 1_000_000 e.max_context
   | None -> fail "gemini should exist");
  (match Provider_registry.find reg "glm" with
   | Some e -> check int "glm 200K" 200_000 e.max_context
   | None -> fail "glm should exist");
  (match Provider_registry.find reg "kimi" with
   | Some e -> check int "kimi 262K" 262_144 e.max_context
   | None -> fail "kimi should exist");
  (match Provider_registry.find reg "groq" with
   | Some e -> check int "groq 131K" 131_072 e.max_context
   | None -> fail "groq should exist");
  (match Provider_registry.find reg "deepseek" with
   | Some e -> check int "deepseek 1M" 1_000_000 e.max_context
   | None -> fail "deepseek should exist");
  (match Provider_registry.find reg "dashscope" with
   | Some e -> check int "dashscope 131K" 131_072 e.max_context
   | None -> fail "dashscope should exist");
  (match Provider_registry.find reg "alibaba" with
   | Some e -> check int "alibaba 131K" 131_072 e.max_context
   | None -> fail "alibaba should exist");
  match Provider_registry.find reg "siliconflow" with
  | Some e -> check int "siliconflow 128K" 128_000 e.max_context
  | None -> fail "siliconflow should exist"
;;

let test_default_max_context_matches_capabilities () =
  let reg = Provider_registry.default () in
  Provider_registry.all reg
  |> List.iter (fun (entry : Provider_registry.entry) ->
    match entry.capabilities.max_context_tokens with
    | None -> ()
    | Some caps_ctx ->
      check
        bool
        (Printf.sprintf "%s registry max_context >= capabilities" entry.name)
        true
        (entry.max_context >= caps_ctx))
;;

let test_default_zai_base_urls () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "glm" with
   | Some e ->
     check string "glm base_url" Zai_catalog.general_base_url e.defaults.base_url;
     check string "glm api_key_env" "ZAI_API_KEY" e.defaults.api_key_env
   | None -> fail "glm should exist");
  (match Provider_registry.find reg "glm-coding" with
   | Some e ->
     check string "glm-coding base_url" Zai_catalog.coding_base_url e.defaults.base_url;
     check string "glm-coding api_key_env" "ZAI_CODING_API_KEY" e.defaults.api_key_env
   | None -> fail "glm-coding should exist");
  match Provider_registry.find reg "kimi" with
  | Some e ->
    check string "kimi base_url" "https://api.kimi.com/coding" e.defaults.base_url;
    check string "kimi request_path" "/v1/messages" e.defaults.request_path
  | None -> fail "kimi should exist"
;;

let test_glm_coding_api_key_env_isolated () =
  let prev_general = Sys.getenv_opt "ZAI_API_KEY" in
  let prev_coding = Sys.getenv_opt "ZAI_CODING_API_KEY" in
  let restore key = function
    | Some v -> Unix.putenv key v
    | None -> Unix.putenv key ""
  in
  Fun.protect
    ~finally:(fun () ->
      restore "ZAI_API_KEY" prev_general;
      restore "ZAI_CODING_API_KEY" prev_coding)
    (fun () ->
       Unix.putenv "ZAI_API_KEY" "general-key";
       Unix.putenv "ZAI_CODING_API_KEY" "";
       let general_only = Provider_registry.default () in
       (match Provider_registry.find general_only "glm-coding" with
        | Some e ->
          check bool "general key does not enable coding lane" false (e.is_available ())
        | None -> fail "glm-coding should exist");
       Unix.putenv "ZAI_CODING_API_KEY" "coding-key";
       let coding = Provider_registry.default () in
       match Provider_registry.find coding "glm-coding" with
       | Some e -> check bool "coding key enables coding lane" true (e.is_available ())
       | None -> fail "glm-coding should exist")
;;

let test_blank_zai_base_urls_fall_back () =
  let prev_general = Sys.getenv_opt "ZAI_BASE_URL" in
  let prev_coding = Sys.getenv_opt "ZAI_CODING_BASE_URL" in
  let restore key = function
    | Some v -> Unix.putenv key v
    | None -> Unix.putenv key ""
  in
  Fun.protect
    ~finally:(fun () ->
      restore "ZAI_BASE_URL" prev_general;
      restore "ZAI_CODING_BASE_URL" prev_coding)
    (fun () ->
       Unix.putenv "ZAI_BASE_URL" "   ";
       Unix.putenv "ZAI_CODING_BASE_URL" "\t";
       let reg = Provider_registry.default () in
       (match Provider_registry.find reg "glm" with
        | Some e ->
          check
            string
            "glm blank fallback"
            Zai_catalog.general_base_url
            e.defaults.base_url
        | None -> fail "glm should exist");
       match Provider_registry.find reg "glm-coding" with
       | Some e ->
         check
           string
           "glm-coding blank fallback"
           Zai_catalog.coding_base_url
           e.defaults.base_url
       | None -> fail "glm-coding should exist")
;;

(* ── Provider catalog overlay ────────────────────────── *)

let with_provider_catalog json f =
  match Provider_catalog.of_json (Yojson.Safe.from_string json) with
  | Error msg -> fail msg
  | Ok catalog ->
    Provider_catalog.set_global catalog;
    Fun.protect ~finally:Provider_catalog.clear_global f
;;

let test_catalog_overlay_adds_provider_and_alias () =
  with_provider_catalog
    {|{
      "schema_version": 1,
      "providers": [
        {
          "id": "vllm-local",
          "aliases": ["Subscriber-Local"],
          "kind": "openai_compat",
          "transport": "http",
          "base_url": "http://127.0.0.1:8000",
          "request_path": "/v1/chat/completions",
          "auth": {"type": "none"},
          "default_model": "local-model",
          "capabilities_base": "openai_chat",
          "capabilities": {
            "max_context_tokens": 131072,
            "supports_tools": true,
            "supports_tool_choice": true
          }
        }
      ]
    }|}
    (fun () ->
       let reg = Provider_registry.default () in
       (match Provider_registry.find reg "vllm-local" with
        | Some e ->
          check string "base url" "http://127.0.0.1:8000" e.defaults.base_url;
          check int "max context" 131_072 e.max_context;
          check bool "tools" true e.capabilities.supports_tools;
          check bool "tool choice" true e.capabilities.supports_tool_choice
        | None -> fail "catalog provider should be registered");
       match Provider_registry.find reg "subscriber-local" with
       | Some e ->
         check string "alias base url" "http://127.0.0.1:8000" e.defaults.base_url
       | None -> fail "catalog alias should be registered")
;;

let test_catalog_overlay_replaces_seed_provider () =
  with_provider_catalog
    {|{
      "schema_version": 1,
      "providers": [
        {
          "id": "openrouter",
          "kind": "openai_compat",
          "transport": "http",
          "base_url": "https://example.test/openrouter",
          "request_path": "/chat/completions",
          "auth": {"type": "api_key_env", "env": "OPENROUTER_API_KEY"},
          "capabilities_base": "openai_chat"
        }
      ]
    }|}
    (fun () ->
       let reg = Provider_registry.default () in
       match Provider_registry.find reg "openrouter" with
       | Some e ->
         check string "catalog wins" "https://example.test/openrouter" e.defaults.base_url
       | None -> fail "openrouter should still exist")
;;

let test_catalog_overlay_normalizes_provider_id () =
  with_provider_catalog
    {|{
      "schema_version": 1,
      "providers": [
        {
          "id": "Acme-Cloud",
          "kind": "openai_compat",
          "transport": "http",
          "base_url": "https://acme.example/v1",
          "auth": {"type": "none"},
          "capabilities_base": "openai_chat"
        }
      ]
    }|}
    (fun () ->
       let reg = Provider_registry.default () in
       match Provider_registry.find reg "acme-cloud" with
       | Some e -> check string "base url" "https://acme.example/v1" e.defaults.base_url
       | None -> fail "catalog provider id should be normalized")
;;

let test_catalog_rejects_empty_provider_id () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "  ", "kind": "openai_compat"}
           ]
         }|})
  with
  | Error _ -> ()
  | Ok _ -> fail "empty provider id should be rejected"
;;

let test_catalog_rejects_unknown_transport () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "x", "kind": "openai_compat", "transport": "ftp"}
           ]
         }|})
  with
  | Error _ -> ()
  | Ok _ -> fail "unknown transport should be rejected, not silently coerced"
;;

let test_catalog_rejects_unknown_auth_type () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "x", "kind": "openai_compat",
              "auth": {"type": "magick"}}
           ]
         }|})
  with
  | Error _ -> ()
  | Ok _ -> fail "unknown auth type should be rejected, not silently coerced to none"
;;

let test_catalog_rejects_unknown_capabilities_base () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "x", "kind": "openai_compat",
              "capabilities_base": "nonexistent_preset"}
           ]
         }|})
  with
  | Error _ -> ()
  | Ok _ ->
    fail "unknown capabilities_base should be rejected, not silently coerced to defaults"
;;

let test_catalog_rejects_unknown_thinking_control_format () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "x", "kind": "openai_compat",
              "capabilities": {"thinking_control_format": "telepathy"}}
           ]
         }|})
  with
  | Error _ -> ()
  | Ok _ ->
    fail "unknown thinking_control_format should be rejected, not silently coerced"
;;

let test_catalog_accepts_explicit_thinking_control_formats () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "kimi-k2", "kind": "openai_compat",
              "capabilities": {"thinking_control_format": "thinking_object_only"}},
             {"id": "dashscope", "kind": "openai_compat",
              "capabilities": {"thinking_control_format": "enable_thinking"}},
             {"id": "openai-reasoning", "kind": "openai_compat",
              "capabilities": {"thinking_control_format": "reasoning_effort",
                               "reasoning_visibility": "visible_text"}}
           ]
         }|})
  with
  | Error msg -> fail msg
  | Ok catalog ->
    let check_format id expected =
      match Provider_catalog.lookup catalog id with
      | Some entry ->
        check
          bool
          (id ^ " thinking_control_format")
          true
          (entry.capabilities.thinking_control_format = expected)
      | None -> fail (id ^ " should exist")
    in
    check_format "kimi-k2" Capabilities.Thinking_object_only;
    check_format "dashscope" Capabilities.Enable_thinking;
    check_format "openai-reasoning" Capabilities.Reasoning_effort;
    (match Provider_catalog.lookup catalog "openai-reasoning" with
     | Some entry ->
       check
         bool
         "openai-reasoning visible_text override"
         true
         (entry.capabilities.reasoning_visibility_override
          = Capabilities.Force_visible_text)
     | None -> fail "openai-reasoning should exist")
;;

let test_catalog_lookup_first_match_wins () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "dup", "kind": "openai_compat",
              "base_url": "http://first.example"},
             {"id": "dup", "kind": "openai_compat",
              "base_url": "http://second.example"}
           ]
         }|})
  with
  | Error msg -> fail msg
  | Ok catalog ->
    (match Provider_catalog.lookup catalog "dup" with
     | Some entry ->
       check
         string
         "first-match-wins resolves to earlier entry"
         "http://first.example"
         entry.base_url
     | None -> fail "duplicate id should still resolve to first entry")
;;

let test_catalog_lookup_case_insensitive () =
  match
    Provider_catalog.of_json
      (Yojson.Safe.from_string
         {|{
           "schema_version": 1,
           "providers": [
             {"id": "mixed-Case",
              "aliases": ["AlsoMixed"],
              "kind": "openai_compat",
              "base_url": "http://x.example"}
           ]
         }|})
  with
  | Error msg -> fail msg
  | Ok catalog ->
    check
      bool
      "id uppercase MIXED-CASE"
      true
      (Option.is_some (Provider_catalog.lookup catalog "MIXED-CASE"));
    check
      bool
      "id lowercase mixed-case"
      true
      (Option.is_some (Provider_catalog.lookup catalog "mixed-case"));
    check
      bool
      "alias uppercase ALSOMIXED"
      true
      (Option.is_some (Provider_catalog.lookup catalog "ALSOMIXED"));
    check
      bool
      "alias trim/whitespace"
      true
      (Option.is_some (Provider_catalog.lookup catalog "  alsomixed  "))
;;

(* Constructs Provider_catalog.t directly (bypassing JSON parse) so that
   empty/whitespace aliases survive to the registry overlay, where the
   register_name warn is emitted. JSON parse-time filtering would otherwise
   drop them before the overlay sees them. *)
let test_catalog_empty_alias_not_registered () =
  let entry : Provider_catalog.entry =
    { id = "host"
    ; aliases = [ "good-alias"; ""; "   " ]
    ; kind = OpenAI_compat
    ; transport = Http
    ; command = None
    ; base_url = "http://host.example"
    ; request_path = "/v1/chat/completions"
    ; api_key_env = ""
    ; auth = No_auth
    ; default_model = None
    ; max_context = None
    ; capabilities = Capabilities.default_capabilities
    ; credential_scope = None
    }
  in
  Provider_catalog.set_global [ entry ];
  let warns = ref [] in
  let sink level ~ctx msg =
    match level with
    | Diag.Warn -> warns := (ctx, msg) :: !warns
    | _ -> ()
  in
  Fun.protect ~finally:Provider_catalog.clear_global (fun () ->
    Diag.with_sink sink (fun () ->
      let reg = Provider_registry.default () in
      check bool "id registered" true (Option.is_some (Provider_registry.find reg "host"));
      check
        bool
        "good alias registered"
        true
        (Option.is_some (Provider_registry.find reg "good-alias"));
      check
        bool
        "empty alias not registered"
        false
        (Option.is_some (Provider_registry.find reg ""))));
  let starts_with ~prefix s =
    let plen = String.length prefix in
    String.length s >= plen && String.sub s 0 plen = prefix
  in
  let empty_alias_warns =
    List.filter
      (fun (ctx, msg) ->
         ctx = "provider_registry" && starts_with ~prefix:"ignoring empty alias" msg)
      !warns
  in
  check
    bool
    "Diag.warn fired from provider_registry for at least one empty alias"
    true
    (List.length empty_alias_warns >= 1)
;;

let test_catalog_load_file_and_lookup_alias () =
  let path = Filename.temp_file "provider-catalog" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () ->
       let oc = open_out path in
       Fun.protect
         ~finally:(fun () -> close_out_noerr oc)
         (fun () ->
            output_string
              oc
              {|{
                "schema_version": 1,
                "providers": [
                  {
                    "id": "file-cloud",
                    "aliases": ["file-cloud-alias"],
                    "kind": "openai_compat",
                    "transport": "http",
                    "base_url": "https://file-cloud.example/v1",
                    "default_model": "file-model",
                    "auth": {"type": "none"}
                  }
                ]
              }|});
       match Provider_catalog.load_file path with
       | Error msg -> fail msg
       | Ok catalog ->
         (match Provider_catalog.lookup catalog "file-cloud-alias" with
          | Some entry ->
            check string "id" "file-cloud" entry.id;
            check (option string) "default model" (Some "file-model") entry.default_model
          | None -> fail "catalog alias should resolve"))
;;

let test_catalog_api_key_env_availability () =
  let env_name = "OAS_TEST_PROVIDER_CATALOG_API_KEY" in
  let json =
    Printf.sprintf
      {|{
        "schema_version": 1,
        "providers": [
          {
            "id": "cloud-api",
            "kind": "openai_compat",
            "transport": "http",
            "base_url": "https://cloud-api.example/v1",
            "auth": {"type": "api_key_env", "env": "%s"},
            "capabilities_base": "openai_chat"
          }
        ]
      }|}
      env_name
  in
  let available_with value =
    with_env env_name value (fun () ->
      with_provider_catalog json (fun () ->
        let reg = Provider_registry.default () in
        match Provider_registry.find reg "cloud-api" with
        | Some entry -> entry.is_available ()
        | None -> fail "cloud-api should be registered"))
  in
  check bool "missing api key unavailable" false (available_with "");
  check bool "present api key available" true (available_with "secret")
;;

(* ── Types usage helpers ───────────────────────────── *)

let test_zero_api_usage () =
  let z = Types.zero_api_usage in
  check int "input 0" 0 z.input_tokens;
  check int "output 0" 0 z.output_tokens;
  check int "cache_creation 0" 0 z.cache_creation_input_tokens;
  check int "cache_read 0" 0 z.cache_read_input_tokens
;;

let test_usage_of_response_some () =
  let usage : Types.api_usage =
    { input_tokens = 100
    ; output_tokens = 50
    ; cache_creation_input_tokens = 10
    ; cache_read_input_tokens = 5
    ; cost_usd = None
    }
  in
  let resp : Types.api_response =
    { id = "r1"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage = Some usage
    ; telemetry = None
    }
  in
  match Types.usage_of_response resp with
  | Some u ->
    check int "input" 100 u.input_tokens;
    check int "output" 50 u.output_tokens
  | None -> fail "expected reported usage"
;;

let test_usage_of_response_none () =
  let resp : Types.api_response =
    { id = "r2"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "ok" ]
    ; usage = None
    ; telemetry = None
    }
  in
  check (option reject) "missing usage preserved" None (Types.usage_of_response resp)
;;

(* ── Capability_filter combinators ──────────────────── *)

let test_requires_any () =
  let caps = { Capabilities.default_capabilities with supports_reasoning = true } in
  check
    bool
    "any: reasoning or tools"
    true
    (Capability_filter.requires_any
       [ Capability_filter.requires_tools; Capability_filter.requires_reasoning ]
       caps);
  check
    bool
    "any: tools or streaming = false"
    false
    (Capability_filter.requires_any
       [ Capability_filter.requires_tools; Capability_filter.requires_streaming ]
       caps)
;;

(* ── Kind ↔ registry integrity ────────────────────────── *)

(** Minimal [Provider_config.t] construction for a given kind, using a
    localhost base URL so [is_local = true] for [OpenAI_compat] (resolves
    to the registry's "nous" entry) and a plain (non-coding) URL for
    [Glm] (resolves to "glm"). *)
let mk_config_for_kind kind =
  let base_url =
    match kind with
    | Provider_config.OpenAI_compat -> "http://127.0.0.1:8085"
    | _ -> "https://example.test"
  in
  Provider_config.make ~kind ~model_id:"test" ~base_url ()
;;

(** Regression guard for the masc-mcp capability-lookup bug (boundary-allow) fixed in
    masc-mcp#9306 (boundary-allow). That bug passed [Anthropicdapter.string_of_provider_kind]
    (masc canonical_name: "claude-api", ...) (boundary-allow) to
    [Provider_registry.find], but the registry is keyed on the names
    returned by [Provider_registry.provider_name_of_config] ("claude",
    "kimi", "nous", "ollama", "claude_code", "gemini", ...). For
    direct-API kinds the lookup silently fell back to
    [default_capabilities]; for CLI kinds the masc vocabulary (boundary-allow) happened
    to match direct-API entries ("claude" → Anthropic, "gemini" → Gemini,
    "kimi" → Kimi) and returned the wrong capability matrix.

    Assert here that [provider_name_of_config] is the authoritative key
    source: every variant in [Provider_config.all_provider_kinds]
    produces a name that resolves to [Some entry] in the default
    registry. Adding a variant without a corresponding registry
    registration fails this test. *)
let test_every_kind_resolves_in_registry () =
  let registry = Provider_registry.default () in
  List.iter
    (fun kind ->
       let cfg = mk_config_for_kind kind in
       let name = Provider_registry.provider_name_of_config cfg in
       let label =
         Printf.sprintf
           "kind=%s name=%s"
           (Provider_config.string_of_provider_kind kind)
           name
       in
       match Provider_registry.find registry name with
       | Some entry ->
         check
           string
           (Printf.sprintf "%s: entry.name echoes lookup key" label)
           name
           entry.name
       | None ->
         failf
           "%s: provider_name_of_config returned %S but registry has no entry for it; \
            either register the provider or fix the naming function"
           label
           name)
    Provider_config.all_provider_kinds
;;

(* ── Suite ──────────────────────────────────────────── *)

let () =
  run
    "provider_registry"
    [ ( "crud"
      , [ test_case "empty" `Quick test_empty_registry
        ; test_case "register and find" `Quick test_register_and_find
        ; test_case "overwrite" `Quick test_overwrite
        ; test_case "unregister" `Quick test_unregister
        ] )
    ; ( "availability"
      , [ test_case "filter" `Quick test_available_filter
        ; test_case
            "command_in_path finds binary"
            `Quick
            test_command_in_path_finds_binary
        ; test_case
            "command_in_path rejects directory"
            `Quick
            test_command_in_path_rejects_directory
        ; test_case
            "command_in_path misses unknown binary"
            `Quick
            test_command_in_path_misses_unknown_binary
        ] )
    ; ( "capabilities"
      , [ test_case "find with tools" `Quick test_find_capable_tools
        ; test_case "composite predicate" `Quick test_find_capable_composite
        ; test_case "requires_any" `Quick test_requires_any
        ] )
    ; ( "default"
      , [ test_case "has 14 providers" `Quick test_default_has_14
        ; test_case "correct capabilities" `Quick test_default_capabilities
        ; test_case "ollama_cloud entry" `Quick test_default_ollama_cloud_entry
        ; test_case "deepseek entry" `Quick test_default_deepseek_entry
        ; test_case "deepseek api key env" `Quick test_default_deepseek_api_key_env
        ; test_case
            "provider_name_of_config returns ollama_cloud"
            `Quick
            test_provider_name_of_ollama_cloud_config
        ; test_case
            "nous base_url reads LLM_ENDPOINTS at registry construction"
            `Quick
            test_default_nous_base_url_reads_llm_endpoints_at_call_time
        ; test_case
            "ollama base_url reads OLLAMA_HOST at registry construction"
            `Quick
            test_default_ollama_base_url_reads_ollama_host_at_call_time
        ; test_case "max_context values" `Quick test_default_max_context
        ; test_case
            "max_context matches capabilities"
            `Quick
            test_default_max_context_matches_capabilities
        ; test_case "zai base urls" `Quick test_default_zai_base_urls
        ; test_case
            "glm coding api key env isolated"
            `Quick
            test_glm_coding_api_key_env_isolated
        ; test_case
            "blank zai base urls fall back"
            `Quick
            test_blank_zai_base_urls_fall_back
        ] )
    ; ( "provider_catalog"
      , [ test_case
            "overlay adds provider and alias"
            `Quick
            test_catalog_overlay_adds_provider_and_alias
        ; test_case
            "overlay replaces seed provider"
            `Quick
            test_catalog_overlay_replaces_seed_provider
        ; test_case
            "overlay normalizes provider id"
            `Quick
            test_catalog_overlay_normalizes_provider_id
        ; test_case
            "rejects empty provider id"
            `Quick
            test_catalog_rejects_empty_provider_id
        ; test_case
            "rejects unknown transport"
            `Quick
            test_catalog_rejects_unknown_transport
        ; test_case
            "rejects unknown auth type"
            `Quick
            test_catalog_rejects_unknown_auth_type
        ; test_case
            "rejects unknown capabilities_base"
            `Quick
            test_catalog_rejects_unknown_capabilities_base
        ; test_case
            "rejects unknown thinking_control_format"
            `Quick
            test_catalog_rejects_unknown_thinking_control_format
        ; test_case
            "accepts explicit thinking_control_format values"
            `Quick
            test_catalog_accepts_explicit_thinking_control_formats
        ; test_case
            "lookup first-match-wins on duplicate id"
            `Quick
            test_catalog_lookup_first_match_wins
        ; test_case
            "lookup is case-insensitive"
            `Quick
            test_catalog_lookup_case_insensitive
        ; test_case
            "empty alias not registered"
            `Quick
            test_catalog_empty_alias_not_registered
        ; test_case
            "load_file and lookup alias"
            `Quick
            test_catalog_load_file_and_lookup_alias
        ; test_case
            "api key env gates availability"
            `Quick
            test_catalog_api_key_env_availability
        ] )
    ; ( "kind_registry_integrity"
      , [ test_case "every kind resolves" `Quick test_every_kind_resolves_in_registry ] )
    ; ( "types_usage"
      , [ test_case "zero_api_usage" `Quick test_zero_api_usage
        ; test_case "usage_of_response some" `Quick test_usage_of_response_some
        ; test_case "usage_of_response none" `Quick test_usage_of_response_none
        ] )
    ]
;;
