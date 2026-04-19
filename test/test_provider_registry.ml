(** Tests for Provider_registry and Capability_filter (v0.69.0). *)

open Alcotest
open Llm_provider

(* ── Registry CRUD ──────────────────────────────────── *)

let test_empty_registry () =
  let reg = Provider_registry.create () in
  check int "empty has 0 entries" 0 (List.length (Provider_registry.all reg));
  check (option reject) "find on empty is None"
    None (Provider_registry.find reg "nope")

let test_register_and_find () =
  let reg = Provider_registry.create () in
  let entry : Provider_registry.entry = {
    name = "test-provider";
    defaults = {
      kind = OpenAI_compat;
      base_url = "http://localhost:9999";
      api_key_env = "";
      request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> true);
  } in
  Provider_registry.register reg entry;
  (match Provider_registry.find reg "test-provider" with
   | Some e -> check string "name" "test-provider" e.name
   | None -> fail "should find registered provider");
  check int "1 entry" 1 (List.length (Provider_registry.all reg))

let test_overwrite () =
  let reg = Provider_registry.create () in
  let mk url : Provider_registry.entry = {
    name = "p";
    defaults = {
      kind = OpenAI_compat; base_url = url;
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> true);
  } in
  Provider_registry.register reg (mk "http://old");
  Provider_registry.register reg (mk "http://new");
  (match Provider_registry.find reg "p" with
   | Some e -> check string "updated url" "http://new" e.defaults.base_url
   | None -> fail "should exist");
  check int "still 1 entry" 1 (List.length (Provider_registry.all reg))

let test_unregister () =
  let reg = Provider_registry.create () in
  let entry : Provider_registry.entry = {
    name = "temp";
    defaults = {
      kind = OpenAI_compat; base_url = "http://x";
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> true);
  } in
  Provider_registry.register reg entry;
  Provider_registry.unregister reg "temp";
  check (option reject) "gone" None (Provider_registry.find reg "temp");
  check int "0 entries" 0 (List.length (Provider_registry.all reg))

(* ── Availability ───────────────────────────────────── *)

let test_available_filter () =
  let reg = Provider_registry.create () in
  let mk name avail : Provider_registry.entry = {
    name;
    defaults = {
      kind = OpenAI_compat; base_url = "http://x";
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> avail);
  } in
  Provider_registry.register reg (mk "up" true);
  Provider_registry.register reg (mk "down" false);
  let avail = Provider_registry.available reg in
  check int "only 1 available" 1 (List.length avail);
  check string "the up one" "up" (List.hd avail).name

let test_command_in_path_finds_binary () =
  let tmp = Filename.temp_file "provider-registry" ".bin" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
    (fun () ->
      let dir = Filename.dirname tmp in
      let name = Filename.basename tmp in
      check bool "binary found" true
        (Provider_registry.command_in_path ~path:dir name))

let test_command_in_path_rejects_directory () =
  let dir = Filename.temp_file "provider-registry" ".dir" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> try Unix.rmdir dir with Unix.Unix_error _ -> ())
    (fun () ->
      let parent = Filename.dirname dir in
      let name = Filename.basename dir in
      check bool "directory is not runnable" false
        (Provider_registry.command_in_path ~path:parent name))

let test_command_in_path_misses_unknown_binary () =
  let dir = Filename.get_temp_dir_name () in
  check bool "missing binary" false
    (Provider_registry.command_in_path ~path:dir "provider-registry-missing-binary")

(* ── Capability queries ─────────────────────────────── *)

let test_find_capable_tools () =
  let reg = Provider_registry.create () in
  let mk name caps : Provider_registry.entry = {
    name;
    defaults = {
      kind = OpenAI_compat; base_url = "http://x";
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = caps;
    is_available = (fun () -> true);
  } in
  Provider_registry.register reg
    (mk "with-tools" { Capabilities.default_capabilities with supports_tools = true });
  Provider_registry.register reg
    (mk "no-tools" Capabilities.default_capabilities);
  let capable = Provider_registry.find_capable reg Capability_filter.requires_tools in
  check int "1 with tools" 1 (List.length capable);
  check string "correct one" "with-tools" (List.hd capable).name

let test_find_capable_composite () =
  let reg = Provider_registry.create () in
  let mk name caps : Provider_registry.entry = {
    name;
    defaults = {
      kind = OpenAI_compat; base_url = "http://x";
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
    max_context = 128_000;
    capabilities = caps;
    is_available = (fun () -> true);
  } in
  Provider_registry.register reg
    (mk "full" { Capabilities.default_capabilities with
                 supports_tools = true; supports_reasoning = true });
  Provider_registry.register reg
    (mk "tools-only" { Capabilities.default_capabilities with supports_tools = true });
  Provider_registry.register reg
    (mk "none" Capabilities.default_capabilities);
  let need_both = Capability_filter.requires_all
    [Capability_filter.requires_tools; Capability_filter.requires_reasoning] in
  let capable = Provider_registry.find_capable reg need_both in
  check int "only full matches" 1 (List.length capable);
  check string "the full one" "full" (List.hd capable).name

(* ── Default registry ───────────────────────────────── *)

let test_default_has_15 () =
  let reg = Provider_registry.default () in
  let all = Provider_registry.all reg in
  check int "15 known providers" 15 (List.length all);
  check bool "llama exists" true
    (Option.is_some (Provider_registry.find reg "llama"));
  check bool "ollama exists" true
    (Option.is_some (Provider_registry.find reg "ollama"));
  check bool "claude exists" true
    (Option.is_some (Provider_registry.find reg "claude"));
  check bool "gemini exists" true
    (Option.is_some (Provider_registry.find reg "gemini"));
  check bool "glm exists" true
    (Option.is_some (Provider_registry.find reg "glm"));
  check bool "glm-coding exists" true
    (Option.is_some (Provider_registry.find reg "glm-coding"));
  check bool "openrouter exists" true
    (Option.is_some (Provider_registry.find reg "openrouter"));
  check bool "groq exists" true
    (Option.is_some (Provider_registry.find reg "groq"));
  check bool "deepseek exists" true
    (Option.is_some (Provider_registry.find reg "deepseek"));
  check bool "alibaba exists" true
    (Option.is_some (Provider_registry.find reg "alibaba"));
  check bool "siliconflow exists" true
    (Option.is_some (Provider_registry.find reg "siliconflow"));
  check bool "claude_code exists" true
    (Option.is_some (Provider_registry.find reg "claude_code"));
  check bool "cc exists" true
    (Option.is_some (Provider_registry.find reg "cc"));
  check bool "gemini_cli exists" true
    (Option.is_some (Provider_registry.find reg "gemini_cli"));
  check bool "codex_cli exists" true
    (Option.is_some (Provider_registry.find reg "codex_cli"))

let test_default_capabilities () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "claude" with
   | Some e ->
     check bool "claude has tools" true e.capabilities.supports_tools;
     check bool "claude has reasoning" true e.capabilities.supports_reasoning
   | None -> fail "claude should exist");
  (match Provider_registry.find reg "llama" with
   | Some e ->
     check bool "llama has tools" true e.capabilities.supports_tools;
     check bool "llama has top_k" true e.capabilities.supports_top_k
   | None -> fail "llama should exist")

let test_default_max_context () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "llama" with
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
  (match Provider_registry.find reg "cc" with
   | Some e -> check int "cc 1M" 1_000_000 e.max_context
   | None -> fail "cc should exist");
  (match Provider_registry.find reg "groq" with
   | Some e -> check int "groq 131K" 131_072 e.max_context
   | None -> fail "groq should exist");
  (match Provider_registry.find reg "deepseek" with
   | Some e -> check int "deepseek 128K" 128_000 e.max_context
   | None -> fail "deepseek should exist");
  (match Provider_registry.find reg "alibaba" with
   | Some e -> check int "alibaba 131K" 131_072 e.max_context
   | None -> fail "alibaba should exist");
  (match Provider_registry.find reg "siliconflow" with
   | Some e -> check int "siliconflow 128K" 128_000 e.max_context
   | None -> fail "siliconflow should exist");
  (match Provider_registry.find reg "codex_cli" with
   | Some e -> check int "codex_cli 1.05M" 1_050_000 e.max_context
   | None -> fail "codex_cli should exist")

let test_default_max_context_matches_capabilities () =
  let reg = Provider_registry.default () in
  Provider_registry.all reg
  |> List.iter (fun (entry : Provider_registry.entry) ->
    match entry.capabilities.max_context_tokens with
    | None -> ()
    | Some caps_ctx ->
      check bool
        (Printf.sprintf "%s registry max_context >= capabilities" entry.name)
        true
        (entry.max_context >= caps_ctx))

let test_default_zai_base_urls () =
  let reg = Provider_registry.default () in
  (match Provider_registry.find reg "glm" with
   | Some e ->
       check string "glm base_url" Zai_catalog.general_base_url
         e.defaults.base_url
   | None -> fail "glm should exist");
  (match Provider_registry.find reg "glm-coding" with
   | Some e ->
       check string "glm-coding base_url" Zai_catalog.coding_base_url
         e.defaults.base_url
   | None -> fail "glm-coding should exist")

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
           check string "glm blank fallback" Zai_catalog.general_base_url
             e.defaults.base_url
       | None -> fail "glm should exist");
      (match Provider_registry.find reg "glm-coding" with
       | Some e ->
           check string "glm-coding blank fallback" Zai_catalog.coding_base_url
             e.defaults.base_url
       | None -> fail "glm-coding should exist"))

(* ── Types usage helpers ───────────────────────────── *)

let test_zero_api_usage () =
  let z = Types.zero_api_usage in
  check int "input 0" 0 z.input_tokens;
  check int "output 0" 0 z.output_tokens;
  check int "cache_creation 0" 0 z.cache_creation_input_tokens;
  check int "cache_read 0" 0 z.cache_read_input_tokens

let test_usage_of_response_some () =
  let usage : Types.api_usage =
    { input_tokens = 100; output_tokens = 50;
      cache_creation_input_tokens = 10; cache_read_input_tokens = 5 ; cost_usd = None } in
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [Text "ok"]; usage = Some usage; telemetry = None } in
  let u = Types.usage_of_response resp in
  check int "input" 100 u.input_tokens;
  check int "output" 50 u.output_tokens

let test_usage_of_response_none () =
  let resp : Types.api_response =
    { id = "r2"; model = "m"; stop_reason = EndTurn;
      content = [Text "ok"]; usage = None; telemetry = None } in
  let u = Types.usage_of_response resp in
  check int "input fallback" 0 u.input_tokens;
  check int "output fallback" 0 u.output_tokens

(* ── Capability_filter combinators ──────────────────── *)

let test_requires_any () =
  let caps = { Capabilities.default_capabilities with
               supports_reasoning = true } in
  check bool "any: reasoning or tools" true
    (Capability_filter.requires_any
       [Capability_filter.requires_tools; Capability_filter.requires_reasoning] caps);
  check bool "any: tools or streaming = false" false
    (Capability_filter.requires_any
       [Capability_filter.requires_tools; Capability_filter.requires_streaming] caps)

(* ── Suite ──────────────────────────────────────────── *)

let () =
  run "provider_registry" [
    "crud", [
      test_case "empty" `Quick test_empty_registry;
      test_case "register and find" `Quick test_register_and_find;
      test_case "overwrite" `Quick test_overwrite;
      test_case "unregister" `Quick test_unregister;
    ];
    "availability", [
      test_case "filter" `Quick test_available_filter;
      test_case "command_in_path finds binary" `Quick test_command_in_path_finds_binary;
      test_case "command_in_path rejects directory" `Quick test_command_in_path_rejects_directory;
      test_case "command_in_path misses unknown binary" `Quick test_command_in_path_misses_unknown_binary;
    ];
    "capabilities", [
      test_case "find with tools" `Quick test_find_capable_tools;
      test_case "composite predicate" `Quick test_find_capable_composite;
      test_case "requires_any" `Quick test_requires_any;
    ];
    "default", [
      test_case "has 15 providers" `Quick test_default_has_15;
      test_case "correct capabilities" `Quick test_default_capabilities;
      test_case "max_context values" `Quick test_default_max_context;
      test_case "max_context matches capabilities" `Quick
        test_default_max_context_matches_capabilities;
      test_case "zai base urls" `Quick test_default_zai_base_urls;
      test_case "blank zai base urls fall back" `Quick test_blank_zai_base_urls_fall_back;
    ];
    "types_usage", [
      test_case "zero_api_usage" `Quick test_zero_api_usage;
      test_case "usage_of_response some" `Quick test_usage_of_response_some;
      test_case "usage_of_response none" `Quick test_usage_of_response_none;
    ];
  ]
