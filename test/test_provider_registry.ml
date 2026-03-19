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
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> avail);
  } in
  Provider_registry.register reg (mk "up" true);
  Provider_registry.register reg (mk "down" false);
  let avail = Provider_registry.available reg in
  check int "only 1 available" 1 (List.length avail);
  check string "the up one" "up" (List.hd avail).name

(* ── Capability queries ─────────────────────────────── *)

let test_find_capable_tools () =
  let reg = Provider_registry.create () in
  let mk name caps : Provider_registry.entry = {
    name;
    defaults = {
      kind = OpenAI_compat; base_url = "http://x";
      api_key_env = ""; request_path = "/v1/chat/completions";
    };
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

let test_default_has_5 () =
  let reg = Provider_registry.default () in
  let all = Provider_registry.all reg in
  check int "5 known providers" 5 (List.length all);
  check bool "llama exists" true
    (Option.is_some (Provider_registry.find reg "llama"));
  check bool "claude exists" true
    (Option.is_some (Provider_registry.find reg "claude"));
  check bool "gemini exists" true
    (Option.is_some (Provider_registry.find reg "gemini"));
  check bool "glm exists" true
    (Option.is_some (Provider_registry.find reg "glm"));
  check bool "openrouter exists" true
    (Option.is_some (Provider_registry.find reg "openrouter"))

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
    ];
    "capabilities", [
      test_case "find with tools" `Quick test_find_capable_tools;
      test_case "composite predicate" `Quick test_find_capable_composite;
      test_case "requires_any" `Quick test_requires_any;
    ];
    "default", [
      test_case "has 5 providers" `Quick test_default_has_5;
      test_case "correct capabilities" `Quick test_default_capabilities;
    ];
  ]
