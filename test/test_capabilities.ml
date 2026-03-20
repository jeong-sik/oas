(** Tests for expanded capabilities — model lookup, limits, feature flags. *)

open Alcotest
open Llm_provider

(* ── Default capabilities ────────────────────────────── *)

let test_default_no_limits () =
  let c = Capabilities.default_capabilities in
  check bool "no context limit" true (c.max_context_tokens = None);
  check bool "no output limit" true (c.max_output_tokens = None);
  check bool "no tools" false c.supports_tools;
  check bool "system prompt default true" true c.supports_system_prompt

let test_default_new_fields_false () =
  let c = Capabilities.default_capabilities in
  check bool "no parallel tools" false c.supports_parallel_tool_calls;
  check bool "no extended thinking" false c.supports_extended_thinking;
  check bool "no structured output" false c.supports_structured_output;
  check bool "no image input" false c.supports_image_input;
  check bool "no audio input" false c.supports_audio_input;
  check bool "no video input" false c.supports_video_input;
  check bool "no caching" false c.supports_caching;
  check bool "no computer use" false c.supports_computer_use;
  check bool "no code execution" false c.supports_code_execution

(* ── Preset capabilities ─────────────────────────────── *)

let test_anthropic_capabilities () =
  let c = Capabilities.anthropic_capabilities in
  check bool "has tools" true c.supports_tools;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "has extended thinking" true c.supports_extended_thinking;
  check bool "has reasoning budget" true c.supports_reasoning_budget;
  check bool "has image" true c.supports_image_input;
  check bool "has caching" true c.supports_caching;
  check bool "has computer use" true c.supports_computer_use;
  check bool "no audio" false c.supports_audio_input;
  check bool "context 200K" true (c.max_context_tokens = Some 200_000)

let test_openai_capabilities () =
  let c = Capabilities.openai_chat_capabilities in
  check bool "has structured output" true c.supports_structured_output;
  check bool "has parallel tools" true c.supports_parallel_tool_calls;
  check bool "no reasoning" false c.supports_reasoning;
  check bool "context 128K" true (c.max_context_tokens = Some 128_000)

let test_openai_extended () =
  let c = Capabilities.openai_chat_extended_capabilities in
  check bool "has reasoning" true c.supports_reasoning;
  check bool "has top_k" true c.supports_top_k;
  check bool "has min_p" true c.supports_min_p

(* ── Model lookup ────────────────────────────────────── *)

let test_lookup_claude_opus () =
  match Capabilities.for_model_id "claude-opus-4-6" with
  | Some c ->
    check (option int) "context 1M" (Some 1_000_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match claude-opus"

let test_lookup_claude_sonnet () =
  match Capabilities.for_model_id "claude-sonnet-4-6" with
  | Some c ->
    check (option int) "output 64K" (Some 64_000) c.max_output_tokens;
    check bool "parallel tools" true c.supports_parallel_tool_calls
  | None -> fail "should match claude-sonnet"

let test_lookup_gpt5 () =
  match Capabilities.for_model_id "gpt-5.4" with
  | Some c ->
    check (option int) "context 1.05M" (Some 1_050_000) c.max_context_tokens;
    check (option int) "output 128K" (Some 128_000) c.max_output_tokens;
    check bool "structured output" true c.supports_structured_output;
    check bool "computer use" true c.supports_computer_use
  | None -> fail "should match gpt-5"

let test_lookup_gemini () =
  match Capabilities.for_model_id "gemini-3.1-pro" with
  | Some c ->
    check bool "audio" true c.supports_audio_input;
    check bool "video" true c.supports_video_input;
    check bool "code execution" true c.supports_code_execution;
    check bool "structured output" true c.supports_structured_output
  | None -> fail "should match gemini"

let test_lookup_qwen () =
  match Capabilities.for_model_id "qwen3.5-35b-a3b" with
  | Some c ->
    check (option int) "context 262K" (Some 262_144) c.max_context_tokens;
    check bool "tools" true c.supports_tools;
    check bool "thinking" true c.supports_extended_thinking;
    check bool "top_k" true c.supports_top_k
  | None -> fail "should match qwen3"

let test_lookup_deepseek_r1 () =
  match Capabilities.for_model_id "deepseek-r1" with
  | Some c ->
    check bool "NO tools" false c.supports_tools;
    check bool "reasoning" true c.supports_reasoning;
    check (option int) "output 8K" (Some 8_000) c.max_output_tokens
  | None -> fail "should match deepseek-r1"

let test_lookup_grok () =
  match Capabilities.for_model_id "grok-4" with
  | Some c ->
    check (option int) "context 2M" (Some 2_000_000) c.max_context_tokens;
    check bool "structured" true c.supports_structured_output
  | None -> fail "should match grok"

let test_lookup_unknown () =
  check bool "unknown returns None" true
    (Capabilities.for_model_id "totally-unknown-model" = None)

let test_lookup_case_insensitive () =
  check bool "uppercase matches" true
    (Capabilities.for_model_id "Claude-Opus-4-6" <> None)

(* ── with_context_size ───────────────────────────────── *)

let test_with_context_size () =
  let c = Capabilities.default_capabilities in
  let c2 = Capabilities.with_context_size c ~ctx_size:131072 in
  check (option int) "ctx_size set" (Some 131072) c2.max_context_tokens;
  check bool "other fields unchanged" false c2.supports_tools

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run "Capabilities" [
    "defaults", [
      test_case "no limits" `Quick test_default_no_limits;
      test_case "new fields false" `Quick test_default_new_fields_false;
    ];
    "presets", [
      test_case "anthropic" `Quick test_anthropic_capabilities;
      test_case "openai" `Quick test_openai_capabilities;
      test_case "openai extended" `Quick test_openai_extended;
    ];
    "model_lookup", [
      test_case "claude opus" `Quick test_lookup_claude_opus;
      test_case "claude sonnet" `Quick test_lookup_claude_sonnet;
      test_case "gpt-5" `Quick test_lookup_gpt5;
      test_case "gemini" `Quick test_lookup_gemini;
      test_case "qwen" `Quick test_lookup_qwen;
      test_case "deepseek r1 no tools" `Quick test_lookup_deepseek_r1;
      test_case "grok 2M context" `Quick test_lookup_grok;
      test_case "unknown" `Quick test_lookup_unknown;
      test_case "case insensitive" `Quick test_lookup_case_insensitive;
    ];
    "merge", [
      test_case "with_context_size" `Quick test_with_context_size;
    ];
  ]
