(** Provider capabilities -- per-provider/model feature flags and limits.

    Tracks what a provider/model supports so the agent runtime can make
    correct decisions (e.g., context reduction, tool filtering, thinking
    budget enforcement).

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split *)

type capabilities = {
  (* ── Numeric limits ────────────────────────────────── *)
  max_context_tokens: int option;  (** Model's context window. None = unknown. *)
  max_output_tokens: int option;   (** Model's max output. None = unknown. *)

  (* ── Tool use ──────────────────────────────────────── *)
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_parallel_tool_calls: bool;

  (* ── Thinking / reasoning ──────────────────────────── *)
  supports_reasoning: bool;             (** Any form of reasoning/thinking *)
  supports_extended_thinking: bool;     (** budget_tokens / reasoning_effort *)
  supports_reasoning_budget: bool;      (** Controllable reasoning depth *)

  (* ── Output format ─────────────────────────────────── *)
  supports_response_format_json: bool;  (** JSON mode *)
  supports_structured_output: bool;     (** JSON schema 100% guarantee *)

  (* ── Input modalities ──────────────────────────────── *)
  supports_multimodal_inputs: bool;     (** Any non-text input *)
  supports_image_input: bool;
  supports_audio_input: bool;
  supports_video_input: bool;

  (* ── Protocol ──────────────────────────────────────── *)
  supports_native_streaming: bool;
  supports_system_prompt: bool;
  supports_caching: bool;

  (* ── Sampling parameters ───────────────────────────── *)
  supports_top_k: bool;
  supports_min_p: bool;

  (* ── Advanced modalities ───────────────────────────── *)
  supports_computer_use: bool;
  supports_code_execution: bool;

  (* ── Provider identity ───────────────────────────────── *)
  is_ollama: bool;
}

let default_capabilities = {
  max_context_tokens = None;
  max_output_tokens = None;
  supports_tools = false;
  supports_tool_choice = false;
  supports_parallel_tool_calls = false;
  supports_reasoning = false;
  supports_extended_thinking = false;
  supports_reasoning_budget = false;
  supports_response_format_json = false;
  supports_structured_output = false;
  supports_multimodal_inputs = false;
  supports_image_input = false;
  supports_audio_input = false;
  supports_video_input = false;
  supports_native_streaming = false;
  supports_system_prompt = true;  (* most models support it *)
  supports_caching = false;
  supports_top_k = false;
  supports_min_p = false;
  supports_computer_use = false;
  supports_code_execution = false;
  is_ollama = false;
}

let anthropic_capabilities = {
  default_capabilities with
  max_context_tokens = Some 200_000;  (* default; opus/sonnet 4.6 = 1M *)
  max_output_tokens = Some 8_192;     (* default; higher for newer models *)
  supports_tools = true;
  supports_tool_choice = true;
  supports_parallel_tool_calls = true;
  supports_reasoning = true;
  supports_extended_thinking = true;
  supports_reasoning_budget = true;
  supports_structured_output = true;
  supports_multimodal_inputs = true;
  supports_image_input = true;
  supports_native_streaming = true;
  supports_caching = true;
  supports_computer_use = true;
  (* Anthropic Messages API documents [top_k] as a valid sampling
     parameter ("Only sample from the top K options for each
     subsequent token", docs.anthropic.com/en/api/messages body
     params). [backend_anthropic.build_request] already serializes
     [config.top_k] unconditionally when [Some]; the capability record
     must match so cross-layer consumers (the #831 Api_openai gate,
     the #830 Backend_openai gate, and the capability_filter passes)
     do not silently drop top_k when the caller routes an Anthropic
     config through a capability-checking path. [supports_min_p]
     remains [false] — Anthropic does not accept min_p. *)
  supports_top_k = true;
}

let openai_chat_capabilities = {
  default_capabilities with
  max_context_tokens = Some 128_000;
  max_output_tokens = Some 16_384;
  supports_tools = true;
  supports_tool_choice = true;
  supports_parallel_tool_calls = true;
  supports_response_format_json = true;
  supports_structured_output = true;
  supports_multimodal_inputs = true;
  supports_image_input = true;
  supports_native_streaming = true;
  supports_caching = true;
}

let openai_chat_extended_capabilities = {
  openai_chat_capabilities with
  supports_reasoning = true;
  supports_extended_thinking = true;
  supports_reasoning_budget = true;
  supports_top_k = true;
  supports_min_p = true;
}

(* Ollama OpenAI-compat endpoint behavior on tool_choice is model-dependent
   (docs.ollama.com/capabilities/tool-calling: the parameter is silently
   ignored for some models). Some Qwen3.5 deployments w/ native Jinja
   chat template do honor tool_choice:required in practice.

   Industry context: LiteLLM's model_prices_and_context_window.json
   registry lacks per-Ollama-model tool support flags for the same
   reason (see BerriAI/litellm#14067 — Ollama model metadata surfaces
   no authoritative capability flag, so any static table is a guess).

   Design choice here: declaration-over-probing. The default stays
   conservative (supports_tool_choice = false → contract relaxes to
   Allow_text_or_tool, text-only replies accepted even when the consumer
   asked for a tool). Consumers who have verified their model-side
   support declare it per Provider_config via
   [Provider_config.supports_tool_choice_override]. The SDK does not
   match on [model_id] to guess model-side behavior — the consumer
   (e.g. a config loader that knows it deployed Qwen3.5 w/ the Jinja
   chat template) owns that policy. This is stricter than LiteLLM's
   static-table approach, which requires JSON edits + redeploy to
   flip capability, and avoids the fragile model_id pattern match that
   the Claude Agent SDK sidesteps by being single-provider. *)
let ollama_capabilities = {
  openai_chat_extended_capabilities with
  supports_tool_choice = false;
  supports_min_p = false;
  is_ollama = true;
}

let glm_capabilities = {
  default_capabilities with
  max_context_tokens = Some 200_000;
  (* GLM-5.1 API enforces max_tokens <= 40960 at request time; keeping a
     higher value here causes server-side rejection with
     "Invalid request: `max_tokens` must be less than or equal to `40960`".
     Empirical upper bound observed on 2026-04-12 during keeper unified
     turns against glm-coding:glm-5.1 and glm:glm-5.1. *)
  max_output_tokens = Some 40_960;
  supports_tools = true;
  (* GLM does not reliably honor tool_choice=required/Any — it frequently
     returns a text-only response even when the caller sets the field.
     Observed empirically on 2026-04-18 (MASC cascade vendor_mix_balanced
     8+ CompletionContractViolation events in a single session against
     glm-5-turbo / glm-4.7 / glm-5.1 via both glm: and glm-coding: prefix).
     Cross-reference: BFCL tool-calling benchmarks rank GLM-4.5 at 77 and
     local ~GLM families at 67 — tool routing works but is unreliable.

     Flipping this to [false] causes
     [Completion_contract.of_tool_choice ~supports_tool_choice:false]
     to relax any tool_choice contract to [Allow_text_or_tool], so a
     text response is accepted without raising
     [CompletionContractViolation].  [supports_tools = true] remains
     unchanged — tool DESCRIPTIONS are sent and the model may still
     emit tool_use blocks when it decides to; we just don't error when
     it picks text for a request that asked for a forced tool call. *)
  supports_tool_choice = false;
  supports_reasoning = true;
  supports_extended_thinking = true;
  supports_response_format_json = true;
  supports_native_streaming = true;
}

let gemini_capabilities = {
  default_capabilities with
  max_context_tokens = Some 1_000_000;
  max_output_tokens = Some 65_000;
  supports_tools = true;
  supports_tool_choice = true;
  supports_parallel_tool_calls = true;
  supports_reasoning = true;
  supports_extended_thinking = true;
  supports_reasoning_budget = true;
  supports_response_format_json = true;
  supports_structured_output = true;
  supports_multimodal_inputs = true;
  supports_image_input = true;
  supports_audio_input = true;
  supports_video_input = true;
  supports_native_streaming = true;
  supports_caching = true;
  supports_code_execution = true;
  (* Google Gemini's generateContent API documents [topK] as part of
     generationConfig (ai.google.dev/api/generate-content). The
     [backend_gemini.build_request] serializer already emits it at
     lib/llm_provider/backend_gemini.ml:162-164, so the capability
     record must match. Same discrepancy story as anthropic_capabilities
     (#832) — OpenAI-compat consumers that route a Gemini config
     through a capability-checking path were silently dropping top_k.
     [supports_min_p] stays false; Gemini's generationConfig has no
     min_p field. *)
  supports_top_k = true;
}

let claude_code_capabilities = {
  anthropic_capabilities with
  max_context_tokens = Some 1_000_000;  (* 1M context via Claude Code *)
  max_output_tokens = Some 64_000;
  supports_structured_output = false;
  supports_computer_use = true;
  supports_code_execution = true;
}

let gemini_cli_capabilities = {
  default_capabilities with
  max_context_tokens = Some 1_000_000;
  max_output_tokens = Some 65_000;
  supports_tools = false;
  supports_tool_choice = false;
  supports_native_streaming = false;
  supports_system_prompt = true;
}

let codex_cli_capabilities = {
  default_capabilities with
  max_context_tokens = Some 1_050_000;
  max_output_tokens = Some 32_000;
  supports_tools = false;
  supports_tool_choice = false;
  supports_native_streaming = false;
  supports_system_prompt = true;
}

(* ── Model-specific overrides (lookup table) ─────────── *)

(** Lookup capabilities by model_id prefix.
    Returns None if no specific override is known. *)
let for_model_id model_id =
  (* Normalize: lowercase, strip quantization suffixes *)
  let m = String.lowercase_ascii model_id in
  let starts_with prefix = String.length m >= String.length prefix &&
    String.sub m 0 (String.length prefix) = prefix in
  if starts_with "claude-opus-4" then
    Some { anthropic_capabilities with
           max_context_tokens = Some 1_000_000;
           max_output_tokens = Some 128_000 }
  else if starts_with "claude-sonnet-4" then
    Some { anthropic_capabilities with
           max_context_tokens = Some 1_000_000;
           max_output_tokens = Some 64_000 }
  else if starts_with "claude-haiku-4" then
    Some { anthropic_capabilities with
           max_context_tokens = Some 200_000;
           max_output_tokens = Some 8_192 }
  else if starts_with "gpt-5" then
    Some { openai_chat_extended_capabilities with
           max_context_tokens = Some 1_050_000;
           max_output_tokens = Some 128_000;
           supports_computer_use = true }
  else if starts_with "gpt-4.1" then
    Some { openai_chat_capabilities with
           max_context_tokens = Some 1_000_000;
           max_output_tokens = Some 32_000 }
  else if starts_with "gpt-4o" then
    Some { openai_chat_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 16_384 }
  else if starts_with "gemini-3" || starts_with "gemini-2.5" then
    Some gemini_capabilities
  else if starts_with "qwen3" then
    Some { default_capabilities with
           max_context_tokens = Some 262_144;
           supports_tools = true;
           supports_tool_choice = true;
           supports_parallel_tool_calls = true;
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_native_streaming = true;
           supports_top_k = true;
           supports_min_p = true }
  else if starts_with "llama-4" || starts_with "llama4" then
    Some { default_capabilities with
           max_context_tokens = Some 1_000_000;
           supports_tools = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true }
  else if starts_with "deepseek-chat" || starts_with "deepseek-v3" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 8_000;
           supports_tools = true;
           supports_reasoning = true;
           supports_response_format_json = true;
           supports_native_streaming = true;
           supports_caching = true }
  else if starts_with "deepseek-reasoner" || starts_with "deepseek-r1" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 8_000;
           supports_tools = false;  (* R1 does NOT support tools *)
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_native_streaming = true;
           supports_caching = true }
  else if starts_with "mistral-large" then
    Some { default_capabilities with
           max_context_tokens = Some 260_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_parallel_tool_calls = true;
           supports_structured_output = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true;
           supports_caching = true }
  else if starts_with "mistral-small" then
    Some { default_capabilities with
           max_context_tokens = Some 256_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_parallel_tool_calls = true;
           supports_reasoning = true;
           supports_structured_output = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true;
           supports_caching = true }
  else if starts_with "command" then
    Some { default_capabilities with
           max_context_tokens = Some 256_000;
           max_output_tokens = Some 32_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_parallel_tool_calls = true;
           supports_structured_output = true;
           supports_native_streaming = true }
  else if starts_with "grok" then
    Some { default_capabilities with
           max_context_tokens = Some 2_000_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_parallel_tool_calls = true;
           supports_reasoning = true;
           supports_structured_output = true;
           supports_native_streaming = true;
           supports_caching = true }
  (* GLM flash/air variants: faster, no reasoning, smaller output.
     Must precede the broad glm-4.5/4.6/4.7/5 match below. *)
  else if starts_with "glm-4.7-flash" || starts_with "glm-4.5-flash"
          || starts_with "glm-4.5-air" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 16_384;
           supports_tools = true;
           supports_tool_choice = true;
           supports_response_format_json = true;
           supports_native_streaming = true }
  (* GLM 5-turbo: tool-calling optimized, fast, reasoning but no extended thinking *)
  else if starts_with "glm-5-turbo" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 16_384;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_response_format_json = true;
           supports_native_streaming = true }
  else if starts_with "glm-5v-turbo" then
    Some { default_capabilities with
           max_context_tokens = Some 200_000;
           max_output_tokens = Some 128_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_response_format_json = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true }
  else if starts_with "glm-ocr" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 16_384;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true }
  else if starts_with "glm-4.6v" || starts_with "glm-4.5v" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 32_768;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true }
  (* GLM full text models: reasoning, large context/output, but no vision. *)
  else if starts_with "glm-4.5" || starts_with "glm-4.6" || starts_with "glm-4.7"
          || starts_with "glm-5" then
    Some { default_capabilities with
           max_context_tokens = Some 200_000;
           max_output_tokens = Some 128_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_response_format_json = true;
           supports_native_streaming = true }
  else if starts_with "glm-4-flash" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 4_096;
           supports_tools = true;
           supports_native_streaming = true }
  else if starts_with "glm-4v" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 4_096;
           supports_tools = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
           supports_native_streaming = true }
  else if starts_with "glm-4" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 4_096;
           supports_tools = true;
           supports_tool_choice = true;
           supports_native_streaming = true }
  else
    None

(** Merge Discovery ctx_size into capabilities. *)
let with_context_size caps ~ctx_size =
  { caps with max_context_tokens = Some ctx_size }

[@@@coverage off]

let%test "for_model_id glm-4.5 has reasoning" =
  match for_model_id "glm-4.5" with
  | Some c -> c.supports_reasoning && c.max_context_tokens = Some 200_000
  | None -> false

let%test "for_model_id glm-4 no reasoning" =
  match for_model_id "glm-4-chat" with
  | Some c -> not c.supports_reasoning && c.max_context_tokens = Some 128_000
  | None -> false

let%test "for_model_id glm-4v has vision" =
  match for_model_id "glm-4v-flash" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false

let%test "for_model_id glm-4-flash basic" =
  match for_model_id "glm-4-flash" with
  | Some c -> c.supports_tools && c.max_output_tokens = Some 4_096
  | None -> false

let%test "for_model_id glm-5 is text only" =
  match for_model_id "glm-5" with
  | Some c -> c.supports_reasoning && not c.supports_image_input
  | None -> false

let%test "for_model_id glm-5v has vision" =
  match for_model_id "glm-5v-turbo" with
  | Some c -> c.supports_reasoning && c.supports_image_input
  | None -> false

let%test "for_model_id glm-4.6v stays vision-capable" =
  match for_model_id "glm-4.6v" with
  | Some c ->
      c.supports_reasoning
      && c.supports_image_input
      && c.max_output_tokens = Some 32_768
  | None -> false

let%test "for_model_id glm-4.5v stays vision-capable" =
  match for_model_id "glm-4.5v" with
  | Some c ->
      c.supports_reasoning
      && c.supports_image_input
      && c.max_output_tokens = Some 32_768
  | None -> false

let%test "for_model_id glm-4.7-flashx is flash (no reasoning, 16K output)" =
  match for_model_id "glm-4.7-flashx" with
  | Some c ->
    not c.supports_reasoning
    && c.max_output_tokens = Some 16_384
    && c.supports_tools
  | None -> false

let%test "for_model_id glm-4.7-flash is flash (not broad glm-4.7)" =
  match for_model_id "glm-4.7-flash" with
  | Some c ->
    not c.supports_reasoning
    && c.max_output_tokens = Some 16_384
  | None -> false

let%test "for_model_id glm-4.5-flash is flash (not broad glm-4.5)" =
  match for_model_id "glm-4.5-flash" with
  | Some c ->
    not c.supports_reasoning
    && c.max_output_tokens = Some 16_384
    && c.supports_tools
  | None -> false

let%test "for_model_id glm-5-turbo has reasoning but not extended thinking" =
  match for_model_id "glm-5-turbo" with
  | Some c ->
    c.supports_reasoning
    && not c.supports_extended_thinking
    && c.max_output_tokens = Some 16_384
  | None -> false

let%test "for_model_id glm-5.1 full model (reasoning + extended thinking)" =
  match for_model_id "glm-5.1" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_output_tokens = Some 128_000
  | None -> false
