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
  supports_multimodal_inputs = true;
  supports_image_input = true;
  supports_native_streaming = true;
  supports_caching = true;
  supports_computer_use = true;
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

let ollama_capabilities = {
  openai_chat_extended_capabilities with
  supports_tool_choice = true;  (* Ollama 0.19+: supports tool_choice in OpenAI compat endpoint *)
  is_ollama = true;
}

let glm_capabilities = {
  default_capabilities with
  max_context_tokens = Some 200_000;
  max_output_tokens = Some 128_000;
  supports_tools = true;
  supports_tool_choice = true;
  supports_reasoning = true;
  supports_extended_thinking = true;
  supports_response_format_json = true;
  supports_structured_output = true;
  supports_multimodal_inputs = true;
  supports_image_input = true;
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
}

let claude_code_capabilities = {
  anthropic_capabilities with
  max_context_tokens = Some 1_000_000;  (* 1M context via Claude Code *)
  max_output_tokens = Some 64_000;
  supports_computer_use = true;
  supports_code_execution = true;
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
           supports_structured_output = true;
           supports_response_format_json = true;
           supports_native_streaming = true }
  (* GLM 5-turbo: tool-calling optimized, fast, no extended thinking *)
  else if starts_with "glm-5-turbo" then
    Some { default_capabilities with
           max_context_tokens = Some 128_000;
           max_output_tokens = Some 16_384;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_structured_output = true;
           supports_response_format_json = true;
           supports_native_streaming = true }
  (* GLM full models: reasoning, large context/output *)
  else if starts_with "glm-4.5" || starts_with "glm-4.6" || starts_with "glm-4.7"
          || starts_with "glm-5" then
    Some { default_capabilities with
           max_context_tokens = Some 200_000;
           max_output_tokens = Some 128_000;
           supports_tools = true;
           supports_tool_choice = true;
           supports_reasoning = true;
           supports_extended_thinking = true;
           supports_structured_output = true;
           supports_response_format_json = true;
           supports_multimodal_inputs = true;
           supports_image_input = true;
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

let%test "for_model_id glm-5 advanced" =
  match for_model_id "glm-5" with
  | Some c -> c.supports_reasoning && c.supports_image_input
  | None -> false
