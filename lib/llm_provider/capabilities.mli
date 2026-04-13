(** Provider capabilities -- per-provider/model feature flags and limits.

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split

    @stability Internal
    @since 0.93.1 *)

type capabilities = {
  (* Numeric limits *)
  max_context_tokens: int option;
  max_output_tokens: int option;
  (* Tool use *)
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_parallel_tool_calls: bool;
  (* Thinking / reasoning *)
  supports_reasoning: bool;
  supports_extended_thinking: bool;
  supports_reasoning_budget: bool;
  (* Output format *)
  supports_response_format_json: bool;
  supports_structured_output: bool;
  (* Input modalities *)
  supports_multimodal_inputs: bool;
  supports_image_input: bool;
  supports_audio_input: bool;
  supports_video_input: bool;
  (* Protocol *)
  supports_native_streaming: bool;
  supports_system_prompt: bool;
  supports_caching: bool;
  (* Sampling parameters *)
  supports_top_k: bool;
  supports_min_p: bool;
  (* Advanced modalities *)
  supports_computer_use: bool;
  supports_code_execution: bool;
  (* Provider identity *)
  is_ollama: bool;
}

val default_capabilities : capabilities
val anthropic_capabilities : capabilities
val openai_chat_capabilities : capabilities
val openai_chat_extended_capabilities : capabilities
val gemini_capabilities : capabilities
val ollama_capabilities : capabilities
val glm_capabilities : capabilities
val claude_code_capabilities : capabilities
val gemini_cli_capabilities : capabilities
val codex_cli_capabilities : capabilities

(** Lookup capabilities for a known model_id.
    Returns [None] if the model is not in the built-in table. *)
val for_model_id : string -> capabilities option

(** Merge Discovery ctx_size into existing capabilities. *)
val with_context_size : capabilities -> ctx_size:int -> capabilities
