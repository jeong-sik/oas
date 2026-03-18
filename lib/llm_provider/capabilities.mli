(** Provider capabilities -- per-provider/model feature flags. *)

type capabilities = {
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_reasoning: bool;
  supports_response_format_json: bool;
  supports_multimodal_inputs: bool;
  supports_native_streaming: bool;
  supports_top_k: bool;
  supports_min_p: bool;
}

val default_capabilities : capabilities
val anthropic_capabilities : capabilities
val openai_chat_capabilities : capabilities
val openai_chat_extended_capabilities : capabilities
