(** Provider capabilities -- per-provider/model feature flags.

    Extracted from OAS Provider module. Used by both OAS (via Provider)
    and MASC (directly) to query what a provider supports.

    @since 0.42.0 *)

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

let default_capabilities = {
  supports_tools = false;
  supports_tool_choice = false;
  supports_reasoning = false;
  supports_response_format_json = false;
  supports_multimodal_inputs = false;
  supports_native_streaming = false;
  supports_top_k = false;
  supports_min_p = false;
}

let anthropic_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_reasoning = true;
  supports_multimodal_inputs = true;
  supports_native_streaming = true;
}

let openai_chat_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_response_format_json = true;
  supports_multimodal_inputs = true;
}

let qwen_openai_chat_capabilities = {
  openai_chat_capabilities with
  supports_reasoning = true;
  supports_top_k = true;
  supports_min_p = true;
}
