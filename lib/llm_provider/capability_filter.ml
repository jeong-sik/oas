(** Capability predicates for provider selection.

    @since 0.69.0 *)

let requires_tools (c : Capabilities.capabilities) = c.supports_tools
let requires_streaming (c : Capabilities.capabilities) = c.supports_native_streaming
let requires_reasoning (c : Capabilities.capabilities) = c.supports_reasoning
let requires_multimodal (c : Capabilities.capabilities) = c.supports_multimodal_inputs
let requires_json_format (c : Capabilities.capabilities) = c.supports_response_format_json

let requires_all preds caps = List.for_all (fun p -> p caps) preds
let requires_any preds caps = List.exists (fun p -> p caps) preds
