(** Capability predicates for provider selection.

    @since 0.69.0
    @since 0.72.0 — added limit checks, thinking, structured output *)

(* ── Feature predicates ──────────────────────────────── *)

let requires_tools (c : Capabilities.capabilities) = c.supports_tools
let requires_streaming (c : Capabilities.capabilities) = c.supports_native_streaming
let requires_reasoning (c : Capabilities.capabilities) = c.supports_reasoning
let requires_multimodal (c : Capabilities.capabilities) = c.supports_multimodal_inputs
let requires_json_format (c : Capabilities.capabilities) = c.supports_response_format_json

let requires_parallel_tools (c : Capabilities.capabilities) = c.supports_parallel_tool_calls
let requires_thinking (c : Capabilities.capabilities) = c.supports_extended_thinking
let requires_structured_output (c : Capabilities.capabilities) = c.supports_structured_output
let requires_caching (c : Capabilities.capabilities) = c.supports_caching
let requires_vision (c : Capabilities.capabilities) = c.supports_image_input
let requires_computer_use (c : Capabilities.capabilities) = c.supports_computer_use
let requires_system_prompt (c : Capabilities.capabilities) = c.supports_system_prompt

(* ── Limit checks ────────────────────────────────────── *)

let fits_context ~tokens (c : Capabilities.capabilities) =
  match c.max_context_tokens with
  | None -> true  (* unknown = assume fits *)
  | Some max -> tokens <= max

let fits_output ~tokens (c : Capabilities.capabilities) =
  match c.max_output_tokens with
  | None -> true
  | Some max -> tokens <= max

(* ── Combinators ─────────────────────────────────────── *)

let requires_all preds caps = List.for_all (fun p -> p caps) preds
let requires_any preds caps = List.exists (fun p -> p caps) preds
