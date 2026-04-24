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
let emits_usage_tokens (c : Capabilities.capabilities) = c.emits_usage_tokens

(* ── Limit checks ────────────────────────────────────── *)

type fit_result = Fits | Does_not_fit | Unknown_limit

let check_context ~tokens (c : Capabilities.capabilities) =
  match c.max_context_tokens with
  | None -> Unknown_limit
  | Some max -> if tokens <= max then Fits else Does_not_fit

let check_output ~tokens (c : Capabilities.capabilities) =
  match c.max_output_tokens with
  | None -> Unknown_limit
  | Some max -> if tokens <= max then Fits else Does_not_fit

let fits_context ~tokens c =
  match check_context ~tokens c with
  | Fits -> true
  | Does_not_fit | Unknown_limit -> false

let fits_output ~tokens c =
  match check_output ~tokens c with
  | Fits -> true
  | Does_not_fit | Unknown_limit -> false

(* ── Combinators ─────────────────────────────────────── *)

let requires_all preds caps = List.for_all (fun p -> p caps) preds
let requires_any preds caps = List.exists (fun p -> p caps) preds
