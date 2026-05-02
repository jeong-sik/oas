(** Tool-calling verification harness for LLM provider backends.

    Validates that parsed API responses contain well-formed tool calls,
    correct stop reasons, and no silently dropped content blocks.
    Pure functions — no Eio, no network.

    @since 0.187.7 *)

open Types

type tool_call_check =
  { name : string
  ; arguments_valid : bool
  }

type validation_result =
  { tool_calls_found : tool_call_check list
  ; stop_reason_correct : bool
  ; all_tools_declared : bool
  ; dropped_content_blocks : int
  }

val empty_result : validation_result

val validate_response :
  declared_tools:string list -> api_response -> validation_result

val validate_anthropic_response :
  declared_tools:string list -> Yojson.Safe.t -> validation_result

val validate_gemini_response :
  declared_tools:string list -> Yojson.Safe.t -> validation_result

val validate_openai_response :
  declared_tools:string list -> Yojson.Safe.t -> validation_result
