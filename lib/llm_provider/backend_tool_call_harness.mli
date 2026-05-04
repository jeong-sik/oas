(** Tool-calling verification harness for LLM provider backends.

    Validates that parsed API responses contain well-formed tool calls,
    correct stop reasons, and no silently dropped content blocks.
    When tool schemas are provided, validates arguments against their
    JSON Schema declarations.

    Pure functions — no Eio, no network.

    @since 0.187.7
    @since 0.185.0 — schema-aware argument validation (P0) *)

open Types

(** A single schema violation detected in tool call arguments. *)
type schema_violation =
  { path : string
  ; expected : string
  ; actual : string
  }

type tool_call_check =
  { name : string
  ; arguments_valid : bool
  ; violations : schema_violation list
  }

type validation_result =
  { tool_calls_found : tool_call_check list
  ; stop_reason_correct : bool
  ; all_tools_declared : bool
  ; dropped_content_blocks : int
  }

val empty_result : validation_result

(** {1 Schema extraction} *)

(** Extract parameter schema from a tool definition JSON.
    Handles "input_schema" (Anthropic), "parameters" (OpenAI),
    and "function.parameters" (OpenAI nested). *)
val extract_tool_schema : Yojson.Safe.t -> Yojson.Safe.t option

(** Build a name→schema map from a list of tool definition JSONs. *)
val build_schema_map : Yojson.Safe.t list -> (string * Yojson.Safe.t) list

(** {1 Validation} *)

(** Validate a parsed {!api_response} against declared tool names. *)
val validate_response : declared_tools:string list -> api_response -> validation_result

(** Schema-aware response validation.
    Also validates each tool call's arguments against the corresponding
    JSON Schema from [tool_schemas].
    @since 0.185.0 *)
val validate_response_with_schemas
  :  declared_tools:string list
  -> tool_schemas:(string * Yojson.Safe.t) list
  -> api_response
  -> validation_result

(** {1 Structured feedback} *)

(** Format schema violations into a structured feedback message suitable
    for re-injection into the LLM conversation as a tool result error.
    @since 0.185.0 *)
val format_violations_feedback : tool_call_check -> string

(** {1 Per-backend convenience} *)

val validate_anthropic_response
  :  declared_tools:string list
  -> Yojson.Safe.t
  -> validation_result

val validate_gemini_response
  :  declared_tools:string list
  -> Yojson.Safe.t
  -> validation_result

val validate_openai_response
  :  declared_tools:string list
  -> Yojson.Safe.t
  -> validation_result
