(** Tool input validation — deterministic schema checking with type coercion.

    Validates tool call arguments against declared [Types.tool_param] schemas
    before execution. Performs type coercion (e.g. ["42"] → [42]) to recover
    from common LLM formatting errors.

    Inspired by Samchon's Function Calling Harness:
    type schema + deterministic validator + structured feedback = harness.

    @since 0.100.0 *)

(** A single field-level validation error. *)
type field_error = {
  path: string;      (** JSON path, e.g. ["/room"], ["/interval_seconds"] *)
  expected: string;  (** Expected type or constraint, e.g. ["integer"], ["required"] *)
  actual: string;    (** What was received, e.g. ["string(\"sixty\")"], ["missing"] *)
}

(** Validation outcome: either coerced input or a list of errors. *)
type validation_result =
  | Valid of Yojson.Safe.t
  | Invalid of field_error list

(** Validate [input] against the parameter schema of [tool].
    Performs type coercion before failing:
    - string ["42"] → integer [42]
    - string ["true"]/["false"] → boolean
    - string ["3.14"] → number [3.14]
    Returns [Valid coerced_input] or [Invalid errors]. *)
val validate : Types.tool_schema -> Yojson.Safe.t -> validation_result

(** Format field errors as a structured, LLM-readable feedback string.
    Designed for [ToolResult] with [is_error=true]. *)
val format_errors : tool_name:string -> field_error list -> string

(** Samchon-style inline error feedback: shows the LLM's original JSON
    alongside field-level error annotations. More surgical than [format_errors]
    because the LLM sees its own output with precise error markers.

    Use this in heal_tool_call retry loops for higher recovery rates. *)
val format_errors_inline :
  tool_name:string -> args:Yojson.Safe.t -> field_error list -> string
