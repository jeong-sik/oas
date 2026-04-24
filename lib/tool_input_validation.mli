(** Tool input validation — deterministic schema checking with type coercion.

    Validates tool call arguments against declared [Types.tool_param] schemas
    before execution. Performs type coercion (e.g. ["42"] → [42]) to recover
    from common LLM formatting errors.

    Inspired by Samchon's Function Calling Harness:
    type schema + deterministic validator + structured feedback = harness.

    @since 0.100.0 *)

(** A single field-level validation error. *)
type field_error = {
  path: string;      (** JSON path, e.g. ["/workspace"], ["/interval_seconds"] *)
  expected: string;  (** Expected type or constraint, e.g. ["integer"], ["required"] *)
  actual: string;    (** What was received, e.g. ["string(\"sixty\")"], [{!missing_actual}] *)
}

(** Sentinel value for [field_error.actual] when the field is absent. *)
val missing_actual : string

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

(** {1 Low-level helpers (for Correction_pipeline, Tool_schema_gen)} *)

(** Human-readable description of a JSON value for error messages.
    E.g. [null], [integer(42)], [string("hello")].
    @since 0.120.0 *)
val describe_json_value : Yojson.Safe.t -> string

(** Try to coerce a JSON value to the expected param_type.
    Returns [Some coerced] on success, [None] if not coercible.
    @since 0.120.0 *)
val try_coerce : Types.param_type -> Yojson.Safe.t -> Yojson.Safe.t option

(** Check if a JSON value matches the expected param_type.
    @since 0.120.0 *)
val matches_type : Types.param_type -> Yojson.Safe.t -> bool
