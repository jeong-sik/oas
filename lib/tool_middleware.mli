(** Tool call middleware — reusable validation, coercion, and dispatch primitives.

    Consumers (MCP servers, custom agent loops) use these to build their own
    tool dispatch pipelines without embedding [Agent.run].

    @since 0.101.0 *)

(** {1 Pre-hook action type}

    General-purpose middleware control flow for tool call pipelines.
    Consumers define pre-hooks as [name -> args -> pre_hook_action]. *)

type pre_hook_action =
  | Pass
      (** This hook has no opinion — continue to next hook or handler. *)
  | Proceed of Yojson.Safe.t
      (** Replace args (e.g. after type coercion) and continue. *)
  | Reject of { is_error: bool; message: string }
      (** Short-circuit with an error result.  The handler is never called. *)

(** {1 Validation convenience} *)

val validate_and_coerce :
  tool_name:string ->
  schema:Types.tool_schema ->
  Yojson.Safe.t ->
  pre_hook_action
(** Validate [args] against [schema], returning a {!pre_hook_action}.

    - If the schema has no parameters, returns [Pass].
    - If validation succeeds with coercion, returns [Proceed coerced_args].
    - If validation succeeds without coercion, returns [Pass].
    - If validation fails, returns [Reject] with formatted error message.

    Delegates to {!Tool_input_validation.validate} and
    {!Tool_input_validation.format_errors}. *)

(** {1 Schema conversion}

    Convert JSON Schema objects to OAS typed parameter lists.
    Delegates to {!Mcp_schema.json_schema_to_params} for consumers
    that work with raw JSON schemas (e.g. MCP tool definitions). *)

val tool_schema_of_json :
  name:string ->
  ?description:string ->
  Yojson.Safe.t ->
  Types.tool_schema
(** Build a [Types.tool_schema] from a name and raw JSON Schema.
    Description defaults to [""].
    Extracts parameters via {!Mcp_schema.json_schema_to_params}. *)

(** {1 Hook factory}

    Build a pre-hook function from a schema lookup.
    Returns [Pass] for unknown tools (permissive by default). *)

val make_validation_hook :
  lookup:(string -> Types.tool_schema option) ->
  (name:string -> args:Yojson.Safe.t -> pre_hook_action)
(** Create a validation pre-hook closure.

    The [lookup] function resolves tool names to schemas.
    Unknown tools (when [lookup] returns [None]) get [Pass]. *)
