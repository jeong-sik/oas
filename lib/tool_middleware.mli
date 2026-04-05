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

(** {1 Self-Healing Retry Loop}

    Deterministic wrapper around non-deterministic LLM output.
    Validates tool call args, and on failure re-prompts the LLM
    with error feedback until the call is valid or retries exhaust.

    @since 0.100.7 *)

(** Result of a healing attempt. *)
type healing_result = {
  value: Yojson.Safe.t;
  attempts: int;  (** Total attempts (1 = first try succeeded) *)
  healed: bool;   (** Whether any retry was needed *)
  final_tool_use_id: string;  (** ToolUse ID to use for the final ToolResult *)
}

(** Reason the healing loop terminated without success. *)
type healing_failure =
  | Exhausted of {
      attempts: int;
      limit: int;
      last_error: string;
    }
  | Llm_error of Error.sdk_error

(** LLM re-prompt callback.

    Given conversation history including error feedback,
    return a new response.  The callback owns API dispatch. *)
type llm_callback =
  Types.message list -> (Types.api_response, Error.sdk_error) result

val heal_tool_call :
  tool_name:string ->
  schema:Types.tool_schema ->
  tool_use_id:string ->
  args:Yojson.Safe.t ->
  prior_messages:Types.message list ->
  llm:llm_callback ->
  ?max_retries:int ->
  ?on_retry:(attempt:int -> error:string -> unit) ->
  unit ->
  (healing_result, healing_failure) result
(** Run the self-healing validation loop.

    1. Validate [args] against [schema] via {!validate_and_coerce}.
    2. If [Pass] or [Proceed]: return immediately.
    3. If [Reject]: construct error feedback, call [llm], extract the
       corrected tool call, and repeat from (1).

    Negative [max_retries] values are clamped to [0].

    @param max_retries Maximum re-prompts (default 3).
    @param on_retry Observability callback, invoked before each retry.
    @param tool_use_id ID of the original tool_use block.
    @param prior_messages Conversation context preceding the tool call. *)
