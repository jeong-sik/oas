(** Typed tool — compile-time enforced schema/handler type agreement.

    Extends the ['a schema] pattern from {!Structured} to tool definitions.
    The type parameters ['input] and ['output] connect the JSON parse function
    to the handler: a mismatch is a compile error, not a runtime error.

    Backward compatible: {!to_untyped} erases type parameters for registration
    in existing {!Tool_set} / {!Tool.t} based dispatch.

    Inspired by Typia's "type is the schema" and AutoBE's typed AST IR.

    @stability Evolving
    @since 0.120.0 *)

(** {1 Core type} *)

(** A tool whose input parsing and handler share the same ['input] type.
    The compiler enforces that [parse] produces what [handler] consumes. *)
type ('input, 'output) t

(** {1 Construction} *)

(** Create a typed tool.

    The key invariant: [parse] returns ['input] and [handler] accepts ['input].
    If these types disagree, the program does not compile.

    @param params Schema-level parameter declarations (for LLM tool_use).
    @param parse  Deserialize raw JSON into typed input. Return [Error] for
                  structurally invalid JSON. Type coercion should NOT be done
                  here — that is the correction pipeline's job.
    @param handler Pure business logic. Receives parsed, validated input.
                   Return [Error] for domain-level rejections (e.g. empty message).
    @param encode  Serialize output to JSON for tool_result content.
    @param descriptor Optional safety metadata (permission, concurrency, shell). *)
val create :
  name:string ->
  description:string ->
  params:Types.tool_param list ->
  parse:(Yojson.Safe.t -> ('input, string) result) ->
  handler:('input -> ('output, string) result) ->
  encode:('output -> Yojson.Safe.t) ->
  ?descriptor:Tool.descriptor ->
  unit ->
  ('input, 'output) t

(** Create a context-aware typed tool.

    Same as {!create} but the handler receives a {!Context.t} for accessing
    shared agent state (e.g. retry counts, custom keys). *)
val create_with_context :
  name:string ->
  description:string ->
  params:Types.tool_param list ->
  parse:(Yojson.Safe.t -> ('input, string) result) ->
  handler:(Context.t -> 'input -> ('output, string) result) ->
  encode:('output -> Yojson.Safe.t) ->
  ?descriptor:Tool.descriptor ->
  unit ->
  ('input, 'output) t

(** {1 Execution} *)

(** Execute the typed tool: parse -> handler -> encode.

    Returns [Ok tool_output] on success, [Error tool_error] on parse or
    handler failure. Parse errors are marked [recoverable = true] (the LLM
    can retry with corrected input). Handler errors use [recoverable = false]
    by default (domain rejection). *)
val execute : ?context:Context.t -> ('input, 'output) t -> Yojson.Safe.t -> Types.tool_result

(** Execute with access to the intermediate parsed input.

    Useful for logging, metrics, or correction pipeline integration. *)
val execute_parsed :
  ?context:Context.t ->
  ('input, 'output) t ->
  Yojson.Safe.t ->
  ('input * ('output, string) result, string) result

(** {1 Backward compatibility} *)

(** Erase type parameters for registration in {!Tool_set} or {!Tool.t}-based
    dispatch. The returned {!Tool.t} composes parse -> handler -> encode into
    a single [Yojson.Safe.t -> Types.tool_result] function.

    This is the migration bridge: convert one tool at a time without
    changing the dispatch infrastructure. *)
val to_untyped : (_, _) t -> Tool.t

(** {1 Introspection} *)

(** Extract the tool schema for validation hooks and LLM tool_use. *)
val schema : (_, _) t -> Types.tool_schema

(** Extract the tool name. *)
val name : (_, _) t -> string

(** Extract the optional safety descriptor. *)
val descriptor : (_, _) t -> Tool.descriptor option
