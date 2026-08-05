(** Tool definition and execution.

    @stability Stable
    @since 0.93.1 *)

type tool_handler = Yojson.Safe.t -> Types.tool_result
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

(** Explicit resources available at one tool execution occurrence.
    Context and invocation are orthogonal optional capabilities rather than
    mutually exclusive handler variants.

    @since 0.215.0 *)
module Execution_env : sig
  type t

  val create : ?context:Context.t -> ?invocation:Tool_contract.Invocation.t -> unit -> t
  val context : t -> Context.t option
  val invocation : t -> Tool_contract.Invocation.t option
end

type execution_env_tool_handler = Execution_env.t -> Yojson.Safe.t -> Types.tool_result

(** Immutable execution metadata. A terminal tool is serial by construction,
    so [Terminal + Concurrent] is not representable. *)
type descriptor

val ordinary_descriptor : Tool_contract.execution_mode -> descriptor
val terminal_descriptor : Tool_contract.failure_effect_disposition -> descriptor
val descriptor_execution_mode : descriptor -> Tool_contract.execution_mode
val descriptor_completion : descriptor -> Tool_contract.completion

type t =
  { schema : Types.tool_schema
  ; descriptor : descriptor option
  ; handler : execution_env_tool_handler
  }

(** [?input_schema] is the authoritative wire form sent to providers verbatim
    by {!schema_to_json}; omitting it derives the wire form from [~parameters].
    When supplied it must be the schema [~parameters] was derived from — see
    [Types.tool_schema.input_schema]. [Mcp_schema.tool_of_input_schema_result]
    derives both from one schema and is the preferred entry point. *)
val create
  :  ?descriptor:descriptor
  -> ?strict:bool
  -> ?input_schema:Yojson.Safe.t
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> tool_handler
  -> t

val create_with_context
  :  ?descriptor:descriptor
  -> ?strict:bool
  -> ?input_schema:Yojson.Safe.t
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> context_tool_handler
  -> t

(** Creates a raw JSON tool whose handler receives the explicit execution
    environment. The environment can contain both shared {!Context.t} and exact
    {!Invocation.t} occurrence metadata.

    @since 0.215.0 *)
val create_with_execution_env
  :  ?descriptor:descriptor
  -> ?strict:bool
  -> ?input_schema:Yojson.Safe.t
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> execution_env_tool_handler
  -> t

val execute
  :  ?context:Context.t
  -> ?invocation:Tool_contract.Invocation.t
  -> t
  -> Yojson.Safe.t
  -> Types.tool_result

val descriptor : t -> descriptor option

(** Exact declared execution mode, or [Serial] when no descriptor exists. *)
val execution_mode : t -> Tool_contract.execution_mode

(** Exact completion policy, or [Continue_after_success] when no descriptor
    exists. *)
val completion : t -> Tool_contract.completion

val descriptor_to_yojson : descriptor option -> Yojson.Safe.t

(** Provider-facing tool definition. ["input_schema"] is the authoritative
    schema verbatim when the tool carries one, and
    [Types.params_to_input_schema] of the parameters otherwise. *)
val schema_to_json : t -> Yojson.Safe.t

(** Wrap a tool to inject default arguments when not provided.
    Defaults are merged into the input JSON before the handler runs. *)
val with_defaults : (string * Yojson.Safe.t) list -> t -> t
