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

(** Build a tool from the parameter view. {!schema_to_json} then derives the
    wire form with [Types.params_to_input_schema], which keeps only type,
    description and required. Use {!of_input_schema_result} when the caller has
    a JSON Schema — that is the only path an authoritative schema can take, and
    it cannot be combined with [~parameters]. *)
val create
  :  ?descriptor:descriptor
  -> ?strict:bool
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> tool_handler
  -> t

(** Build a tool from one authoritative JSON Schema, sent to providers verbatim
    by {!schema_to_json}. The parameter view used for validation is derived
    from that same schema by [Types.tool_schema_of_input_schema], so the two
    cannot disagree. Fails when the value is not a tool argument schema or when
    a property cannot be projected onto a [Types.tool_param]. *)
val of_input_schema_result
  :  ?descriptor:descriptor
  -> ?strict:bool
  -> name:string
  -> description:string
  -> input_schema:Yojson.Safe.t
  -> tool_handler
  -> (t, string) result

val create_with_context
  :  ?descriptor:descriptor
  -> ?strict:bool
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
