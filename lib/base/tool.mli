(** Tool definition and execution.

    @stability Stable
    @since 0.93.1 *)

type tool_handler = Yojson.Safe.t -> Types.tool_result
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

(** Exact occurrence metadata for correlation and observability only.
    It does not authorize tool execution. [turn] and [planned_index] scope
    provider [tool_use_id] values that may be blank or repeated. The embedding
    runtime owns any broader agent/run identity.

    @since 0.215.0 *)
module Invocation : sig
  type t

  val create : tool_use_id:string -> turn:int -> planned_index:int -> t
  val tool_use_id : t -> string
  val turn : t -> int
  val planned_index : t -> int
end

type invocation_tool_handler = Invocation.t -> Yojson.Safe.t -> Types.tool_result

type execution_mode =
  | Concurrent
  | Serial
[@@deriving show]

val execution_mode_to_yojson : execution_mode -> Yojson.Safe.t
val execution_mode_of_yojson : Yojson.Safe.t -> (execution_mode, string) result

type descriptor = { execution_mode : execution_mode }

type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler
  | WithInvocation of invocation_tool_handler

type t =
  { schema : Types.tool_schema
  ; descriptor : descriptor option
  ; handler : handler_kind
  }

val create
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> tool_handler
  -> t

val create_with_context
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> context_tool_handler
  -> t

(** Creates a raw JSON tool whose handler receives exact invocation occurrence
    metadata. Typed-tool projection is a separate API surface.

    @since 0.215.0 *)
val create_with_invocation
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> invocation_tool_handler
  -> t

val execute
  :  ?context:Context.t
  -> ?invocation:Invocation.t
  -> t
  -> Yojson.Safe.t
  -> Types.tool_result

val descriptor : t -> descriptor option

(** Exact declared execution mode, or [Serial] when no descriptor exists. *)
val execution_mode : t -> execution_mode

val descriptor_to_yojson : descriptor option -> Yojson.Safe.t
val schema_to_json : t -> Yojson.Safe.t

(** Wrap a tool to inject default arguments when not provided.
    Defaults are merged into the input JSON before the handler runs. *)
val with_defaults : (string * Yojson.Safe.t) list -> t -> t
