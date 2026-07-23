(** Tool definition and execution.

    @stability Stable
    @since 0.93.1 *)

type tool_handler = Yojson.Safe.t -> Types.tool_result
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

(** Declared execution ordering for a tool. *)
type execution_mode =
  | Concurrent
  | Serial
[@@deriving show]

val execution_mode_to_yojson : execution_mode -> Yojson.Safe.t
val execution_mode_of_yojson : Yojson.Safe.t -> (execution_mode, string) result

(** Whether a terminal failure is proven to have occurred before any external
    effect, after an external effect, or at an unknown effect boundary. *)
type failure_effect_disposition =
  | Proven_pre_effect
  | Proven_post_effect
  | Effect_outcome_unknown
[@@deriving show]

(** Whether a successful invocation permits another provider turn. *)
type completion =
  | Continue_after_success
  | Terminal_after_success of failure_effect_disposition
[@@deriving show]

val completion_to_yojson : completion -> Yojson.Safe.t
val completion_of_yojson : Yojson.Safe.t -> (completion, string) result

(** Exact scheduler placement for one tool occurrence.

    @since 0.216.0 *)
type schedule =
  { planned_index : int
  ; batch_index : int (** Zero-based execution-batch ordinal within the turn. *)
  ; batch_size : int (** Number of invocations in that execution batch. *)
  ; execution_mode : execution_mode
  }

(** Exact occurrence metadata for correlation and observability only.
    It does not authorize tool execution. [turn] is the zero-based provider
    turn shared by BeforeTurn, AfterTurn, TurnCompleted, and every tool
    occurrence produced by that response. Together, [turn] and
    [schedule.planned_index] scope provider [tool_use_id] values that may be
    blank or repeated. The embedding runtime owns any broader agent/run
    identity.

    @since 0.215.0
    @since 0.216.0 Owns the canonical [schedule]. *)
module Invocation : sig
  type t

  val create
    :  tool_use_id:string
    -> turn:int
    -> schedule:schedule
    -> completion:completion
    -> t

  val tool_use_id : t -> string
  val turn : t -> int
  val schedule : t -> schedule
  val completion : t -> completion
  val planned_index : t -> int
end

(** Explicit resources available at one tool execution occurrence.
    Context and invocation are orthogonal optional capabilities, not mutually
    exclusive handler variants. Future execution metadata belongs in this
    record-shaped boundary rather than in additional [handler_kind]
    constructors.

    @since 0.215.0 *)
module Execution_env : sig
  type t

  val create : ?context:Context.t -> ?invocation:Invocation.t -> unit -> t
  val context : t -> Context.t option
  val invocation : t -> Invocation.t option
end

type execution_env_tool_handler = Execution_env.t -> Yojson.Safe.t -> Types.tool_result

(** Immutable execution metadata. A terminal tool is serial by construction,
    so [Terminal + Concurrent] is not representable. *)
type descriptor

val ordinary_descriptor : execution_mode -> descriptor
val terminal_descriptor : failure_effect_disposition -> descriptor
val descriptor_execution_mode : descriptor -> execution_mode
val descriptor_completion : descriptor -> completion

type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler
  | WithExecutionEnv of execution_env_tool_handler

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

(** Creates a raw JSON tool whose handler receives the explicit execution
    environment. The environment can contain both shared {!Context.t} and exact
    {!Invocation.t} occurrence metadata.

    @since 0.215.0 *)
val create_with_execution_env
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> execution_env_tool_handler
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

(** Exact completion policy, or [Continue_after_success] when no descriptor
    exists. *)
val completion : t -> completion

val descriptor_to_yojson : descriptor option -> Yojson.Safe.t
val schema_to_json : t -> Yojson.Safe.t

(** Wrap a tool to inject default arguments when not provided.
    Defaults are merged into the input JSON before the handler runs. *)
val with_defaults : (string * Yojson.Safe.t) list -> t -> t
