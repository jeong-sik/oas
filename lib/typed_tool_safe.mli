(** Phantom-typed permission enforcement for typed tools.

    Wraps {!Typed_tool.t} with a phantom type parameter ['perm] that
    encodes the permission level at compile time. A [read_only] tool
    cannot be passed to {!execute_with_approval}, and a [destructive]
    tool cannot be passed to {!execute_read_only}.

    This is a compile-time-only layer — no runtime cost. The phantom
    parameter is erased by {!to_typed_tool}.

    Inspired by MASC's {!Typed_state} GADT PoC (phantom task status).

    @stability Evolving
    @since 0.120.0 *)

(** {1 Phantom permission types} *)

type read_only
type write
type destructive

(** {1 Safe tool type} *)

(** A typed tool tagged with its permission level at the type level.
    The ['perm] parameter is phantom — it exists only for the compiler. *)
type ('perm, 'input, 'output) t

(** {1 Construction} *)

(** Wrap a typed tool with read-only permission.
    The descriptor must have [permission = Some ReadOnly] or [None]. *)
val read_only :
  ('input, 'output) Typed_tool.t -> (read_only, 'input, 'output) t

(** Wrap a typed tool with write permission. *)
val write :
  ('input, 'output) Typed_tool.t -> (write, 'input, 'output) t

(** Wrap a typed tool with destructive permission. *)
val destructive :
  ('input, 'output) Typed_tool.t -> (destructive, 'input, 'output) t

(** {1 Permission-gated execution} *)

(** Execute a read-only tool. No approval needed.
    Passing a [write] or [destructive] tool here is a compile error. *)
val execute_read_only :
  ?context:Context.t ->
  (read_only, 'input, 'output) t ->
  Yojson.Safe.t ->
  Types.tool_result

(** Execute a write tool with mandatory approval callback.
    The callback receives the tool name and parsed input description.
    Passing a [read_only] tool here is a compile error —
    use {!execute_read_only} instead.

    @param approve Called before execution. Returns [true] to proceed,
                   [false] to reject with "approval denied" error. *)
val execute_write :
  ?context:Context.t ->
  approve:(tool_name:string -> input_desc:string -> bool) ->
  (write, 'input, 'output) t ->
  Yojson.Safe.t ->
  Types.tool_result

(** Execute a destructive tool with mandatory approval callback.
    Same as {!execute_write} but semantically distinct for auditing. *)
val execute_destructive :
  ?context:Context.t ->
  approve:(tool_name:string -> input_desc:string -> bool) ->
  (destructive, 'input, 'output) t ->
  Yojson.Safe.t ->
  Types.tool_result

(** {1 Erasure} *)

(** Erase the permission phantom for interop with untyped dispatch. *)
val to_typed_tool : (_, 'input, 'output) t -> ('input, 'output) Typed_tool.t

(** Erase both permission and input/output types. *)
val to_untyped : (_, _, _) t -> Tool.t

(** {1 Introspection} *)

val name : (_, _, _) t -> string
val permission_name : ('perm, _, _) t -> string
