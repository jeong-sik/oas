open Base
(** Policy_channel -- shared ref for lazy tool policy propagation.

    When a parent agent modifies its tool policy (e.g. confiscating a tool)
    after spawning children, children that share the same channel will pick
    up the new policy at their next turn boundary.

    The channel holds an accumulated [Tool_op.t] that children poll via
    {!poll}.  Reads are lock-free ([Atomic.get]); only writes use a
    compare-and-set loop.  This is safe under Eio's cooperative
    scheduling since CAS never contends in practice, but remains correct
    even if OCaml 5 preemptive domains are involved.

    @since 0.100.0 *)

type t

(** Create a fresh channel with no pending operations. *)
val create : unit -> t

(** Push a new operation.  The operation is composed (via {!Tool_op.compose})
    with any previously accumulated operations, and the version counter is
    incremented.  Multiple calls compose left-to-right. *)
val push : t -> Tool_op.t -> unit

(** Return the accumulated operation if any updates have been pushed,
    [None] otherwise.  This is a lock-free read. *)
val poll : t -> Tool_op.t option

(** Return the current version counter (starts at 0, incremented on each
    {!push}).  Useful for detecting whether anything changed since the last
    poll. *)
val version : t -> int
