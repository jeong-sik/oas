(** Priority-aware slot scheduler for LLM requests.

    Replaces FIFO semaphore-based throttling with a priority queue.
    Higher priority requests (Interactive) are granted slots before
    lower priority requests (Background) when capacity is constrained.

    Cancel-safe: if a waiting fiber is cancelled, the slot is not leaked.

    @since 0.96.0 *)

type t

val create : max_slots:int -> t
(** Create a scheduler with [max_slots] concurrent permits.
    @raise Invalid_argument if [max_slots < 1]. *)

val with_permit : priority:Request_priority.t -> t -> (unit -> 'a) -> 'a
(** Run [f] with a permit at the given priority.
    If all slots are in use, the request is queued by priority.
    Higher priority requests are dequeued first.
    Raises the original exception if [f] fails (permit is released). *)

val available : t -> int
(** Number of unused slots. *)

val in_use : t -> int
(** Number of currently active slots. *)

val queue_length : t -> int
(** Number of fibers waiting for a slot. *)

(** {2 Capacity Query} *)

(** Point-in-time snapshot of scheduler state.
    All counts reflect this OAS process only — other clients
    sharing the same server are not visible. *)
type snapshot = {
  max_slots : int;
  active : int;
  available : int;
  queue_length : int;
}

val snapshot : t -> snapshot
(** Non-blocking point-in-time capacity snapshot. *)

(** {2 Non-blocking Acquisition} *)

val try_with_permit : priority:Request_priority.t -> t -> (unit -> 'a) -> 'a option
(** Run [f] if a slot is immediately available, returning [Some result].
    Returns [None] without blocking if all slots are in use.
    The slot is released automatically when [f] returns or raises. *)

(** {2 Explicit Handle API — Turn-Level Slot Yielding}

    Supports the OpenClaw "Agent exists != LLM slot held" pattern.
    Agents acquire a permit, yield it during tool execution (releasing
    the slot for other agents), then resume before the next LLM turn.

    Lifecycle: [acquire_permit] -> [yield_permit] -> [resume_permit] -> [release_permit]

    @since 0.100.0 *)

type permit
(** Opaque handle representing a held slot permit. *)

val acquire_permit : priority:Request_priority.t -> t -> permit
(** Acquire a slot at the given priority. Blocks if all slots are in use.
    The caller is responsible for calling [release_permit] when done. *)

val yield_permit : t -> permit -> unit
(** Yield a held permit. Releases the slot so other agents can use it.
    The permit transitions to a yielded state. Call [resume_permit]
    to re-acquire before the next LLM turn. *)

val resume_permit : t -> permit -> unit
(** Re-acquire a previously yielded permit at [Resume] priority
    (higher than Interactive) to prevent starvation of tool-heavy agents.
    Blocks until a slot is available. *)

val release_permit : t -> permit -> unit
(** Permanently release a permit. Must be called exactly once per
    [acquire_permit], whether the permit is held or yielded. *)

val permit_is_held : permit -> bool
(** [true] if the permit currently holds a slot. *)
