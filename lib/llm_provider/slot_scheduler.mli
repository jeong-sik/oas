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
