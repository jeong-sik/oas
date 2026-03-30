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
