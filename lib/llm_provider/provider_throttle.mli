(** Provider-level concurrency throttle with priority-aware scheduling.

    Limits concurrent LLM requests per provider to avoid overwhelming
    backends with limited capacity (e.g. llama-server with N slots).
    When slots are full, requests are queued by priority.

    @since 0.84.0

    @stability Internal
    @since 0.93.1 *)

type t

val create : max_concurrent:int -> provider_name:string -> t
(** Create a throttle. [max_concurrent] must be >= 1.
    @raise Invalid_argument if [max_concurrent] < 1 *)

val with_permit_priority : priority:Request_priority.t -> t -> (unit -> 'a) -> 'a
(** Run [f] with a permit at the given [priority].
    If all permits are in use, the request is queued by priority.
    Higher priority requests are dequeued first.
    @since 0.96.0 *)

val with_permit : t -> (unit -> 'a) -> 'a
(** Run [f] with a permit at [Background] priority.
    Kept for backward compatibility with pre-scheduling callers. *)

val with_permit_timeout :
  _ Eio.Time.clock -> timeout_sec:float ->
  ?priority:Request_priority.t -> t -> (unit -> 'a) -> 'a
(** Like {!with_permit_priority} but with a timeout on acquire.
    @raise Eio.Time.Timeout if permit is not acquired within [timeout_sec].
    @since 0.91.0 *)

val available : t -> int
(** Number of permits currently available. *)

val in_use : t -> int
(** Number of permits currently held. *)

val of_discovery_status : Discovery.endpoint_status -> t option
(** Create a throttle from discovery slot information.
    Returns [None] if no slot/props data is available. *)

val default_for_kind : Provider_config.provider_kind -> t
(** Default throttle limits per provider kind. *)
