(** Provider-level concurrency throttle with priority-aware scheduling.

    Limits concurrent LLM requests per provider to avoid overwhelming
    backends with limited capacity (e.g. llama-server with N slots).
    When slots are full, requests are queued by priority.

    @since 0.84.0

    @stability Internal
    @since 0.93.1 *)

(** How the throttle's slot count was determined. *)
type capacity_source =
  | Discovered (** Slot count from [GET /slots] or [GET /props] endpoint data. *)
  | Fallback (** Default slot count; no slot data was available at creation time. *)

type t

(** Create a throttle. [max_concurrent] must be >= 1.
    @raise Invalid_argument if [max_concurrent] < 1 *)
val create : max_concurrent:int -> provider_name:string -> t

(** Run [f] with a permit at the given [priority].
    If all permits are in use, the request is queued by priority.
    Higher priority requests are dequeued first.
    @since 0.96.0 *)
val with_permit_priority : priority:Request_priority.t -> t -> (unit -> 'a) -> 'a

(** Run [f] with a permit at [Background] priority.
    Kept for backward compatibility with pre-scheduling callers. *)
val with_permit : t -> (unit -> 'a) -> 'a

(** Like {!with_permit_priority} but with a timeout on acquire.
    @raise Eio.Time.Timeout if permit is not acquired within [timeout_sec].
    @since 0.91.0 *)
val with_permit_timeout
  :  _ Eio.Time.clock
  -> timeout_sec:float
  -> ?priority:Request_priority.t
  -> t
  -> (unit -> 'a)
  -> 'a

(** Number of permits currently available. *)
val available : t -> int

(** Number of permits currently held. *)
val in_use : t -> int

(** Create a throttle from discovery slot information.
    Returns [None] if no slot/props data is available. *)
val of_discovery_status : Discovery.endpoint_status -> t option

(** Default throttle limits per provider kind.
    Created with [source = Fallback]. *)
val default_for_kind : Provider_config.provider_kind -> t

(** {2 Capacity Query}

    All counts reflect this OAS process only — other clients
    sharing the same LLM server are not visible. *)

(** Non-blocking point-in-time capacity snapshot. *)
val snapshot : t -> Slot_scheduler.snapshot

(** How the slot count was determined. *)
val source : t -> capacity_source

(** Run [f] if a permit is immediately available, returning [Some result].
    Returns [None] without blocking if all permits are in use.
    @since 0.97.0 *)
val try_permit : priority:Request_priority.t -> t -> (unit -> 'a) -> 'a option

(** Number of fibers waiting for a permit. *)
val queue_length : t -> int

(** Maximum concurrent permits configured for this throttle. *)
val max_concurrent : t -> int

(** {2 Turn-Level Yield API}

    Supports the "Agent exists != LLM slot held" pattern.
    Agents acquire a permit, yield it during tool execution,
    then resume before the next LLM turn.

    @since 0.100.0 *)

type yield_capability =
  | Explicit_slot_yield (** llama.cpp: KV cache save/restore guaranteed. *)
  | Prefix_hint_yield (** vLLM/SGLang: prefix re-send, best-effort cache hit. *)
  | Replay_yield (** Ollama/cloud: full history re-send. *)

(** Yield capability based on the provider kind. *)
val yield_capability : t -> yield_capability

(** Acquire a slot permit at the given priority. *)
val acquire_permit : priority:Request_priority.t -> t -> Slot_scheduler.permit

(** Yield a held permit (release slot for other agents). *)
val yield_permit : t -> Slot_scheduler.permit -> unit

(** Re-acquire a yielded permit at [Resume] priority. *)
val resume_permit : t -> Slot_scheduler.permit -> unit

(** Permanently release a permit. *)
val release_permit : t -> Slot_scheduler.permit -> unit
