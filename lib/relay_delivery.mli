(** Two-stage event relay delivery.

    This module is a small, domain-neutral state machine for adapters that
    persist an event before publishing it to a live transport.  If publish
    fails after persistence succeeds, retrying the returned pending value
    skips persistence and only retries publish.  This preserves the
    durable-append invariant without making OAS own any downstream transport
    or dashboard schema. *)

type stage =
  | Persist
  | Publish
  | Queue

type 'a pending = private
  { payload : 'a
  ; attempts : int
  ; persisted : bool
  }

type 'a delivery_result =
  | Delivered
  | Retryable_failure of 'a pending * stage * exn

type stats =
  { queue_depth : int
  ; retry_total : int
  ; drop_total : int
  ; retry_persist_total : int
  ; retry_publish_total : int
  ; drop_persist_total : int
  ; drop_publish_total : int
  ; drop_queue_total : int
  }

type 'a t

val stage_to_string : stage -> string
val make_pending : 'a -> 'a pending

val deliver_with
  :  persist:('a -> unit)
  -> publish:('a -> unit)
  -> 'a pending
  -> 'a delivery_result

val create : ?max_attempts:int -> ?max_queue_depth:int -> unit -> 'a t
val enqueue : 'a t -> 'a -> 'a pending option
val process_once : 'a t -> persist:('a -> unit) -> publish:('a -> unit) -> unit
val pending : 'a t -> 'a pending list
val stats : 'a t -> stats
val health_probe : ?checked_at:float -> ?name:string -> stats -> Runtime_health.probe
