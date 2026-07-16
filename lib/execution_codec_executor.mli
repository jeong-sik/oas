(** Shared parallel boundary for canonical execution-event codecs.

    This module is private to OAS. The application owns one long-lived
    {!Eio.Executor_pool.t}, chooses its domain count at startup, and injects it
    into every execution lane. The wrapper never creates domains, applies a
    payload-size threshold, or controls admission. *)

type t

type operation =
  | Encode_events
  | Decode_canonical_events
  | Compare_canonical_payloads
[@@deriving show]

type cause

type failure =
  | Executor_unavailable of
      { operation : operation
      ; cause : cause
      }
  | Codec_raised of
      { operation : operation
      ; cause : cause
      }

type decode_failure =
  | Invalid_event of
      { ordinal : int
      ; detail : string
      }
  | Noncanonical_event of { ordinal : int }

type operation_stats =
  { requested : int
  ; started : int
  ; completed : int
  ; job_failed : int
  ; executor_failed : int
  ; caller_cancelled : int
  ; last_caller_domain : Domain.id option
  ; last_worker_domain : Domain.id option
  }

type stats =
  { encode_events : operation_stats
  ; decode_canonical_events : operation_stats
  ; compare_canonical_payloads : operation_stats
  }

val of_executor_pool : Eio.Executor_pool.t -> t

(** These are the only codec requests. Their operation identity is derived by
    the executor and cannot be supplied independently by callers. Every
    request uses executor weight [1.0] because it is CPU-bound work, not as a
    runtime budget or heuristic. Caller cancellation and reserved worker
    exceptions propagate; non-reserved failures remain typed. *)
val encode_events : t -> Execution_event.t list -> (string list, failure) result

val decode_canonical_events
  :  t
  -> string list
  -> ((Execution_event.t list, decode_failure) result, failure) result

val compare_canonical_payloads
  :  t
  -> expected:string list
  -> actual:string list
  -> (bool, failure) result

val failure_to_string : failure -> string
val pp_failure : Format.formatter -> failure -> unit
val show_failure : failure -> string

(** Read-only per-operation coherent snapshots. These observations never
    affect dispatch, cancellation, admission, or termination. *)
val stats : t -> stats
