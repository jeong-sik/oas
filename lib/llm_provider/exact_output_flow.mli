(** Private affine executor for an ordered exact-output flow.

    The public contract is exposed only through {!Exact_output}. This module is
    generic so it can own orchestration without depending back on the facade
    types that wrap the private exact plan and execution modules. *)

type t
type ('admission, 'attempt) progress
type ('scope, 'candidate) preference_store
type domain_settlement
type domain_settlement_error = Already_settled

type ('admission, 'attempt) progress_snapshot =
  { candidate_visit_count : int
  ; admissions : 'admission list
  ; attempts : 'attempt list
  }

type ('candidate, 'success, 'execution_error, 'advanceable_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'advanceable_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      }

val create : unit -> t

(** Progress has exactly one writer: the invocation that wins [execute_once]'s
    affine gate. Concurrent readers may observe the honest point between a
    recorded admission and its subsequently allocated attempt. *)
val create_progress : unit -> ('admission, 'attempt) progress

val create_preference_store : unit -> ('scope, 'candidate) preference_store
val create_domain_settlement : unit -> domain_settlement

val preferred_candidate
  :  ('scope, 'candidate) preference_store
  -> scope:'scope
  -> ('candidate * int64) option

val settle_domain_rejected_once
  :  domain_settlement
  -> (unit, domain_settlement_error) result

val settle_domain_valid_once
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> scope:'scope
  -> candidate:'candidate
  -> time:int64
  -> (unit, domain_settlement_error) result

val record_admission : ('admission, 'attempt) progress -> 'admission -> unit
val record_attempt : ('admission, 'attempt) progress -> 'attempt -> unit

val progress_snapshot
  :  ('admission, 'attempt) progress
  -> ('admission, 'attempt) progress_snapshot

val duplicate_key
  :  equal:('key -> 'key -> bool)
  -> key:('candidate -> 'key)
  -> 'candidate list
  -> ('key * int * int) option

val promote_candidate
  :  equal:('key -> 'key -> bool)
  -> key:('candidate -> 'key)
  -> preferred:'key option
  -> 'candidate list
  -> 'candidate list

(** Execute an immutable, nonempty candidate snapshot once.

    [execute] owns preparation, durable pre-dispatch binding, and one-shot
    execution for the current candidate. [before_advance] receives the
    already-selected successor and can only confirm or reject its durable
    transition; it cannot replace or reorder that successor. [advanceable]
    refines an execution error into the only error type accepted by
    [before_advance]; terminal errors cannot reach that callback.

    The outer attempt is affine. A duplicate or concurrent invocation returns
    [Attempt_already_started]. Any exception, including Eio cancellation,
    terminalizes the outer attempt before the exception is re-raised. *)
val execute_once
  :  t
  -> candidates:'candidate list
  -> execute:('candidate -> ('success, 'execution_error) result)
  -> advanceable:('execution_error -> 'advanceable_error option)
  -> before_advance:
       (failed:'candidate
        -> failure:'advanceable_error
        -> next:'candidate
        -> (unit, 'callback_error) result)
  -> ('candidate, 'success, 'execution_error, 'advanceable_error, 'callback_error) outcome
