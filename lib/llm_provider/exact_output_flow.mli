(** Private affine executor for an ordered exact-output flow.

    The public contract is exposed only through {!Exact_output}. This module is
    generic so it can own orchestration without depending back on the facade
    types that wrap the private exact plan and execution modules. *)

type t
type ('admission, 'attempt) progress
type ('scope, 'candidate) preference_store
type preference_reservation
type success_ordinal
type domain_settlement
type preference_store_error = Invalid_preference_capacity of int
type preference_reservation_error = Preference_capacity_exhausted of { capacity : int }

type preference_scope_removal =
  | Preference_scope_removed
  | Preference_scope_not_reserved

type success_ordinal_error = Success_ordinal_exhausted

type domain_settlement_error =
  | Already_settled
  | Preference_scope_released

type 'candidate preference_installation =
  | Preference_installed
  | Preference_superseded of
      { current_candidate : 'candidate
      ; current_ordinal : success_ordinal
      }

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

val create_preference_store
  :  capacity:int
  -> (('scope, 'candidate) preference_store, preference_store_error) result

val create_domain_settlement : unit -> domain_settlement

val reserve_preference_scope
  :  ('scope, 'candidate) preference_store
  -> scope:'scope
  -> ( preference_reservation * ('candidate * success_ordinal) option
       , preference_reservation_error )
       result

val remove_preference_scope
  :  ('scope, 'candidate) preference_store
  -> scope:'scope
  -> preference_scope_removal

val allocate_success_ordinal
  :  ('scope, 'candidate) preference_store
  -> (success_ordinal, success_ordinal_error) result

val success_ordinal_to_int64 : success_ordinal -> int64

(** Settlement uses the preference store's single mutex as its publication
    barrier. Domain-valid acquires that lock before changing [Pending] to
    [Publishing], terminalizes [Settled] before unlocking even on exception,
    and publishes the preference while the lock is held. A losing disposition
    synchronizes through the same lock before returning [Already_settled].
    There is no per-settlement mutex or nested lock order. *)
val settle_domain_rejected_once
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> (unit, domain_settlement_error) result

val settle_domain_valid_once
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> scope:'scope
  -> reservation:preference_reservation
  -> candidate:'candidate
  -> ordinal:success_ordinal
  -> ('candidate preference_installation, domain_settlement_error) result

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
