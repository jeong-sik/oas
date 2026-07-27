(** Private affine executor for an ordered exact-output flow.

    The public contract is exposed only through {!Exact_output}. This module is
    generic so it can own orchestration without depending back on the facade
    types that wrap the private exact plan and execution modules. *)

type t
type ('admission, 'attempt, 'measurement) progress
type ('scope, 'candidate) preference_store
type ('scope, 'candidate) preference_recovery
type preference_reservation
type success_ordinal
type domain_settlement

type preference_reservation_error =
  | Preference_capacity_exhausted of { capacity : int }
  | Preference_reservation_exhausted

type success_ordinal_error = Success_ordinal_exhausted

type domain_disposition =
  | Valid
  | Rejected

type domain_settlement_receipt =
  { settlement_id : string
  ; disposition : domain_disposition
  }

type domain_settlement_begin =
  | Domain_settlement_claimed
  | Domain_settlement_replayed of domain_settlement_receipt
  | Domain_settlement_in_progress
  | Domain_settlement_conflict

type domain_settlement_error = Domain_settlement_apply_conflict

type recovery_domain_error =
  | Preference_recovery_finished
  | Preference_recovery_capacity_exhausted of { capacity : int }
  | Recovered_domain_conflict

type preference_retirement_receipt =
  { retirement_id : string
  ; reservation : preference_reservation
  ; success_high_water : int64
  }

type preference_retirement_begin =
  | Preference_retirement_claimed of preference_retirement_receipt
  | Preference_retirement_replayed of preference_retirement_receipt
  | Preference_retirement_in_progress
  | Preference_retirement_not_reserved
  | Preference_retirement_conflict

type preference_retirement_error = Preference_retirement_apply_conflict

type recovery_retirement_error =
  | Preference_retirement_recovery_finished
  | Recovered_retirement_conflict

type ('admission, 'attempt, 'measurement) progress_snapshot =
  { candidate_visit_count : int
  ; admissions : 'admission list
  ; attempts : 'attempt list
  ; measurements : 'measurement list
  }

type ('accepted, 'rejection) semantic_verdict =
  | Accept of 'accepted
  | Reject_and_advance of 'rejection

type ('candidate
     , 'accepted
     , 'execution_error
     , 'advanceable_error
     , 'semantic_rejection
     , 'callback_error)
     outcome =
  | Succeeded of
      { accepted : 'accepted
      ; prior_rejections : 'semantic_rejection list
      }
  | Semantic_candidates_exhausted of
      { first_rejection : 'semantic_rejection
      ; rest_rejections : 'semantic_rejection list
      }
  | Attempt_already_started
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'advanceable_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      ; prior_rejections : 'semantic_rejection list
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      ; prior_rejections : 'semantic_rejection list
      }

val create : unit -> t

(** Progress has exactly one writer: the invocation that wins [execute_once]'s
    affine gate. Concurrent readers may observe the honest point between a
    recorded admission and its subsequently allocated attempt. *)
val create_progress : unit -> ('admission, 'attempt, 'measurement) progress

val create_validated_preference_recovery
  :  capacity:int
  -> ('scope, 'candidate) preference_recovery

val activate_validated_preference_recovery
  :  ('scope, 'candidate) preference_recovery
  -> ('scope, 'candidate) preference_store

val create_domain_settlement : unit -> domain_settlement

val reserve_preference_scope
  :  ('scope, 'candidate) preference_store
  -> scope:'scope
  -> ( preference_reservation * ('candidate * success_ordinal) option
       , preference_reservation_error )
       result

val allocate_success_ordinal
  :  ('scope, 'candidate) preference_store
  -> (success_ordinal, success_ordinal_error) result

val preference_reservation_to_int64 : preference_reservation -> int64
val preference_reservation_of_int64 : int64 -> preference_reservation option
val success_ordinal_to_int64 : success_ordinal -> int64
val success_ordinal_of_int64 : int64 -> success_ordinal option

(** Claim one live durable settlement. The preference-store mutex is only a
    publication barrier; no caller callback runs while it is held. A concurrent
    same-ID/same-disposition caller receives a typed nonblocking in-progress
    outcome and can replay later for the same receipt. A different disposition
    for that ID is a conflict. *)
val begin_domain_settlement
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> domain_settlement_receipt
  -> domain_settlement_begin

val abort_domain_settlement
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> domain_settlement_receipt
  -> unit

val finish_domain_settlement
  :  domain_settlement
  -> ('scope, 'candidate) preference_store
  -> scope:'scope
  -> reservation:preference_reservation
  -> candidate:'candidate
  -> ordinal:success_ordinal
  -> domain_settlement_receipt
  -> (domain_settlement_receipt, domain_settlement_error) result

(** Replay one caller-authenticated committed intent while the store is still
    recovering. This path has no transport capability. It advances reservation
    and success high-water marks before the store can become active. A valid
    intent for a new scope fails before any mutation if capacity is full. *)
val resume_committed_domain
  :  ('scope, 'candidate) preference_recovery
  -> restore_preference:bool
  -> scope:'scope
  -> reservation:preference_reservation
  -> candidate:'candidate
  -> ordinal:success_ordinal
  -> domain_settlement_receipt
  -> (domain_settlement_receipt, recovery_domain_error) result

val begin_preference_retirement
  :  ('scope, 'candidate) preference_store
  -> scope:'scope
  -> make_receipt:
       (reservation:preference_reservation
        -> success_high_water:int64
        -> preference_retirement_receipt)
  -> preference_retirement_begin

val abort_preference_retirement
  :  ('scope, 'candidate) preference_store
  -> same_receipt:(preference_retirement_receipt -> preference_retirement_receipt -> bool)
  -> scope:'scope
  -> preference_retirement_receipt
  -> unit

val mark_preference_retirement_indeterminate
  :  ('scope, 'candidate) preference_store
  -> same_receipt:(preference_retirement_receipt -> preference_retirement_receipt -> bool)
  -> scope:'scope
  -> preference_retirement_receipt
  -> unit

val finish_preference_retirement
  :  ('scope, 'candidate) preference_store
  -> same_receipt:(preference_retirement_receipt -> preference_retirement_receipt -> bool)
  -> scope:'scope
  -> preference_retirement_receipt
  -> (preference_retirement_receipt, preference_retirement_error) result

val resume_committed_retirement
  :  ('scope, 'candidate) preference_recovery
  -> same_receipt:(preference_retirement_receipt -> preference_retirement_receipt -> bool)
  -> scope:'scope
  -> preference_retirement_receipt
  -> (preference_retirement_receipt, recovery_retirement_error) result

val record_admission : ('admission, 'attempt, 'measurement) progress -> 'admission -> unit
val record_attempt : ('admission, 'attempt, 'measurement) progress -> 'attempt -> unit

val publish_attempt
  :  ('admission, 'attempt, 'measurement) progress
  -> same:('attempt -> 'attempt -> bool)
  -> 'attempt
  -> unit

val publish_measurement
  :  ('admission, 'attempt, 'measurement) progress
  -> same:('measurement -> 'measurement -> bool)
  -> 'measurement
  -> unit

val progress_snapshot
  :  ('admission, 'attempt, 'measurement) progress
  -> ('admission, 'attempt, 'measurement) progress_snapshot

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

(** Execute one immutable, nonempty candidate snapshot in its declared order.

    Each candidate is passed to [execute] at most once. A successful transport
    result is passed exactly once to the pure [validate] callback. [Accept]
    terminates the flow; [Reject_and_advance] records opaque evidence and moves
    directly to the predetermined successor without invoking [before_advance].
    [before_advance] remains reserved for OAS-classified execution failures.

    The outer attempt is affine. A duplicate or concurrent invocation returns
    [Attempt_already_started]. Any exception, including Eio cancellation,
    terminalizes the outer attempt before the exception is re-raised. *)
val execute_once
  :  t
  -> candidates:'candidate list
  -> execute:('candidate -> ('success, 'execution_error) result)
  -> validate:
       ('candidate -> 'success -> ('accepted, 'semantic_rejection) semantic_verdict)
  -> advanceable:('execution_error -> 'advanceable_error option)
  -> before_advance:
       (failed:'candidate
        -> failure:'advanceable_error
        -> next:'candidate
        -> (unit, 'callback_error) result)
  -> ( 'candidate
       , 'accepted
       , 'execution_error
       , 'advanceable_error
       , 'semantic_rejection
       , 'callback_error )
       outcome
