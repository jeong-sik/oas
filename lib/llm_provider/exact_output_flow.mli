(** Private affine executor for an ordered exact-output flow.

    The public contract is exposed only through {!Exact_output}. This module is
    generic so it can own orchestration without depending back on the facade
    types that wrap the private exact plan and execution modules. *)

type t
type ('admission, 'attempt) progress

type ('admission, 'attempt) progress_snapshot =
  { candidate_attempt_count : int
  ; admissions : 'admission list
  ; attempts : 'attempt list
  }

type ('candidate, 'success, 'execution_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'execution_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      }

val create : unit -> t
val create_progress : unit -> ('admission, 'attempt) progress

val record_admission
  :  ('admission, 'attempt) progress
  -> (candidate_attempt_count:int -> 'admission * 'result)
  -> 'result

val record_attempt : ('admission, 'attempt) progress -> 'attempt -> unit

val progress_snapshot
  :  ('admission, 'attempt) progress
  -> ('admission, 'attempt) progress_snapshot

val duplicate_key
  :  equal:('key -> 'key -> bool)
  -> key:('candidate -> 'key)
  -> 'candidate list
  -> ('key * int * int) option

(** Execute an immutable, nonempty candidate snapshot once.

    [execute] owns preparation, durable pre-dispatch binding, and one-shot
    execution for the current candidate. [before_advance] receives the
    already-selected successor and can only confirm or reject its durable
    transition; it cannot replace or reorder that successor. [can_advance] is
    supplied exclusively by the private facade adapter.

    The outer attempt is affine. A duplicate or concurrent invocation returns
    [Attempt_already_started]. Any exception, including Eio cancellation,
    terminalizes the outer attempt before the exception is re-raised. *)
val execute_once
  :  t
  -> candidates:'candidate list
  -> execute:('candidate -> ('success, 'execution_error) result)
  -> can_advance:('execution_error -> bool)
  -> before_advance:
       (failed:'candidate
        -> failure:'execution_error
        -> next:'candidate
        -> (unit, 'callback_error) result)
  -> ('candidate, 'success, 'execution_error, 'callback_error) outcome
