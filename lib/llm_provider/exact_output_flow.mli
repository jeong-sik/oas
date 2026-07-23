(** Private affine executor for an ordered exact-output flow.

    The public contract is exposed only through {!Exact_output}. This module is
    generic so it can own orchestration without depending back on the facade
    types that wrap the private exact plan and execution modules. *)

type t

type ('candidate, 'success, 'execution_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_dispatch_callback_failed of
      { candidate : 'candidate
      ; cause : 'callback_error
      }
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

(** Execute an immutable, nonempty candidate snapshot once.

    [before_dispatch] must confirm the caller's durable binding before
    [execute] is entered. [before_advance] receives the already-selected
    successor and can only confirm or reject its durable transition; it cannot
    replace or reorder that successor. [can_advance] is supplied exclusively by
    the private facade adapter.

    The outer attempt is affine. A duplicate or concurrent invocation returns
    [Attempt_already_started]. Any exception, including Eio cancellation,
    terminalizes the outer attempt before the exception is re-raised. *)
val execute_once
  :  t
  -> candidates:'candidate list
  -> before_dispatch:('candidate -> (unit, 'callback_error) result)
  -> execute:('candidate -> ('success, 'execution_error) result)
  -> can_advance:('execution_error -> bool)
  -> before_advance:
       (failed:'candidate
        -> failure:'execution_error
        -> next:'candidate
        -> (unit, 'callback_error) result)
  -> ('candidate, 'success, 'execution_error, 'callback_error) outcome
