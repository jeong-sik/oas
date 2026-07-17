(** Private authority: [execute] commits the exact attempt before invoking a handler. *)

type t

type status =
  | Ready_to_execute
  | Outcome_unknown
  | Settled of Llm_provider.Types.content_block

type error =
  | Authority_unavailable of Execution_lane_writer.read_error
  | Invocation_not_found
  | Invocation_identity_mismatch
  | Effect_outcome_unknown
  | Attempt_admission_failed of Execution_lane_writer.submit_error
  | Attempt_commit_failed of Execution_lane_writer.ticket_error
  | Receipt_admission_outcome_unknown of Execution_lane_writer.submit_error
  | Receipt_settlement_outcome_unknown of Execution_lane_writer.ticket_error

type receipt = private
  { invocation : Tool.Invocation.t
  ; result : Llm_provider.Types.content_block
  ; through : Execution_journal.cursor
  }

type execution =
  | Executed of receipt
  | Replayed of Llm_provider.Types.content_block

val create
  :  writer:Execution_lane_writer.t
  -> run:Execution_journal.run
  -> invocation_node:Execution_event.Node_id.t
  -> invocation:Tool.Invocation.t
  -> (t, error) result

val status : t -> (status, error) result

(** [execute authority ~invoke] commits
    the exact attempt before invoking [invoke], then atomically settles the
    canonical result. A previously settled invocation is replayed without
    invoking [invoke]. An open prior attempt is returned explicitly as
    [Effect_outcome_unknown]; it is never retried. Exceptions from [invoke]
    propagate with their original backtrace and leave that committed attempt
    open, so a later caller observes the unknown outcome. *)
val execute
  :  t
  -> invoke:(unit -> Llm_provider.Types.content_block)
  -> (execution, error) result
