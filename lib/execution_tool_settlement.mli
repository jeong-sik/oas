(** Private authority: handlers run only after [begin_attempt] returns. *)

type t
type attempt

type status =
  | Ready_to_execute
  | Outcome_unknown
  | Settled of Llm_provider.Types.content_block

type error =
  | Authority_unavailable of Execution_lane_writer.read_error
  | Invocation_not_found
  | Invocation_identity_mismatch
  | Attempt_admission_failed of Execution_lane_writer.submit_error
  | Attempt_commit_failed of Execution_lane_writer.ticket_error
  | Receipt_admission_outcome_unknown of Execution_lane_writer.submit_error
  | Receipt_settlement_outcome_unknown of Execution_lane_writer.ticket_error

type receipt = private
  { invocation : Tool.Invocation.t
  ; result : Llm_provider.Types.content_block
  ; through : Execution_journal.cursor
  }

val create
  :  writer:Execution_lane_writer.t
  -> run:Execution_journal.run
  -> invocation_node:Execution_event.Node_id.t
  -> invocation:Tool.Invocation.t
  -> (t, error) result

val status : t -> (status, error) result
val begin_attempt : t -> (attempt, error) result
val settle : attempt -> result:Llm_provider.Types.content_block -> (receipt, error) result
