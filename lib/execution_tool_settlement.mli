type t

type error =
  | Authority_unavailable of Execution_lane_writer.read_error
  | Invocation_not_found
  | Invocation_identity_mismatch
  | Effect_outcome_unknown
  | Attempt_admission_failed of Execution_lane_writer.submit_error
  | Attempt_commit_failed of Execution_lane_writer.ticket_error
  | Receipt_admission_outcome_unknown of Execution_lane_writer.submit_error
  | Receipt_settlement_outcome_unknown of Execution_lane_writer.ticket_error

type execution =
  | Executed of Llm_provider.Types.content_block * Execution_journal.cursor * int
  | Replayed of Llm_provider.Types.content_block

val create
  :  writer:Execution_lane_writer.t
  -> invocation_node:Execution_event.Node_id.t
  -> invocation:Tool.Invocation.t
  -> (t, error) result

val execute
  :  t
  -> invoke:(unit -> Llm_provider.Types.content_block)
  -> (execution, error) result
