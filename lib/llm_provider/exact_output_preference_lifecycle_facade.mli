(** Private implementation facade for the canonical {!Exact_output} surface. *)

type flow_preference_retirement_id = Exact_output_scope_retirement.id
type flow_preference_retirement_intent = Exact_output_scope_retirement.intent

type flow_preference_retirement_receipt = private
  { retirement_id : flow_preference_retirement_id }

type flow_preference_retirement_intent_decode_error =
      Exact_output_scope_retirement.decode_error =
  | Flow_preference_retirement_intent_malformed_json of string
  | Flow_preference_retirement_intent_invalid_fields
  | Flow_preference_retirement_intent_unknown_format of string
  | Flow_preference_retirement_intent_unsupported_version of int
  | Flow_preference_retirement_intent_invalid_field of string
  | Flow_preference_retirement_intent_integrity_mismatch

type 'commit_error flow_preference_retirement_commit_error =
      'commit_error Exact_output_scope_retirement.commit_error =
  | Flow_preference_retirement_commit_failed of 'commit_error
  | Flow_preference_retirement_in_progress
  | Flow_preference_retirement_conflict
  | Flow_preference_scope_not_reserved

type flow_preference_recovery_evidence = Exact_output_preference_recovery.evidence =
  | Domain_settlement_evidence of Exact_output_domain_settlement.intent
  | Scope_retirement_evidence of flow_preference_retirement_intent

type flow_preference_recovery_error = Exact_output_preference_recovery.error =
  | Invalid_concurrent_scope_budget of int
  | Conflicting_domain_settlement_evidence of
      Exact_output_flow_contract.domain_settlement_id
  | Conflicting_scope_retirement_evidence of flow_preference_retirement_id

val domain_settlement_intent_disposition
  :  Exact_output_domain_settlement.intent
  -> Exact_output_flow_contract.domain_disposition

val flow_preference_retirement_id_to_string : flow_preference_retirement_id -> string

val flow_preference_retirement_intent_id
  :  flow_preference_retirement_intent
  -> flow_preference_retirement_id

val flow_preference_retirement_intent_to_string
  :  flow_preference_retirement_intent
  -> string

val flow_preference_retirement_intent_of_string
  :  string
  -> ( flow_preference_retirement_intent
       , flow_preference_retirement_intent_decode_error )
       result

val flow_preference_retirement_receipt_id
  :  flow_preference_retirement_receipt
  -> flow_preference_retirement_id

val commit_and_retire_flow_preference_scope
  :  commit:(flow_preference_retirement_intent -> (unit, 'commit_error) result)
  -> Exact_output_flow_contract.flow_preference_store
  -> Exact_output_flow_contract.flow_scope
  -> ( flow_preference_retirement_receipt
       , 'commit_error flow_preference_retirement_commit_error )
       result

val recover_flow_preferences
  :  concurrent_scope_budget:int
  -> evidence:flow_preference_recovery_evidence list
  -> ( Exact_output_flow_contract.flow_preference_store
       , flow_preference_recovery_error )
       result
