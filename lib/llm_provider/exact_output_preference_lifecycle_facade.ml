module Domain_settlement = Exact_output_domain_settlement
module Flow_contract = Exact_output_flow_contract
module Preference_recovery = Exact_output_preference_recovery
module Scope_retirement = Exact_output_scope_retirement

type flow_preference_retirement_id = Scope_retirement.id
type flow_preference_retirement_intent = Scope_retirement.intent

type flow_preference_retirement_receipt = Scope_retirement.receipt =
  { retirement_id : flow_preference_retirement_id
  }

type flow_preference_retirement_intent_decode_error = Scope_retirement.decode_error =
  | Flow_preference_retirement_intent_malformed_json of string
  | Flow_preference_retirement_intent_invalid_fields
  | Flow_preference_retirement_intent_unknown_format of string
  | Flow_preference_retirement_intent_unsupported_version of int
  | Flow_preference_retirement_intent_invalid_field of string
  | Flow_preference_retirement_intent_integrity_mismatch

type 'commit_error flow_preference_retirement_commit_error =
  'commit_error Scope_retirement.commit_error =
  | Flow_preference_retirement_commit_failed of 'commit_error
  | Flow_preference_retirement_in_progress
  | Flow_preference_retirement_conflict
  | Flow_preference_scope_not_reserved

type flow_preference_recovery_evidence = Preference_recovery.evidence =
  | Domain_settlement_evidence of Domain_settlement.intent
  | Scope_retirement_evidence of flow_preference_retirement_intent

type flow_preference_recovery_error = Preference_recovery.error =
  | Invalid_concurrent_scope_budget of int
  | Conflicting_domain_settlement_evidence of Flow_contract.domain_settlement_id
  | Conflicting_scope_retirement_evidence of flow_preference_retirement_id

let domain_settlement_intent_disposition = Domain_settlement.intent_disposition
let flow_preference_retirement_id_to_string = Scope_retirement.id_to_string
let flow_preference_retirement_intent_id = Scope_retirement.intent_id
let flow_preference_retirement_intent_to_string = Scope_retirement.intent_to_string
let flow_preference_retirement_intent_of_string = Scope_retirement.intent_of_string
let flow_preference_retirement_receipt_id = Scope_retirement.receipt_id

let commit_and_retire_flow_preference_scope ~commit preferences scope =
  Scope_retirement.commit_and_retire ~commit preferences scope
;;

let recover_flow_preferences = Preference_recovery.recover
