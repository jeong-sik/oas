(** Private current-schema durable preference-scope retirement codec. *)

type id
type intent

type receipt = private
  { retirement_id : id
  }

type decode_error =
  | Flow_preference_retirement_intent_malformed_json of string
  | Flow_preference_retirement_intent_invalid_fields
  | Flow_preference_retirement_intent_unknown_format of string
  | Flow_preference_retirement_intent_unsupported_version of int
  | Flow_preference_retirement_intent_invalid_field of string
  | Flow_preference_retirement_intent_integrity_mismatch

type 'commit_error commit_error =
  | Flow_preference_retirement_commit_failed of 'commit_error
  | Flow_preference_retirement_in_progress
  | Flow_preference_retirement_conflict
  | Flow_preference_scope_not_reserved

type resume_error =
  | Flow_preference_retirement_recovery_finished
  | Flow_preference_retirement_recovery_conflict

type recovery_evidence =
  { retirement_id : id
  ; scope : Exact_output_flow_contract.flow_scope
  ; reservation : Exact_output_flow_contract.flow_preference_reservation
  ; success_high_water : int64
  }

val id_to_string : id -> string
val intent_id : intent -> id
val intent_to_string : intent -> string
val intent_of_string : string -> (intent, decode_error) result
val receipt_id : receipt -> id
val recovery_evidence : intent -> recovery_evidence

val commit_and_retire
  :  commit:(intent -> (unit, 'commit_error) result)
  -> Exact_output_flow_contract.flow_preference_store
  -> Exact_output_flow_contract.flow_scope
  -> (receipt, 'commit_error commit_error) result

val resume
  :  Exact_output_flow_contract.flow_preference_recovery
  -> intent
  -> (receipt, resume_error) result
