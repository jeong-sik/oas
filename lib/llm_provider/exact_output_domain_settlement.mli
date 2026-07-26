(** Private current-schema durable settlement codec and transition fence. *)

type intent

type decode_error =
  | Domain_settlement_intent_malformed_json of string
  | Domain_settlement_intent_invalid_fields
  | Domain_settlement_intent_unknown_format of string
  | Domain_settlement_intent_unsupported_version of int
  | Domain_settlement_intent_invalid_field of string
  | Domain_settlement_intent_integrity_mismatch

type 'commit_error commit_error =
  | Domain_commit_failed of 'commit_error
  | Domain_settlement_conflict

type resume_error =
  | Domain_preference_recovery_finished
  | Preference_recovery_capacity_exhausted of { capacity : int }
  | Domain_settlement_recovery_conflict

val intent_id : intent -> Exact_output_flow_contract.domain_settlement_id
val intent_to_string : intent -> string
val intent_of_string : string -> (intent, decode_error) result

val commit_and_settle
  :  commit:(intent -> (unit, 'commit_error) result)
  -> domain_settlement:Exact_output_flow.domain_settlement
  -> preferences:Exact_output_flow_contract.flow_preference_store
  -> scope:Exact_output_flow_contract.flow_scope
  -> reservation:Exact_output_flow_contract.flow_preference_reservation
  -> candidate:Exact_output_flow_contract.flow_candidate_identity
  -> success_ordinal:Exact_output_flow_contract.flow_success_ordinal
  -> flow_id:string
  -> execution:Exact_output_generation_receipt.snapshot
  -> Exact_output_flow_contract.domain_disposition
  -> ( Exact_output_flow_contract.domain_settlement_receipt
       , 'commit_error commit_error )
       result

val resume
  :  Exact_output_flow_contract.flow_preference_recovery
  -> intent
  -> (Exact_output_flow_contract.domain_settlement_receipt, resume_error) result
