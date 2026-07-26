(** Private typed contract for scope-local exact-output preferences.

    The public surface is re-exported by {!Exact_output}. Keeping the state
    transition and its evidence types together prevents the facade from
    reimplementing preference ordering or domain settlement. *)

type flow_candidate_identity =
  { candidate_id : string
  ; catalog_generation : Exact_output_resolver.catalog_generation
  ; catalog_evidence : Exact_output_resolver.catalog_evidence
  ; target_identity : Exact_output_resolver.target_identity
  }

type flow_preference_identity = private
  { candidate_id : string
  ; binding_sha256 : string
  }

type flow_preference_store
type flow_preference_recovery
type flow_scope
type flow_preference_reservation
type flow_success_ordinal

type flow_preference_reservation_error =
  | Preference_capacity_exhausted of { capacity : int }
  | Preference_reservation_space_exhausted

type flow_success_ordinal_error = Success_ordinal_space_exhausted

type flow_preference_not_applied_reason =
  | Preference_candidate_absent
  | Preference_candidate_binding_changed

type flow_preference_observation =
  | No_preference_recorded
  | Preference_applied of
      { candidate : flow_preference_identity
      ; success_ordinal : flow_success_ordinal
      }
  | Preference_not_applied of
      { candidate : flow_preference_identity
      ; success_ordinal : flow_success_ordinal
      ; reason : flow_preference_not_applied_reason
      }

type flow_scope_error = Blank_flow_scope_id

type domain_disposition =
  | Domain_valid
  | Domain_rejected

type domain_settlement_id

type domain_settlement_receipt = private
  { settlement_id : domain_settlement_id
  ; disposition : domain_disposition
  }

type domain_settlement_begin =
  | Domain_settlement_claimed
  | Domain_settlement_replayed of domain_settlement_receipt
  | Domain_settlement_in_progress
  | Domain_settlement_conflict

type domain_settlement_apply_error = Domain_settlement_apply_conflict

type domain_settlement_recovery_error =
  | Domain_preference_recovery_finished
  | Preference_recovery_capacity_exhausted of { capacity : int }
  | Domain_settlement_recovery_conflict

type flow_preference_retirement_receipt =
  { retirement_id : string
  ; reservation : flow_preference_reservation
  ; success_high_water : int64
  }

type flow_preference_retirement_begin =
  | Flow_preference_retirement_claimed of flow_preference_retirement_receipt
  | Flow_preference_retirement_replayed of flow_preference_retirement_receipt
  | Flow_preference_retirement_in_progress
  | Flow_preference_scope_not_reserved
  | Flow_preference_retirement_conflict

type flow_preference_retirement_apply_error =
  | Flow_preference_retirement_apply_conflict

type flow_preference_retirement_recovery_error =
  | Flow_preference_retirement_recovery_finished
  | Flow_preference_retirement_recovery_conflict

val create_validated_flow_preference_recovery
  :  capacity:int
  -> flow_preference_recovery

val activate_validated_flow_preference_recovery
  :  flow_preference_recovery
  -> flow_preference_store

val make_flow_scope : id:string -> (flow_scope, flow_scope_error) result
val flow_scope_equal : flow_scope -> flow_scope -> bool
val flow_scope_to_string : flow_scope -> string
val flow_preference_reservation_to_int64 : flow_preference_reservation -> int64
val flow_preference_reservation_of_int64 : int64 -> flow_preference_reservation option
val flow_success_ordinal_to_int64 : flow_success_ordinal -> int64
val flow_success_ordinal_of_int64 : int64 -> flow_success_ordinal option

val flow_preference_identity_of_candidate
  :  flow_candidate_identity
  -> flow_preference_identity

val make_flow_preference_identity
  :  candidate_id:string
  -> binding_sha256:string
  -> flow_preference_identity

val allocate_flow_success_ordinal
  :  flow_preference_store
  -> (flow_success_ordinal, flow_success_ordinal_error) result

val prefer_last_good
  :  flow_preference_store
  -> flow_scope
  -> candidate_identity:('candidate -> flow_candidate_identity)
  -> 'candidate list
  -> ( 'candidate list * flow_preference_observation * flow_preference_reservation
       , flow_preference_reservation_error )
       result

val domain_settlement_id_to_string : domain_settlement_id -> string
val domain_settlement_id_of_string : string -> domain_settlement_id

val make_domain_settlement_receipt
  :  settlement_id:domain_settlement_id
  -> disposition:domain_disposition
  -> domain_settlement_receipt

val domain_settlement_receipt_id : domain_settlement_receipt -> domain_settlement_id

val domain_settlement_receipt_disposition
  :  domain_settlement_receipt
  -> domain_disposition

val begin_domain_settlement
  :  Exact_output_flow.domain_settlement
  -> flow_preference_store
  -> domain_settlement_receipt
  -> domain_settlement_begin

val abort_domain_settlement
  :  Exact_output_flow.domain_settlement
  -> flow_preference_store
  -> domain_settlement_receipt
  -> unit

val finish_domain_settlement
  :  Exact_output_flow.domain_settlement
  -> flow_preference_store
  -> flow_scope
  -> reservation:flow_preference_reservation
  -> candidate:flow_candidate_identity
  -> success_ordinal:flow_success_ordinal
  -> domain_settlement_receipt
  -> (domain_settlement_receipt, domain_settlement_apply_error) result

val resume_committed_domain
  :  flow_preference_recovery
  -> restore_preference:bool
  -> flow_scope
  -> reservation:flow_preference_reservation
  -> candidate:flow_preference_identity
  -> success_ordinal:flow_success_ordinal
  -> domain_settlement_receipt
  -> (domain_settlement_receipt, domain_settlement_recovery_error) result

val begin_flow_preference_retirement
  :  flow_preference_store
  -> flow_scope
  -> make_receipt:
       (reservation:flow_preference_reservation
        -> success_high_water:int64
        -> flow_preference_retirement_receipt)
  -> flow_preference_retirement_begin

val abort_flow_preference_retirement
  :  flow_preference_store
  -> flow_scope
  -> flow_preference_retirement_receipt
  -> unit

val mark_flow_preference_retirement_indeterminate
  :  flow_preference_store
  -> flow_scope
  -> flow_preference_retirement_receipt
  -> unit

val finish_flow_preference_retirement
  :  flow_preference_store
  -> flow_scope
  -> flow_preference_retirement_receipt
  -> ( flow_preference_retirement_receipt
       , flow_preference_retirement_apply_error )
       result

val resume_committed_flow_preference_retirement
  :  flow_preference_recovery
  -> flow_scope
  -> flow_preference_retirement_receipt
  -> ( flow_preference_retirement_receipt
       , flow_preference_retirement_recovery_error )
       result
