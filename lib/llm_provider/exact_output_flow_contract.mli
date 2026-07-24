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

type flow_preference_store
type flow_scope
type flow_preference_reservation
type flow_success_ordinal
type flow_preference_store_error = Invalid_flow_preference_capacity of int

type flow_preference_reservation_error =
  | Preference_capacity_exhausted of { capacity : int }

type flow_preference_scope_removal =
  | Flow_preference_scope_removed
  | Flow_preference_scope_not_reserved

type flow_success_ordinal_error = Success_ordinal_space_exhausted

type flow_preference_not_applied_reason =
  | Preference_candidate_absent
  | Preference_candidate_binding_changed

type flow_preference_observation =
  | No_preference_recorded
  | Preference_applied of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      }
  | Preference_not_applied of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      ; reason : flow_preference_not_applied_reason
      }

type flow_scope_error = Blank_flow_scope_id

type domain_disposition =
  | Domain_valid
  | Domain_rejected

type domain_settlement_receipt = private
  | Domain_rejected_recorded
  | Domain_valid_preference_installed of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      }
  | Domain_valid_preference_superseded of
      { current_candidate : flow_candidate_identity
      ; current_success_ordinal : flow_success_ordinal
      }

type domain_settlement_error =
  | Domain_already_settled
  | Domain_preference_scope_released

val create_flow_preference_store
  :  capacity:int
  -> (flow_preference_store, flow_preference_store_error) result

val make_flow_scope : id:string -> (flow_scope, flow_scope_error) result
val flow_scope_equal : flow_scope -> flow_scope -> bool
val flow_success_ordinal_to_int64 : flow_success_ordinal -> int64

val remove_flow_preference_scope
  :  flow_preference_store
  -> flow_scope
  -> flow_preference_scope_removal

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

val settle_domain
  :  Exact_output_flow.domain_settlement
  -> flow_preference_store
  -> flow_scope
  -> reservation:flow_preference_reservation
  -> candidate:flow_candidate_identity
  -> success_ordinal:flow_success_ordinal
  -> domain_disposition
  -> (domain_settlement_receipt, domain_settlement_error) result
