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

type flow_preference_not_applied_reason =
  | Preference_candidate_absent
  | Preference_candidate_binding_changed

type flow_preference_observation =
  | No_preference_recorded
  | Preference_applied of
      { candidate : flow_candidate_identity
      ; success_time_unix_s : int64
      }
  | Preference_not_applied of
      { candidate : flow_candidate_identity
      ; success_time_unix_s : int64
      ; reason : flow_preference_not_applied_reason
      }

type flow_scope_error = Blank_flow_scope_id

type domain_disposition =
  | Domain_valid of { success_time_unix_s : int64 }
  | Domain_rejected

type domain_settlement_receipt =
  | Domain_rejected_recorded
  | Domain_valid_preference_installed of
      { candidate : flow_candidate_identity
      ; success_time_unix_s : int64
      }
  | Domain_valid_preference_superseded of
      { current_candidate : flow_candidate_identity
      ; current_success_time_unix_s : int64
      }

type domain_settlement_error = Domain_already_settled

val create_flow_preference_store : unit -> flow_preference_store
val make_flow_scope : id:string -> (flow_scope, flow_scope_error) result
val flow_scope_equal : flow_scope -> flow_scope -> bool

val prefer_last_good
  :  flow_preference_store
  -> flow_scope
  -> candidate_identity:('candidate -> flow_candidate_identity)
  -> 'candidate list
  -> 'candidate list * flow_preference_observation

val settle_domain
  :  Exact_output_flow.domain_settlement
  -> flow_preference_store
  -> flow_scope
  -> candidate:flow_candidate_identity
  -> domain_disposition
  -> (domain_settlement_receipt, domain_settlement_error) result
