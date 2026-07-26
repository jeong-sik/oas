module Flow_state = Exact_output_flow
module Resolver = Exact_output_resolver

type flow_candidate_identity =
  { candidate_id : string
  ; catalog_generation : Resolver.catalog_generation
  ; catalog_evidence : Resolver.catalog_evidence
  ; target_identity : Resolver.target_identity
  }

type flow_preference_identity =
  { candidate_id : string
  ; binding_sha256 : string
  }

type flow_preference_store =
  (string, flow_preference_identity) Flow_state.preference_store

type flow_preference_recovery =
  (string, flow_preference_identity) Flow_state.preference_recovery

type flow_scope = Flow_scope of string
type flow_preference_reservation = Flow_state.preference_reservation
type flow_success_ordinal = Flow_state.success_ordinal

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

type domain_settlement_id = Domain_settlement_id of string

type domain_settlement_receipt =
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

type flow_preference_retirement_receipt = Flow_state.preference_retirement_receipt =
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

let create_validated_flow_preference_recovery ~capacity =
  Flow_state.create_validated_preference_recovery ~capacity
;;

let activate_validated_flow_preference_recovery recovery =
  Flow_state.activate_validated_preference_recovery recovery
;;

let make_flow_scope ~id =
  let id = String.trim id in
  if String.equal id "" then Error Blank_flow_scope_id else Ok (Flow_scope id)
;;

let flow_scope_equal (Flow_scope left) (Flow_scope right) = String.equal left right
let flow_scope_to_string (Flow_scope scope) = scope
let flow_preference_reservation_to_int64 = Flow_state.preference_reservation_to_int64
let flow_preference_reservation_of_int64 = Flow_state.preference_reservation_of_int64
let flow_success_ordinal_to_int64 = Flow_state.success_ordinal_to_int64
let flow_success_ordinal_of_int64 = Flow_state.success_ordinal_of_int64

let flow_preference_identity_of_candidate (candidate : flow_candidate_identity) =
  { candidate_id = candidate.candidate_id
  ; binding_sha256 = Resolver.target_identity_fingerprint candidate.target_identity
  }
;;

let make_flow_preference_identity ~candidate_id ~binding_sha256 =
  { candidate_id; binding_sha256 }
;;

let allocate_flow_success_ordinal preferences =
  match Flow_state.allocate_success_ordinal preferences with
  | Ok ordinal -> Ok ordinal
  | Error Flow_state.Success_ordinal_exhausted -> Error Success_ordinal_space_exhausted
;;

let target_binding_equal recorded current =
  String.equal
    recorded.binding_sha256
    (Resolver.target_identity_fingerprint current.target_identity)
;;

let prefer_last_good
      preferences
      (Flow_scope scope)
      ~(candidate_identity : _ -> flow_candidate_identity)
      candidates
  =
  match Flow_state.reserve_preference_scope preferences ~scope with
  | Error (Flow_state.Preference_capacity_exhausted { capacity }) ->
    Error (Preference_capacity_exhausted { capacity })
  | Error Flow_state.Preference_reservation_exhausted ->
    Error Preference_reservation_space_exhausted
  | Ok (reservation, None) -> Ok (candidates, No_preference_recorded, reservation)
  | Ok (reservation, Some (recorded, success_ordinal)) ->
    (match
       List.find_opt
         (fun candidate ->
            String.equal (candidate_identity candidate).candidate_id recorded.candidate_id)
         candidates
     with
     | None ->
       Ok
         ( candidates
         , Preference_not_applied
             { candidate = recorded
             ; success_ordinal
             ; reason = Preference_candidate_absent
             }
         , reservation )
     | Some current when not (target_binding_equal recorded (candidate_identity current))
       ->
       Ok
         ( candidates
         , Preference_not_applied
             { candidate = recorded
             ; success_ordinal
             ; reason = Preference_candidate_binding_changed
             }
         , reservation )
     | Some _ ->
       Ok
         ( Flow_state.promote_candidate
             ~equal:String.equal
             ~key:(fun candidate -> (candidate_identity candidate).candidate_id)
             ~preferred:(Some recorded.candidate_id)
             candidates
         , Preference_applied { candidate = recorded; success_ordinal }
         , reservation ))
;;

let domain_settlement_id_to_string (Domain_settlement_id id) = id
let domain_settlement_id_of_string id = Domain_settlement_id id

let make_domain_settlement_receipt ~settlement_id ~disposition =
  { settlement_id; disposition }
;;

let domain_settlement_receipt_id receipt = receipt.settlement_id
let domain_settlement_receipt_disposition receipt = receipt.disposition

let flow_disposition = function
  | Domain_valid -> Flow_state.Valid
  | Domain_rejected -> Flow_state.Rejected
;;

let flow_receipt receipt : Flow_state.domain_settlement_receipt =
  { settlement_id = domain_settlement_id_to_string receipt.settlement_id
  ; disposition = flow_disposition receipt.disposition
  }
;;

let begin_domain_settlement settlement preferences receipt =
  match
    Flow_state.begin_domain_settlement settlement preferences (flow_receipt receipt)
  with
  | Flow_state.Domain_settlement_claimed -> Domain_settlement_claimed
  | Flow_state.Domain_settlement_replayed _ -> Domain_settlement_replayed receipt
  | Flow_state.Domain_settlement_in_progress -> Domain_settlement_in_progress
  | Flow_state.Domain_settlement_conflict -> Domain_settlement_conflict
;;

let abort_domain_settlement settlement preferences receipt =
  Flow_state.abort_domain_settlement settlement preferences (flow_receipt receipt)
;;

let finish_domain_settlement
      settlement
      preferences
      (Flow_scope scope)
      ~reservation
      ~candidate
      ~success_ordinal
      receipt
  =
  match
    Flow_state.finish_domain_settlement
      settlement
      preferences
      ~scope
      ~reservation
      ~candidate:(flow_preference_identity_of_candidate candidate)
      ~ordinal:success_ordinal
      (flow_receipt receipt)
  with
  | Ok _ -> Ok receipt
  | Error Flow_state.Domain_settlement_apply_conflict ->
    Error Domain_settlement_apply_conflict
;;

let resume_committed_domain
      recovery
      ~restore_preference
      (Flow_scope scope)
      ~reservation
      ~candidate
      ~success_ordinal
      receipt
  =
  match
    Flow_state.resume_committed_domain
      recovery
      ~restore_preference
      ~scope
      ~reservation
      ~candidate
      ~ordinal:success_ordinal
      (flow_receipt receipt)
  with
  | Ok _ -> Ok receipt
  | Error Flow_state.Preference_recovery_finished ->
    Error Domain_preference_recovery_finished
  | Error (Flow_state.Preference_recovery_capacity_exhausted { capacity }) ->
    Error (Preference_recovery_capacity_exhausted { capacity })
  | Error Flow_state.Recovered_domain_conflict ->
    Error Domain_settlement_recovery_conflict
;;

let begin_flow_preference_retirement
      preferences
      (Flow_scope scope)
      ~make_receipt
  =
  match Flow_state.begin_preference_retirement preferences ~scope ~make_receipt with
  | Flow_state.Preference_retirement_claimed receipt ->
    Flow_preference_retirement_claimed receipt
  | Flow_state.Preference_retirement_replayed receipt ->
    Flow_preference_retirement_replayed receipt
  | Flow_state.Preference_retirement_in_progress ->
    Flow_preference_retirement_in_progress
  | Flow_state.Preference_retirement_not_reserved ->
    Flow_preference_scope_not_reserved
  | Flow_state.Preference_retirement_conflict ->
    Flow_preference_retirement_conflict
;;

let abort_flow_preference_retirement preferences (Flow_scope scope) receipt =
  Flow_state.abort_preference_retirement preferences ~scope receipt
;;

let mark_flow_preference_retirement_indeterminate
      preferences
      (Flow_scope scope)
      receipt
  =
  Flow_state.mark_preference_retirement_indeterminate preferences ~scope receipt
;;

let finish_flow_preference_retirement preferences (Flow_scope scope) receipt =
  match Flow_state.finish_preference_retirement preferences ~scope receipt with
  | Ok receipt -> Ok receipt
  | Error Flow_state.Preference_retirement_apply_conflict ->
    Error Flow_preference_retirement_apply_conflict
;;

let resume_committed_flow_preference_retirement
      recovery
      (Flow_scope scope)
      receipt
  =
  match Flow_state.resume_committed_retirement recovery ~scope receipt with
  | Ok receipt -> Ok receipt
  | Error Flow_state.Preference_retirement_recovery_finished ->
    Error Flow_preference_retirement_recovery_finished
  | Error Flow_state.Recovered_retirement_conflict ->
    Error Flow_preference_retirement_recovery_conflict
;;
