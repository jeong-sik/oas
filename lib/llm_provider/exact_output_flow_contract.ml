module Flow_state = Exact_output_flow
module Resolver = Exact_output_resolver

type flow_candidate_identity =
  { candidate_id : string
  ; catalog_generation : Resolver.catalog_generation
  ; catalog_evidence : Resolver.catalog_evidence
  ; target_identity : Resolver.target_identity
  }

type flow_preference_store = (string, flow_candidate_identity) Flow_state.preference_store
type flow_scope = Flow_scope of string
type flow_preference_reservation = Flow_state.preference_reservation
type flow_success_ordinal = Flow_state.success_ordinal
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

type domain_settlement_receipt =
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

let create_flow_preference_store ~capacity =
  match Flow_state.create_preference_store ~capacity with
  | Ok store -> Ok store
  | Error (Flow_state.Invalid_preference_capacity capacity) ->
    Error (Invalid_flow_preference_capacity capacity)
;;

let make_flow_scope ~id =
  let id = String.trim id in
  if String.equal id "" then Error Blank_flow_scope_id else Ok (Flow_scope id)
;;

let flow_scope_equal (Flow_scope left) (Flow_scope right) = String.equal left right
let flow_success_ordinal_to_int64 = Flow_state.success_ordinal_to_int64

let remove_flow_preference_scope preferences (Flow_scope scope) =
  match Flow_state.remove_preference_scope preferences ~scope with
  | Flow_state.Preference_scope_removed -> Flow_preference_scope_removed
  | Flow_state.Preference_scope_not_reserved -> Flow_preference_scope_not_reserved
;;

let allocate_flow_success_ordinal preferences =
  match Flow_state.allocate_success_ordinal preferences with
  | Ok ordinal -> Ok ordinal
  | Error Flow_state.Success_ordinal_exhausted -> Error Success_ordinal_space_exhausted
;;

let target_binding_equal left right =
  String.equal
    (Resolver.target_identity_fingerprint left.target_identity)
    (Resolver.target_identity_fingerprint right.target_identity)
;;

let prefer_last_good preferences (Flow_scope scope) ~candidate_identity candidates =
  match Flow_state.reserve_preference_scope preferences ~scope with
  | Error (Flow_state.Preference_capacity_exhausted { capacity }) ->
    Error (Preference_capacity_exhausted { capacity })
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

let settle_domain
      settlement
      preferences
      (Flow_scope scope)
      ~reservation
      ~candidate
      ~success_ordinal
      disposition
  =
  let result =
    match disposition with
    | Domain_rejected ->
      Flow_state.settle_domain_rejected_once settlement
      |> Result.map (fun () -> Domain_rejected_recorded)
    | Domain_valid ->
      Flow_state.settle_domain_valid_once
        settlement
        preferences
        ~scope
        ~reservation
        ~candidate
        ~ordinal:success_ordinal
      |> Result.map (function
        | Flow_state.Preference_installed ->
          Domain_valid_preference_installed { candidate; success_ordinal }
        | Flow_state.Preference_superseded { current_candidate; current_ordinal } ->
          Domain_valid_preference_superseded
            { current_candidate; current_success_ordinal = current_ordinal })
  in
  match result with
  | Error Flow_state.Already_settled -> Error Domain_already_settled
  | Error Flow_state.Preference_scope_released -> Error Domain_preference_scope_released
  | Ok receipt -> Ok receipt
;;
