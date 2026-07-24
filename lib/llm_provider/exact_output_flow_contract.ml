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

let create_flow_preference_store () = Flow_state.create_preference_store ()

let make_flow_scope ~id =
  let id = String.trim id in
  if String.equal id "" then Error Blank_flow_scope_id else Ok (Flow_scope id)
;;

let flow_scope_equal (Flow_scope left) (Flow_scope right) = String.equal left right

let target_binding_equal left right =
  String.equal
    (Resolver.target_identity_fingerprint left.target_identity)
    (Resolver.target_identity_fingerprint right.target_identity)
;;

let prefer_last_good preferences (Flow_scope scope) ~candidate_identity candidates =
  match Flow_state.preferred_candidate preferences ~scope with
  | None -> candidates, No_preference_recorded
  | Some (recorded, success_time_unix_s) ->
    (match
       List.find_opt
         (fun candidate ->
            String.equal (candidate_identity candidate).candidate_id recorded.candidate_id)
         candidates
     with
     | None ->
       ( candidates
       , Preference_not_applied
           { candidate = recorded
           ; success_time_unix_s
           ; reason = Preference_candidate_absent
           } )
     | Some current when not (target_binding_equal recorded (candidate_identity current))
       ->
       ( candidates
       , Preference_not_applied
           { candidate = recorded
           ; success_time_unix_s
           ; reason = Preference_candidate_binding_changed
           } )
     | Some _ ->
       ( Flow_state.promote_candidate
           ~equal:String.equal
           ~key:(fun candidate -> (candidate_identity candidate).candidate_id)
           ~preferred:(Some recorded.candidate_id)
           candidates
       , Preference_applied { candidate = recorded; success_time_unix_s } ))
;;

let settle_domain settlement preferences (Flow_scope scope) ~candidate disposition =
  let result =
    match disposition with
    | Domain_rejected ->
      Flow_state.settle_domain_rejected_once settlement
      |> Result.map (fun () -> Domain_rejected_recorded)
    | Domain_valid { success_time_unix_s } ->
      Flow_state.settle_domain_valid_once
        settlement
        preferences
        ~scope
        ~candidate
        ~time:success_time_unix_s
      |> Result.map (function
        | Flow_state.Preference_installed ->
          Domain_valid_preference_installed { candidate; success_time_unix_s }
        | Flow_state.Preference_superseded { current_candidate; current_time } ->
          Domain_valid_preference_superseded
            { current_candidate; current_success_time_unix_s = current_time })
  in
  match result with
  | Error Flow_state.Already_settled -> Error Domain_already_settled
  | Ok receipt -> Ok receipt
;;
