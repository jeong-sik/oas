module Domain_settlement = Exact_output_domain_settlement
module Flow_contract = Exact_output_flow_contract
module Scope_retirement = Exact_output_scope_retirement
module String_map = Map.Make (String)
module String_set = Set.Make (String)

type evidence =
  | Domain_settlement_evidence of Domain_settlement.intent
  | Scope_retirement_evidence of Scope_retirement.intent

type error =
  | Invalid_concurrent_scope_budget of int
  | Conflicting_domain_settlement_evidence of Flow_contract.domain_settlement_id
  | Conflicting_scope_retirement_evidence of Scope_retirement.id

type domain_item =
  { intent : Domain_settlement.intent
  ; encoded : string
  ; evidence : Domain_settlement.recovery_evidence
  }

type retirement_item =
  { intent : Scope_retirement.intent
  ; encoded : string
  ; evidence : Scope_retirement.recovery_evidence
  }

let ( let* ) = Result.bind

let collect evidence =
  let rec loop
            (domains : domain_item String_map.t)
            (retirements : retirement_item String_map.t)
    = function
    | [] -> Ok (domains, retirements)
    | Domain_settlement_evidence intent :: rest ->
      let id = Domain_settlement.intent_id intent in
      let key = Flow_contract.domain_settlement_id_to_string id in
      let encoded = Domain_settlement.intent_to_string intent in
      (match String_map.find_opt key domains with
       | Some existing when String.equal existing.encoded encoded ->
         loop domains retirements rest
       | Some _ -> Error (Conflicting_domain_settlement_evidence id)
       | None ->
         let item : domain_item =
           { intent; encoded; evidence = Domain_settlement.recovery_evidence intent }
         in
         loop (String_map.add key item domains) retirements rest)
    | Scope_retirement_evidence intent :: rest ->
      let id = Scope_retirement.intent_id intent in
      let key = Scope_retirement.id_to_string id in
      let encoded = Scope_retirement.intent_to_string intent in
      (match String_map.find_opt key retirements with
       | Some existing when String.equal existing.encoded encoded ->
         loop domains retirements rest
       | Some _ -> Error (Conflicting_scope_retirement_evidence id)
       | None ->
         let item : retirement_item =
           { intent; encoded; evidence = Scope_retirement.recovery_evidence intent }
         in
         loop domains (String_map.add key item retirements) rest)
  in
  loop
    (String_map.empty : domain_item String_map.t)
    (String_map.empty : retirement_item String_map.t)
    evidence
;;

let latest_retirements retirements =
  String_map.fold
    (fun _ item accumulated ->
       let* by_scope = accumulated in
       let scope = Flow_contract.flow_scope_to_string item.evidence.scope in
       match String_map.find_opt scope by_scope with
       | None -> Ok (String_map.add scope item by_scope)
       | Some current ->
         let incoming =
           Flow_contract.flow_preference_reservation_to_int64 item.evidence.reservation
         in
         let existing =
           Flow_contract.flow_preference_reservation_to_int64 current.evidence.reservation
         in
         if Int64.compare incoming existing > 0
         then Ok (String_map.add scope item by_scope)
         else if Int64.compare incoming existing < 0
         then Ok by_scope
         else if
           String.equal
             (Scope_retirement.id_to_string item.evidence.retirement_id)
             (Scope_retirement.id_to_string current.evidence.retirement_id)
         then Ok by_scope
         else Error (Conflicting_scope_retirement_evidence item.evidence.retirement_id))
    retirements
    (Ok String_map.empty)
;;

let settlement_survives_retirement latest item =
  let scope = Flow_contract.flow_scope_to_string item.evidence.scope in
  match String_map.find_opt scope latest with
  | None -> true
  | Some retirement ->
    Int64.compare
      (Flow_contract.flow_preference_reservation_to_int64 item.evidence.reservation)
      (Flow_contract.flow_preference_reservation_to_int64 retirement.evidence.reservation)
    > 0
;;

let active_scope_count domains latest =
  String_map.fold
    (fun _ item active ->
       match item.evidence.disposition with
       | Flow_contract.Domain_rejected -> active
       | Flow_contract.Domain_valid ->
         if settlement_survives_retirement latest item
         then
           String_set.add (Flow_contract.flow_scope_to_string item.evidence.scope) active
         else active)
    domains
    String_set.empty
  |> String_set.cardinal
;;

let recover ~concurrent_scope_budget ~evidence =
  if concurrent_scope_budget < 0
  then Error (Invalid_concurrent_scope_budget concurrent_scope_budget)
  else
    let* domains, retirements = collect evidence in
    let* latest = latest_retirements retirements in
    let capacity = Int.max concurrent_scope_budget (active_scope_count domains latest) in
    let recovery = Flow_contract.create_validated_flow_preference_recovery ~capacity in
    let* () =
      String_map.fold
        (fun _ item resumed ->
           let* () = resumed in
           match Scope_retirement.resume recovery item.intent with
           | Ok _ -> Ok ()
           | Error _ ->
             Error (Conflicting_scope_retirement_evidence item.evidence.retirement_id))
        latest
        (Ok ())
    in
    let* () =
      String_map.fold
        (fun _ item resumed ->
           let* () = resumed in
           let restore_preference =
             match item.evidence.disposition with
             | Flow_contract.Domain_rejected -> false
             | Flow_contract.Domain_valid -> settlement_survives_retirement latest item
           in
           match Domain_settlement.resume recovery ~restore_preference item.intent with
           | Ok _ -> Ok ()
           | Error _ ->
             Error
               (Conflicting_domain_settlement_evidence
                  (Domain_settlement.intent_id item.intent)))
        domains
        (Ok ())
    in
    Ok (Flow_contract.activate_validated_flow_preference_recovery recovery)
;;
