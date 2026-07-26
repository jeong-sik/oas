module Flow_state = Exact_output_flow
module Flow_contract = Exact_output_flow_contract
module Generation_receipt = Exact_output_generation_receipt

type intent =
  { settlement_id : Flow_contract.domain_settlement_id
  ; flow_id : string
  ; scope : Flow_contract.flow_scope
  ; reservation : Flow_contract.flow_preference_reservation
  ; candidate : Flow_contract.flow_preference_identity
  ; success_ordinal : Flow_contract.flow_success_ordinal
  ; execution_evidence_sha256 : string
  ; disposition : Flow_contract.domain_disposition
  ; integrity_sha256 : string
  }

type decode_error =
  | Domain_settlement_intent_malformed_json of string
  | Domain_settlement_intent_invalid_fields
  | Domain_settlement_intent_unknown_format of string
  | Domain_settlement_intent_unsupported_version of int
  | Domain_settlement_intent_invalid_field of string
  | Domain_settlement_intent_integrity_mismatch

type 'commit_error commit_error =
  | Domain_commit_failed of 'commit_error
  | Domain_settlement_in_progress
  | Domain_settlement_conflict

type resume_error =
  | Domain_preference_recovery_finished
  | Preference_recovery_capacity_exhausted of { capacity : int }
  | Domain_settlement_recovery_conflict

let ( let* ) = Result.bind
let intent_format = "oas.exact-output.domain-settlement-intent"
let intent_version = 1
let sha256_string value = Digestif.SHA256.(to_hex (digest_string value))

let disposition_to_string = function
  | Flow_contract.Domain_valid -> "domain_valid"
  | Flow_contract.Domain_rejected -> "domain_rejected"
;;

let disposition_of_string = function
  | "domain_valid" -> Some Flow_contract.Domain_valid
  | "domain_rejected" -> Some Flow_contract.Domain_rejected
  | _ -> None
;;

let structural_json intent =
  `Assoc
    [ "format", `String intent_format
    ; "version", `Int intent_version
    ; "flow_id", `String intent.flow_id
    ; "scope", `String (Flow_contract.flow_scope_to_string intent.scope)
    ; ( "reservation_ordinal"
      , `String
          (Int64.to_string
             (Flow_contract.flow_preference_reservation_to_int64 intent.reservation)) )
    ; "candidate_id", `String intent.candidate.candidate_id
    ; "candidate_binding_sha256", `String intent.candidate.binding_sha256
    ; ( "success_ordinal"
      , `String
          (Int64.to_string
             (Flow_contract.flow_success_ordinal_to_int64 intent.success_ordinal)) )
    ; "execution_evidence_sha256", `String intent.execution_evidence_sha256
    ]
;;

let payload_json intent =
  match structural_json intent with
  | `Assoc fields ->
    `Assoc
      (fields
       @ [ ( "settlement_id"
           , `String (Flow_contract.domain_settlement_id_to_string intent.settlement_id) )
         ; "disposition", `String (disposition_to_string intent.disposition)
         ])
  | _ -> assert false
;;

let intent_to_string intent =
  match payload_json intent with
  | `Assoc fields ->
    Yojson.Safe.to_string
      (`Assoc (fields @ [ "integrity_sha256", `String intent.integrity_sha256 ]))
  | _ -> assert false
;;

let intent_id intent = intent.settlement_id

let effect_phase_to_string = function
  | Generation_receipt.Not_started -> "not_started"
  | Generation_receipt.Before_dispatch -> "before_dispatch"
  | Generation_receipt.Dispatch_started -> "dispatch_started"
  | Generation_receipt.Response_received -> "response_received"
  | Generation_receipt.Terminal -> "terminal"
;;

let call_id_to_string (Generation_receipt.Call_id id) = id

let make_intent
      ~flow_id
      ~scope
      ~reservation
      ~candidate
      ~success_ordinal
      ~execution
      disposition
  =
  let execution_evidence =
    `Assoc
      [ ( "call_id"
        , `String (Generation_receipt.snapshot_call_id execution |> call_id_to_string) )
      ; ( "plan_fingerprint"
        , `String (Generation_receipt.snapshot_plan_fingerprint execution) )
      ; ( "request_body_sha256"
        , `String (Generation_receipt.snapshot_request_body_sha256 execution) )
      ; ( "phase"
        , `String (Generation_receipt.snapshot_phase execution |> effect_phase_to_string)
        )
      ; "dispatch_count", `Int (Generation_receipt.snapshot_dispatch_count execution)
      ; ( "http_status"
        , match Generation_receipt.snapshot_http_status execution with
          | None -> `Null
          | Some status -> `Int status )
      ]
  in
  let provisional =
    { settlement_id = Flow_contract.domain_settlement_id_of_string (String.make 64 '0')
    ; flow_id
    ; scope
    ; reservation
    ; candidate = Flow_contract.flow_preference_identity_of_candidate candidate
    ; success_ordinal
    ; execution_evidence_sha256 = sha256_string (Yojson.Safe.to_string execution_evidence)
    ; disposition
    ; integrity_sha256 = String.make 64 '0'
    }
  in
  let settlement_id =
    structural_json provisional
    |> Yojson.Safe.to_string
    |> sha256_string
    |> Flow_contract.domain_settlement_id_of_string
  in
  let with_id = { provisional with settlement_id } in
  let integrity_sha256 = payload_json with_id |> Yojson.Safe.to_string |> sha256_string in
  { with_id with integrity_sha256 }
;;

let expected_fields =
  [ "format"
  ; "version"
  ; "flow_id"
  ; "scope"
  ; "reservation_ordinal"
  ; "candidate_id"
  ; "candidate_binding_sha256"
  ; "success_ordinal"
  ; "execution_evidence_sha256"
  ; "settlement_id"
  ; "disposition"
  ; "integrity_sha256"
  ]
;;

let is_sha256 value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' -> true
         | _ -> false)
       value
;;

let intent_of_string encoded =
  let invalid field = Error (Domain_settlement_intent_invalid_field field) in
  let find fields name =
    match List.assoc_opt name fields with
    | Some value -> Ok value
    | None -> invalid name
  in
  let string_field fields name =
    let* value = find fields name in
    match value with
    | `String value -> Ok value
    | _ -> invalid name
  in
  let int64_field fields name =
    let* raw = string_field fields name in
    match Int64.of_string_opt raw with
    | Some value
      when Int64.compare value 0L > 0 && String.equal raw (Int64.to_string value) ->
      Ok value
    | Some _ | None -> invalid name
  in
  try
    match Yojson.Safe.from_string encoded with
    | `Assoc fields ->
      let actual = List.map fst fields |> List.sort String.compare in
      let expected = List.sort String.compare expected_fields in
      if actual <> expected
      then Error Domain_settlement_intent_invalid_fields
      else
        let* format = string_field fields "format" in
        let* () =
          if String.equal format intent_format
          then Ok ()
          else Error (Domain_settlement_intent_unknown_format format)
        in
        let* version = find fields "version" in
        let* () =
          match version with
          | `Int version when version = intent_version -> Ok ()
          | `Int version -> Error (Domain_settlement_intent_unsupported_version version)
          | _ -> invalid "version"
        in
        let* flow_id = string_field fields "flow_id" in
        let* scope_text = string_field fields "scope" in
        let* scope =
          match Flow_contract.make_flow_scope ~id:scope_text with
          | Ok scope
            when String.equal scope_text (Flow_contract.flow_scope_to_string scope) ->
            Ok scope
          | Ok _ | Error _ -> invalid "scope"
        in
        let* reservation_ordinal = int64_field fields "reservation_ordinal" in
        let* reservation =
          match
            Flow_contract.flow_preference_reservation_of_int64 reservation_ordinal
          with
          | Some reservation -> Ok reservation
          | None -> invalid "reservation_ordinal"
        in
        let* candidate_id = string_field fields "candidate_id" in
        let* candidate_binding_sha256 = string_field fields "candidate_binding_sha256" in
        let* () =
          if
            (not (String.equal (String.trim candidate_id) ""))
            && is_sha256 candidate_binding_sha256
          then Ok ()
          else invalid "candidate"
        in
        let candidate =
          Flow_contract.make_flow_preference_identity
            ~candidate_id
            ~binding_sha256:candidate_binding_sha256
        in
        let* success_ordinal_raw = int64_field fields "success_ordinal" in
        let* success_ordinal =
          match Flow_contract.flow_success_ordinal_of_int64 success_ordinal_raw with
          | Some ordinal -> Ok ordinal
          | None -> invalid "success_ordinal"
        in
        let* execution_evidence_sha256 =
          string_field fields "execution_evidence_sha256"
        in
        let* settlement_id_text = string_field fields "settlement_id" in
        let* disposition_text = string_field fields "disposition" in
        let* disposition =
          match disposition_of_string disposition_text with
          | Some disposition -> Ok disposition
          | None -> invalid "disposition"
        in
        let* integrity_sha256 = string_field fields "integrity_sha256" in
        let* () =
          if
            is_sha256 execution_evidence_sha256
            && is_sha256 settlement_id_text
            && is_sha256 integrity_sha256
          then Ok ()
          else invalid "sha256"
        in
        let intent =
          { settlement_id =
              Flow_contract.domain_settlement_id_of_string settlement_id_text
          ; flow_id
          ; scope
          ; reservation
          ; candidate
          ; success_ordinal
          ; execution_evidence_sha256
          ; disposition
          ; integrity_sha256
          }
        in
        let expected_settlement_id =
          structural_json intent |> Yojson.Safe.to_string |> sha256_string
        in
        let expected_integrity =
          payload_json intent |> Yojson.Safe.to_string |> sha256_string
        in
        if
          String.equal expected_settlement_id settlement_id_text
          && String.equal expected_integrity integrity_sha256
        then Ok intent
        else Error Domain_settlement_intent_integrity_mismatch
    | _ -> Error Domain_settlement_intent_invalid_fields
  with
  | Yojson.Json_error detail -> Error (Domain_settlement_intent_malformed_json detail)
;;

let commit_and_settle
      ~commit
      ~domain_settlement
      ~preferences
      ~scope
      ~reservation
      ~candidate
      ~success_ordinal
      ~flow_id
      ~execution
      disposition
  =
  let intent =
    make_intent
      ~flow_id
      ~scope
      ~reservation
      ~candidate
      ~success_ordinal
      ~execution
      disposition
  in
  let receipt =
    Flow_contract.make_domain_settlement_receipt
      ~settlement_id:intent.settlement_id
      ~disposition
  in
  match Flow_contract.begin_domain_settlement domain_settlement preferences receipt with
  | Flow_contract.Domain_settlement_replayed receipt -> Ok receipt
  | Flow_contract.Domain_settlement_in_progress -> Error Domain_settlement_in_progress
  | Flow_contract.Domain_settlement_conflict -> Error Domain_settlement_conflict
  | Flow_contract.Domain_settlement_claimed ->
    let finished = ref false in
    Fun.protect
      ~finally:(fun () ->
        if not !finished
        then Flow_contract.abort_domain_settlement domain_settlement preferences receipt)
      (fun () ->
         match commit intent with
         | Error cause -> Error (Domain_commit_failed cause)
         | Ok () ->
           (match
              Flow_contract.finish_domain_settlement
                domain_settlement
                preferences
                scope
                ~reservation
                ~candidate
                ~success_ordinal
                receipt
            with
            | Error Flow_contract.Domain_settlement_apply_conflict ->
              Error Domain_settlement_conflict
            | Ok receipt ->
              finished := true;
              Ok receipt))
;;

let resume recovery intent =
  let receipt =
    Flow_contract.make_domain_settlement_receipt
      ~settlement_id:intent.settlement_id
      ~disposition:intent.disposition
  in
  match
    Flow_contract.resume_committed_domain
      recovery
      intent.scope
      ~reservation:intent.reservation
      ~candidate:intent.candidate
      ~success_ordinal:intent.success_ordinal
      receipt
  with
  | Ok receipt -> Ok receipt
  | Error Flow_contract.Domain_preference_recovery_finished ->
    Error Domain_preference_recovery_finished
  | Error (Flow_contract.Preference_recovery_capacity_exhausted { capacity }) ->
    Error (Preference_recovery_capacity_exhausted { capacity })
  | Error Flow_contract.Domain_settlement_recovery_conflict ->
    Error Domain_settlement_recovery_conflict
;;
