module Flow_contract = Exact_output_flow_contract

type id = Flow_preference_retirement_id of string

type intent =
  { retirement_id : id
  ; scope : Flow_contract.flow_scope
  ; reservation : Flow_contract.flow_preference_reservation
  ; success_high_water : int64
  ; integrity_sha256 : string
  }

type receipt = { retirement_id : id }

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
  ; scope : Flow_contract.flow_scope
  ; reservation : Flow_contract.flow_preference_reservation
  ; success_high_water : int64
  }

let ( let* ) = Result.bind
let intent_format = "oas.exact-output.flow-preference-retirement-intent"
let intent_version = 1
let sha256_string value = Digestif.SHA256.(to_hex (digest_string value))
let id_to_string (Flow_preference_retirement_id id) = id
let intent_id (intent : intent) = intent.retirement_id
let receipt_id (receipt : receipt) = receipt.retirement_id

let structural_fields (intent : intent) =
  [ "format", `String intent_format
  ; "version", `Int intent_version
  ; "scope", `String (Flow_contract.flow_scope_to_string intent.scope)
  ; ( "reservation_ordinal"
    , `String
        (Int64.to_string
           (Flow_contract.flow_preference_reservation_to_int64 intent.reservation)) )
  ; "success_high_water", `String (Int64.to_string intent.success_high_water)
  ]
;;

let structural_json intent = `Assoc (structural_fields intent)

let payload_fields (intent : intent) =
  structural_fields intent
  @ [ "retirement_id", `String (id_to_string intent.retirement_id) ]
;;

let payload_json (intent : intent) = `Assoc (payload_fields intent)

let intent_to_string (intent : intent) =
  Yojson.Safe.to_string
    (`Assoc
        (payload_fields intent @ [ "integrity_sha256", `String intent.integrity_sha256 ]))
;;

let make_intent ~scope ~reservation ~success_high_water =
  let provisional =
    { retirement_id = Flow_preference_retirement_id (String.make 64 '0')
    ; scope
    ; reservation
    ; success_high_water
    ; integrity_sha256 = String.make 64 '0'
    }
  in
  let retirement_id =
    structural_json provisional
    |> Yojson.Safe.to_string
    |> sha256_string
    |> fun id -> Flow_preference_retirement_id id
  in
  let with_id = { provisional with retirement_id } in
  let integrity_sha256 = payload_json with_id |> Yojson.Safe.to_string |> sha256_string in
  { with_id with integrity_sha256 }
;;

let expected_fields =
  [ "format"
  ; "version"
  ; "scope"
  ; "reservation_ordinal"
  ; "success_high_water"
  ; "retirement_id"
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
  let invalid field = Error (Flow_preference_retirement_intent_invalid_field field) in
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
  let canonical_int64_field fields name ~allow_zero =
    let* raw = string_field fields name in
    match Int64.of_string_opt raw with
    | Some value
      when (if allow_zero then Int64.compare value 0L >= 0 else Int64.compare value 0L > 0)
           && String.equal raw (Int64.to_string value) -> Ok value
    | Some _ | None -> invalid name
  in
  try
    match Yojson.Safe.from_string encoded with
    | `Assoc fields ->
      let actual = List.map fst fields |> List.sort String.compare in
      let expected = List.sort String.compare expected_fields in
      if actual <> expected
      then Error Flow_preference_retirement_intent_invalid_fields
      else
        let* format = string_field fields "format" in
        let* () =
          if String.equal format intent_format
          then Ok ()
          else Error (Flow_preference_retirement_intent_unknown_format format)
        in
        let* version = find fields "version" in
        let* () =
          match version with
          | `Int version when version = intent_version -> Ok ()
          | `Int version ->
            Error (Flow_preference_retirement_intent_unsupported_version version)
          | _ -> invalid "version"
        in
        let* scope_text = string_field fields "scope" in
        let* scope =
          match Flow_contract.make_flow_scope ~id:scope_text with
          | Ok scope
            when String.equal scope_text (Flow_contract.flow_scope_to_string scope) ->
            Ok scope
          | Ok _ | Error _ -> invalid "scope"
        in
        let* reservation_ordinal =
          canonical_int64_field fields "reservation_ordinal" ~allow_zero:false
        in
        let* reservation =
          match
            Flow_contract.flow_preference_reservation_of_int64 reservation_ordinal
          with
          | Some reservation -> Ok reservation
          | None -> invalid "reservation_ordinal"
        in
        let* success_high_water =
          canonical_int64_field fields "success_high_water" ~allow_zero:true
        in
        let* retirement_id_text = string_field fields "retirement_id" in
        let* integrity_sha256 = string_field fields "integrity_sha256" in
        let* () =
          if is_sha256 retirement_id_text && is_sha256 integrity_sha256
          then Ok ()
          else invalid "sha256"
        in
        let intent =
          { retirement_id = Flow_preference_retirement_id retirement_id_text
          ; scope
          ; reservation
          ; success_high_water
          ; integrity_sha256
          }
        in
        let expected_id =
          structural_json intent |> Yojson.Safe.to_string |> sha256_string
        in
        let expected_integrity =
          payload_json intent |> Yojson.Safe.to_string |> sha256_string
        in
        if
          String.equal expected_id retirement_id_text
          && String.equal expected_integrity integrity_sha256
        then Ok intent
        else Error Flow_preference_retirement_intent_integrity_mismatch
    | _ -> Error Flow_preference_retirement_intent_invalid_fields
  with
  | Yojson.Json_error detail ->
    Error (Flow_preference_retirement_intent_malformed_json detail)
;;

let recovery_evidence (intent : intent) =
  { retirement_id = intent.retirement_id
  ; scope = intent.scope
  ; reservation = intent.reservation
  ; success_high_water = intent.success_high_water
  }
;;

let flow_receipt (intent : intent) : Flow_contract.flow_preference_retirement_receipt =
  { retirement_id = id_to_string intent.retirement_id
  ; reservation = intent.reservation
  ; success_high_water = intent.success_high_water
  }
;;

let public_receipt (flow_receipt : Flow_contract.flow_preference_retirement_receipt) =
  { retirement_id = Flow_preference_retirement_id flow_receipt.retirement_id }
;;

let commit_and_retire ~commit preferences scope =
  let make_receipt ~reservation ~success_high_water =
    make_intent ~scope ~reservation ~success_high_water |> flow_receipt
  in
  match
    Flow_contract.begin_flow_preference_retirement preferences scope ~make_receipt
  with
  | Flow_contract.Flow_preference_retirement_replayed receipt ->
    Ok (public_receipt receipt)
  | Flow_contract.Flow_preference_retirement_in_progress ->
    Error Flow_preference_retirement_in_progress
  | Flow_contract.Flow_preference_retirement_conflict ->
    Error Flow_preference_retirement_conflict
  | Flow_contract.Flow_preference_scope_not_reserved ->
    Error Flow_preference_scope_not_reserved
  | Flow_contract.Flow_preference_retirement_claimed requested ->
    let intent =
      make_intent
        ~scope
        ~reservation:requested.reservation
        ~success_high_water:requested.success_high_water
    in
    (match commit intent with
     | Error cause ->
       Flow_contract.mark_flow_preference_retirement_indeterminate
         preferences
         scope
         requested;
       Error (Flow_preference_retirement_commit_failed cause)
     | Ok () ->
       (match
          Flow_contract.finish_flow_preference_retirement preferences scope requested
        with
        | Error Flow_contract.Flow_preference_retirement_apply_conflict ->
          Flow_contract.mark_flow_preference_retirement_indeterminate
            preferences
            scope
            requested;
          Error Flow_preference_retirement_conflict
        | Ok receipt -> Ok (public_receipt receipt))
     | exception exn ->
       let raw_backtrace = Printexc.get_raw_backtrace () in
       Flow_contract.mark_flow_preference_retirement_indeterminate
         preferences
         scope
         requested;
       Printexc.raise_with_backtrace exn raw_backtrace)
;;

let resume (recovery : Flow_contract.flow_preference_recovery) (intent : intent) =
  match
    Flow_contract.resume_committed_flow_preference_retirement
      recovery
      intent.scope
      (flow_receipt intent)
  with
  | Ok receipt -> Ok (public_receipt receipt)
  | Error Flow_contract.Flow_preference_retirement_recovery_finished ->
    Error Flow_preference_retirement_recovery_finished
  | Error Flow_contract.Flow_preference_retirement_recovery_conflict ->
    Error Flow_preference_retirement_recovery_conflict
;;
