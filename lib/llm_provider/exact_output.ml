module Plan = Exact_output_plan
module Flow_admission = Exact_output_flow_admission

type measurement_dispatch_fact = Flow_admission.measurement_dispatch_fact =
  | No_measurement_dispatch
  | Measurement_dispatch_unknown
  | Measurement_dispatch_started

type measurement_outcome = Flow_admission.measurement_outcome =
  | Measurement_not_required
  | Measurement_succeeded
  | Measurement_unsupported
  | Measurement_local_invalid
  | Measurement_transport_failed
  | Measurement_invalid_response
  | Measurement_fence_rejected
  | Measurement_cancelled

type measurement_evidence = Flow_admission.measurement_evidence =
  { dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome
  }

type measurement_operation_id = Flow_admission.measurement_operation_id

type measurement_receipt_phase = Flow_admission.measurement_receipt_phase =
  | Measurement_fence_committed
  | Measurement_wire_started
  | Measurement_terminal

module Exec = Exact_output_execution
module Flow_state = Exact_output_flow
module Flow_contract = Exact_output_flow_contract
module Trace = Exact_output_provider_trace
module Generation_receipt = Exact_output_generation_receipt
include Exact_output_resolver
include Flow_contract
include Exact_output_ready_admission

let plan_provenance_source_schema_fingerprint (provenance : plan_provenance) =
  provenance.source_schema_fingerprint
;;

let plan_provenance_effective_schema_fingerprint (provenance : plan_provenance) =
  provenance.effective_schema_fingerprint
;;

let plan_provenance_actual_assurance (provenance : plan_provenance) =
  provenance.actual_assurance
;;

let plan_provenance_catalog_generation (provenance : plan_provenance) =
  provenance.catalog_generation
;;

let plan_provenance_catalog_evidence (provenance : plan_provenance) =
  provenance.catalog_evidence
;;

let plan_provenance_target_identity (provenance : plan_provenance) =
  provenance.target_identity
;;

type call_id = Generation_receipt.call_id = Call_id of string
type provider_trace = Trace.t
type receipt = Generation_receipt.t

type attempt =
  { ready : ready_plan
  ; receipt : receipt
  }

type input_capacity_disposition =
  | Token_measurement_required of
      { accepted_through_tokens : int
      ; rejected_from_tokens : int option
      }
  | Context_window_exceeded of
      { input_tokens : int
      ; reserved_output_tokens : int
      ; max_context_tokens : int
      }
  | Token_capacity_rejected of token_capacity_rejection
  | Serialized_request_body_too_large of
      { actual_bytes : int
      ; limit_bytes : int
      }

type candidate_rejection_disposition =
  | Runtime_slot_unavailable
  | Runtime_contract_rejected
  | Input_contract_rejected
  | Output_requirement_rejected
  | Input_capacity of input_capacity_disposition
  | Request_preparation_failed

type effect_phase = Generation_receipt.effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type generation_receipt_snapshot = Generation_receipt.snapshot

type raw_response = Trace.raw_response =
  { body : string
  ; body_sha256 : string
  }

type execution_error_cause =
  | Attempt_already_started
  | Clock_required_for_timeout
  | Frozen_request_mismatch
  | Completion_failed
  | Incomplete_output
  | Missing_output
  | Ambiguous_output of int
  | Unexpected_output_content
  | Invalid_json_output
  | Internal_non_json_output

type execution_error =
  { call_id : call_id
  ; receipt : receipt
  ; cause : execution_error_cause
  ; raw_response : raw_response option
  }

type success =
  { call_id : call_id
  ; receipt : receipt
  ; output : Yojson.Safe.t
  ; provenance : plan_provenance
  ; raw_response : raw_response
  }

type flow_candidate =
  { identity : flow_candidate_identity
  ; admitted_target : admitted_target
  }

type flow_id = Flow_id of string
type flow_visit_ordinal = Flow_visit_ordinal of int
type candidate_visit_count = Candidate_visit_count of int

type flow_candidate_visit =
  { flow_id : flow_id
  ; ordinal : flow_visit_ordinal
  ; identity : flow_candidate_identity
  }

type flow_measurement_receipt =
  { visit : flow_candidate_visit
  ; receipt : Flow_admission.measurement_receipt
  }

type measurement_receipt_snapshot =
  { operation_id : measurement_operation_id
  ; visit : flow_candidate_visit
  ; request_body_sha256 : string
  ; phase : measurement_receipt_phase
  ; dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome option
  }

type flow_candidate_step =
  { visit : flow_candidate_visit
  ; admitted_target : admitted_target
  }

type candidate_rejection_cause =
  | Target_selection_rejected of target_selection_error
  | Request_admission_rejected of admission_error

type candidate_rejection_receipt =
  { scope : flow_scope
  ; visit : flow_candidate_visit
  ; cause : candidate_rejection_cause
  ; measurement : measurement_evidence
  }

type admitted_flow_candidate =
  { visit : flow_candidate_visit
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; provenance : plan_provenance
  ; measurement : measurement_evidence
  }

type candidate_admission =
  | Candidate_admitted of admitted_flow_candidate
  | Candidate_rejected of candidate_rejection_receipt

type flow_snapshot =
  { preferences : flow_preference_store
  ; scope : flow_scope
  ; preference_reservation : Flow_contract.flow_preference_reservation
  ; declared_candidate_snapshot : flow_candidate_identity list
  ; preference_observation : flow_preference_observation
  ; candidates : flow_candidate list
  ; messages : Types.message list
  ; requirement : output_requirement
  }

type flow_attempt_receipt =
  { scope : flow_scope
  ; visit : flow_candidate_visit
  ; receipt : receipt
  }

type flow_attempt_snapshot =
  { scope : flow_scope
  ; visit : flow_candidate_visit
  ; receipt : generation_receipt_snapshot
  }

type flow_attempt_publication =
  { call_id : call_id
  ; snapshot : flow_attempt_snapshot
  }

type flow_attempt =
  { execution : Flow_state.t
  ; flow_id : flow_id
  ; preferences : flow_preference_store
  ; scope : flow_scope
  ; preference_reservation : Flow_contract.flow_preference_reservation
  ; declared_candidate_snapshot : flow_candidate_identity list
  ; candidate_snapshot : flow_candidate_identity list
  ; preference_observation : flow_preference_observation
  ; candidates : flow_candidate_step list
  ; messages : Types.message list
  ; requirement : output_requirement
  ; progress :
      ( candidate_admission
        , flow_attempt_publication
        , measurement_receipt_snapshot )
        Flow_state.progress
  }

type flow_candidate_error = Blank_flow_candidate_id

type flow_snapshot_error =
  | Duplicate_flow_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }
  | Flow_preference_capacity_exhausted of { capacity : int }
  | Flow_preference_reservation_exhausted

type start_attempt_error = Call_id_generation_failed of string

type measurement_start_error =
  | Measurement_operation_id_generation_failed of string
  | Measurement_clock_required_for_timeout

type flow_start_error = Flow_id_generation_failed of string

type flow_evidence =
  { flow_id : flow_id
  ; scope : flow_scope
  ; declared_candidate_snapshot : flow_candidate_identity list
  ; candidate_snapshot : flow_candidate_identity list
  ; preference_observation : flow_preference_observation
  ; candidate_visit_count : candidate_visit_count
  ; measurements : measurement_receipt_snapshot list
  ; admissions : candidate_admission list
  ; attempts : flow_attempt_snapshot list
  }

type flow_success =
  { candidate : flow_attempt_receipt
  ; success : success
  ; success_ordinal : flow_success_ordinal
  ; evidence : flow_evidence
  ; domain_settlement : Flow_state.domain_settlement
  ; preferences : flow_preference_store
  ; scope : flow_scope
  ; preference_reservation : Flow_contract.flow_preference_reservation
  }

type domain_settlement_intent =
  { settlement_id : domain_settlement_id
  ; flow_id : string
  ; scope : flow_scope
  ; reservation : Flow_contract.flow_preference_reservation
  ; candidate : flow_preference_identity
  ; success_ordinal : flow_success_ordinal
  ; execution_evidence_sha256 : string
  ; disposition : domain_disposition
  ; integrity_sha256 : string
  }

type domain_settlement_intent_decode_error =
  | Domain_settlement_intent_malformed_json of string
  | Domain_settlement_intent_invalid_fields
  | Domain_settlement_intent_unknown_format of string
  | Domain_settlement_intent_unsupported_version of int
  | Domain_settlement_intent_invalid_field of string
  | Domain_settlement_intent_integrity_mismatch

type 'commit_error domain_commit_error =
  | Domain_commit_failed of 'commit_error
  | Domain_settlement_conflict

type domain_settlement_resume_error =
  | Domain_preference_recovery_finished
  | Domain_settlement_recovery_conflict

type flow_candidate_failure =
  | Flow_candidate_rejected of candidate_rejection_receipt
  | Flow_candidate_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      }

type generation_dispatch_fact =
  | No_generation_dispatch
  | Generation_dispatch_started

type 'callback_error flow_execution_error =
  | Flow_attempt_already_started of flow_evidence
  | Flow_success_ordinal_exhausted of flow_evidence
  | Flow_attempt_start_failed of
      { candidate : flow_candidate_visit
      ; cause : start_attempt_error
      ; evidence : flow_evidence
      }
  | Flow_measurement_start_failed of
      { candidate : flow_candidate_visit
      ; cause : measurement_start_error
      ; evidence : flow_evidence
      }
  | Flow_before_measurement_dispatch_callback_failed of
      { measurement : flow_measurement_receipt
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_measurement_terminal_callback_failed of
      { measurement : flow_measurement_receipt
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_before_dispatch_callback_failed of
      { candidate : flow_attempt_receipt
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_before_advance_callback_failed of
      { failed : flow_candidate_failure
      ; next : flow_candidate_visit
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_candidates_exhausted of
      { rejection : candidate_rejection_receipt
      ; evidence : flow_evidence
      }
  | Flow_exact_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      ; evidence : flow_evidence
      }

type 'callback_error flow_step_failure =
  | Flow_step_candidate_rejected of candidate_rejection_receipt
  | Flow_step_attempt_start_failed of flow_candidate_visit * start_attempt_error
  | Flow_step_measurement_start_failed of flow_candidate_visit * measurement_start_error
  | Flow_step_before_measurement_dispatch_callback_failed of
      flow_measurement_receipt * 'callback_error
  | Flow_step_measurement_terminal_callback_failed of
      flow_measurement_receipt * 'callback_error
  | Flow_step_before_dispatch_callback_failed of flow_attempt_receipt * 'callback_error
  | Flow_step_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      }

let ( let* ) = Result.bind

let make_flow_candidate ~id ~admitted_target =
  let id = String.trim id in
  if String.equal id ""
  then Error Blank_flow_candidate_id
  else
    Ok
      { identity =
          { candidate_id = id
          ; catalog_generation = admitted_target_catalog_generation admitted_target
          ; catalog_evidence = admitted_target_catalog_evidence admitted_target
          ; target_identity = admitted_target_identity admitted_target
          }
      ; admitted_target
      }
;;

let flow_candidate_identity (candidate : flow_candidate) = candidate.identity

let snapshot_flow ~preferences ~scope ~first ~rest ~messages requirement =
  let candidates = first :: rest in
  match
    Flow_state.duplicate_key
      ~equal:String.equal
      ~key:(fun (candidate : flow_candidate) -> candidate.identity.candidate_id)
      candidates
  with
  | Some (candidate_id, first_position, duplicate_position) ->
    Error
      (Duplicate_flow_candidate_id { candidate_id; first_position; duplicate_position })
  | None ->
    let declared_candidate_snapshot = List.map flow_candidate_identity candidates in
    (match
       Flow_contract.prefer_last_good
         preferences
         scope
         ~candidate_identity:flow_candidate_identity
         candidates
     with
     | Error (Preference_capacity_exhausted { capacity }) ->
       Error (Flow_preference_capacity_exhausted { capacity })
     | Error Preference_reservation_space_exhausted ->
       Error Flow_preference_reservation_exhausted
     | Ok (candidates, preference_observation, preference_reservation) ->
       Ok
         { preferences
         ; scope
         ; preference_reservation
         ; declared_candidate_snapshot
         ; preference_observation
         ; candidates
         ; messages
         ; requirement
         })
;;

let start_attempt (ready : ready_plan) =
  match Exact_output_call_id.create () with
  | Error detail -> Error (Call_id_generation_failed detail)
  | Ok id ->
    let receipt =
      Generation_receipt.create
        ~call_id:(Call_id id)
        ~plan_fingerprint:ready.plan_fingerprint
        ~request_body_sha256:ready.request_body_sha256
        ~catalog_generation:ready.catalog_generation
        ~catalog_evidence:ready.catalog_evidence
        ~target_identity:ready.target_identity
    in
    Ok { ready; receipt }
;;

let start_flow (ready : flow_snapshot) =
  match Exact_output_call_id.create () with
  | Error detail -> Error (Flow_id_generation_failed detail)
  | Ok raw_flow_id ->
    let flow_id = Flow_id raw_flow_id in
    let candidates =
      List.mapi
        (fun index (candidate : flow_candidate) ->
           { visit =
               { flow_id
               ; ordinal = Flow_visit_ordinal (index + 1)
               ; identity = candidate.identity
               }
           ; admitted_target = candidate.admitted_target
           })
        ready.candidates
    in
    Ok
      { execution = Flow_state.create ()
      ; flow_id
      ; preferences = ready.preferences
      ; scope = ready.scope
      ; preference_reservation = ready.preference_reservation
      ; declared_candidate_snapshot = ready.declared_candidate_snapshot
      ; candidate_snapshot = List.map flow_candidate_identity ready.candidates
      ; preference_observation = ready.preference_observation
      ; candidates
      ; messages = ready.messages
      ; requirement = ready.requirement
      ; progress = Flow_state.create_progress ()
      }
;;

let flow_success_candidate success = success.candidate
let flow_success_output success = success.success
let flow_success_evidence success = success.evidence
let flow_success_ordinal success = success.success_ordinal

let call_id_to_string (Call_id id) = id
let flow_id_to_string (Flow_id id) = id
let flow_visit_ordinal_to_int (Flow_visit_ordinal ordinal) = ordinal
let flow_attempt_id (flow : flow_attempt) = flow.flow_id
let attempt_receipt (attempt : attempt) = attempt.receipt
let receipt_call_id = Generation_receipt.call_id
let measurement_operation_id_to_string = Flow_admission.operation_id_to_string

let flow_measurement_receipt_snapshot (measurement : flow_measurement_receipt) =
  let snapshot = Flow_admission.receipt_snapshot measurement.receipt in
  { operation_id = snapshot.operation_id
  ; visit = measurement.visit
  ; request_body_sha256 = snapshot.request_body_sha256
  ; phase = snapshot.phase
  ; dispatch = snapshot.dispatch
  ; outcome = snapshot.outcome
  }
;;

let same_measurement
      (left : measurement_receipt_snapshot)
      (right : measurement_receipt_snapshot)
  =
  String.equal
    (measurement_operation_id_to_string left.operation_id)
    (measurement_operation_id_to_string right.operation_id)
;;

let publish_measurement (flow : flow_attempt) (measurement : flow_measurement_receipt) =
  Flow_state.publish_measurement
    flow.progress
    ~same:same_measurement
    (flow_measurement_receipt_snapshot measurement)
;;

let same_attempt (left : flow_attempt_publication) (right : flow_attempt_publication) =
  String.equal (call_id_to_string left.call_id) (call_id_to_string right.call_id)
;;

let publish_attempt_snapshot (flow : flow_attempt) (live : flow_attempt_receipt) =
  let publication : flow_attempt_publication =
    { call_id = receipt_call_id live.receipt
    ; snapshot =
        { scope = live.scope
        ; visit = live.visit
        ; receipt = Generation_receipt.snapshot live.receipt
        }
    }
  in
  Flow_state.publish_attempt flow.progress ~same:same_attempt publication
;;

let receipt_phase = Generation_receipt.phase
let receipt_dispatch_count = Generation_receipt.dispatch_count

let generation_dispatch_fact_of_receipt receipt =
  if Generation_receipt.generation_dispatched receipt
  then Generation_dispatch_started
  else No_generation_dispatch
;;

let flow_execution_error_generation_dispatch = function
  | Flow_attempt_already_started _
  | Flow_attempt_start_failed _
  | Flow_measurement_start_failed _
  | Flow_before_measurement_dispatch_callback_failed _
  | Flow_measurement_terminal_callback_failed _
  | Flow_before_dispatch_callback_failed _
  | Flow_before_advance_callback_failed _
  | Flow_candidates_exhausted _ -> No_generation_dispatch
  | Flow_success_ordinal_exhausted _ -> Generation_dispatch_started
  | Flow_exact_execution_failed { cause; _ } ->
    generation_dispatch_fact_of_receipt cause.receipt
;;

let receipt_http_status = Generation_receipt.http_status
let receipt_provider_trace = Generation_receipt.provider_trace
let provider_trace_fingerprint = Trace.fingerprint
let receipt_plan_fingerprint = Generation_receipt.plan_fingerprint
let receipt_request_body_sha256 = Generation_receipt.request_body_sha256
let receipt_catalog_generation = Generation_receipt.catalog_generation
let receipt_catalog_evidence = Generation_receipt.catalog_evidence
let receipt_target_identity = Generation_receipt.target_identity
let candidate_visit_count_to_int (Candidate_visit_count count) = count
let generation_receipt_snapshot = Generation_receipt.snapshot
let generation_receipt_snapshot_phase = Generation_receipt.snapshot_phase

let generation_receipt_snapshot_dispatch_count =
  Generation_receipt.snapshot_dispatch_count
;;

let generation_receipt_snapshot_http_status = Generation_receipt.snapshot_http_status

let generation_receipt_snapshot_provider_trace =
  Generation_receipt.snapshot_provider_trace
;;

let generation_receipt_snapshot_call_id = Generation_receipt.snapshot_call_id

let generation_receipt_snapshot_plan_fingerprint =
  Generation_receipt.snapshot_plan_fingerprint
;;

let generation_receipt_snapshot_request_body_sha256 =
  Generation_receipt.snapshot_request_body_sha256
;;

let generation_receipt_snapshot_catalog_generation =
  Generation_receipt.snapshot_catalog_generation
;;

let generation_receipt_snapshot_catalog_evidence =
  Generation_receipt.snapshot_catalog_evidence
;;

let generation_receipt_snapshot_target_identity =
  Generation_receipt.snapshot_target_identity
;;

let domain_settlement_intent_format = "oas.exact-output.domain-settlement-intent"
let domain_settlement_intent_version = 1
let sha256_string value = Digestif.SHA256.(to_hex (digest_string value))

let domain_disposition_to_string = function
  | Domain_valid -> "domain_valid"
  | Domain_rejected -> "domain_rejected"
;;

let domain_disposition_of_string = function
  | "domain_valid" -> Some Domain_valid
  | "domain_rejected" -> Some Domain_rejected
  | _ -> None
;;

let domain_settlement_structural_json intent =
  `Assoc
    [ "format", `String domain_settlement_intent_format
    ; "version", `Int domain_settlement_intent_version
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

let domain_settlement_payload_json intent =
  match domain_settlement_structural_json intent with
  | `Assoc fields ->
    `Assoc
      (fields
       @ [ ( "settlement_id"
           , `String
               (Flow_contract.domain_settlement_id_to_string intent.settlement_id) )
         ; "disposition", `String (domain_disposition_to_string intent.disposition)
         ])
  | _ -> assert false
;;

let domain_settlement_intent_to_string intent =
  match domain_settlement_payload_json intent with
  | `Assoc fields ->
    Yojson.Safe.to_string
      (`Assoc (fields @ [ "integrity_sha256", `String intent.integrity_sha256 ]))
  | _ -> assert false
;;

let domain_settlement_intent_id intent = intent.settlement_id

let effect_phase_to_string = function
  | Not_started -> "not_started"
  | Before_dispatch -> "before_dispatch"
  | Dispatch_started -> "dispatch_started"
  | Response_received -> "response_received"
  | Terminal -> "terminal"
;;

let make_domain_settlement_intent success disposition =
  let candidate = success.candidate in
  let snapshot = generation_receipt_snapshot candidate.receipt in
  let execution_evidence =
    `Assoc
      [ ( "call_id"
        , `String
            (generation_receipt_snapshot_call_id snapshot |> call_id_to_string) )
      ; ( "plan_fingerprint"
        , `String (generation_receipt_snapshot_plan_fingerprint snapshot) )
      ; ( "request_body_sha256"
        , `String (generation_receipt_snapshot_request_body_sha256 snapshot) )
      ; ( "phase"
        , `String
            (generation_receipt_snapshot_phase snapshot |> effect_phase_to_string) )
      ; ( "dispatch_count"
        , `Int (generation_receipt_snapshot_dispatch_count snapshot) )
      ; ( "http_status"
        , match generation_receipt_snapshot_http_status snapshot with
          | None -> `Null
          | Some status -> `Int status )
      ]
  in
  let provisional =
    { settlement_id =
        Flow_contract.domain_settlement_id_of_string (String.make 64 '0')
    ; flow_id = flow_id_to_string candidate.visit.flow_id
    ; scope = success.scope
    ; reservation = success.preference_reservation
    ; candidate =
        Flow_contract.flow_preference_identity_of_candidate candidate.visit.identity
    ; success_ordinal = success.success_ordinal
    ; execution_evidence_sha256 =
        sha256_string (Yojson.Safe.to_string execution_evidence)
    ; disposition
    ; integrity_sha256 = String.make 64 '0'
    }
  in
  let settlement_id =
    domain_settlement_structural_json provisional
    |> Yojson.Safe.to_string
    |> sha256_string
    |> Flow_contract.domain_settlement_id_of_string
  in
  let with_id = { provisional with settlement_id } in
  let integrity_sha256 =
    domain_settlement_payload_json with_id |> Yojson.Safe.to_string |> sha256_string
  in
  { with_id with integrity_sha256 }
;;

let domain_settlement_intent_expected_fields =
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

let domain_settlement_intent_of_string encoded =
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
    let* value = string_field fields name in
    match Int64.of_string_opt value with
    | Some value when Int64.compare value 0L > 0 -> Ok value
    | Some _ | None -> invalid name
  in
  try
    match Yojson.Safe.from_string encoded with
    | `Assoc fields ->
      let actual = List.map fst fields |> List.sort String.compare in
      let expected =
        List.sort String.compare domain_settlement_intent_expected_fields
      in
      if actual <> expected
      then Error Domain_settlement_intent_invalid_fields
      else (
        let* format = string_field fields "format" in
        let* () =
          if String.equal format domain_settlement_intent_format
          then Ok ()
          else Error (Domain_settlement_intent_unknown_format format)
        in
        let* version = find fields "version" in
        let* () =
          match version with
          | `Int version when version = domain_settlement_intent_version -> Ok ()
          | `Int version ->
            Error (Domain_settlement_intent_unsupported_version version)
          | _ -> invalid "version"
        in
        let* flow_id = string_field fields "flow_id" in
        let* scope_text = string_field fields "scope" in
        let* scope =
          match Flow_contract.make_flow_scope ~id:scope_text with
          | Ok scope when String.equal scope_text (Flow_contract.flow_scope_to_string scope)
            -> Ok scope
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
        let* candidate_binding_sha256 =
          string_field fields "candidate_binding_sha256"
        in
        let* () =
          if (not (String.equal (String.trim candidate_id) ""))
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
          match domain_disposition_of_string disposition_text with
          | Some disposition -> Ok disposition
          | None -> invalid "disposition"
        in
        let* integrity_sha256 = string_field fields "integrity_sha256" in
        let* () =
          if is_sha256 execution_evidence_sha256
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
          domain_settlement_structural_json intent
          |> Yojson.Safe.to_string
          |> sha256_string
        in
        let expected_integrity =
          domain_settlement_payload_json intent
          |> Yojson.Safe.to_string
          |> sha256_string
        in
        if String.equal expected_settlement_id settlement_id_text
           && String.equal expected_integrity integrity_sha256
        then Ok intent
        else Error Domain_settlement_intent_integrity_mismatch)
    | _ -> Error Domain_settlement_intent_invalid_fields
  with
  | Yojson.Json_error detail ->
    Error (Domain_settlement_intent_malformed_json detail)
;;

let commit_and_settle_flow_domain ~commit success disposition =
  let intent = make_domain_settlement_intent success disposition in
  let receipt =
    Flow_contract.make_domain_settlement_receipt
      ~settlement_id:intent.settlement_id
      ~disposition
  in
  match
    Flow_contract.begin_domain_settlement
      success.domain_settlement
      success.preferences
      receipt
  with
  | Flow_contract.Domain_settlement_replayed receipt -> Ok receipt
  | Flow_contract.Domain_settlement_conflict -> Error Domain_settlement_conflict
  | Flow_contract.Domain_settlement_claimed ->
    let finished = ref false in
    Fun.protect
      ~finally:(fun () ->
        if not !finished
        then
          Flow_contract.abort_domain_settlement
            success.domain_settlement
            success.preferences
            receipt)
      (fun () ->
         match commit intent with
         | Error cause -> Error (Domain_commit_failed cause)
         | Ok () ->
           (match
              Flow_contract.finish_domain_settlement
                success.domain_settlement
                success.preferences
                success.scope
                ~reservation:success.preference_reservation
                ~candidate:success.candidate.visit.identity
                ~success_ordinal:success.success_ordinal
                receipt
            with
            | Error Flow_contract.Domain_settlement_apply_conflict ->
              Error Domain_settlement_conflict
            | Ok receipt ->
              finished := true;
              Ok receipt))
;;

let resume_committed_flow_domain recovery intent =
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
  | Error Flow_contract.Domain_settlement_recovery_conflict ->
    Error Domain_settlement_recovery_conflict
;;

let candidate_rejection_identity (receipt : candidate_rejection_receipt) =
  receipt.visit.identity
;;

let candidate_rejection_scope (receipt : candidate_rejection_receipt) = receipt.scope
let candidate_rejection_visit (receipt : candidate_rejection_receipt) = receipt.visit

let candidate_rejection_measurement_dispatch_fact (receipt : candidate_rejection_receipt) =
  receipt.measurement.dispatch
;;

let candidate_rejection_measurement_outcome (receipt : candidate_rejection_receipt) =
  receipt.measurement.outcome
;;

let target_selection_error_disposition = function
  | Missing_target_credential _
  | Target_credential_invalid _
  | Target_credential_read_failed _ -> Runtime_slot_unavailable
;;

let wire_admission_error_disposition = function
  | Capability_snapshot_missing
  | Inconsistent_output_contract
  | Global_admission_not_allowed
  | Invalid_connect_timeout
  | Invalid_body_timeout
  | Context_limit_unavailable
  | Invalid_context_limit
  | Unsupported_target_model _ -> Runtime_contract_rejected
  | Output_contract_unavailable -> Output_requirement_rejected
  | Cross_feature_not_allowed
  | Caller_supplied_header_not_allowed
  | Unsupported_image_input
  | Unsupported_document_input
  | Unsupported_audio_input
  | Unsupported_system_prompt -> Input_contract_rejected
  | Token_measurement_required constraint_ ->
    Input_capacity
      (Token_measurement_required
         { accepted_through_tokens = constraint_.accepted_through_tokens
         ; rejected_from_tokens = constraint_.rejected_from_tokens
         })
  | Measured_context_window_exceeded
      { input_tokens; reserved_output_tokens; max_context_tokens } ->
    Input_capacity
      (Context_window_exceeded
         { input_tokens; reserved_output_tokens; max_context_tokens })
  | Measured_serving_constraint_rejected reason ->
    Input_capacity (Token_capacity_rejected reason)
  | Request_body_too_large { actual_bytes; limit_bytes } ->
    Input_capacity (Serialized_request_body_too_large { actual_bytes; limit_bytes })
  | Output_reservation_unavailable
  | Token_measurement_failed
  | Target_request_rejected
  | Request_serialization_rejected -> Request_preparation_failed
;;

let admission_error_disposition = function
  | Provider_schema_unavailable
  | Json_syntax_unavailable
  | Unsupported_schema_keyword _
  | Unsupported_schema_type _
  | Invalid_schema -> Output_requirement_rejected
  | Wire_admission_rejected cause -> wire_admission_error_disposition cause
;;

let candidate_rejection_disposition (receipt : candidate_rejection_receipt) =
  match receipt.cause with
  | Target_selection_rejected cause -> target_selection_error_disposition cause
  | Request_admission_rejected cause -> admission_error_disposition cause
;;

let flow_attempt_evidence (flow : flow_attempt) =
  let progress = Flow_state.progress_snapshot flow.progress in
  { flow_id = flow.flow_id
  ; scope = flow.scope
  ; declared_candidate_snapshot = flow.declared_candidate_snapshot
  ; candidate_snapshot = flow.candidate_snapshot
  ; preference_observation = flow.preference_observation
  ; candidate_visit_count = Candidate_visit_count progress.candidate_visit_count
  ; measurements = progress.measurements
  ; admissions = progress.admissions
  ; attempts = List.map (fun publication -> publication.snapshot) progress.attempts
  }
;;

let observe_phase = Generation_receipt.observe_phase
let synchronize_receipt = Generation_receipt.synchronize
let raw_response = Trace.raw_response
let record_provider_trace = Generation_receipt.record_provider_trace

let execution_error_cause = function
  | Exec.Clock_required_for_timeout -> Clock_required_for_timeout
  | Exec.Frozen_request_mismatch -> Frozen_request_mismatch
  | Exec.Provider_error _ -> Completion_failed
  | Exec.Output_normalization_failed (Exec.Incomplete_structured_response _) ->
    Incomplete_output
  | Exec.Output_normalization_failed Exec.Missing_structured_text -> Missing_output
  | Exec.Output_normalization_failed (Exec.Ambiguous_structured_text count) ->
    Ambiguous_output count
  | Exec.Output_normalization_failed Exec.Unexpected_structured_content ->
    Unexpected_output_content
  | Exec.Output_normalization_failed (Exec.Invalid_json _) -> Invalid_json_output
;;

let execute_once_with_publication ~publish ~net ?clock (attempt : attempt) =
  let ready = attempt.ready in
  let receipt = attempt.receipt in
  if not (Generation_receipt.try_start receipt)
  then
    Error
      { call_id = receipt_call_id receipt
      ; receipt
      ; cause = Attempt_already_started
      ; raw_response = None
      }
  else (
    publish ();
    match
      Exec.execute_once_with_evidence
        ~net
        ?clock
        ~on_phase:(fun phase ->
          observe_phase receipt phase;
          publish ())
        ready.plan
    with
    | Error
        ({ receipt = complete_receipt; cause; raw_response = evidence } :
          Exec.execute_once_error_with_evidence) ->
      synchronize_receipt receipt complete_receipt;
      publish ();
      Option.iter
        (fun response_evidence ->
           response_evidence
           |> Trace.of_evidence complete_receipt
           |> record_provider_trace receipt)
        evidence;
      publish ();
      Error
        { call_id = receipt_call_id receipt
        ; receipt
        ; cause = execution_error_cause cause
        ; raw_response = Option.map raw_response evidence
        }
    | Ok { outcome; raw_response = evidence } ->
      synchronize_receipt receipt outcome.receipt;
      publish ();
      let provider_trace =
        Trace.of_evidence ~response:outcome.response outcome.receipt evidence
      in
      record_provider_trace receipt provider_trace;
      publish ();
      (match outcome.output with
       | Exec.Json_output { value; _ } ->
         Ok
           { call_id = receipt_call_id receipt
           ; receipt
           ; output = value
           ; provenance = ready.provenance
           ; raw_response = raw_response evidence
           }
       | Exec.Text_output _ ->
         Error
           { call_id = receipt_call_id receipt
           ; receipt
           ; cause = Internal_non_json_output
           ; raw_response = Some (raw_response evidence)
           }))
;;

let execute_once ~net ?clock attempt =
  execute_once_with_publication ~publish:ignore ~net ?clock attempt
;;

let execution_failure_may_advance (error : execution_error) =
  match error.cause, receipt_phase error.receipt with
  | Completion_failed, Before_dispatch -> receipt_dispatch_count error.receipt = 0
  | Completion_failed, (Not_started | Dispatch_started | Response_received | Terminal)
  | ( ( Attempt_already_started
      | Clock_required_for_timeout
      | Frozen_request_mismatch
      | Incomplete_output
      | Missing_output
      | Ambiguous_output _
      | Unexpected_output_content
      | Invalid_json_output
      | Internal_non_json_output )
    , _ ) -> false
;;

let admitted_flow_candidate visit (plan : ready_plan) =
  { visit
  ; plan_fingerprint = plan.plan_fingerprint
  ; request_body_sha256 = plan.request_body_sha256
  ; provenance = plan.provenance
  ; measurement = plan.measurement
  }
;;

let record_candidate_rejection (flow : flow_attempt) visit cause measurement =
  let rejection = { scope = flow.scope; visit; cause; measurement } in
  Flow_state.record_admission flow.progress (Candidate_rejected rejection);
  rejection
;;

let execute_flow_candidate
      ~net
      ?clock
      ~before_measurement_dispatch
      ~on_measurement_terminal
      ~before_dispatch
      flow
      (candidate : flow_candidate_step)
  =
  let reject
        ?(measurement =
          { dispatch = No_measurement_dispatch; outcome = Measurement_not_required })
        cause
    =
    let rejection = record_candidate_rejection flow candidate.visit cause measurement in
    Error (Flow_step_candidate_rejected rejection)
  in
  match resolve_target candidate.admitted_target with
  | Error cause -> reject (Target_selection_rejected cause)
  | Ok target ->
    (match
       admit_candidate_request
         ~net
         ?clock
         ~on_measurement_receipt:(fun receipt ->
           let measurement = { visit = candidate.visit; receipt } in
           publish_measurement flow measurement)
         ~before_measurement_dispatch:(fun receipt ->
           before_measurement_dispatch { visit = candidate.visit; receipt })
         ~on_measurement_terminal:(fun receipt ->
           on_measurement_terminal { visit = candidate.visit; receipt })
         ~target
         ~messages:flow.messages
         flow.requirement
     with
     | Error (Flow_request_admission_failed (cause, measurement)) ->
       reject ~measurement (Request_admission_rejected cause)
     | Error (Flow_request_measurement_start_failed detail) ->
       Error
         (Flow_step_measurement_start_failed
            (candidate.visit, Measurement_operation_id_generation_failed detail))
     | Error Flow_request_measurement_clock_required_for_timeout ->
       Error
         (Flow_step_measurement_start_failed
            (candidate.visit, Measurement_clock_required_for_timeout))
     | Error (Flow_request_before_measurement_dispatch_failed (receipt, cause)) ->
       Error
         (Flow_step_before_measurement_dispatch_callback_failed
            ({ visit = candidate.visit; receipt }, cause))
     | Error (Flow_request_measurement_terminal_callback_failed (receipt, cause)) ->
       Error
         (Flow_step_measurement_terminal_callback_failed
            ({ visit = candidate.visit; receipt }, cause))
     | Ok plan ->
       let admitted = admitted_flow_candidate candidate.visit plan in
       Flow_state.record_admission flow.progress (Candidate_admitted admitted);
       (match start_attempt plan with
        | Error cause -> Error (Flow_step_attempt_start_failed (candidate.visit, cause))
        | Ok attempt ->
          let candidate_receipt : flow_attempt_receipt =
            { scope = flow.scope
            ; visit = candidate.visit
            ; receipt = attempt_receipt attempt
            }
          in
          publish_attempt_snapshot flow candidate_receipt;
          (match before_dispatch candidate_receipt with
           | Error cause ->
             Error (Flow_step_before_dispatch_callback_failed (candidate_receipt, cause))
           | Ok () ->
             (match
                execute_once_with_publication
                  ~publish:(fun () -> publish_attempt_snapshot flow candidate_receipt)
                  ~net
                  ?clock
                  attempt
              with
              | Ok success -> Ok (candidate_receipt, success)
              | Error cause ->
                Error
                  (Flow_step_execution_failed { candidate = candidate_receipt; cause })))))
;;

let advanceable_flow_failure = function
  | Flow_step_candidate_rejected receipt
    when receipt.measurement.dispatch = No_measurement_dispatch ->
    Some (Flow_candidate_rejected receipt)
  | Flow_step_candidate_rejected _ -> None
  | Flow_step_execution_failed ({ cause; _ } as failure)
    when execution_failure_may_advance cause ->
    Some
      (Flow_candidate_execution_failed
         { candidate = failure.candidate; cause = failure.cause })
  | Flow_step_execution_failed _
  | Flow_step_attempt_start_failed _
  | Flow_step_measurement_start_failed _
  | Flow_step_before_measurement_dispatch_callback_failed _
  | Flow_step_measurement_terminal_callback_failed _
  | Flow_step_before_dispatch_callback_failed _ -> None
;;

let execute_flow_once
      ~net
      ?clock
      ~before_measurement_dispatch
      ~on_measurement_terminal
      ~before_dispatch
      ~before_advance
      flow
  =
  let outcome =
    Flow_state.execute_once
      flow.execution
      ~candidates:flow.candidates
      ~execute:
        (execute_flow_candidate
           ~net
           ?clock
           ~before_measurement_dispatch
           ~on_measurement_terminal
           ~before_dispatch
           flow)
      ~advanceable:advanceable_flow_failure
      ~before_advance:(fun ~failed:_ ~failure ~next ->
        before_advance ~failed:failure ~next:next.visit)
  in
  let evidence = flow_attempt_evidence flow in
  match outcome with
  | Flow_state.Succeeded { success = candidate, success; _ } ->
    (match Flow_contract.allocate_flow_success_ordinal flow.preferences with
     | Error Success_ordinal_space_exhausted ->
       Error (Flow_success_ordinal_exhausted evidence)
     | Ok success_ordinal ->
       Ok
         { candidate
         ; success
         ; success_ordinal
         ; evidence
         ; domain_settlement = Flow_state.create_domain_settlement ()
         ; preferences = flow.preferences
         ; scope = flow.scope
         ; preference_reservation = flow.preference_reservation
         })
  | Flow_state.Attempt_already_started -> Error (Flow_attempt_already_started evidence)
  | Flow_state.Before_advance_callback_failed { failure; next_candidate; cause; _ } ->
    Error
      (Flow_before_advance_callback_failed
         { failed = failure; next = next_candidate.visit; cause; evidence })
  | Flow_state.Execution_failed { cause; _ } ->
    (match cause with
     | Flow_step_candidate_rejected rejection ->
       Error (Flow_candidates_exhausted { rejection; evidence })
     | Flow_step_attempt_start_failed (candidate, cause) ->
       Error (Flow_attempt_start_failed { candidate; cause; evidence })
     | Flow_step_measurement_start_failed (candidate, cause) ->
       Error (Flow_measurement_start_failed { candidate; cause; evidence })
     | Flow_step_before_measurement_dispatch_callback_failed (measurement, cause) ->
       Error
         (Flow_before_measurement_dispatch_callback_failed
            { measurement; cause; evidence })
     | Flow_step_measurement_terminal_callback_failed (measurement, cause) ->
       Error (Flow_measurement_terminal_callback_failed { measurement; cause; evidence })
     | Flow_step_before_dispatch_callback_failed (candidate, cause) ->
       Error (Flow_before_dispatch_callback_failed { candidate; cause; evidence })
     | Flow_step_execution_failed { candidate; cause; _ } ->
       Error (Flow_exact_execution_failed { candidate; cause; evidence }))
;;
