type candidate =
  { candidate_id : string
  ; candidate_binding_sha256 : string
  ; catalog_generation_sha256 : string
  ; catalog_evidence_sha256 : string
  }

type assurance =
  | Json_syntax_only
  | Provider_schema_requested

type provenance =
  { source_schema_sha256 : string
  ; effective_schema_sha256 : string option
  ; assurance : assurance
  ; candidate_binding_sha256 : string
  ; catalog_generation_sha256 : string
  ; catalog_evidence_sha256 : string
  }

type measurement_dispatch =
  | No_measurement_dispatch
  | Measurement_dispatch_started

type measurement_outcome =
  | Measurement_not_required
  | Measurement_succeeded
  | Measurement_unsupported
  | Measurement_local_invalid
  | Measurement_transport_failed
  | Measurement_invalid_response
  | Measurement_fence_rejected
  | Measurement_cancelled

type measurement_evidence =
  { dispatch : measurement_dispatch
  ; outcome : measurement_outcome
  }

type admitted =
  { plan_sha256 : string
  ; request_body_sha256 : string
  ; provenance : provenance
  ; measurement : measurement_evidence
  }

type rejected =
  { rejection : Yojson.Safe.t
  ; measurement : measurement_evidence
  }

type admission =
  | Rejected of rejected
  | Admitted of admitted

type measurement =
  { operation_id : string
  ; request_body_sha256 : string
  ; candidate_binding_sha256 : string
  ; catalog_generation_sha256 : string
  ; catalog_evidence_sha256 : string
  ; dispatch : measurement_dispatch
  ; outcome : measurement_outcome
  }

type attempt_phase =
  | Before_dispatch
  | Response_received
  | Terminal

type attempt =
  { call_id : string
  ; plan_sha256 : string
  ; request_body_sha256 : string
  ; candidate_binding_sha256 : string
  ; catalog_generation_sha256 : string
  ; catalog_evidence_sha256 : string
  ; phase : attempt_phase
  ; dispatch_count : int
  ; http_status : int option
  ; provider_trace_sha256 : string option
  ; raw_response_sha256 : string option
  }

type transport_failure =
  | Candidate_rejected
  | Completion_failed_before_dispatch
  | Serialized_request_refused of { http_status : int }
  | Invalid_json_output

type advance =
  { next_ordinal : int
  ; failure : transport_failure
  }

type outcome =
  | Advance of advance
  | Semantic_rejected of
      { projector : Yojson.Safe.t
      ; output_sha256 : string
      }
  | Accepted of
      { projector : Yojson.Safe.t
      ; output_sha256 : string
      }

type step =
  { ordinal : int
  ; admission : admission
  ; measurement : measurement option
  ; attempt : attempt option
  ; outcome : outcome
  }

type invariant_error =
  | Empty_flow_id
  | Empty_declared_candidates
  | Empty_steps
  | Non_canonical_identifier of
      { field : string
      ; ordinal : int option
      }
  | Invalid_sha256 of
      { field : string
      ; ordinal : int option
      }
  | Invalid_http_status of
      { field : string
      ; ordinal : int
      }
  | Duplicate_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }
  | Duplicate_call_id of
      { call_id : string
      ; first_ordinal : int
      ; duplicate_ordinal : int
      }
  | Duplicate_measurement_operation_id of
      { operation_id : string
      ; first_ordinal : int
      ; duplicate_ordinal : int
      }
  | More_steps_than_declared_candidates
  | Non_contiguous_step_ordinal of
      { expected : int
      ; actual : int
      }
  | Invalid_measurement_state of { ordinal : int }
  | Measurement_binding_mismatch of { ordinal : int }
  | Rejected_admission_has_attempt of { ordinal : int }
  | Rejected_admission_did_not_advance of { ordinal : int }
  | Admitted_candidate_missing_attempt of { ordinal : int }
  | Attempt_binding_mismatch of { ordinal : int }
  | Invalid_attempt_state of { ordinal : int }
  | Non_adjacent_advance of
      { ordinal : int
      ; next_ordinal : int
      }
  | Advance_failure_mismatch of { ordinal : int }
  | Nonfinal_step_accepted of { ordinal : int }
  | Final_step_not_accepted of { ordinal : int }
  | Invalid_projector_json of
      { ordinal : int
      ; location : string
      }

type decode_error =
  | Malformed_json of string
  | Invalid_fields of
      { path : string
      ; detail : string
      }
  | Invalid_transcript of invariant_error
  | Integrity_mismatch
  | Non_canonical_encoding

type projector =
  { json : Yojson.Safe.t
  ; digest : string
  }

type normalized_admission =
  | Normalized_rejected of
      { rejection : projector
      ; measurement : measurement_evidence
      }
  | Normalized_admitted of admitted

type normalized_outcome =
  | Normalized_advance of advance
  | Normalized_semantic_rejected of
      { projector : projector
      ; output_sha256 : string
      }
  | Normalized_accepted of
      { projector : projector
      ; output_sha256 : string
      }

type normalized_step =
  { ordinal : int
  ; admission : normalized_admission
  ; measurement : measurement option
  ; attempt : attempt option
  ; outcome : normalized_outcome
  }

type t =
  { flow_id : string
  ; declared_candidates : candidate array
  ; steps : normalized_step array
  ; integrity_sha256 : string
  ; accepted_sha256 : string
  }

let ( let* ) = Result.bind
let digest value = Digestif.SHA256.(to_hex (digest_string value))

let is_sha256 value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' -> true
         | _ -> false)
       value
;;

let is_canonical_identifier value =
  (not (String.equal value "")) && String.equal value (String.trim value)
;;

let duplicate_key fields =
  let seen = Hashtbl.create (List.length fields) in
  List.find_map
    (fun (key, _) ->
       if Hashtbl.mem seen key
       then Some key
       else (
         Hashtbl.add seen key ();
         None))
    fields
;;

let rec canonicalize_projector_json json =
  match json with
  | `Assoc fields ->
    (match duplicate_key fields with
     | Some _ -> Error ()
     | None ->
       let rec loop acc = function
         | [] ->
           Ok
             (`Assoc
                 (List.sort (fun (left, _) (right, _) -> String.compare left right) acc))
         | (key, value) :: rest ->
           let* value = canonicalize_projector_json value in
           loop ((key, value) :: acc) rest
       in
       loop [] fields)
  | `List values ->
    let rec loop acc = function
      | [] -> Ok (`List (List.rev acc))
      | value :: rest ->
        let* value = canonicalize_projector_json value in
        loop (value :: acc) rest
    in
    loop [] values
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Floatlit _ | `String _) as scalar ->
    Ok scalar
  | `Float value when Float.is_finite value -> Ok (`Float value)
  | `Float _ -> Error ()
  | `Tuple _ | `Variant _ -> Error ()
;;

let projector ~ordinal ~location json =
  try
    match canonicalize_projector_json json with
    | Error () -> Error (Invalid_projector_json { ordinal; location })
    | Ok json ->
      let canonical = Yojson.Safe.to_string json in
      (match Yojson.Safe.from_string canonical with
       | reparsed when String.equal canonical (Yojson.Safe.to_string reparsed) ->
         Ok { json; digest = digest canonical }
       | _ -> Error (Invalid_projector_json { ordinal; location })
       | exception Yojson.Json_error _ ->
         Error (Invalid_projector_json { ordinal; location }))
  with
  | Stack_overflow -> Error (Invalid_projector_json { ordinal; location })
;;

let check_identifier ?ordinal field value =
  if is_canonical_identifier value
  then Ok ()
  else Error (Non_canonical_identifier { field; ordinal })
;;

let check_sha256 ?ordinal field value =
  if is_sha256 value then Ok () else Error (Invalid_sha256 { field; ordinal })
;;

let check_optional_sha256 ~ordinal field = function
  | None -> Ok ()
  | Some value -> check_sha256 ~ordinal field value
;;

let check_candidate ?ordinal candidate =
  let* () = check_identifier ?ordinal "candidate_id" candidate.candidate_id in
  let* () =
    check_sha256 ?ordinal "candidate_binding_sha256" candidate.candidate_binding_sha256
  in
  let* () =
    check_sha256 ?ordinal "catalog_generation_sha256" candidate.catalog_generation_sha256
  in
  check_sha256 ?ordinal "catalog_evidence_sha256" candidate.catalog_evidence_sha256
;;

let check_provenance ~ordinal candidate provenance =
  let* () =
    check_sha256 ~ordinal "source_schema_sha256" provenance.source_schema_sha256
  in
  let* () =
    check_optional_sha256
      ~ordinal
      "effective_schema_sha256"
      provenance.effective_schema_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "provenance.candidate_binding_sha256"
      provenance.candidate_binding_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "provenance.catalog_generation_sha256"
      provenance.catalog_generation_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "provenance.catalog_evidence_sha256"
      provenance.catalog_evidence_sha256
  in
  if
    String.equal candidate.candidate_binding_sha256 provenance.candidate_binding_sha256
    && String.equal
         candidate.catalog_generation_sha256
         provenance.catalog_generation_sha256
    && String.equal candidate.catalog_evidence_sha256 provenance.catalog_evidence_sha256
  then Ok ()
  else Error (Attempt_binding_mismatch { ordinal })
;;

let rejected_measurement_state_is_valid evidence =
  evidence.dispatch = No_measurement_dispatch
;;

let admitted_measurement_state_is_valid evidence =
  match evidence.dispatch, evidence.outcome with
  | No_measurement_dispatch, Measurement_not_required
  | Measurement_dispatch_started, Measurement_succeeded -> true
  | No_measurement_dispatch, Measurement_succeeded
  | Measurement_dispatch_started, Measurement_not_required
  | ( (No_measurement_dispatch | Measurement_dispatch_started)
    , ( Measurement_unsupported
      | Measurement_local_invalid
      | Measurement_transport_failed
      | Measurement_invalid_response
      | Measurement_fence_rejected
      | Measurement_cancelled ) ) -> false
;;

let measurement_state_is_valid measurement =
  measurement.dispatch = Measurement_dispatch_started
  && measurement.outcome = Measurement_succeeded
;;

let rejected_measurement_receipt_state_is_valid measurement =
  measurement.dispatch = No_measurement_dispatch
  && measurement.outcome <> Measurement_not_required
;;

let attempt_success_state_is_valid attempt =
  match attempt.phase with
  | Terminal ->
    attempt.dispatch_count = 1
    && Option.exists (fun status -> status >= 200 && status <= 299) attempt.http_status
    && Option.is_some attempt.provider_trace_sha256
    && Option.is_some attempt.raw_response_sha256
  | Before_dispatch | Response_received -> false
;;

let attempt_advance_state_is_valid attempt failure =
  match failure, attempt.phase with
  | Completion_failed_before_dispatch, Before_dispatch ->
    attempt.dispatch_count = 0
    && Option.is_none attempt.http_status
    && Option.is_none attempt.provider_trace_sha256
    && Option.is_none attempt.raw_response_sha256
  | Serialized_request_refused { http_status }, Response_received ->
    http_status = 413
    && attempt.dispatch_count = 1
    && attempt.http_status = Some http_status
    && Option.is_some attempt.provider_trace_sha256
    && Option.is_some attempt.raw_response_sha256
  | Invalid_json_output, (Response_received | Terminal) ->
    attempt.dispatch_count = 1
    && Option.exists (fun status -> status >= 200 && status <= 299) attempt.http_status
    && Option.is_some attempt.provider_trace_sha256
    && Option.is_some attempt.raw_response_sha256
  | Candidate_rejected, _
  | Completion_failed_before_dispatch, (Response_received | Terminal)
  | Serialized_request_refused _, (Before_dispatch | Terminal)
  | Invalid_json_output, Before_dispatch -> false
;;

let check_http_status ~ordinal field = function
  | None -> Ok ()
  | Some status ->
    if status >= 100 && status <= 599
    then Ok ()
    else Error (Invalid_http_status { field; ordinal })
;;

let check_measurement
      ~ordinal
      ~candidate
      ~request_body_sha256
      ~measurement_ids
      ~state_is_valid
      measurement
  =
  let* () =
    check_identifier ~ordinal "measurement.operation_id" measurement.operation_id
  in
  let* () =
    check_sha256
      ~ordinal
      "measurement.request_body_sha256"
      measurement.request_body_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "measurement.candidate_binding_sha256"
      measurement.candidate_binding_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "measurement.catalog_generation_sha256"
      measurement.catalog_generation_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "measurement.catalog_evidence_sha256"
      measurement.catalog_evidence_sha256
  in
  let* () =
    match Hashtbl.find_opt measurement_ids measurement.operation_id with
    | None ->
      Hashtbl.add measurement_ids measurement.operation_id ordinal;
      Ok ()
    | Some first_ordinal ->
      Error
        (Duplicate_measurement_operation_id
           { operation_id = measurement.operation_id
           ; first_ordinal
           ; duplicate_ordinal = ordinal
           })
  in
  if
    (not (state_is_valid measurement))
    || not
         (Option.fold
            ~none:true
            ~some:(String.equal measurement.request_body_sha256)
            request_body_sha256
          && String.equal
               measurement.candidate_binding_sha256
               candidate.candidate_binding_sha256
          && String.equal
               measurement.catalog_generation_sha256
               candidate.catalog_generation_sha256
          && String.equal
               measurement.catalog_evidence_sha256
               candidate.catalog_evidence_sha256)
  then
    if state_is_valid measurement
    then Error (Measurement_binding_mismatch { ordinal })
    else Error (Invalid_measurement_state { ordinal })
  else Ok ()
;;

let check_attempt ~ordinal ~candidate ~admitted ~call_ids ~outcome attempt =
  let* () = check_identifier ~ordinal "attempt.call_id" attempt.call_id in
  let* () = check_sha256 ~ordinal "attempt.plan_sha256" attempt.plan_sha256 in
  let* () =
    check_sha256 ~ordinal "attempt.request_body_sha256" attempt.request_body_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "attempt.candidate_binding_sha256"
      attempt.candidate_binding_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "attempt.catalog_generation_sha256"
      attempt.catalog_generation_sha256
  in
  let* () =
    check_sha256
      ~ordinal
      "attempt.catalog_evidence_sha256"
      attempt.catalog_evidence_sha256
  in
  let* () =
    check_optional_sha256
      ~ordinal
      "attempt.provider_trace_sha256"
      attempt.provider_trace_sha256
  in
  let* () =
    check_optional_sha256
      ~ordinal
      "attempt.raw_response_sha256"
      attempt.raw_response_sha256
  in
  let* () = check_http_status ~ordinal "attempt.http_status" attempt.http_status in
  let* () =
    match Hashtbl.find_opt call_ids attempt.call_id with
    | None ->
      Hashtbl.add call_ids attempt.call_id ordinal;
      Ok ()
    | Some first_ordinal ->
      Error
        (Duplicate_call_id
           { call_id = attempt.call_id; first_ordinal; duplicate_ordinal = ordinal })
  in
  let provenance = admitted.provenance in
  let binding_matches =
    String.equal attempt.plan_sha256 admitted.plan_sha256
    && String.equal attempt.request_body_sha256 admitted.request_body_sha256
    && String.equal attempt.candidate_binding_sha256 candidate.candidate_binding_sha256
    && String.equal attempt.catalog_generation_sha256 candidate.catalog_generation_sha256
    && String.equal attempt.catalog_evidence_sha256 candidate.catalog_evidence_sha256
    && String.equal provenance.candidate_binding_sha256 candidate.candidate_binding_sha256
    && String.equal
         provenance.catalog_generation_sha256
         candidate.catalog_generation_sha256
    && String.equal provenance.catalog_evidence_sha256 candidate.catalog_evidence_sha256
  in
  if not binding_matches
  then Error (Attempt_binding_mismatch { ordinal })
  else (
    match outcome with
    | Normalized_accepted _ | Normalized_semantic_rejected _ ->
      if attempt_success_state_is_valid attempt
      then Ok ()
      else Error (Invalid_attempt_state { ordinal })
    | Normalized_advance { failure; _ } ->
      if attempt_advance_state_is_valid attempt failure
      then Ok ()
      else Error (Invalid_attempt_state { ordinal }))
;;

let assurance_to_string = function
  | Json_syntax_only -> "json_syntax_only"
  | Provider_schema_requested -> "provider_schema_requested"
;;

let measurement_dispatch_to_string = function
  | No_measurement_dispatch -> "no_dispatch"
  | Measurement_dispatch_started -> "dispatch_started"
;;

let measurement_outcome_to_string = function
  | Measurement_not_required -> "not_required"
  | Measurement_succeeded -> "succeeded"
  | Measurement_unsupported -> "unsupported"
  | Measurement_local_invalid -> "local_invalid"
  | Measurement_transport_failed -> "transport_failed"
  | Measurement_invalid_response -> "invalid_response"
  | Measurement_fence_rejected -> "fence_rejected"
  | Measurement_cancelled -> "cancelled"
;;

let attempt_phase_to_string = function
  | Before_dispatch -> "before_dispatch"
  | Response_received -> "response_received"
  | Terminal -> "terminal"
;;

let candidate_json candidate =
  `Assoc
    [ "candidate_id", `String candidate.candidate_id
    ; "candidate_binding_sha256", `String candidate.candidate_binding_sha256
    ; "catalog_generation_sha256", `String candidate.catalog_generation_sha256
    ; "catalog_evidence_sha256", `String candidate.catalog_evidence_sha256
    ]
;;

let provenance_json provenance =
  `Assoc
    [ "source_schema_sha256", `String provenance.source_schema_sha256
    ; ( "effective_schema_sha256"
      , Option.fold
          ~none:`Null
          ~some:(fun value -> `String value)
          provenance.effective_schema_sha256 )
    ; "assurance", `String (assurance_to_string provenance.assurance)
    ; "candidate_binding_sha256", `String provenance.candidate_binding_sha256
    ; "catalog_generation_sha256", `String provenance.catalog_generation_sha256
    ; "catalog_evidence_sha256", `String provenance.catalog_evidence_sha256
    ]
;;

let measurement_evidence_json measurement =
  `Assoc
    [ "dispatch", `String (measurement_dispatch_to_string measurement.dispatch)
    ; "outcome", `String (measurement_outcome_to_string measurement.outcome)
    ]
;;

let admission_json = function
  | Normalized_rejected rejected ->
    `Assoc
      [ "kind", `String "rejected"
      ; "rejection", rejected.rejection.json
      ; "measurement", measurement_evidence_json rejected.measurement
      ]
  | Normalized_admitted admitted ->
    `Assoc
      [ "kind", `String "admitted"
      ; "plan_sha256", `String admitted.plan_sha256
      ; "request_body_sha256", `String admitted.request_body_sha256
      ; "provenance", provenance_json admitted.provenance
      ; "measurement", measurement_evidence_json admitted.measurement
      ]
;;

let measurement_json measurement =
  `Assoc
    [ "operation_id", `String measurement.operation_id
    ; "request_body_sha256", `String measurement.request_body_sha256
    ; "candidate_binding_sha256", `String measurement.candidate_binding_sha256
    ; "catalog_generation_sha256", `String measurement.catalog_generation_sha256
    ; "catalog_evidence_sha256", `String measurement.catalog_evidence_sha256
    ; "dispatch", `String (measurement_dispatch_to_string measurement.dispatch)
    ; "outcome", `String (measurement_outcome_to_string measurement.outcome)
    ]
;;

let attempt_json attempt =
  let optional_string = Option.fold ~none:`Null ~some:(fun value -> `String value) in
  `Assoc
    [ "call_id", `String attempt.call_id
    ; "plan_sha256", `String attempt.plan_sha256
    ; "request_body_sha256", `String attempt.request_body_sha256
    ; "candidate_binding_sha256", `String attempt.candidate_binding_sha256
    ; "catalog_generation_sha256", `String attempt.catalog_generation_sha256
    ; "catalog_evidence_sha256", `String attempt.catalog_evidence_sha256
    ; "phase", `String (attempt_phase_to_string attempt.phase)
    ; "dispatch_count", `Int attempt.dispatch_count
    ; ( "http_status"
      , Option.fold ~none:`Null ~some:(fun status -> `Int status) attempt.http_status )
    ; "provider_trace_sha256", optional_string attempt.provider_trace_sha256
    ; "raw_response_sha256", optional_string attempt.raw_response_sha256
    ]
;;

let failure_json = function
  | Candidate_rejected -> `Assoc [ "kind", `String "candidate_rejected" ]
  | Completion_failed_before_dispatch ->
    `Assoc [ "kind", `String "completion_failed_before_dispatch" ]
  | Serialized_request_refused { http_status } ->
    `Assoc
      [ "kind", `String "serialized_request_refused"; "http_status", `Int http_status ]
  | Invalid_json_output -> `Assoc [ "kind", `String "invalid_json_output" ]
;;

let outcome_json = function
  | Normalized_advance advance ->
    `Assoc
      [ "kind", `String "advance"
      ; "next_ordinal", `Int advance.next_ordinal
      ; "failure", failure_json advance.failure
      ]
  | Normalized_semantic_rejected { projector; output_sha256 } ->
    `Assoc
      [ "kind", `String "semantic_rejected"
      ; "projector", projector.json
      ; "output_sha256", `String output_sha256
      ]
  | Normalized_accepted { projector; output_sha256 } ->
    `Assoc
      [ "kind", `String "accepted"
      ; "projector", projector.json
      ; "output_sha256", `String output_sha256
      ]
;;

let step_json step =
  `Assoc
    [ "ordinal", `Int step.ordinal
    ; "admission", admission_json step.admission
    ; "measurement", Option.fold ~none:`Null ~some:measurement_json step.measurement
    ; "attempt", Option.fold ~none:`Null ~some:attempt_json step.attempt
    ; "outcome", outcome_json step.outcome
    ]
;;

let payload_json ~flow_id ~declared_candidates ~steps =
  `Assoc
    [ "flow_id", `String flow_id
    ; ( "declared_candidates"
      , `List (Array.to_list declared_candidates |> List.map candidate_json) )
    ; "steps", `List (Array.to_list steps |> List.map step_json)
    ]
;;

let payload_string ~flow_id ~declared_candidates ~steps =
  payload_json ~flow_id ~declared_candidates ~steps |> Yojson.Safe.to_string
;;

let create ~flow_id ~declared_candidates ~steps =
  let* () = if is_canonical_identifier flow_id then Ok () else Error Empty_flow_id in
  let declared_candidates = Array.of_list declared_candidates in
  let steps = Array.of_list steps in
  let declared_count = Array.length declared_candidates in
  let step_count = Array.length steps in
  let* () = if declared_count = 0 then Error Empty_declared_candidates else Ok () in
  let* () = if step_count = 0 then Error Empty_steps else Ok () in
  let* () =
    if step_count > declared_count
    then Error More_steps_than_declared_candidates
    else Ok ()
  in
  let candidate_ids = Hashtbl.create declared_count in
  let rec validate_declared position =
    if position = declared_count
    then Ok ()
    else (
      let candidate = declared_candidates.(position) in
      let* () = check_candidate candidate in
      match Hashtbl.find_opt candidate_ids candidate.candidate_id with
      | Some first_position ->
        Error
          (Duplicate_candidate_id
             { candidate_id = candidate.candidate_id
             ; first_position
             ; duplicate_position = position + 1
             })
      | None ->
        Hashtbl.add candidate_ids candidate.candidate_id (position + 1);
        validate_declared (position + 1))
  in
  let* () = validate_declared 0 in
  let call_ids = Hashtbl.create step_count in
  let measurement_ids = Hashtbl.create step_count in
  let rec validate_steps index normalized_rev =
    if index = step_count
    then Ok (List.rev normalized_rev)
    else (
      let step = steps.(index) in
      let ordinal = index + 1 in
      let is_final = index = step_count - 1 in
      let* () =
        if step.ordinal = ordinal
        then Ok ()
        else
          Error
            (Non_contiguous_step_ordinal { expected = ordinal; actual = step.ordinal })
      in
      let candidate = declared_candidates.(index) in
      let* normalized_admission =
        match step.admission with
        | Rejected rejected ->
          let* rejection =
            projector ~ordinal ~location:"admission.rejection" rejected.rejection
          in
          let* () =
            if rejected_measurement_state_is_valid rejected.measurement
            then Ok ()
            else Error (Invalid_measurement_state { ordinal })
          in
          Ok (Normalized_rejected { rejection; measurement = rejected.measurement })
        | Admitted admitted ->
          let* () = check_sha256 ~ordinal "admission.plan_sha256" admitted.plan_sha256 in
          let* () =
            check_sha256
              ~ordinal
              "admission.request_body_sha256"
              admitted.request_body_sha256
          in
          let* () = check_provenance ~ordinal candidate admitted.provenance in
          let* () =
            if admitted_measurement_state_is_valid admitted.measurement
            then Ok ()
            else Error (Invalid_measurement_state { ordinal })
          in
          Ok (Normalized_admitted admitted)
      in
      let* normalized_outcome =
        match step.outcome with
        | Advance advance -> Ok (Normalized_advance advance)
        | Semantic_rejected { projector = value; output_sha256 } ->
          let* projector =
            projector ~ordinal ~location:"outcome.semantic_rejected" value
          in
          let* () = check_sha256 ~ordinal "outcome.output_sha256" output_sha256 in
          Ok (Normalized_semantic_rejected { projector; output_sha256 })
        | Accepted { projector = value; output_sha256 } ->
          let* projector = projector ~ordinal ~location:"outcome.accepted" value in
          let* () = check_sha256 ~ordinal "outcome.output_sha256" output_sha256 in
          Ok (Normalized_accepted { projector; output_sha256 })
      in
      let* () =
        match normalized_outcome with
        | Normalized_advance advance ->
          if advance.next_ordinal = ordinal + 1 && not is_final
          then Ok ()
          else
            Error (Non_adjacent_advance { ordinal; next_ordinal = advance.next_ordinal })
        | Normalized_accepted _ when not is_final ->
          Error (Nonfinal_step_accepted { ordinal })
        | Normalized_accepted _ -> Ok ()
        | Normalized_semantic_rejected _ when is_final ->
          Error (Final_step_not_accepted { ordinal })
        | Normalized_semantic_rejected _ -> Ok ()
      in
      let ( admission_measurement
          , request_body_sha256
          , receipt_required
          , receipt_state_is_valid )
        =
        match normalized_admission with
        | Normalized_admitted admitted ->
          ( admitted.measurement
          , Some admitted.request_body_sha256
          , admitted.measurement.outcome <> Measurement_not_required
          , measurement_state_is_valid )
        | Normalized_rejected rejected ->
          rejected.measurement, None, false, rejected_measurement_receipt_state_is_valid
      in
      let* () =
        match admission_measurement.outcome, step.measurement with
        | Measurement_not_required, None -> Ok ()
        | Measurement_not_required, Some _ ->
          Error (Invalid_measurement_state { ordinal })
        | _, None ->
          if not receipt_required
          then Ok ()
          else Error (Measurement_binding_mismatch { ordinal })
        | _, Some measurement ->
          if
            admission_measurement.dispatch <> measurement.dispatch
            || admission_measurement.outcome <> measurement.outcome
          then Error (Measurement_binding_mismatch { ordinal })
          else
            check_measurement
              ~ordinal
              ~candidate
              ~request_body_sha256
              ~measurement_ids
              ~state_is_valid:receipt_state_is_valid
              measurement
      in
      let* () =
        match normalized_admission, step.attempt, normalized_outcome with
        | Normalized_rejected _, Some _, _ ->
          Error (Rejected_admission_has_attempt { ordinal })
        | ( Normalized_rejected _
          , None
          , Normalized_advance { failure = Candidate_rejected; _ } ) -> Ok ()
        | Normalized_rejected _, None, _ ->
          Error (Rejected_admission_did_not_advance { ordinal })
        | Normalized_admitted _, None, _ ->
          Error (Admitted_candidate_missing_attempt { ordinal })
        | ( Normalized_admitted admitted
          , Some attempt
          , Normalized_advance { failure = Candidate_rejected; _ } ) ->
          let _ = admitted, attempt in
          Error (Advance_failure_mismatch { ordinal })
        | Normalized_admitted admitted, Some attempt, outcome ->
          check_attempt ~ordinal ~candidate ~admitted ~call_ids ~outcome attempt
      in
      let normalized =
        { ordinal
        ; admission = normalized_admission
        ; measurement = step.measurement
        ; attempt = step.attempt
        ; outcome = normalized_outcome
        }
      in
      validate_steps (index + 1) (normalized :: normalized_rev))
  in
  let* normalized_steps = validate_steps 0 [] in
  let normalized_steps = Array.of_list normalized_steps in
  let* accepted_sha256 =
    match normalized_steps.(step_count - 1).outcome with
    | Normalized_accepted { projector; _ } -> Ok projector.digest
    | Normalized_advance _ | Normalized_semantic_rejected _ ->
      Error (Final_step_not_accepted { ordinal = step_count })
  in
  let integrity_sha256 =
    payload_string ~flow_id ~declared_candidates ~steps:normalized_steps |> digest
  in
  Ok
    { flow_id
    ; declared_candidates
    ; steps = normalized_steps
    ; integrity_sha256
    ; accepted_sha256
    }
;;

let to_string transcript =
  `Assoc
    [ "flow_id", `String transcript.flow_id
    ; ( "declared_candidates"
      , `List (Array.to_list transcript.declared_candidates |> List.map candidate_json) )
    ; "steps", `List (Array.to_list transcript.steps |> List.map step_json)
    ; "integrity_sha256", `String transcript.integrity_sha256
    ]
  |> Yojson.Safe.to_string
;;

let sha256 transcript = transcript.integrity_sha256
let accepted_domain_sha256 transcript = transcript.accepted_sha256
let invalid_fields path detail = Error (Invalid_fields { path; detail })

let exact_assoc ~path expected = function
  | `Assoc fields ->
    (match duplicate_key fields with
     | Some key -> invalid_fields path ("duplicate field: " ^ key)
     | None ->
       let actual = List.map fst fields |> List.sort String.compare in
       let expected = List.sort String.compare expected in
       if actual = expected
       then Ok fields
       else invalid_fields path "fields do not match current schema")
  | _ -> invalid_fields path "expected object"
;;

let field ~path fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> invalid_fields (path ^ "." ^ name) "missing field"
;;

let string_field ~path fields name =
  let* value = field ~path fields name in
  match value with
  | `String value -> Ok value
  | _ -> invalid_fields (path ^ "." ^ name) "expected string"
;;

let int_field ~path fields name =
  let* value = field ~path fields name in
  match value with
  | `Int value -> Ok value
  | _ -> invalid_fields (path ^ "." ^ name) "expected integer"
;;

let optional_string_field ~path fields name =
  let* value = field ~path fields name in
  match value with
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> invalid_fields (path ^ "." ^ name) "expected string or null"
;;

let optional_int_field ~path fields name =
  let* value = field ~path fields name in
  match value with
  | `Null -> Ok None
  | `Int value -> Ok (Some value)
  | _ -> invalid_fields (path ^ "." ^ name) "expected integer or null"
;;

let list_field ~path fields name =
  let* value = field ~path fields name in
  match value with
  | `List values -> Ok values
  | _ -> invalid_fields (path ^ "." ^ name) "expected array"
;;

let parse_candidate ~path json =
  let* fields =
    exact_assoc
      ~path
      [ "candidate_id"
      ; "candidate_binding_sha256"
      ; "catalog_generation_sha256"
      ; "catalog_evidence_sha256"
      ]
      json
  in
  let* candidate_id = string_field ~path fields "candidate_id" in
  let* candidate_binding_sha256 = string_field ~path fields "candidate_binding_sha256" in
  let* catalog_generation_sha256 =
    string_field ~path fields "catalog_generation_sha256"
  in
  let* catalog_evidence_sha256 = string_field ~path fields "catalog_evidence_sha256" in
  Ok
    { candidate_id
    ; candidate_binding_sha256
    ; catalog_generation_sha256
    ; catalog_evidence_sha256
    }
;;

let assurance_of_string ~path = function
  | "json_syntax_only" -> Ok Json_syntax_only
  | "provider_schema_requested" -> Ok Provider_schema_requested
  | _ -> invalid_fields path "unknown assurance"
;;

let parse_provenance ~path json =
  let* fields =
    exact_assoc
      ~path
      [ "source_schema_sha256"
      ; "effective_schema_sha256"
      ; "assurance"
      ; "candidate_binding_sha256"
      ; "catalog_generation_sha256"
      ; "catalog_evidence_sha256"
      ]
      json
  in
  let* source_schema_sha256 = string_field ~path fields "source_schema_sha256" in
  let* effective_schema_sha256 =
    optional_string_field ~path fields "effective_schema_sha256"
  in
  let* assurance_value = string_field ~path fields "assurance" in
  let* assurance = assurance_of_string ~path:(path ^ ".assurance") assurance_value in
  let* candidate_binding_sha256 = string_field ~path fields "candidate_binding_sha256" in
  let* catalog_generation_sha256 =
    string_field ~path fields "catalog_generation_sha256"
  in
  let* catalog_evidence_sha256 = string_field ~path fields "catalog_evidence_sha256" in
  Ok
    { source_schema_sha256
    ; effective_schema_sha256
    ; assurance
    ; candidate_binding_sha256
    ; catalog_generation_sha256
    ; catalog_evidence_sha256
    }
;;

let measurement_dispatch_of_string ~path = function
  | "no_dispatch" -> Ok No_measurement_dispatch
  | "dispatch_started" -> Ok Measurement_dispatch_started
  | _ -> invalid_fields path "unknown measurement dispatch"
;;

let measurement_outcome_of_string ~path = function
  | "not_required" -> Ok Measurement_not_required
  | "succeeded" -> Ok Measurement_succeeded
  | "unsupported" -> Ok Measurement_unsupported
  | "local_invalid" -> Ok Measurement_local_invalid
  | "transport_failed" -> Ok Measurement_transport_failed
  | "invalid_response" -> Ok Measurement_invalid_response
  | "fence_rejected" -> Ok Measurement_fence_rejected
  | "cancelled" -> Ok Measurement_cancelled
  | _ -> invalid_fields path "unknown measurement outcome"
;;

let parse_measurement_evidence ~path json =
  let* fields = exact_assoc ~path [ "dispatch"; "outcome" ] json in
  let* dispatch_value = string_field ~path fields "dispatch" in
  let* dispatch =
    measurement_dispatch_of_string ~path:(path ^ ".dispatch") dispatch_value
  in
  let* outcome_value = string_field ~path fields "outcome" in
  let* outcome = measurement_outcome_of_string ~path:(path ^ ".outcome") outcome_value in
  Ok { dispatch; outcome }
;;

let parse_admission ~path json =
  match json with
  | `Assoc raw_fields ->
    (match List.assoc_opt "kind" raw_fields with
     | Some (`String "rejected") ->
       let* fields = exact_assoc ~path [ "kind"; "rejection"; "measurement" ] json in
       let* rejection = field ~path fields "rejection" in
       let* measurement_json = field ~path fields "measurement" in
       let* measurement =
         parse_measurement_evidence ~path:(path ^ ".measurement") measurement_json
       in
       Ok (Rejected { rejection; measurement })
     | Some (`String "admitted") ->
       let* fields =
         exact_assoc
           ~path
           [ "kind"; "plan_sha256"; "request_body_sha256"; "provenance"; "measurement" ]
           json
       in
       let* plan_sha256 = string_field ~path fields "plan_sha256" in
       let* request_body_sha256 = string_field ~path fields "request_body_sha256" in
       let* provenance_json = field ~path fields "provenance" in
       let* provenance = parse_provenance ~path:(path ^ ".provenance") provenance_json in
       let* measurement_json = field ~path fields "measurement" in
       let* measurement =
         parse_measurement_evidence ~path:(path ^ ".measurement") measurement_json
       in
       Ok (Admitted { plan_sha256; request_body_sha256; provenance; measurement })
     | Some (`String _) -> invalid_fields (path ^ ".kind") "unknown admission kind"
     | Some _ -> invalid_fields (path ^ ".kind") "expected string"
     | None -> invalid_fields (path ^ ".kind") "missing field")
  | _ -> invalid_fields path "expected object"
;;

let parse_measurement ~path json =
  match json with
  | `Null -> Ok None
  | _ ->
    let* fields =
      exact_assoc
        ~path
        [ "operation_id"
        ; "request_body_sha256"
        ; "candidate_binding_sha256"
        ; "catalog_generation_sha256"
        ; "catalog_evidence_sha256"
        ; "dispatch"
        ; "outcome"
        ]
        json
    in
    let* operation_id = string_field ~path fields "operation_id" in
    let* request_body_sha256 = string_field ~path fields "request_body_sha256" in
    let* candidate_binding_sha256 =
      string_field ~path fields "candidate_binding_sha256"
    in
    let* catalog_generation_sha256 =
      string_field ~path fields "catalog_generation_sha256"
    in
    let* catalog_evidence_sha256 = string_field ~path fields "catalog_evidence_sha256" in
    let* dispatch_value = string_field ~path fields "dispatch" in
    let* dispatch =
      measurement_dispatch_of_string ~path:(path ^ ".dispatch") dispatch_value
    in
    let* outcome_value = string_field ~path fields "outcome" in
    let* outcome =
      measurement_outcome_of_string ~path:(path ^ ".outcome") outcome_value
    in
    Ok
      (Some
         { operation_id
         ; request_body_sha256
         ; candidate_binding_sha256
         ; catalog_generation_sha256
         ; catalog_evidence_sha256
         ; dispatch
         ; outcome
         })
;;

let attempt_phase_of_string ~path = function
  | "before_dispatch" -> Ok Before_dispatch
  | "response_received" -> Ok Response_received
  | "terminal" -> Ok Terminal
  | _ -> invalid_fields path "unknown attempt phase"
;;

let parse_attempt ~path json =
  match json with
  | `Null -> Ok None
  | _ ->
    let* fields =
      exact_assoc
        ~path
        [ "call_id"
        ; "plan_sha256"
        ; "request_body_sha256"
        ; "candidate_binding_sha256"
        ; "catalog_generation_sha256"
        ; "catalog_evidence_sha256"
        ; "phase"
        ; "dispatch_count"
        ; "http_status"
        ; "provider_trace_sha256"
        ; "raw_response_sha256"
        ]
        json
    in
    let* call_id = string_field ~path fields "call_id" in
    let* plan_sha256 = string_field ~path fields "plan_sha256" in
    let* request_body_sha256 = string_field ~path fields "request_body_sha256" in
    let* candidate_binding_sha256 =
      string_field ~path fields "candidate_binding_sha256"
    in
    let* catalog_generation_sha256 =
      string_field ~path fields "catalog_generation_sha256"
    in
    let* catalog_evidence_sha256 = string_field ~path fields "catalog_evidence_sha256" in
    let* phase_value = string_field ~path fields "phase" in
    let* phase = attempt_phase_of_string ~path:(path ^ ".phase") phase_value in
    let* dispatch_count = int_field ~path fields "dispatch_count" in
    let* http_status = optional_int_field ~path fields "http_status" in
    let* provider_trace_sha256 =
      optional_string_field ~path fields "provider_trace_sha256"
    in
    let* raw_response_sha256 = optional_string_field ~path fields "raw_response_sha256" in
    Ok
      (Some
         { call_id
         ; plan_sha256
         ; request_body_sha256
         ; candidate_binding_sha256
         ; catalog_generation_sha256
         ; catalog_evidence_sha256
         ; phase
         ; dispatch_count
         ; http_status
         ; provider_trace_sha256
         ; raw_response_sha256
         })
;;

let parse_failure ~path json =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "kind" fields with
     | Some (`String "candidate_rejected") ->
       let* _ = exact_assoc ~path [ "kind" ] json in
       Ok Candidate_rejected
     | Some (`String "completion_failed_before_dispatch") ->
       let* _ = exact_assoc ~path [ "kind" ] json in
       Ok Completion_failed_before_dispatch
     | Some (`String "serialized_request_refused") ->
       let* fields = exact_assoc ~path [ "kind"; "http_status" ] json in
       let* http_status = int_field ~path fields "http_status" in
       Ok (Serialized_request_refused { http_status })
     | Some (`String "invalid_json_output") ->
       let* _ = exact_assoc ~path [ "kind" ] json in
       Ok Invalid_json_output
     | Some (`String _) -> invalid_fields (path ^ ".kind") "unknown failure kind"
     | Some _ -> invalid_fields (path ^ ".kind") "expected string"
     | None -> invalid_fields (path ^ ".kind") "missing field")
  | _ -> invalid_fields path "expected object"
;;

let parse_outcome ~path json =
  match json with
  | `Assoc raw_fields ->
    (match List.assoc_opt "kind" raw_fields with
     | Some (`String "advance") ->
       let* fields = exact_assoc ~path [ "kind"; "next_ordinal"; "failure" ] json in
       let* next_ordinal = int_field ~path fields "next_ordinal" in
       let* failure_json = field ~path fields "failure" in
       let* failure = parse_failure ~path:(path ^ ".failure") failure_json in
       Ok (Advance { next_ordinal; failure })
     | Some (`String "semantic_rejected") ->
       let* fields = exact_assoc ~path [ "kind"; "projector"; "output_sha256" ] json in
       let* projector = field ~path fields "projector" in
       let* output_sha256 = string_field ~path fields "output_sha256" in
       Ok (Semantic_rejected { projector; output_sha256 })
     | Some (`String "accepted") ->
       let* fields = exact_assoc ~path [ "kind"; "projector"; "output_sha256" ] json in
       let* projector = field ~path fields "projector" in
       let* output_sha256 = string_field ~path fields "output_sha256" in
       Ok (Accepted { projector; output_sha256 })
     | Some (`String _) -> invalid_fields (path ^ ".kind") "unknown outcome kind"
     | Some _ -> invalid_fields (path ^ ".kind") "expected string"
     | None -> invalid_fields (path ^ ".kind") "missing field")
  | _ -> invalid_fields path "expected object"
;;

let parse_step ~index json =
  let path = Printf.sprintf "$.steps[%d]" index in
  let* fields =
    exact_assoc ~path [ "ordinal"; "admission"; "measurement"; "attempt"; "outcome" ] json
  in
  let* ordinal = int_field ~path fields "ordinal" in
  let* admission_json = field ~path fields "admission" in
  let* admission = parse_admission ~path:(path ^ ".admission") admission_json in
  let* measurement_json = field ~path fields "measurement" in
  let* measurement = parse_measurement ~path:(path ^ ".measurement") measurement_json in
  let* attempt_json = field ~path fields "attempt" in
  let* attempt = parse_attempt ~path:(path ^ ".attempt") attempt_json in
  let* outcome_json = field ~path fields "outcome" in
  let* outcome = parse_outcome ~path:(path ^ ".outcome") outcome_json in
  Ok { ordinal; admission; measurement; attempt; outcome }
;;

let rec parse_list_indexed parse index acc = function
  | [] -> Ok (List.rev acc)
  | value :: rest ->
    let* value = parse ~index value in
    parse_list_indexed parse (index + 1) (value :: acc) rest
;;

let of_string encoded =
  try
    let json = Yojson.Safe.from_string encoded in
    let* fields =
      exact_assoc
        ~path:"$"
        [ "flow_id"; "declared_candidates"; "steps"; "integrity_sha256" ]
        json
    in
    let* flow_id = string_field ~path:"$" fields "flow_id" in
    let* declared_json = list_field ~path:"$" fields "declared_candidates" in
    let* declared_candidates =
      parse_list_indexed
        (fun ~index value ->
           parse_candidate ~path:(Printf.sprintf "$.declared_candidates[%d]" index) value)
        0
        []
        declared_json
    in
    let* steps_json = list_field ~path:"$" fields "steps" in
    let* steps = parse_list_indexed parse_step 0 [] steps_json in
    let* encoded_integrity = string_field ~path:"$" fields "integrity_sha256" in
    if not (is_sha256 encoded_integrity)
    then invalid_fields "$.integrity_sha256" "expected lowercase SHA-256"
    else (
      match create ~flow_id ~declared_candidates ~steps with
      | Error error -> Error (Invalid_transcript error)
      | Ok transcript ->
        if not (String.equal encoded_integrity transcript.integrity_sha256)
        then Error Integrity_mismatch
        else if not (String.equal encoded (to_string transcript))
        then Error Non_canonical_encoding
        else Ok transcript)
  with
  | Yojson.Json_error detail -> Error (Malformed_json detail)
  | Stack_overflow -> Error (Malformed_json "JSON nesting exhausted the parser stack")
;;

let invariant_error_to_string = function
  | Empty_flow_id -> "flow_id must be nonempty and canonical"
  | Empty_declared_candidates -> "declared candidate snapshot must be nonempty"
  | Empty_steps -> "successful transcript must contain at least one step"
  | Non_canonical_identifier { field; ordinal } ->
    Printf.sprintf
      "%s%s must be nonempty and canonical"
      (Option.fold
         ~none:""
         ~some:(fun ordinal -> Printf.sprintf "step %d " ordinal)
         ordinal)
      field
  | Invalid_sha256 { field; ordinal } ->
    Printf.sprintf
      "%s%s must be a lowercase SHA-256"
      (Option.fold
         ~none:""
         ~some:(fun ordinal -> Printf.sprintf "step %d " ordinal)
         ordinal)
      field
  | Invalid_http_status { field; ordinal } ->
    Printf.sprintf "step %d %s must be a valid HTTP status" ordinal field
  | Duplicate_candidate_id { candidate_id; first_position; duplicate_position } ->
    Printf.sprintf
      "candidate_id %s is duplicated at positions %d and %d"
      candidate_id
      first_position
      duplicate_position
  | Duplicate_call_id { call_id; first_ordinal; duplicate_ordinal } ->
    Printf.sprintf
      "call_id %s is duplicated at steps %d and %d"
      call_id
      first_ordinal
      duplicate_ordinal
  | Duplicate_measurement_operation_id { operation_id; first_ordinal; duplicate_ordinal }
    ->
    Printf.sprintf
      "measurement operation_id %s is duplicated at steps %d and %d"
      operation_id
      first_ordinal
      duplicate_ordinal
  | More_steps_than_declared_candidates ->
    "visited steps exceed the declared candidate snapshot"
  | Non_contiguous_step_ordinal { expected; actual } ->
    Printf.sprintf "step ordinal is not contiguous: expected %d, got %d" expected actual
  | Invalid_measurement_state { ordinal } ->
    Printf.sprintf "step %d measurement dispatch/outcome state is invalid" ordinal
  | Measurement_binding_mismatch { ordinal } ->
    Printf.sprintf "step %d measurement binding does not match its candidate" ordinal
  | Rejected_admission_has_attempt { ordinal } ->
    Printf.sprintf "step %d rejected admission has a generation attempt" ordinal
  | Rejected_admission_did_not_advance { ordinal } ->
    Printf.sprintf "step %d rejected admission did not advance as rejected" ordinal
  | Admitted_candidate_missing_attempt { ordinal } ->
    Printf.sprintf "step %d admitted candidate has no generation attempt" ordinal
  | Attempt_binding_mismatch { ordinal } ->
    Printf.sprintf "step %d attempt/provenance binding is inconsistent" ordinal
  | Invalid_attempt_state { ordinal } ->
    Printf.sprintf "step %d attempt phase/dispatch/status state is invalid" ordinal
  | Non_adjacent_advance { ordinal; next_ordinal } ->
    Printf.sprintf "step %d advances to nonadjacent ordinal %d" ordinal next_ordinal
  | Advance_failure_mismatch { ordinal } ->
    Printf.sprintf "step %d advance failure contradicts its admission" ordinal
  | Nonfinal_step_accepted { ordinal } ->
    Printf.sprintf "nonfinal step %d is accepted" ordinal
  | Final_step_not_accepted { ordinal } ->
    Printf.sprintf "final step %d is not accepted" ordinal
  | Invalid_projector_json { ordinal; location } ->
    Printf.sprintf "step %d %s is not canonicalizable JSON" ordinal location
;;

let decode_error_to_string = function
  | Malformed_json detail -> "validated flow evidence malformed JSON: " ^ detail
  | Invalid_fields { path; detail } ->
    Printf.sprintf "validated flow evidence invalid at %s: %s" path detail
  | Invalid_transcript error ->
    "validated flow evidence violates an invariant: " ^ invariant_error_to_string error
  | Integrity_mismatch -> "validated flow evidence integrity mismatch"
  | Non_canonical_encoding -> "validated flow evidence is not canonical current JSON"
;;
