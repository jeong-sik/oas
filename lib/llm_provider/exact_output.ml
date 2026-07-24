module Plan = Exact_output_plan
module Exec = Exact_output_execution
module Flow_state = Exact_output_flow
module Flow_contract = Exact_output_flow_contract
module Gemini_schema = Exact_output_gemini_schema
module Trace = Exact_output_provider_trace
module Caps = Capabilities
module PC = Provider_config
include Exact_output_resolver
include Flow_contract

type schema_fingerprint = Schema_fingerprint of string
type domain_schema = Domain_schema of Yojson.Safe.t

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type output_requirement =
  { schema : domain_schema
  ; source_schema_fingerprint : schema_fingerprint
  ; minimum_guarantee : minimum_guarantee
  }

type plan_provenance =
  { source_schema_fingerprint : schema_fingerprint
  ; effective_schema_fingerprint : schema_fingerprint option
  ; actual_assurance : actual_assurance
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type attempt_state =
  | Not_started_state
  | Before_dispatch_state
  | Dispatch_started_state
  | Response_received_state of int option
  | Terminal_state of int

type call_id = Call_id of string
type provider_trace = Trace.t

type receipt =
  { state : attempt_state Atomic.t
  ; provider_trace_state : provider_trace option Atomic.t
  ; call_id : call_id
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type ready_plan =
  { plan : Plan.t
  ; provenance : plan_provenance
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type attempt =
  { ready : ready_plan
  ; receipt : receipt
  }

type wire_admission_error =
  | Capability_snapshot_missing
  | Inconsistent_output_contract
  | Output_contract_unavailable
  | Cross_feature_not_allowed
  | Global_admission_not_allowed
  | Invalid_connect_timeout
  | Invalid_body_timeout
  | Caller_supplied_header_not_allowed
  | Unsupported_image_input
  | Unsupported_document_input
  | Unsupported_audio_input
  | Unsupported_system_prompt
  | Token_measurement_required of Serving_constraint.t
  | Unsupported_target_model of { model_id : string }
  | Target_request_rejected
  | Request_body_too_large of
      { actual_bytes : int
      ; limit_bytes : int
      }
  | Request_serialization_rejected

type admission_error =
  | Provider_schema_unavailable
  | Json_syntax_unavailable
  | Unsupported_schema_keyword of string
  | Unsupported_schema_type of string
  | Invalid_schema
  | Wire_admission_rejected of wire_admission_error

type input_capacity_disposition =
  | Token_measurement_required of
      { accepted_through_tokens : int
      ; rejected_from_tokens : int option
      }
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

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

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
  }

type admitted_flow_candidate =
  { visit : flow_candidate_visit
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; provenance : plan_provenance
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
  ; progress : (candidate_admission, flow_attempt_receipt) Flow_state.progress
  }

type flow_candidate_error = Blank_flow_candidate_id

type flow_snapshot_error =
  | Duplicate_flow_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }
  | Flow_preference_capacity_exhausted of { capacity : int }

type start_attempt_error = Call_id_generation_failed of string
type flow_start_error = Flow_id_generation_failed of string

type flow_evidence =
  { flow_id : flow_id
  ; scope : flow_scope
  ; declared_candidate_snapshot : flow_candidate_identity list
  ; candidate_snapshot : flow_candidate_identity list
  ; preference_observation : flow_preference_observation
  ; candidate_visit_count : candidate_visit_count
  ; admissions : candidate_admission list
  ; attempts : flow_attempt_receipt list
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

type flow_candidate_failure =
  | Flow_candidate_rejected of candidate_rejection_receipt
  | Flow_candidate_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      }

type outward_dispatch_fact =
  | No_outward_dispatch
  | Outward_dispatch_started

type 'callback_error flow_execution_error =
  | Flow_attempt_already_started of flow_evidence
  | Flow_success_ordinal_exhausted of flow_evidence
  | Flow_attempt_start_failed of
      { candidate : flow_candidate_visit
      ; cause : start_attempt_error
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
  | Flow_step_before_dispatch_callback_failed of flow_attempt_receipt * 'callback_error
  | Flow_step_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      }

let ( let* ) = Result.bind

let rec canonical_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, canonical_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map canonical_json values)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar -> scalar
;;

let fingerprint_schema schema =
  canonical_json schema
  |> Yojson.Safe.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
  |> fun value -> Schema_fingerprint value
;;

let schema_fingerprint_to_string (Schema_fingerprint value) = value

let make_output_requirement ~schema ~minimum_guarantee =
  { schema = Domain_schema schema
  ; source_schema_fingerprint = fingerprint_schema schema
  ; minimum_guarantee
  }
;;

let validate_gemini_schema ~path schema =
  match Gemini_schema.validate ~path schema with
  | Ok () -> Ok ()
  | Error (Gemini_schema.Unsupported_keyword keyword) ->
    Error (Unsupported_schema_keyword keyword)
  | Error (Gemini_schema.Unsupported_type type_name) ->
    Error (Unsupported_schema_type type_name)
  | Error Gemini_schema.Invalid_schema -> Error Invalid_schema
;;

let schema_for_wire target (Domain_schema domain_schema) =
  match Provider_http_codec.(json_schema_wire (of_config target.config)) with
  | Raw_schema -> domain_schema
  | Openai_named_schema ->
    `Assoc
      [ "name", `String (Provider_config.structured_output_name_of_schema domain_schema)
      ; "schema", domain_schema
      ; "strict", `Bool true
      ]
;;

let response_format target requirement =
  match
    Caps.structured_output_support target.capabilities, requirement.minimum_guarantee
  with
  | Caps.Native_json_schema, (Json_syntax | Provider_schema) ->
    let* () =
      match target.config.kind, requirement.schema with
      | PC.Gemini, Domain_schema schema -> validate_gemini_schema ~path:"$" schema
      | ( (PC.Anthropic | PC.Kimi | PC.OpenAI_compat | PC.Ollama | PC.Glm | PC.DashScope)
        , Domain_schema _ ) -> Ok ()
    in
    let wire_schema = schema_for_wire target requirement.schema in
    Ok
      ( Types.JsonSchema wire_schema
      , Provider_schema_requested
      , Some (fingerprint_schema wire_schema) )
  | Caps.Json_object_only, Json_syntax -> Ok (Types.JsonMode, Json_syntax_only, None)
  | Caps.Json_object_only, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Provider_schema -> Error Provider_schema_unavailable
  | Caps.No_structured_output, Json_syntax -> Error Json_syntax_unavailable
;;

let exact_config target response_format =
  let output_schema = PC.output_schema_of_response_format response_format in
  { target.config with
    temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; system_prompt = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; clear_thinking = None
  ; tool_stream = false
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; response_format
  ; output_schema
  ; cache_system_prompt = false
  ; keep_alive = None
  ; internal_model_rotation_count = None
  ; previous_response_id = None
  ; max_concurrent_requests = None
  ; model_capabilities_override = Some target.capabilities
  }
;;

let wire_admission_error = function
  | Plan.Explicit_capability_snapshot_required -> Capability_snapshot_missing
  | Plan.Contradictory_output_state -> Inconsistent_output_contract
  | Plan.Unsupported_output_contract _ -> Output_contract_unavailable
  | Plan.Unsupported_exact_cross_feature -> Cross_feature_not_allowed
  | Plan.Global_admission_not_allowed -> Global_admission_not_allowed
  | Plan.Invalid_connect_timeout _ -> Invalid_connect_timeout
  | Plan.Invalid_body_timeout _ -> Invalid_body_timeout
  | Plan.Caller_supplied_header_not_allowed _ -> Caller_supplied_header_not_allowed
  | Plan.Unsupported_image_input -> Unsupported_image_input
  | Plan.Unsupported_document_input -> Unsupported_document_input
  | Plan.Unsupported_audio_input -> Unsupported_audio_input
  | Plan.Unsupported_system_prompt -> Unsupported_system_prompt
  | Plan.Token_measurement_required constraint_ -> Token_measurement_required constraint_
  | Plan.Provider_request_rejected _ -> Target_request_rejected
  | Plan.Request_body_too_large { actual_bytes; limit_bytes } ->
    Request_body_too_large { actual_bytes; limit_bytes }
  | Plan.Request_serialization_rejected _ -> Request_serialization_rejected
;;

let admit ~target ~messages requirement =
  let* () =
    if Exact_output_resolver.selected_target_model_admitted target
    then Ok ()
    else
      Error
        (Wire_admission_rejected
           (Unsupported_target_model { model_id = target.config.model_id }))
  in
  let* response_format, actual_assurance, effective_schema_fingerprint =
    response_format target requirement
  in
  Plan.admit
    (Plan.Unmeasured
       { config = exact_config target response_format
       ; messages
       ; body_timeout_s = target.body_timeout_s
       ; anthropic_thinking_control = target.anthropic_thinking_control
       })
  |> Result.map_error (fun error -> Wire_admission_rejected (wire_admission_error error))
  |> Result.map (fun plan ->
    let request_body_sha256 = Plan.request_body_sha256 plan in
    let plan_fingerprint =
      hash_parts
        [ "oas-exact-output-ready-plan-v2"
        ; request_body_sha256
        ; catalog_generation_fingerprint target.generation
        ; target_identity_fingerprint target.identity
        ; Provider_http_codec.fingerprint_tag (Plan.response_codec plan)
        ; option_float (Plan.connect_timeout_s plan)
        ; option_float (Plan.body_timeout_s plan)
        ]
    in
    { plan
    ; provenance =
        { source_schema_fingerprint = requirement.source_schema_fingerprint
        ; effective_schema_fingerprint
        ; actual_assurance
        ; catalog_generation = target.generation
        ; catalog_evidence = target.evidence
        ; target_identity = target.identity
        }
    ; plan_fingerprint
    ; request_body_sha256
    ; catalog_generation = target.generation
    ; catalog_evidence = target.evidence
    ; target_identity = target.identity
    })
;;

let plan_provenance (ready : ready_plan) = ready.provenance
let plan_fingerprint (ready : ready_plan) = ready.plan_fingerprint

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
      { state = Atomic.make Not_started_state
      ; provider_trace_state = Atomic.make None
      ; call_id = Call_id id
      ; plan_fingerprint = ready.plan_fingerprint
      ; request_body_sha256 = ready.request_body_sha256
      ; catalog_generation = ready.catalog_generation
      ; catalog_evidence = ready.catalog_evidence
      ; target_identity = ready.target_identity
      }
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

let settle_flow_domain success disposition =
  Flow_contract.settle_domain
    success.domain_settlement
    success.preferences
    success.scope
    ~reservation:success.preference_reservation
    ~candidate:success.candidate.visit.identity
    ~success_ordinal:success.success_ordinal
    disposition
;;

let call_id_to_string (Call_id id) = id
let flow_id_to_string (Flow_id id) = id
let flow_visit_ordinal_to_int (Flow_visit_ordinal ordinal) = ordinal
let flow_attempt_id (flow : flow_attempt) = flow.flow_id
let attempt_receipt (attempt : attempt) = attempt.receipt
let receipt_call_id (receipt : receipt) = receipt.call_id

let receipt_phase receipt =
  match Atomic.get receipt.state with
  | Not_started_state -> Not_started
  | Before_dispatch_state -> Before_dispatch
  | Dispatch_started_state -> Dispatch_started
  | Response_received_state _ -> Response_received
  | Terminal_state _ -> Terminal
;;

let receipt_dispatch_count receipt =
  match Atomic.get receipt.state with
  | Not_started_state | Before_dispatch_state -> 0
  | Dispatch_started_state | Response_received_state _ | Terminal_state _ -> 1
;;

let outward_dispatch_fact_of_receipt receipt =
  match Atomic.get receipt.state with
  | Not_started_state | Before_dispatch_state -> No_outward_dispatch
  | Dispatch_started_state | Response_received_state _ | Terminal_state _ ->
    Outward_dispatch_started
;;

let flow_execution_error_outward_dispatch = function
  | Flow_attempt_already_started _
  | Flow_attempt_start_failed _
  | Flow_before_dispatch_callback_failed _
  | Flow_before_advance_callback_failed _
  | Flow_candidates_exhausted _ -> No_outward_dispatch
  | Flow_success_ordinal_exhausted _ -> Outward_dispatch_started
  | Flow_exact_execution_failed { cause; _ } ->
    outward_dispatch_fact_of_receipt cause.receipt
;;

let receipt_http_status receipt =
  match Atomic.get receipt.state with
  | Response_received_state status -> status
  | Terminal_state status -> Some status
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let receipt_provider_trace receipt = Atomic.get receipt.provider_trace_state
let provider_trace_fingerprint = Trace.fingerprint
let receipt_plan_fingerprint (receipt : receipt) = receipt.plan_fingerprint
let receipt_request_body_sha256 (receipt : receipt) = receipt.request_body_sha256
let receipt_catalog_generation (receipt : receipt) = receipt.catalog_generation
let receipt_catalog_evidence (receipt : receipt) = receipt.catalog_evidence
let receipt_target_identity (receipt : receipt) = receipt.target_identity
let candidate_visit_count_to_int (Candidate_visit_count count) = count

let candidate_rejection_identity (receipt : candidate_rejection_receipt) =
  receipt.visit.identity
;;

let candidate_rejection_scope (receipt : candidate_rejection_receipt) = receipt.scope
let candidate_rejection_visit (receipt : candidate_rejection_receipt) = receipt.visit
let candidate_rejection_phase _ = Before_dispatch
let candidate_rejection_dispatch_count _ = 0

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
         { accepted_through_tokens =
             constraint_.Serving_constraint.observation.accepted_through
         ; rejected_from_tokens = constraint_.observation.rejected_from
         })
  | Request_body_too_large { actual_bytes; limit_bytes } ->
    Input_capacity (Serialized_request_body_too_large { actual_bytes; limit_bytes })
  | Target_request_rejected | Request_serialization_rejected -> Request_preparation_failed
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
  ; admissions = progress.admissions
  ; attempts = progress.attempts
  }
;;

let record_attempt flow = Flow_state.record_attempt flow.progress

let state_rank = function
  | Not_started_state -> 0
  | Before_dispatch_state -> 1
  | Dispatch_started_state -> 2
  | Response_received_state _ -> 3
  | Terminal_state _ -> 4
;;

let rec advance receipt desired =
  let current = Atomic.get receipt.state in
  let adds_information =
    state_rank desired > state_rank current
    ||
    match current, desired with
    | Response_received_state None, Response_received_state (Some _) -> true
    | _ -> false
  in
  if adds_information
  then
    if not (Atomic.compare_and_set receipt.state current desired)
    then advance receipt desired
;;

let observe_phase receipt = function
  | Http_client_phase_observer.Dispatch_started -> advance receipt Dispatch_started_state
  | Http_client_phase_observer.Response_received status ->
    advance receipt (Response_received_state (Some status))
;;

let synchronize_receipt receipt complete_receipt =
  match Exec.receipt_phase complete_receipt with
  | Exec.Before_dispatch -> advance receipt Before_dispatch_state
  | Exec.Dispatch_started -> advance receipt Dispatch_started_state
  | Exec.Response_received ->
    advance receipt (Response_received_state (Exec.receipt_http_status complete_receipt))
  | Exec.Terminal ->
    (match Exec.receipt_http_status complete_receipt with
     | Some status -> advance receipt (Terminal_state status)
     | None -> invalid_arg "Exact_output: terminal receipt without HTTP status")
;;

let raw_response = Trace.raw_response
let record_provider_trace receipt = Trace.record_once receipt.provider_trace_state

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

let execute_once ~net ?clock (attempt : attempt) =
  let ready = attempt.ready in
  let receipt = attempt.receipt in
  if not (Atomic.compare_and_set receipt.state Not_started_state Before_dispatch_state)
  then
    Error
      { call_id = receipt.call_id
      ; receipt
      ; cause = Attempt_already_started
      ; raw_response = None
      }
  else (
    match
      Exec.execute_once_with_evidence
        ~net
        ?clock
        ~on_phase:(observe_phase receipt)
        ready.plan
    with
    | Error
        ({ receipt = complete_receipt; cause; raw_response = evidence } :
          Exec.execute_once_error_with_evidence) ->
      synchronize_receipt receipt complete_receipt;
      Option.iter
        (fun response_evidence ->
           response_evidence
           |> Trace.of_evidence complete_receipt
           |> record_provider_trace receipt)
        evidence;
      Error
        { call_id = receipt.call_id
        ; receipt
        ; cause = execution_error_cause cause
        ; raw_response = Option.map raw_response evidence
        }
    | Ok { outcome; raw_response = evidence } ->
      synchronize_receipt receipt outcome.receipt;
      let provider_trace =
        Trace.of_evidence ~response:outcome.response outcome.receipt evidence
      in
      record_provider_trace receipt provider_trace;
      (match outcome.output with
       | Exec.Json_output { value; _ } ->
         Ok
           { call_id = receipt.call_id
           ; receipt
           ; output = value
           ; provenance = ready.provenance
           ; raw_response = raw_response evidence
           }
       | Exec.Text_output _ ->
         Error
           { call_id = receipt.call_id
           ; receipt
           ; cause = Internal_non_json_output
           ; raw_response = Some (raw_response evidence)
           }))
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
  }
;;

let record_candidate_rejection (flow : flow_attempt) visit cause =
  let rejection = { scope = flow.scope; visit; cause } in
  Flow_state.record_admission flow.progress (Candidate_rejected rejection);
  rejection
;;

let execute_flow_candidate
      ~net
      ?clock
      ~before_dispatch
      flow
      (candidate : flow_candidate_step)
  =
  let reject cause =
    let rejection = record_candidate_rejection flow candidate.visit cause in
    Error (Flow_step_candidate_rejected rejection)
  in
  match resolve_target candidate.admitted_target with
  | Error cause -> reject (Target_selection_rejected cause)
  | Ok target ->
    (match admit ~target ~messages:flow.messages flow.requirement with
     | Error cause -> reject (Request_admission_rejected cause)
     | Ok plan ->
       let admitted = admitted_flow_candidate candidate.visit plan in
       Flow_state.record_admission flow.progress (Candidate_admitted admitted);
       (match start_attempt plan with
        | Error cause -> Error (Flow_step_attempt_start_failed (candidate.visit, cause))
        | Ok attempt ->
          let candidate_receipt =
            { scope = flow.scope
            ; visit = candidate.visit
            ; receipt = attempt_receipt attempt
            }
          in
          record_attempt flow candidate_receipt;
          (match before_dispatch candidate_receipt with
           | Error cause ->
             Error (Flow_step_before_dispatch_callback_failed (candidate_receipt, cause))
           | Ok () ->
             (match execute_once ~net ?clock attempt with
              | Ok success -> Ok (candidate_receipt, success)
              | Error cause ->
                Error
                  (Flow_step_execution_failed { candidate = candidate_receipt; cause })))))
;;

let advanceable_flow_failure = function
  | Flow_step_candidate_rejected receipt -> Some (Flow_candidate_rejected receipt)
  | Flow_step_execution_failed ({ cause; _ } as failure)
    when execution_failure_may_advance cause ->
    Some
      (Flow_candidate_execution_failed
         { candidate = failure.candidate; cause = failure.cause })
  | Flow_step_execution_failed _
  | Flow_step_attempt_start_failed _
  | Flow_step_before_dispatch_callback_failed _ -> None
;;

let execute_flow_once ~net ?clock ~before_dispatch ~before_advance flow =
  let outcome =
    Flow_state.execute_once
      flow.execution
      ~candidates:flow.candidates
      ~execute:(execute_flow_candidate ~net ?clock ~before_dispatch flow)
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
     | Flow_step_before_dispatch_callback_failed (candidate, cause) ->
       Error (Flow_before_dispatch_callback_failed { candidate; cause; evidence })
     | Flow_step_execution_failed { candidate; cause; _ } ->
       Error (Flow_exact_execution_failed { candidate; cause; evidence }))
;;
