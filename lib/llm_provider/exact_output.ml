module Plan = Exact_output_plan
module Exec = Exact_output_execution
module Flow_state = Exact_output_flow
module Caps = Capabilities
module PC = Provider_config
include Exact_output_resolver

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

type receipt =
  { state : attempt_state Atomic.t
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
  | Unsupported_target_model of { model_id : string }
  | Target_request_rejected
  | Request_serialization_rejected

type admission_error =
  | Provider_schema_unavailable
  | Json_syntax_unavailable
  | Unsupported_schema_keyword of string
  | Unsupported_schema_type of string
  | Invalid_schema
  | Wire_admission_rejected of wire_admission_error

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

type raw_response =
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

type flow_candidate_identity =
  { candidate_id : string
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type flow_candidate =
  { identity : flow_candidate_identity
  ; target : selected_target
  }

type admitted_flow_candidate =
  { identity : flow_candidate_identity
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; provenance : plan_provenance
  }

type candidate_admission =
  | Candidate_admitted of admitted_flow_candidate
  | Candidate_rejected of
      { identity : flow_candidate_identity
      ; cause : admission_error
      }

type ready_flow_candidate =
  { evidence : admitted_flow_candidate
  ; plan : ready_plan
  }

type ready_flow =
  { admissions : candidate_admission list
  ; candidates : ready_flow_candidate list
  }

type flow_attempt_candidate =
  { evidence : admitted_flow_candidate
  ; attempt : attempt
  }

type flow_attempt =
  { execution : Flow_state.t
  ; admissions : candidate_admission list
  ; candidates : flow_attempt_candidate list
  }

type flow_candidate_error = Blank_flow_candidate_id

type flow_admission_error =
  | Duplicate_flow_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }
  | No_admitted_flow_candidates of candidate_admission list

type start_attempt_error = Call_id_generation_failed of string

type flow_start_error =
  | Flow_candidate_attempt_start_failed of
      { identity : flow_candidate_identity
      ; position : int
      ; cause : start_attempt_error
      ; admissions : candidate_admission list
      }

type flow_attempt_receipt =
  { identity : flow_candidate_identity
  ; receipt : receipt
  }

type flow_evidence =
  { admissions : candidate_admission list
  ; attempts : flow_attempt_receipt list
  }

type flow_success =
  { candidate : flow_attempt_receipt
  ; success : success
  ; evidence : flow_evidence
  }

type 'callback_error flow_execution_error =
  | Flow_attempt_already_started of flow_evidence
  | Flow_before_dispatch_callback_failed of
      { candidate : flow_attempt_receipt
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_before_advance_callback_failed of
      { failed : flow_attempt_receipt
      ; failure : execution_error
      ; next : flow_attempt_receipt
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_exact_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      ; evidence : flow_evidence
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

let gemini_schema_keywords =
  [ "type"
  ; "title"
  ; "description"
  ; "properties"
  ; "required"
  ; "additionalProperties"
  ; "enum"
  ; "format"
  ; "minimum"
  ; "maximum"
  ; "items"
  ; "prefixItems"
  ; "minItems"
  ; "maxItems"
  ]
;;

let gemini_keywords_for_type = function
  | "object" ->
    [ "type"; "title"; "description"; "properties"; "required"; "additionalProperties" ]
  | "string" -> [ "type"; "title"; "description"; "enum"; "format" ]
  | "number" | "integer" ->
    [ "type"; "title"; "description"; "enum"; "minimum"; "maximum" ]
  | "array" ->
    [ "type"; "title"; "description"; "items"; "prefixItems"; "minItems"; "maxItems" ]
  | "boolean" | "null" -> [ "type"; "title"; "description" ]
  | type_name -> raise_notrace (Invalid_argument type_name)
;;

let assoc_keys_are_unique fields =
  let keys = List.map fst fields in
  List.length keys = List.length (List.sort_uniq String.compare keys)
;;

let json_number = function
  | `Int _ | `Intlit _ | `Float _ -> true
  | `Null | `Bool _ | `String _ | `Assoc _ | `List _ -> false
;;

let json_integer = function
  | `Int _ | `Intlit _ -> true
  | `Null | `Bool _ | `Float _ | `String _ | `Assoc _ | `List _ -> false
;;

let gemini_non_null_schema_types =
  [ "string"; "number"; "integer"; "boolean"; "object"; "array" ]
;;

let gemini_schema_base_type = function
  | Some (`String type_name)
    when String.equal type_name "null" || List.mem type_name gemini_non_null_schema_types
    -> Ok type_name
  | Some (`String type_name) -> Error (Unsupported_schema_type type_name)
  | Some (`List [ `String left; `String right ])
    when String.equal left "null" && List.mem right gemini_non_null_schema_types ->
    Ok right
  | Some (`List [ `String left; `String right ])
    when String.equal right "null" && List.mem left gemini_non_null_schema_types ->
    Ok left
  | Some (`List _) | Some _ | None -> Error Invalid_schema
;;

let rec validate_gemini_schema ~path = function
  | `Assoc fields when assoc_keys_are_unique fields ->
    (match
       List.find_opt
         (fun (keyword, _) -> not (List.mem keyword gemini_schema_keywords))
         fields
     with
     | Some (keyword, _) -> Error (Unsupported_schema_keyword (path ^ "." ^ keyword))
     | None ->
       (match gemini_schema_base_type (List.assoc_opt "type" fields) with
        | Ok type_name ->
          let supported = gemini_keywords_for_type type_name in
          (match
             List.find_opt (fun (keyword, _) -> not (List.mem keyword supported)) fields
           with
           | Some (keyword, _) ->
             Error (Unsupported_schema_keyword (path ^ "." ^ keyword))
           | None -> validate_gemini_schema_fields ~path ~type_name fields)
        | Error _ as error -> error))
  | `Assoc _ | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
    Error Invalid_schema

and validate_gemini_schema_fields ~path ~type_name fields =
  let rec validate_all = function
    | [] -> Ok ()
    | field :: rest ->
      let* () = validate_gemini_schema_field ~path ~type_name field in
      validate_all rest
  in
  validate_all fields

and validate_gemini_schema_field ~path ~type_name = function
  | "type", (`String _ | `List _) -> Ok ()
  | ("title" | "description"), `String _ -> Ok ()
  | "properties", `Assoc properties
    when String.equal type_name "object" && assoc_keys_are_unique properties ->
    let rec validate = function
      | [] -> Ok ()
      | (name, schema) :: rest ->
        let* () = validate_gemini_schema ~path:(path ^ ".properties." ^ name) schema in
        validate rest
    in
    validate properties
  | "required", `List names
    when String.equal type_name "object"
         && List.for_all
              (function
                | `String _ -> true
                | _ -> false)
              names -> Ok ()
  | "additionalProperties", `Bool _ when String.equal type_name "object" -> Ok ()
  | "additionalProperties", schema when String.equal type_name "object" ->
    validate_gemini_schema ~path:(path ^ ".additionalProperties") schema
  | "enum", `List values
    when values <> []
         && String.equal type_name "string"
         && List.for_all
              (function
                | `String _ -> true
                | _ -> false)
              values -> Ok ()
  | "enum", `List values
    when values <> []
         && String.equal type_name "number"
         && List.for_all json_number values -> Ok ()
  | "enum", `List values
    when values <> []
         && String.equal type_name "integer"
         && List.for_all json_integer values -> Ok ()
  | "format", `String _ when String.equal type_name "string" -> Ok ()
  | ("minimum" | "maximum"), value
    when (String.equal type_name "number" || String.equal type_name "integer")
         && json_number value -> Ok ()
  | "items", schema when String.equal type_name "array" ->
    validate_gemini_schema ~path:(path ^ ".items") schema
  | "prefixItems", `List schemas when String.equal type_name "array" ->
    let rec validate index = function
      | [] -> Ok ()
      | schema :: rest ->
        let* () =
          validate_gemini_schema
            ~path:(Printf.sprintf "%s.prefixItems[%d]" path index)
            schema
        in
        validate (index + 1) rest
    in
    validate 0 schemas
  | ("minItems" | "maxItems"), `Int value
    when String.equal type_name "array" && value >= 0 -> Ok ()
  | _ -> Error Invalid_schema
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
  | Plan.Provider_request_rejected _ -> Target_request_rejected
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

let make_flow_candidate ~id ~target =
  let id = String.trim id in
  if String.equal id ""
  then Error Blank_flow_candidate_id
  else
    Ok
      { identity =
          { candidate_id = id
          ; catalog_generation = selected_target_catalog_generation target
          ; catalog_evidence = selected_target_catalog_evidence target
          ; target_identity = selected_target_identity target
          }
      ; target
      }
;;

let flow_candidate_identity (candidate : flow_candidate) = candidate.identity

let duplicate_flow_candidate_id (candidates : flow_candidate list) =
  let rec find position seen = function
    | [] -> None
    | (candidate : flow_candidate) :: rest ->
      let candidate_id = candidate.identity.candidate_id in
      (match
         List.find_opt (fun (seen_id, _) -> String.equal seen_id candidate_id) seen
       with
       | Some (_, first_position) ->
         Some
           (Duplicate_flow_candidate_id
              { candidate_id; first_position; duplicate_position = position })
       | None -> find (position + 1) ((candidate_id, position) :: seen) rest)
  in
  find 1 [] candidates
;;

let admit_flow ~first ~rest ~messages requirement =
  let candidates = first :: rest in
  match duplicate_flow_candidate_id candidates with
  | Some duplicate -> Error duplicate
  | None ->
    let admissions, admitted =
      List.fold_left
        (fun (admissions, admitted) candidate ->
           match admit ~target:candidate.target ~messages requirement with
           | Error cause ->
             ( Candidate_rejected { identity = candidate.identity; cause } :: admissions
             , admitted )
           | Ok plan ->
             let evidence =
               { identity = candidate.identity
               ; plan_fingerprint = plan.plan_fingerprint
               ; request_body_sha256 = plan.request_body_sha256
               ; provenance = plan.provenance
               }
             in
             Candidate_admitted evidence :: admissions, { evidence; plan } :: admitted)
        ([], [])
        candidates
    in
    let admissions = List.rev admissions in
    (match List.rev admitted with
     | [] -> Error (No_admitted_flow_candidates admissions)
     | candidates -> Ok { admissions; candidates })
;;

let ready_flow_admissions (ready : ready_flow) = ready.admissions

let start_attempt (ready : ready_plan) =
  match Exact_output_call_id.create () with
  | Error detail -> Error (Call_id_generation_failed detail)
  | Ok id ->
    let receipt =
      { state = Atomic.make Not_started_state
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

let start_flow (ready : ready_flow) =
  let rec start position started = function
    | [] ->
      Ok
        { execution = Flow_state.create ()
        ; admissions = ready.admissions
        ; candidates = List.rev started
        }
    | candidate :: rest ->
      (match start_attempt candidate.plan with
       | Error cause ->
         Error
           (Flow_candidate_attempt_start_failed
              { identity = candidate.evidence.identity
              ; position
              ; cause
              ; admissions = ready.admissions
              })
       | Ok attempt ->
         start (position + 1) ({ evidence = candidate.evidence; attempt } :: started) rest)
  in
  start 1 [] ready.candidates
;;

let call_id_to_string (Call_id id) = id
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

let receipt_http_status receipt =
  match Atomic.get receipt.state with
  | Response_received_state status -> status
  | Terminal_state status -> Some status
  | Not_started_state | Before_dispatch_state | Dispatch_started_state -> None
;;

let receipt_plan_fingerprint (receipt : receipt) = receipt.plan_fingerprint
let receipt_request_body_sha256 (receipt : receipt) = receipt.request_body_sha256
let receipt_catalog_generation (receipt : receipt) = receipt.catalog_generation
let receipt_catalog_evidence (receipt : receipt) = receipt.catalog_evidence
let receipt_target_identity (receipt : receipt) = receipt.target_identity

let flow_attempt_receipt (candidate : flow_attempt_candidate) =
  { identity = candidate.evidence.identity; receipt = attempt_receipt candidate.attempt }
;;

let flow_attempt_evidence (flow : flow_attempt) =
  { admissions = flow.admissions
  ; attempts = List.map flow_attempt_receipt flow.candidates
  }
;;

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

let raw_response (evidence : Exec.raw_response_evidence) =
  { body = evidence.raw_body; body_sha256 = evidence.raw_body_sha256 }
;;

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
      Error
        { call_id = receipt.call_id
        ; receipt
        ; cause = execution_error_cause cause
        ; raw_response = Option.map raw_response evidence
        }
    | Ok { outcome; raw_response = evidence } ->
      synchronize_receipt receipt outcome.receipt;
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
  match error.cause with
  | Completion_failed ->
    (match receipt_phase error.receipt with
     | Before_dispatch -> receipt_dispatch_count error.receipt = 0
     | Not_started | Dispatch_started | Response_received | Terminal -> false)
  | Attempt_already_started
  | Clock_required_for_timeout
  | Frozen_request_mismatch
  | Incomplete_output
  | Missing_output
  | Ambiguous_output _
  | Unexpected_output_content
  | Invalid_json_output
  | Internal_non_json_output -> false
;;

let execute_flow_once ~net ?clock ~before_dispatch ~before_advance flow =
  let public_candidate = flow_attempt_receipt in
  let outcome =
    Flow_state.execute_once
      flow.execution
      ~candidates:flow.candidates
      ~before_dispatch:(fun candidate -> before_dispatch (public_candidate candidate))
      ~execute:(fun candidate -> execute_once ~net ?clock candidate.attempt)
      ~can_advance:execution_failure_may_advance
      ~before_advance:(fun ~failed ~failure ~next ->
        before_advance
          ~failed:(public_candidate failed)
          ~failure
          ~next:(public_candidate next))
  in
  let evidence = flow_attempt_evidence flow in
  match outcome with
  | Flow_state.Succeeded { candidate; success } ->
    Ok { candidate = public_candidate candidate; success; evidence }
  | Flow_state.Attempt_already_started -> Error (Flow_attempt_already_started evidence)
  | Flow_state.Before_dispatch_callback_failed { candidate; cause } ->
    Error
      (Flow_before_dispatch_callback_failed
         { candidate = public_candidate candidate; cause; evidence })
  | Flow_state.Before_advance_callback_failed
      { failed_candidate; failure; next_candidate; cause } ->
    Error
      (Flow_before_advance_callback_failed
         { failed = public_candidate failed_candidate
         ; failure
         ; next = public_candidate next_candidate
         ; cause
         ; evidence
         })
  | Flow_state.Execution_failed { candidate; cause } ->
    Error
      (Flow_exact_execution_failed
         { candidate = public_candidate candidate; cause; evidence })
;;
