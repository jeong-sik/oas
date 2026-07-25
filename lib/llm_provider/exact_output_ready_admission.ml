module Plan = Exact_output_plan
module Flow_admission = Exact_output_flow_admission
module Resolver = Exact_output_resolver
module Gemini_schema = Exact_output_gemini_schema
module Caps = Capabilities
module PC = Provider_config

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
  ; catalog_generation : Resolver.catalog_generation
  ; catalog_evidence : Resolver.catalog_evidence
  ; target_identity : Resolver.target_identity
  }

type ready_plan =
  { plan : Plan.t
  ; provenance : plan_provenance
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : Resolver.catalog_generation
  ; catalog_evidence : Resolver.catalog_evidence
  ; target_identity : Resolver.target_identity
  ; measurement : Flow_admission.measurement_evidence
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
  | Context_limit_unavailable
  | Invalid_context_limit
  | Output_reservation_unavailable
  | Measured_context_window_exceeded of Complete.context_fit
  | Measured_serving_constraint_rejected of Serving_constraint.admission_error
  | Token_measurement_failed
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

type 'callback_error flow_request_error =
  | Flow_request_admission_failed of
      admission_error * Flow_admission.measurement_evidence
  | Flow_request_measurement_start_failed of string
  | Flow_request_before_measurement_dispatch_failed of
      Flow_admission.measurement_receipt * 'callback_error

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

let schema_for_wire (target : Resolver.selected_target) (Domain_schema domain_schema) =
  match Provider_http_codec.(json_schema_wire (of_config target.config)) with
  | Raw_schema -> domain_schema
  | Openai_named_schema ->
    `Assoc
      [ "name", `String (PC.structured_output_name_of_schema domain_schema)
      ; "schema", domain_schema
      ; "strict", `Bool true
      ]
;;

let response_format (target : Resolver.selected_target) requirement =
  match Caps.structured_output_support target.capabilities, requirement.minimum_guarantee with
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

let exact_config (target : Resolver.selected_target) response_format =
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
  | Plan.Request_body_too_large { actual_bytes; limit_bytes } ->
    Request_body_too_large { actual_bytes; limit_bytes }
  | Plan.Request_serialization_rejected _ -> Request_serialization_rejected
;;

let admission_contract ~(target : Resolver.selected_target) requirement =
  let* () =
    if Resolver.selected_target_model_admitted target
    then Ok ()
    else
      Error
        (Wire_admission_rejected
           (Unsupported_target_model { model_id = target.config.model_id }))
  in
  let* response_format, actual_assurance, effective_schema_fingerprint =
    response_format target requirement
  in
  Ok (response_format, actual_assurance, effective_schema_fingerprint)
;;

let ready_plan
      ~(target : Resolver.selected_target)
      ~(requirement : output_requirement)
      ~actual_assurance
      ~effective_schema_fingerprint
      ~measurement
      plan
  =
  let request_body_sha256 = Plan.request_body_sha256 plan in
  let plan_fingerprint =
    Resolver.hash_parts
      [ "oas-exact-output-ready-plan-v2"
      ; request_body_sha256
      ; Resolver.catalog_generation_fingerprint target.generation
      ; Resolver.target_identity_fingerprint target.identity
      ; Provider_http_codec.fingerprint_tag (Plan.response_codec plan)
      ; Resolver.option_float (Plan.connect_timeout_s plan)
      ; Resolver.option_float (Plan.body_timeout_s plan)
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
  ; measurement
  }
;;

let admit ~target ~messages requirement =
  let* response_format, actual_assurance, effective_schema_fingerprint =
    admission_contract ~target requirement
  in
  Plan.preflight
    ~config:(exact_config target response_format)
    ~messages
    ~body_timeout_s:target.body_timeout_s
    ~anthropic_thinking_control:target.anthropic_thinking_control
  |> Result.map_error (fun error -> Wire_admission_rejected (wire_admission_error error))
  |> Result.bind (fun preflight ->
    Plan.finalize_unmeasured preflight
    |> Result.map_error (function
      | Plan.Token_measurement_required constraint_ ->
        Wire_admission_rejected (Token_measurement_required constraint_)
      | Plan.Measured_request_mismatch ->
        Wire_admission_rejected Request_serialization_rejected))
  |> Result.map (fun plan ->
    ready_plan
      ~target
      ~requirement
      ~actual_assurance
      ~effective_schema_fingerprint
      ~measurement:
        { dispatch = Flow_admission.No_measurement_dispatch
        ; outcome = Flow_admission.Measurement_not_required
        }
      plan)
;;

let admit_flow_request
      ~net
      ?clock
      ~on_measurement_receipt
      ~before_measurement_dispatch
      ~target
      ~messages
      requirement
  =
  let* response_format, actual_assurance, effective_schema_fingerprint =
    admission_contract ~target requirement
    |> Result.map_error (fun error ->
      Flow_request_admission_failed
        ( error
        , { dispatch = Flow_admission.No_measurement_dispatch
          ; outcome = Flow_admission.Measurement_local_invalid
          }
        ))
  in
  let* preflight =
    Plan.preflight
      ~config:(exact_config target response_format)
      ~messages
      ~body_timeout_s:target.body_timeout_s
      ~anthropic_thinking_control:target.anthropic_thinking_control
    |> Result.map_error (fun error ->
      Flow_request_admission_failed
        ( Wire_admission_rejected (wire_admission_error error)
        , { dispatch = Flow_admission.No_measurement_dispatch
          ; outcome = Flow_admission.Measurement_local_invalid
          }
        ))
  in
  match
    Flow_admission.admit
      ~net
      ?clock
      ~now_unix_s:(fun () -> int_of_float (Unix.gettimeofday ()))
      ~on_measurement_receipt
      ~before_measurement_dispatch
      preflight
  with
  | Flow_admission.Admitted { plan; measurement } ->
    Ok
      (ready_plan
         ~target
         ~requirement
         ~actual_assurance
         ~effective_schema_fingerprint
         ~measurement
         plan)
  | Flow_admission.Rejected { cause; measurement } ->
    let constraint_ = Plan.serving_constraint preflight in
    let error =
      match cause with
      | Flow_admission.Serving_evidence_rejected reason ->
        Measured_serving_constraint_rejected reason
      | Flow_admission.Context_admission_rejected error ->
        (match error with
         | Complete.Context_limit_unknown _ -> Context_limit_unavailable
         | Complete.Invalid_context_limit _ -> Invalid_context_limit
         | Complete.Output_reservation_unknown _ -> Output_reservation_unavailable
         | Complete.Context_window_exceeded fit -> Measured_context_window_exceeded fit
         | Complete.Serving_constraint_rejected { reason; _ } ->
           Measured_serving_constraint_rejected reason)
      | Flow_admission.Measurement_rejected (Flow_admission.Unsupported_failure _) ->
        (match constraint_ with
         | Some constraint_ -> Token_measurement_required constraint_
         | None -> Token_measurement_failed)
      | Flow_admission.Measurement_rejected
          ( Flow_admission.Transport_failure _
          | Flow_admission.Invalid_response_failure _
          | Flow_admission.Output_token_resolution_failure _
          | Flow_admission.Invalid_request_failure _ ) -> Token_measurement_failed
      | Flow_admission.Plan_finalization_rejected error ->
        (match error with
         | Plan.Token_measurement_required constraint_ ->
           Token_measurement_required constraint_
         | Plan.Measured_request_mismatch -> Request_serialization_rejected)
    in
    Error
      (Flow_request_admission_failed
         (Wire_admission_rejected error, measurement))
  | Flow_admission.Measurement_operation_start_failed detail ->
    Error (Flow_request_measurement_start_failed detail)
  | Flow_admission.Before_measurement_dispatch_failed { receipt; cause } ->
    Error (Flow_request_before_measurement_dispatch_failed (receipt, cause))
;;

let plan_provenance (ready : ready_plan) = ready.provenance
let plan_fingerprint (ready : ready_plan) = ready.plan_fingerprint
