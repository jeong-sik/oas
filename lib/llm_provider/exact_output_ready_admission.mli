(** Private provider-neutral exact-output requirement and ready-plan admission.

    This module owns the pure requirement projection, frozen generation plan,
    and measured-flow error normalization. The public facade only orchestrates
    outer-flow evidence and generation execution. *)

type schema_fingerprint
type domain_schema

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type output_requirement = private
  { schema : domain_schema
  ; source_schema_fingerprint : schema_fingerprint
  ; minimum_guarantee : minimum_guarantee
  }

type plan_provenance = private
  { source_schema_fingerprint : schema_fingerprint
  ; effective_schema_fingerprint : schema_fingerprint option
  ; actual_assurance : actual_assurance
  ; catalog_generation : Exact_output_resolver.catalog_generation
  ; catalog_evidence : Exact_output_resolver.catalog_evidence
  ; target_identity : Exact_output_resolver.target_identity
  }

type ready_plan = private
  { plan : Exact_output_plan.t
  ; provenance : plan_provenance
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; catalog_generation : Exact_output_resolver.catalog_generation
  ; catalog_evidence : Exact_output_resolver.catalog_evidence
  ; target_identity : Exact_output_resolver.target_identity
  ; measurement : Exact_output_flow_admission.measurement_evidence
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
      admission_error * Exact_output_flow_admission.measurement_evidence
  | Flow_request_measurement_start_failed of string
  | Flow_request_before_measurement_dispatch_failed of
      Exact_output_flow_admission.measurement_receipt * 'callback_error

val schema_fingerprint_to_string : schema_fingerprint -> string

val make_output_requirement
  :  schema:Yojson.Safe.t
  -> minimum_guarantee:minimum_guarantee
  -> output_requirement

val admit
  :  target:Exact_output_resolver.selected_target
  -> messages:Types.message list
  -> output_requirement
  -> (ready_plan, admission_error) result

val admit_flow_request
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> on_measurement_receipt:
       (Exact_output_flow_admission.measurement_receipt -> unit)
  -> before_measurement_dispatch:
       (Exact_output_flow_admission.measurement_receipt
        -> (unit, 'callback_error) result)
  -> target:Exact_output_resolver.selected_target
  -> messages:Types.message list
  -> output_requirement
  -> (ready_plan, 'callback_error flow_request_error) result

val plan_provenance : ready_plan -> plan_provenance
val plan_fingerprint : ready_plan -> string
