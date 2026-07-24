(** Provider-neutral, exact structured-output Single Surface.

    The canonical downstream path is [Agent_sdk.Exact_output]. The
    [Llm_provider.Exact_output] path is the same packaged module, not a second
    contract or entrypoint.

    The caller supplies an immutable OAS resolver snapshot, one exact target
    reference, messages, a raw domain JSON schema, and the minimum guarantee. Provider
    config, wire response formats, schema envelopes, capability overrides,
    tools, reasoning controls, token measurement, and retry/fallback are
    deliberately absent from this interface. *)

type resolver_snapshot
type admitted_target
type catalog_generation
type catalog_evidence
type target_identity
type selected_target
type output_requirement
type ready_plan
type attempt
type receipt
type call_id
type schema_fingerprint
type flow_candidate
type flow_snapshot
type flow_attempt
type resolver_io = { getenv : string -> (string option, unit) result }

type catalog_document =
  { source : string
  ; contents : string
  }

type resolver_catalog_input =
  | Embedded_default
  | Embedded_with_overlay of catalog_document
  | Full_replacement of catalog_document
  | Full_replacement_file of string

type target_ref_error =
  | Empty_target_ref
  | Invalid_target_ref

type resolver_catalog_source =
  | Embedded_catalog
  | Full_replacement_catalog
  | Overlay_catalog

type resolver_collision =
  | Duplicate_provider_identity
  | Duplicate_model_identity
  | Duplicate_target_identity
  | Provider_alias_shadow
  | Target_identity_shadow
  | Model_identity_shadow

type resolver_binding_component =
  | Target_provider
  | Target_model

type resolver_endpoint_error =
  | Malformed_base_url
  | Base_url_userinfo_not_allowed
  | Base_url_query_not_allowed
  | Base_url_fragment_not_allowed
  | Invalid_request_path
  | Unsupported_gemini_request_path
  | Invalid_gemini_model_path

type resolver_snapshot_error =
  | Catalog_read_failed of
      { path : string
      ; detail : string
      }
  | Catalog_parse_failed of
      { source : resolver_catalog_source
      ; detail : string
      }
  | Target_catalog_invalid of
      { source : resolver_catalog_source
      ; detail : string
      }
  | Catalog_collision of resolver_collision
  | Target_binding_missing of
      { target_ref : string
      ; component : resolver_binding_component
      }
  | Target_endpoint_invalid of
      { target_ref : string
      ; cause : resolver_endpoint_error
      }
  | Environment_read_failed of { environment_variable : string }

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type target_selection_error =
  | Missing_target_credential of
      { target_ref : string
      ; environment_variable : string
      }
  | Target_credential_invalid of
      { target_ref : string
      ; environment_variable : string
      }
  | Target_credential_read_failed of
      { target_ref : string
      ; environment_variable : string
      }

type target_catalog_admission_error =
  | Target_ref_rejected of target_ref_error
  | Target_not_in_catalog of string

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

type plan_provenance =
  { source_schema_fingerprint : schema_fingerprint
  ; effective_schema_fingerprint : schema_fingerprint option
  ; actual_assurance : actual_assurance
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type effect_phase =
  | Not_started
  | Before_dispatch
  | Dispatch_started
  | Response_received
  | Terminal

(** Provider-neutral response evidence owned by OAS. Detailed headers and
    provider metadata remain opaque; consumers compare only its fingerprint. *)
type provider_trace

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

(** Provider-neutral identity for one caller-labelled candidate in a frozen
    exact flow. The target fields remain opaque and can only be projected to
    their stable fingerprints. *)
type flow_candidate_identity =
  { candidate_id : string
  ; catalog_generation : catalog_generation
  ; catalog_evidence : catalog_evidence
  ; target_identity : target_identity
  }

type candidate_attempt_count
type admission_rejection_receipt

type admitted_flow_candidate =
  { identity : flow_candidate_identity
  ; plan_fingerprint : string
  ; request_body_sha256 : string
  ; provenance : plan_provenance
  }

type candidate_admission =
  | Candidate_admitted of admitted_flow_candidate
  | Candidate_rejected of admission_rejection_receipt

type flow_candidate_error = Blank_flow_candidate_id

type flow_snapshot_error =
  | Duplicate_flow_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }

(** Construct one provider-neutral candidate. The trimmed caller identity must
    be nonempty; it is evidence only and never drives provider policy. *)
val make_flow_candidate
  :  id:string
  -> target:selected_target
  -> (flow_candidate, flow_candidate_error) result

val flow_candidate_identity : flow_candidate -> flow_candidate_identity

(** Freeze one nonempty ordered candidate snapshot and its immutable domain
    input. This validates only flow topology. Exact target admission is deferred
    to the current candidate during {!execute_flow_once}; later candidates are
    neither prepared nor assigned attempts speculatively. *)
val snapshot_flow
  :  first:flow_candidate
  -> rest:flow_candidate list
  -> messages:Types.message list
  -> output_requirement
  -> (flow_snapshot, flow_snapshot_error) result

(** Parse exactly one typed catalog input and freeze a private immutable target
    map. The default input is the embedded OAS catalog. [Embedded_with_overlay]
    applies the existing sparse exact-output overlay precedence to that
    embedded base. A full replacement, supplied as owned bytes or a file path,
    suppresses every embedded and overlay row; the input type provides no way
    to combine a full replacement with an overlay.
    [io.getenv] is observed exactly once per referenced environment name during
    this call and is never retained. Invalid paths, syntax, bindings,
    collisions, base-URL environment reads, and endpoint declarations fail
    closed; missing, invalid, or read-failed credentials are instead frozen as
    per-target outcomes for [resolve_target]. No source falls back to the
    embedded catalog. *)
val load_resolver_snapshot
  :  io:resolver_io
  -> ?catalog:resolver_catalog_input
  -> unit
  -> (resolver_snapshot, resolver_snapshot_error) result

val resolver_catalog_generation : resolver_snapshot -> catalog_generation
val resolver_catalog_evidence : resolver_snapshot -> catalog_evidence
val catalog_generation_fingerprint : catalog_generation -> string
val catalog_evidence_sha256 : catalog_evidence -> string
val target_identity_fingerprint : target_identity -> string
val selected_target_identity : selected_target -> target_identity
val selected_target_catalog_generation : selected_target -> catalog_generation
val selected_target_catalog_evidence : selected_target -> catalog_evidence

(** Validate [value], prove that its exact identity exists in [resolver_snapshot],
    and capture that frozen target together with its catalog generation and
    evidence. Credential outcomes are intentionally deferred to [resolve_target]. *)
val admit_target_ref
  :  resolver_snapshot
  -> string
  -> (admitted_target, target_catalog_admission_error) result

(** Resolve the binding captured by [admit_target_ref]. This performs no map,
    environment, global, alias, default-model, ranking, probing, or fallback
    lookup. It injects an available frozen secret into a fresh provider config,
    or reports the frozen missing, invalid, or read-failed credential outcome. *)
val resolve_target : admitted_target -> (selected_target, target_selection_error) result

(** Brand an opaque domain JSON schema. OAS never interprets domain keys as a
    provider wire envelope; it always constructs the selected target's wire
    envelope itself. *)
val make_output_requirement
  :  schema:Yojson.Safe.t
  -> minimum_guarantee:minimum_guarantee
  -> output_requirement

(** Pure admission. It performs no token-count request, estimation, provider
    completion, or global admission. The returned immutable plan freezes one
    selected target and one serialized request. *)
val admit
  :  target:selected_target
  -> messages:Types.message list
  -> output_requirement
  -> (ready_plan, admission_error) result

val plan_provenance : ready_plan -> plan_provenance
val plan_fingerprint : ready_plan -> string
val schema_fingerprint_to_string : schema_fingerprint -> string

type start_attempt_error = Call_id_generation_failed of string

(** Allocate a fresh, independent execution attempt for an admitted plan.
    [admit] remains pure and the same immutable plan may start multiple attempts.
    Each attempt owns an opaque call identity and affine execution state. *)
val start_attempt : ready_plan -> (attempt, start_attempt_error) result

(** Create one affine execution handle over the frozen snapshot. This performs
    no target admission, identity allocation, or network effect. *)
val start_flow : flow_snapshot -> flow_attempt

val call_id_to_string : call_id -> string
val attempt_receipt : attempt -> receipt
val receipt_call_id : receipt -> call_id
val receipt_phase : receipt -> effect_phase
val receipt_dispatch_count : receipt -> int
val receipt_http_status : receipt -> int option
val receipt_provider_trace : receipt -> provider_trace option
val provider_trace_fingerprint : provider_trace -> string
val receipt_plan_fingerprint : receipt -> string
val receipt_request_body_sha256 : receipt -> string
val receipt_catalog_generation : receipt -> catalog_generation
val receipt_catalog_evidence : receipt -> catalog_evidence
val receipt_target_identity : receipt -> target_identity

type flow_attempt_receipt =
  { identity : flow_candidate_identity
  ; receipt : receipt
  }

val candidate_attempt_count_to_int : candidate_attempt_count -> int
val admission_rejection_identity : admission_rejection_receipt -> flow_candidate_identity
val admission_rejection_cause : admission_rejection_receipt -> admission_error
val admission_rejection_phase : admission_rejection_receipt -> effect_phase
val admission_rejection_dispatch_count : admission_rejection_receipt -> int

val admission_rejection_candidate_attempt_count
  :  admission_rejection_receipt
  -> candidate_attempt_count

type flow_evidence =
  { candidate_snapshot : flow_candidate_identity list
  ; candidate_attempt_count : candidate_attempt_count
  ; admissions : candidate_admission list
  ; attempts : flow_attempt_receipt list
  }

type flow_success =
  { candidate : flow_attempt_receipt
  ; success : success
  ; evidence : flow_evidence
  }

type flow_candidate_failure =
  | Flow_candidate_admission_rejected of admission_rejection_receipt
  | Flow_candidate_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      ; candidate_attempt_count : candidate_attempt_count
      }

type 'callback_error flow_execution_error =
  | Flow_attempt_already_started of flow_evidence
  | Flow_attempt_start_failed of
      { candidate : flow_candidate_identity
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
      ; next : flow_candidate_identity
      ; cause : 'callback_error
      ; evidence : flow_evidence
      }
  | Flow_admission_failed of
      { rejection : admission_rejection_receipt
      ; evidence : flow_evidence
      }
  | Flow_exact_execution_failed of
      { candidate : flow_attempt_receipt
      ; cause : execution_error
      ; evidence : flow_evidence
      }

(** Point-in-time aggregate evidence. [candidate_snapshot] is the original
    immutable order. [candidate_attempt_count], [admissions], and [attempts]
    contain only candidates reached so far. Attempts are allocated only after
    current-candidate admission succeeds. This remains queryable after
    cancellation escapes. The affine executor is the sole writer. A concurrent
    reader may observe the exact point after an admitted outcome is recorded
    and before its fresh attempt is allocated. *)
val flow_attempt_evidence : flow_attempt -> flow_evidence

(** Execute the frozen request once. The attempt is single-use: duplicate or
    concurrent invocation of the same attempt is rejected before a second dispatch.
    Obtain {!attempt_receipt} before entering a cancellation scope; its phase is
    monotonic and remains queryable if cancellation escapes this function. The
    sole invocation performs at most one outward completion HTTP POST. It calls
    the direct one-dispatch transport rather than a provider SDK or the generic
    retry layer; internal model rotation and requested server-side fallback are
    disabled. This is an outward-dispatch guarantee, not a claim about opaque
    physical execution inside a provider service. *)
val execute_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> attempt
  -> (success, execution_error) result

(** Execute one affine outer flow.

    OAS prepares only the current frozen candidate. A successful preparation
    receives one fresh attempt identity, then [before_dispatch] must durably bind
    that attempt before its sole {!execute_once} call. A typed admission
    rejection receives a real [admission_rejection_receipt] whose phase is
    [Before_dispatch] and dispatch count is zero, but no plan, call ID, or
    execution receipt.

    [before_advance] receives the settled typed failure and the predetermined
    next candidate identity. OAS may call it only for an admission rejection or
    an execution receipt that is exactly [Before_dispatch] with zero dispatch.
    The callback can stop but cannot select, replace, skip, or reorder the
    successor.

    A final admission rejection returns [Flow_admission_failed]. Callback errors,
    replay, identity-allocation failure, missing execution prerequisites,
    frozen-request corruption, cancellation, every post-dispatch outcome, and
    success are terminal. Cancellation is re-raised after the flow is
    terminalized; inspect {!flow_attempt_evidence} for monotonic progress.
    Domain validation occurs after an OAS success and is therefore outside this
    API and never failover eligible. *)
val execute_flow_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> before_dispatch:(flow_attempt_receipt -> (unit, 'callback_error) result)
  -> before_advance:
       (failed:flow_candidate_failure
        -> next:flow_candidate_identity
        -> (unit, 'callback_error) result)
  -> flow_attempt
  -> (flow_success, 'callback_error flow_execution_error) result
