(** Provider-neutral, exact structured-output Single Surface.

    The canonical downstream path is [Agent_sdk.Exact_output]. The
    [Llm_provider.Exact_output] path is the same packaged module, not a second
    contract or entrypoint.

    The caller supplies an immutable OAS resolver snapshot, one exact target
    reference, messages, a raw domain JSON schema, and the minimum guarantee. Provider
    config, wire response formats, schema envelopes, capability overrides,
    tools, reasoning controls, token measurement, and retry/fallback are
    deliberately absent from this interface. *)

type target_ref
type resolver_snapshot
type catalog_generation
type catalog_evidence
type target_identity
type selected_target
type output_requirement
type ready_plan
type receipt
type schema_fingerprint
type resolver_io = { getenv : string -> (string option, unit) result }

type catalog_overlay =
  { source : string
  ; contents : string
  }

type target_ref_error =
  | Empty_target_ref
  | Invalid_target_ref

type resolver_catalog_source =
  | Embedded_catalog
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
  | Invalid_gemini_model_path

type resolver_snapshot_error =
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
      { target_ref : target_ref
      ; component : resolver_binding_component
      }
  | Target_endpoint_invalid of
      { target_ref : target_ref
      ; cause : resolver_endpoint_error
      }
  | Environment_read_failed of { environment_variable : string }
  | Target_credential_invalid of
      { target_ref : target_ref
      ; environment_variable : string
      }

type minimum_guarantee =
  | Json_syntax
  | Provider_schema

type actual_assurance =
  | Json_syntax_only
  | Provider_schema_requested

type target_selection_error =
  | Unknown_target of string
  | Missing_target_credential of
      { target_ref : string
      ; environment_variable : string
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
  | Target_request_rejected
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
  { receipt : receipt
  ; cause : execution_error_cause
  ; raw_response : raw_response option
  }

type success =
  { receipt : receipt
  ; output : Yojson.Safe.t
  ; provenance : plan_provenance
  ; raw_response : raw_response
  }

(** Brand an exact target identifier. Path/query delimiters, whitespace,
    controls, and non-ASCII bytes are rejected before lookup. *)
val target_ref : string -> (target_ref, target_ref_error) result

val target_ref_id : target_ref -> string

(** Parse the embedded catalog plus an optional OAS-owned overlay and freeze a
    private immutable target map. [io.getenv] is consumed during this call and
    is never retained. Overlay replacement is permitted only for the same
    primary id; alias shadowing and ambiguous normalized identities fail
    closed. *)
val load_resolver_snapshot
  :  io:resolver_io
  -> ?overlay:catalog_overlay
  -> unit
  -> (resolver_snapshot, resolver_snapshot_error) result

val resolver_catalog_generation : resolver_snapshot -> catalog_generation
val resolver_catalog_evidence : resolver_snapshot -> catalog_evidence
val catalog_generation_fingerprint : catalog_generation -> string
val catalog_evidence_sha256 : catalog_evidence -> string
val target_identity_ref : target_identity -> target_ref
val target_identity_fingerprint : target_identity -> string
val selected_target_identity : selected_target -> target_identity
val selected_target_catalog_generation : selected_target -> catalog_generation
val selected_target_catalog_evidence : selected_target -> catalog_evidence

(** Resolve exactly one frozen binding. This performs no environment, global,
    alias, default-model, ranking, probing, or fallback lookup. *)
val resolve_target
  :  resolver_snapshot
  -> target_ref
  -> (selected_target, target_selection_error) result

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
val attempt_receipt : ready_plan -> receipt
val receipt_phase : receipt -> effect_phase
val receipt_dispatch_count : receipt -> int
val receipt_http_status : receipt -> int option
val receipt_plan_fingerprint : receipt -> string
val receipt_request_body_sha256 : receipt -> string
val receipt_catalog_generation : receipt -> catalog_generation
val receipt_catalog_evidence : receipt -> catalog_evidence
val receipt_target_identity : receipt -> target_identity

(** Execute the frozen request once. The plan is a single-use attempt:
    duplicate or concurrent invocation is rejected before a second dispatch.
    Obtain {!attempt_receipt} before entering a cancellation scope; its phase is
    monotonic and remains queryable if cancellation escapes this function. The
    sole invocation performs at most one outward completion POST and never
    retries or falls back. *)
val execute_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ready_plan
  -> (success, execution_error) result
