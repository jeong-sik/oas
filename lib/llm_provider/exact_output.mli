type measurement_dispatch_fact =
  | No_measurement_dispatch
  | Measurement_dispatch_unknown
  | Measurement_dispatch_started

type measurement_outcome =
  | Measurement_not_required
  | Measurement_succeeded
  | Measurement_unsupported
  | Measurement_local_invalid
  | Measurement_transport_failed
  | Measurement_invalid_response
  | Measurement_fence_rejected

type measurement_evidence =
  { dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome
  }

type measurement_operation_id

type measurement_receipt_phase =
  | Measurement_fence_committed
  | Measurement_wire_started
  | Measurement_terminal

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
type flow_preference_store
type flow_scope
type flow_success_ordinal
type flow_snapshot
type flow_attempt
type flow_success
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

type target_selection_error

type target_catalog_admission_error =
  | Target_ref_rejected of target_ref_error
  | Target_not_in_catalog of string

type wire_admission_error
type admission_error

type token_capacity_rejection =
  | Capacity_evidence_not_yet_valid of
      { now_unix_s : int
      ; checked_at_unix_s : int
      }
  | Capacity_evidence_expired of
      { now_unix_s : int
      ; expires_at_unix_s : int
      }
  | Capacity_boundary_unknown of
      { input_tokens : int
      ; accepted_through_tokens : int
      ; rejected_from_tokens : int option
      }
  | Capacity_input_rejected of
      { input_tokens : int
      ; accepted_through_tokens : int
      ; rejected_from_tokens : int
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

type flow_id
type flow_visit_ordinal
type candidate_visit_count

type flow_preference_not_applied_reason =
  | Preference_candidate_absent
  | Preference_candidate_binding_changed

type flow_preference_observation =
  | No_preference_recorded
  | Preference_applied of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      }
  | Preference_not_applied of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      ; reason : flow_preference_not_applied_reason
      }

type flow_candidate_visit = private
  { flow_id : flow_id
  ; ordinal : flow_visit_ordinal
  ; identity : flow_candidate_identity
  }

type flow_measurement_receipt

type measurement_receipt_snapshot =
  { operation_id : measurement_operation_id
  ; visit : flow_candidate_visit
  ; request_body_sha256 : string
  ; phase : measurement_receipt_phase
  ; dispatch : measurement_dispatch_fact
  ; outcome : measurement_outcome option
  }

type candidate_rejection_receipt

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

type flow_candidate_error = Blank_flow_candidate_id
type flow_scope_error = Blank_flow_scope_id
type flow_preference_store_error = Invalid_flow_preference_capacity of int

type flow_preference_scope_removal =
  | Flow_preference_scope_removed
  | Flow_preference_scope_not_reserved

type flow_snapshot_error =
  | Duplicate_flow_candidate_id of
      { candidate_id : string
      ; first_position : int
      ; duplicate_position : int
      }
  | Flow_preference_capacity_exhausted of { capacity : int }

(** Construct one provider-neutral candidate from a catalog-admitted target.
    Credential selection remains frozen but unresolved until this candidate is
    reached by {!execute_flow_once}. The trimmed caller identity must be
    nonempty. It is an opaque caller slot identity: a domain-valid preference
    may reorder a future snapshot only when both this exact slot identity and
    the opaque target-identity fingerprint remain unchanged. OAS does not parse
    provider, model, tier, or coordinator meaning from it. *)
val make_flow_candidate
  :  id:string
  -> admitted_target:admitted_target
  -> (flow_candidate, flow_candidate_error) result

val flow_candidate_identity : flow_candidate -> flow_candidate_identity

(** Create one caller-owned, process-local preference store with an explicit
    hard scope capacity. Nonpositive capacities fail closed. The store has no
    global singleton, environment-derived policy, expiry, clock, persistence,
    or background refresh. *)
val create_flow_preference_store
  :  capacity:int
  -> (flow_preference_store, flow_preference_store_error) result

(** Brand one opaque caller scope. OAS compares the trimmed nonempty identity
    exactly; it does not parse coordinator, tenant, provider, or model fields. *)
val make_flow_scope : id:string -> (flow_scope, flow_scope_error) result

val flow_scope_equal : flow_scope -> flow_scope -> bool
val flow_success_ordinal_to_int64 : flow_success_ordinal -> int64

(** Remove one explicitly reserved scope and free its capacity. Existing frozen
    snapshots remain immutable, but a later domain-valid settlement from an
    already-running flow for this reservation fails with
    [Domain_preference_scope_released], even if the same textual scope has since
    been reserved again, and is still consumed exactly once. *)
val remove_flow_preference_scope
  :  flow_preference_store
  -> flow_scope
  -> flow_preference_scope_removal

(** Freeze one nonempty ordered candidate snapshot and its immutable domain
    input. This validates only flow topology. Credential selection and exact
    request admission are deferred to the current candidate during
    {!execute_flow_once}; later candidates are neither selected, prepared, nor
    assigned attempts speculatively.

    The snapshot atomically reserves [scope] in [preferences]. A new scope fails
    with [Flow_preference_capacity_exhausted] when the store is full. A
    domain-valid last-good candidate recorded for the scope is moved to the
    front only when the same caller slot and opaque target binding are both
    present. An absent slot or changed target binding is retained as a typed
    non-applied observation. The remaining candidates retain their declared
    relative order. This lookup happens exactly once: existing snapshots never
    change, and preferences from another scope cannot affect this snapshot. *)
val snapshot_flow
  :  preferences:flow_preference_store
  -> scope:flow_scope
  -> first:flow_candidate
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
type measurement_start_error =
  | Measurement_operation_id_generation_failed of string
  | Measurement_clock_required_for_timeout
type flow_start_error = Flow_id_generation_failed of string

(** Allocate a fresh, independent execution attempt for an admitted plan.
    [admit] remains pure and the same immutable plan may start multiple attempts.
    Each attempt owns an opaque call identity and affine execution state. *)
val start_attempt : ready_plan -> (attempt, start_attempt_error) result

(** Allocate one fresh outer-flow identity and precompute one immutable visit
    for each frozen candidate. This performs no credential selection, request
    admission, call identity allocation, callback, or network effect. A new
    invocation always creates a new flow; restart resume belongs to the
    caller's authenticated durable journal. *)
val start_flow : flow_snapshot -> (flow_attempt, flow_start_error) result

val call_id_to_string : call_id -> string
val flow_id_to_string : flow_id -> string
val flow_visit_ordinal_to_int : flow_visit_ordinal -> int
val flow_attempt_id : flow_attempt -> flow_id
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

(** One immutable outer-flow binding. [scope], opaque candidate identity, and
    the one-shot execution receipt travel together; consumers do not rebuild
    that join from coordinator or target strings. *)
type flow_attempt_receipt = private
  { scope : flow_scope
  ; visit : flow_candidate_visit
  ; receipt : receipt
  }

val candidate_visit_count_to_int : candidate_visit_count -> int
val measurement_operation_id_to_string : measurement_operation_id -> string

val flow_measurement_receipt_snapshot
  :  flow_measurement_receipt
  -> measurement_receipt_snapshot

val target_selection_error_disposition
  :  target_selection_error
  -> candidate_rejection_disposition

val admission_error_disposition : admission_error -> candidate_rejection_disposition
val candidate_rejection_identity : candidate_rejection_receipt -> flow_candidate_identity
val candidate_rejection_scope : candidate_rejection_receipt -> flow_scope
val candidate_rejection_visit : candidate_rejection_receipt -> flow_candidate_visit
val candidate_rejection_measurement_dispatch_fact
  :  candidate_rejection_receipt
  -> measurement_dispatch_fact

val candidate_rejection_measurement_outcome
  :  candidate_rejection_receipt
  -> measurement_outcome

val candidate_rejection_disposition
  :  candidate_rejection_receipt
  -> candidate_rejection_disposition

type flow_evidence = private
  { flow_id : flow_id
  ; scope : flow_scope
  ; declared_candidate_snapshot : flow_candidate_identity list
  ; candidate_snapshot : flow_candidate_identity list
  ; preference_observation : flow_preference_observation
  ; candidate_visit_count : candidate_visit_count
  ; measurements : flow_measurement_receipt list
  ; admissions : candidate_admission list
  ; attempts : flow_attempt_receipt list
  }

val flow_success_candidate : flow_success -> flow_attempt_receipt
val flow_success_output : flow_success -> success
val flow_success_evidence : flow_success -> flow_evidence
val flow_success_ordinal : flow_success -> flow_success_ordinal

type domain_disposition =
  | Domain_valid
  | Domain_rejected

type domain_settlement_receipt = private
  | Domain_rejected_recorded
  | Domain_valid_preference_installed of
      { candidate : flow_candidate_identity
      ; success_ordinal : flow_success_ordinal
      }
  | Domain_valid_preference_superseded of
      { current_candidate : flow_candidate_identity
      ; current_success_ordinal : flow_success_ordinal
      }

type domain_settlement_error =
  | Domain_already_settled
  | Domain_preference_scope_released

(** Settle caller-owned domain validation exactly once after structural
    success. OAS freezes an immutable monotonic ordinal when structural success
    is created; the caller cannot supply or alter freshness. [Domain_valid]
    records the successful opaque candidate and frozen ordinal as last-good for
    this flow's scope. [Domain_rejected] records no preference and never resumes
    or advances the terminal flow. Concurrent or duplicate settlement returns
    [Domain_already_settled]. A success whose ordinal is not newer cannot
    overwrite the retained preference and returns a typed
    [Domain_valid_preference_superseded] receipt. If the caller explicitly
    removed the scope reservation after snapshot, domain-valid settlement
    returns [Domain_preference_scope_released]; recreating the same textual
    scope does not revive the old reservation. Every disposition is consumed
    exactly once. *)
val settle_flow_domain
  :  flow_success
  -> domain_disposition
  -> (domain_settlement_receipt, domain_settlement_error) result

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

(** Closed fact for the invocation returning the error: whether its one outward
    completion dispatch began. This does not claim provider acceptance, response
    receipt, billing, retryability, failover eligibility, or any Pricing
    decision. *)
val flow_execution_error_generation_dispatch
  :  'callback_error flow_execution_error
  -> generation_dispatch_fact

(** Point-in-time aggregate evidence. [scope] is the exact opaque flow scope,
    [declared_candidate_snapshot] is the caller-declared order, and
    [candidate_snapshot] is the frozen effective order.
    [preference_observation] is the single scope lookup frozen when the snapshot
    was created; it distinguishes no record, an applied binding, an absent
    recorded slot, and a changed target binding.
    [candidate_visit_count], [admissions], and [attempts] contain only
    candidates reached so far. Attempts are allocated only after
    current-candidate target selection and request admission succeed. This
    remains queryable after cancellation escapes. The affine executor is the
    sole writer. A concurrent reader may observe the exact point after an
    admitted outcome is recorded and before its fresh attempt is allocated. *)
val flow_attempt_evidence : flow_attempt -> flow_evidence

(** Execute the frozen request once. The attempt is single-use: duplicate or
    concurrent invocation of the same attempt is rejected before a second dispatch.
    Obtain {!attempt_receipt} before entering a cancellation scope; its phase is
    monotonic and remains queryable if cancellation escapes this function. The
    sole invocation performs at most one outward completion HTTP POST. It calls
    the direct one-dispatch transport rather than a provider SDK or the generic
    retry layer; internal model rotation and requested server-side fallback are
    disabled. This is a generation-dispatch guarantee, not a claim about opaque
    physical execution inside a provider service. *)
val execute_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> attempt
  -> (success, execution_error) result

(** Execute one affine outer flow.

    OAS resolves credentials and prepares the request only for the current
    frozen candidate. A successful preparation receives one fresh attempt
    identity, then [before_dispatch] must durably bind that attempt before its
    sole {!execute_once} call. When provider-native measurement is required,
    [before_measurement_dispatch] first receives an opaque operation receipt
    and must durably journal it as a may-dispatch intent. Only after the callback
    returns [Ok] can the count-token POST start. The receipt is already visible
    in [flow_attempt_evidence.measurements] while the callback runs.

    The callback receives a committed receipt whose dispatch fact is already
    [Measurement_dispatch_unknown], never [No_measurement_dispatch]. A committed
    receipt without a terminal outcome is dispatch-ambiguous and reports
    [Measurement_dispatch_unknown]; a wire observer advances it to
    [Measurement_dispatch_started]. Token measurement is a read-only operation
    and may be replayed only after the caller reconciles and records the prior
    unclosed intent. OAS neither retries it automatically nor claims that an
    unclosed committed receipt did not dispatch.

    [on_measurement_terminal] receives the same opaque receipt after its terminal
    measurement outcome is installed. OAS allocates no generation attempt,
    dispatches no generation request, and advances to no successor until this
    terminal record is durably accepted. Callback rejection returns a typed
    terminal flow error.

    A typed target-selection or request-admission rejection receives a real
    [candidate_rejection_receipt] with explicit measurement evidence, but no
    generation plan, generation call ID, or generation execution receipt.

    [before_advance] receives the settled typed failure and the predetermined
    next candidate visit. OAS may call it only for a candidate rejection or an
    execution receipt that is exactly [Before_dispatch] with zero dispatch. The
    callback can stop but cannot select, replace, skip, or reorder the successor.

    A final candidate rejection returns [Flow_candidates_exhausted]. Callback
    errors, replay, identity-allocation failure, missing execution prerequisites,
    frozen-request corruption, cancellation, every post-dispatch outcome, and
    success are terminal. Cancellation is re-raised after the flow is
    terminalized; inspect {!flow_attempt_evidence} for monotonic progress.
    Domain validation occurs after an OAS success through
    {!settle_flow_domain}; either disposition remains terminal and is never
    failover eligible. Only a domain-valid settlement can affect a future
    snapshot in the same explicit scope. *)
val execute_flow_once
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> before_measurement_dispatch:
       (flow_measurement_receipt -> (unit, 'callback_error) result)
  -> on_measurement_terminal:
       (flow_measurement_receipt -> (unit, 'callback_error) result)
  -> before_dispatch:(flow_attempt_receipt -> (unit, 'callback_error) result)
  -> before_advance:
       (failed:flow_candidate_failure
        -> next:flow_candidate_visit
        -> (unit, 'callback_error) result)
  -> flow_attempt
  -> (flow_success, 'callback_error flow_execution_error) result
