type target_ref
type catalog_generation
type catalog_evidence
type target_identity
type resolver_snapshot
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
  | Unsupported_gemini_request_path
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

type selected_target = private
  { config : Provider_config.t
  ; capabilities : Capabilities.capabilities
  ; anthropic_thinking_control : Capabilities.anthropic_thinking_control option
  ; body_timeout_s : float option
  ; identity : target_identity
  ; generation : catalog_generation
  ; evidence : catalog_evidence
  }

type target_selection_error =
  | Unknown_target of string
  | Missing_target_credential of
      { target_ref : string
      ; environment_variable : string
      }

val target_ref : string -> (target_ref, target_ref_error) result
val target_ref_id : target_ref -> string
val catalog_generation_fingerprint : catalog_generation -> string
val catalog_evidence_sha256 : catalog_evidence -> string
val resolver_catalog_generation : resolver_snapshot -> catalog_generation
val resolver_catalog_evidence : resolver_snapshot -> catalog_evidence
val target_identity_ref : target_identity -> target_ref
val target_identity_fingerprint : target_identity -> string
val selected_target_identity : selected_target -> target_identity
val selected_target_catalog_generation : selected_target -> catalog_generation
val selected_target_catalog_evidence : selected_target -> catalog_evidence
val selected_target_model_admitted : selected_target -> bool
val hash_parts : string list -> string
val option_float : float option -> string

val load_resolver_snapshot
  :  io:resolver_io
  -> ?overlay:catalog_overlay
  -> unit
  -> (resolver_snapshot, resolver_snapshot_error) result

val resolve_target
  :  resolver_snapshot
  -> target_ref
  -> (selected_target, target_selection_error) result
