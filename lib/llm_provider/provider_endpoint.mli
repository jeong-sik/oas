(** Endpoint-scoped provider capability policy.

    This module keeps URL/endpoint classification separate from
    {!Provider_config.t}, so raw OpenAI-compatible endpoints do not inherit
    model-family capabilities unless a catalog/endpoint declaration proves the
    runtime contract. *)

val base_url_targets_ollama_cloud : string -> bool
val base_url_targets_openai : string -> bool
val base_url_targets_runpod_proxy : string -> bool
val capability_provider_label : kind:Provider_kind.t -> base_url:string -> string

val raw_openai_compat_without_builtin_source
  :  kind:Provider_kind.t
  -> base_url:string
  -> provider_label:string
  -> bool

val capability_requires_endpoint_declaration : Capabilities.capabilities -> bool
val catalog_entry_requires_endpoint_declaration : Model_catalog.model_entry -> bool

val raw_openai_compat_requires_endpoint_declaration
  :  model_id:string
  -> Capabilities.capabilities
  -> bool
