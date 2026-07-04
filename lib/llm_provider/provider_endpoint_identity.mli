(** Endpoint identity helpers for provider configuration. *)

val capability_provider_label : kind:Provider_kind.t -> base_url:string -> string

val raw_openai_compat_without_builtin_source
  :  kind:Provider_kind.t
  -> base_url:string
  -> provider_label:string
  -> bool

val openai_compat_endpoint_declared_for_output_schema_gate : string -> bool
