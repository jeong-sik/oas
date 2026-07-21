(** Provider identity rows embedded in the model catalog TOML. *)

type entry =
  { id : string
  ; aliases : string list
  ; kind : Provider_kind.t
  ; identity_kinds : Provider_kind.t list
  ; base_url : string
  ; base_url_env : string option
  ; request_path : string
  ; api_key_env : string
  ; default_model : string option
  ; capabilities_base : string option
  ; identity_hosts : string list
  ; vendor_model_ids : bool
    (** [true] declares that this provider serves its own vendor's models
            under their own ids, so a bare (provider-independent) catalog row
            for a model id is authoritative for it. Defaults to [false], which
            keeps aggregators and generic OpenAI-compatible hosts fail-closed:
            an aggregator's ["gpt-4o"] is not evidence that the row describing
            OpenAI's gpt-4o applies. RFC-OAS-034 §1.3 names canonical vendor
            domains as the one case where host and provider genuinely
            coincide; this field is that case declared as data rather than
            inferred from a URL. *)
  }

val parse_entry : Otoml.t -> (entry, string) result

(** Resolve the exact provider base URL. The optional environment lookup is
    consulted only when the row explicitly declares [base_url_env]; an absent
    or empty override leaves the row's [base_url] unchanged. *)
val resolved_base_url : ?getenv:(string -> string option) -> entry -> string

val provider_label_for_base_url
  :  ?getenv:(string -> string option)
  -> entry list
  -> kind:Provider_kind.t
  -> base_url:string
  -> string option

val provider_label_for_endpoint
  :  ?getenv:(string -> string option)
  -> entry list
  -> kind:Provider_kind.t
  -> base_url:string
  -> request_path:string
  -> string option
