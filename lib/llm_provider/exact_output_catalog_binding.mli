type exact_binding_error =
  | Provider_missing
  | Model_missing

val has_control : string -> bool

val target_string_field
  :  target_label:string
  -> field:string
  -> Otoml.t
  -> (string, string) result

val target_float_field
  :  target_label:string
  -> field:string
  -> Otoml.t
  -> (float option, string) result

val target_positive_int_field
  :  target_label:string
  -> field:string
  -> Otoml.t
  -> (int option, string) result

val validate_timeout
  :  target_label:string
  -> field:string
  -> float option
  -> (unit, string) result

val model_identities_unique : Model_catalog.model_entry list -> bool

val validate_overlay_model_identities
  :  base:Model_catalog.model_entry list
  -> overlay:Model_catalog.model_entry list
  -> bool

val resolve_exact
  :  catalog:Model_catalog.t
  -> model_entries:Model_catalog.model_entry list
  -> provider_ref:string
  -> model_id:string
  -> ( Model_catalog.provider_entry * Model_catalog.model_entry
       , exact_binding_error )
       result

val merge_exact_model_entries
  :  base:Model_catalog.model_entry list
  -> overlay:Model_catalog.model_entry list
  -> Model_catalog.model_entry list

val compare_model_entries : Model_catalog.model_entry -> Model_catalog.model_entry -> int
val bool_string : bool -> string
val option_int : int option -> string
val task_string : Capabilities.task option -> string

val anthropic_thinking_control_string
  :  Capabilities.anthropic_thinking_control option
  -> string

val target_model_admitted : Capabilities.capabilities -> model_id:string -> bool

val catalog_anthropic_thinking_control
  :  Capability_vocab.anthropic_thinking_control option
  -> Capabilities.anthropic_thinking_control option

val capabilities_of_catalog_binding
  :  Model_catalog.provider_entry
  -> Model_catalog.model_entry
  -> Capabilities.capabilities

val anthropic_thinking_control_of_model
  :  Model_catalog.model_entry
  -> Capabilities.anthropic_thinking_control option

val functional_capability_projection
  :  Capabilities.capabilities
  -> anthropic_thinking_control:Capabilities.anthropic_thinking_control option
  -> string list
