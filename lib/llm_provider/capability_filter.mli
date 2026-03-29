(** Capability predicates for provider selection.

    Composable filters for querying {!Provider_registry} by
    required capabilities.

    @since 0.69.0
    @since 0.72.0 — added limit checks, thinking, structured output

    @stability Internal
    @since 0.93.1 *)

(** {2 Feature predicates} *)

val requires_tools : Capabilities.capabilities -> bool
val requires_streaming : Capabilities.capabilities -> bool
val requires_reasoning : Capabilities.capabilities -> bool
val requires_multimodal : Capabilities.capabilities -> bool
val requires_json_format : Capabilities.capabilities -> bool
val requires_parallel_tools : Capabilities.capabilities -> bool
val requires_thinking : Capabilities.capabilities -> bool
val requires_structured_output : Capabilities.capabilities -> bool
val requires_caching : Capabilities.capabilities -> bool
val requires_vision : Capabilities.capabilities -> bool
val requires_computer_use : Capabilities.capabilities -> bool
val requires_system_prompt : Capabilities.capabilities -> bool

(** {2 Limit checks} *)

(** Does the model's context window fit the given token count? *)
val fits_context : tokens:int -> Capabilities.capabilities -> bool

(** Does the model's output limit fit the given token count? *)
val fits_output : tokens:int -> Capabilities.capabilities -> bool

(** {2 Combinators} *)

val requires_all :
  (Capabilities.capabilities -> bool) list -> Capabilities.capabilities -> bool
val requires_any :
  (Capabilities.capabilities -> bool) list -> Capabilities.capabilities -> bool
