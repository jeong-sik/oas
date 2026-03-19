(** Capability predicates for provider selection.

    Composable filters for querying {!Provider_registry} by
    required capabilities.

    @since 0.69.0 *)

(** Provider supports tool use. *)
val requires_tools : Capabilities.capabilities -> bool

(** Provider supports native streaming (SSE). *)
val requires_streaming : Capabilities.capabilities -> bool

(** Provider supports extended thinking / chain-of-thought. *)
val requires_reasoning : Capabilities.capabilities -> bool

(** Provider supports multimodal inputs (images, etc). *)
val requires_multimodal : Capabilities.capabilities -> bool

(** Provider supports JSON response format. *)
val requires_json_format : Capabilities.capabilities -> bool

(** All predicates must be satisfied. *)
val requires_all :
  (Capabilities.capabilities -> bool) list -> Capabilities.capabilities -> bool

(** At least one predicate must be satisfied. *)
val requires_any :
  (Capabilities.capabilities -> bool) list -> Capabilities.capabilities -> bool
