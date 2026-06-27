(** Shared JSON helpers for the llm_provider library. *)

(** Return a human-readable JSON type name.

    Uses JSON-schema vocabulary: [object], [array], [string], [number],
    [boolean], [null]. *)
val json_type_name : Yojson.Safe.t -> string
