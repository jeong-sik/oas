(** Shared JSON helpers for the llm_provider library. *)

let json_type_name = function
  | `Assoc _ -> "object"
  | `List _ -> "array"
  | `String _ -> "string"
  | `Int _ | `Intlit _ | `Float _ -> "number"
  | `Bool _ -> "boolean"
  | `Null -> "null"
;;
