(** Provider-neutral correlation from canonical tool-use identities to names.

    The index is built once from immutable message history and exposes only
    read operations.  Provider serializers use it when their tool-result wire
    requires the original function name while {!Types.ToolResult} correctly
    retains only the canonical [tool_use_id]. *)

type t

type error =
  | Conflicting_tool_use_id of
      { tool_use_id : string
      ; first_name : string
      ; conflicting_name : string
      }
  | Missing_tool_use_id of { tool_use_id : string }

(** [of_messages messages] scans [messages] once. Repeating the same
    [(tool_use_id, name)] pair is idempotent; reusing an identity for a
    different name is rejected. The returned index has no mutation API. *)
val of_messages : Types.message list -> (t, error) result

(** Resolve the exact name for [tool_use_id], or report that history cannot
    represent the provider result wire without inventing one. *)
val resolve : t -> tool_use_id:string -> (string, error) result

val error_to_string : error -> string
