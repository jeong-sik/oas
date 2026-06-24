(** Sessions — public API facade.

    Re-exports types from Sessions_types and store operations from
    Sessions_store. Proof-bundle assembly migrated to

    the downstream coordinator (RFC-OAS-011 OAS-E PR-6). *)

include Sessions_types
include Sessions_store

let participant_by_name (session : Runtime.session) name =
  List.find_opt
    (fun (participant : Runtime.participant) -> String.equal participant.name name)
    session.participants
;;
