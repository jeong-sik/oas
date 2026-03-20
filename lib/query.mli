(** Convenience wrapper: connect, query, finalize, return messages. *)

val query :
  sw:Eio.Switch.t ->
  mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  ?options:Sdk_client_types.options ->
  prompt:string ->
  unit ->
  (Sdk_client_types.message list, Error.sdk_error) result
