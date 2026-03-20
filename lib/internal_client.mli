(** Internal client helper: connect, query, finalize in one call. *)

val process_query :
  sw:Eio.Switch.t ->
  mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  ?options:Sdk_client_types.options ->
  prompt:string ->
  unit ->
  (Sdk_client_types.message list, Error.sdk_error) result
