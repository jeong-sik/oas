(** One-shot runtime query.

    Alias for {!Internal_client.process_query}. *)

val query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:Sdk_client_types.options ->
  prompt:string ->
  unit ->
  (Sdk_client_types.message list, Error.sdk_error) result
