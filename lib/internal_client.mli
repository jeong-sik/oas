(** Internal one-shot query client.

    Connects to a runtime, sends a prompt, collects responses,
    and disconnects. Used by {!Query.query}. *)

(** Execute a single query against the runtime.
    Connects, sends the prompt, finalizes, and returns all
    received messages. *)
val process_query :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:Sdk_client_types.options ->
  prompt:string ->
  unit ->
  (Sdk_client_types.message list, Error.sdk_error) result
