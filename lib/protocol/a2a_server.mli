(** A2A Server -- JSON-RPC server for Agent-to-Agent protocol.

    Handles incoming task requests, serves the agent card, and manages
    task lifecycle via callback functions.

    @stability Internal
    @since 0.93.1 *)

type config = {
  port: int;
  agent_card: Agent_card.agent_card;
  on_task_send: A2a_task.task_message -> (A2a_task.task, Error.sdk_error) result;
  on_task_cancel: A2a_task.task_id -> (unit, Error.sdk_error) result;
}

type t

val create :
  ?event_bus:Event_bus.t ->
  ?persistent_store:A2a_task_store.t ->
  config -> t

val start : sw:Eio.Switch.t -> net:_ Eio.Net.t -> t -> unit
val stop : t -> unit
val is_running : t -> bool

val process_request :
  t -> meth:string -> path:string -> body:string ->
  int * string
