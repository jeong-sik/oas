(** A2A Client — client for Agent-to-Agent protocol.

    Discovers remote agents via [/.well-known/agent.json],
    sends tasks via JSON-RPC 2.0, and exposes a thin remote-agent client
    for higher-level orchestration built outside this SDK.

    @since 0.55.0

    @stability Internal
    @since 0.93.1 *)

(** A discovered remote agent endpoint. *)
type remote_agent = {
  endpoint: string;
  agent_card: Agent_card.agent_card;
}

(** [discover ~sw ~net url] fetches [url/.well-known/agent.json]
    and returns a {!remote_agent} on success. *)
val discover :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  string ->
  (remote_agent, Error.sdk_error) result

(** [send_task ~sw ~net remote message] sends a task message to the
    remote agent via JSON-RPC [tasks/send] and returns the created task. *)
val send_task :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  remote_agent ->
  A2a_task.task_message ->
  (A2a_task.task, Error.sdk_error) result

(** [get_task ~sw ~net remote task_id] retrieves a task by ID from
    the remote agent via JSON-RPC [tasks/get]. *)
val get_task :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  remote_agent ->
  A2a_task.task_id ->
  (A2a_task.task, Error.sdk_error) result

(** [cancel_task ~sw ~net remote task_id] cancels a task on the
    remote agent via JSON-RPC [tasks/cancel]. *)
val cancel_task :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  remote_agent ->
  A2a_task.task_id ->
  (A2a_task.task, Error.sdk_error) result

(** [text_of_task task] extracts concatenated text from all Text_part
    messages in a task. *)
val text_of_task : A2a_task.task -> string
