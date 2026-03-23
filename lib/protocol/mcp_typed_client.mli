(** Typed MCP client using the SDK Generic_client functor.

    Wraps {!Mcp_protocol_eio.Generic_client} over stdio transport,
    providing the full typed API: tools, resources, prompts, tasks,
    resource subscriptions, and resource template listing.

    This is the recommended client for new code.  The existing {!Mcp}
    module remains for backward compatibility and subprocess management. *)

(** {1 Client handle} *)

type t

(** {1 Connection lifecycle} *)

(** Create a typed client from an already-spawned subprocess's pipes.
    The caller is responsible for process lifecycle (spawn, kill). *)
val create :
  stdin:_ Eio.Flow.source ->
  stdout:_ Eio.Flow.sink ->
  ?clock:_ Eio.Time.clock ->
  unit -> t

(** Perform the MCP initialize handshake.
    Returns the server's initialize result on success. *)
val initialize : t ->
  (Mcp_protocol.Mcp_types.initialize_result, Error.sdk_error) result

(** Close the underlying transport and kill the subprocess (if managed). *)
val close : t -> unit

(** {1 Tool operations} *)

val list_tools : ?cursor:string -> t ->
  (Mcp_protocol.Mcp_types.tool list, Error.sdk_error) result

val call_tool : t -> name:string -> ?arguments:Yojson.Safe.t -> unit ->
  (Mcp_protocol.Mcp_types.tool_result, Error.sdk_error) result

(** {1 Resource operations} *)

val list_resources : ?cursor:string -> t ->
  (Mcp_protocol.Mcp_types.resource list, Error.sdk_error) result

val read_resource : t -> uri:string ->
  (Mcp_protocol.Mcp_types.resource_contents list, Error.sdk_error) result

(** Subscribe to change notifications for a resource URI.
    Requires the server to support resource subscriptions. *)
val subscribe_resource : t -> uri:string -> (unit, Error.sdk_error) result

(** Unsubscribe from change notifications for a resource URI. *)
val unsubscribe_resource : t -> uri:string -> (unit, Error.sdk_error) result

(** List available resource templates from the server. *)
val list_resource_templates : ?cursor:string -> t ->
  (Mcp_protocol.Mcp_types.resource_template list, Error.sdk_error) result

(** {1 Prompt operations} *)

val list_prompts : ?cursor:string -> t ->
  (Mcp_protocol.Mcp_types.prompt list, Error.sdk_error) result

val get_prompt : t -> name:string -> ?arguments:(string * string) list ->
  unit -> (Mcp_protocol.Mcp_types.prompt_result, Error.sdk_error) result

(** {1 Task operations (experimental, MCP 2025-11-25)} *)

val get_task : t -> task_id:string ->
  (Mcp_protocol.Mcp_types.task, Error.sdk_error) result

val list_tasks : ?cursor:string -> t ->
  (Mcp_protocol.Mcp_types.task list, Error.sdk_error) result

val cancel_task : t -> task_id:string ->
  (Mcp_protocol.Mcp_types.task, Error.sdk_error) result

(** {1 Ping} *)

val ping : t -> (unit, Error.sdk_error) result

(** {1 SDK re-exports}

    Convenience modules re-exported for downstream consumers. *)

module Tool_arg = Mcp_protocol.Tool_arg

(** {1 Managed lifecycle}

    Spawn a subprocess, initialize, list tools, and return both a
    {!Mcp.managed} (for compatibility with existing agent wiring)
    and the typed client (for resource/task/subscription operations). *)

val connect_and_load_typed :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  Mcp.server_spec ->
  (Mcp.managed * t, Error.sdk_error) result

(** {1 Server capabilities}

    Parsed from the initialize handshake. Available after {!initialize}. *)

val server_capabilities : t ->
  Mcp_protocol.Mcp_types.server_capabilities option

(** {1 Convert SDK tools to OAS Tool.t} *)

val sdk_tools_to_oas : t ->
  Mcp_protocol.Mcp_types.tool list -> Tool.t list
