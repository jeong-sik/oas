(** IPC transport for oas_runtime using Eio structured concurrency.

    Spawns an oas_runtime subprocess, communicates via NDJSON over
    stdin/stdout pipes, and coordinates responses via [Eio.Promise].

    All mutable state is protected by [Eio.Mutex].

    @stability Evolving
    @since 0.93.1 *)

(** {1 Configuration} *)

type options = {
  runtime_path: string option;
  session_root: string option;
  provider: string option;
  model: string option;
  permission_mode: string option;
  include_partial_messages: bool;
  setting_sources: string list;
  resume_session: string option;
  cwd: string option;
}

val default_options : options

(** {1 Callback types} *)

type control_handler =
  Runtime.control_request -> (Runtime.control_response, Error.sdk_error) result

type event_handler = Runtime.event -> unit

(** {1 Transport handle} *)

(** An opaque transport connection to an oas_runtime subprocess. *)
type t

(** {1 Lifecycle} *)

(** Spawn a runtime subprocess and establish the NDJSON transport.
    Performs the Initialize handshake and verifies protocol version. *)
val connect :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:options ->
  unit ->
  (t, Error.sdk_error) result

(** Send a request and await the response.
    Optionally sets control and event handlers for the duration. *)
val request :
  ?control_handler:control_handler ->
  ?event_handler:event_handler ->
  t ->
  Runtime.request ->
  (Runtime.response, Error.sdk_error) result

(** Query connection status. *)
val status : t -> [ `Connected | `Disconnected | `Error of string ]

(** Gracefully shut down the transport: send Shutdown, then SIGTERM. *)
val close : t -> unit

(** Return the init response received during [connect], if available. *)
val server_info : t -> Runtime.init_response option
