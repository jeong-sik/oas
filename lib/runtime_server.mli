(** Runtime protocol server: reads NDJSON from stdin, dispatches requests.

    Entry point for the oas_runtime binary. Integrates with
    {!Runtime_server_types}, {!Runtime_server_resolve}, and
    {!Runtime_server_worker} for request handling.

    @stability Internal
    @since 0.93.1 *)

open Runtime
open Runtime_server_types

(** {1 Server entry point} *)

(** Main server loop: reads protocol messages from stdin and processes
    them until a Shutdown message is received. *)
val serve_stdio : sw:Eio.Switch.t -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t -> unit -> unit

(** {1 Request handling} *)

val handle_request :
  sw:Eio.Switch.t -> state -> request -> (response, Error.sdk_error) result

val start_session :
  state -> start_request -> (response, Error.sdk_error) result

val finalize_session :
  state -> Runtime_store.t -> session -> string option -> (response, Error.sdk_error) result

val apply_command :
  sw:Eio.Switch.t -> state -> Runtime_store.t -> session -> command -> (response, Error.sdk_error) result

(** {1 Control channel} *)

val read_control_response :
  state -> string -> (control_response, Error.sdk_error) result

val ask_permission :
  state ->
  action:string ->
  subject:string ->
  payload:Yojson.Safe.t ->
  (control_response, Error.sdk_error) result

val invoke_hook :
  state ->
  hook_name:string ->
  payload:Yojson.Safe.t ->
  (control_response, Error.sdk_error) result
