(** Internal query engine: SDK-side event loop over runtime transport.

    Manages runtime connection, event synchronization, message buffering,
    and permission/hook callbacks. Used by {!Client} and {!Internal_client}.

    @stability Internal
    @since 0.93.0 *)

type t = {
  runtime: Runtime_client.t;
  mutable options: Sdk_client_types.options;
  mutable session_id: string option;
  mutable last_event_seq: int;
  mutable buffered_messages: Sdk_client_types.message list;
  message_mu: Eio.Mutex.t;
  message_cv: Eio.Condition.t;
  mutable can_use_tool: Sdk_client_types.can_use_tool option;
  mutable hook_callback: Sdk_client_types.hook_callback option;
}

(** {1 Lifecycle} *)

val connect :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  ?options:Sdk_client_types.options ->
  unit ->
  (t, Error.sdk_error) result

val close : t -> unit

(** {1 Message operations} *)

val receive_messages : t -> Sdk_client_types.message list
val has_pending_messages : t -> bool
val wait_for_messages : ?timeout:float -> t -> Sdk_client_types.message list

(** {1 Session state} *)

val current_session_id : t -> string option

(** {1 Configuration} *)

val set_permission_mode :
  t -> Sdk_client_types.permission_mode -> (unit, Error.sdk_error) result
val set_model : t -> string option -> (unit, Error.sdk_error) result
val set_can_use_tool : t -> Sdk_client_types.can_use_tool -> unit
val set_hook_callback : t -> Sdk_client_types.hook_callback -> unit

(** {1 Execution} *)

val query_turn : t -> string -> (unit, Error.sdk_error) result
val finalize : t -> ?reason:string -> unit -> (unit, Error.sdk_error) result
val interrupt : t -> (unit, Error.sdk_error) result
