(** Client for the runtime protocol over stdio transport.

    @stability Internal
    @since 0.93.1 *)

(** {1 Options} *)

type options = Transport.options =
  { runtime_path : string option
  ; session_root : string option
  ; provider : string option
  ; model : string option
  ; permission_mode : string option
  ; include_partial_messages : bool
  ; setting_sources : string list
  ; resume_session : string option
  ; cwd : string option
  }

val default_options : options

(** {1 Client} *)

type t

val connect
  :  sw:Eio.Switch.t
  -> mgr:_ Eio.Process.mgr
  -> ?options:options
  -> unit
  -> (t, Error.sdk_error) result

val request
  :  ?control_handler:Transport.control_handler
  -> ?event_handler:Transport.event_handler
  -> t
  -> Runtime.request
  -> (Runtime.response, Error.sdk_error) result

val close : t -> unit

(** {1 Convenience helpers} *)

val init_info : t -> (Runtime.response, Error.sdk_error) result
val get_server_info : t -> Runtime.init_response option

val start_session
  :  ?control_handler:Transport.control_handler
  -> ?event_handler:Transport.event_handler
  -> t
  -> Runtime.start_request
  -> (Runtime.session, Error.sdk_error) result

val apply_command
  :  ?control_handler:Transport.control_handler
  -> ?event_handler:Transport.event_handler
  -> t
  -> session_id:string
  -> Runtime.command
  -> (Runtime.session, Error.sdk_error) result

val status : t -> session_id:string -> (Runtime.session, Error.sdk_error) result

val events
  :  t
  -> session_id:string
  -> ?after_seq:int
  -> unit
  -> (Runtime.event list, Error.sdk_error) result

val finalize
  :  ?control_handler:Transport.control_handler
  -> ?event_handler:Transport.event_handler
  -> t
  -> session_id:string
  -> ?reason:string
  -> unit
  -> (Runtime.session, Error.sdk_error) result

val report : t -> session_id:string -> (Runtime.report, Error.sdk_error) result
val prove : t -> session_id:string -> (Runtime.proof, Error.sdk_error) result
