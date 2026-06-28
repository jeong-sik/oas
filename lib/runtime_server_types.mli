(** Server-side state and helpers for the runtime server.

    @stability Internal
    @since 0.93.1 *)

type paused_participant =
  { detail : Runtime.spawn_agent_request
  ; resolution : Runtime_server_resolve.execution_resolution
  ; agent : Agent.t
  ; input_required : Error.input_required
  ; trace_sink : Raw_trace.t option
  ; delta_warn_logged : bool ref
  ; delta_error_count : int ref
  }

type state =
  { net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t
  ; event_bus : Event_bus.t
  ; mutable session_root : string option
  ; next_control_id : int Atomic.t
  ; stdout_mu : Eio.Mutex.t
  ; store_mu : Eio.Mutex.t
  ; control_waiters_mu : Eio.Mutex.t
  ; control_waiters : (string, Runtime.control_response Eio.Promise.u) Hashtbl.t
  ; paused_inputs_mu : Eio.Mutex.t
  ; paused_inputs : (string * string, paused_participant) Hashtbl.t
  }

val runtime_version : string

val create
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> clock:float Eio.Time.clock_ty Eio.Resource.t
  -> unit
  -> state

val store_of_state : state -> (Runtime_store.t, Error.sdk_error) result
val session_root_request_path : string option -> string option
val write_protocol_message : state -> Runtime.protocol_message -> unit
val next_control_id : state -> string
val custom_name_of_kind : Runtime.event_kind -> string
val event_bus_run_id_of_event : Runtime.event -> string option
val emit_event : state -> string -> Runtime.event -> unit
