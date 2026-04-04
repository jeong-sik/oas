(** Server-side state and helpers for the runtime server.

    @stability Internal
    @since 0.93.1 *)

type state = {
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  event_bus: Event_bus.t;
  mutable session_root: string option;
  next_control_id: int Atomic.t;
  stdout_mu: Eio.Mutex.t;
  store_mu: Eio.Mutex.t;
}

val runtime_version : string

val create :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  unit -> state

val store_of_state :
  state -> (Runtime_store.t, Error.sdk_error) result

val session_root_request_path : string option -> string option

val write_protocol_message :
  state -> Runtime.protocol_message -> unit

val next_control_id : state -> string

val emit_event :
  state -> string -> Runtime.event -> unit
