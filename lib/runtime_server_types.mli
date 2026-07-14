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

type initialized_runtime =
  { store : Runtime_store.t
  ; request : Runtime.init_request
  }

type initialization_state =
  | Uninitialized
  | Initialized of initialized_runtime

type session_lane_phase =
  | Open
  | Settling
  | Settled

type participant_lane
type session_lane

type state =
  { net : [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  ; event_bus : Event_bus.t
  ; initialization_mu : Eio.Mutex.t
  ; mutable initialization : initialization_state
  ; stdout_mu : Eio.Mutex.t
  ; store_mu : Eio.Mutex.t
  ; paused_inputs_mu : Eio.Mutex.t
  ; paused_inputs : (string * string, paused_participant) Hashtbl.t
  ; session_lanes_mu : Eio.Mutex.t
  ; session_lanes : (string, session_lane) Hashtbl.t
  ; mutable accepting_lanes : bool
  }

val runtime_version : string
val create : net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t -> unit -> state

val initialize
  :  state
  -> Runtime.init_request
  -> (initialized_runtime, Error.sdk_error) result

val is_initialized : state -> bool
val store_of_state : state -> (Runtime_store.t, Error.sdk_error) result
val initialization_request : state -> (Runtime.init_request, Error.sdk_error) result
val session_root_request_path : string option -> string option
val clear_paused_inputs_for_session : state -> string -> unit
val clear_all_paused_inputs : state -> unit

(** [fork_participant_lane] registers one participant and starts its callback
    only after registration succeeds. Its settlement handle resolves even if
    [sw] starts cancelling during registration. Reserved callback exceptions
    fail [sw] after the participant has been removed from the lane. *)
val fork_participant_lane
  :  sw:Eio.Switch.t
  -> state
  -> session_id:string
  -> participant_name:string
  -> (unit -> unit)
  -> (unit, Error.sdk_error) result

val settle_session_lane : state -> string -> unit

(** [settle_all_session_lanes] rejects new lanes, snapshots every active lane,
    sends cancellation to every snapshot, and only then joins participants. *)
val settle_all_session_lanes : state -> unit

val write_protocol_message : state -> Runtime.protocol_message -> unit

(** Emit an explicit runtime-protocol error observation without converting an
    ordinary stdout failure into a command or participant failure. Reserved
    exceptions still propagate. If stdout itself is unavailable, the process
    log is the only remaining observation channel. *)
val emit_system_error : state -> string -> unit

val custom_name_of_kind : Runtime.event_kind -> string
val event_bus_run_id_of_event : Runtime.event -> string option
val emit_event : state -> string -> Runtime.event -> unit
