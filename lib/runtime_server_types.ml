open Runtime

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

let runtime_version = Sdk_version.version

let create ~net ~clock () =
  { net
  ; clock
  ; event_bus = Event_bus.create ()
  ; session_root = None
  ; next_control_id = Atomic.make 1
  ; stdout_mu = Eio.Mutex.create ()
  ; store_mu = Eio.Mutex.create ()
  ; control_waiters_mu = Eio.Mutex.create ()
  ; control_waiters = Hashtbl.create 16
  ; paused_inputs_mu = Eio.Mutex.create ()
  ; paused_inputs = Hashtbl.create 16
  }
;;

let store_of_state state = Runtime_store.create ?root:state.session_root ()
let session_root_request_path = Util.trim_non_empty_opt

let write_protocol_message state message =
  Eio.Mutex.use_rw ~protect:true state.stdout_mu (fun () ->
    output_string stdout (protocol_message_to_string message);
    output_char stdout '\n';
    flush stdout)
;;

let next_control_id state =
  let id = Atomic.fetch_and_add state.next_control_id 1 in
  Printf.sprintf "ctrl-%06d" id
;;

(** Map a Runtime.event_kind constructor to its Custom event name.
    Each variant gets a distinct [runtime.<snake_case>] name so
    subscribers can filter by topic without JSON-parsing the payload. *)
let custom_name_of_kind = function
  | Session_started _ -> "runtime.session_started"
  | Session_settings_updated _ -> "runtime.session_settings_updated"
  | Turn_recorded _ -> "runtime.turn_recorded"
  | Input_required _ -> "runtime.input_required"
  | Input_provided _ -> "runtime.input_provided"
  | Pending_input_updated _ -> "runtime.pending_input_updated"
  | Agent_spawn_requested _ -> "runtime.agent_spawn_requested"
  | Agent_became_live _ -> "runtime.agent_became_live"
  | Agent_output_delta _ -> "runtime.agent_output_delta"
  | Agent_completed _ -> "runtime.agent_completed"
  | Agent_failed _ -> "runtime.agent_failed"
  | Artifact_attached _ -> "runtime.artifact_attached"
  | Checkpoint_saved _ -> "runtime.checkpoint_saved"
  | Finalize_requested _ -> "runtime.finalize_requested"
  | Session_completed _ -> "runtime.session_completed"
  | Session_failed _ -> "runtime.session_failed"
;;

let event_bus_run_id_of_event (event : event) =
  let clean_run_id = function
    | Some run_id when String.trim run_id <> "" -> Some run_id
    | _ -> None
  in
  let participant_run_id (participant : participant_event) =
    clean_run_id participant.raw_trace_run_id
  in
  match event.kind with
  | Agent_became_live participant | Agent_completed participant | Agent_failed participant
    -> participant_run_id participant
  | Agent_output_delta detail -> clean_run_id detail.raw_trace_run_id
  | Session_started _
  | Session_settings_updated _
  | Turn_recorded _
  | Input_required _
  | Input_provided _
  | Pending_input_updated _
  | Agent_spawn_requested _
  | Artifact_attached _
  | Checkpoint_saved _
  | Finalize_requested _
  | Session_completed _
  | Session_failed _ -> None
;;

let emit_event state session_id (event : event) =
  let name = custom_name_of_kind event.kind in
  let payload = Event_bus.Custom (name, event |> event_to_yojson) in
  let event_bus_event =
    match event_bus_run_id_of_event event with
    | Some run_id -> Event_bus.mk_event ~correlation_id:session_id ~run_id payload
    | None -> Event_bus.mk_event ~correlation_id:session_id payload
  in
  (try Event_bus.publish state.event_bus event_bus_event with
   | exn ->
     Log.warn
       (Log.create ~module_name:"runtime_server_types" ())
       "Event_bus.publish failed"
       [ Log.S ("error", Printexc.to_string exn) ]);
  write_protocol_message state (Event_message { session_id = Some session_id; event })
;;
