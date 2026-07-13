open Runtime
open Result_syntax

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

type participant_lane =
  { participant_name : string
  ; settled : unit Eio.Promise.t
  ; settle : unit Eio.Promise.u
  ; mutable cancel_context : Eio.Cancel.t option
  }

type session_lane =
  { mutable phase : session_lane_phase
  ; mutable participants : participant_lane list
  }

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

let runtime_version = Sdk_version.version

let create ~net () =
  { net
  ; event_bus = Event_bus.create ()
  ; initialization_mu = Eio.Mutex.create ()
  ; initialization = Uninitialized
  ; stdout_mu = Eio.Mutex.create ()
  ; store_mu = Eio.Mutex.create ()
  ; paused_inputs_mu = Eio.Mutex.create ()
  ; paused_inputs = Hashtbl.create 16
  ; session_lanes_mu = Eio.Mutex.create ()
  ; session_lanes = Hashtbl.create 16
  ; accepting_lanes = true
  }
;;

let session_root_request_path = Util.trim_non_empty_opt

let canonical_init_request (request : Runtime.init_request) =
  { request with
    session_root = session_root_request_path request.session_root
  ; provider = Util.trim_non_empty_opt request.provider
  ; model = Util.trim_non_empty_opt request.model
  ; resume_session = Util.trim_non_empty_opt request.resume_session
  ; cwd = Util.trim_non_empty_opt request.cwd
  }
;;

let init_request_equal left right =
  Yojson.Safe.equal
    (Runtime.init_request_to_yojson left)
    (Runtime.init_request_to_yojson right)
;;

let initialization_error detail =
  Error (Error.Config (InvalidConfig { field = "runtime.initialize"; detail }))
;;

let initialize state request =
  Eio.Mutex.use_rw ~protect:true state.initialization_mu (fun () ->
    let request = canonical_init_request request in
    match state.initialization with
    | Uninitialized ->
      let* store = Runtime_store.create ?root:request.session_root () in
      let initialized = { store; request } in
      state.initialization <- Initialized initialized;
      Ok initialized
    | Initialized current when init_request_equal current.request request ->
      initialization_error "runtime is already initialized; Initialize is one-shot"
    | Initialized _ ->
      initialization_error
        "runtime is already initialized with different settings; reinitialization is not \
         permitted")
;;

let is_initialized state =
  Eio.Mutex.use_ro state.initialization_mu (fun () ->
    match state.initialization with
    | Initialized _ -> true
    | Uninitialized -> false)
;;

let store_of_state state =
  Eio.Mutex.use_ro state.initialization_mu (fun () ->
    match state.initialization with
    | Initialized initialized -> Ok initialized.store
    | Uninitialized ->
      initialization_error "runtime must be initialized before handling this request")
;;

let initialization_request state =
  Eio.Mutex.use_ro state.initialization_mu (fun () ->
    match state.initialization with
    | Initialized initialized -> Ok initialized.request
    | Uninitialized ->
      initialization_error "runtime must be initialized before handling this request")
;;

let clear_paused_inputs_for_session state session_id =
  Eio.Mutex.use_rw ~protect:true state.paused_inputs_mu (fun () ->
    Hashtbl.filter_map_inplace
      (fun (stored_session_id, _request_id) paused ->
         if String.equal stored_session_id session_id then None else Some paused)
      state.paused_inputs)
;;

let clear_all_paused_inputs state =
  Eio.Mutex.use_rw ~protect:true state.paused_inputs_mu (fun () ->
    Hashtbl.clear state.paused_inputs)
;;

exception Session_lane_cancelled of string

let lane_error ~field detail = Error (Error.Config (InvalidConfig { field; detail }))

let register_participant_lane state ~session_id ~participant_name =
  Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
    if not state.accepting_lanes
    then
      lane_error
        ~field:"runtime.lifecycle"
        "runtime is shutting down; new participant lanes are rejected"
    else (
      let lane =
        match Hashtbl.find_opt state.session_lanes session_id with
        | Some lane -> lane
        | None ->
          let lane = { phase = Open; participants = [] } in
          Hashtbl.add state.session_lanes session_id lane;
          lane
      in
      match lane.phase with
      | Settling | Settled ->
        lane_error
          ~field:"runtime.session_lane"
          (Printf.sprintf
             "session %S is settling; new participant lanes are rejected"
             session_id)
      | Open ->
        let settled, settle = Eio.Promise.create () in
        let participant = { participant_name; settled; settle; cancel_context = None } in
        lane.participants <- participant :: lane.participants;
        Ok participant))
;;

let finish_participant_lane state session_id participant =
  Eio.Cancel.protect (fun () ->
    ignore (Eio.Promise.try_resolve participant.settle ());
    Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
      match Hashtbl.find_opt state.session_lanes session_id with
      | None -> ()
      | Some lane ->
        lane.participants
        <- List.filter (fun active -> active != participant) lane.participants;
        if lane.phase = Settling && lane.participants = [] then lane.phase <- Settled))
;;

let fork_participant_lane ~sw state ~session_id ~participant_name run =
  let* participant = register_participant_lane state ~session_id ~participant_name in
  Eio.Fiber.fork ~sw (fun () ->
    let run_in_lane () =
      Eio.Cancel.sub (fun cancel_context ->
        let should_run =
          Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
            participant.cancel_context <- Some cancel_context;
            match Hashtbl.find_opt state.session_lanes session_id with
            | Some { phase = Open; _ } when state.accepting_lanes -> true
            | Some { phase = Open | Settling | Settled; _ } | None -> false)
        in
        if should_run then run ())
    in
    match run_in_lane () with
    | () -> finish_participant_lane state session_id participant
    | exception Eio.Cancel.Cancelled _ ->
      finish_participant_lane state session_id participant
    | exception exn ->
      finish_participant_lane state session_id participant;
      Log.error
        (Log.create ~module_name:"runtime_server_types" ())
        "participant lane escaped with an exception"
        [ Log.S ("session_id", session_id)
        ; Log.S ("participant", participant_name)
        ; Log.S ("error", Printexc.to_string exn)
        ]);
  Ok ()
;;

let settle_participants session_id participants =
  List.iter
    (fun participant ->
       match participant.cancel_context with
       | Some cancel_context ->
         Eio.Cancel.cancel cancel_context (Session_lane_cancelled session_id)
       | None -> ())
    participants;
  List.iter (fun participant -> Eio.Promise.await participant.settled) participants
;;

let settle_session_lane state session_id =
  let participants =
    Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
      match Hashtbl.find_opt state.session_lanes session_id with
      | None ->
        Hashtbl.add state.session_lanes session_id { phase = Settling; participants = [] };
        []
      | Some lane ->
        lane.phase <- Settling;
        lane.participants)
  in
  settle_participants session_id participants;
  Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
    match Hashtbl.find_opt state.session_lanes session_id with
    | Some lane when lane.participants = [] -> lane.phase <- Settled
    | Some _ | None -> ())
;;

let settle_all_session_lanes state =
  let lanes =
    Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
      state.accepting_lanes <- false;
      Hashtbl.fold
        (fun session_id lane acc ->
           lane.phase <- Settling;
           (session_id, lane.participants) :: acc)
        state.session_lanes
        [])
  in
  List.iter
    (fun (session_id, participants) -> settle_participants session_id participants)
    lanes;
  Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
    Hashtbl.iter (fun _ lane -> lane.phase <- Settled) state.session_lanes)
;;

let write_protocol_message state message =
  Eio.Mutex.use_rw ~protect:true state.stdout_mu (fun () ->
    output_string stdout (protocol_message_to_string message);
    output_char stdout '\n';
    flush stdout)
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
