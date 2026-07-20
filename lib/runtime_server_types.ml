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
  ; settled : unit Eio.Promise.or_exn
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

let unavailable_switch_error exn =
  Llm_provider.Reserved_exn.reraise_if_reserved exn;
  lane_error ~field:"runtime.lifecycle" "participant switch is no longer accepting fibers"
;;

let register_participant_lane state ~session_id participant =
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
        lane.participants <- participant :: lane.participants;
        Ok ()))
;;

let finish_participant_lane state session_id participant =
  Eio.Cancel.protect (fun () ->
    Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
      match Hashtbl.find_opt state.session_lanes session_id with
      | None -> ()
      | Some lane ->
        lane.participants
        <- List.filter (fun active -> active != participant) lane.participants;
        if lane.phase = Settling && lane.participants = [] then lane.phase <- Settled))
;;

let _log = Log.create ~module_name:"runtime_server_types" ()

let write_protocol_message state message =
  Eio.Mutex.use_rw ~protect:true state.stdout_mu (fun () ->
    output_string stdout (protocol_message_to_string message);
    output_char stdout '\n';
    flush stdout)
;;

let emit_protocol_message state message =
  try write_protocol_message state message with
  | exn ->
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    (* The protocol stream itself is unavailable, so there is no second
       external channel on which this failure can be reported.  Keep the
       durable state authoritative and fall back to the process log. *)
    Log.error
      _log
      "Runtime protocol observation write failed"
      [ Log.S ("error", Printexc.to_string exn) ]
;;

let emit_system_error state message =
  emit_protocol_message state (System_message { level = "error"; message })
;;

let fork_participant_lane ~sw state ~session_id ~participant_name run =
  match Eio.Switch.get_error sw with
  | Some exn -> unavailable_switch_error exn
  | None ->
    (* [Fiber.fork] is a no-op when [sw] turns off.  Registering a participant
       before calling it can therefore leave an unresolved settlement promise.
       [fork_promise] always resolves its result, including when [sw] is already
       cancelling.  The start gate keeps the callback dormant until the lane is
       registered, while the result promise gives settlement an unconditional
       join handle. *)
    let start, start_resolver = Eio.Promise.create () in
    let settled =
      Eio.Fiber.fork_promise ~sw (fun () ->
        (* Registration may suspend on [session_lanes_mu] while [sw] is being
           cancelled.  The gate must still deliver the participant so this
           fiber can remove it from the lane before settling. *)
        match Eio.Cancel.protect (fun () -> Eio.Promise.await start) with
        | None -> ()
        | Some participant ->
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
          (match run_in_lane () with
           | () -> finish_participant_lane state session_id participant
           | exception Eio.Cancel.Cancelled _ ->
             finish_participant_lane state session_id participant
           | exception exn ->
             let bt = Printexc.get_raw_backtrace () in
             finish_participant_lane state session_id participant;
             let observe_ordinary_failure () =
               match Llm_provider.Reserved_exn.reraise_if_reserved exn with
               | () ->
                 Log.error
                   _log
                   "participant lane escaped with an exception"
                   [ Log.S ("session_id", session_id)
                   ; Log.S ("participant", participant_name)
                   ; Log.S ("error", Printexc.to_string exn)
                   ];
                 emit_system_error
                   state
                   (Printf.sprintf
                      "participant lane escaped with an unexpected error: session=%S \
                       participant=%S error=%s"
                      session_id
                      participant_name
                      (Printexc.to_string exn))
               | exception reserved -> Printexc.raise_with_backtrace reserved bt
             in
             (match observe_ordinary_failure () with
              | () -> ()
              | exception reserved ->
                let reserved_bt = Printexc.get_raw_backtrace () in
                Eio.Switch.fail ~bt:reserved_bt sw reserved;
                Printexc.raise_with_backtrace reserved reserved_bt)))
    in
    let participant = { participant_name; settled; cancel_context = None } in
    let registration =
      match register_participant_lane state ~session_id participant with
      | result -> result
      | exception exn ->
        ignore (Eio.Promise.try_resolve start_resolver None);
        raise exn
    in
    (match registration with
     | Error _ as error ->
       ignore (Eio.Promise.try_resolve start_resolver None);
       error
     | Ok () ->
       Eio.Promise.resolve start_resolver (Some participant);
       (match Eio.Switch.get_error sw with
        | None -> Ok ()
        | Some exn ->
          finish_participant_lane state session_id participant;
          unavailable_switch_error exn))
;;

let cancel_participants session_id participants =
  List.iter
    (fun participant ->
       match participant.cancel_context with
       | Some cancel_context ->
         Eio.Cancel.cancel cancel_context (Session_lane_cancelled session_id)
       | None -> ())
    participants
;;

let join_participants initial_failure participants =
  Eio.Cancel.protect (fun () ->
    List.fold_left
      (fun failure participant ->
         match Eio.Promise.await participant.settled with
         | Ok () -> failure
         | Error exn ->
           (* [fork_promise] stores the exception but not its backtrace.  The
              participant path has already attached the original backtrace to
              its owning switch before resolving this promise. *)
           let failure_with_backtrace = exn, Eio.Exn.empty_backtrace in
           Some
             (match failure with
              | None -> failure_with_backtrace
              | Some previous -> Eio.Exn.combine previous failure_with_backtrace))
      initial_failure
      participants)
;;

let settle_participants session_id participants =
  cancel_participants session_id participants;
  join_participants None participants
;;

let reraise_join_failure = function
  | None -> ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
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
  let join_failure = settle_participants session_id participants in
  Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
    match Hashtbl.find_opt state.session_lanes session_id with
    | Some lane ->
      lane.participants <- [];
      lane.phase <- Settled
    | None -> ());
  reraise_join_failure join_failure
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
  (* Cancellation must reach every lane before any join.  Joining each lane in
     the same loop can let one slow participant prevent all later sessions from
     receiving cancellation. *)
  List.iter
    (fun (session_id, participants) -> cancel_participants session_id participants)
    lanes;
  let join_failure =
    List.fold_left
      (fun failure (_session_id, participants) -> join_participants failure participants)
      None
      lanes
  in
  Eio.Mutex.use_rw ~protect:true state.session_lanes_mu (fun () ->
    Hashtbl.iter
      (fun _ lane ->
         lane.participants <- [];
         lane.phase <- Settled)
      state.session_lanes);
  reraise_join_failure join_failure
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
  let participant_run_id (participant : participant_event_common) =
    clean_run_id participant.raw_trace_run_id
  in
  match event.kind with
  | Agent_became_live { participant }
  | Agent_completed { participant; _ }
  | Agent_failed { participant; _ } -> participant_run_id participant
  | Agent_output_delta detail -> clean_run_id detail.raw_trace_run_id
  | Session_started _
  | Session_settings_updated _
  | Turn_recorded _
  | Input_required _
  | Input_provided _
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
  let observer_failure =
    try
      Event_bus.publish state.event_bus event_bus_event;
      None
    with
    | exn ->
      Llm_provider.Reserved_exn.reraise_if_reserved exn;
      Log.warn _log "Event_bus.publish failed" [ Log.S ("error", Printexc.to_string exn) ];
      Some exn
  in
  (* The event is already durable.  Observer failure must neither roll it back
     nor turn the originating command into a retryable failure. *)
  emit_protocol_message state (Event_message { session_id = Some session_id; event });
  match observer_failure with
  | None -> ()
  | Some exn ->
    emit_system_error
      state
      (Printf.sprintf
         "runtime event observer failed after durable commit: session=%S seq=%d \
          observer=event_bus error=%s"
         session_id
         event.seq
         (Printexc.to_string exn))
;;
