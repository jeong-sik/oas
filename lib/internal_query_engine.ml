type t =
  { runtime : Runtime_client.t
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
  ; mutable options : Sdk_client_types.options
  ; mutable session_id : string option
  ; mutable last_event_seq : int
  ; mutable buffered_messages : Sdk_client_types.message list
  ; message_mu : Eio.Mutex.t
  ; message_cv : Eio.Condition.t
  ; mutable can_use_tool : Sdk_client_types.can_use_tool option
  ; mutable hook_callback : Sdk_client_types.hook_callback option
  }

open Result_syntax

let append_message state message =
  Eio.Mutex.use_rw ~protect:true state.message_mu (fun () ->
    state.buffered_messages <- Util.snoc state.buffered_messages message);
  Eio.Condition.broadcast state.message_cv
;;

let runtime_options options =
  { Runtime_client.runtime_path = options.Sdk_client_types.runtime_path
  ; session_root = options.session_root
  ; provider = options.provider
  ; model = options.model
  ; permission_mode =
      Some (Sdk_client_types.string_of_permission_mode options.permission_mode)
  ; include_partial_messages = options.include_partial_messages
  ; setting_sources =
      List.map
        (function
          | Sdk_client_types.User -> "user"
          | Sdk_client_types.Project -> "project"
          | Sdk_client_types.Local -> "local")
        options.setting_sources
  ; resume_session = options.resume_session
  ; cwd = options.cwd
  }
;;

let connect ~sw ?clock ~mgr ?(options = Sdk_client_types.default_options) () =
  let* runtime = Runtime_client.connect ~sw ~mgr ~options:(runtime_options options) () in
  let state =
    { runtime
    ; clock
    ; options
    ; session_id = None
    ; last_event_seq = 0
    ; buffered_messages = []
    ; message_mu = Eio.Mutex.create ()
    ; message_cv = Eio.Condition.create ()
    ; can_use_tool = None
    ; hook_callback = None
    }
  in
  match options.resume_session with
  | Some session_id when String.trim session_id <> "" ->
    let* session = Runtime_client.status runtime ~session_id in
    state.options
    <- { state.options with
         model = session.model
       ; permission_mode =
           (match session.permission_mode with
            | Some raw ->
              (match Sdk_client_types.permission_mode_of_string raw with
               | Some mode -> mode
               | None -> state.options.permission_mode)
            | None -> state.options.permission_mode)
       };
    state.session_id <- Some session.session_id;
    state.last_event_seq <- session.last_seq;
    append_message state (Internal_message_parser.status session);
    Ok state
  | _ -> Ok state
;;

let receive_messages state =
  Eio.Mutex.use_rw ~protect:true state.message_mu (fun () ->
    let messages = state.buffered_messages in
    state.buffered_messages <- [];
    messages)
;;

let has_pending_messages state =
  Eio.Mutex.use_ro state.message_mu (fun () -> state.buffered_messages <> [])
;;

let wait_blocking state =
  Eio.Mutex.use_rw ~protect:true state.message_mu (fun () ->
    while state.buffered_messages = [] do
      Eio.Condition.await state.message_cv state.message_mu
    done;
    let messages = state.buffered_messages in
    state.buffered_messages <- [];
    messages)
;;

let wait_for_messages ?timeout state =
  let drain_if_any () =
    Eio.Mutex.use_rw ~protect:true state.message_mu (fun () ->
      let messages = state.buffered_messages in
      let had_messages = messages <> [] in
      if had_messages then state.buffered_messages <- [];
      if had_messages then Some messages else None)
  in
  match drain_if_any () with
  | Some messages -> messages
  | None ->
    (match timeout with
     | None -> wait_blocking state
     | Some timeout_s when timeout_s <= 0.0 -> []
     | Some timeout_s ->
       (match state.clock with
        | Some clock ->
          (try
             Eio.Time.with_timeout_exn clock timeout_s (fun () -> wait_blocking state)
           with
           | Eio.Time.Timeout -> [])
        | None ->
          let deadline = Unix.gettimeofday () +. timeout_s in
          let rec await_without_clock () =
            match drain_if_any () with
            | Some messages -> messages
            | None ->
              if Unix.gettimeofday () >= deadline
              then []
              else (
                (* No Eio clock was supplied, so fall back to a
                          cooperative poll instead of blocking the runtime
                          thread with Unix.sleepf. *)
                Eio.Fiber.yield ();
                await_without_clock ())
          in
          await_without_clock ()))
;;

let current_session_id state = state.session_id

let set_permission_mode state permission_mode =
  state.options <- { state.options with permission_mode };
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
    let* session =
      Runtime_client.apply_command
        state.runtime
        ~session_id
        (Runtime.Update_session_settings
           { model = state.options.model
           ; permission_mode =
               Some (Sdk_client_types.string_of_permission_mode permission_mode)
           })
    in
    append_message state (Internal_message_parser.status session);
    Ok ()
;;

let set_model state model =
  state.options <- { state.options with model };
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
    let* session =
      Runtime_client.apply_command
        state.runtime
        ~session_id
        (Runtime.Update_session_settings
           { model
           ; permission_mode =
               Some
                 (Sdk_client_types.string_of_permission_mode
                    state.options.permission_mode)
           })
    in
    append_message state (Internal_message_parser.status session);
    Ok ()
;;

let set_can_use_tool state callback = state.can_use_tool <- Some callback
let set_hook_callback state callback = state.hook_callback <- Some callback
let close state = Runtime_client.close state.runtime

let handle_event state (event : Runtime.event) =
  state.last_event_seq <- max state.last_event_seq event.seq;
  (match state.options.include_partial_messages, event.kind with
   | true, Runtime.Agent_output_delta delta ->
     append_message
       state
       (Internal_message_parser.partial_output delta.participant_name delta.delta)
   | _ -> ());
  append_message state (Internal_message_parser.events [ event ])
;;

let handle_control_request state request =
  let open Sdk_client_types in
  match request with
  | Runtime.Permission_request detail ->
    (match state.can_use_tool with
     | Some callback ->
       (match callback detail.action detail.payload { suggestions = [] } with
        | Permission_result_allow { message } ->
          Ok (Runtime.Permission_response { allow = true; message; interrupt = false })
        | Permission_result_deny { message; interrupt } ->
          Ok (Runtime.Permission_response { allow = false; message; interrupt }))
     | None ->
       Ok
         (Runtime.Permission_response { allow = true; message = None; interrupt = false }))
  | Runtime.Hook_request detail ->
    (match state.hook_callback with
     | Some callback ->
       (match callback detail.hook_name detail.payload with
        | Hook_continue -> Ok (Runtime.Hook_response { continue_ = true; message = None })
        | Hook_block message -> Ok (Runtime.Hook_response { continue_ = false; message }))
     | None -> Ok (Runtime.Hook_response { continue_ = true; message = None }))
;;

let sync_events state =
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
    let* events =
      Runtime_client.events state.runtime ~session_id ~after_seq:state.last_event_seq ()
    in
    (match List.rev events with
     | { Runtime.seq; _ } :: _ -> state.last_event_seq <- seq
     | [] -> ());
    if events <> [] then append_message state (Internal_message_parser.events events);
    Ok ()
;;

let ensure_session state ~prompt =
  let start_new () =
    let participants = Sdk_client_types.agent_entries state.options |> List.map fst in
    let request =
      Runtime.
        { session_id = state.options.session_id
        ; goal = prompt
        ; participants
        ; provider = state.options.provider
        ; model = state.options.model
        ; permission_mode =
            Some
              (Sdk_client_types.string_of_permission_mode state.options.permission_mode)
        ; system_prompt = state.options.system_prompt
        ; max_turns = state.options.max_turns
        ; workdir = state.options.cwd
        }
    in
    let* session =
      Runtime_client.start_session
        ~control_handler:(handle_control_request state)
        ~event_handler:(handle_event state)
        state.runtime
        request
    in
    state.session_id <- Some session.session_id;
    state.last_event_seq <- session.last_seq;
    append_message state (Internal_message_parser.status session);
    Ok session
  in
  match state.session_id with
  | None -> start_new ()
  | Some session_id ->
    let* session = Runtime_client.status state.runtime ~session_id in
    (match session.phase with
     | Runtime.Completed | Runtime.Failed | Runtime.Cancelled ->
       state.session_id <- None;
       state.last_event_seq <- 0;
       start_new ()
     | Runtime.Bootstrapping
     | Runtime.Running
     | Runtime.Waiting_on_workers
     | Runtime.Finalizing -> Ok session)
;;

let compose_spawn_prompt definition prompt =
  let prefix =
    match String.trim definition.Sdk_client_types.prompt with
    | "" -> ""
    | value -> value ^ "\n\n"
  in
  prefix ^ prompt
;;

let query_turn state prompt =
  let* session = ensure_session state ~prompt in
  let* session =
    Runtime_client.apply_command
      ~control_handler:(handle_control_request state)
      ~event_handler:(handle_event state)
      state.runtime
      ~session_id:session.session_id
      (Runtime.Record_turn { actor = None; message = prompt })
  in
  append_message state (Internal_message_parser.status session);
  let* () =
    Sdk_client_types.agent_entries state.options
    |> List.fold_left
         (fun acc (name, (definition : Sdk_client_types.agent_definition)) ->
            let* () = acc in
            let command =
              Runtime.Spawn_agent
                { participant_name = name
                ; role = Some definition.description
                ; prompt = compose_spawn_prompt definition prompt
                ; provider = state.options.provider
                ; model =
                    (match definition.model with
                     | Some _ as model -> model
                     | None -> state.options.model)
                ; system_prompt = state.options.system_prompt
                ; max_turns = state.options.max_turns
                }
            in
            let* session =
              Runtime_client.apply_command
                ~control_handler:(handle_control_request state)
                ~event_handler:(handle_event state)
                state.runtime
                ~session_id:session.session_id
                command
            in
            append_message state (Internal_message_parser.status session);
            Ok ())
         (Ok ())
  in
  sync_events state
;;

let finalize state ?reason () =
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
    let* session =
      Runtime_client.finalize
        ~control_handler:(handle_control_request state)
        ~event_handler:(handle_event state)
        state.runtime
        ~session_id
        ?reason
        ()
    in
    append_message state (Internal_message_parser.status session);
    let* report = Runtime_client.report state.runtime ~session_id in
    append_message state (Internal_message_parser.report report);
    let* proof = Runtime_client.prove state.runtime ~session_id in
    append_message state (Internal_message_parser.proof proof);
    let* () = sync_events state in
    state.session_id <- None;
    Ok ()
;;

let interrupt state =
  match state.session_id with
  | None ->
    append_message
      state
      (Internal_message_parser.system "No active session to interrupt.");
    Ok ()
  | Some _ -> finalize state ~reason:"Interrupted by user" ()
;;
