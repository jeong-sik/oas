type t = {
  runtime: Runtime_client.t;
  mutable options: Sdk_client_types.options;
  mutable session_id: string option;
  mutable last_event_seq: int;
  mutable buffered_messages: Sdk_client_types.message list;
  mutable can_use_tool: Sdk_client_types.can_use_tool option;
  mutable hook_callback: Sdk_client_types.hook_callback option;
}

let ( let* ) = Result.bind

let append_message state message =
  state.buffered_messages <- state.buffered_messages @ [ message ]

let runtime_options options =
  {
    Runtime_client.runtime_path = options.Sdk_client_types.runtime_path;
    session_root = options.session_root;
  }

let connect ?(options = Sdk_client_types.default_options) () =
  let* runtime = Runtime_client.connect ~options:(runtime_options options) () in
  Ok
    {
      runtime;
      options;
      session_id = None;
      last_event_seq = 0;
      buffered_messages = [];
      can_use_tool = None;
      hook_callback = None;
    }

let receive_messages state =
  let messages = state.buffered_messages in
  state.buffered_messages <- [];
  messages

let current_session_id state = state.session_id

let set_permission_mode state permission_mode =
  state.options <- { state.options with permission_mode }

let set_model state model =
  state.options <- { state.options with model }

let set_can_use_tool state callback =
  state.can_use_tool <- Some callback

let set_hook_callback state callback =
  state.hook_callback <- Some callback

let close state = Runtime_client.close state.runtime

let handle_event state (event : Runtime.event) =
  state.last_event_seq <- max state.last_event_seq event.seq;
  append_message state (Internal_message_parser.events [ event ])

let handle_control_request state request =
  let open Sdk_client_types in
  match request with
  | Runtime.Permission_request detail -> (
      match state.can_use_tool with
      | Some callback -> (
          match callback detail.action detail.payload { suggestions = [] } with
          | Permission_result_allow { message } ->
              Ok
                (Runtime.Permission_response
                   { allow = true; message; interrupt = false })
          | Permission_result_deny { message; interrupt } ->
              Ok
                (Runtime.Permission_response
                   { allow = false; message; interrupt }))
      | None ->
          Ok
            (Runtime.Permission_response
               { allow = true; message = None; interrupt = false }))
  | Runtime.Hook_request detail -> (
      match state.hook_callback with
      | Some callback -> (
          match callback detail.hook_name detail.payload with
          | Hook_continue ->
              Ok (Runtime.Hook_response { continue_ = true; message = None })
          | Hook_block message ->
              Ok (Runtime.Hook_response { continue_ = false; message }))
      | None ->
          Ok (Runtime.Hook_response { continue_ = true; message = None }))

let sync_events state =
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
      let* events =
        Runtime_client.events state.runtime ~session_id
          ~after_seq:state.last_event_seq ()
      in
      (match List.rev events with
       | { Runtime.seq; _ } :: _ -> state.last_event_seq <- seq
       | [] -> ());
      if events <> [] then
        append_message state (Internal_message_parser.events events);
      Ok ()

let ensure_session state ~prompt =
  let start_new () =
    let participants =
      Sdk_client_types.agent_entries state.options |> List.map fst
    in
    let request =
      Runtime.
        {
          session_id = None;
          goal = prompt;
          participants;
          provider = state.options.provider;
          model = state.options.model;
          system_prompt = state.options.system_prompt;
          max_turns = state.options.max_turns;
          workdir = state.options.cwd;
        }
    in
    let* session =
      Runtime_client.start_session ~control_handler:(handle_control_request state)
        ~event_handler:(handle_event state) state.runtime request
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
       | Runtime.Bootstrapping | Runtime.Running | Runtime.Waiting_on_workers
       | Runtime.Finalizing ->
           Ok session)

let compose_spawn_prompt definition prompt =
  let prefix =
    match String.trim definition.Sdk_client_types.prompt with
    | "" -> ""
    | value -> value ^ "\n\n"
  in
  prefix ^ prompt

let query_turn state prompt =
  let* session = ensure_session state ~prompt in
  let* session =
    Runtime_client.apply_command ~control_handler:(handle_control_request state)
      ~event_handler:(handle_event state) state.runtime
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
               {
                 participant_name = name;
                 role = Some definition.description;
                 prompt = compose_spawn_prompt definition prompt;
                 provider = state.options.provider;
                 model =
                   (match definition.model with
                    | Some _ as model -> model
                    | None -> state.options.model);
                 system_prompt = state.options.system_prompt;
                 max_turns = state.options.max_turns;
               }
           in
           let* session =
             Runtime_client.apply_command
               ~control_handler:(handle_control_request state)
               ~event_handler:(handle_event state) state.runtime
               ~session_id:session.session_id
               command
           in
           append_message state (Internal_message_parser.status session);
           Ok ())
         (Ok ())
  in
  sync_events state

let finalize state ?reason () =
  match state.session_id with
  | None -> Ok ()
  | Some session_id ->
      let* session =
        Runtime_client.finalize
          ~control_handler:(handle_control_request state)
          ~event_handler:(handle_event state) state.runtime
          ~session_id ?reason ()
      in
      append_message state (Internal_message_parser.status session);
      let* report = Runtime_client.report state.runtime ~session_id in
      append_message state (Internal_message_parser.report report);
      let* proof = Runtime_client.prove state.runtime ~session_id in
      append_message state (Internal_message_parser.proof proof);
      let* () = sync_events state in
      state.session_id <- None;
      Ok ()

let interrupt state =
  match state.session_id with
  | None ->
      append_message state
        (Internal_message_parser.system "No active session to interrupt.");
      Ok ()
  | Some _ -> finalize state ~reason:"Interrupted by user" ()
