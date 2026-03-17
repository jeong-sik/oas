open Runtime
open Runtime_server_types
open Runtime_server_resolve
open Runtime_server_worker

let ( let* ) = Result.bind

let first_some = Util.first_some

let rec read_control_response state control_id =
  match input_line stdin with
  | raw when String.trim raw = "" ->
      read_control_response state control_id
  | exception End_of_file ->
      Error
        (Error.Io
           (FileOpFailed
              {
                op = "read";
                path = "stdin";
                detail = "runtime control channel closed";
              }))
  | raw -> (
      match protocol_message_of_string raw with
      | Error detail ->
          Error (Error.Serialization (JsonParseError { detail }))
      | Ok (Control_response_message payload)
        when String.equal payload.control_id control_id ->
          Ok payload.response
      | Ok _ -> read_control_response state control_id)

let ask_permission state ~action ~subject ~payload =
  let control_id = next_control_id state in
  write_protocol_message state
    (Control_request_message
       {
         control_id;
         request = Permission_request { action; subject; payload };
       });
  read_control_response state control_id

let invoke_hook state ~hook_name ~payload =
  let control_id = next_control_id state in
  write_protocol_message state
    (Control_request_message
       {
         control_id;
         request = Hook_request { hook_name; payload };
       });
  read_control_response state control_id

let start_session state (request : start_request) =
  let* store = store_of_state state in
  let session = Runtime_projection.initial_session request in
  let* _ =
    invoke_hook state ~hook_name:"SessionStart"
      ~payload:(start_request_to_yojson request)
  in
  let* () =
    with_store_lock state (fun () -> Runtime_store.save_session store session)
  in
  let* projected, _ =
    persist_event store state session.session_id
      (Session_started { goal = request.goal; participants = request.participants })
  in
  Ok (Session_started_response projected)

let finalize_session state store (session : session) reason =
  let session_id = session.session_id in
  let* session, _ =
    match session.phase with
    | Finalizing ->
        Ok (session, make_event session (Finalize_requested { reason }))
    | Bootstrapping | Running | Waiting_on_workers ->
        persist_event store state session_id (Finalize_requested { reason })
    | Completed | Failed | Cancelled ->
        Ok (session, make_event session (Session_completed { outcome = session.outcome }))
  in
  let completion_kind =
    match session.phase with
    | Failed -> Session_failed { outcome = reason }
    | Completed | Cancelled -> Session_completed { outcome = session.outcome }
    | Bootstrapping | Running | Waiting_on_workers | Finalizing ->
        Session_completed { outcome = first_some reason session.outcome }
  in
  let* _final_session, _ = persist_event store state session_id completion_kind in
  let* final_session, _report, _proof =
    generate_report_and_proof store state session_id
  in
  Ok (Finalized final_session)

let apply_command state store (session : session) command =
  let session_id = session.session_id in
  match command with
  | Record_turn detail ->
      let* session, _ =
        persist_event store state session_id
          (Turn_recorded { actor = detail.actor; message = detail.message })
      in
      Ok (Command_applied session)
  | Update_session_settings detail ->
      let* session, _ =
        persist_event store state session_id
          (Session_settings_updated detail)
      in
      Ok (Command_applied session)
  | Spawn_agent detail ->
      let* permission =
        ask_permission state ~action:"spawn_agent" ~subject:detail.participant_name
          ~payload:(spawn_agent_request_to_yojson detail)
      in
      let permission_allowed, permission_message =
        match permission with
        | Permission_response result -> (result.allow, result.message)
        | Hook_response _ -> (true, None)
      in
      let* session, _ =
        persist_event store state session_id
          (Agent_spawn_requested
             {
               participant_name = detail.participant_name;
               role = detail.role;
               prompt = detail.prompt;
               provider = detail.provider;
               model = detail.model;
               permission_mode = session.permission_mode;
             })
      in
      let* hook_response =
        invoke_hook state ~hook_name:"PreSpawn"
          ~payload:(spawn_agent_request_to_yojson detail)
      in
      let hook_allowed, hook_message =
        match hook_response with
        | Hook_response result -> (result.continue_, result.message)
        | Permission_response _ -> (true, None)
      in
      let resolution = resolve_execution session detail in
      if not permission_allowed || not hook_allowed then
        let* session, _ =
          persist_event store state session_id
            (Agent_failed
               {
                 participant_name = detail.participant_name;
                 summary = None;
                 provider = resolution.resolved_provider;
                 model = resolution.resolved_model;
                 error =
                   Some
                     (Option.value
                        ~default:"spawn blocked by control policy"
                        (first_some permission_message hook_message));
               })
        in
        Ok (Command_applied session)
      else
        let participant_name = detail.participant_name in
        let _worker =
          Thread.create
            (fun () ->
              ignore
                (match
                   persist_event store state session_id
                     (Agent_became_live
                        {
                          participant_name;
                          summary = Some "runtime-started";
                          provider = resolution.resolved_provider;
                          model = resolution.resolved_model;
                          error = None;
                        })
                 with
                 | Error _ -> Ok ()
                 | Ok _ -> (
                     match
                       run_participant store state session_id resolution detail
                     with
                     | Ok summary ->
                         let* _session, _ =
                           persist_event store state session_id
                             (Agent_completed
                                {
                                  participant_name;
                                  summary = Some summary;
                                  provider = resolution.resolved_provider;
                                  model = resolution.resolved_model;
                                  error = None;
                                })
                         in
                         Ok ()
                     | Error err ->
                         let* _session, _ =
                           persist_event store state session_id
                             (Agent_failed
                                {
                                  participant_name;
                                  summary = None;
                                  provider = resolution.resolved_provider;
                                  model = resolution.resolved_model;
                                  error = Some (Error.to_string err);
                                })
                         in
                         Ok ())))
            ()
        in
        Ok (Command_applied session)
  | Attach_artifact detail ->
      let* artifact =
        Artifact_service.save_text_internal store ~session_id:session.session_id
          ~name:detail.name ~kind:detail.kind ~content:detail.content
      in
      let* session, _ =
        persist_event store state session_id
          (Artifact_attached
             {
               artifact_id = artifact.artifact_id;
               name = artifact.name;
               kind = artifact.kind;
               mime_type = artifact.mime_type;
               path = Option.value ~default:"" artifact.path;
               size_bytes = artifact.size_bytes;
             })
      in
      Ok (Command_applied session)
  | Vote detail ->
      let vote =
        {
          topic = detail.topic;
          options = detail.options;
          choice = detail.choice;
          actor = detail.actor;
          created_at = Unix.gettimeofday ();
        }
      in
      let* session, _ = persist_event store state session_id (Vote_recorded vote) in
      Ok (Command_applied session)
  | Checkpoint detail ->
      let path =
        Runtime_store.snapshot_path store session.session_id
          ~seq:(session.last_seq + 1) ~label:detail.label
      in
      let* session, _ =
        persist_event store state session_id
          (Checkpoint_saved { label = detail.label; path })
      in
      let* _ = Runtime_store.save_snapshot store session ~label:detail.label in
      Ok (Command_applied session)
  | Request_finalize detail -> finalize_session state store session detail.reason

let handle_request state request =
  match request with
  | Initialize detail ->
      state.session_root <- session_root_request_path detail.session_root;
      let* _store = store_of_state state in
      Ok
        (Initialized
           {
             sdk_name = "agent_sdk";
             sdk_version = Sdk_version.version;
             runtime_version;
             protocol_version = Runtime.protocol_version;
             capabilities =
               [
                 "initialize";
                 "start_session";
                 "apply_command";
                 "status";
                 "events";
                 "finalize";
                 "report";
                 "prove";
               ];
           })
  | Start_session detail -> start_session state detail
  | Apply_command { session_id; command } ->
      let* store = store_of_state state in
      let* session = Runtime_store.load_session store session_id in
      apply_command state store session command
  | Status { session_id } ->
      let* store = store_of_state state in
      let* session = Runtime_store.load_session store session_id in
      Ok (Status_response session)
  | Events { session_id; after_seq } ->
      let* store = store_of_state state in
      let* events = Runtime_store.read_events store session_id ?after_seq () in
      Ok (Events_response events)
  | Finalize { session_id; reason } ->
      let* store = store_of_state state in
      let* session = Runtime_store.load_session store session_id in
      let* _ =
        invoke_hook state ~hook_name:"Stop"
          ~payload:
            (`Assoc
               [
                 ("session_id", `String session_id);
                 ( "reason",
                   match reason with Some value -> `String value | None -> `Null );
               ])
      in
      finalize_session state store session reason
  | Report { session_id } ->
      let* store = store_of_state state in
      let* session = Runtime_store.load_session store session_id in
      let* events = Runtime_store.read_events store session_id () in
      let report = Runtime_projection.build_report session events in
      let* () = Runtime_store.save_report store report in
      Ok (Report_response report)
  | Prove { session_id } ->
      let* store = store_of_state state in
      let* session = Runtime_store.load_session store session_id in
      let* events = Runtime_store.read_events store session_id () in
      let proof = Runtime_projection.build_proof session events in
      let* () = Runtime_store.save_proof store proof in
      Ok (Prove_response proof)
  | Shutdown -> Ok Shutdown_ack

let serve_stdio ~net () =
  let state = create ~net () in
  let rec loop () =
    match input_line stdin with
    | raw when String.trim raw = "" -> loop ()
    | exception End_of_file -> ()
    | raw -> (
        match protocol_message_of_string raw with
        | Ok (Request_message payload) ->
            let response =
              match handle_request state payload.request with
              | Ok response -> response
              | Error err -> Error_response (Error.to_string err)
            in
            write_protocol_message state
              (Response_message
                 { request_id = payload.request_id; response });
            (match response with
             | Shutdown_ack -> ()
             | _ -> loop ())
        | Ok _ -> loop ()
        | Error _ -> (
            match request_of_string raw with
            | Error detail ->
                write_protocol_message state
                  (Response_message
                     { request_id = "legacy"; response = Error_response detail });
                loop ()
            | Ok request ->
            let response =
              match handle_request state request with
              | Ok response -> response
              | Error err -> Error_response (Error.to_string err)
            in
            write_protocol_message state
              (Response_message { request_id = "legacy"; response });
            match response with
            | Shutdown_ack -> ()
            | _ -> loop ()))
  in
  loop ()
