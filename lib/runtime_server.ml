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

let apply_command ~sw state store (session : session) command =
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
        Eio.Fiber.fork ~sw (fun () ->
          try
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
                       Ok ()))
          with
          | Eio.Cancel.Cancelled _ as ex -> raise ex
          | exn ->
            Eio.traceln "Runtime_server: participant %s fork failed: %s"
              participant_name (Printexc.to_string exn));
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

let handle_request ~sw state request =
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
      apply_command ~sw state store session command
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

let serve_stdio ~sw ~net () =
  let state = create ~net () in
  let rec loop () =
    match input_line stdin with
    | raw when String.trim raw = "" -> loop ()
    | exception End_of_file -> ()
    | raw -> (
        match protocol_message_of_string raw with
        | Ok (Request_message payload) ->
            let response =
              match handle_request ~sw state payload.request with
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
              match handle_request ~sw state request with
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

[@@@coverage off]
(* === Inline tests === *)

(* runtime_server.ml is heavily I/O-bound (stdin/stdout, Eio.Mutex, store).
   We test the pure functions it depends on:
   - session_root_request_path (from runtime_server_types)
   - Runtime wire protocol serialization (open Runtime)
   - first_some (alias for Util.first_some) *)

(* --- first_some --- *)

let%test "first_some: Some a, Some b -> Some a" =
  first_some (Some "a") (Some "b") = Some "a"

let%test "first_some: None, Some b -> Some b" =
  first_some None (Some "b") = Some "b"

let%test "first_some: Some a, None -> Some a" =
  first_some (Some "a") None = Some "a"

let%test "first_some: None, None -> None" =
  first_some None None = None

(* --- session_root_request_path --- *)

let%test "session_root_request_path: None -> None" =
  session_root_request_path None = None

let%test "session_root_request_path: Some empty -> None" =
  session_root_request_path (Some "") = None

let%test "session_root_request_path: Some whitespace -> None" =
  session_root_request_path (Some "   ") = None

let%test "session_root_request_path: Some valid -> Some trimmed" =
  session_root_request_path (Some "/tmp/sessions") = Some "/tmp/sessions"

let%test "session_root_request_path: Some with spaces -> Some trimmed" =
  session_root_request_path (Some "  /tmp/sessions  ") = Some "/tmp/sessions"

(* --- Runtime.protocol_version --- *)

let%test "protocol_version is not empty" =
  String.length Runtime.protocol_version > 0

(* --- Runtime wire protocol: request serialization roundtrip --- *)

let%test "request roundtrip: Shutdown" =
  let json_str = request_to_string Shutdown in
  match request_of_string json_str with
  | Ok Shutdown -> true
  | _ -> false

let%test "request roundtrip: Initialize" =
  let req = Initialize {
    session_root = Some "/tmp";
    provider = Some "mock";
    model = None;
    permission_mode = None;
    include_partial_messages = false;
    setting_sources = [];
    resume_session = None;
    cwd = None;
  } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Initialize r) -> r.session_root = Some "/tmp" && r.provider = Some "mock"
  | _ -> false

let%test "request roundtrip: Start_session" =
  let req = Start_session {
    session_id = None;
    goal = "test goal";
    participants = ["alice"; "bob"];
    provider = Some "mock";
    model = None;
    permission_mode = None;
    system_prompt = None;
    max_turns = Some 5;
    workdir = None;
  } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Start_session r) -> r.goal = "test goal" && r.participants = ["alice"; "bob"]
  | _ -> false

let%test "request roundtrip: Status" =
  let req = Status { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Status { session_id }) -> session_id = "s123"
  | _ -> false

let%test "request roundtrip: Events" =
  let req = Events { session_id = "s123"; after_seq = Some 5 } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Events { session_id; after_seq }) ->
      session_id = "s123" && after_seq = Some 5
  | _ -> false

let%test "request roundtrip: Finalize" =
  let req = Finalize { session_id = "s123"; reason = Some "done" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Finalize { session_id; reason }) ->
      session_id = "s123" && reason = Some "done"
  | _ -> false

let%test "request roundtrip: Report" =
  let req = Report { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Report { session_id }) -> session_id = "s123"
  | _ -> false

let%test "request roundtrip: Prove" =
  let req = Prove { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Prove { session_id }) -> session_id = "s123"
  | _ -> false

let%test "request roundtrip: Apply_command with Record_turn" =
  let req = Apply_command {
    session_id = "s123";
    command = Record_turn { actor = Some "alice"; message = "hello" }
  } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Apply_command { session_id; command = Record_turn { actor; message } }) ->
      session_id = "s123" && actor = Some "alice" && message = "hello"
  | _ -> false

let%test "request roundtrip: Apply_command with Vote" =
  let req = Apply_command {
    session_id = "s1";
    command = Vote { topic = "color"; options = ["red"; "blue"]; choice = "red"; actor = Some "bob" }
  } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Apply_command { command = Vote { topic; options; choice; _ }; _ }) ->
      topic = "color" && options = ["red"; "blue"] && choice = "red"
  | _ -> false

(* --- response serialization roundtrip --- *)

let%test "response roundtrip: Shutdown_ack" =
  let json_str = response_to_string Shutdown_ack in
  match response_of_string json_str with
  | Ok Shutdown_ack -> true
  | _ -> false

let%test "response roundtrip: Error_response" =
  let json_str = response_to_string (Error_response "something failed") in
  match response_of_string json_str with
  | Ok (Error_response msg) -> msg = "something failed"
  | _ -> false

let%test "response roundtrip: Initialized" =
  let resp = Initialized {
    sdk_name = "agent_sdk";
    sdk_version = "1.0.0";
    runtime_version = "1.0.0";
    protocol_version = "oas-runtime-0.1";
    capabilities = ["initialize"; "shutdown"];
  } in
  let json_str = response_to_string resp in
  match response_of_string json_str with
  | Ok (Initialized r) ->
      r.sdk_name = "agent_sdk" && r.protocol_version = "oas-runtime-0.1"
      && List.length r.capabilities = 2
  | _ -> false

(* --- protocol_message serialization --- *)

let%test "protocol_message roundtrip: Request_message" =
  let msg = Request_message {
    request_id = "req-1";
    request = Shutdown;
  } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Request_message { request_id; request = Shutdown }) ->
      request_id = "req-1"
  | _ -> false

let%test "protocol_message roundtrip: Response_message" =
  let msg = Response_message {
    request_id = "req-2";
    response = Shutdown_ack;
  } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Response_message { request_id; response = Shutdown_ack }) ->
      request_id = "req-2"
  | _ -> false

let%test "protocol_message roundtrip: Event_message" =
  let msg = Event_message {
    session_id = Some "s1";
    event = { seq = 1; ts = 100.0;
              kind = Session_started { goal = "test"; participants = [] } };
  } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Event_message { session_id = Some "s1"; event }) ->
      event.seq = 1
  | _ -> false

let%test "protocol_message roundtrip: Control_request_message" =
  let msg = Control_request_message {
    control_id = "ctrl-000001";
    request = Permission_request { action = "spawn"; subject = "a1"; payload = `Null };
  } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Control_request_message { control_id; _ }) ->
      control_id = "ctrl-000001"
  | _ -> false

let%test "protocol_message roundtrip: Control_response_message" =
  let msg = Control_response_message {
    control_id = "ctrl-000002";
    response = Permission_response { allow = true; message = None; interrupt = false };
  } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Control_response_message { control_id; response = Permission_response r }) ->
      control_id = "ctrl-000002" && r.allow = true
  | _ -> false

let%test "protocol_message roundtrip: System_message" =
  let msg = System_message { level = "info"; message = "hello" } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (System_message { level; message }) ->
      level = "info" && message = "hello"
  | _ -> false

(* --- request_of_string error handling --- *)

let%test "request_of_string: invalid JSON returns Error" =
  match request_of_string "not valid json {{{" with
  | Error _ -> true
  | Ok _ -> false

let%test "protocol_message_of_string: invalid JSON returns Error" =
  match protocol_message_of_string "garbage" with
  | Error _ -> true
  | Ok _ -> false

let%test "response_of_string: invalid JSON returns Error" =
  match response_of_string "not json" with
  | Error _ -> true
  | Ok _ -> false

(* --- event_kind serialization roundtrip via event --- *)

let%test "event roundtrip: Turn_recorded" =
  let event = { seq = 3; ts = 200.0;
                kind = Turn_recorded { actor = Some "alice"; message = "hi" } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> e.seq = 3 && (match e.kind with Turn_recorded { actor = Some "alice"; message = "hi" } -> true | _ -> false)
  | Error _ -> false

let%test "event roundtrip: Artifact_attached" =
  let event = { seq = 4; ts = 300.0;
                kind = Artifact_attached {
                  artifact_id = "art-1"; name = "report"; kind = "json";
                  mime_type = "application/json"; path = "/tmp/report.json";
                  size_bytes = 1234 } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> (match e.kind with Artifact_attached { artifact_id = "art-1"; size_bytes = 1234; _ } -> true | _ -> false)
  | Error _ -> false

let%test "event roundtrip: Vote_recorded" =
  let event = { seq = 5; ts = 400.0;
                kind = Vote_recorded { topic = "color"; options = ["red"; "blue"];
                  choice = "red"; actor = Some "bob"; created_at = 400.0 } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> (match e.kind with Vote_recorded { topic = "color"; _ } -> true | _ -> false)
  | Error _ -> false

let%test "event roundtrip: Checkpoint_saved" =
  let event = { seq = 6; ts = 500.0;
                kind = Checkpoint_saved { label = Some "mid"; path = "/tmp/snap" } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> (match e.kind with Checkpoint_saved { label = Some "mid"; _ } -> true | _ -> false)
  | Error _ -> false

let%test "event roundtrip: Session_completed" =
  let event = { seq = 7; ts = 600.0;
                kind = Session_completed { outcome = Some "success" } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> (match e.kind with Session_completed { outcome = Some "success" } -> true | _ -> false)
  | Error _ -> false

let%test "event roundtrip: Session_failed" =
  let event = { seq = 8; ts = 700.0;
                kind = Session_failed { outcome = Some "timeout" } } in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e -> (match e.kind with Session_failed { outcome = Some "timeout" } -> true | _ -> false)
  | Error _ -> false
