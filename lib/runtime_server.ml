open Runtime

let ( let* ) = Result.bind

type state = {
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  event_bus: Event_bus.t;
  mutable session_root: string option;
  mutable next_control_id: int;
  stdout_mu: Mutex.t;
  store_mu: Mutex.t;
}

let runtime_version = "0.1.0"

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let provider_runtime_name selected (cfg : Provider.config option) =
  match cfg with
  | None -> selected
  | Some cfg -> (
      match cfg.provider with
      | Provider.Local _ -> "local"
      | Provider.Anthropic -> "anthropic"
      | Provider.OpenAICompat _ -> "openai-compat"
      | Provider.Ollama _ -> "ollama")

type execution_resolution = {
  selected_provider: string;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  provider_cfg: Provider.config option;
}

let create ~net () =
  {
    net;
    event_bus = Event_bus.create ();
    session_root = None;
    next_control_id = 1;
    stdout_mu = Mutex.create ();
    store_mu = Mutex.create ();
  }

let store_of_state state =
  Runtime_store.create ?root:state.session_root ()

let session_root_request_path = function
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let write_protocol_message state message =
  Mutex.lock state.stdout_mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock state.stdout_mu)
    (fun () ->
      output_string stdout (protocol_message_to_string message);
      output_char stdout '\n';
      flush stdout)

let next_control_id state =
  let id = state.next_control_id in
  state.next_control_id <- id + 1;
  Printf.sprintf "ctrl-%06d" id

let emit_event state session_id (event : event) =
  Event_bus.publish state.event_bus
    (Event_bus.Custom ("runtime.event", event |> event_to_yojson));
  write_protocol_message state
    (Event_message { session_id = Some session_id; event })

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

let resolve_provider ?provider ?model () =
  let selected =
    match provider with
    | Some value when String.trim value <> "" -> String.lowercase_ascii (String.trim value)
    | _ -> "local-qwen"
  in
  let base =
    match selected with
    | "mock" | "echo" -> None
    | "local-qwen" -> Some (Provider.local_qwen ())
    | "local-mlx" -> Some (Provider.local_mlx ())
    | "sonnet" -> Some (Provider.anthropic_sonnet ())
    | "haiku" -> Some (Provider.anthropic_haiku ())
    | "opus" -> Some (Provider.anthropic_opus ())
    | "openrouter" -> Some (Provider.openrouter ())
    | "ollama" -> Some (Provider.ollama ())
    | other ->
        Some
          {
            Provider.provider = Local { base_url = "http://127.0.0.1:8085" };
            model_id = other;
            api_key_env = "LOCAL_LLM_KEY";
          }
  in
  match base with
  | None -> None
  | Some cfg ->
      Some
        {
          cfg with
          model_id =
            (match model with Some value when String.trim value <> "" -> value | _ -> cfg.model_id);
        }

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let selected_provider =
    match detail.provider with
    | Some value when String.trim value <> "" ->
        String.lowercase_ascii (String.trim value)
    | _ -> (
        match session.provider with
        | Some value when String.trim value <> "" ->
            String.lowercase_ascii (String.trim value)
        | _ -> "local-qwen")
  in
  let requested_model =
    match detail.model with
    | Some value when String.trim value <> "" -> Some (String.trim value)
    | _ -> None
  in
  match selected_provider with
  | "mock" | "echo" ->
      {
        selected_provider;
        requested_model;
        resolved_provider = Some selected_provider;
        resolved_model = first_some requested_model session.model;
        provider_cfg = None;
      }
  | _ ->
      let provider_cfg =
        resolve_provider ~provider:selected_provider
          ?model:(first_some requested_model session.model) ()
      in
      {
        selected_provider;
        requested_model;
        resolved_provider =
          Some (provider_runtime_name selected_provider provider_cfg);
        resolved_model =
          (match provider_cfg with
          | Some cfg -> Some cfg.model_id
          | None -> first_some requested_model session.model);
        provider_cfg;
      }

let extract_text (resp : Types.api_response) =
  resp.content
  |> List.filter_map (function Types.Text s -> Some s | _ -> None)
  |> String.concat "\n"

let make_event (session : session) kind =
  {
    seq = session.last_seq + 1;
    ts = Unix.gettimeofday ();
    kind;
  }

let with_store_lock state f =
  Mutex.lock state.store_mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock state.store_mu) f

let persist_event_locked store state (session : session) kind =
  let event = make_event session kind in
  let* projected = Runtime_projection.apply_event session event in
  let* () = Runtime_store.append_event store session.session_id event in
  let* () = Runtime_store.save_session store projected in
  let () = emit_event state session.session_id event in
  Ok (projected, event)

let persist_event store state session_id kind =
  with_store_lock state (fun () ->
      let* session = Runtime_store.load_session store session_id in
      persist_event_locked store state session kind)

let generate_report_and_proof store state session_id =
  with_store_lock state (fun () ->
      let* session = Runtime_store.load_session store session_id in
      let* events = Runtime_store.read_events store session_id () in
      let report = Runtime_projection.build_report session events in
      let proof = Runtime_projection.build_proof session events in
      let* () = Runtime_store.save_report store report in
      let* () = Runtime_store.save_proof store proof in
      let telemetry =
        Runtime_evidence.build_telemetry_report session events
      in
      let telemetry_json =
        Runtime_evidence.telemetry_report_to_json telemetry
        |> Yojson.Safe.pretty_to_string
      in
      let telemetry_md =
        Runtime_evidence.telemetry_report_to_markdown telemetry
      in
      let evidence =
        Runtime_evidence.build_evidence_bundle ~session_id
          [
            ("session_json", Runtime_store.session_path store session_id);
            ("events_jsonl", Runtime_store.events_path store session_id);
            ("report_json", Runtime_store.report_json_path store session_id);
            ("report_md", Runtime_store.report_md_path store session_id);
            ("proof_json", Runtime_store.proof_json_path store session_id);
            ("proof_md", Runtime_store.proof_md_path store session_id);
          ]
      in
      let evidence_json =
        Runtime_evidence.evidence_bundle_to_json evidence
        |> Yojson.Safe.pretty_to_string
      in
      let* telemetry_json_artifact =
        Artifact_service.save_text_internal store ~session_id
          ~name:"runtime-telemetry-json" ~kind:"json"
          ~content:telemetry_json
      in
      let* telemetry_md_artifact =
        Artifact_service.save_text_internal store ~session_id
          ~name:"runtime-telemetry" ~kind:"markdown"
          ~content:telemetry_md
      in
      let* evidence_artifact =
        Artifact_service.save_text_internal store ~session_id
          ~name:"runtime-evidence" ~kind:"json"
          ~content:evidence_json
      in
      let* session, _ =
        persist_event_locked store state session
          (Artifact_attached
             {
               artifact_id = telemetry_json_artifact.artifact_id;
               name = telemetry_json_artifact.name;
               kind = telemetry_json_artifact.kind;
               mime_type = telemetry_json_artifact.mime_type;
               path = Option.value ~default:"" telemetry_json_artifact.path;
               size_bytes = telemetry_json_artifact.size_bytes;
             })
      in
      let* session, _ =
        persist_event_locked store state session
          (Artifact_attached
             {
               artifact_id = telemetry_md_artifact.artifact_id;
               name = telemetry_md_artifact.name;
               kind = telemetry_md_artifact.kind;
               mime_type = telemetry_md_artifact.mime_type;
               path = Option.value ~default:"" telemetry_md_artifact.path;
               size_bytes = telemetry_md_artifact.size_bytes;
             })
      in
      let* session, _ =
        persist_event_locked store state session
          (Artifact_attached
             {
               artifact_id = evidence_artifact.artifact_id;
               name = evidence_artifact.name;
               kind = evidence_artifact.kind;
               mime_type = evidence_artifact.mime_type;
               path = Option.value ~default:"" evidence_artifact.path;
               size_bytes = evidence_artifact.size_bytes;
             })
      in
      Ok (session, report, proof))

let emit_output_delta store state session_id participant_name delta =
  if String.trim delta = "" then Ok ()
  else
    let* _session, _ =
      persist_event store state session_id
        (Agent_output_delta { participant_name; delta })
    in
    Ok ()

let run_participant store state session_id
    (resolution : execution_resolution) (detail : spawn_agent_request) =
  let emit_delta_text text =
    match emit_output_delta store state session_id detail.participant_name text with
    | Ok () -> ()
    | Error _ -> ()
  in
  let trace_sink =
    match
      Raw_trace.create_for_session ~session_root:store.root
        ~session_id ~agent_name:detail.participant_name ()
    with
    | Ok trace -> Some trace
    | Error _ -> None
  in
  match resolution.selected_provider with
  | "mock" | "echo" ->
      let full =
        Printf.sprintf "Mock runtime response for %s: %s" detail.participant_name
          detail.prompt
      in
      (match trace_sink with
       | Some sink -> (
           match
             Raw_trace.start_run sink ~agent_name:detail.participant_name
               ~prompt:detail.prompt
           with
           | Ok active ->
               ignore
                 (Raw_trace.record_assistant_block active ~block_index:0
                    (Types.Text full));
               ignore
                 (Raw_trace.finish_run active ~final_text:(Some full)
                    ~stop_reason:(Some "EndTurn") ~error:None)
           | Error _ -> ())
       | None -> ());
      let half = String.length full / 2 in
      Thread.delay 0.02;
      emit_delta_text (String.sub full 0 half);
      Thread.delay 0.02;
      emit_delta_text (String.sub full half (String.length full - half));
      Ok full
  | _ ->
      Eio.Switch.run @@ fun sw ->
      let session =
        match Runtime_store.load_session store session_id with
        | Ok session -> session
        | Error err -> raise (Failure (Error.to_string err))
      in
      let config =
        {
          Types.default_config with
          name = detail.participant_name;
          model =
            (match detail.model with
             | Some value when String.trim value <> "" -> Types.Custom value
             | _ -> Types.default_config.model);
          system_prompt =
            (match detail.system_prompt with
             | Some prompt when String.trim prompt <> "" -> Some prompt
             | _ -> session.system_prompt);
          max_turns = Option.value detail.max_turns ~default:session.max_turns;
        }
      in
      let options =
        match resolution.provider_cfg with
        | Some provider ->
            {
              Agent.default_options with
              provider = Some provider;
              raw_trace = trace_sink;
            }
        | None -> { Agent.default_options with raw_trace = trace_sink }
      in
      let agent = Agent.create ~net:state.net ~config ~options () in
      let on_event = function
        | Types.ContentBlockDelta { delta = Types.TextDelta text; _ } ->
            emit_delta_text text
        | _ -> ()
      in
      match Agent.run_stream ~sw ~on_event agent detail.prompt with
      | Ok response -> Ok (extract_text response)
      | Error err -> Error err

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
             sdk_version = "0.10.0";
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
