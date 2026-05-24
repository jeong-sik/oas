open Runtime
open Runtime_server_types
open Runtime_server_resolve
open Result_syntax

let _wlog = Log.create ~module_name:"runtime_server_worker" ()

let unsupported_test_provider provider =
  Error.Config
    (Error.UnsupportedProvider
       { detail =
           Printf.sprintf
             "provider %S is test-only; set OAS_ALLOW_TEST_PROVIDERS=1 to enable it \
              explicitly"
             provider
       })
;;

let extract_text (resp : Types.api_response) =
  resp.content
  |> List.filter_map (function
    | Types.Text s -> Some s
    | Types.Thinking _
    | Types.RedactedThinking _
    | Types.ToolUse _
    | Types.ToolResult _
    | Types.Image _
    | Types.Document _
    | Types.Audio _ -> None)
  |> String.concat "\n"
;;

type participant_run_success =
  { summary : string
  ; raw_trace_run_id : string option
  ; stop_reason : string option
  ; completion_anomaly : Runtime.completion_anomaly option
  }

type participant_run_failure =
  { error : Error.sdk_error
  ; raw_trace_run_id : string option
  }

type paused_participant =
  { detail : Runtime.spawn_agent_request
  ; resolution : execution_resolution
  ; agent : Agent.t
  ; input_required : Error.input_required
  ; trace_sink : Raw_trace.t option
  ; delta_warn_logged : bool ref
  ; delta_error_count : int ref
  }

type participant_run_result =
  | Participant_completed of participant_run_success
  | Participant_input_required of Runtime.input_request * paused_participant

let latest_raw_trace_run_id = function
  | Some sink ->
    Option.map
      (fun (run : Raw_trace.run_ref) -> run.worker_run_id)
      (Raw_trace.last_run sink)
  | None -> None
;;

let paused_inputs_mu = Eio.Mutex.create ()
let paused_inputs : (string, paused_participant) Hashtbl.t = Hashtbl.create 16
let paused_input_key session_id request_id = session_id ^ "\000" ^ request_id

let store_paused_input session_id (paused : paused_participant) =
  Eio.Mutex.use_rw ~protect:true paused_inputs_mu (fun () ->
    Hashtbl.replace
      paused_inputs
      (paused_input_key session_id paused.input_required.request_id)
      paused)
;;

let take_paused_input session_id request_id =
  Eio.Mutex.use_rw ~protect:true paused_inputs_mu (fun () ->
    let key = paused_input_key session_id request_id in
    let paused = Hashtbl.find_opt paused_inputs key in
    Hashtbl.remove paused_inputs key;
    paused)
;;

let make_event (session : session) kind =
  { seq = session.last_seq + 1; ts = Unix.gettimeofday (); kind }
;;

let with_store_lock state f = Eio.Mutex.use_rw ~protect:true state.store_mu f

let persist_event_locked store state (session : session) kind =
  let event = make_event session kind in
  let* projected = Runtime_projection.apply_event session event in
  let* () = Runtime_store.append_event store session.session_id event in
  let* () = Runtime_store.save_session store projected in
  let () = emit_event state session.session_id event in
  Ok (projected, event)
;;

let persist_event store state session_id kind =
  with_store_lock state (fun () ->
    let* session = Runtime_store.load_session store session_id in
    persist_event_locked store state session kind)
;;

let persist_artifact_events_locked store state session (artifacts : Runtime.artifact list)
  =
  List.fold_left
    (fun acc artifact ->
       let* session = acc in
       let* session, _ =
         persist_event_locked
           store
           state
           session
           (Runtime_evidence.artifact_attached_event artifact)
       in
       Ok session)
    (Ok session)
    artifacts
;;

let build_raw_trace_manifest (store : Runtime_store.t) session_id =
  let session_root = Some store.root in
  let* latest_raw_trace_run =
    Sessions.get_latest_raw_trace_run ?session_root ~session_id ()
  in
  let* raw_trace_runs = Sessions.get_raw_trace_runs ?session_root ~session_id () in
  let* raw_trace_summaries =
    Sessions.get_raw_trace_summaries ?session_root ~session_id ()
  in
  let* raw_trace_validations =
    Sessions.get_raw_trace_validations ?session_root ~session_id ()
  in
  Ok
    (Runtime_evidence.build_raw_trace_manifest
       ~session_id
       ~latest_raw_trace_run
       ~raw_trace_runs
       ~raw_trace_summaries
       ~raw_trace_validations)
;;

let generate_report_and_proof store state session_id =
  with_store_lock state (fun () ->
    let* session = Runtime_store.load_session store session_id in
    let* events = Runtime_store.read_events store session_id () in
    let report = Runtime_projection.build_report session events in
    let proof = Runtime_projection.build_proof session events in
    let* () = Runtime_store.save_report store report in
    let* () = Runtime_store.save_proof store proof in
    let telemetry = Runtime_evidence.build_telemetry_report session events in
    let telemetry_json =
      Runtime_evidence.telemetry_report_to_json telemetry |> Yojson.Safe.pretty_to_string
    in
    let telemetry_md = Runtime_evidence.telemetry_report_to_markdown telemetry in
    let* telemetry_json_artifact =
      Artifact_service.save_text_internal
        store
        ~session_id
        ~name:"runtime-telemetry-json"
        ~kind:"json"
        ~content:telemetry_json
    in
    let* telemetry_md_artifact =
      Artifact_service.save_text_internal
        store
        ~session_id
        ~name:"runtime-telemetry"
        ~kind:"markdown"
        ~content:telemetry_md
    in
    let* telemetry_json_path = Artifact_service.persisted_path telemetry_json_artifact in
    let* telemetry_md_path = Artifact_service.persisted_path telemetry_md_artifact in
    let* raw_trace_manifest = build_raw_trace_manifest store session_id in
    let raw_trace_json =
      Runtime_evidence.raw_trace_manifest_to_json raw_trace_manifest
      |> Yojson.Safe.pretty_to_string
    in
    let* raw_trace_artifact =
      Artifact_service.save_text_internal
        store
        ~session_id
        ~name:"runtime-raw-trace-json"
        ~kind:"json"
        ~content:raw_trace_json
    in
    let* raw_trace_json_path = Artifact_service.persisted_path raw_trace_artifact in
    let evidence =
      Runtime_evidence.build_evidence_bundle
        ~session_id
        (Runtime_evidence.base_evidence_file_specs store session_id
         @ [ "telemetry_json", telemetry_json_path
           ; "telemetry_md", telemetry_md_path
           ; "raw_trace_json", raw_trace_json_path
           ])
    in
    let evidence_json =
      Runtime_evidence.evidence_bundle_to_json evidence |> Yojson.Safe.pretty_to_string
    in
    let* evidence_artifact =
      Artifact_service.save_text_internal
        store
        ~session_id
        ~name:"runtime-evidence"
        ~kind:"json"
        ~content:evidence_json
    in
    let artifacts =
      [ telemetry_json_artifact
      ; telemetry_md_artifact
      ; raw_trace_artifact
      ; evidence_artifact
      ]
    in
    let* final_session = persist_artifact_events_locked store state session artifacts in
    let* final_events = Runtime_store.read_events store session_id () in
    let final_report = Runtime_projection.build_report final_session final_events in
    let final_proof = Runtime_projection.build_proof final_session final_events in
    let final_telemetry =
      Runtime_evidence.build_telemetry_report final_session final_events
    in
    let final_telemetry_json =
      Runtime_evidence.telemetry_report_to_json final_telemetry
      |> Yojson.Safe.pretty_to_string
    in
    let final_telemetry_md =
      Runtime_evidence.telemetry_report_to_markdown final_telemetry
    in
    let* () = Runtime_store.save_report store final_report in
    let* () = Runtime_store.save_proof store final_proof in
    let* () =
      Artifact_service.overwrite_text_internal
        telemetry_json_artifact
        ~content:final_telemetry_json
    in
    let* () =
      Artifact_service.overwrite_text_internal
        telemetry_md_artifact
        ~content:final_telemetry_md
    in
    let final_evidence =
      Runtime_evidence.build_evidence_bundle
        ~session_id
        (Runtime_evidence.base_evidence_file_specs store session_id
         @ [ "telemetry_json", telemetry_json_path
           ; "telemetry_md", telemetry_md_path
           ; "raw_trace_json", raw_trace_json_path
           ])
    in
    let final_evidence_json =
      Runtime_evidence.evidence_bundle_to_json final_evidence
      |> Yojson.Safe.pretty_to_string
    in
    let* () =
      Artifact_service.overwrite_text_internal
        evidence_artifact
        ~content:final_evidence_json
    in
    Ok (final_session, final_report, final_proof))
;;

let emit_output_delta store state session_id participant_name delta =
  if String.trim delta = ""
  then Ok ()
  else
    let* _session, _ =
      persist_event
        store
        state
        session_id
        (Agent_output_delta { participant_name; delta })
    in
    Ok ()
;;

let emit_delta_text_with_refs
      store
      state
      session_id
      participant_name
      ~delta_warn_logged
      ~delta_error_count
      text
  =
  match emit_output_delta store state session_id participant_name text with
  | Ok () -> ()
  | Error e ->
    incr delta_error_count;
    if not !delta_warn_logged
    then (
      delta_warn_logged := true;
      Log.warn
        _wlog
        "output delta emission failed"
        [ Log.S ("session_id", session_id)
        ; Log.S ("participant", participant_name)
        ; Log.S ("error", Error.to_string e)
        ])
;;

let completion_anomaly_of_delta_errors delta_error_count =
  if !delta_error_count > 0
  then Some (Runtime.Dropped_output_deltas { count = !delta_error_count })
  else None
;;

let run_participant
      store
      state
      session_id
      (resolution : execution_resolution)
      (detail : spawn_agent_request)
  =
  let delta_warn_logged = ref false in
  let delta_error_count = ref 0 in
  let emit_delta_text text =
    emit_delta_text_with_refs
      store
      state
      session_id
      detail.participant_name
      ~delta_warn_logged
      ~delta_error_count
      text
  in
  let trace_sink =
    match
      Raw_trace.create_for_session
        ~session_root:store.root
        ~session_id
        ~agent_name:detail.participant_name
        ()
    with
    | Ok trace -> Some trace
    | Error e ->
      let _wlog = Log.create ~module_name:"runtime_server_worker" () in
      Log.warn
        _wlog
        "trace sink creation failed"
        [ Log.S ("session_id", session_id)
        ; Log.S ("agent", detail.participant_name)
        ; Log.S ("error", Error.to_string e)
        ];
      None
  in
  let completion_anomaly () = completion_anomaly_of_delta_errors delta_error_count in
  match resolution.selected_provider with
  | "mock" | "echo" ->
    if not (Defaults.allow_test_providers ())
    then
      Error
        { error = unsupported_test_provider resolution.selected_provider
        ; raw_trace_run_id = latest_raw_trace_run_id trace_sink
        }
    else (
      let full =
        Printf.sprintf
          "Mock runtime response for %s: %s"
          detail.participant_name
          detail.prompt
      in
      (match trace_sink with
       | Some sink ->
         (match
            Raw_trace.start_run
              sink
              ~agent_name:detail.participant_name
              ~prompt:detail.prompt
              ?model:resolution.resolved_model
              ()
          with
          | Ok active ->
            ignore
              (Raw_trace.record_assistant_block active ~block_index:0 (Types.Text full));
            ignore
              (Raw_trace.finish_run
                 active
                 ~final_text:(Some full)
                 ~stop_reason:(Some "EndTurn")
                 ~error:None)
          | Error e ->
            Log.warn
              _wlog
              "trace start_run failed for mock provider"
              [ Log.S ("session_id", session_id)
              ; Log.S ("agent", detail.participant_name)
              ; Log.S ("error", Error.to_string e)
              ])
       | None -> ());
      let half = String.length full / 2 in
      emit_delta_text (String.sub full 0 half);
      emit_delta_text (String.sub full half (String.length full - half));
      if !delta_error_count > 0
      then
        Log.warn
          _wlog
          "participant completed with dropped output deltas"
          [ Log.S ("session_id", session_id)
          ; Log.S ("participant", detail.participant_name)
          ; Log.I ("dropped_output_deltas", !delta_error_count)
          ];
      Ok
        (Participant_completed
           { summary = full
           ; raw_trace_run_id = latest_raw_trace_run_id trace_sink
           ; stop_reason = Some "EndTurn"
           ; completion_anomaly = completion_anomaly ()
           }))
  | _selected_provider ->
    Eio.Switch.run
    @@ fun sw ->
    (match Runtime_store.load_session store session_id with
     | Error err ->
       Error { error = err; raw_trace_run_id = latest_raw_trace_run_id trace_sink }
     | Ok session ->
       let config =
         { Types.default_config with
           name = detail.participant_name
         ; model =
             (match detail.model with
              | Some value when String.trim value <> "" ->
                Model_registry.resolve_model_id value
              | _missing_model_override -> Types.default_config.model)
         ; system_prompt =
             (match detail.system_prompt with
              | Some prompt when String.trim prompt <> "" -> Some prompt
              | _missing_system_prompt_override -> session.system_prompt)
         ; max_turns = Option.value detail.max_turns ~default:session.max_turns
         }
       in
       let options =
         match resolution.provider_cfg with
         | Some provider ->
           { Agent.default_options with provider = Some provider; raw_trace = trace_sink }
         | None -> { Agent.default_options with raw_trace = trace_sink }
       in
       let agent = Agent.create ~net:state.net ~config ~options () in
       let on_event = function
         | Types.ContentBlockDelta { delta = Types.TextDelta text; _ } ->
           emit_delta_text text
         | _other_event -> ()
       in
       (match Agent.run_stream ~sw ~on_event agent detail.prompt with
        | Ok response ->
          if !delta_error_count > 0
          then
            Log.warn
              _wlog
              "participant completed with dropped output deltas"
              [ Log.S ("session_id", session_id)
              ; Log.S ("participant", detail.participant_name)
              ; Log.I ("dropped_output_deltas", !delta_error_count)
              ];
          Ok
            (Participant_completed
               { summary = extract_text response
               ; raw_trace_run_id = latest_raw_trace_run_id trace_sink
               ; stop_reason = Some (Types.show_stop_reason response.stop_reason)
               ; completion_anomaly = completion_anomaly ()
               })
        | Error (Error.Agent (Error.InputRequired request)) ->
          Ok
            (Participant_input_required
               ( Agent_elicitation.runtime_input_request_of_input_required request
               , { detail
                 ; resolution
                 ; agent
                 ; input_required = request
                 ; trace_sink
                 ; delta_warn_logged
                 ; delta_error_count
                 } ))
        | Error err ->
          Error { error = err; raw_trace_run_id = latest_raw_trace_run_id trace_sink }))
;;

let first_some = Util.first_some
let _log = Log.create ~module_name:"runtime_server" ()
let read_control_response = Runtime_server_control.read_control_response
let ask_permission = Runtime_server_control.ask_permission
let invoke_hook = Runtime_server_control.invoke_hook

let log_participant_persist_failure ~session_id ~participant_name ~phase err =
  Log.error
    _log
    "participant event persistence failed"
    [ Log.S ("session_id", session_id)
    ; Log.S ("participant", participant_name)
    ; Log.S ("phase", phase)
    ; Log.S ("error", Error.to_string err)
    ]
;;

let persist_participant_failure
      store
      state
      ~session_id
      ~participant_name
      ~provider
      ~model
      ~detail
      ?raw_trace_run_id
      ?failure_cause
      ()
  =
  match
    persist_event
      store
      state
      session_id
      (Agent_failed
         { participant_name
         ; summary = None
         ; provider
         ; model
         ; error = Some detail
         ; raw_trace_run_id
         ; stop_reason = None
         ; completion_anomaly = None
         ; failure_cause
         })
  with
  | Ok _ -> ()
  | Error err ->
    log_participant_persist_failure
      ~session_id
      ~participant_name
      ~phase:"agent_failed"
      err
;;

let persist_participant_completion
      store
      state
      ~session_id
      ~participant_name
      ~(resolution : execution_resolution)
      (outcome : participant_run_success)
  =
  match
    persist_event
      store
      state
      session_id
      (Agent_completed
         { participant_name
         ; summary = Some outcome.summary
         ; provider = resolution.resolved_provider
         ; model = resolution.resolved_model
         ; error = None
         ; raw_trace_run_id = outcome.raw_trace_run_id
         ; stop_reason = outcome.stop_reason
         ; completion_anomaly = outcome.completion_anomaly
         ; failure_cause = None
         })
  with
  | Ok _ -> ()
  | Error err ->
    let detail =
      Printf.sprintf
        "participant completed but completion event could not be persisted: %s"
        (Error.to_string err)
    in
    log_participant_persist_failure
      ~session_id
      ~participant_name
      ~phase:"agent_completed"
      err;
    persist_participant_failure
      store
      state
      ~session_id
      ~participant_name
      ~provider:resolution.resolved_provider
      ~model:resolution.resolved_model
      ~detail
      ?raw_trace_run_id:outcome.raw_trace_run_id
      ~failure_cause:(Persistence_failure { phase = "agent_completed"; detail })
      ()
;;

let persist_participant_input_required
      store
      state
      ~session_id
      ~participant_name
      ~(resolution : execution_resolution)
      request
      paused
  =
  store_paused_input session_id paused;
  match persist_event store state session_id (Input_required request) with
  | Ok _ -> ()
  | Error err ->
    ignore (take_paused_input session_id request.request_id);
    let detail =
      Printf.sprintf
        "participant requested input but input_required event could not be persisted: %s"
        (Error.to_string err)
    in
    log_participant_persist_failure
      ~session_id
      ~participant_name
      ~phase:"input_required"
      err;
    persist_participant_failure
      store
      state
      ~session_id
      ~participant_name
      ~provider:resolution.resolved_provider
      ~model:resolution.resolved_model
      ~detail
      ~failure_cause:(Persistence_failure { phase = "input_required"; detail })
      ()
;;

let run_paused_participant_to_completion store state session_id paused runtime_response =
  let participant_name = paused.detail.participant_name in
  Eio.Switch.run
  @@ fun sw ->
  Agent.provide_input
    paused.agent
    paused.input_required
    (Agent_elicitation.runtime_response_to_hooks runtime_response);
  let emit_delta_text text =
    emit_delta_text_with_refs
      store
      state
      session_id
      participant_name
      ~delta_warn_logged:paused.delta_warn_logged
      ~delta_error_count:paused.delta_error_count
      text
  in
  let on_event = function
    | Types.ContentBlockDelta { delta = Types.TextDelta text; _ } -> emit_delta_text text
    | _other_event -> ()
  in
  let rec loop () =
    let agent_state = Agent.state paused.agent in
    if agent_state.turn_count >= agent_state.config.max_turns
    then
      Error
        { error =
            Error.Agent
              (Error.MaxTurnsExceeded
                 { turns = agent_state.turn_count; limit = agent_state.config.max_turns })
        ; raw_trace_run_id = latest_raw_trace_run_id paused.trace_sink
        }
    else (
      match Agent.run_turn_stream ~sw ~on_event paused.agent with
      | Ok (`Complete response) ->
        Ok
          (Participant_completed
             { summary = extract_text response
             ; raw_trace_run_id = latest_raw_trace_run_id paused.trace_sink
             ; stop_reason = Some (Types.show_stop_reason response.stop_reason)
             ; completion_anomaly =
                 completion_anomaly_of_delta_errors paused.delta_error_count
             })
      | Ok `ToolsExecuted -> loop ()
      | Error (Error.Agent (Error.InputRequired request)) ->
        Ok
          (Participant_input_required
             ( Agent_elicitation.runtime_input_request_of_input_required request
             , { paused with input_required = request } ))
      | Error err ->
        Error
          { error = err; raw_trace_run_id = latest_raw_trace_run_id paused.trace_sink })
  in
  loop ()
;;

let resume_paused_participant store state session_id paused runtime_response =
  let participant_name = paused.detail.participant_name in
  match
    run_paused_participant_to_completion store state session_id paused runtime_response
  with
  | Ok (Participant_completed outcome) ->
    persist_participant_completion
      store
      state
      ~session_id
      ~participant_name
      ~resolution:paused.resolution
      outcome
  | Ok (Participant_input_required (request, paused)) ->
    persist_participant_input_required
      store
      state
      ~session_id
      ~participant_name
      ~resolution:paused.resolution
      request
      paused
  | Error failure ->
    persist_participant_failure
      store
      state
      ~session_id
      ~participant_name
      ~provider:paused.resolution.resolved_provider
      ~model:paused.resolution.resolved_model
      ~detail:(Error.to_string failure.error)
      ?raw_trace_run_id:failure.raw_trace_run_id
      ~failure_cause:(Execution_error (Error.to_string failure.error))
      ()
;;

let start_session state (request : start_request) =
  let* store = store_of_state state in
  let session = Runtime_projection.initial_session request in
  let* _ =
    invoke_hook state ~hook_name:"SessionStart" ~payload:(start_request_to_yojson request)
  in
  let* () = with_store_lock state (fun () -> Runtime_store.save_session store session) in
  let* projected, _ =
    persist_event
      store
      state
      session.session_id
      (Session_started { goal = request.goal; participants = request.participants })
  in
  Ok (Session_started_response projected)
;;

let finalize_session state store (session : session) reason =
  let session_id = session.session_id in
  (match session.pending_input with
   | Some pending -> ignore (take_paused_input session_id pending.request_id)
   | None -> ());
  let* session, _ =
    match session.phase with
    | Finalizing -> Ok (session, make_event session (Finalize_requested { reason }))
    | Bootstrapping | Running | Input_required | Waiting_on_workers ->
      persist_event store state session_id (Finalize_requested { reason })
    | Completed | Failed | Cancelled ->
      Ok (session, make_event session (Session_completed { outcome = session.outcome }))
  in
  let completion_kind =
    match session.phase with
    | Failed -> Session_failed { outcome = reason }
    | Completed | Cancelled -> Session_completed { outcome = session.outcome }
    | Bootstrapping | Running | Input_required | Waiting_on_workers | Finalizing ->
      Session_completed { outcome = first_some reason session.outcome }
  in
  let* _final_session, _ = persist_event store state session_id completion_kind in
  let* final_session, _report, _proof =
    generate_report_and_proof store state session_id
  in
  Ok (Finalized final_session)
;;

let apply_command ~sw state store (session : session) command =
  let session_id = session.session_id in
  match command with
  | Record_turn detail ->
    let* session, _ =
      persist_event
        store
        state
        session_id
        (Turn_recorded { actor = detail.actor; message = detail.message })
    in
    Ok (Command_applied session)
  | Request_input detail ->
    if String.trim detail.request_id = ""
    then Error (Error.Internal "input request_id must be non-empty")
    else
      let* session, _ = persist_event store state session_id (Input_required detail) in
      Ok (Command_applied session)
  | Provide_input detail ->
    let paused_to_resume = ref None in
    let applied =
      with_store_lock state (fun () ->
        let* session = Runtime_store.load_session store session_id in
        match session.pending_input with
        | None ->
          Error
            (Error.Internal
               (Printf.sprintf
                  "cannot provide input %s: no pending input request"
                  detail.request_id))
        | Some pending when not (String.equal pending.request_id detail.request_id) ->
          Error
            (Error.Internal
               (Printf.sprintf
                  "cannot provide input %s: pending request is %s"
                  detail.request_id
                  pending.request_id))
        | Some pending ->
          let* session, _ =
            persist_event_locked
              store
              state
              session
              (Input_provided
                 { request_id = detail.request_id
                 ; participant_name = pending.participant_name
                 ; response = detail.response
                 })
          in
          paused_to_resume := take_paused_input session_id detail.request_id;
          Ok (Command_applied session))
    in
    (match applied, !paused_to_resume with
     | Ok _, Some paused ->
       Eio.Fiber.fork ~sw (fun () ->
         resume_paused_participant store state session_id paused detail.response)
     | _ -> ());
    applied
  | Update_session_settings detail ->
    let* session, _ =
      persist_event store state session_id (Session_settings_updated detail)
    in
    Ok (Command_applied session)
  | Spawn_agent detail ->
    let* permission =
      ask_permission
        state
        ~action:"spawn_agent"
        ~subject:detail.participant_name
        ~payload:(spawn_agent_request_to_yojson detail)
    in
    let permission_allowed, permission_message =
      match permission with
      | Permission_response result -> result.allow, result.message
      | Hook_response _ -> true, None
    in
    let* session, _ =
      persist_event
        store
        state
        session_id
        (Agent_spawn_requested
           { participant_name = detail.participant_name
           ; role = detail.role
           ; prompt = detail.prompt
           ; provider = detail.provider
           ; model = detail.model
           ; permission_mode = session.permission_mode
           })
    in
    let* hook_response =
      invoke_hook
        state
        ~hook_name:"PreSpawn"
        ~payload:(spawn_agent_request_to_yojson detail)
    in
    let hook_allowed, hook_message =
      match hook_response with
      | Hook_response result -> result.continue_, result.message
      | Permission_response _ -> true, None
    in
    (match resolve_execution session detail with
     | Error err ->
       let* session, _ =
         persist_event
           store
           state
           session_id
           (Agent_failed
              { participant_name = detail.participant_name
              ; summary = None
              ; provider = detail.provider
              ; model = detail.model
              ; error = Some (Error.to_string err)
              ; raw_trace_run_id = None
              ; stop_reason = None
              ; completion_anomaly = None
              ; failure_cause = Some (Execution_error (Error.to_string err))
              })
       in
       Ok (Command_applied session)
     | Ok resolution ->
       if (not permission_allowed) || not hook_allowed
       then
         let* session, _ =
           persist_event
             store
             state
             session_id
             (Agent_failed
                { participant_name = detail.participant_name
                ; summary = None
                ; provider = resolution.resolved_provider
                ; model = resolution.resolved_model
                ; error =
                    Some
                      (Option.value
                         ~default:"spawn blocked by control policy"
                         (first_some permission_message hook_message))
                ; raw_trace_run_id = None
                ; stop_reason = None
                ; completion_anomaly = None
                ; failure_cause =
                    Some
                      (Execution_error
                         (Option.value
                            ~default:"spawn blocked by control policy"
                            (first_some permission_message hook_message)))
                })
         in
         Ok (Command_applied session)
       else (
         let participant_name = detail.participant_name in
         Eio.Fiber.fork ~sw (fun () ->
           try
             match
               persist_event
                 store
                 state
                 session_id
                 (Agent_became_live
                    { participant_name
                    ; summary = Some "runtime-started"
                    ; provider = resolution.resolved_provider
                    ; model = resolution.resolved_model
                    ; error = None
                    ; raw_trace_run_id = None
                    ; stop_reason = None
                    ; completion_anomaly = None
                    ; failure_cause = None
                    })
             with
             | Error err ->
               let detail =
                 Printf.sprintf
                   "failed to persist runtime-started event: %s"
                   (Error.to_string err)
               in
               log_participant_persist_failure
                 ~session_id
                 ~participant_name
                 ~phase:"agent_became_live"
                 err;
               persist_participant_failure
                 store
                 state
                 ~session_id
                 ~participant_name
                 ~provider:resolution.resolved_provider
                 ~model:resolution.resolved_model
                 ~detail
                 ~failure_cause:
                   (Persistence_failure { phase = "agent_became_live"; detail })
                 ()
             | Ok _ ->
               (match run_participant store state session_id resolution detail with
                | Ok (Participant_completed outcome) ->
                  persist_participant_completion
                    store
                    state
                    ~session_id
                    ~participant_name
                    ~resolution
                    outcome
                | Ok (Participant_input_required (request, paused)) ->
                  persist_participant_input_required
                    store
                    state
                    ~session_id
                    ~participant_name
                    ~resolution
                    request
                    paused
                | Error failure ->
                  persist_participant_failure
                    store
                    state
                    ~session_id
                    ~participant_name
                    ~provider:resolution.resolved_provider
                    ~model:resolution.resolved_model
                    ~detail:(Error.to_string failure.error)
                    ?raw_trace_run_id:failure.raw_trace_run_id
                    ~failure_cause:(Execution_error (Error.to_string failure.error))
                    ())
           with
           | Eio.Cancel.Cancelled _ as ex -> raise ex
           | exn ->
             persist_participant_failure
               store
               state
               ~session_id
               ~participant_name
               ~provider:resolution.resolved_provider
               ~model:resolution.resolved_model
               ~detail:
                 (Printf.sprintf "participant fiber crashed: %s" (Printexc.to_string exn))
               ~failure_cause:
                 (Execution_error
                    (Printf.sprintf
                       "participant fiber crashed: %s"
                       (Printexc.to_string exn)))
               ());
         Ok (Command_applied session)))
  | Attach_artifact detail ->
    let* artifact =
      Artifact_service.save_text_internal
        store
        ~session_id:session.session_id
        ~name:detail.name
        ~kind:detail.kind
        ~content:detail.content
    in
    let* session, _ =
      persist_event
        store
        state
        session_id
        (Artifact_attached
           { artifact_id = artifact.artifact_id
           ; name = artifact.name
           ; kind = artifact.kind
           ; mime_type = artifact.mime_type
           ; path = Option.value ~default:"" artifact.path
           ; size_bytes = artifact.size_bytes
           })
    in
    Ok (Command_applied session)
  | Checkpoint detail ->
    with_store_lock state (fun () ->
      let* session = Runtime_store.load_session store session_id in
      let seq = session.last_seq + 1 in
      let path =
        Runtime_store.snapshot_path store session.session_id ~seq ~label:detail.label
      in
      let event =
        { seq
        ; ts = Unix.gettimeofday ()
        ; kind = Checkpoint_saved { label = detail.label; path }
        }
      in
      let* projected = Runtime_projection.apply_event session event in
      let* _path = Runtime_store.save_snapshot store projected ~label:detail.label in
      let* () = Runtime_store.append_event store session.session_id event in
      let* () = Runtime_store.save_session store projected in
      let () = emit_event state session.session_id event in
      Ok (Command_applied projected))
  | Request_finalize detail -> finalize_session state store session detail.reason
;;

let handle_request ~sw state request =
  match request with
  | Initialize detail ->
    state.session_root <- session_root_request_path detail.session_root;
    let* _store = store_of_state state in
    Ok
      (Initialized
         { sdk_name = "agent_sdk"
         ; sdk_version = Sdk_version.version
         ; runtime_version
         ; protocol_version = Runtime.protocol_version
         ; capabilities =
             [ "initialize"
             ; "start_session"
             ; "apply_command"
             ; "input_required"
             ; "status"
             ; "events"
             ; "finalize"
             ; "report"
             ; "prove"
             ]
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
      invoke_hook
        state
        ~hook_name:"Stop"
        ~payload:
          (`Assoc
              [ "session_id", `String session_id
              ; ( "reason"
                , match reason with
                  | Some value -> `String value
                  | None -> `Null )
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
;;

let serve_stdio ~sw ~net () =
  let state = create ~net () in
  let rec loop () =
    match input_line stdin with
    | raw when String.trim raw = "" -> loop ()
    | exception End_of_file -> ()
    | raw ->
      (match protocol_message_of_string raw with
       | Ok (Request_message payload) ->
         let response =
           match handle_request ~sw state payload.request with
           | Ok response -> response
           | Error err -> Error_response (Error.to_string err)
         in
         write_protocol_message
           state
           (Response_message { request_id = payload.request_id; response });
         (match response with
          | Shutdown_ack -> ()
          | _continue_response -> loop ())
       | Ok _non_request_message -> loop ()
       | Error _ ->
         (match request_of_string raw with
          | Error detail ->
            write_protocol_message
              state
              (Response_message
                 { request_id = "legacy"; response = Error_response detail });
            loop ()
          | Ok request ->
            let response =
              match handle_request ~sw state request with
              | Ok response -> response
              | Error err -> Error_response (Error.to_string err)
            in
            write_protocol_message
              state
              (Response_message { request_id = "legacy"; response });
            (match response with
             | Shutdown_ack -> ()
             | _continue_response -> loop ())))
  in
  loop ()
;;

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
;;

let%test "first_some: None, Some b -> Some b" = first_some None (Some "b") = Some "b"
let%test "first_some: Some a, None -> Some a" = first_some (Some "a") None = Some "a"
let%test "first_some: None, None -> None" = first_some None None = None

(* --- session_root_request_path --- *)

let%test "session_root_request_path: None -> None" = session_root_request_path None = None

let%test "session_root_request_path: Some empty -> None" =
  session_root_request_path (Some "") = None
;;

let%test "session_root_request_path: Some whitespace -> None" =
  session_root_request_path (Some "   ") = None
;;

let%test "session_root_request_path: Some valid -> Some trimmed" =
  session_root_request_path (Some "/tmp/sessions") = Some "/tmp/sessions"
;;

let%test "session_root_request_path: Some with spaces -> Some trimmed" =
  session_root_request_path (Some "  /tmp/sessions  ") = Some "/tmp/sessions"
;;

(* --- Runtime.protocol_version --- *)

let%test "protocol_version is not empty" = String.length Runtime.protocol_version > 0

(* --- Runtime wire protocol: request serialization roundtrip --- *)

let%test "request roundtrip: Shutdown" =
  let json_str = request_to_string Shutdown in
  match request_of_string json_str with
  | Ok Shutdown -> true
  | _unexpected_request -> false
;;

let%test "request roundtrip: Initialize" =
  let req =
    Initialize
      { session_root = Some "/tmp"
      ; provider = Some "mock"
      ; model = None
      ; permission_mode = None
      ; include_partial_messages = false
      ; setting_sources = []
      ; resume_session = None
      ; cwd = None
      }
  in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Initialize r) -> r.session_root = Some "/tmp" && r.provider = Some "mock"
  | _unexpected_request -> false
;;

let%test "request roundtrip: Start_session" =
  let req =
    Start_session
      { session_id = None
      ; goal = "test goal"
      ; participants = [ "alice"; "bob" ]
      ; provider = Some "mock"
      ; model = None
      ; permission_mode = None
      ; system_prompt = None
      ; max_turns = Some 5
      ; workdir = None
      }
  in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Start_session r) -> r.goal = "test goal" && r.participants = [ "alice"; "bob" ]
  | _unexpected_request -> false
;;

let%test "request roundtrip: Status" =
  let req = Status { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Status { session_id }) -> session_id = "s123"
  | _unexpected_request -> false
;;

let%test "request roundtrip: Events" =
  let req = Events { session_id = "s123"; after_seq = Some 5 } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Events { session_id; after_seq }) -> session_id = "s123" && after_seq = Some 5
  | _unexpected_request -> false
;;

let%test "request roundtrip: Finalize" =
  let req = Finalize { session_id = "s123"; reason = Some "done" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Finalize { session_id; reason }) -> session_id = "s123" && reason = Some "done"
  | _unexpected_request -> false
;;

let%test "request roundtrip: Report" =
  let req = Report { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Report { session_id }) -> session_id = "s123"
  | _unexpected_request -> false
;;

let%test "request roundtrip: Prove" =
  let req = Prove { session_id = "s123" } in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Prove { session_id }) -> session_id = "s123"
  | _unexpected_request -> false
;;

let%test "request roundtrip: Apply_command with Record_turn" =
  let req =
    Apply_command
      { session_id = "s123"
      ; command = Record_turn { actor = Some "alice"; message = "hello" }
      }
  in
  let json_str = request_to_string req in
  match request_of_string json_str with
  | Ok (Apply_command { session_id; command = Record_turn { actor; message } }) ->
    session_id = "s123" && actor = Some "alice" && message = "hello"
  | _unexpected_request -> false
;;

(* --- response serialization roundtrip --- *)

let%test "response roundtrip: Shutdown_ack" =
  let json_str = response_to_string Shutdown_ack in
  match response_of_string json_str with
  | Ok Shutdown_ack -> true
  | _unexpected_response -> false
;;

let%test "response roundtrip: Error_response" =
  let json_str = response_to_string (Error_response "something failed") in
  match response_of_string json_str with
  | Ok (Error_response msg) -> msg = "something failed"
  | _unexpected_response -> false
;;

let%test "response roundtrip: Initialized" =
  let resp =
    Initialized
      { sdk_name = "agent_sdk"
      ; sdk_version = "1.0.0"
      ; runtime_version = "1.0.0"
      ; protocol_version = "oas-runtime-0.1"
      ; capabilities = [ "initialize"; "shutdown" ]
      }
  in
  let json_str = response_to_string resp in
  match response_of_string json_str with
  | Ok (Initialized r) ->
    r.sdk_name = "agent_sdk"
    && r.protocol_version = "oas-runtime-0.1"
    && List.length r.capabilities = 2
  | _unexpected_response -> false
;;

(* --- protocol_message serialization --- *)

let%test "protocol_message roundtrip: Request_message" =
  let msg = Request_message { request_id = "req-1"; request = Shutdown } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Request_message { request_id; request = Shutdown }) -> request_id = "req-1"
  | _unexpected_message -> false
;;

let%test "protocol_message roundtrip: Response_message" =
  let msg = Response_message { request_id = "req-2"; response = Shutdown_ack } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Response_message { request_id; response = Shutdown_ack }) -> request_id = "req-2"
  | _unexpected_message -> false
;;

let%test "protocol_message roundtrip: Event_message" =
  let msg =
    Event_message
      { session_id = Some "s1"
      ; event =
          { seq = 1
          ; ts = 100.0
          ; kind = Session_started { goal = "test"; participants = [] }
          }
      }
  in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Event_message { session_id = Some "s1"; event }) -> event.seq = 1
  | _unexpected_message -> false
;;

let%test "protocol_message roundtrip: Control_request_message" =
  let msg =
    Control_request_message
      { control_id = "ctrl-000001"
      ; request = Permission_request { action = "spawn"; subject = "a1"; payload = `Null }
      }
  in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Control_request_message { control_id; _ }) -> control_id = "ctrl-000001"
  | _unexpected_message -> false
;;

let%test "protocol_message roundtrip: Control_response_message" =
  let msg =
    Control_response_message
      { control_id = "ctrl-000002"
      ; response = Permission_response { allow = true; message = None; interrupt = false }
      }
  in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (Control_response_message { control_id; response = Permission_response r }) ->
    control_id = "ctrl-000002" && r.allow = true
  | _unexpected_message -> false
;;

let%test "protocol_message roundtrip: System_message" =
  let msg = System_message { level = "info"; message = "hello" } in
  let json_str = protocol_message_to_string msg in
  match protocol_message_of_string json_str with
  | Ok (System_message { level; message }) -> level = "info" && message = "hello"
  | _unexpected_message -> false
;;

(* --- request_of_string error handling --- *)

let%test "request_of_string: invalid JSON returns Error" =
  match request_of_string "not valid json {{{" with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "protocol_message_of_string: invalid JSON returns Error" =
  match protocol_message_of_string "garbage" with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "response_of_string: invalid JSON returns Error" =
  match response_of_string "not json" with
  | Error _ -> true
  | Ok _ -> false
;;

(* --- event_kind serialization roundtrip via event --- *)

let%test "event roundtrip: Turn_recorded" =
  let event =
    { seq = 3; ts = 200.0; kind = Turn_recorded { actor = Some "alice"; message = "hi" } }
  in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e ->
    e.seq = 3
    &&
      (match e.kind with
      | Turn_recorded { actor = Some "alice"; message = "hi" } -> true
      | _unexpected_event_kind -> false)
  | Error _ -> false
;;

let%test "event roundtrip: Artifact_attached" =
  let event =
    { seq = 4
    ; ts = 300.0
    ; kind =
        Artifact_attached
          { artifact_id = "art-1"
          ; name = "report"
          ; kind = "json"
          ; mime_type = "application/json"
          ; path = "/tmp/report.json"
          ; size_bytes = 1234
          }
    }
  in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e ->
    (match e.kind with
     | Artifact_attached { artifact_id = "art-1"; size_bytes = 1234; _ } -> true
     | _unexpected_event_kind -> false)
  | Error _ -> false
;;

let%test "event roundtrip: Checkpoint_saved" =
  let event =
    { seq = 6
    ; ts = 500.0
    ; kind = Checkpoint_saved { label = Some "mid"; path = "/tmp/snap" }
    }
  in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e ->
    (match e.kind with
     | Checkpoint_saved { label = Some "mid"; _ } -> true
     | _unexpected_event_kind -> false)
  | Error _ -> false
;;

let%test "event roundtrip: Session_completed" =
  let event =
    { seq = 7; ts = 600.0; kind = Session_completed { outcome = Some "success" } }
  in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e ->
    (match e.kind with
     | Session_completed { outcome = Some "success" } -> true
     | _unexpected_event_kind -> false)
  | Error _ -> false
;;

let%test "event roundtrip: Session_failed" =
  let event =
    { seq = 8; ts = 700.0; kind = Session_failed { outcome = Some "timeout" } }
  in
  let json = event_to_yojson event in
  match event_of_yojson json with
  | Ok e ->
    (match e.kind with
     | Session_failed { outcome = Some "timeout" } -> true
     | _unexpected_event_kind -> false)
  | Error _ -> false
;;
