open Runtime
open Runtime_server_types
open Runtime_server_resolve

let ( let* ) = Result.bind
let _log = Log.create ~module_name:"runtime_server_worker" ()

let unsupported_test_provider provider =
  Error.Config
    (Error.UnsupportedProvider
       {
         detail =
           Printf.sprintf
             "provider %S is test-only; set OAS_ALLOW_TEST_PROVIDERS=1 to enable it explicitly"
             provider;
       })

let extract_text (resp : Types.api_response) =
  resp.content
  |> List.filter_map (function Types.Text s -> Some s | _ -> None)
  |> String.concat "\n"

type participant_run_success = {
  summary: string;
  raw_trace_run_id: string option;
  stop_reason: string option;
  completion_anomaly: Runtime.completion_anomaly option;
}

type participant_run_failure = {
  error: Error.sdk_error;
  raw_trace_run_id: string option;
}

let latest_raw_trace_run_id = function
  | Some sink ->
      Option.map (fun (run : Raw_trace.run_ref) -> run.worker_run_id)
        (Raw_trace.last_run sink)
  | None -> None

let make_event (session : session) kind =
  {
    seq = session.last_seq + 1;
    ts = Unix.gettimeofday ();
    kind;
  }

let with_store_lock state f =
  Eio.Mutex.use_rw ~protect:true state.store_mu f

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

let persist_artifact_events_locked store state session
    (artifacts : Runtime.artifact list) =
  List.fold_left
    (fun acc artifact ->
      let* session = acc in
      let* session, _ =
        persist_event_locked store state session
          (Runtime_evidence.artifact_attached_event artifact)
      in
      Ok session)
    (Ok session) artifacts

let build_raw_trace_manifest (store : Runtime_store.t) session_id =
  let session_root = Some store.root in
  let* latest_raw_trace_run =
    Sessions.get_latest_raw_trace_run ?session_root ~session_id ()
  in
  let* raw_trace_runs =
    Sessions.get_raw_trace_runs ?session_root ~session_id ()
  in
  let* raw_trace_summaries =
    Sessions.get_raw_trace_summaries ?session_root ~session_id ()
  in
  let* raw_trace_validations =
    Sessions.get_raw_trace_validations ?session_root ~session_id ()
  in
  Ok
    (Runtime_evidence.build_raw_trace_manifest ~session_id ~latest_raw_trace_run
       ~raw_trace_runs ~raw_trace_summaries ~raw_trace_validations)

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
      let* telemetry_json_path =
        Artifact_service.persisted_path telemetry_json_artifact
      in
      let* telemetry_md_path =
        Artifact_service.persisted_path telemetry_md_artifact
      in
      let* raw_trace_manifest =
        build_raw_trace_manifest store session_id
      in
      let raw_trace_json =
        Runtime_evidence.raw_trace_manifest_to_json raw_trace_manifest
        |> Yojson.Safe.pretty_to_string
      in
      let* raw_trace_artifact =
        Artifact_service.save_text_internal store ~session_id
          ~name:"runtime-raw-trace-json" ~kind:"json"
          ~content:raw_trace_json
      in
      let* raw_trace_json_path =
        Artifact_service.persisted_path raw_trace_artifact
      in
      let evidence =
        Runtime_evidence.build_evidence_bundle ~session_id
          (Runtime_evidence.base_evidence_file_specs store session_id
           @
           [
             ("telemetry_json", telemetry_json_path);
             ("telemetry_md", telemetry_md_path);
             ("raw_trace_json", raw_trace_json_path);
           ])
      in
      let evidence_json =
        Runtime_evidence.evidence_bundle_to_json evidence
        |> Yojson.Safe.pretty_to_string
      in
      let* evidence_artifact =
        Artifact_service.save_text_internal store ~session_id
          ~name:"runtime-evidence" ~kind:"json"
          ~content:evidence_json
      in
      let artifacts =
        [
          telemetry_json_artifact;
          telemetry_md_artifact;
          raw_trace_artifact;
          evidence_artifact;
        ]
      in
      let* final_session =
        persist_artifact_events_locked store state session artifacts
      in
      let* final_events = Runtime_store.read_events store session_id () in
      let final_report =
        Runtime_projection.build_report final_session final_events
      in
      let final_proof =
        Runtime_projection.build_proof final_session final_events
      in
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
        Artifact_service.overwrite_text_internal telemetry_json_artifact
          ~content:final_telemetry_json
      in
      let* () =
        Artifact_service.overwrite_text_internal telemetry_md_artifact
          ~content:final_telemetry_md
      in
      let final_evidence =
        Runtime_evidence.build_evidence_bundle ~session_id
          (Runtime_evidence.base_evidence_file_specs store session_id
           @
           [
             ("telemetry_json", telemetry_json_path);
             ("telemetry_md", telemetry_md_path);
             ("raw_trace_json", raw_trace_json_path);
           ])
      in
      let final_evidence_json =
        Runtime_evidence.evidence_bundle_to_json final_evidence
        |> Yojson.Safe.pretty_to_string
      in
      let* () =
        Artifact_service.overwrite_text_internal evidence_artifact
          ~content:final_evidence_json
      in
      Ok (final_session, final_report, final_proof))

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
  let delta_warn_logged = ref false in
  let delta_error_count = ref 0 in
  let emit_delta_text text =
    match emit_output_delta store state session_id detail.participant_name text with
    | Ok () -> ()
    | Error e ->
      incr delta_error_count;
      if not !delta_warn_logged then begin
        delta_warn_logged := true;

        Log.warn _log "output delta emission failed"
          [Log.S ("session_id", session_id);
           Log.S ("participant", detail.participant_name);
           Log.S ("error", Error.to_string e)]
      end
  in
  let trace_sink =
    match
      Raw_trace.create_for_session ~session_root:store.root
        ~session_id ~agent_name:detail.participant_name ()
    with
    | Ok trace -> Some trace
    | Error e ->
      let _log = Log.create ~module_name:"runtime_server_worker" () in
      Log.warn _log "trace sink creation failed"
        [Log.S ("session_id", session_id);
         Log.S ("agent", detail.participant_name);
         Log.S ("error", Error.to_string e)];
      None
  in
  let completion_anomaly () =
    if !delta_error_count > 0 then
      Some (Runtime.Dropped_output_deltas { count = !delta_error_count })
    else None
  in
  match resolution.selected_provider with
  | "mock" | "echo" ->
      if not (Defaults.allow_test_providers ()) then
        Error
          {
            error = unsupported_test_provider resolution.selected_provider;
            raw_trace_run_id = latest_raw_trace_run_id trace_sink;
          }
      else
      let full =
        Printf.sprintf "Mock runtime response for %s: %s" detail.participant_name
          detail.prompt
      in
      (match trace_sink with
       | Some sink -> (
           match
             Raw_trace.start_run sink ~agent_name:detail.participant_name
               ~prompt:detail.prompt
               ?model:resolution.resolved_model
               ()
           with
           | Ok active ->
               ignore
                 (Raw_trace.record_assistant_block active ~block_index:0
                    (Types.Text full));
               ignore
                 (Raw_trace.finish_run active ~final_text:(Some full)
                    ~stop_reason:(Some "EndTurn") ~error:None)
           | Error e ->
       
               Log.warn _log "trace start_run failed for mock provider"
                 [Log.S ("session_id", session_id);
                  Log.S ("agent", detail.participant_name);
                  Log.S ("error", Error.to_string e)])
       | None -> ());
      let half = String.length full / 2 in
      emit_delta_text (String.sub full 0 half);
      emit_delta_text (String.sub full half (String.length full - half));
      if !delta_error_count > 0 then
        Log.warn _log "participant completed with dropped output deltas"
          [ Log.S ("session_id", session_id);
            Log.S ("participant", detail.participant_name);
            Log.I ("dropped_output_deltas", !delta_error_count) ];
      Ok
        {
          summary = full;
          raw_trace_run_id = latest_raw_trace_run_id trace_sink;
          stop_reason = Some "EndTurn";
          completion_anomaly = completion_anomaly ();
        }
  | _ ->
      Eio.Switch.run @@ fun sw ->
      match Runtime_store.load_session store session_id with
      | Error err ->
          Error
            {
              error = err;
              raw_trace_run_id = latest_raw_trace_run_id trace_sink;
            }
      | Ok session ->
      let config =
        {
          Types.default_config with
          name = detail.participant_name;
          model =
            (match detail.model with
             | Some value when String.trim value <> "" -> Model_registry.resolve_model_id value
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
      | Ok response ->
          if !delta_error_count > 0 then
            Log.warn _log "participant completed with dropped output deltas"
              [ Log.S ("session_id", session_id);
                Log.S ("participant", detail.participant_name);
                Log.I ("dropped_output_deltas", !delta_error_count) ];
          Ok
            {
              summary = extract_text response;
              raw_trace_run_id = latest_raw_trace_run_id trace_sink;
              stop_reason = Some (Types.show_stop_reason response.stop_reason);
              completion_anomaly = completion_anomaly ();
            }
      | Error err ->
          Error
            {
              error = err;
              raw_trace_run_id = latest_raw_trace_run_id trace_sink;
            }
