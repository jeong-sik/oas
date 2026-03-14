open Runtime

let ( let* ) = Result.bind

type options = {
  session_root: string option;
  session_id: string;
  goal: string;
  title: string option;
  tag: string option;
  role: string option;
  aliases: string list;
  requested_provider: string option;
  requested_model: string option;
  requested_policy: string option;
  workdir: string option;
}

let now () = Unix.gettimeofday ()

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let validation_error detail =
  Error.Io (ValidationFailed { detail })

let safe_name name =
  let trimmed = String.trim name in
  let base = if trimmed = "" then "agent" else trimmed in
  String.map
    (function
      | '/' | '\\' | ' ' | '\t' | '\n' | '\r' -> '_'
      | c -> c)
    base

let save_events store session_id (events : event list) =
  let content =
    events
    |> List.map (fun event -> event |> event_to_yojson |> Yojson.Safe.to_string)
    |> String.concat "\n"
    |> fun body -> if body = "" then "" else body ^ "\n"
  in
  Runtime_store.save_text (Runtime_store.events_path store session_id) content

let lifecycle_snapshot_or_default agent =
  match Agent.lifecycle_snapshot agent with
  | Some snapshot -> snapshot
  | None ->
      {
        Agent.current_run_id = None;
        agent_name = agent.state.config.name;
        status = Completed;
        requested_provider = None;
        requested_model = Some (Types.model_to_string agent.state.config.model);
        resolved_provider = None;
        resolved_model = Some (Types.model_to_string agent.state.config.model);
        last_error = None;
        started_at = None;
        last_progress_at = None;
        finished_at = None;
      }

let extract_prompt (records : Raw_trace.record list) =
  records
  |> List.find_map (fun (record : Raw_trace.record) ->
         match record.record_type, record.prompt with
         | Raw_trace.Run_started, Some prompt -> Some prompt
         | _ -> None)
  |> Option.value ~default:""

let extract_text_deltas (records : Raw_trace.record list) =
  records
  |> List.filter_map (fun (record : Raw_trace.record) ->
         match record.record_type, record.block_kind, record.assistant_block with
         | Raw_trace.Assistant_block, Some "text", Some (`Assoc fields) -> (
             match List.assoc_opt "text" fields with
             | Some (`String text) when String.trim text <> "" -> Some (record.ts, text)
             | _ -> None)
         | _ -> None)

let write_latest_run_to_session store session_id agent_name (run_ref : Raw_trace.run_ref) =
  let* records = Raw_trace.read_run run_ref in
  let path =
    Filename.concat (Runtime_store.raw_traces_dir store session_id)
      (safe_name agent_name ^ ".jsonl")
  in
  let content =
    records
    |> List.map (fun record ->
           record |> Raw_trace.record_to_json |> Yojson.Safe.to_string)
    |> String.concat "\n"
    |> fun body -> if body = "" then "" else body ^ "\n"
  in
  let* () = Runtime_store.save_text path content in
  let* runs = Raw_trace.read_runs ~path () in
  match
    List.find_opt
      (fun (candidate : Raw_trace.run_ref) ->
        String.equal candidate.worker_run_id run_ref.worker_run_id)
      runs
  with
  | Some run -> Ok (run, records)
  | None ->
      Error
        (validation_error
           (Printf.sprintf "Failed to materialize latest raw trace run %s"
              run_ref.worker_run_id))

let apply_events initial_session (events : event list) =
  List.fold_left
    (fun acc event ->
      let* session = acc in
      Runtime_projection.apply_event session event)
    (Ok initial_session) events

let make_event seq ts kind = { seq; ts; kind }

let persist ~agent ~raw_trace ~(options : options) () =
  let snapshot = lifecycle_snapshot_or_default agent in
  let terminal =
    match snapshot.status with
    | Agent.Completed | Agent.Failed -> true
    | Agent.Accepted | Agent.Ready | Agent.Running -> false
  in
  if not terminal then
    Error
      (validation_error
         "Direct evidence materialization requires a terminal direct-agent run")
  else
    let* store = Runtime_store.create ?root:options.session_root () in
    let raw_session_id = Raw_trace.session_id raw_trace in
    let* () =
      match raw_session_id with
      | Some value when not (String.equal value options.session_id) ->
          Error
            (validation_error
               (Printf.sprintf
                  "Raw trace session_id %s did not match requested session_id %s"
                  value options.session_id))
      | _ -> Ok ()
    in
    let last_run =
      match Raw_trace.last_run raw_trace with
      | Some run -> Some run
      | None -> Agent.last_raw_trace_run agent
    in
    let* last_run =
      match last_run with
      | Some run -> Ok run
      | None -> Error (validation_error "No raw trace run was available")
    in
    let* raw_run, latest_records =
      write_latest_run_to_session store options.session_id agent.state.config.name
        last_run
    in
    let* raw_summary = Raw_trace.summarize_run raw_run in
    let* _raw_validation = Raw_trace.validate_run raw_run in
    let prompt = extract_prompt latest_records in
    let selected_provider =
      first_some snapshot.requested_provider options.requested_provider
    in
    let selected_model =
      first_some snapshot.requested_model options.requested_model
    in
    let start_request =
      {
        session_id = Some options.session_id;
        goal = options.goal;
        participants = [ agent.state.config.name ];
        provider = selected_provider;
        model = selected_model;
        permission_mode = options.requested_policy;
        system_prompt = agent.state.config.system_prompt;
        max_turns = Some agent.state.config.max_turns;
        workdir = options.workdir;
      }
    in
    let initial_session =
      let session = Runtime_projection.initial_session start_request in
      {
        session with
        title = options.title;
        tag = options.tag;
        participants =
          session.participants
          |> List.map (fun (participant : Runtime.participant) ->
                 if String.equal participant.name agent.state.config.name then
                   {
                     participant with
                     role = first_some options.role participant.role;
                     aliases = options.aliases;
                   }
                 else participant);
      }
    in
    let started_at = Option.value raw_summary.started_at ~default:(now ()) in
    let finished_at =
      Option.value raw_summary.finished_at
        ~default:(Option.value snapshot.finished_at ~default:started_at)
    in
    let deltas = extract_text_deltas latest_records in
    let base_events =
      [
        make_event 1 (started_at -. 0.003)
          (Session_started
             { goal = options.goal; participants = [ agent.state.config.name ] });
        make_event 2 (started_at -. 0.002)
          (Turn_recorded { actor = None; message = prompt });
        make_event 3 (started_at -. 0.001)
          (Agent_spawn_requested
             {
               participant_name = agent.state.config.name;
               role = first_some options.role None;
               prompt;
               provider = selected_provider;
               model = selected_model;
               permission_mode = options.requested_policy;
             });
        make_event 4 started_at
          (Agent_became_live
             {
               participant_name = agent.state.config.name;
               summary = Some "direct-agent-started";
               provider = snapshot.resolved_provider;
               model = snapshot.resolved_model;
               error = None;
             });
      ]
      @ (deltas
        |> List.mapi (fun index (ts, text) ->
               make_event (5 + index) ts
                 (Agent_output_delta
                    { participant_name = agent.state.config.name; delta = text })))
      @ [
          make_event (5 + List.length deltas) finished_at
            (match snapshot.status with
            | Agent.Failed ->
                Agent_failed
                  {
                    participant_name = agent.state.config.name;
                    summary = raw_summary.final_text;
                    provider = snapshot.resolved_provider;
                    model = snapshot.resolved_model;
                    error = first_some snapshot.last_error raw_summary.error;
                  }
            | Agent.Accepted | Agent.Ready | Agent.Running | Agent.Completed ->
                Agent_completed
                  {
                    participant_name = agent.state.config.name;
                    summary = raw_summary.final_text;
                    provider = snapshot.resolved_provider;
                    model = snapshot.resolved_model;
                    error = None;
                  });
          make_event (6 + List.length deltas) (finished_at +. 0.001)
            (Finalize_requested { reason = None });
          make_event (7 + List.length deltas) (finished_at +. 0.002)
            (match snapshot.status with
            | Agent.Failed ->
                Session_failed
                  { outcome = first_some snapshot.last_error raw_summary.error }
            | Agent.Accepted | Agent.Ready | Agent.Running | Agent.Completed ->
                Session_completed { outcome = raw_summary.stop_reason });
        ]
    in
    let* terminal_session = apply_events initial_session base_events in
    let* () = Runtime_store.save_session store terminal_session in
    let* () = save_events store options.session_id base_events in
    let telemetry = Runtime_evidence.build_telemetry_report terminal_session base_events in
    let telemetry_json =
      Runtime_evidence.telemetry_report_to_json telemetry
      |> Yojson.Safe.pretty_to_string
    in
    let telemetry_md = Runtime_evidence.telemetry_report_to_markdown telemetry in
    let report = Runtime_projection.build_report terminal_session base_events in
    let proof = Runtime_projection.build_proof terminal_session base_events in
    let* () = Runtime_store.save_report store report in
    let* () = Runtime_store.save_proof store proof in
    let* telemetry_json_artifact =
      Artifact_service.save_text_internal store ~session_id:options.session_id
        ~name:"runtime-telemetry-json" ~kind:"json" ~content:telemetry_json
    in
    let* telemetry_md_artifact =
      Artifact_service.save_text_internal store ~session_id:options.session_id
        ~name:"runtime-telemetry" ~kind:"markdown" ~content:telemetry_md
    in
    let evidence =
      Runtime_evidence.build_evidence_bundle ~session_id:options.session_id
        [
          ("session_json", Runtime_store.session_path store options.session_id);
          ("events_jsonl", Runtime_store.events_path store options.session_id);
          ("report_json", Runtime_store.report_json_path store options.session_id);
          ("report_md", Runtime_store.report_md_path store options.session_id);
          ("proof_json", Runtime_store.proof_json_path store options.session_id);
          ("proof_md", Runtime_store.proof_md_path store options.session_id);
        ]
    in
    let evidence_json =
      Runtime_evidence.evidence_bundle_to_json evidence
      |> Yojson.Safe.pretty_to_string
    in
    let* evidence_artifact =
      Artifact_service.save_text_internal store ~session_id:options.session_id
        ~name:"runtime-evidence" ~kind:"json" ~content:evidence_json
    in
    let artifact_events =
      [
        telemetry_json_artifact;
        telemetry_md_artifact;
        evidence_artifact;
      ]
      |> List.mapi (fun index (artifact : Runtime.artifact) ->
             make_event (8 + List.length deltas + index)
               (finished_at +. 0.003 +. (float_of_int index *. 0.001))
               (Artifact_attached
                  {
                    artifact_id = artifact.artifact_id;
                    name = artifact.name;
                    kind = artifact.kind;
                    mime_type = artifact.mime_type;
                    path = Option.value ~default:"" artifact.path;
                    size_bytes = artifact.size_bytes;
                  }))
    in
    let all_events = base_events @ artifact_events in
    let* final_session = apply_events initial_session all_events in
    let final_report = Runtime_projection.build_report final_session all_events in
    let final_proof = Runtime_projection.build_proof final_session all_events in
    let* () = Runtime_store.save_session store final_session in
    let* () = save_events store options.session_id all_events in
    let* () = Runtime_store.save_report store final_report in
    let* () = Runtime_store.save_proof store final_proof in
    Sessions.get_proof_bundle ?session_root:options.session_root
      ~session_id:options.session_id ()

let get_worker_run ~agent ~raw_trace ~options () =
  let* bundle = persist ~agent ~raw_trace ~options () in
  match bundle.Sessions.latest_worker_run with
  | Some worker -> Ok worker
  | None -> Error (validation_error "Direct evidence did not produce a worker run")

let run_conformance ~agent ~raw_trace ~options () =
  let* bundle = persist ~agent ~raw_trace ~options () in
  Ok (Conformance.report bundle)
