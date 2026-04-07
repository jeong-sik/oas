open Runtime
open Runtime_server_types
open Runtime_server_resolve

let ( let* ) = Result.bind

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
  let delta_warn_logged = ref false in
  let emit_delta_text text =
    match emit_output_delta store state session_id detail.participant_name text with
    | Ok () -> ()
    | Error e ->
      if not !delta_warn_logged then begin
        delta_warn_logged := true;
        let _log = Log.create ~module_name:"runtime_server_worker" () in
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
           | Error e ->
               let _log = Log.create ~module_name:"runtime_server_worker" () in
               Log.warn _log "trace start_run failed for mock provider"
                 [Log.S ("session_id", session_id);
                  Log.S ("agent", detail.participant_name);
                  Log.S ("error", Error.to_string e)])
       | None -> ());
      let half = String.length full / 2 in
      emit_delta_text (String.sub full 0 half);
      emit_delta_text (String.sub full half (String.length full - half));
      Ok full
  | _ ->
      Eio.Switch.run @@ fun sw ->
      match Runtime_store.load_session store session_id with
      | Error err -> Error err
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
      | Ok response -> Ok (extract_text response)
      | Error err -> Error err

[@@@coverage off]
(* === Inline tests === *)

(* --- extract_text --- *)

let%test "extract_text: empty content" =
  let resp = { Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
               content = []; usage = None } in
  extract_text resp = ""

let%test "extract_text: single Text block" =
  let resp = { Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
               content = [Types.Text "hello"]; usage = None } in
  extract_text resp = "hello"

let%test "extract_text: multiple Text blocks joined by newline" =
  let resp = { Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
               content = [Types.Text "hello"; Types.Text "world"]; usage = None } in
  extract_text resp = "hello\nworld"

let%test "extract_text: non-Text blocks filtered out" =
  let resp = { Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
               content = [
                 Types.Text "before";
                 Types.ToolUse { id = "t1"; name = "fn"; input = `Null };
                 Types.Text "after";
               ]; usage = None } in
  extract_text resp = "before\nafter"

let%test "extract_text: only non-Text blocks returns empty" =
  let resp = { Types.id = "r1"; model = "m"; stop_reason = Types.EndTurn;
               content = [
                 Types.ToolUse { id = "t1"; name = "fn"; input = `Null };
                 Types.Thinking { thinking_type = "thinking"; content = "hmm" };
               ]; usage = None } in
  extract_text resp = ""

(* --- make_event --- *)

let dummy_session : Runtime.session = {
  session_id = "s1";
  goal = "test";
  title = None;
  tag = None;
  permission_mode = None;
  phase = Runtime.Running;
  created_at = 0.0;
  updated_at = 0.0;
  provider = None;
  model = None;
  system_prompt = None;
  max_turns = 10;
  workdir = None;
  planned_participants = [];
  participants = [];
  artifacts = [];
  votes = [];
  turn_count = 0;
  last_seq = 5;
  outcome = None;
}

let%test "make_event: seq is last_seq + 1" =
  let event = make_event dummy_session
    (Session_started { goal = "test"; participants = [] }) in
  event.seq = 6

let%test "make_event: ts is positive" =
  let event = make_event dummy_session
    (Session_started { goal = "test"; participants = [] }) in
  event.ts > 0.0

let%test "make_event: kind is preserved" =
  let event = make_event dummy_session
    (Turn_recorded { actor = Some "alice"; message = "hi" }) in
  match event.kind with
  | Turn_recorded { actor = Some "alice"; message = "hi" } -> true
  | _ -> false
