type session_info = {
  session_id: string;
  title: string option;
  tag: string option;
  goal: string;
  updated_at: float;
  phase: Runtime.phase;
  participant_count: int;
  path: string;
}

type telemetry_event_count = {
  name: string;
  count: int;
}

type telemetry_step = {
  seq: int;
  ts: float;
  kind: string;
  participant: string option;
  detail: string option;
}

type telemetry = {
  session_id: string;
  generated_at: float;
  step_count: int;
  event_counts: telemetry_event_count list;
  steps: telemetry_step list;
}

type structured_event_count = {
  event_name: string;
  count: int;
}

type structured_telemetry_step = {
  seq: int;
  ts: float;
  event_name: string;
  participant: string option;
  detail: string option;
  actor: string option;
  role: string option;
  provider: string option;
  model: string option;
  artifact_id: string option;
  artifact_name: string option;
  artifact_kind: string option;
  checkpoint_label: string option;
  outcome: string option;
}

type structured_telemetry = {
  session_id: string;
  generated_at: float;
  step_count: int;
  event_counts: structured_event_count list;
  steps: structured_telemetry_step list;
}

type evidence_file = {
  label: string;
  path: string;
  size_bytes: int;
  md5: string;
}

type missing_file = {
  label: string;
  path: string;
}

type evidence = {
  session_id: string;
  generated_at: float;
  files: evidence_file list;
  missing_files: missing_file list;
}

type raw_trace_run = Raw_trace.run_ref
type raw_trace_summary = Raw_trace.run_summary
type raw_trace_validation = Raw_trace.run_validation

type evidence_capabilities = {
  raw_trace: bool;
  validated_summary: bool;
  proof_bundle: bool;
}

type proof_bundle = {
  session: Runtime.session;
  report: Runtime.report;
  proof: Runtime.proof;
  telemetry: telemetry;
  structured_telemetry: structured_telemetry;
  evidence: evidence;
  latest_raw_trace_run: raw_trace_run option;
  raw_trace_runs: raw_trace_run list;
  raw_trace_summaries: raw_trace_summary list;
  raw_trace_validations: raw_trace_validation list;
  capabilities: evidence_capabilities;
}

let ( let* ) = Result.bind

let make_store ?session_root () =
  Runtime_store.create ?root:session_root ()

let json_parse_error detail =
  Error.Serialization (JsonParseError { detail })

let file_read_error ~path ~detail =
  Error.Io (FileOpFailed { op = "read"; path; detail })

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop index =
    if index + sub_len > text_len then false
    else if String.sub text index sub_len = sub then true
    else loop (index + 1)
  in
  if sub_len = 0 then true else loop 0

let parse_json_string raw =
  try Ok (Yojson.Safe.from_string raw)
  with Yojson.Json_error detail -> Error (json_parse_error detail)

let parse_runtime_json of_yojson raw =
  let* json = parse_json_string raw in
  match of_yojson json with
  | Ok value -> Ok value
  | Error detail -> Error (json_parse_error detail)

let decode_json_with decoder raw =
  let* json = parse_json_string raw in
  try Ok (decoder json)
  with Yojson.Safe.Util.Type_error (detail, _) -> Error (json_parse_error detail)

let latest_named_artifact artifacts name =
  List.fold_left
    (fun acc (artifact : Runtime.artifact) ->
      if not (String.equal artifact.name name) then acc
      else
        match acc with
        | None -> Some artifact
        | Some current when artifact.created_at >= current.created_at ->
            Some artifact
        | Some _ -> acc)
    None artifacts

let get_named_artifact ?session_root ~session_id ~name () =
  let* artifacts = Artifact_service.list ?session_root ~session_id () in
  match latest_named_artifact artifacts name with
  | Some artifact -> Ok artifact
  | None ->
      Error
        (file_read_error ~path:name
           ~detail:
             (Printf.sprintf "Artifact '%s' not found in session %s" name
                session_id))

let get_raw_trace_dir ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  Ok (Runtime_store.raw_traces_dir store session_id)

let get_raw_trace_files ?session_root ~session_id () =
  let* dir = get_raw_trace_dir ?session_root ~session_id () in
  if not (Sys.file_exists dir) then Ok []
  else
    dir |> Sys.readdir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".jsonl")
    |> List.sort String.compare
    |> List.map (fun name -> Filename.concat dir name)
    |> fun paths -> Ok paths

let telemetry_of_json json =
  let open Yojson.Safe.Util in
  let event_counts : telemetry_event_count list =
    json |> member "event_counts" |> to_assoc
    |> List.map (fun (name, value) -> { name; count = to_int value })
  in
  let steps : telemetry_step list =
    json |> member "steps" |> to_list
    |> List.map (fun step ->
           {
             seq = step |> member "seq" |> to_int;
             ts = step |> member "ts" |> to_float;
             kind = step |> member "kind" |> to_string;
             participant = step |> member "participant" |> to_string_option;
             detail = step |> member "detail" |> to_string_option;
           })
  in
  ({
    session_id = json |> member "session_id" |> to_string;
    generated_at = json |> member "generated_at" |> to_float;
    step_count = json |> member "step_count" |> to_int;
    event_counts;
    steps;
  } : telemetry)

let infer_event_name_from_kind kind =
  let known =
    [
      ("Session_started", "session_started");
      ("Session_settings_updated", "session_settings_updated");
      ("Turn_recorded", "turn_recorded");
      ("Agent_spawn_requested", "agent_spawn_requested");
      ("Agent_became_live", "agent_became_live");
      ("Agent_output_delta", "agent_output_delta");
      ("Agent_completed", "agent_completed");
      ("Agent_failed", "agent_failed");
      ("Artifact_attached", "artifact_attached");
      ("Vote_recorded", "vote_recorded");
      ("Checkpoint_saved", "checkpoint_saved");
      ("Finalize_requested", "finalize_requested");
      ("Session_completed", "session_completed");
      ("Session_failed", "session_failed");
    ]
  in
  known
  |> List.find_opt (fun (needle, _) -> contains_substring ~sub:needle kind)
  |> Option.map snd
  |> Option.value ~default:(String.lowercase_ascii kind)

let structured_telemetry_of_json json =
  let open Yojson.Safe.Util in
  let event_counts : structured_event_count list =
    match json |> member "event_name_counts" with
    | `Null ->
        json |> member "event_counts" |> to_assoc
        |> List.map (fun (name, value) -> { event_name = name; count = to_int value })
    | counts_json ->
        counts_json |> to_list
        |> List.map (fun item ->
               {
                 event_name = item |> member "event_name" |> to_string;
                 count = item |> member "count" |> to_int;
               })
  in
  let steps : structured_telemetry_step list =
    json |> member "steps" |> to_list
    |> List.map (fun step ->
           let kind = step |> member "kind" |> to_string in
           {
             seq = step |> member "seq" |> to_int;
             ts = step |> member "ts" |> to_float;
             event_name =
               step |> member "event_name" |> to_string_option
               |> Option.value ~default:(infer_event_name_from_kind kind);
             participant = step |> member "participant" |> to_string_option;
             detail = step |> member "detail" |> to_string_option;
             actor = step |> member "actor" |> to_string_option;
             role = step |> member "role" |> to_string_option;
             provider = step |> member "provider" |> to_string_option;
             model = step |> member "model" |> to_string_option;
             artifact_id = step |> member "artifact_id" |> to_string_option;
             artifact_name = step |> member "artifact_name" |> to_string_option;
             artifact_kind = step |> member "artifact_kind" |> to_string_option;
             checkpoint_label = step |> member "checkpoint_label" |> to_string_option;
             outcome = step |> member "outcome" |> to_string_option;
           })
  in
  ({
    session_id = json |> member "session_id" |> to_string;
    generated_at = json |> member "generated_at" |> to_float;
    step_count = json |> member "step_count" |> to_int;
    event_counts;
    steps;
  } : structured_telemetry)

let evidence_of_json json =
  let open Yojson.Safe.Util in
  let files =
    json |> member "files" |> to_list
    |> List.map (fun file ->
           {
             label = file |> member "label" |> to_string;
             path = file |> member "path" |> to_string;
             size_bytes = file |> member "size_bytes" |> to_int;
             md5 = file |> member "md5" |> to_string;
           })
  in
  let missing_files =
    json |> member "missing_files" |> to_list
    |> List.map (fun file ->
           {
             label = file |> member "label" |> to_string;
             path = file |> member "path" |> to_string;
           })
  in
  {
    session_id = json |> member "session_id" |> to_string;
    generated_at = json |> member "generated_at" |> to_float;
    files;
    missing_files;
  }

let get_report ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  let path = Runtime_store.report_json_path store session_id in
  let* raw = Runtime_store.load_text path in
  parse_runtime_json Runtime.report_of_yojson raw

let get_proof ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  let path = Runtime_store.proof_json_path store session_id in
  let* raw = Runtime_store.load_text path in
  parse_runtime_json Runtime.proof_of_yojson raw

let list_sessions ?session_root () =
  let* store = make_store ?session_root () in
  let root = Runtime_store.sessions_dir store in
  if not (Sys.file_exists root) then Ok []
  else
    root |> Sys.readdir |> Array.to_list |> List.sort String.compare
    |> List.fold_left
         (fun acc session_id ->
           let* rev = acc in
           let path = Runtime_store.session_path store session_id in
           if Sys.is_directory (Runtime_store.session_dir store session_id) then
             match Runtime_store.load_session store session_id with
             | Ok session ->
                 Ok
                   ({
                      session_id = session.session_id;
                      title = session.title;
                      tag = session.tag;
                      goal = session.goal;
                      updated_at = session.updated_at;
                      phase = session.phase;
                      participant_count = List.length session.participants;
                      path;
                    }
                    :: rev)
             | Error _ -> Ok rev
           else Ok rev)
         (Ok [])
    |> Result.map List.rev

let get_session ?session_root session_id =
  let* store = make_store ?session_root () in
  Runtime_store.load_session store session_id

let get_session_events ?session_root session_id =
  let* store = make_store ?session_root () in
  Runtime_store.read_events store session_id ()

let list_artifacts ?session_root ~session_id () =
  Artifact_service.list ?session_root ~session_id ()

let get_artifact_text ?session_root ~session_id ~artifact_id () =
  Artifact_service.get_text ?session_root ~session_id ~artifact_id ()

let get_telemetry ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-telemetry-json"
      ()
  in
  let* raw =
    Artifact_service.get_text ?session_root ~session_id
      ~artifact_id:artifact.artifact_id ()
  in
  decode_json_with telemetry_of_json raw

let get_telemetry_structured ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-telemetry-json"
      ()
  in
  let* raw =
    Artifact_service.get_text ?session_root ~session_id
      ~artifact_id:artifact.artifact_id ()
  in
  decode_json_with structured_telemetry_of_json raw

let get_evidence ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-evidence" ()
  in
  let* raw =
    Artifact_service.get_text ?session_root ~session_id
      ~artifact_id:artifact.artifact_id ()
  in
  decode_json_with evidence_of_json raw

let get_raw_trace_runs ?session_root ~session_id () =
  let* paths = get_raw_trace_files ?session_root ~session_id () in
  paths
  |> List.map (fun path -> Raw_trace.read_runs ~path ())
  |> List.fold_left
       (fun acc item ->
         match acc, item with
         | Ok runs, Ok entries -> Ok (entries @ runs)
         | Error _ as err, _ -> err
         | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map
       (List.sort (fun (a : raw_trace_run) (b : raw_trace_run) ->
            Int.compare a.start_seq b.start_seq))

let get_raw_trace_run ?session_root ~session_id ~worker_run_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  match
    List.find_opt
      (fun (run : raw_trace_run) ->
        String.equal run.worker_run_id worker_run_id)
      runs
  with
  | Some run -> Ok run
  | None ->
      Error
        (file_read_error ~path:worker_run_id
           ~detail:
             (Printf.sprintf "Raw trace run '%s' not found in session %s"
                worker_run_id session_id))

let get_raw_trace_records ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace.read_run run

let get_raw_trace_summary ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace.summarize_run run

let validate_raw_trace_run ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace.validate_run run

let get_latest_raw_trace_run ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  match List.rev runs with
  | latest :: _ -> Ok (Some latest)
  | [] -> Ok None

let get_raw_trace_summaries ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  runs
  |> List.map Raw_trace.summarize_run
  |> List.fold_left
       (fun acc item ->
         match acc, item with
         | Ok summaries, Ok summary -> Ok (summary :: summaries)
         | Error _ as err, _ -> err
         | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev

let get_raw_trace_validations ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  runs
  |> List.map Raw_trace.validate_run
  |> List.fold_left
       (fun acc item ->
         match acc, item with
         | Ok validations, Ok validation -> Ok (validation :: validations)
         | Error _ as err, _ -> err
         | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev

let evidence_capabilities_of_bundle ~raw_trace_runs =
  {
    raw_trace = raw_trace_runs <> [];
    validated_summary = true;
    proof_bundle = true;
  }

let get_proof_bundle ?session_root ~session_id () =
  let* session = get_session ?session_root session_id in
  let* report = get_report ?session_root ~session_id () in
  let* proof = get_proof ?session_root ~session_id () in
  let* telemetry = get_telemetry ?session_root ~session_id () in
  let* structured_telemetry =
    get_telemetry_structured ?session_root ~session_id ()
  in
  let* evidence = get_evidence ?session_root ~session_id () in
  let* latest_raw_trace_run = get_latest_raw_trace_run ?session_root ~session_id () in
  let* raw_trace_runs = get_raw_trace_runs ?session_root ~session_id () in
  let* raw_trace_summaries =
    get_raw_trace_summaries ?session_root ~session_id ()
  in
  let* raw_trace_validations =
    get_raw_trace_validations ?session_root ~session_id ()
  in
  let capabilities = evidence_capabilities_of_bundle ~raw_trace_runs in
  Ok
    {
      session;
      report;
      proof;
      telemetry;
      structured_telemetry;
      evidence;
      latest_raw_trace_run;
      raw_trace_runs;
      raw_trace_summaries;
      raw_trace_validations;
      capabilities;
    }

let rename_session ?session_root ~session_id ~title () =
  let* store = make_store ?session_root () in
  let* session = Runtime_store.load_session store session_id in
  let title =
    match String.trim title with
    | "" ->
        None
    | value -> Some value
  in
  Runtime_store.save_session store { session with title }

let tag_session ?session_root ~session_id ~tag () =
  let* store = make_store ?session_root () in
  let* session = Runtime_store.load_session store session_id in
  let normalized =
    match tag with
    | Some value when String.trim value <> "" -> Some (String.trim value)
    | _ -> None
  in
  Runtime_store.save_session store { session with tag = normalized }
