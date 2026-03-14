type trace_capability =
  | Raw
  | Summary_only
  | No_trace
[@@deriving show]

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

type worker_status =
  | Planned
  | Accepted
  | Ready
  | Running
  | Completed
  | Failed
[@@deriving show]

type worker_run = {
  worker_run_id: string;
  agent_name: string;
  role: string option;
  aliases: string list;
  provider: string option;
  model: string option;
  requested_provider: string option;
  requested_model: string option;
  requested_policy: string option;
  resolved_provider: string option;
  resolved_model: string option;
  status: worker_status;
  trace_capability: trace_capability;
  validated: bool;
  tool_names: string list;
  final_text: string option;
  stop_reason: string option;
  error: string option;
  failure_reason: string option;
  started_at: float option;
  finished_at: float option;
  last_progress_at: float option;
  policy_snapshot: string option;
  paired_tool_result_count: int;
  has_file_write: bool;
  verification_pass_after_file_write: bool;
}

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
  worker_runs: worker_run list;
  latest_accepted_worker_run: worker_run option;
  latest_ready_worker_run: worker_run option;
  latest_running_worker_run: worker_run option;
  latest_worker_run: worker_run option;
  latest_completed_worker_run: worker_run option;
  latest_validated_worker_run: worker_run option;
  latest_failed_worker_run: worker_run option;
  validated_worker_runs: worker_run list;
  raw_trace_run_count: int;
  validated_worker_run_count: int;
  trace_capabilities: trace_capability list;
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

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

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

let summarize_runs runs =
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

let get_raw_trace_summaries ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  summarize_runs runs

let validate_runs runs =
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

let get_raw_trace_validations ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  validate_runs runs

let evidence_capabilities_of_bundle ~raw_trace_runs ~raw_trace_summaries
    ~raw_trace_validations =
  {
    raw_trace = raw_trace_runs <> [];
    validated_summary =
      raw_trace_summaries <> [] && raw_trace_validations <> [];
    proof_bundle = true;
  }

let participant_by_name (session : Runtime.session) name =
  List.find_opt
    (fun (participant : Runtime.participant) -> String.equal participant.name name)
    session.participants

let resolved_provider_of_participant (session : Runtime.session)
    (participant : Runtime.participant option) =
  match participant with
  | Some p -> (
      match p.resolved_provider with
      | Some _ as value -> value
      | None -> p.provider)
  | None -> session.provider

let resolved_model_of_participant (session : Runtime.session)
    (participant : Runtime.participant option) =
  match participant with
  | Some p -> (
      match p.resolved_model with
      | Some _ as value -> value
      | None -> p.model)
  | None -> session.model

let worker_status_of_participant (participant : Runtime.participant option) =
  match participant with
  | Some p -> (
      match p.state with
      | Runtime.Failed_participant -> Failed
      | Runtime.Done -> Completed
      | Runtime.Live | Runtime.Idle ->
          if p.last_progress_at <> None && p.last_progress_at <> p.started_at then
            Running
          else Ready
      | Runtime.Starting -> Accepted
      | Runtime.Planned | Runtime.Detached -> Planned)
  | None -> Completed

let worker_order_ts (worker : worker_run) =
  match worker.finished_at with
  | Some _ as value -> value
  | None -> (
      match worker.last_progress_at with
      | Some _ as value -> value
      | None -> worker.started_at)

let sort_worker_runs worker_runs =
  List.sort
    (fun a b ->
      match worker_order_ts a, worker_order_ts b with
      | Some x, Some y -> Float.compare x y
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> String.compare a.worker_run_id b.worker_run_id)
    worker_runs

let latest_worker_by predicate worker_runs =
  worker_runs
  |> List.filter predicate |> sort_worker_runs |> List.rev
  |> function
  | worker :: _ -> Some worker
  | [] -> None

let worker_run_of_raw (session : Runtime.session)
    (summary : raw_trace_summary) (validation : raw_trace_validation) =
  let participant = participant_by_name session summary.run_ref.agent_name in
  let provider = resolved_provider_of_participant session participant in
  let model = resolved_model_of_participant session participant in
  let final_text =
    match summary.final_text with
    | Some _ as value -> value
    | None -> Option.bind participant (fun p -> p.summary)
  in
  let error =
    match summary.error with
    | Some _ as value -> value
    | None -> Option.bind participant (fun p -> p.last_error)
  in
  let failure_reason =
    match validation.failure_reason with
    | Some _ as value -> value
    | None -> error
  in
  {
    worker_run_id = summary.run_ref.worker_run_id;
    agent_name = summary.run_ref.agent_name;
    role = Option.bind participant (fun p -> p.role);
    aliases = Option.bind participant (fun p -> Some p.aliases) |> Option.value ~default:[];
    provider;
    model;
    requested_provider = Option.bind participant (fun p -> p.requested_provider);
    requested_model = Option.bind participant (fun p -> p.requested_model);
    requested_policy =
      first_some
        (Option.bind participant (fun p -> p.requested_policy))
        session.permission_mode;
    resolved_provider = provider;
    resolved_model = model;
    status = worker_status_of_participant participant;
    trace_capability = Raw;
    validated = validation.ok;
    tool_names = validation.tool_names;
    final_text;
    stop_reason = validation.stop_reason;
    error;
    failure_reason;
    started_at =
      first_some (Option.bind participant (fun p -> p.started_at)) summary.started_at;
    finished_at =
      first_some (Option.bind participant (fun p -> p.finished_at)) summary.finished_at;
    last_progress_at = Option.bind participant (fun p -> p.last_progress_at);
    policy_snapshot = session.permission_mode;
    paired_tool_result_count = validation.paired_tool_result_count;
    has_file_write = validation.has_file_write;
    verification_pass_after_file_write =
      validation.verification_pass_after_file_write;
  }

let summary_only_worker_run (session : Runtime.session) index
    (participant : Runtime.participant) =
  let ts =
    match participant.started_at with
    | Some _ as value -> value
    | None -> participant.finished_at
  in
  let stamp =
    match ts with
    | Some ts -> Int64.of_float (ts *. 1000.)
    | None -> Int64.of_int index
  in
  {
    worker_run_id =
      Printf.sprintf "summary-only:%s:%Ld" participant.name stamp;
    agent_name = participant.name;
    role = participant.role;
    aliases = participant.aliases;
    provider = resolved_provider_of_participant session (Some participant);
    model = resolved_model_of_participant session (Some participant);
    requested_provider = participant.requested_provider;
    requested_model = participant.requested_model;
    requested_policy = first_some participant.requested_policy session.permission_mode;
    resolved_provider = resolved_provider_of_participant session (Some participant);
    resolved_model = resolved_model_of_participant session (Some participant);
    status = worker_status_of_participant (Some participant);
    trace_capability = Summary_only;
    validated = false;
    tool_names = [];
    final_text = participant.summary;
    stop_reason = None;
    error = participant.last_error;
    failure_reason = participant.last_error;
    started_at = participant.started_at;
    finished_at = participant.finished_at;
    last_progress_at = participant.last_progress_at;
    policy_snapshot = session.permission_mode;
    paired_tool_result_count = 0;
    has_file_write = false;
    verification_pass_after_file_write = false;
  }

let unique_trace_capabilities worker_runs =
  worker_runs
  |> List.fold_left
       (fun acc (worker : worker_run) ->
         if List.exists (( = ) worker.trace_capability) acc then acc
         else acc @ [ worker.trace_capability ])
       []

let get_worker_runs ?session_root ~session_id () =
  let* session = get_session ?session_root session_id in
  let* summaries = get_raw_trace_summaries ?session_root ~session_id () in
  let* validations = get_raw_trace_validations ?session_root ~session_id () in
  let validation_by_run =
    validations
    |> List.map (fun (validation : raw_trace_validation) ->
           (validation.run_ref.worker_run_id, validation))
  in
  let raw_worker_runs =
    summaries
    |> List.filter_map (fun (summary : raw_trace_summary) ->
           match List.assoc_opt summary.run_ref.worker_run_id validation_by_run with
           | Some validation -> Some (worker_run_of_raw session summary validation)
           | None -> None)
  in
  let raw_agent_names =
    raw_worker_runs |> List.map (fun worker -> worker.agent_name)
  in
  let summary_only_runs =
    session.participants
    |> List.mapi (fun index (participant : Runtime.participant) ->
           if List.exists (String.equal participant.name) raw_agent_names then None
           else Some (summary_only_worker_run session index participant))
    |> List.filter_map (fun value -> value)
  in
  Ok (sort_worker_runs (raw_worker_runs @ summary_only_runs))

let get_latest_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun _ -> true) workers)

let get_latest_accepted_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.status = Accepted) workers)

let get_latest_ready_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.status = Ready) workers)

let get_latest_running_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.status = Running) workers)

let get_latest_completed_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.status = Completed) workers)

let get_latest_failed_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.status = Failed) workers)

let get_latest_validated_worker_run ?session_root ~session_id () =
  let* workers = get_worker_runs ?session_root ~session_id () in
  Ok (latest_worker_by (fun worker -> worker.validated) workers)

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
  let* raw_trace_summaries = summarize_runs raw_trace_runs in
  let* raw_trace_validations = validate_runs raw_trace_runs in
  let* worker_runs = get_worker_runs ?session_root ~session_id () in
  let latest_worker_run = latest_worker_by (fun _ -> true) worker_runs in
  let latest_accepted_worker_run =
    latest_worker_by (fun worker -> worker.status = Accepted) worker_runs
  in
  let latest_ready_worker_run =
    latest_worker_by (fun worker -> worker.status = Ready) worker_runs
  in
  let latest_running_worker_run =
    latest_worker_by (fun worker -> worker.status = Running) worker_runs
  in
  let latest_completed_worker_run =
    latest_worker_by (fun worker -> worker.status = Completed) worker_runs
  in
  let latest_validated_worker_run =
    latest_worker_by (fun worker -> worker.validated) worker_runs
  in
  let latest_failed_worker_run =
    latest_worker_by (fun worker -> worker.status = Failed) worker_runs
  in
  let validated_worker_runs =
    worker_runs |> List.filter (fun worker -> worker.validated)
  in
  let raw_trace_run_count = List.length raw_trace_runs in
  let validated_worker_run_count = List.length validated_worker_runs in
  let trace_capabilities = unique_trace_capabilities worker_runs in
  let capabilities =
    evidence_capabilities_of_bundle ~raw_trace_runs ~raw_trace_summaries
      ~raw_trace_validations
  in
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
      worker_runs;
      latest_accepted_worker_run;
      latest_ready_worker_run;
      latest_running_worker_run;
      latest_worker_run;
      latest_completed_worker_run;
      latest_validated_worker_run;
      latest_failed_worker_run;
      validated_worker_runs;
      raw_trace_run_count;
      validated_worker_run_count;
      trace_capabilities;
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
