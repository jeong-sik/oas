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

type proof_bundle = {
  session: Runtime.session;
  report: Runtime.report;
  proof: Runtime.proof;
  telemetry: telemetry;
  evidence: evidence;
}

let ( let* ) = Result.bind

let make_store ?session_root () =
  Runtime_store.create ?root:session_root ()

let json_parse_error detail =
  Error.Serialization (JsonParseError { detail })

let file_read_error ~path ~detail =
  Error.Io (FileOpFailed { op = "read"; path; detail })

let parse_json_string raw =
  try Ok (Yojson.Safe.from_string raw)
  with Yojson.Json_error detail -> Error (json_parse_error detail)

let parse_runtime_json of_yojson raw =
  let* json = parse_json_string raw in
  match of_yojson json with
  | Ok value -> Ok value
  | Error detail -> Error (json_parse_error detail)

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

let telemetry_of_json json =
  let open Yojson.Safe.Util in
  let event_counts =
    json |> member "event_counts" |> to_assoc
    |> List.map (fun (name, value) -> { name; count = to_int value })
  in
  let steps =
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
  {
    session_id = json |> member "session_id" |> to_string;
    generated_at = json |> member "generated_at" |> to_float;
    step_count = json |> member "step_count" |> to_int;
    event_counts;
    steps;
  }

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
  try Ok (telemetry_of_json (Yojson.Safe.from_string raw))
  with
  | Yojson.Json_error detail -> Error (json_parse_error detail)
  | Yojson.Safe.Util.Type_error (detail, _) -> Error (json_parse_error detail)

let get_evidence ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-evidence" ()
  in
  let* raw =
    Artifact_service.get_text ?session_root ~session_id
      ~artifact_id:artifact.artifact_id ()
  in
  try Ok (evidence_of_json (Yojson.Safe.from_string raw))
  with
  | Yojson.Json_error detail -> Error (json_parse_error detail)
  | Yojson.Safe.Util.Type_error (detail, _) -> Error (json_parse_error detail)

let get_proof_bundle ?session_root ~session_id () =
  let* session = get_session ?session_root session_id in
  let* report = get_report ?session_root ~session_id () in
  let* proof = get_proof ?session_root ~session_id () in
  let* telemetry = get_telemetry ?session_root ~session_id () in
  let* evidence = get_evidence ?session_root ~session_id () in
  Ok { session; report; proof; telemetry; evidence }

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
