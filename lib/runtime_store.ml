open Runtime

let ( let* ) = Result.bind

type t = {
  root: string;
}

let sessions_dir store = Filename.concat store.root "sessions"
let session_dir store session_id = Filename.concat (sessions_dir store) session_id
let session_path store session_id = Filename.concat (session_dir store session_id) "session.json"
let events_path store session_id = Filename.concat (session_dir store session_id) "events.jsonl"
let snapshots_dir store session_id = Filename.concat (session_dir store session_id) "snapshots"
let artifacts_dir store session_id = Filename.concat (session_dir store session_id) "artifacts"
let raw_traces_dir store session_id = Filename.concat (session_dir store session_id) "raw-traces"
let report_json_path store session_id = Filename.concat (artifacts_dir store session_id) "report.json"
let report_md_path store session_id = Filename.concat (artifacts_dir store session_id) "report.md"
let proof_json_path store session_id = Filename.concat (artifacts_dir store session_id) "proof.json"
let proof_md_path store session_id = Filename.concat (artifacts_dir store session_id) "proof.md"

let ensure_dir path =
  try
    if Sys.file_exists path then Ok ()
    else (
      Unix.mkdir path 0o755;
      Ok ())
  with
  | Unix.Unix_error (err, _, _) ->
      Error
        (Error.Io
           (FileOpFailed
              {
                op = "mkdir";
                path;
                detail = Unix.error_message err;
              }))
  | Sys_error detail ->
      Error
        (Error.Io
           (FileOpFailed { op = "mkdir"; path; detail }))

let ensure_tree store session_id =
  let* () = ensure_dir store.root in
  let* () = ensure_dir (sessions_dir store) in
  let* () = ensure_dir (session_dir store session_id) in
  let* () = ensure_dir (snapshots_dir store session_id) in
  let* () = ensure_dir (artifacts_dir store session_id) in
  ensure_dir (raw_traces_dir store session_id)

let default_root () =
  match Sys.getenv_opt "OAS_RUNTIME_SESSION_ROOT" with
  | Some value when String.trim value <> "" -> String.trim value
  | _ -> Filename.concat (Sys.getcwd ()) ".oas-runtime"

let create ?root () =
  let resolved =
    match root with
    | Some value when String.trim value <> "" -> String.trim value
    | _ -> default_root ()
  in
  let store = { root = resolved } in
  let* () = ensure_dir resolved in
  let* () = ensure_dir (sessions_dir store) in
  Ok store

let save_text path content =
  try
    let temp_path =
      Printf.sprintf "%s.tmp-%d-%06x" path (Unix.getpid ()) (Random.int 0xFFFFFF)
    in
    let oc = open_out_bin temp_path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc content;
        flush oc;
        Unix.rename temp_path path;
        Ok ())
  with
  | Sys_error detail ->
      Error
        (Error.Io
           (FileOpFailed { op = "write"; path; detail }))
  | Unix.Unix_error (err, _, _) ->
      Error
        (Error.Io
           (FileOpFailed { op = "write"; path; detail = Unix.error_message err }))

let load_text path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        Ok (really_input_string ic len))
  with
  | Sys_error detail ->
      Error
        (Error.Io
           (FileOpFailed { op = "read"; path; detail }))
  | Unix.Unix_error (err, _, _) ->
      Error
        (Error.Io
           (FileOpFailed { op = "read"; path; detail = Unix.error_message err }))
  | End_of_file ->
      Error
        (Error.Io
           (FileOpFailed { op = "read"; path; detail = "unexpected end of file" }))

let save_session store (session : session) =
  let* () = ensure_tree store session.session_id in
  save_text
    (session_path store session.session_id)
    (session |> session_to_yojson |> Yojson.Safe.pretty_to_string)

let load_session store session_id =
  let* raw = load_text (session_path store session_id) in
  try
    let with_default key value fields =
      if List.mem_assoc key fields then fields else (key, value) :: fields
    in
    let normalize_participant = function
        | `Assoc fields ->
          `Assoc
            (fields
            |> with_default "aliases" (`List [])
            |> with_default "worker_id" `Null
            |> with_default "runtime_actor" `Null
            |> with_default "requested_provider" `Null
            |> with_default "requested_model" `Null
            |> with_default "requested_policy" `Null
            |> with_default "resolved_provider" `Null
            |> with_default "resolved_model" `Null
            |> with_default "accepted_at" `Null
            |> with_default "ready_at" `Null
            |> with_default "first_progress_at" `Null
            |> with_default "last_progress_at" `Null)
      | json -> json
    in
    let normalize_session = function
      | `Assoc fields ->
          let participants =
            match List.assoc_opt "participants" fields with
            | Some (`List items) -> `List (List.map normalize_participant items)
            | Some value -> value
            | None -> `List []
          in
          `Assoc
            ((fields |> List.remove_assoc "participants")
            @ [ ("participants", participants) ])
      | json -> json
    in
    match session_of_yojson (Yojson.Safe.from_string raw |> normalize_session) with
    | Ok session -> Ok session
    | Error detail ->
        Error
          (Error.Serialization (JsonParseError { detail }))
  with Yojson.Json_error detail ->
    Error (Error.Serialization (JsonParseError { detail }))

let append_event store session_id (event : event) =
  let* () = ensure_tree store session_id in
  let path = events_path store session_id in
  try
    let oc =
      open_out_gen [ Open_creat; Open_text; Open_append ] 0o644 path
    in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc (event |> event_to_yojson |> Yojson.Safe.to_string);
        output_char oc '\n';
        flush oc;
        Ok ())
  with
  | Sys_error detail ->
      Error
        (Error.Io (FileOpFailed { op = "append"; path; detail }))
  | Unix.Unix_error (err, _, _) ->
      Error
        (Error.Io
           (FileOpFailed { op = "append"; path; detail = Unix.error_message err }))

let read_events store session_id ?after_seq () =
  let path = events_path store session_id in
  if not (Sys.file_exists path) then Ok []
  else
    let* raw = load_text path in
    raw
    |> String.split_on_char '\n'
    |> List.filter (fun line -> String.trim line <> "")
    |> List.fold_left
         (fun acc line ->
           let* rev = acc in
           try
             match event_of_yojson (Yojson.Safe.from_string line) with
             | Ok event ->
                 let should_include =
                   match after_seq with
                   | Some min_seq -> event.seq > min_seq
                   | None -> true
                 in
                 if should_include then Ok (event :: rev) else Ok rev
             | Error detail ->
                 Error
                   (Error.Serialization (JsonParseError { detail }))
           with Yojson.Json_error detail ->
             Error (Error.Serialization (JsonParseError { detail })))
         (Ok [])
    |> Result.map List.rev

let snapshot_path store session_id ~seq ~label =
  let base =
    match label with
    | Some value when String.trim value <> "" ->
        Printf.sprintf "%04d-%s.json" seq
          (String.map (function '/' | ' ' -> '_' | c -> c) value)
    | _ -> Printf.sprintf "%04d.json" seq
  in
  Filename.concat (snapshots_dir store session_id) base

let save_snapshot store (session : session) ~label =
  let path = snapshot_path store session.session_id ~seq:session.last_seq ~label in
  let* () = ensure_tree store session.session_id in
  let* () =
    save_text path (session |> session_to_yojson |> Yojson.Safe.pretty_to_string)
  in
  Ok path

let save_artifact_text store session_id ~name ~kind ~content =
  let safe_name =
    if String.trim name = "" then "artifact"
    else String.map (function '/' | ' ' -> '_' | c -> c) name
  in
  let extension =
    match String.lowercase_ascii kind with
    | "markdown" | "md" -> "md"
    | "json" -> "json"
    | "text" | "txt" -> "txt"
    | other when other <> "" -> other
    | _ -> "txt"
  in
  let path =
    Filename.concat (artifacts_dir store session_id)
      (Printf.sprintf "%s.%s" safe_name extension)
  in
  let* () = ensure_tree store session_id in
  let* () = save_text path content in
  Ok path

let save_report store (report : report) =
  let* () = ensure_tree store report.session_id in
  let* () =
    save_text (report_json_path store report.session_id)
      (report |> report_to_yojson |> Yojson.Safe.pretty_to_string)
  in
  save_text (report_md_path store report.session_id) report.markdown

let save_proof store (proof : proof) =
  let* () = ensure_tree store proof.session_id in
  let* () =
    save_text (proof_json_path store proof.session_id)
      (proof |> proof_to_yojson |> Yojson.Safe.pretty_to_string)
  in
  let markdown =
    let checks =
      proof.checks
      |> List.map (fun check ->
             Printf.sprintf "- [%s] %s"
               (if check.passed then "x" else " ") check.name)
      |> String.concat "\n"
    in
    let evidence =
      proof.evidence |> List.map (fun line -> "- " ^ line) |> String.concat "\n"
    in
    String.concat "\n"
      [
        "# Runtime Proof";
        "";
        Printf.sprintf "- Session ID: %s" proof.session_id;
        Printf.sprintf "- Overall OK: %b" proof.ok;
        "";
        "## Checks";
        checks;
        "";
        "## Evidence";
        evidence;
        "";
      ]
  in
  save_text (proof_md_path store proof.session_id) markdown
