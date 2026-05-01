open Base
(** Sessions store operations — file I/O, artifact retrieval, raw trace access.

    Read-from-store operations that bridge the runtime file layout
    with the typed Sessions domain. JSON parsing lives in
    {!Sessions_store_parsers}. *)

open Sessions_types
open Sessions_store_parsers

let ( let* ) = Result.bind
let make_store ?session_root () = Runtime_store.create ?root:session_root ()
let file_read_error = Util.file_read_error
let first_some = Util.first_some

let primary_alias aliases =
  match aliases with
  | alias :: _ when String.trim alias <> "" -> Some alias
  | _ -> None
;;

let latest_named_artifact artifacts name =
  List.fold_left
    (fun acc (artifact : Runtime.artifact) ->
       if not (String.equal artifact.name name)
       then acc
       else (
         match acc with
         | None -> Some artifact
         | Some current when artifact.created_at >= current.created_at -> Some artifact
         | Some _ -> acc))
    None
    artifacts
;;

let get_named_artifact ?session_root ~session_id ~name () =
  let* artifacts = Artifact_service.list ?session_root ~session_id () in
  match latest_named_artifact artifacts name with
  | Some artifact -> Ok artifact
  | None ->
    Error
      (file_read_error
         ~path:name
         ~detail:(Printf.sprintf "Artifact '%s' not found in session %s" name session_id))
;;

let get_optional_named_artifact ?session_root ~session_id ~name () =
  let* artifacts = Artifact_service.list ?session_root ~session_id () in
  Ok (latest_named_artifact artifacts name)
;;

let get_raw_trace_dir ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  Ok (Runtime_store.raw_traces_dir store session_id)
;;

let get_raw_trace_files ?session_root ~session_id () =
  let* dir = get_raw_trace_dir ?session_root ~session_id () in
  if not (Sys.file_exists dir)
  then Ok []
  else
    dir
    |> Sys.readdir
    |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".jsonl")
    |> List.sort String.compare
    |> List.map (fun name -> Filename.concat dir name)
    |> fun paths -> Ok paths
;;

let get_report ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  let path = Runtime_store.report_json_path store session_id in
  let* raw = Runtime_store.load_text path in
  parse_runtime_json Runtime.report_of_yojson raw
;;

let get_proof ?session_root ~session_id () =
  let* store = make_store ?session_root () in
  let path = Runtime_store.proof_json_path store session_id in
  let* raw = Runtime_store.load_text path in
  parse_runtime_json Runtime.proof_of_yojson raw
;;

let list_sessions ?session_root () =
  let* store = make_store ?session_root () in
  let root = Runtime_store.sessions_dir store in
  if not (Sys.file_exists root)
  then Ok []
  else
    root
    |> Sys.readdir
    |> Array.to_list
    |> List.sort String.compare
    |> List.fold_left
         (fun acc session_id ->
            let* rev = acc in
            let path = Runtime_store.session_path store session_id in
            if Sys.is_directory (Runtime_store.session_dir store session_id)
            then (
              match Runtime_store.load_session store session_id with
              | Ok session ->
                Ok
                  ({ session_id = session.session_id
                   ; title = session.title
                   ; tag = session.tag
                   ; goal = session.goal
                   ; updated_at = session.updated_at
                   ; phase = session.phase
                   ; participant_count = List.length session.participants
                   ; path
                   }
                   :: rev)
              | Error _ -> Ok rev)
            else Ok rev)
         (Ok [])
    |> Result.map List.rev
;;

let get_session ?session_root session_id =
  let* store = make_store ?session_root () in
  Runtime_store.load_session store session_id
;;

let get_session_events ?session_root session_id =
  let* store = make_store ?session_root () in
  Runtime_store.read_events store session_id ()
;;

let list_artifacts ?session_root ~session_id () =
  Artifact_service.list ?session_root ~session_id ()
;;

let get_artifact_text ?session_root ~session_id ~artifact_id () =
  Artifact_service.get_text ?session_root ~session_id ~artifact_id ()
;;

let get_telemetry ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-telemetry-json" ()
  in
  let* raw =
    Artifact_service.get_text
      ?session_root
      ~session_id
      ~artifact_id:artifact.artifact_id
      ()
  in
  decode_json_with telemetry_of_json raw
;;

let get_telemetry_structured ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-telemetry-json" ()
  in
  let* raw =
    Artifact_service.get_text
      ?session_root
      ~session_id
      ~artifact_id:artifact.artifact_id
      ()
  in
  decode_json_with structured_telemetry_of_json raw
;;

let get_evidence ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-evidence" ()
  in
  let* raw =
    Artifact_service.get_text
      ?session_root
      ~session_id
      ~artifact_id:artifact.artifact_id
      ()
  in
  decode_json_with evidence_of_json raw
;;

let get_raw_trace_manifest ?session_root ~session_id () =
  let* artifact =
    get_named_artifact ?session_root ~session_id ~name:"runtime-raw-trace-json" ()
  in
  let* raw =
    Artifact_service.get_text
      ?session_root
      ~session_id
      ~artifact_id:artifact.artifact_id
      ()
  in
  parse_runtime_json raw_trace_manifest_of_json raw
;;

let get_hook_summary ?session_root ~session_id () =
  let* paths = get_raw_trace_files ?session_root ~session_id () in
  let* runs =
    paths
    |> List.map (fun path -> Raw_trace_query.read_runs ~path ())
    |> List.fold_left
         (fun acc item ->
            match acc, item with
            | Ok runs, Ok entries -> Ok (entries @ runs)
            | (Error _ as err), _ -> err
            | _, (Error _ as err) -> err)
         (Ok [])
  in
  let table : (string, hook_summary) Hashtbl.t = Hashtbl.create 8 in
  let update_summary hook_name decision detail ts =
    let current =
      match Hashtbl.find_opt table hook_name with
      | Some value -> value
      | None ->
        { hook_name
        ; count = 0
        ; latest_decision = None
        ; latest_detail = None
        ; latest_ts = None
        }
    in
    Hashtbl.replace
      table
      hook_name
      { hook_name
      ; count = current.count + 1
      ; latest_decision = Some decision
      ; latest_detail = detail
      ; latest_ts = Some ts
      }
  in
  let* () =
    runs
    |> List.fold_left
         (fun acc run ->
            let* () = acc in
            let* records = Raw_trace_query.read_run run in
            List.iter
              (fun (record : Raw_trace.record) ->
                 match record.record_type, record.hook_name, record.hook_decision with
                 | Raw_trace.Hook_invoked, Some hook_name, Some decision ->
                   update_summary hook_name decision record.hook_detail record.ts
                 | _ -> ())
              records;
            Ok ())
         (Ok ())
  in
  Ok
    (Hashtbl.to_seq_values table
     |> List.of_seq
     |> List.sort (fun a b -> String.compare a.hook_name b.hook_name))
;;

let get_tool_catalog ?session_root ~session_id () =
  let* artifact_opt =
    get_optional_named_artifact ?session_root ~session_id ~name:"tool-catalog" ()
  in
  match artifact_opt with
  | None -> Ok []
  | Some artifact ->
    let* raw =
      Artifact_service.get_text
        ?session_root
        ~session_id
        ~artifact_id:artifact.artifact_id
        ()
    in
    let* json = parse_json_string raw in
    let open Yojson.Safe.Util in
    Ok (json |> to_list |> List.map tool_contract_of_json)
;;

let get_raw_trace_runs ?session_root ~session_id () =
  let* paths = get_raw_trace_files ?session_root ~session_id () in
  paths
  |> List.map (fun path -> Raw_trace_query.read_runs ~path ())
  |> List.fold_left
       (fun acc item ->
          match acc, item with
          | Ok runs, Ok entries -> Ok (entries @ runs)
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map
       (List.sort (fun (a : raw_trace_run) (b : raw_trace_run) ->
          Int.compare a.start_seq b.start_seq))
;;

let get_raw_trace_run ?session_root ~session_id ~worker_run_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  match
    List.find_opt
      (fun (run : raw_trace_run) -> String.equal run.worker_run_id worker_run_id)
      runs
  with
  | Some run -> Ok run
  | None ->
    Error
      (file_read_error
         ~path:worker_run_id
         ~detail:
           (Printf.sprintf
              "Raw trace run '%s' not found in session %s"
              worker_run_id
              session_id))
;;

let get_raw_trace_records ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace_query.read_run run
;;

let get_raw_trace_summary ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace_query.summarize_run run
;;

let validate_raw_trace_run ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace_query.validate_run run
;;

let get_latest_raw_trace_run ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  match List.rev runs with
  | latest :: _ -> Ok (Some latest)
  | [] -> Ok None
;;

let summarize_runs runs =
  runs
  |> List.map Raw_trace_query.summarize_run
  |> List.fold_left
       (fun acc item ->
          match acc, item with
          | Ok summaries, Ok summary -> Ok (summary :: summaries)
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev
;;

let get_raw_trace_summaries ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  summarize_runs runs
;;

let validate_runs runs =
  runs
  |> List.map Raw_trace_query.validate_run
  |> List.fold_left
       (fun acc item ->
          match acc, item with
          | Ok validations, Ok validation -> Ok (validation :: validations)
          | (Error _ as err), _ -> err
          | _, (Error _ as err) -> err)
       (Ok [])
  |> Result.map List.rev
;;

let get_raw_trace_validations ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  validate_runs runs
;;

let rename_session ?session_root ~session_id ~title () =
  let* store = make_store ?session_root () in
  let* session = Runtime_store.load_session store session_id in
  let title =
    match String.trim title with
    | "" -> None
    | value -> Some value
  in
  Runtime_store.save_session store { session with title }
;;

let tag_session ?session_root ~session_id ~tag () =
  let* store = make_store ?session_root () in
  let* session = Runtime_store.load_session store session_id in
  let normalized =
    match tag with
    | Some value when String.trim value <> "" -> Some (String.trim value)
    | _ -> None
  in
  Runtime_store.save_session store { session with tag = normalized }
;;
