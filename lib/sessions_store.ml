(** Sessions store operations — JSON parsing, file I/O, artifact retrieval.

    Pure read-from-store operations that bridge the runtime file layout
    with the typed Sessions domain. No proof assembly or worker-run
    construction happens here. *)

open Sessions_types

let ( let* ) = Result.bind

let make_store ?session_root () =
  Runtime_store.create ?root:session_root ()

let json_parse_error = Util.json_parse_error
let file_read_error = Util.file_read_error
let contains_substring ~sub text = Util.string_contains ~needle:sub text
let first_some = Util.first_some

let primary_alias aliases =
  match aliases with
  | alias :: _ when String.trim alias <> "" -> Some alias
  | _ -> None

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

let get_optional_named_artifact ?session_root ~session_id ~name () =
  let* artifacts = Artifact_service.list ?session_root ~session_id () in
  Ok (latest_named_artifact artifacts name)

let workdir_policy_of_string = function
  | "required" -> Some Tool.Required
  | "recommended" -> Some Tool.Recommended
  | "none_expected" -> Some Tool.None_expected
  | _ -> None

let shell_constraints_of_json json =
  let open Yojson.Safe.Util in
  match json with
  | `Null -> None
  | value ->
      Some
        {
          Tool.single_command_only = value |> member "single_command_only" |> to_bool;
          shell_metacharacters_allowed =
            value |> member "shell_metacharacters_allowed" |> to_bool;
          chaining_allowed = value |> member "chaining_allowed" |> to_bool;
          redirection_allowed = value |> member "redirection_allowed" |> to_bool;
          pipes_allowed = value |> member "pipes_allowed" |> to_bool;
          workdir_policy =
            (match value |> member "workdir_policy" |> to_string_option with
            | Some policy -> workdir_policy_of_string policy
            | None -> None);
        }

let tool_contract_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    origin = json |> member "origin" |> to_string_option;
    kind = json |> member "kind" |> to_string_option;
    shell = shell_constraints_of_json (json |> member "shell");
    notes =
      json |> member "notes" |> to_list |> List.map to_string;
    examples =
      json |> member "examples" |> to_list |> List.map to_string;
  }

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

let get_hook_summary ?session_root ~session_id () =
  let* paths = get_raw_trace_files ?session_root ~session_id () in
  let* runs =
    paths
    |> List.map (fun path -> Raw_trace_query.read_runs ~path ())
    |> List.fold_left
         (fun acc item ->
           match acc, item with
           | Ok runs, Ok entries -> Ok (entries @ runs)
           | Error _ as err, _ -> err
           | _, (Error _ as err) -> err)
         (Ok [])
  in
  let table : (string, hook_summary) Hashtbl.t = Hashtbl.create 8 in
  let update_summary hook_name decision detail ts =
    let current =
      match Hashtbl.find_opt table hook_name with
      | Some value -> value
      | None ->
          {
            hook_name;
            count = 0;
            latest_decision = None;
            latest_detail = None;
            latest_ts = None;
          }
    in
    Hashtbl.replace table hook_name
      {
        hook_name;
        count = current.count + 1;
        latest_decision = Some decision;
        latest_detail = detail;
        latest_ts = Some ts;
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
               match
                 record.record_type, record.hook_name, record.hook_decision
               with
               | Raw_trace.Hook_invoked, Some hook_name, Some decision ->
                   update_summary hook_name decision record.hook_detail record.ts
               | _ -> ())
             records;
           Ok ())
         (Ok ())
  in
  Ok
    (Hashtbl.to_seq_values table |> List.of_seq
    |> List.sort (fun a b -> String.compare a.hook_name b.hook_name))

let get_tool_catalog ?session_root ~session_id () =
  let* artifact_opt =
    get_optional_named_artifact ?session_root ~session_id ~name:"tool-catalog"
      ()
  in
  match artifact_opt with
  | None -> Ok []
  | Some artifact ->
      let* raw =
        Artifact_service.get_text ?session_root ~session_id
          ~artifact_id:artifact.artifact_id ()
      in
      let* json = parse_json_string raw in
      let open Yojson.Safe.Util in
      Ok (json |> to_list |> List.map tool_contract_of_json)

let get_raw_trace_runs ?session_root ~session_id () =
  let* paths = get_raw_trace_files ?session_root ~session_id () in
  paths
  |> List.map (fun path -> Raw_trace_query.read_runs ~path ())
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
  Raw_trace_query.read_run run

let get_raw_trace_summary ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace_query.summarize_run run

let validate_raw_trace_run ?session_root ~session_id ~worker_run_id () =
  let* run = get_raw_trace_run ?session_root ~session_id ~worker_run_id () in
  Raw_trace_query.validate_run run

let get_latest_raw_trace_run ?session_root ~session_id () =
  let* runs = get_raw_trace_runs ?session_root ~session_id () in
  match List.rev runs with
  | latest :: _ -> Ok (Some latest)
  | [] -> Ok None

let summarize_runs runs =
  runs
  |> List.map Raw_trace_query.summarize_run
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
  |> List.map Raw_trace_query.validate_run
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
