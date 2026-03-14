open Runtime

type telemetry_step = {
  seq: int;
  ts: float;
  event_name: string;
  kind: string;
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

type telemetry_report = {
  session_id: string;
  generated_at: float;
  step_count: int;
  event_counts: (string * int) list;
  event_name_counts: (string * int) list;
  steps: telemetry_step list;
}

type evidence_file = {
  label: string;
  path: string;
  size_bytes: int;
  md5: string;
}

type evidence_bundle = {
  session_id: string;
  generated_at: float;
  files: evidence_file list;
  missing_files: (string * string) list;
}

let now () = Unix.gettimeofday ()

let participant_and_detail_of_event = function
  | Session_started _ -> (None, Some "session_started")
  | Session_settings_updated _ -> (None, Some "session_settings_updated")
  | Turn_recorded detail -> (detail.actor, Some detail.message)
  | Agent_spawn_requested detail ->
      (Some detail.participant_name, Some detail.prompt)
  | Agent_became_live detail ->
      (Some detail.participant_name, detail.summary)
  | Agent_output_delta detail ->
      (Some detail.participant_name, Some detail.delta)
  | Agent_completed detail ->
      (Some detail.participant_name, detail.summary)
  | Agent_failed detail ->
      (Some detail.participant_name, detail.error)
  | Artifact_attached detail ->
      (None, Some (detail.name ^ ":" ^ detail.kind))
  | Vote_recorded vote -> (vote.actor, Some vote.topic)
  | Checkpoint_saved detail -> (None, detail.label)
  | Finalize_requested detail -> (None, detail.reason)
  | Session_completed detail -> (None, detail.outcome)
  | Session_failed detail -> (None, detail.outcome)

let event_name_of_kind = function
  | Session_started _ -> "session_started"
  | Session_settings_updated _ -> "session_settings_updated"
  | Turn_recorded _ -> "turn_recorded"
  | Agent_spawn_requested _ -> "agent_spawn_requested"
  | Agent_became_live _ -> "agent_became_live"
  | Agent_output_delta _ -> "agent_output_delta"
  | Agent_completed _ -> "agent_completed"
  | Agent_failed _ -> "agent_failed"
  | Artifact_attached _ -> "artifact_attached"
  | Vote_recorded _ -> "vote_recorded"
  | Checkpoint_saved _ -> "checkpoint_saved"
  | Finalize_requested _ -> "finalize_requested"
  | Session_completed _ -> "session_completed"
  | Session_failed _ -> "session_failed"

let structured_fields_of_event = function
  | Session_started _ ->
      (None, None, None, None, None, None, None, None, None)
  | Session_settings_updated detail ->
      (None, None, None, detail.model, None, None, None, None, None)
  | Turn_recorded detail ->
      (detail.actor, None, None, None, None, None, None, None, None)
  | Agent_spawn_requested detail ->
      ( None,
        detail.role,
        detail.provider,
        detail.model,
        None,
        None,
        None,
        None,
        None )
  | Agent_became_live detail ->
      (None, None, detail.provider, detail.model, None, None, None, None, None)
  | Agent_output_delta _ ->
      (None, None, None, None, None, None, None, None, None)
  | Agent_completed detail ->
      (None, None, detail.provider, detail.model, None, None, None, None, None)
  | Agent_failed detail ->
      (None, None, detail.provider, detail.model, None, None, None, None, None)
  | Artifact_attached detail ->
      ( None,
        None,
        None,
        None,
        Some detail.artifact_id,
        Some detail.name,
        Some detail.kind,
        None,
        None )
  | Vote_recorded vote ->
      (vote.actor, None, None, None, None, None, None, None, None)
  | Checkpoint_saved detail ->
      (None, None, None, None, None, None, None, detail.label, None)
  | Finalize_requested _ ->
      (None, None, None, None, None, None, None, None, None)
  | Session_completed detail ->
      (None, None, None, None, None, None, None, None, detail.outcome)
  | Session_failed detail ->
      (None, None, None, None, None, None, None, None, detail.outcome)

let build_telemetry_report (session : session) (events : event list) =
  let event_counts =
    List.fold_left
      (fun acc (event : event) ->
        let key = show_event_kind event.kind in
        let prev = Option.value (List.assoc_opt key acc) ~default:0 in
        (key, prev + 1) :: List.remove_assoc key acc)
      [] events
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let event_name_counts =
    List.fold_left
      (fun acc (event : event) ->
        let key = event_name_of_kind event.kind in
        let prev = Option.value (List.assoc_opt key acc) ~default:0 in
        (key, prev + 1) :: List.remove_assoc key acc)
      [] events
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  let steps =
    List.map
      (fun (event : event) ->
        let participant, detail = participant_and_detail_of_event event.kind in
        let actor, role, provider, model, artifact_id, artifact_name, artifact_kind,
            checkpoint_label, outcome =
          structured_fields_of_event event.kind
        in
        {
          seq = event.seq;
          ts = event.ts;
          event_name = event_name_of_kind event.kind;
          kind = show_event_kind event.kind;
          participant;
          detail;
          actor;
          role;
          provider;
          model;
          artifact_id;
          artifact_name;
          artifact_kind;
          checkpoint_label;
          outcome;
        })
      events
  in
  {
    session_id = session.session_id;
    generated_at = now ();
    step_count = List.length steps;
    event_counts;
    event_name_counts;
    steps;
  }

let telemetry_report_to_json (report : telemetry_report) =
  `Assoc
    [
      ("session_id", `String report.session_id);
      ("generated_at", `Float report.generated_at);
      ("step_count", `Int report.step_count);
      ( "event_counts",
        `Assoc
          (List.map (fun (name, count) -> (name, `Int count)) report.event_counts)
      );
      ( "event_name_counts",
        `List
          (List.map
             (fun (event_name, count) ->
               `Assoc
                 [
                   ("event_name", `String event_name);
                   ("count", `Int count);
                 ])
             report.event_name_counts) );
      ( "steps",
        `List
          (List.map
             (fun step ->
               `Assoc
                 [
                   ("seq", `Int step.seq);
                   ("ts", `Float step.ts);
                   ("event_name", `String step.event_name);
                   ("kind", `String step.kind);
                   ( "participant",
                     match step.participant with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "detail",
                     match step.detail with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "actor",
                     match step.actor with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "role",
                     match step.role with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "provider",
                     match step.provider with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "model",
                     match step.model with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "artifact_id",
                     match step.artifact_id with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "artifact_name",
                     match step.artifact_name with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "artifact_kind",
                     match step.artifact_kind with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "checkpoint_label",
                     match step.checkpoint_label with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "outcome",
                     match step.outcome with
                     | Some value -> `String value
                     | None -> `Null );
                 ])
             report.steps) );
    ]

let telemetry_report_to_markdown (report : telemetry_report) =
  let counts =
    report.event_counts
    |> List.map (fun (name, count) -> Printf.sprintf "- %s: %d" name count)
    |> String.concat "\n"
  in
  let steps =
    report.steps
    |> List.map (fun step ->
           let participant =
             match step.participant with
             | Some value -> value
             | None -> "-"
           in
           let detail =
             match step.detail with
             | Some value when String.trim value <> "" -> value
             | _ -> "-"
           in
           Printf.sprintf "- #%d %s participant=%s detail=%s"
             step.seq step.kind participant detail)
    |> String.concat "\n"
  in
  String.concat "\n"
    [
      "# Runtime Telemetry";
      "";
      Printf.sprintf "- Session ID: %s" report.session_id;
      Printf.sprintf "- Step Count: %d" report.step_count;
      "";
      "## Event Name Counts";
      (report.event_name_counts
      |> List.map (fun (name, count) -> Printf.sprintf "- %s: %d" name count)
      |> String.concat "\n");
      "";
      "## Event Counts";
      counts;
      "";
      "## Steps";
      steps;
      "";
    ]

let digest_file_md5 path =
  Digest.file path |> Digest.to_hex

let file_size path =
  (Unix.stat path).st_size

let build_evidence_bundle ~session_id file_specs =
  let files, missing_files =
    List.fold_left
      (fun (files, missing_files) (label, path) ->
        if Sys.file_exists path then
          ( {
              label;
              path;
              size_bytes = file_size path;
              md5 = digest_file_md5 path;
            }
            :: files,
            missing_files )
        else
          (files, (label, path) :: missing_files))
      ([], []) file_specs
  in
  {
    session_id;
    generated_at = now ();
    files = List.rev files;
    missing_files = List.rev missing_files;
  }

let evidence_bundle_to_json (bundle : evidence_bundle) =
  `Assoc
    [
      ("session_id", `String bundle.session_id);
      ("generated_at", `Float bundle.generated_at);
      ( "files",
        `List
          (List.map
             (fun file ->
               `Assoc
                 [
                   ("label", `String file.label);
                   ("path", `String file.path);
                   ("size_bytes", `Int file.size_bytes);
                   ("md5", `String file.md5);
                 ])
             bundle.files) );
      ( "missing_files",
        `List
          (List.map
             (fun (label, path) ->
               `Assoc
                 [
                   ("label", `String label);
                   ("path", `String path);
                 ])
             bundle.missing_files) );
    ]
