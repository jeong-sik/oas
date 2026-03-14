open Runtime

type telemetry_step = {
  seq: int;
  ts: float;
  kind: string;
  participant: string option;
  detail: string option;
}

type telemetry_report = {
  session_id: string;
  generated_at: float;
  step_count: int;
  event_counts: (string * int) list;
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
  let steps =
    List.map
      (fun (event : event) ->
        let participant, detail = participant_and_detail_of_event event.kind in
        {
          seq = event.seq;
          ts = event.ts;
          kind = show_event_kind event.kind;
          participant;
          detail;
        })
      events
  in
  {
    session_id = session.session_id;
    generated_at = now ();
    step_count = List.length steps;
    event_counts;
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
      ( "steps",
        `List
          (List.map
             (fun step ->
               `Assoc
                 [
                   ("seq", `Int step.seq);
                   ("ts", `Float step.ts);
                   ("kind", `String step.kind);
                   ( "participant",
                     match step.participant with
                     | Some value -> `String value
                     | None -> `Null );
                   ( "detail",
                     match step.detail with
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
  let files =
    file_specs
    |> List.filter_map (fun (label, path) ->
           if Sys.file_exists path then
             Some
               {
                 label;
                 path;
                 size_bytes = file_size path;
                 md5 = digest_file_md5 path;
               }
           else
             None)
  in
  { session_id; generated_at = now (); files }

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
    ]
