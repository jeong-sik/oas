open Result_syntax

type sync_window_set =
  { windows : Runtime_sync.window list
  ; runs : Runtime_store.run_record list
  ; failures : Runtime_store.run_load_failure list
  }

let artifact_refs (session : Runtime.session) =
  session.artifacts
  |> List.map (fun (artifact : Runtime.artifact) -> artifact.artifact_id)
;;

let events_for_session session_id records =
  records
  |> List.filter_map (fun (record : Runtime_store.run_event_record) ->
    if String.equal record.session_id session_id then Some record.event else None)
;;

let sync_windows_from_store
      ?(after_seq = 0)
      ?persistence
      ?(merge_policy = Runtime_sync.Append_only)
      store
      selectors
  =
  let* selected = Runtime_store.read_window_events store selectors in
  let windows =
    selected.runs
    |> List.map (fun (run : Runtime_store.run_record) ->
      let session_id = run.session.session_id in
      Runtime_sync.make_window
        ?persistence
        ~merge_policy
        ~artifact_refs:(artifact_refs run.session)
        ~stream_id:session_id
        ~after_seq
        (events_for_session session_id selected.events))
  in
  Ok { windows; runs = selected.runs; failures = selected.failures }
;;

let run_to_yojson (run : Runtime_store.run_record) =
  `Assoc
    [ "session_id", `String run.session.session_id
    ; "path", `String run.path
    ; "updated_at", `Float run.session.updated_at
    ; "phase", Runtime.phase_to_yojson run.session.phase
    ]
;;

let failure_to_yojson (failure : Runtime_store.run_load_failure) =
  `Assoc
    [ "session_id", `String failure.session_id
    ; "path", `String failure.path
    ; "detail", `String failure.detail
    ]
;;

let sync_window_set_to_yojson set =
  `Assoc
    [ "schema_version", `Int Runtime_sync.schema_version_current
    ; "windows", `List (List.map Runtime_sync.to_json set.windows)
    ; "runs", `List (List.map run_to_yojson set.runs)
    ; "failures", `List (List.map failure_to_yojson set.failures)
    ]
;;

let sync_windows_json_from_store ?after_seq ?persistence ?merge_policy store selectors =
  let* set =
    sync_windows_from_store ?after_seq ?persistence ?merge_policy store selectors
  in
  Ok (sync_window_set_to_yojson set)
;;
