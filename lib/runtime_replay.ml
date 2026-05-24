open Result_syntax

type sync_window_set =
  { windows : Runtime_sync.window list
  ; runs : Runtime_store.run_record list
  ; failures : Runtime_store.run_load_failure list
  }

type checkpoint_ref =
  { session_id : string
  ; event_seq : int
  ; label : string option
  ; path : string
  }

type checkpoint_record =
  { checkpoint_ref : checkpoint_ref
  ; checkpoint : Checkpoint.t
  }

type checkpoint_delta_entry =
  | Full_checkpoint of
      { checkpoint_ref : checkpoint_ref
      ; checkpoint : Checkpoint.t
      }
  | Delta_checkpoint of
      { base : checkpoint_ref
      ; target : checkpoint_ref
      ; delta : Checkpoint.delta
      }

type checkpoint_delta_projection =
  { entries : checkpoint_delta_entry list
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

let checkpoint_ref_to_yojson (ref_ : checkpoint_ref) =
  `Assoc
    [ "session_id", `String ref_.session_id
    ; "event_seq", `Int ref_.event_seq
    ; ( "label"
      , match ref_.label with
        | None -> `Null
        | Some label -> `String label )
    ; "path", `String ref_.path
    ]
;;

let checkpoint_delta_entry_to_yojson = function
  | Full_checkpoint { checkpoint_ref; checkpoint } ->
    `Assoc
      [ "kind", `String "full_checkpoint"
      ; "checkpoint_ref", checkpoint_ref_to_yojson checkpoint_ref
      ; "checkpoint", Checkpoint.to_json checkpoint
      ]
  | Delta_checkpoint { base; target; delta } ->
    `Assoc
      [ "kind", `String "delta_checkpoint"
      ; "base", checkpoint_ref_to_yojson base
      ; "target", checkpoint_ref_to_yojson target
      ; "delta", Checkpoint.delta_to_json delta
      ]
;;

let checkpoint_delta_projection_to_yojson projection =
  `Assoc
    [ "schema_version", `Int Runtime_sync.schema_version_current
    ; "projection", `String "checkpoint_delta_v1"
    ; "entries", `List (List.map checkpoint_delta_entry_to_yojson projection.entries)
    ; "failures", `List (List.map failure_to_yojson projection.failures)
    ]
;;

let checkpoint_failure (ref_ : checkpoint_ref) detail : Runtime_store.run_load_failure =
  { session_id = ref_.session_id; path = ref_.path; detail }
;;

let checkpoint_refs_from_events events =
  events
  |> List.filter_map (fun (record : Runtime_store.run_event_record) ->
    match record.event.kind with
    | Runtime.Checkpoint_saved { label; path } ->
      Some { session_id = record.session_id; event_seq = record.event.seq; label; path }
    | _ -> None)
;;

let dedupe_checkpoint_refs refs =
  let seen = Hashtbl.create (List.length refs) in
  refs
  |> List.filter (fun (ref_ : checkpoint_ref) ->
    if Hashtbl.mem seen ref_.path
    then false
    else (
      Hashtbl.replace seen ref_.path ();
      true))
;;

let dedupe_failures failures =
  let seen = Hashtbl.create (List.length failures) in
  failures
  |> List.filter (fun (failure : Runtime_store.run_load_failure) ->
    let key = failure.session_id ^ "\x00" ^ failure.path ^ "\x00" ^ failure.detail in
    if Hashtbl.mem seen key
    then false
    else (
      Hashtbl.replace seen key ();
      true))
  |> List.sort (fun (left : Runtime_store.run_load_failure) right ->
    match String.compare left.session_id right.session_id with
    | 0 ->
      (match String.compare left.path right.path with
       | 0 -> String.compare left.detail right.detail
       | order -> order)
    | order -> order)
;;

let load_checkpoint_ref ref_ =
  match Runtime_store.load_text ref_.path with
  | Error err -> Error (checkpoint_failure ref_ (Error.to_string err))
  | Ok raw ->
    (match Checkpoint.of_string raw with
     | Ok checkpoint -> Ok { checkpoint_ref = ref_; checkpoint }
     | Error err -> Error (checkpoint_failure ref_ (Error.to_string err)))
;;

let checkpoint_delta_entries records =
  match records with
  | [] -> []
  | first :: rest ->
    let _, rev_entries =
      List.fold_left
        (fun (base, entries) target ->
           let delta = Checkpoint.compute_delta base.checkpoint target.checkpoint in
           ( target
           , Delta_checkpoint
               { base = base.checkpoint_ref; target = target.checkpoint_ref; delta }
             :: entries ))
        ( first
        , [ Full_checkpoint
              { checkpoint_ref = first.checkpoint_ref; checkpoint = first.checkpoint }
          ] )
        rest
    in
    List.rev rev_entries
;;

let checkpoint_delta_projection_from_store store selectors =
  let* selected = Runtime_store.read_window_events store selectors in
  let refs = selected.events |> checkpoint_refs_from_events |> dedupe_checkpoint_refs in
  let records, checkpoint_failures =
    List.fold_left
      (fun (records, failures) ref_ ->
         match load_checkpoint_ref ref_ with
         | Ok record -> record :: records, failures
         | Error failure -> records, failure :: failures)
      ([], [])
      refs
  in
  Ok
    { entries = checkpoint_delta_entries (List.rev records)
    ; failures = dedupe_failures (selected.failures @ checkpoint_failures)
    }
;;

let checkpoint_delta_projection_json_from_store store selectors =
  let* projection = checkpoint_delta_projection_from_store store selectors in
  Ok (checkpoint_delta_projection_to_yojson projection)
;;
