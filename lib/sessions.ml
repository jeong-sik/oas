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

let ( let* ) = Result.bind

let make_store ?session_root () =
  Runtime_store.create ?root:session_root ()

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
