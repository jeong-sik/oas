open Base
(** File-backed A2A task persistence using Eio.Path.

    Layout: [<base_dir>/<task_id>.json].
    Atomic writes via {!Fs_atomic_eio.save_atomic}: unique tmp + fsync + rename.
    In-memory Hashtbl cache for fast lookups; file I/O for durability. *)

let _log = Log.create ~module_name:"a2a_task_store" ()

type t =
  { base_dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache : (A2a_task.task_id, A2a_task.task) Hashtbl.t
  }

(** Validate task_id: reject empty, containing '/', containing '\000'. *)
let validate_task_id id =
  if String.length id = 0
  then Error (Error.Io (ValidationFailed { detail = "task_id must not be empty" }))
  else if String.contains id '/'
  then Error (Error.Io (ValidationFailed { detail = "task_id must not contain '/'" }))
  else if String.contains id '\000'
  then
    Error (Error.Io (ValidationFailed { detail = "task_id must not contain null byte" }))
  else Ok ()
;;

let io_error_of_exn = Fs_result.io_error_of_exn
let file_path store id = Eio.Path.(store.base_dir / (id ^ ".json"))

let create base_dir =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 base_dir;
    Ok { base_dir; cache = Hashtbl.create 64 }
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> io_error_of_exn ~op:"create" ~path:"task_store_dir" exn
;;

let store_task store (task : A2a_task.task) =
  match validate_task_id task.id with
  | Error e -> Error e
  | Ok () ->
    let json = A2a_task.task_to_yojson task in
    let data = Yojson.Safe.to_string json in
    (match
       Fs_atomic_eio.save_atomic ~dir:store.base_dir ~name:(task.id ^ ".json") data
     with
     | Ok () ->
       Hashtbl.replace store.cache task.id task;
       Ok ()
     | Error _ as e -> e)
;;

let get_task store id = Hashtbl.find_opt store.cache id
let list_tasks store = Hashtbl.fold (fun _ task acc -> task :: acc) store.cache []

let delete_task store id =
  match validate_task_id id with
  | Error e -> Error e
  | Ok () ->
    let path = file_path store id in
    (try
       Eio.Path.unlink path;
       Hashtbl.remove store.cache id;
       Ok ()
     with
     | Eio.Cancel.Cancelled _ as e -> raise e
     | exn -> io_error_of_exn ~op:"delete_task" ~path:id exn)
;;

(** Reload all tasks from disk into the cache.
    Skips files that fail to parse (corrupted JSON). *)
let reload store =
  try
    Hashtbl.clear store.cache;
    let entries = Eio.Path.read_dir store.base_dir in
    let json_files =
      entries
      |> List.filter (fun name ->
        let len = String.length name in
        (* Tmp files now use writer-unique suffix ending in
                ".tmp"; match that directly. *)
        len > 5
        && String.sub name (len - 5) 5 = ".json"
        && not (len > 4 && String.sub name (len - 4) 4 = ".tmp"))
    in
    List.iter
      (fun filename ->
         let path = Eio.Path.(store.base_dir / filename) in
         try
           let data = Eio.Path.load path in
           let json = Yojson.Safe.from_string data in
           match A2a_task.task_of_yojson json with
           | Ok task -> Hashtbl.replace store.cache task.id task
           | Error e ->
             Log.warn
               _log
               "skipping corrupted task file during reload"
               [ Log.S ("file", filename); Log.S ("error", e) ]
         with
         | (Eio.Io _ | Yojson.Json_error _) as exn ->
           Log.warn
             _log
             "skipping unreadable task file during reload"
             [ Log.S ("file", filename); Log.S ("error", Printexc.to_string exn) ])
      json_files;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> io_error_of_exn ~op:"reload" ~path:"task_store_dir" exn
;;

(** Garbage-collect terminal tasks older than [max_age_s] seconds.
    Returns the number of tasks removed. *)
let gc ?(max_age_s = 86400.0) store =
  let now = Unix.gettimeofday () in
  let to_remove =
    Hashtbl.fold
      (fun id (task : A2a_task.task) acc ->
         if A2a_task.is_terminal task.state && now -. task.updated_at > max_age_s
         then id :: acc
         else acc)
      store.cache
      []
  in
  let errors =
    List.filter_map
      (fun id ->
         match delete_task store id with
         | Ok () -> None
         | Error e ->
           Log.warn
             _log
             "gc failed to delete task"
             [ Log.S ("task_id", id); Log.S ("error", Error.to_string e) ];
           Some id)
      to_remove
  in
  let removed = List.length to_remove - List.length errors in
  if errors = []
  then Ok removed
  else (
    let detail =
      Printf.sprintf
        "gc removed %d tasks, %d failed to delete"
        removed
        (List.length errors)
    in
    Error (Error.Io (FileOpFailed { op = "gc"; path = "task_store_dir"; detail })))
;;
