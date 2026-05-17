(** File-backed long-term memory persistence.

    Follows {!Checkpoint_store} patterns: Eio.Path, atomic tmp+rename.

    @since 0.102.0 *)

type t = { base_dir : Eio.Fs.dir_ty Eio.Path.t }

let _log = Log.create ~module_name:"memory_file_backend" ()

let warn_backend_issue ~op ~path exn =
  Log.warn
    _log
    "memory backend operation failed"
    [ Log.S ("op", op); Log.S ("path", path); Log.S ("error", Printexc.to_string exn) ]
;;

(* ── Key encoding ────────────────────────────────────────────── *)

(** Encode a key as a hex string for filesystem safety.
    Avoids special characters and path traversal in filenames, but does
    not remove filesystem filename length limits (hex encoding doubles
    the key length). *)
let hex_encode s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents buf
;;

let hex_decode s =
  let len = String.length s in
  if len mod 2 <> 0
  then None
  else (
    try
      let buf = Buffer.create (len / 2) in
      for i = 0 to (len / 2) - 1 do
        let hex = String.sub s (i * 2) 2 in
        Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ hex)))
      done;
      Some (Buffer.contents buf)
    with
    | _ -> None)
;;

let file_path store key = Eio.Path.(store.base_dir / (hex_encode key ^ ".json"))
let io_error_of_exn = Fs_result.io_error_of_exn

type retrieve_error =
  | Missing_key
  | Corrupt_json of string
  | Backend_error of string

let retrieve_error_to_string = function
  | Missing_key -> "missing_key"
  | Corrupt_json reason -> "corrupt_json: " ^ reason
  | Backend_error reason -> "backend_error: " ^ reason
;;

(* ── Lifecycle ───────────────────────────────────────────────── *)

let create base_dir =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 base_dir;
    Ok { base_dir }
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> io_error_of_exn ~op:"create" ~path:"memory_dir" exn
;;

(* ── Core operations ─────────────────────────────────────────── *)

let persist t ~key value =
  let data = Yojson.Safe.to_string value in
  let name = hex_encode key ^ ".json" in
  match Fs_atomic_eio.save_atomic ~dir:t.base_dir ~name data with
  | Ok () -> Ok ()
  | Error e -> Error (Printf.sprintf "persist '%s' failed: %s" key (Error.to_string e))
;;

let retrieve_result t ~key =
  let path = file_path t key in
  try
    let data = Eio.Path.load path in
    Ok (Yojson.Safe.from_string data)
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> Error Missing_key
  | Yojson.Json_error reason -> Error (Corrupt_json reason)
  | Eio.Io _ as exn -> Error (Backend_error (Printexc.to_string exn))
  | Unix.Unix_error _ as exn -> Error (Backend_error (Printexc.to_string exn))
;;

let retrieve t ~key =
  match retrieve_result t ~key with
  | Ok json -> Some json
  | Error Missing_key -> None
  | Error (Corrupt_json _ as err) ->
    warn_backend_issue
      ~op:"retrieve_parse"
      ~path:key
      (Failure (retrieve_error_to_string err));
    None
  | Error (Backend_error _ as err) ->
    warn_backend_issue ~op:"retrieve" ~path:key (Failure (retrieve_error_to_string err));
    None
;;

let remove t ~key =
  let path = file_path t key in
  try
    Eio.Path.unlink path;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok ()
  | exn -> Error (Printf.sprintf "remove '%s' failed: %s" key (Printexc.to_string exn))
;;

let batch_persist t pairs =
  (* Best-effort: individual files are atomic, batch is not transactional *)
  let errors =
    List.filter_map
      (fun (key, value) ->
         match persist t ~key value with
         | Ok () -> None
         | Error reason -> Some reason)
      pairs
  in
  match errors with
  | [] -> Ok ()
  | errs -> Error (String.concat "; " errs)
;;

let query t ~prefix ~limit =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    entries
    |> List.filter_map (fun name ->
      let len = String.length name in
      (* Writer-unique tmp names end with ".tmp"; match that suffix
         directly (see Fs_atomic_eio.save_atomic). *)
      if
        len > 5
        && String.sub name (len - 5) 5 = ".json"
        && not (len > 4 && String.sub name (len - 4) 4 = ".tmp")
      then (
        let hex = String.sub name 0 (len - 5) in
        match hex_decode hex with
        | Some key
          when String.length key >= String.length prefix
               && String.sub key 0 (String.length prefix) = prefix ->
          (match retrieve t ~key with
           | Some value -> Some (key, value)
           | None -> None)
        | _ -> None)
      else None)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> fun lst ->
    if limit > 0
    then (
      let rec take n acc = function
        | [] -> List.rev acc
        | _ when n <= 0 -> List.rev acc
        | x :: xs -> take (n - 1) (x :: acc) xs
      in
      take limit [] lst)
    else lst
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    warn_backend_issue ~op:"query" ~path:prefix exn;
    []
;;

(* ── Backend conversion ──────────────────────────────────────── *)

let to_backend t : Memory.long_term_backend =
  let retrieve_result_as_memory_error ~key =
    match retrieve_result t ~key with
    | Ok value -> Ok value
    | Error Missing_key -> Error Memory.Missing_key
    | Error (Corrupt_json reason) ->
      Error (Memory.Backend_error ("corrupt_json: " ^ reason))
    | Error (Backend_error reason) -> Error (Memory.Backend_error reason)
  in
  { persist = persist t
  ; retrieve = retrieve t
  ; retrieve_result = retrieve_result_as_memory_error
  ; remove = remove t
  ; batch_persist = batch_persist t
  ; query = query t
  }
;;

let episodic_prefix = "ep:"
let procedural_prefix = "pr:"
let episodic_key id = episodic_prefix ^ id
let procedural_key id = procedural_prefix ^ id

let warn_callback_error ~op ~key reason =
  warn_backend_issue ~op ~path:key (Failure reason)
;;

let to_episodic_backend t : Memory.episodic_backend =
  { persist_episode =
      (fun ep ->
        let key = episodic_key ep.id in
        match persist t ~key (Memory_episodic.episode_to_json ep) with
        | Ok () -> ()
        | Error reason -> warn_callback_error ~op:"persist_episode" ~key reason)
  ; retrieve_episode =
      (fun ~id ->
        match retrieve t ~key:(episodic_key id) with
        | Some json -> Memory_episodic.episode_of_json json
        | None -> None)
  ; remove_episode =
      (fun ~id ->
        let key = episodic_key id in
        match remove t ~key with
        | Ok () -> ()
        | Error reason -> warn_callback_error ~op:"remove_episode" ~key reason)
  ; all_episodes =
      (fun () ->
        query t ~prefix:episodic_prefix ~limit:0
        |> List.filter_map (fun (_, json) -> Memory_episodic.episode_of_json json))
  }
;;

let to_procedural_backend t : Memory.procedural_backend =
  { persist_procedure =
      (fun proc ->
        let key = procedural_key proc.id in
        match persist t ~key (Memory_procedural.procedure_to_json proc) with
        | Ok () -> ()
        | Error reason -> warn_callback_error ~op:"persist_procedure" ~key reason)
  ; retrieve_procedure =
      (fun ~id ->
        match retrieve t ~key:(procedural_key id) with
        | Some json -> Memory_procedural.procedure_of_json json
        | None -> None)
  ; remove_procedure =
      (fun ~id ->
        let key = procedural_key id in
        match remove t ~key with
        | Ok () -> ()
        | Error reason -> warn_callback_error ~op:"remove_procedure" ~key reason)
  ; all_procedures =
      (fun () ->
        query t ~prefix:procedural_prefix ~limit:0
        |> List.filter_map (fun (_, json) -> Memory_procedural.procedure_of_json json))
  }
;;

(* ── Utility ─────────────────────────────────────────────────── *)

let keys t =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    entries
    |> List.filter_map (fun name ->
      let len = String.length name in
      if
        len > 5
        && String.sub name (len - 5) 5 = ".json"
        && not (len > 4 && String.sub name (len - 4) 4 = ".tmp")
      then hex_decode (String.sub name 0 (len - 5))
      else None)
    |> List.sort String.compare
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    warn_backend_issue ~op:"keys" ~path:"memory_dir" exn;
    []
;;

let entry_count t = List.length (keys t)

let clear t =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    let failures =
      List.filter_map
        (fun name ->
           let path = Eio.Path.(t.base_dir / name) in
           try
             Eio.Path.unlink path;
             None
           with
           | Eio.Cancel.Cancelled _ as e -> raise e
           | exn ->
             warn_backend_issue ~op:"clear_entry" ~path:name exn;
             Some (Printf.sprintf "%s: %s" name (Printexc.to_string exn)))
        entries
    in
    match failures with
    | [] -> Ok ()
    | errs ->
      Error
        (Error.Io
           (FileOpFailed
              { op = "clear"; path = "memory_dir"; detail = String.concat "; " errs }))
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    (match io_error_of_exn ~op:"clear" ~path:"memory_dir" exn with
     | Error _ ->
       Error
         (Error.Io
            (FileOpFailed
               { op = "clear"; path = "memory_dir"; detail = Printexc.to_string exn }))
     | Ok () -> Ok ())
;;
