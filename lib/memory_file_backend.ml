(** File-backed long-term memory persistence.

    Follows {!Checkpoint_store} patterns: Eio.Path, atomic tmp+rename.

    @since 0.102.0 *)

type t = { base_dir: Eio.Fs.dir_ty Eio.Path.t }

(* ── Key encoding ────────────────────────────────────────────── *)

(** Encode a key as hex string for filesystem safety.
    Avoids special chars, path traversal, and length issues. *)
let hex_encode s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
  ) s;
  Buffer.contents buf

let hex_decode s =
  let len = String.length s in
  if len mod 2 <> 0 then None
  else
    try
      let buf = Buffer.create (len / 2) in
      for i = 0 to (len / 2) - 1 do
        let hex = String.sub s (i * 2) 2 in
        Buffer.add_char buf (Char.chr (int_of_string ("0x" ^ hex)))
      done;
      Some (Buffer.contents buf)
    with _ -> None

let file_path store key =
  Eio.Path.(store.base_dir / (hex_encode key ^ ".json"))

let tmp_path store key =
  Eio.Path.(store.base_dir / (hex_encode key ^ ".json.tmp"))

let io_error_of_exn = Fs_result.io_error_of_exn

(* ── Lifecycle ───────────────────────────────────────────────── *)

let create base_dir =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 base_dir;
    Ok { base_dir }
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> io_error_of_exn ~op:"create" ~path:"memory_dir" exn

(* ── Core operations ─────────────────────────────────────────── *)

let persist t ~key value =
  let data = Yojson.Safe.to_string value in
  let tmp = tmp_path t key in
  let target = file_path t key in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) tmp data;
    Eio.Path.rename tmp target;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    (try Eio.Path.unlink tmp with Eio.Io _ | Unix.Unix_error _ -> ());
    match io_error_of_exn ~op:"persist" ~path:key exn with
    | Error (Error.Io _) -> Error (Printf.sprintf "persist '%s' failed: %s" key (Printexc.to_string exn))
    | _ -> Error (Printf.sprintf "persist '%s' failed: %s" key (Printexc.to_string exn))

let retrieve t ~key =
  let path = file_path t key in
  try
    let data = Eio.Path.load path in
    Some (Yojson.Safe.from_string data)
  with
  | _ -> None

let remove t ~key =
  let path = file_path t key in
  try
    Eio.Path.unlink path;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok ()
  | exn ->
    Error (Printf.sprintf "remove '%s' failed: %s" key (Printexc.to_string exn))

let batch_persist t pairs =
  (* Best-effort: individual files are atomic, batch is not transactional *)
  let errors = List.filter_map (fun (key, value) ->
    match persist t ~key value with
    | Ok () -> None
    | Error reason -> Some reason
  ) pairs in
  match errors with
  | [] -> Ok ()
  | errs -> Error (String.concat "; " errs)

let query t ~prefix ~limit =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    entries
    |> List.filter_map (fun name ->
      let len = String.length name in
      if len > 5 && String.sub name (len - 5) 5 = ".json"
         && not (len > 9 && String.sub name (len - 9) 9 = ".json.tmp")
      then
        let hex = String.sub name 0 (len - 5) in
        match hex_decode hex with
        | Some key when String.length key >= String.length prefix
                     && String.sub key 0 (String.length prefix) = prefix ->
          (match retrieve t ~key with
           | Some value -> Some (key, value)
           | None -> None)
        | _ -> None
      else None)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> (fun lst ->
      if limit > 0 then
        let rec take n acc = function
          | [] -> List.rev acc
          | _ when n <= 0 -> List.rev acc
          | x :: xs -> take (n - 1) (x :: acc) xs
        in
        take limit [] lst
      else lst)
  with _ -> []

(* ── Backend conversion ──────────────────────────────────────── *)

let to_backend t : Memory.long_term_backend =
  { persist = persist t;
    retrieve = retrieve t;
    remove = remove t;
    batch_persist = batch_persist t;
    query = query t;
  }

(* ── Utility ─────────────────────────────────────────────────── *)

let keys t =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    entries
    |> List.filter_map (fun name ->
      let len = String.length name in
      if len > 5 && String.sub name (len - 5) 5 = ".json"
         && not (len > 9 && String.sub name (len - 9) 9 = ".json.tmp")
      then
        hex_decode (String.sub name 0 (len - 5))
      else None)
    |> List.sort String.compare
  with _ -> []

let entry_count t = List.length (keys t)

let clear t =
  try
    let entries = Eio.Path.read_dir t.base_dir in
    List.iter (fun name ->
      let path = Eio.Path.(t.base_dir / name) in
      (try Eio.Path.unlink path with _ -> ())
    ) entries;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn ->
    match io_error_of_exn ~op:"clear" ~path:"memory_dir" exn with
    | Error _ -> Error (Error.Io (FileOpFailed { op = "clear"; path = "memory_dir"; detail = Printexc.to_string exn }))
    | Ok () -> Ok ()
