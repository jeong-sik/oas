(** Result-based filesystem operations.

    Normalizes [Eio.Io], [Unix.Unix_error], [Sys_error], and [Failure]
    into [Error.Io (FileOpFailed ...)].
    Pattern extracted from checkpoint_store.ml / a2a_task_store.ml. *)

let ( let* ) = Result.bind

let io_error_of_exn ~op ~path = function
  | Eio.Io _ as exn ->
    Error (Error.Io (FileOpFailed { op; path; detail = Printexc.to_string exn }))
  | Unix.Unix_error _ as exn ->
    Error (Error.Io (FileOpFailed { op; path; detail = Printexc.to_string exn }))
  | Sys_error detail ->
    Error (Error.Io (FileOpFailed { op; path; detail }))
  | Failure msg ->
    Error (Error.Io (FileOpFailed { op; path; detail = msg }))
  | Yojson.Json_error msg ->
    Error (Error.Io (FileOpFailed { op; path; detail = "JSON error: " ^ msg }))
  | exn -> raise exn

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with exn -> io_error_of_exn ~op:"read" ~path exn

let ensure_dir path =
  try
    if not (Sys.file_exists path) then
      Sys.mkdir path 0o755;
    Ok ()
  with exn -> io_error_of_exn ~op:"mkdir" ~path exn

let ensure_dir_recursive path =
  let rec aux p =
    if Sys.file_exists p then ()
    else begin
      aux (Filename.dirname p);
      (try Sys.mkdir p 0o755 with Sys_error _ -> ())
    end
  in
  try aux path; Ok ()
  with exn -> io_error_of_exn ~op:"mkdir_p" ~path exn

let write_file path content =
  try
    let* () = ensure_dir_recursive (Filename.dirname path) in
    let tmp_path = path ^ ".tmp" in
    Out_channel.with_open_bin tmp_path (fun oc ->
      Out_channel.output_string oc content);
    Sys.rename tmp_path path;
    Ok ()
  with exn -> io_error_of_exn ~op:"write" ~path exn

let append_file path content =
  try
    let* () = ensure_dir_recursive (Filename.dirname path) in
    let oc = open_out_gen [ Open_append; Open_creat; Open_binary ] 0o644 path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc content);
    Ok ()
  with exn -> io_error_of_exn ~op:"append" ~path exn

let read_dir path =
  try Ok (Sys.readdir path |> Array.to_list |> List.sort String.compare)
  with exn -> io_error_of_exn ~op:"read_dir" ~path exn

let file_exists path =
  try Sys.file_exists path && not (Sys.is_directory path)
  with Sys_error _ -> false

let remove_file path =
  try
    if Sys.file_exists path then Sys.remove path;
    Ok ()
  with exn -> io_error_of_exn ~op:"remove" ~path exn
