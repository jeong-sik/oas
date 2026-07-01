(** See [wire_capture.mli]. *)

let env_dir = "OAS_WIRE_CAPTURE_DIR"
let capture_filename = "raw-stream.jsonl"

type sink = string -> unit

let noop : sink = fun _ -> ()

let unix_error_message err fn arg =
  Printf.sprintf "%s(%S): %s" fn arg (Unix.error_message err)
;;

let warn_activation_failure ~dir reason =
  Diag.warn "wire_capture" "disabled OAS wire capture for %S: %s" dir reason
;;

let require_capture_dir dir =
  try
    match (Unix.stat dir).st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> Error "path exists but is not a directory"
  with
  | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg)
;;

let ensure_capture_dir dir =
  try
    match (Unix.stat dir).st_kind with
    | Unix.S_DIR -> Ok ()
    | _ -> Error "path exists but is not a directory"
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    (try Ok (Unix.mkdir dir 0o700) with
     | Unix.Unix_error (Unix.EEXIST, _, _) -> require_capture_dir dir
     | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg))
  | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg)
;;

let append_mutex = Mutex.create ()

let with_append_mutex f =
  Mutex.lock append_mutex;
  Fun.protect f ~finally:(fun () -> Mutex.unlock append_mutex)
;;

let close_noerr fd =
  try Unix.close fd with
  | Unix.Unix_error _ -> ()
;;

let unlock_noerr fd =
  try Unix.lockf fd Unix.F_ULOCK 0 with
  | Unix.Unix_error _ -> ()
;;

let rec write_all fd line offset remaining =
  if remaining > 0
  then (
    match Unix.write_substring fd line offset remaining with
    | 0 -> raise (Sys_error "write returned 0")
    | written -> write_all fd line (offset + written) (remaining - written))
;;

let append_json_line ~path line =
  with_append_mutex (fun () ->
    let fd =
      Unix.openfile
        path
        [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT; Unix.O_CLOEXEC ]
        0o600
    in
    let locked = ref false in
    Fun.protect
      ~finally:(fun () ->
        if !locked then unlock_noerr fd;
        close_noerr fd)
      (fun () ->
         ignore (Unix.lseek fd 0 Unix.SEEK_SET : int);
         Unix.lockf fd Unix.F_TLOCK 0;
         locked := true;
         write_all fd line 0 (String.length line)))
;;

let write_line ~path ~provider ~model ~warned chunk =
  let json : Yojson.Safe.t =
    `Assoc
      [ "provider", `String provider
      ; "model", `String model
      ; "chunk", `String (Secret_redactor.redact_string chunk)
      ]
  in
  let line = Yojson.Safe.to_string json ^ "\n" in
  try append_json_line ~path line with
  | Sys_error msg ->
    if not !warned
    then (
      warned := true;
      Diag.warn "wire_capture" "write failed for %S: %s" path msg)
  | Unix.Unix_error (err, fn, arg) ->
    if not !warned
    then (
      warned := true;
      Diag.warn
        "wire_capture"
        "write failed for %S: %s"
        path
        (unix_error_message err fn arg))
;;

let make_sink ?getenv ~provider ~model =
  match Cli_common_env.get ?getenv env_dir with
  | None -> noop
  | Some dir ->
    (match ensure_capture_dir dir with
     | Error reason ->
       warn_activation_failure ~dir reason;
       noop
     | Ok () ->
       let path = Filename.concat dir capture_filename in
       let warned = ref false in
       fun chunk -> write_line ~path ~provider ~model ~warned chunk)
;;

(* ── Inline tests ─────────────────────────────────────────────── *)

let getenv_of_pairs pairs name = List.assoc_opt name pairs
let capture_getenv dir name = if String.equal name env_dir then Some dir else None

let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  if nl = 0
  then true
  else (
    let rec loop i =
      i + nl <= hl && (String.equal (String.sub haystack i nl) needle || loop (i + 1))
    in
    loop 0)
;;

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))
;;

let with_cwd dir f =
  let original = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir original)
    (fun () ->
       Sys.chdir dir;
       f ())
;;

let%test "make_sink is a no-op when env is unset or empty" =
  let unset = make_sink ~getenv:(fun _ -> None) ~provider:"p" ~model:"m" in
  let empty =
    make_sink ~getenv:(getenv_of_pairs [ env_dir, "   " ]) ~provider:"p" ~model:"m"
  in
  unset "raw chunk";
  empty "raw chunk";
  (* no exception, no output path assumed *)
  true
;;

let%test "make_sink writes redacted binary JSONL when env is set" =
  let dir = Filename.temp_dir "oas_wire" "" in
  let s =
    make_sink
      ~getenv:(capture_getenv dir)
      ~provider:"ollama_cloud"
      ~model:"deepseek-v4-flash"
  in
  (* Built at runtime so no literal secret appears in source. *)
  let token = "ghp_" ^ String.make 36 '7' in
  s ("delta content " ^ token ^ " end");
  let path = Filename.concat dir capture_filename in
  let content = read_file path in
  (not (contains ~needle:token content))
  && contains ~needle:"[REDACTED]" content
  && contains ~needle:"deepseek-v4-flash" content
;;

let%test "multiple active sinks append complete JSONL lines" =
  let dir = Filename.temp_dir "oas_wire_multi" "" in
  let s1 = make_sink ~getenv:(capture_getenv dir) ~provider:"p1" ~model:"m1" in
  let s2 = make_sink ~getenv:(capture_getenv dir) ~provider:"p2" ~model:"m2" in
  s1 (String.make 4096 'a');
  s2 (String.make 4096 'b');
  let content = read_file (Filename.concat dir capture_filename) in
  let lines = String.split_on_char '\n' content |> List.filter (fun line -> line <> "") in
  match lines with
  | [ line1; line2 ] ->
    contains ~needle:"\"provider\":\"p1\"" line1
    && contains ~needle:"\"provider\":\"p2\"" line2
  | _ -> false
;;

let%test "make_sink disables capture when env path is a file" =
  let path = Filename.temp_file "oas_wire_file" ".txt" in
  let warnings = ref [] in
  let s =
    Diag.with_sink
      (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
      (fun () -> make_sink ~getenv:(capture_getenv path) ~provider:"p" ~model:"m")
  in
  s "chunk";
  List.exists
    (fun (level, ctx, msg) ->
       level = Diag.Warn
       && String.equal ctx "wire_capture"
       && contains ~needle:"not a directory" msg)
    !warnings
;;

let%test "disabled sink writes nothing" =
  let dir = Filename.temp_dir "oas_wire_off" "" in
  let s = make_sink ~getenv:(fun _ -> None) ~provider:"p" ~model:"m" in
  with_cwd dir (fun () ->
    s "chunk";
    not (Sys.file_exists capture_filename))
;;
