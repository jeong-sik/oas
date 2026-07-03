(** See [wire_capture.mli]. *)

let env_dir = "OAS_WIRE_CAPTURE_DIR"
let env_max_bytes = "OAS_WIRE_CAPTURE_MAX_BYTES"
let capture_filename = "raw-stream.jsonl"
let default_max_bytes = 64 * 1024 * 1024

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

let append_mutex = Eio.Mutex.create ()

let with_append_mutex f =
  Eio.Mutex.lock append_mutex;
  Fun.protect f ~finally:(fun () -> Eio.Mutex.unlock append_mutex)
;;

let close_noerr fd =
  try Unix.close fd with
  | Unix.Unix_error _ -> ()
;;

let unlock_noerr fd =
  try Unix.lockf fd Unix.F_ULOCK 0 with
  | Unix.Unix_error _ -> ()
;;

let with_file_lock ~lock_path f =
  let fd = Unix.openfile lock_path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600 in
  let locked = ref false in
  Fun.protect
    ~finally:(fun () ->
      if !locked then unlock_noerr fd;
      close_noerr fd)
    (fun () ->
       Unix.lockf fd Unix.F_LOCK 0;
       locked := true;
       f ())
;;

let rec write_all fd line offset remaining =
  if remaining > 0
  then (
    match Unix.write_substring fd line offset remaining with
    | 0 -> raise (Sys_error "write returned 0")
    | written -> write_all fd line (offset + written) (remaining - written))
;;

let append_json_line_unlocked ~path line =
  let fd =
    Unix.openfile
      path
      [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT; Unix.O_CLOEXEC ]
      0o600
  in
  Fun.protect
    ~finally:(fun () -> close_noerr fd)
    (fun () -> write_all fd line 0 (String.length line))
;;

let file_size path =
  try (Unix.stat path).Unix.st_size with
  | Unix.Unix_error _ | Sys_error _ -> 0
;;

let capture_max_bytes ?getenv () =
  match Cli_common_env.get ?getenv env_max_bytes with
  | None -> default_max_bytes, None
  | Some "" -> default_max_bytes, None
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> n, None
     | Some _ | None -> default_max_bytes, Some s)
;;

let unlink_if_exists path =
  try Ok (Unix.unlink path) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
  | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg)
;;

let path_exists path =
  try
    ignore (Unix.stat path : Unix.stats);
    true
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | Unix.Unix_error _ | Sys_error _ -> true
;;

(** Rotate [path] to [path ^ ".1"], deleting any previous backup. Failures are
    surfaced to the caller so capture can skip instead of exceeding the cap. *)
let rotate_file path =
  let backup = path ^ ".1" in
  match unlink_if_exists backup with
  | Error _ as err -> err
  | Ok () ->
    if not (path_exists path)
    then Ok ()
    else (
      try Ok (Unix.rename path backup) with
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok ()
      | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg))
;;

let append_bounded_json_line ~path ~max_bytes line =
  with_append_mutex (fun () ->
    with_file_lock ~lock_path:(path ^ ".lock") (fun () ->
      let line_bytes = String.length line in
      let current_size = file_size path in
      if current_size + line_bytes > max_bytes
      then (
        match rotate_file path with
        | Error _ as err -> err
        | Ok () -> Ok (append_json_line_unlocked ~path line))
      else Ok (append_json_line_unlocked ~path line)))
;;

let write_line ~path ~provider ~model ~warned ~oversized_warned ~max_bytes chunk =
  let json : Yojson.Safe.t =
    `Assoc
      [ "provider", `String provider
      ; "model", `String model
      ; "chunk", `String (Secret_redactor.redact_string chunk)
      ]
  in
  let line = Yojson.Safe.to_string json ^ "\n" in
  let line_bytes = String.length line in
  if line_bytes > max_bytes
  then (
    if not !oversized_warned
    then (
      oversized_warned := true;
      Diag.warn
        "wire_capture"
        "skipped capture chunk for %S: encoded JSON line is %d bytes, exceeding cap %d \
         bytes"
        path
        line_bytes
        max_bytes))
  else (
    try
      match append_bounded_json_line ~path ~max_bytes line with
      | Ok () -> ()
      | Error msg ->
        if not !warned
        then (
          warned := true;
          Diag.warn "wire_capture" "skipped capture write for %S: %s" path msg)
    with
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
          (unix_error_message err fn arg)))
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
       let oversized_warned = ref false in
       let max_bytes, invalid_max_bytes = capture_max_bytes ?getenv () in
       (match invalid_max_bytes with
        | None -> ()
        | Some value ->
          Diag.warn
            "wire_capture"
            "%s=%S is invalid; using default cap %d bytes"
            env_max_bytes
            value
            default_max_bytes);
       fun chunk ->
         write_line ~path ~provider ~model ~warned ~oversized_warned ~max_bytes chunk)
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
  Eio_main.run (fun _env ->
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
    && contains ~needle:"deepseek-v4-flash" content)
;;

let%test "multiple active sinks append complete JSONL lines" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire_multi" "" in
    let s1 = make_sink ~getenv:(capture_getenv dir) ~provider:"p1" ~model:"m1" in
    let s2 = make_sink ~getenv:(capture_getenv dir) ~provider:"p2" ~model:"m2" in
    s1 (String.make 4096 'a');
    s2 (String.make 4096 'b');
    let content = read_file (Filename.concat dir capture_filename) in
    let lines =
      String.split_on_char '\n' content |> List.filter (fun line -> line <> "")
    in
    match lines with
    | [ line1; line2 ] ->
      contains ~needle:"\"provider\":\"p1\"" line1
      && contains ~needle:"\"provider\":\"p2\"" line2
    | _ -> false)
;;

let%test "make_sink disables capture when env path is a file" =
  Eio_main.run (fun _env ->
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
      !warnings)
;;

let%test "disabled sink writes nothing" =
  let dir = Filename.temp_dir "oas_wire_off" "" in
  let s = make_sink ~getenv:(fun _ -> None) ~provider:"p" ~model:"m" in
  with_cwd dir (fun () ->
    s "chunk";
    not (Sys.file_exists capture_filename))
;;

let%test "capture mutex does not block Eio fiber scheduling" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let progress = ref false in
    let observed_by_holder = ref false in
    let observed_by_waiter = ref false in
    Eio.Fiber.all
      [ (fun () ->
          with_append_mutex (fun () ->
            (* Simulate slow capture I/O while holding the shared lock. *)
            Eio.Time.sleep clock 0.2;
            observed_by_holder := !progress))
      ; (fun () ->
          Eio.Time.sleep clock 0.05;
          progress := true)
      ; (fun () ->
          Eio.Time.sleep clock 0.01;
          with_append_mutex (fun () -> observed_by_waiter := !progress))
      ];
    !observed_by_holder && !observed_by_waiter)
;;

let%test "make_sink rotates file when max bytes would be exceeded" =
  let dir = Filename.temp_dir "oas_wire_rotate" "" in
  let max_bytes = 128 in
  let s =
    make_sink
      ~getenv:(fun name ->
        if String.equal name env_dir
        then Some dir
        else if String.equal name env_max_bytes
        then Some (string_of_int max_bytes)
        else None)
      ~provider:"p"
      ~model:"m"
  in
  s (String.make 64 'a');
  s (String.make 64 'b');
  let path = Filename.concat dir capture_filename in
  let backup = path ^ ".1" in
  Sys.file_exists backup
  && Sys.file_exists path
  &&
  let content = read_file path in
  contains ~needle:"\"chunk\":\"" content
;;

let%test "make_sink skips oversized records instead of exceeding max bytes" =
  let dir = Filename.temp_dir "oas_wire_oversized" "" in
  let warnings = ref [] in
  let max_bytes = 128 in
  Diag.with_sink
    (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
    (fun () ->
       let s =
         make_sink
           ~getenv:(fun name ->
             if String.equal name env_dir
             then Some dir
             else if String.equal name env_max_bytes
             then Some (string_of_int max_bytes)
             else None)
           ~provider:"p"
           ~model:"m"
       in
       s (String.make 1024 'x'));
  let path = Filename.concat dir capture_filename in
  (not (Sys.file_exists path))
  && List.exists
       (fun (level, ctx, msg) ->
          level = Diag.Warn
          && String.equal ctx "wire_capture"
          && contains ~needle:"skipped capture chunk" msg)
       !warnings
;;

let%test "make_sink skips when rotation cannot preserve cap" =
  let dir = Filename.temp_dir "oas_wire_rotate_fail" "" in
  let warnings = ref [] in
  let max_bytes = 256 in
  let path = Filename.concat dir capture_filename in
  let backup = path ^ ".1" in
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc (String.make 250 'a'));
  Unix.mkdir backup 0o700;
  Diag.with_sink
    (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
    (fun () ->
       let s =
         make_sink
           ~getenv:(fun name ->
             if String.equal name env_dir
             then Some dir
             else if String.equal name env_max_bytes
             then Some (string_of_int max_bytes)
             else None)
           ~provider:"p"
           ~model:"m"
       in
       s "small chunk");
  file_size path <= max_bytes
  && List.exists
       (fun (level, ctx, msg) ->
          level = Diag.Warn
          && String.equal ctx "wire_capture"
          && contains ~needle:"skipped capture write" msg)
       !warnings
;;

let%test "invalid max bytes falls back to default cap with warning" =
  let dir = Filename.temp_dir "oas_wire_nocap" "" in
  let warnings = ref [] in
  let max_bytes, invalid =
    capture_max_bytes
      ~getenv:(fun name -> if String.equal name env_max_bytes then Some "0" else None)
      ()
  in
  let s =
    Diag.with_sink
      (fun level ~ctx msg -> warnings := (level, ctx, msg) :: !warnings)
      (fun () ->
         make_sink
           ~getenv:(fun name ->
             if String.equal name env_dir
             then Some dir
             else if String.equal name env_max_bytes
             then Some "0"
             else None)
           ~provider:"p"
           ~model:"m")
  in
  s "chunk";
  max_bytes = default_max_bytes
  && invalid = Some "0"
  && List.exists
       (fun (level, ctx, msg) ->
          level = Diag.Warn
          && String.equal ctx "wire_capture"
          && contains ~needle:"invalid; using default cap" msg)
       !warnings
;;
