(** See [wire_capture.mli]. *)

let env_dir = "OAS_WIRE_CAPTURE_DIR"
let segment_suffix = ".jsonl"

type failure_stage =
  | Activation
  | Append
  | Writer
[@@deriving yojson, show]

type failure =
  { stage : failure_stage
  ; capture_id : string option
  ; provider : string
  ; model : string
  ; location : string
  ; message : string
  }
[@@deriving yojson, show]

type sink =
  { push_chunk : string -> unit
  ; close_sink : unit -> unit
  ; captured_failures : unit -> failure list
  }

let push sink chunk = sink.push_chunk chunk
let close sink = sink.close_sink ()
let failures sink = sink.captured_failures ()

module Fifo = struct
  type 'a t =
    { ready : 'a list
    ; incoming_rev : 'a list
    }

  let empty = { ready = []; incoming_rev = [] }
  let add item t = { t with incoming_rev = item :: t.incoming_rev }

  let take t =
    match t.ready with
    | item :: ready -> Some (item, { t with ready })
    | [] ->
      (match List.rev t.incoming_rev with
       | [] -> None
       | item :: ready -> Some (item, { ready; incoming_rev = [] }))
  ;;
end

type 'a async_fifo =
  { mutable pending : 'a Fifo.t
  ; mutable closed : bool
  ; mutex : Eio.Mutex.t
  ; changed : Eio.Condition.t
  }

let create_async_fifo () =
  { pending = Fifo.empty
  ; closed = false
  ; mutex = Eio.Mutex.create ()
  ; changed = Eio.Condition.create ()
  }
;;

let enqueue fifo item =
  let accepted =
    Eio.Mutex.use_rw ~protect:false fifo.mutex (fun () ->
      if fifo.closed
      then false
      else (
        fifo.pending <- Fifo.add item fifo.pending;
        true))
  in
  Eio.Condition.broadcast fifo.changed;
  accepted
;;

let close_fifo fifo =
  Eio.Mutex.use_rw ~protect:true fifo.mutex (fun () -> fifo.closed <- true);
  Eio.Condition.broadcast fifo.changed
;;

let rec take fifo =
  Eio.Mutex.lock fifo.mutex;
  match Fifo.take fifo.pending with
  | Some (item, pending) ->
    fifo.pending <- pending;
    Eio.Mutex.unlock fifo.mutex;
    Some item
  | None when fifo.closed ->
    Eio.Mutex.unlock fifo.mutex;
    None
  | None ->
    (match Eio.Condition.await fifo.changed fifo.mutex with
     | () ->
       Eio.Mutex.unlock fifo.mutex;
       take fifo
     | exception exn ->
       (* [Condition.await] reacquires the mutex before propagating
          cancellation, so the FIFO remains internally consistent. *)
       Eio.Mutex.unlock fifo.mutex;
       raise exn)
;;

let unix_error_message err fn arg =
  Printf.sprintf "%s(%S): %s" fn arg (Unix.error_message err)
;;

type failure_log =
  { mutable failures_rev : failure list
  ; mutex : Eio.Mutex.t
  ; on_failure : failure -> unit
  }

let create_failure_log ~on_failure =
  { failures_rev = []; mutex = Eio.Mutex.create (); on_failure }
;;

let record_failure log failure =
  Eio.Mutex.use_rw ~protect:true log.mutex (fun () ->
    log.failures_rev <- failure :: log.failures_rev);
  try log.on_failure failure with
  | exn ->
    Diag.warn
      "wire_capture"
      "wire capture failure observer raised: %s"
      (Printexc.to_string exn)
;;

let recorded_failures log =
  Eio.Mutex.use_ro log.mutex (fun () -> List.rev log.failures_rev)
;;

let disabled_sink log =
  { push_chunk = (fun _ -> ())
  ; close_sink = (fun () -> ())
  ; captured_failures = (fun () -> recorded_failures log)
  }
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
       try
         (* The lock wait runs in a system thread, so another process writing
            the same exact request segment cannot block an Agent fiber. *)
         Unix.lockf fd Unix.F_LOCK 0;
         locked := true;
         f ()
       with
       | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg))
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

let segment_filename capture_id =
  Digestif.SHA256.(digest_string capture_id |> to_hex) ^ segment_suffix
;;

let segment_path ~dir ~capture_id = Filename.concat dir (segment_filename capture_id)

let append_json_line ~path line =
  Eio_unix.run_in_systhread ~label:"wire-capture-append" (fun () ->
    with_file_lock ~lock_path:(path ^ ".lock") (fun () ->
      Ok (append_json_line_unlocked ~path line)))
;;

let write_line ~path ~capture_id ~provider ~model chunk =
  let json : Yojson.Safe.t =
    `Assoc
      [ "capture_id", `String capture_id
      ; "provider", `String provider
      ; "model", `String model
      ; "chunk", `String (Secret_redactor.redact_string chunk)
      ]
  in
  let line = Yojson.Safe.to_string json ^ "\n" in
  try
    match append_json_line ~path line with
    | Ok () -> Ok ()
    | Error msg -> Error msg
  with
  | Sys_error msg -> Error msg
  | Unix.Unix_error (err, fn, arg) -> Error (unix_error_message err fn arg)
;;

let make_async_sink ~sw ~failure_log ~capture_id ~provider ~model ~location ~write =
  let fifo = create_async_fifo () in
  let write_one chunk =
    match write chunk with
    | Ok () -> ()
    | Error (stage, failure_location, message) ->
      record_failure
        failure_log
        { stage; capture_id; provider; model; location = failure_location; message }
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception exn ->
      record_failure
        failure_log
        { stage = Writer
        ; capture_id
        ; provider
        ; model
        ; location
        ; message = Printexc.to_string exn
        }
  in
  let rec writer () =
    match take fifo with
    | Some chunk ->
      write_one chunk;
      writer ()
    | None -> ()
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    (* The stream owner closes the FIFO in a [Fun.protect] finalizer. Protecting
       the writer's whole lifecycle lets it observe that close, drain every
       accepted chunk, and exit even when the outer switch is being cancelled.
       Only outer-scope shutdown joins this daemon; closing a stream merely
       signals the FIFO and never waits for exporter I/O. *)
    (try Eio.Cancel.protect writer with
     | Eio.Cancel.Cancelled _ -> ()
     | exn ->
       record_failure
         failure_log
         { stage = Writer
         ; capture_id
         ; provider
         ; model
         ; location
         ; message = Printexc.to_string exn
         });
    `Stop_daemon);
  { push_chunk =
      (fun chunk ->
        if not (enqueue fifo chunk)
        then invalid_arg "Wire_capture.push: sink is already closed")
  ; close_sink = (fun () -> close_fifo fifo)
  ; captured_failures = (fun () -> recorded_failures failure_log)
  }
;;

let make_sink ?getenv ~sw ~on_failure ~capture_id ~provider ~model () =
  let failure_log = create_failure_log ~on_failure in
  let disable ~location ~capture_id message =
    record_failure
      failure_log
      { stage = Activation; capture_id; provider; model; location; message };
    disabled_sink failure_log
  in
  match Cli_common_env.get ?getenv env_dir with
  | None -> disabled_sink failure_log
  | Some dir ->
    (match capture_id with
     | None ->
       disable
         ~location:dir
         ~capture_id:None
         "the caller supplied no exact capture identity"
     | Some capture_id when String.trim capture_id = "" ->
       disable
         ~location:dir
         ~capture_id:(Some capture_id)
         "the caller supplied an empty capture identity"
     | Some capture_id ->
       let path = segment_path ~dir ~capture_id in
       let activated = ref false in
       let write chunk =
         let activation =
           if !activated
           then Ok ()
           else
             Eio_unix.run_in_systhread ~label:"wire-capture-activate" (fun () ->
               ensure_capture_dir dir)
         in
         match activation with
         | Error message -> Error (Activation, dir, message)
         | Ok () ->
           activated := true;
           Result.map_error
             (fun message -> Append, path, message)
             (write_line ~path ~capture_id ~provider ~model chunk)
       in
       make_async_sink
         ~sw
         ~failure_log
         ~capture_id:(Some capture_id)
         ~provider
         ~model
         ~location:path
         ~write)
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

let%test "segment filename is canonical lowercase SHA-256" =
  String.equal
    (segment_filename "run-first")
    "b1f44c891f58a38b5fae9ddc1937849b89ce267d10a8b6be8e80eb03281a5d3a.jsonl"
;;

let%test "make_sink is a no-op when env is unset or empty" =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let unset =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(fun _ -> None)
          ~capture_id:(Some "request-disabled")
          ~provider:"p"
          ~model:"m"
          ()
      in
      let empty =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(getenv_of_pairs [ env_dir, "   " ])
          ~capture_id:(Some "request-empty")
          ~provider:"p"
          ~model:"m"
          ()
      in
      push unset "raw chunk";
      push empty "raw chunk";
      close unset;
      close empty;
      (* no exception, no output path assumed *)
      true))
;;

let%test "make_sink writes redacted binary JSONL when env is set" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire" "" in
    (* Built at runtime so no literal secret appears in source. *)
    let token = "ghp_" ^ String.make 36 '7' in
    Eio.Switch.run (fun sw ->
      let s =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(capture_getenv dir)
          ~capture_id:(Some "request-redaction")
          ~provider:"ollama_cloud"
          ~model:"deepseek-v4-flash"
          ()
      in
      push s ("delta content " ^ token ^ " end");
      close s);
    let path = segment_path ~dir ~capture_id:"request-redaction" in
    let content = read_file path in
    (not (contains ~needle:token content))
    && contains ~needle:"[REDACTED]" content
    && contains ~needle:"\"capture_id\":\"request-redaction\"" content
    && contains ~needle:"deepseek-v4-flash" content)
;;

let%test "concurrent capture identities use distinct append-only segments" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire_multi" "" in
    let first_id = "run-first" in
    let second_id = "run-second" in
    Eio.Switch.run (fun sw ->
      let s1 =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(capture_getenv dir)
          ~capture_id:(Some first_id)
          ~provider:"p1"
          ~model:"m1"
          ()
      in
      let s2 =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(capture_getenv dir)
          ~capture_id:(Some second_id)
          ~provider:"p2"
          ~model:"m2"
          ()
      in
      push s1 (String.make 4096 'a');
      push s2 (String.make 4096 'b');
      close s1;
      close s2);
    let first_path = segment_path ~dir ~capture_id:first_id in
    let second_path = segment_path ~dir ~capture_id:second_id in
    let first_before = read_file first_path in
    let second_before = read_file second_path in
    Eio.Switch.run (fun sw ->
      let third =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(capture_getenv dir)
          ~capture_id:(Some "run-third")
          ~provider:"p3"
          ~model:"m3"
          ()
      in
      push third "later";
      close third);
    String.equal first_before (read_file first_path)
    && String.equal second_before (read_file second_path)
    && contains ~needle:"\"capture_id\":\"run-first\"" first_before
    && contains ~needle:"\"provider\":\"p1\"" first_before
    && (not (contains ~needle:"\"provider\":\"p2\"" first_before))
    && contains ~needle:"\"capture_id\":\"run-second\"" second_before
    && contains ~needle:"\"provider\":\"p2\"" second_before
    && not (contains ~needle:"\"provider\":\"p1\"" second_before))
;;

let%test "make_sink disables capture when env path is a file" =
  Eio_main.run (fun _env ->
    let path = Filename.temp_file "oas_wire_file" ".txt" in
    let before = read_file path in
    let observed = ref [] in
    Eio.Switch.run (fun sw ->
      let s =
        make_sink
          ~sw
          ~on_failure:(fun failure -> observed := failure :: !observed)
          ~getenv:(capture_getenv path)
          ~capture_id:(Some "request-file-path")
          ~provider:"p"
          ~model:"m"
          ()
      in
      push s "chunk";
      close s);
    Sys.file_exists path
    && String.equal before (read_file path)
    &&
    match !observed with
    | [ { stage = Activation; capture_id = Some "request-file-path"; location; _ } ] ->
      String.equal location path
    | _ -> false)
;;

let%test "disabled sink writes nothing" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire_off" "" in
    Eio.Switch.run (fun sw ->
      let s =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(fun _ -> None)
          ~capture_id:(Some "request-off")
          ~provider:"p"
          ~model:"m"
          ()
      in
      push s "chunk";
      close s);
    not (Sys.file_exists (segment_path ~dir ~capture_id:"request-off")))
;;

let%test "configured capture without an exact identity fails explicitly" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire_no_id" "" in
    let observed = ref [] in
    Eio.Switch.run (fun sw ->
      let s =
        make_sink
          ~sw
          ~on_failure:(fun failure -> observed := failure :: !observed)
          ~getenv:(capture_getenv dir)
          ~capture_id:None
          ~provider:"p"
          ~model:"m"
          ()
      in
      push s "chunk";
      close s);
    match !observed with
    | [ { stage = Activation; capture_id = None; location; message; _ } ] ->
      String.equal location dir && contains ~needle:"no exact capture identity" message
    | _ -> false)
;;

let%test "append-only segment preserves a chunk larger than the retired cap" =
  Eio_main.run (fun _env ->
    let dir = Filename.temp_dir "oas_wire_large" "" in
    let capture_id = "request-large" in
    let chunk = String.make (1024 * 1024) 'x' in
    Eio.Switch.run (fun sw ->
      let s =
        make_sink
          ~sw
          ~on_failure:ignore
          ~getenv:(capture_getenv dir)
          ~capture_id:(Some capture_id)
          ~provider:"p"
          ~model:"m"
          ()
      in
      push s chunk;
      close s);
    let content = read_file (segment_path ~dir ~capture_id) in
    match Yojson.Safe.from_string (String.trim content) with
    | `Assoc fields -> List.assoc_opt "chunk" fields = Some (`String chunk)
    | _ -> false)
;;

let%test "async FIFO is nonblocking and preserves a stalled burst" =
  Eio_main.run (fun env ->
    let clock = Eio.Stdenv.clock env in
    let chunks = List.init 256 (fun i -> Printf.sprintf "chunk-%03d" i) in
    let first_write = ref true in
    let written_rev = ref [] in
    let enqueue_elapsed = ref 0.0 in
    let close_elapsed = ref 0.0 in
    Eio.Switch.run (fun sw ->
      let write chunk =
        if !first_write
        then (
          first_write := false;
          Eio.Time.sleep clock 0.3);
        written_rev := chunk :: !written_rev;
        Ok ()
      in
      let failure_log = create_failure_log ~on_failure:ignore in
      let s =
        make_async_sink
          ~sw
          ~failure_log
          ~capture_id:(Some "stalled-burst")
          ~provider:"p"
          ~model:"m"
          ~location:"test"
          ~write
      in
      let t0 = Unix.gettimeofday () in
      List.iter (push s) chunks;
      enqueue_elapsed := Unix.gettimeofday () -. t0;
      let close_started = Unix.gettimeofday () in
      close s;
      close_elapsed := Unix.gettimeofday () -. close_started);
    !enqueue_elapsed < 0.1 && !close_elapsed < 0.1 && List.rev !written_rev = chunks)
;;

let%test "async exporter failure is retained and delivered as typed data" =
  Eio_main.run (fun _env ->
    let observed = ref [] in
    let finished_sink = ref None in
    Eio.Switch.run (fun sw ->
      let failure_log =
        create_failure_log ~on_failure:(fun failure -> observed := failure :: !observed)
      in
      let s =
        make_async_sink
          ~sw
          ~failure_log
          ~capture_id:(Some "failed-capture")
          ~provider:"p"
          ~model:"m"
          ~location:"failed-segment.jsonl"
          ~write:(fun _ -> Error (Append, "failed-segment.jsonl", "storage unavailable"))
      in
      push s "chunk";
      close s;
      finished_sink := Some s);
    match !finished_sink, !observed with
    | ( Some s
      , [ { stage = Append
          ; capture_id = Some "failed-capture"
          ; provider = "p"
          ; model = "m"
          ; location = "failed-segment.jsonl"
          ; message = "storage unavailable"
          }
        ] ) -> failures s = List.rev !observed
    | _ -> false)
;;
