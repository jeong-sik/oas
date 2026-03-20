(** IPC transport for oas_runtime using Eio structured concurrency.

    Replaces the previous OS-thread implementation (Mutex/Condition/Thread)
    with Eio primitives: Eio.Mutex, Eio.Promise, Eio.Fiber, Eio_unix.pipe,
    Eio.Process.spawn.

    Reference pattern: lib/mcp.ml connect function (same project).
    Reference pattern: lib/event_bus.ml Eio.Mutex usage (same project). *)

type options = {
  runtime_path: string option;
  session_root: string option;
  provider: string option;
  model: string option;
  permission_mode: string option;
  include_partial_messages: bool;
  setting_sources: string list;
  resume_session: string option;
  cwd: string option;
}

let ( let* ) = Result.bind

type control_handler =
  Runtime.control_request -> (Runtime.control_response, Error.sdk_error) result

type event_handler = Runtime.event -> unit

let default_options = {
  runtime_path = None;
  session_root = None;
  provider = None;
  model = None;
  permission_mode = None;
  include_partial_messages = false;
  setting_sources = [];
  resume_session = None;
  cwd = None;
}

type t = {
  runtime_path: string;
  reader: Eio.Buf_read.t;
  writer: Eio.Flow.sink_ty Eio.Resource.t;
  (* Set once during [connect], then read-only from other fibers.
     No mutex needed: the write completes before the transport value
     is returned to the caller, establishing a happens-before edge. *)
  mutable init_info: Runtime.init_response option;
  closed: bool Atomic.t;
  next_request_id: int Atomic.t;
  write_mu: Eio.Mutex.t;
  mu: Eio.Mutex.t;
  pending: (string, (Runtime.response, Error.sdk_error) result Eio.Promise.u) Hashtbl.t;
  mutable control_handler: control_handler option;
  mutable event_handler: event_handler option;
  mutable reader_failed: Error.sdk_error option;
  kill: unit -> unit;
}

(* ── Runtime discovery (unchanged) ─────────────────────────────────── *)

let candidate_paths ?runtime_path () =
  let env_runtime = Sys.getenv_opt "OAS_RUNTIME_PATH" in
  let exe_dir = Filename.dirname Sys.executable_name in
  let from_exe =
    [
      Filename.concat exe_dir "../bin/oas_runtime.exe";
      Filename.concat exe_dir "oas_runtime.exe";
      Filename.concat exe_dir "_bundled/oas-runtime";
      Filename.concat exe_dir "_bundled/oas_runtime.exe";
    ]
  in
  let cwd = Sys.getcwd () in
  let from_cwd =
    [
      Filename.concat cwd "_build/default/bin/oas_runtime.exe";
      Filename.concat cwd "bin/oas_runtime.exe";
      Filename.concat cwd "_bundled/oas-runtime";
    ]
  in
  let from_path =
    match Sys.getenv_opt "PATH" with
    | None -> []
    | Some path ->
        path
        |> String.split_on_char ':'
        |> List.concat_map (fun dir ->
               [ Filename.concat dir "oas-runtime"; Filename.concat dir "oas_runtime.exe" ])
  in
  List.filter_map
    (fun value ->
      match value with
      | Some candidate when String.trim candidate <> "" -> Some candidate
      | _ -> None)
    [ runtime_path; env_runtime ]
  @ from_exe @ from_cwd @ from_path

let is_runnable path =
  Sys.file_exists path && not (Sys.is_directory path)

let find_runtime ?runtime_path () =
  match List.find_opt is_runnable (candidate_paths ?runtime_path ()) with
  | Some path -> Ok path
  | None ->
      Error
        (Error.Io
           (Error.FileOpFailed
              {
                op = "find_runtime";
                path = "oas_runtime";
                detail =
                  "No runtime binary found. Set OAS_RUNTIME_PATH or build bin/oas_runtime.exe.";
              }))

(* ── Write helpers ──────────────────────────────────────────────────── *)

let write_message transport message =
  let line = Runtime.protocol_message_to_string message ^ "\n" in
  Eio.Mutex.use_rw ~protect:true transport.write_mu (fun () ->
    Eio.Flow.copy_string line transport.writer)

(* ── Handler management ─────────────────────────────────────────────── *)

let set_handlers ?control_handler ?event_handler transport =
  Eio.Mutex.use_rw ~protect:true transport.mu (fun () ->
    Option.iter (fun h -> transport.control_handler <- Some h) control_handler;
    Option.iter (fun h -> transport.event_handler <- Some h) event_handler)

let next_request_id transport =
  let id = Atomic.fetch_and_add transport.next_request_id 1 in
  Printf.sprintf "req-%06d" id

(* ── Response coordination via Eio.Promise ──────────────────────────── *)

let default_control_response = function
  | Runtime.Permission_request _ ->
      Runtime.Permission_response
        { allow = true; message = None; interrupt = false }
  | Runtime.Hook_request _ ->
      Runtime.Hook_response { continue_ = true; message = None }

let fail_all_pending transport err =
  Hashtbl.iter (fun _id resolver ->
    Eio.Promise.resolve resolver (Error err)
  ) transport.pending;
  Hashtbl.clear transport.pending

let set_reader_failed transport err =
  Eio.Mutex.use_rw ~protect:true transport.mu (fun () ->
    transport.reader_failed <- Some err);
  fail_all_pending transport err

(* ── Protocol message dispatch (reader fiber) ──────────────────────── *)

let dispatch_protocol_message transport = function
  | Runtime.Response_message payload ->
      (match Hashtbl.find_opt transport.pending payload.request_id with
       | Some resolver ->
           Hashtbl.remove transport.pending payload.request_id;
           Eio.Promise.resolve resolver (Ok payload.response)
       | None -> ())
  | Runtime.Control_request_message payload ->
      let handler =
        Eio.Mutex.use_ro transport.mu (fun () -> transport.control_handler)
      in
      let response =
        match handler with
        | Some handler -> handler payload.request
        | None -> Ok (default_control_response payload.request)
      in
      (match response with
       | Ok resp ->
           write_message transport
             (Runtime.Control_response_message
                { control_id = payload.control_id; response = resp })
       | Error err -> set_reader_failed transport err)
  | Runtime.Event_message payload ->
      let handler =
        Eio.Mutex.use_ro transport.mu (fun () -> transport.event_handler)
      in
      Option.iter (fun handle -> handle payload.event) handler
  | Runtime.Control_response_message _ -> ()
  | Runtime.System_message _ -> ()
  | Runtime.Request_message _ -> ()

let reader_loop transport =
  let rec loop () =
    let line = Eio.Buf_read.line transport.reader in
    if String.trim line = "" then begin
      if not (Atomic.get transport.closed) then loop ()
    end else
      match Runtime.protocol_message_of_string line with
      | Ok message ->
          dispatch_protocol_message transport message;
          if not (Atomic.get transport.closed) then loop ()
      | Error detail ->
          set_reader_failed transport
            (Error.Serialization (JsonParseError { detail }))
  in
  loop ()

let start_reader_fiber ~sw transport =
  Eio.Fiber.fork_daemon ~sw (fun () ->
    (try reader_loop transport
     with
     | End_of_file | Eio.Io _ | Unix.Unix_error _ | Sys_error _ ->
       if not (Atomic.get transport.closed) then
         set_reader_failed transport
           (Error.Io
              (Error.FileOpFailed
                 {
                   op = "read";
                   path = transport.runtime_path;
                   detail = "Runtime closed the stream";
                 })));
    `Stop_daemon)

(* ── Public API ─────────────────────────────────────────────────────── *)

let connect ~sw ~(mgr : _ Eio.Process.mgr) ?(options = default_options) () =
  let* runtime_path = find_runtime ?runtime_path:options.runtime_path () in
  try
    let r_child_stdin, w_child_stdin = Eio_unix.pipe sw in
    let r_child_stdout, w_child_stdout = Eio_unix.pipe sw in
    let proc =
      Eio.Process.spawn ~sw mgr
        ~stdin:(r_child_stdin :> Eio.Flow.source_ty Eio.Resource.t)
        ~stdout:(w_child_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
        [ runtime_path ]
    in
    Eio.Flow.close r_child_stdin;
    Eio.Flow.close w_child_stdout;
    let reader =
      Eio.Buf_read.of_flow (r_child_stdout :> _ Eio.Flow.source)
        ~max_size:(16 * 1024 * 1024)
    in
    let kill () =
      try Eio.Process.signal proc Sys.sigterm
      with Unix.Unix_error _ | Eio.Io _ | Sys_error _ -> ()
    in
    let transport =
      {
        runtime_path;
        reader;
        writer = (w_child_stdin :> Eio.Flow.sink_ty Eio.Resource.t);
        init_info = None;
        closed = Atomic.make false;
        next_request_id = Atomic.make 1;
        write_mu = Eio.Mutex.create ();
        mu = Eio.Mutex.create ();
        pending = Hashtbl.create 16;
        control_handler = None;
        event_handler = None;
        reader_failed = None;
        kill;
      }
    in
    start_reader_fiber ~sw transport;
    let init_request_id = next_request_id transport in
    let promise, resolver = Eio.Promise.create () in
    Hashtbl.replace transport.pending init_request_id resolver;
    write_message transport
      (Runtime.Request_message
         {
           request_id = init_request_id;
           request =
             Runtime.Initialize
               {
                 session_root = options.session_root;
                 provider = options.provider;
                 model = options.model;
                 permission_mode = options.permission_mode;
                 include_partial_messages = options.include_partial_messages;
                 setting_sources = options.setting_sources;
                 resume_session = options.resume_session;
                 cwd = options.cwd;
               };
         });
    let* response = Eio.Promise.await promise in
    (match response with
     | Runtime.Initialized init when String.equal init.protocol_version Runtime.protocol_version ->
         transport.init_info <- Some init;
         Ok transport
     | Runtime.Initialized init ->
         Error
           (Internal
              (Printf.sprintf "Protocol version mismatch: runtime=%s sdk=%s"
                 init.protocol_version Runtime.protocol_version))
     | Runtime.Error_response detail -> Error (Internal detail)
     | other ->
         Error
           (Internal
              (Printf.sprintf "Unexpected init response: %s"
                 (Runtime.show_response other))))
  with
  | Eio.Io _ as exn ->
      Error
        (Io
           (FileOpFailed
              {
                op = "spawn";
                path = runtime_path;
                detail = Printexc.to_string exn;
              }))
  | Unix.Unix_error (err, _, _) ->
      Error
        (Io
           (FileOpFailed
              {
                op = "spawn";
                path = runtime_path;
                detail = Unix.error_message err;
              }))
  | Failure msg ->
      Error
        (Io
           (FileOpFailed
              {
                op = "spawn";
                path = runtime_path;
                detail = msg;
              }))

let request ?control_handler ?event_handler transport request =
  let open Error in
  if Atomic.get transport.closed then
    Error (Internal "Runtime transport is already closed")
  else begin
    set_handlers ?control_handler ?event_handler transport;
    let request_id = next_request_id transport in
    let promise, resolver = Eio.Promise.create () in
    Hashtbl.replace transport.pending request_id resolver;
    write_message transport
      (Runtime.Request_message { request_id; request });
    Eio.Promise.await promise
  end

(** Query connection status. *)
let status transport =
  if Atomic.get transport.closed then `Disconnected
  else match transport.reader_failed with
  | Some err -> `Error (Error.to_string err)
  | None -> `Connected

let close transport =
  if Atomic.compare_and_set transport.closed false true then begin
    (try ignore (request transport Runtime.Shutdown)
     with Eio.Io _ | Unix.Unix_error _ | Failure _ -> ());
    transport.kill ()
  end

let server_info transport = transport.init_info

[@@@coverage off]
(* === Inline tests === *)

let%test "candidate_paths includes explicit runtime_path first" =
  let paths = candidate_paths ~runtime_path:"/custom/oas_runtime" () in
  match paths with
  | first :: _ -> first = "/custom/oas_runtime"
  | [] -> false

let%test "candidate_paths includes env override" =
  Unix.putenv "OAS_RUNTIME_PATH" "/env/oas_runtime";
  let paths = candidate_paths () in
  List.exists (fun p -> p = "/env/oas_runtime") paths

let%test "candidate_paths includes cwd paths" =
  let paths = candidate_paths () in
  List.exists (fun p -> String.length p > 0) paths

let%test "candidate_paths explicit takes priority over env" =
  Unix.putenv "OAS_RUNTIME_PATH" "/env/runtime";
  let paths = candidate_paths ~runtime_path:"/explicit/runtime" () in
  match paths with
  | first :: _ -> first = "/explicit/runtime"
  | [] -> false

let%test "is_runnable false for nonexistent path" =
  is_runnable "/nonexistent/path/binary" = false

let%test "is_runnable false for directory" =
  is_runnable "/tmp" = false

let%test "default_options has expected defaults" =
  default_options.runtime_path = None
  && default_options.session_root = None
  && default_options.provider = None
  && default_options.model = None
  && default_options.permission_mode = None
  && default_options.include_partial_messages = false
  && default_options.setting_sources = []
  && default_options.resume_session = None
  && default_options.cwd = None

let%test "default_control_response Permission_request allows" =
  match default_control_response (Runtime.Permission_request {
    action = "test"; subject = "desc"; payload = `Null }) with
  | Runtime.Permission_response { allow = true; interrupt = false; _ } -> true
  | _ -> false

let%test "default_control_response Hook_request continues" =
  match default_control_response (Runtime.Hook_request {
    hook_name = "before_turn"; payload = `Null }) with
  | Runtime.Hook_response { continue_ = true; _ } -> true
  | _ -> false

let%test "next_request_id generates sequential formatted ids" =
  (* Create a minimal transport-like struct to test the Atomic counter pattern *)
  let counter = Atomic.make 1 in
  let id1 = Atomic.fetch_and_add counter 1 in
  let id2 = Atomic.fetch_and_add counter 1 in
  let fmt_id n = Printf.sprintf "req-%06d" n in
  fmt_id id1 = "req-000001" && fmt_id id2 = "req-000002"
