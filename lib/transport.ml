type options = {
  runtime_path: string option;
  session_root: string option;
}

let ( let* ) = Result.bind

type control_handler =
  Runtime.control_request -> (Runtime.control_response, Error.sdk_error) result

type event_handler = Runtime.event -> unit

let default_options = {
  runtime_path = None;
  session_root = None;
}

type t = {
  runtime_path: string;
  ic: in_channel;
  oc: out_channel;
  ec: in_channel;
  mutable closed: bool;
  mutable next_request_id: int;
  write_mu: Mutex.t;
  state_mu: Mutex.t;
  state_cv: Condition.t;
  pending_responses: (string, Runtime.response) Hashtbl.t;
  mutable control_handler: control_handler option;
  mutable event_handler: event_handler option;
  mutable reader_failed: Error.sdk_error option;
  mutable reader_thread: Thread.t option;
}

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

let write_protocol_message oc message =
  output_string oc (Runtime.protocol_message_to_string message);
  output_char oc '\n';
  flush oc

let with_write_lock transport f =
  Mutex.lock transport.write_mu;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock transport.write_mu)
    f

let set_handlers ?control_handler ?event_handler transport =
  Mutex.lock transport.state_mu;
  (match control_handler with Some handler -> transport.control_handler <- Some handler | None -> ());
  (match event_handler with Some handler -> transport.event_handler <- Some handler | None -> ());
  Mutex.unlock transport.state_mu

let next_request_id transport =
  let id = transport.next_request_id in
  transport.next_request_id <- id + 1;
  Printf.sprintf "req-%06d" id

let respond_to_control transport control_id response =
  with_write_lock transport (fun () ->
      write_protocol_message transport.oc
        (Runtime.Control_response_message { control_id; response }))

let default_control_response = function
  | Runtime.Permission_request _ ->
      Runtime.Permission_response
        { allow = true; message = None; interrupt = false }
  | Runtime.Hook_request _ ->
      Runtime.Hook_response { continue_ = true; message = None }

let set_reader_failed transport err =
  Mutex.lock transport.state_mu;
  transport.reader_failed <- Some err;
  Condition.broadcast transport.state_cv;
  Mutex.unlock transport.state_mu

let dispatch_protocol_message transport = function
  | Runtime.Response_message payload ->
      Mutex.lock transport.state_mu;
      Hashtbl.replace transport.pending_responses payload.request_id payload.response;
      Condition.broadcast transport.state_cv;
      Mutex.unlock transport.state_mu
  | Runtime.Control_request_message payload ->
      let response =
        Mutex.lock transport.state_mu;
        let handler = transport.control_handler in
        Mutex.unlock transport.state_mu;
        match handler with
        | Some handler -> handler payload.request
        | None -> Ok (default_control_response payload.request)
      in
      (match response with
       | Ok response -> respond_to_control transport payload.control_id response
       | Error err -> set_reader_failed transport err)
  | Runtime.Event_message payload ->
      Mutex.lock transport.state_mu;
      let handler = transport.event_handler in
      Mutex.unlock transport.state_mu;
      Option.iter (fun handle -> handle payload.event) handler
  | Runtime.Control_response_message _ -> ()
  | Runtime.System_message _ -> ()
  | Runtime.Request_message _ -> ()

let start_reader_thread transport =
  let rec loop () =
    match input_line transport.ic with
    | line -> (
        match Runtime.protocol_message_of_string line with
        | Ok message ->
            dispatch_protocol_message transport message;
            if not transport.closed then loop ()
        | Error detail ->
            set_reader_failed transport
              (Error.Serialization (JsonParseError { detail })))
    | exception End_of_file ->
        if not transport.closed then
          set_reader_failed transport
            (Error.Io
               (Error.FileOpFailed
                  {
                    op = "read";
                    path = transport.runtime_path;
                    detail = "Runtime closed the stream";
                  }))
  in
  let thread = Thread.create loop () in
  transport.reader_thread <- Some thread

let await_response transport request_id =
  Mutex.lock transport.state_mu;
  let rec wait () =
    match Hashtbl.find_opt transport.pending_responses request_id with
    | Some response ->
        Hashtbl.remove transport.pending_responses request_id;
        Mutex.unlock transport.state_mu;
        Ok response
    | None -> (
        match transport.reader_failed with
        | Some err ->
            Mutex.unlock transport.state_mu;
            Error err
        | None ->
            Condition.wait transport.state_cv transport.state_mu;
            wait ())
  in
  wait ()

let connect ?(options = default_options) () =
  let* runtime_path = find_runtime ?runtime_path:options.runtime_path () in
  try
    let env = Unix.environment () in
    let argv = [| runtime_path |] in
    let ic, oc, ec = Unix.open_process_args_full runtime_path argv env in
    let transport =
      {
        runtime_path;
        ic;
        oc;
        ec;
        closed = false;
        next_request_id = 1;
        write_mu = Mutex.create ();
        state_mu = Mutex.create ();
        state_cv = Condition.create ();
        pending_responses = Hashtbl.create 16;
        control_handler = None;
        event_handler = None;
        reader_failed = None;
        reader_thread = None;
      }
    in
    start_reader_thread transport;
    let init_request_id = next_request_id transport in
    with_write_lock transport (fun () ->
        write_protocol_message oc
          (Runtime.Request_message
             {
               request_id = init_request_id;
               request = Runtime.Initialize { session_root = options.session_root };
             }));
    let* response = await_response transport init_request_id in
    (match response with
     | Runtime.Initialized init when String.equal init.protocol_version Runtime.protocol_version ->
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
  | Unix.Unix_error (err, _, _) ->
      Error
        (Io
           (FileOpFailed
              {
                op = "spawn";
                path = runtime_path;
                detail = Unix.error_message err;
              }))
  | exn ->
      Error
        (Io
           (FileOpFailed
              {
                op = "spawn";
                path = runtime_path;
                detail = Printexc.to_string exn;
              }))

let request ?control_handler ?event_handler transport request =
  let open Error in
  if transport.closed then
    Error (Internal "Runtime transport is already closed")
  else
    let () = set_handlers ?control_handler ?event_handler transport in
    let request_id = next_request_id transport in
    with_write_lock transport (fun () ->
        write_protocol_message transport.oc
          (Runtime.Request_message { request_id; request }));
    await_response transport request_id

let close transport =
  if not transport.closed then (
    (match request transport Runtime.Shutdown with _ -> ());
    transport.closed <- true;
    ignore (Unix.close_process_full (transport.ic, transport.oc, transport.ec));
    Option.iter Thread.join transport.reader_thread)
