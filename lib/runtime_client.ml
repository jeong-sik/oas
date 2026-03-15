type options = Transport.options = {
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

let default_options = Transport.default_options

type t = {
  transport: Transport.t;
}

let connect ~sw ~mgr ?(options = default_options) () =
  match Transport.connect ~sw ~mgr ~options () with
  | Ok transport -> Ok { transport }
  | Error err -> Error err

let request ?control_handler ?event_handler client request =
  let open Error in
  let* response =
    Transport.request ?control_handler ?event_handler client.transport request
  in
  match response with
  | Runtime.Error_response detail -> Error (Internal detail)
  | _ -> Ok response

let init_info client =
  request client
    (Runtime.Initialize
       {
         session_root = None;
         provider = None;
         model = None;
         permission_mode = None;
         include_partial_messages = false;
         setting_sources = [];
         resume_session = None;
         cwd = None;
       })

let get_server_info client = Transport.server_info client.transport

let start_session ?control_handler ?event_handler client request_data =
  let* response =
    request ?control_handler ?event_handler client
      (Runtime.Start_session request_data)
  in
  match response with
  | Runtime.Session_started_response session -> Ok session
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected start_session response: %s"
              (Runtime.show_response other)))

let apply_command ?control_handler ?event_handler client ~session_id command =
  let* response =
    request ?control_handler ?event_handler client
      (Runtime.Apply_command { session_id; command })
  in
  match response with
  | Runtime.Command_applied session
  | Runtime.Finalized session ->
      Ok session
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected apply_command response: %s"
              (Runtime.show_response other)))

let status client ~session_id =
  let* response = request client (Runtime.Status { session_id }) in
  match response with
  | Runtime.Status_response session -> Ok session
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected status response: %s"
              (Runtime.show_response other)))

let events client ~session_id ?after_seq () =
  let* response = request client (Runtime.Events { session_id; after_seq }) in
  match response with
  | Runtime.Events_response events -> Ok events
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected events response: %s"
              (Runtime.show_response other)))

let finalize ?control_handler ?event_handler client ~session_id ?reason () =
  let* response =
    request ?control_handler ?event_handler client
      (Runtime.Finalize { session_id; reason })
  in
  match response with
  | Runtime.Finalized session -> Ok session
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected finalize response: %s"
              (Runtime.show_response other)))

let report client ~session_id =
  let* response = request client (Runtime.Report { session_id }) in
  match response with
  | Runtime.Report_response report -> Ok report
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected report response: %s"
              (Runtime.show_response other)))

let prove client ~session_id =
  let* response = request client (Runtime.Prove { session_id }) in
  match response with
  | Runtime.Prove_response proof -> Ok proof
  | other ->
      Error
        (Error.Internal
           (Printf.sprintf "Unexpected prove response: %s"
              (Runtime.show_response other)))

let close client = Transport.close client.transport
