(** Runtime control-channel request helpers.

    These functions keep stdin/stdout control-message plumbing out of the
    session command state machine in [Runtime_server]. *)

open Runtime
open Runtime_server_types

let rec read_control_response state control_id =
  match input_line stdin with
  | raw when String.trim raw = "" -> read_control_response state control_id
  | exception End_of_file ->
    Error
      (Error.Io
         (FileOpFailed
            { op = "read"; path = "stdin"; detail = "runtime control channel closed" }))
  | raw ->
    (match protocol_message_of_string raw with
     | Error detail -> Error (Error.Serialization (JsonParseError { detail }))
     | Ok (Control_response_message payload)
       when String.equal payload.control_id control_id -> Ok payload.response
     | Ok _ -> read_control_response state control_id)
;;

let ask_permission state ~action ~subject ~payload =
  let control_id = next_control_id state in
  write_protocol_message
    state
    (Control_request_message
       { control_id; request = Permission_request { action; subject; payload } });
  read_control_response state control_id
;;

let invoke_hook state ~hook_name ~payload =
  let control_id = next_control_id state in
  write_protocol_message
    state
    (Control_request_message { control_id; request = Hook_request { hook_name; payload } });
  read_control_response state control_id
;;
