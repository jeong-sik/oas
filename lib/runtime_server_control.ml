(** Runtime control-channel request helpers.

    These functions keep stdin/stdout control-message plumbing out of the
    session command state machine in [Runtime_server]. *)

open Runtime
open Runtime_server_types

let control_response_timeout_s = 60.0

let control_timeout_error control_id =
  Error.Io
    (FileOpFailed
       { op = "read"
       ; path = "runtime-control-channel"
       ; detail =
           Printf.sprintf
             "timed out waiting %.1fs for control response %s"
             control_response_timeout_s
             control_id
       })
;;

let register_control_waiter state control_id =
  let promise, resolver = Eio.Promise.create () in
  Eio.Mutex.use_rw ~protect:true state.control_waiters_mu (fun () ->
    Hashtbl.replace state.control_waiters control_id resolver);
  promise
;;

let remove_control_waiter state control_id =
  Eio.Mutex.use_rw ~protect:true state.control_waiters_mu (fun () ->
    Hashtbl.remove state.control_waiters control_id)
;;

let deliver_control_response state control_id response =
  let resolver =
    Eio.Mutex.use_rw ~protect:true state.control_waiters_mu (fun () ->
      let resolver = Hashtbl.find_opt state.control_waiters control_id in
      Hashtbl.remove state.control_waiters control_id;
      resolver)
  in
  match resolver with
  | None -> false
  | Some resolver ->
    Eio.Promise.resolve resolver response;
    true
;;

let await_control_response state control_id promise =
  try
    Ok
      (Eio.Time.with_timeout_exn state.clock control_response_timeout_s (fun () ->
         Eio.Promise.await promise))
  with
  | Eio.Time.Timeout ->
    remove_control_waiter state control_id;
    Error (control_timeout_error control_id)
;;

let read_control_response state control_id =
  let promise = register_control_waiter state control_id in
  await_control_response state control_id promise
;;

let ask_permission state ~action ~subject ~payload =
  let control_id = next_control_id state in
  let promise = register_control_waiter state control_id in
  write_protocol_message
    state
    (Control_request_message
       { control_id; request = Permission_request { action; subject; payload } });
  await_control_response state control_id promise
;;

let invoke_hook state ~hook_name ~payload =
  let control_id = next_control_id state in
  let promise = register_control_waiter state control_id in
  write_protocol_message
    state
    (Control_request_message { control_id; request = Hook_request { hook_name; payload } });
  await_control_response state control_id promise
;;
