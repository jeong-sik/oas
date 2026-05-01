open Base
let with_runtime f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let mgr = Eio.Stdenv.process_mgr env in
  let clock = Eio.Stdenv.clock env in
  f ~env ~sw ~net ~mgr ~clock
;;

let read_all ic =
  let buf = Buffer.create 4096 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    match input ic chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
      Buffer.add_subbytes buf chunk 0 n;
      loop ()
  in
  loop ();
  Buffer.contents buf
;;

let run_process_capture ?env prog args =
  let argv = Array.of_list (prog :: args) in
  let env = Option.value env ~default:(Unix.environment ()) in
  let stdout_ic, stdin_oc, stderr_ic = Unix.open_process_args_full prog argv env in
  let stdout = read_all stdout_ic in
  let stderr = read_all stderr_ic in
  match Unix.close_process_full (stdout_ic, stdin_oc, stderr_ic) with
  | Unix.WEXITED 0 -> Ok stdout
  | Unix.WEXITED code ->
    let detail =
      match String.trim stderr with
      | "" -> String.trim stdout
      | text -> text
    in
    Error (Printf.sprintf "%s exited with code %d: %s" prog code detail)
  | Unix.WSIGNALED signal -> Error (Printf.sprintf "%s killed by signal %d" prog signal)
  | Unix.WSTOPPED signal -> Error (Printf.sprintf "%s stopped by signal %d" prog signal)
;;

let run_process_capture_eio ~mgr prog args =
  let argv = prog :: args in
  try Ok (Eio.Process.parse_out mgr Eio.Buf_read.take_all argv) with
  | exn -> Error (Printf.sprintf "%s failed: %s" prog (Printexc.to_string exn))
;;
