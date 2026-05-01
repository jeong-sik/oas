(** Shared conversion for unexpected exceptions at agent execution boundaries. *)

let internal_message ?context exn =
  let exn_message = Printexc.to_string exn in
  let backtrace = Printexc.get_backtrace () in
  let prefix =
    match context with
    | Some context -> Printf.sprintf "Unexpected exception while %s" context
    | None -> "Unexpected exception"
  in
  if String.length backtrace = 0
  then Printf.sprintf "%s: %s" prefix exn_message
  else Printf.sprintf "%s: %s\nBacktrace:\n%s" prefix exn_message backtrace
;;

let error_of_raised ?context exn =
  match exn with
  | Raw_trace.Trace_error e -> Error e
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | Out_of_memory -> raise Out_of_memory
  | Stack_overflow -> raise Stack_overflow
  | Sys.Break -> raise Sys.Break
  | exn -> Error (Error.Internal (internal_message ?context exn))
;;

let capture ?context f =
  try f () with
  | exn -> error_of_raised ?context exn
;;
