(* See discovery_http.mli for module rationale. *)

let get_json ~sw ~net url =
  match Http_client.get_sync ~sw ~net ~url ~headers:[] () with
  | Ok (code, body) when code >= 200 && code < 300 ->
    (try Ok (Yojson.Safe.from_string body) with
     | Yojson.Json_error msg -> Error msg)
  | Ok (code, _) -> Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.HttpError { code; _ }) -> Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.AcceptRejected { reason }) -> Error reason
  | Error (Http_client.NetworkError { message; _ }) -> Error message
  | Error (Http_client.TimeoutError { message; _ }) -> Error message
  | Error (Http_client.ProviderTerminal { message; _ }) ->
    (* Discovery hits HTTP endpoints only; CLI subprocess terminals
       cannot reach this match.  Surface the message defensively so the
       exhaustive match stays sound. *)
    Error message
  | Error (Http_client.ProviderFailure { kind; message }) ->
    Error (Http_client.provider_failure_to_string ~kind ~message)
;;

let get_ok ~sw ~net url =
  match Http_client.get_sync ~sw ~net ~url ~headers:[] () with
  | Ok (code, _) when code >= 200 && code < 300 -> true
  | _ -> false
;;
