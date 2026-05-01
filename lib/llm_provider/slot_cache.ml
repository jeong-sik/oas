open Base
(** Slot KV cache persistence for local llama-server endpoints.

    @since 0.100.0 *)

let slot_action ~sw ~net ~endpoint ~slot_id ~action ~body_fields =
  let url = Printf.sprintf "%s/slots/%d?action=%s" endpoint slot_id action in
  let body =
    match body_fields with
    | [] -> "{}"
    | fields -> Yojson.Safe.to_string (`Assoc fields)
  in
  let headers = [ "Content-Type", "application/json" ] in
  match Http_client.post_sync ~sw ~net ~url ~headers ~body () with
  | Ok (code, _) when code >= 200 && code < 300 -> Ok ()
  | Ok (code, resp_body) ->
    let msg =
      Printf.sprintf
        "slot %s HTTP %d: %s"
        action
        code
        (if String.length resp_body > 200 then String.sub resp_body 0 200 else resp_body)
    in
    Error msg
  | Error (Http_client.HttpError { code; body }) ->
    Error (Printf.sprintf "slot %s HTTP error %d: %s" action code body)
  | Error (Http_client.AcceptRejected { reason }) ->
    Error (Printf.sprintf "slot %s rejected: %s" action reason)
  | Error (Http_client.NetworkError { message; _ }) ->
    Error (Printf.sprintf "slot %s network error: %s" action message)
  | Error (Http_client.CliTransportRequired { kind }) ->
    Error (Printf.sprintf "slot %s: CLI transport required for %s" action kind)
  | Error (Http_client.ProviderTerminal { message; _ }) ->
    Error (Printf.sprintf "slot %s: %s" action message)
  | Error (Http_client.ProviderFailure { kind; message }) ->
    Error
      (Printf.sprintf
         "slot %s: %s"
         action
         (Http_client.provider_failure_to_string ~kind ~message))
;;

let save ~sw ~net ~endpoint ~slot_id ~filename =
  slot_action
    ~sw
    ~net
    ~endpoint
    ~slot_id
    ~action:"save"
    ~body_fields:[ "filename", `String filename ]
;;

let restore ~sw ~net ~endpoint ~slot_id ~filename =
  slot_action
    ~sw
    ~net
    ~endpoint
    ~slot_id
    ~action:"restore"
    ~body_fields:[ "filename", `String filename ]
;;

let erase ~sw ~net ~endpoint ~slot_id =
  slot_action ~sw ~net ~endpoint ~slot_id ~action:"erase" ~body_fields:[]
;;

let cleanup_file ~filename ~save_dir =
  let path = Filename.concat save_dir filename in
  if Sys.file_exists path
  then (
    try Sys.remove path with
    | Sys_error _ -> ())
;;

let cache_filename ~session_id ~slot_id =
  Printf.sprintf "slot-%s-%d.bin" session_id slot_id
;;
