(** HTTP client for LLM provider endpoints.

    Wraps Eio + cohttp-eio with TLS. All network and HTTP-level errors
    are captured as {!http_error} so callers do not need [try/with].

    @since 0.45.0 *)

type http_error =
  | HttpError of { code: int; body: string }
  | NetworkError of { message: string }

(* ── Internal helpers ──────────────────────────────────────── *)

let make_client ~net =
  let https = Api_common.make_https () in
  Cohttp_eio.Client.make ~https net

let catch_network f =
  try f ()
  with
  | Eio.Io _ as exn ->
      Error (NetworkError { message = Printexc.to_string exn })
  | Unix.Unix_error _ as exn ->
      Error (NetworkError { message = Printexc.to_string exn })
  | Failure msg ->
      Error (NetworkError { message = msg })

(* ── Public API ────────────────────────────────────────────── *)

let get_sync ~sw ~net ~url ~headers =
  catch_network (fun () ->
    let uri = Uri.of_string url in
    let client = make_client ~net in
    let hdr = Http.Header.of_list headers in
    let resp, resp_body =
      Cohttp_eio.Client.get ~sw client ~headers:hdr uri
    in
    let code =
      Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let body_str =
      Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body
                       resp_body |> take_all) in
    Ok (code, body_str))

let post_sync ~sw ~net ~url ~headers ~body =
  catch_network (fun () ->
    let uri = Uri.of_string url in
    let client = make_client ~net in
    let hdr = Http.Header.of_list headers in
    let resp, resp_body =
      Cohttp_eio.Client.post ~sw client ~headers:hdr
        ~body:(Cohttp_eio.Body.of_string body) uri
    in
    let code =
      Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let body_str =
      Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body
                       resp_body |> take_all) in
    Ok (code, body_str))

let post_stream ~sw ~net ~url ~headers ~body =
  catch_network (fun () ->
    let uri = Uri.of_string url in
    let client = make_client ~net in
    let hdr = Http.Header.of_list headers in
    let resp, resp_body =
      Cohttp_eio.Client.post ~sw client ~headers:hdr
        ~body:(Cohttp_eio.Body.of_string body) uri
    in
    match Cohttp.Response.status resp with
    | `OK ->
        Ok (Eio.Buf_read.of_flow ~max_size:(1024 * 1024 * 10) resp_body)
    | status ->
        let code = Cohttp.Code.code_of_status status in
        let body_str =
          Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body
                           resp_body |> take_all) in
        Error (HttpError { code; body = body_str }))

let read_sse ~reader ~on_data () =
  let current_event_type = ref None in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | line ->
        let len = String.length line in
        if len = 0 then
          current_event_type := None
        else if len > 7 && String.sub line 0 7 = "event: " then
          current_event_type :=
            Some (String.sub line 7 (len - 7))
        else if len > 6 && String.sub line 0 6 = "data: " then begin
          let data = String.sub line 6 (len - 6) in
          on_data ~event_type:!current_event_type data
        end;
        loop ()
    | exception End_of_file -> ()
  in
  loop ()

let inject_stream_param body_str =
  match Yojson.Safe.from_string body_str with
  | `Assoc fields ->
      Yojson.Safe.to_string (`Assoc (("stream", `Bool true) :: fields))
  | other -> Yojson.Safe.to_string other
  | exception Yojson.Json_error _ -> body_str
