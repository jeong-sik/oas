(** HTTP client for LLM provider endpoints.

    Wraps Eio + cohttp-eio with TLS. All network and HTTP-level errors
    are captured as {!http_error} so callers do not need [try/with].

    Each synchronous request runs inside its own [Eio.Switch.run] scope
    so the underlying TCP connection and its file descriptor are released
    as soon as the response body is fully consumed.  Without this,
    connections accumulate for the lifetime of the caller's switch —
    typically the server's main switch — eventually exhausting OS file
    descriptors.

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

let add_connection_close headers =
  ("connection", "close") :: headers

(** Client wrapper that tracks the socket for explicit close.
    cohttp-eio's [Eio.Switch] cleanup does not reliably call
    [Unix.close] on the underlying socket fd (observed on macOS,
    cohttp-eio 6.1.1).  We intercept the connection factory via
    [make_generic] to capture the socket, then close it explicitly
    when the switch exits. *)
let make_closing_client ~sw ~net =
  let net = (net :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let https = Api_common.make_https () in
  (* Track the raw socket separately for explicit close.
     The socket fd is the resource that leaks; closing it releases the fd. *)
  let last_sock :
    [ `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t option ref =
    ref None
  in
  let connect ~sw:conn_sw uri =
    let service =
      match Uri.port uri with
      | Some port -> Int.to_string port
      | _ -> Uri.scheme uri |> Option.value ~default:"http"
    in
    let addr =
      match
        Eio.Net.getaddrinfo_stream ~service net
          (Uri.host_with_default ~default:"localhost" uri)
      with
      | ip :: _ -> ip
      | [] ->
        (* Raised inside connect callback; caught by catch_network at
           the public API boundary and converted to Error NetworkError. *)
        failwith ("failed to resolve hostname: "
          ^ Uri.host_with_default ~default:"(none)" uri)
    in
    let sock = Eio.Net.connect ~sw:conn_sw net addr in
    last_sock := Some sock;
    (* Return type must include `Close for cohttp-eio >= 6.2 make_generic *)
    match Uri.scheme uri with
    | Some "https" -> (
        match https with
        | Some wrap ->
            (wrap uri sock
              :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
        | None ->
          (* Raised inside connect callback; caught by catch_network. *)
          failwith ("HTTPS requested but TLS not available for "
            ^ Uri.to_string uri))
    | _ -> (sock :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
  in
  let client = Cohttp_eio.Client.make_generic connect in
  Eio.Switch.on_release sw (fun () ->
    match !last_sock with
    | None -> ()
    | Some sock ->
        last_sock := None;
        (try Eio.Net.close sock with
         | Eio.Cancel.Cancelled _ as e -> raise e
         | _exn -> (* Socket close failure is non-fatal but logged for FD leak diagnosis. *)
           ()));
  client

let get_sync ~sw:_ ~net ~url ~headers =
  catch_network (fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = make_closing_client ~sw ~net in
    let uri = Uri.of_string url in
    let hdr = Http.Header.of_list (add_connection_close headers) in
    let resp, resp_body =
      Cohttp_eio.Client.get ~sw client ~headers:hdr uri
    in
    let code =
      Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let body_str =
      Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body
                       resp_body |> take_all) in
    Ok (code, body_str))

let post_sync ~sw:_ ~net ~url ~headers ~body =
  catch_network (fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = make_closing_client ~sw ~net in
    let uri = Uri.of_string url in
    let hdr = Http.Header.of_list (add_connection_close headers) in
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
        Ok (Eio.Buf_read.of_flow ~max_size:Api_common.max_response_body resp_body)
    | status ->
        let code = Cohttp.Code.code_of_status status in
        let body_str =
          Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body
                           resp_body |> take_all) in
        Error (HttpError { code; body = body_str }))

let with_post_stream ~net ~url ~headers ~body ~f =
  catch_network (fun () ->
    Eio.Switch.run @@ fun sw ->
    let client = make_closing_client ~sw ~net in
    let uri = Uri.of_string url in
    let hdr = Http.Header.of_list (add_connection_close headers) in
    let resp, resp_body =
      Cohttp_eio.Client.post ~sw client ~headers:hdr
        ~body:(Cohttp_eio.Body.of_string body) uri
    in
    match Cohttp.Response.status resp with
    | `OK ->
        let reader =
          Eio.Buf_read.of_flow ~max_size:Api_common.max_response_body resp_body
        in
        Ok (f reader)
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
