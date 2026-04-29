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

type network_error_kind =
  | Connection_refused
  | Dns_failure
  | Tls_error
  | Timeout
  | Local_resource_exhaustion
  | End_of_file
  | Unknown

(* Provider-internal terminal condition reported via structured exit
   (see .mli for the rationale and the @since note).  Adding a new
   variant rather than overloading [NetworkError] keeps cascades from
   counting a provider's own [max_turns] hit as a flaky network. *)
type provider_terminal_kind =
  | Max_turns of
      { turns : int
      ; limit : int
      }
  | Other of string

type http_error =
  | HttpError of
      { code : int
      ; body : string
      }
  | NetworkError of
      { message : string
      ; kind : network_error_kind
      }
  | AcceptRejected of { reason : string }
  (* Signals that a provider kind requires a non-HTTP transport (e.g. a
     CLI subprocess transport for
     [Claude_code]/[Codex_cli]/[Gemini_cli]/[Kimi_cli])
     but the caller did not wire one.  Distinct from [NetworkError] so
     cascades can skip the candidate without counting it as a flaky
     network failure, and so callers see a clear "configuration/wiring
     bug" rather than a cohttp [Unknown scheme None]. *)
  | CliTransportRequired of { kind : string }
  | ProviderTerminal of
      { kind : provider_terminal_kind
      ; message : string
      }

(* ── Internal helpers ──────────────────────────────────────── *)

let ( let* ) = Result.bind

(** Default wall-clock timeout applied to synchronous HTTP operations
    when a clock is supplied ([get_sync], [post_sync]).  Streaming
    variants use this only to bound the connect + initial-response-headers
    phase; body consumption is governed by the caller. *)
let default_http_timeout_s = 60.0

let with_optional_timeout ~clock ~timeout_s f =
  match clock with
  | Some clk -> Eio.Time.with_timeout_exn clk timeout_s f
  | None -> f ()
;;

(* ── Exception → network_error_kind classification ───────── *)

let classify_unix_error = function
  | Unix.ECONNREFUSED -> Connection_refused
  | Unix.ECONNRESET -> Connection_refused
  | Unix.ETIMEDOUT -> Timeout
  | Unix.ENETUNREACH -> Dns_failure
  | Unix.EHOSTUNREACH -> Dns_failure
  | Unix.EMFILE | Unix.ENFILE | Unix.ENOBUFS -> Local_resource_exhaustion
  | Unix.EADDRNOTAVAIL -> Local_resource_exhaustion
  | _ -> Unknown
;;

let parse_uri url =
  try Ok (Uri.of_string url) with
  | Invalid_argument msg ->
    Error
      (NetworkError
         { message = Printf.sprintf "invalid URL %S: %s" url msg; kind = Unknown })
;;

let log_close_failure ~url ~message =
  let json =
    `Assoc
      [ "event", `String "http_client_socket_close_failed"
      ; "url", `String url
      ; "error", `String message
      ]
  in
  Diag.warn "http_client" "%s" (Yojson.Safe.to_string json)
;;

(* Substring check on already-lowered strings. *)
let has_substr haystack needle =
  let hlen = String.length haystack
  and nlen = String.length needle in
  if nlen > hlen
  then false
  else (
    let rec check i =
      if i > hlen - nlen
      then false
      else if String.sub haystack i nlen = needle
      then true
      else check (i + 1)
    in
    check 0)
;;

let classify_by_message msg =
  let m = String.lowercase_ascii msg in
  if has_substr m "connection refused" || has_substr m "connection reset"
  then Connection_refused
  else if has_substr m "timed out" || has_substr m "timeout"
  then Timeout
  else if
    has_substr m "can't assign requested address"
    || has_substr m "too many open files"
    || has_substr m "no buffer space available"
    || has_substr m "eaddrnotavail"
    || has_substr m "emfile"
    || has_substr m "enfile"
  then Local_resource_exhaustion
  else if
    has_substr m "failed to resolve hostname"
    || has_substr m "name resolution"
    || has_substr m "name or service not known"
    || has_substr m "network is unreachable"
    || has_substr m "host is unreachable"
  then Dns_failure
  else if has_substr m "tls" || has_substr m "ssl" || has_substr m "certificate"
  then Tls_error
  else Unknown
;;

let catch_network f =
  try f () with
  | End_of_file -> Error (NetworkError { message = "End_of_file"; kind = End_of_file })
  | Eio.Time.Timeout ->
    Error
      (NetworkError
         { message = "HTTP operation exceeded wall-clock timeout"; kind = Timeout })
  | Unix.Unix_error (code, _, _) as exn ->
    Error
      (NetworkError { message = Printexc.to_string exn; kind = classify_unix_error code })
  | Eio.Io _ as exn ->
    let msg = Printexc.to_string exn in
    Error (NetworkError { message = msg; kind = classify_by_message msg })
  | Sys_error msg ->
    Error (NetworkError { message = msg; kind = classify_by_message msg })
  | Failure msg -> Error (NetworkError { message = msg; kind = classify_by_message msg })
;;

(** Detect errors caused by local resource exhaustion (port/FD limits).
    Cascading to another provider cannot help — the local machine is
    the bottleneck, not the remote server. *)
let is_local_resource_exhaustion = function
  | NetworkError { kind = Local_resource_exhaustion; _ } -> true
  | AcceptRejected _ -> false
  | HttpError _ -> false
  | CliTransportRequired _ -> false
  | NetworkError _ -> false
  | ProviderTerminal _ -> false
;;

(* ── Public API ────────────────────────────────────────────── *)

let add_connection_close headers = ("connection", "close") :: headers

(** Client wrapper that tracks the socket for explicit close.
    The caller provides the concrete URI so host resolution and TLS
    availability can be checked up front and reported as typed errors. *)
let make_closing_client ~sw ~net ~uri =
  let net = (net :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let https = Api_common.make_https () in
  let* host =
    match Uri.host uri with
    | Some host when String.trim host <> "" -> Ok host
    | _ ->
      Error
        (NetworkError
           { message = Printf.sprintf "invalid URL %S: missing host" (Uri.to_string uri)
           ; kind = Unknown
           })
  in
  let service =
    match Uri.port uri with
    | Some port -> Int.to_string port
    | None -> Uri.scheme uri |> Option.value ~default:"http"
  in
  let addr =
    try
      match Eio.Net.getaddrinfo_stream ~service net host with
      | ip :: _ -> Ok ip
      | [] ->
        Error
          (NetworkError
             { message = Printf.sprintf "failed to resolve hostname: %s" host
             ; kind = Dns_failure
             })
    with
    | Eio.Io _ as exn ->
      let msg = Printexc.to_string exn in
      Error (NetworkError { message = msg; kind = classify_by_message msg })
    | Unix.Unix_error (code, _, _) as exn ->
      Error
        (NetworkError
           { message = Printexc.to_string exn; kind = classify_unix_error code })
    | Failure msg ->
      Error (NetworkError { message = msg; kind = classify_by_message msg })
  in
  let tls_wrap =
    match Uri.scheme uri with
    | Some "https" ->
      (match https with
       | Some wrap -> Ok (Some wrap)
       | None ->
         Error
           (NetworkError
              { message =
                  Printf.sprintf
                    "HTTPS requested but TLS not available for %s"
                    (Uri.to_string uri)
              ; kind = Tls_error
              }))
    | _ -> Ok None
  in
  match addr, tls_wrap with
  | (Error _ as e), _ -> e
  | _, (Error _ as e) -> e
  | Ok addr, Ok tls_wrap ->
    (* Track every transport returned by [connect] so switch release can
         close all of them — not just the most recent one.  [cohttp_eio]
         may call [connect] multiple times per client (keep-alive refresh,
         retry after transient error, etc.), and any socket we stop
         referencing leaks its fd and leaves the TCP endpoint in
         CLOSE_WAIT.

         We also store the TLS-wrapped resource (not the raw socket) so
         [Eio.Resource.close] triggers TLS close_notify before the TCP
         layer closes.  Raw-socket close without TLS shutdown causes the
         peer (e.g. GLM / Cloudflare-fronted endpoints) to interpret the
         half-close as "keep waiting" and hold the connection in
         CLOSE_WAIT indefinitely. *)
    let tracked_transports
      : [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t list ref
      =
      ref []
    in
    let connect ~sw:conn_sw _uri =
      let sock = Eio.Net.connect ~sw:conn_sw net addr in
      let transport : [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t =
        match tls_wrap with
        | Some wrap ->
          (wrap uri sock :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
        | None -> (sock :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
      in
      tracked_transports := transport :: !tracked_transports;
      Diag.debug
        "http_client"
        "connect: new transport #%d for %s"
        (List.length !tracked_transports)
        (Uri.to_string uri);
      transport
    in
    let client = Cohttp_eio.Client.make_generic connect in
    Eio.Switch.on_release sw (fun () ->
      let transports = !tracked_transports in
      tracked_transports := [];
      let n = List.length transports in
      if n > 0
      then
        Diag.debug
          "http_client"
          "on_release: closing %d transport(s) for %s"
          n
          (Uri.to_string uri);
      List.iter
        (fun t ->
           try
             Eio.Resource.close t;
             Diag.debug "http_client" "transport closed for %s" (Uri.to_string uri)
           with
           | Eio.Cancel.Cancelled _ as e -> raise e
           | exn ->
             log_close_failure ~url:(Uri.to_string uri) ~message:(Printexc.to_string exn))
        transports);
    Ok client
;;

let get_sync ?clock ?(timeout_s = default_http_timeout_s) ~sw:_ ~net ~url ~headers () =
  catch_network (fun () ->
    Eio.Switch.run
    @@ fun sw ->
    let* uri = parse_uri url in
    let* client = make_closing_client ~sw ~net ~uri in
    let hdr = Http.Header.of_list (add_connection_close headers) in
    with_optional_timeout ~clock ~timeout_s (fun () ->
      let resp, resp_body = Cohttp_eio.Client.get ~sw client ~headers:hdr uri in
      let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let body_str =
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          let _ =
            try
              let buf = Cstruct.create 4096 in
              let rec drain () =
                let _ = Eio.Flow.single_read resp_body buf in
                drain ()
              in
              drain ()
            with
            | _ -> ()
          in
          raise exn
      in
      Ok (code, body_str)))
;;

let post_sync
      ?clock
      ?(timeout_s = default_http_timeout_s)
      ~sw:_
      ~net
      ~url
      ~headers
      ~body
      ()
  =
  catch_network (fun () ->
    Eio.Switch.run
    @@ fun sw ->
    let* uri = parse_uri url in
    let* client = make_closing_client ~sw ~net ~uri in
    (* Explicitly set Content-Length to prevent chunked transfer encoding.
       Ollama's yyjson parser rejects chunked bodies with
       "Value looks like object, but can't find closing '}' symbol". *)
    let headers_with_length =
      ("content-length", string_of_int (String.length body))
      :: add_connection_close headers
    in
    let hdr = Http.Header.of_list headers_with_length in
    with_optional_timeout ~clock ~timeout_s (fun () ->
      let resp, resp_body =
        Cohttp_eio.Client.post
          ~sw
          client
          ~headers:hdr
          ~body:(Cohttp_eio.Body.of_string body)
          uri
      in
      let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let body_str =
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          let _ =
            try
              let buf = Cstruct.create 4096 in
              let rec drain () =
                let _ = Eio.Flow.single_read resp_body buf in
                drain ()
              in
              drain ()
            with
            | _ -> ()
          in
          raise exn
      in
      Ok (code, body_str)))
;;

let post_stream
      ?clock
      ?(connect_timeout_s = default_http_timeout_s)
      ~sw
      ~net
      ~url
      ~headers
      ~body
      ()
  =
  catch_network (fun () ->
    let* uri = parse_uri url in
    let* client = make_closing_client ~sw ~net ~uri in
    let headers_with_length =
      ("content-length", string_of_int (String.length body))
      :: add_connection_close headers
    in
    let hdr = Http.Header.of_list headers_with_length in
    (* Only the connect + initial response headers are bounded; body
       consumption happens in the returned reader and is the caller's
       responsibility to timebox. *)
    let resp, resp_body =
      with_optional_timeout ~clock ~timeout_s:connect_timeout_s (fun () ->
        Cohttp_eio.Client.post
          ~sw
          client
          ~headers:hdr
          ~body:(Cohttp_eio.Body.of_string body)
          uri)
    in
    match Cohttp.Response.status resp with
    | `OK -> Ok (Eio.Buf_read.of_flow ~max_size:Api_common.max_response_body resp_body)
    | status ->
      let code = Cohttp.Code.code_of_status status in
      let body_str =
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          let _ =
            try
              let buf = Cstruct.create 4096 in
              let rec drain () =
                let _ = Eio.Flow.single_read resp_body buf in
                drain ()
              in
              drain ()
            with
            | _ -> ()
          in
          raise exn
      in
      Error (HttpError { code; body = body_str }))
;;

let with_post_stream
      ?clock
      ?(connect_timeout_s = default_http_timeout_s)
      ~net
      ~url
      ~headers
      ~body
      ~f
      ()
  =
  catch_network (fun () ->
    Eio.Switch.run
    @@ fun sw ->
    let* uri = parse_uri url in
    let* client = make_closing_client ~sw ~net ~uri in
    let headers_with_length =
      ("content-length", string_of_int (String.length body))
      :: add_connection_close headers
    in
    let hdr = Http.Header.of_list headers_with_length in
    (* Only bound connect + initial response headers.  Body consumption
       in [f] is the caller's responsibility to timebox. *)
    let resp, resp_body =
      with_optional_timeout ~clock ~timeout_s:connect_timeout_s (fun () ->
        Cohttp_eio.Client.post
          ~sw
          client
          ~headers:hdr
          ~body:(Cohttp_eio.Body.of_string body)
          uri)
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
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          let _ =
            try
              let buf = Cstruct.create 4096 in
              let rec drain () =
                let _ = Eio.Flow.single_read resp_body buf in
                drain ()
              in
              drain ()
            with
            | _ -> ()
          in
          raise exn
      in
      Error (HttpError { code; body = body_str }))
;;

let read_sse ?clock ?idle_timeout ~reader ~on_data () =
  (* SSE keepalive comments (lines starting with ":" per W3C
     EventSource) carry no payload. Skipping them inside the SAME
     [with_timeout_exn] window preserves the idle deadline so a
     provider that emits only keepalives still trips [idle_timeout]
     when no real event arrives. *)
  let is_keepalive_comment line = String.length line > 0 && line.[0] = ':' in
  let read_meaningful_line () =
    let rec inner () =
      let line = Eio.Buf_read.line reader in
      if is_keepalive_comment line then inner () else line
    in
    match clock, idle_timeout with
    | Some c, Some t -> Eio.Time.with_timeout_exn c t inner
    | _ -> inner ()
  in
  let current_event_type = ref None in
  let rec loop () =
    match read_meaningful_line () with
    | line ->
      let len = String.length line in
      if len = 0
      then current_event_type := None
      else if len > 7 && String.sub line 0 7 = "event: "
      then current_event_type := Some (String.sub line 7 (len - 7))
      else if len > 6 && String.sub line 0 6 = "data: "
      then (
        let data = String.sub line 6 (len - 6) in
        on_data ~event_type:!current_event_type data);
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

(** Read NDJSON-formatted lines from a reader (one JSON object per line).
    Skips blank lines so a trailing newline does not yield an empty payload.
    Returns normally on [End_of_file].

    When [clock] and [idle_timeout] are both set, each line read is
    wrapped in [Eio.Time.with_timeout_exn] so a stalled stream raises
    [Eio.Time.Timeout] after [idle_timeout] seconds of silence. *)
let read_ndjson ?clock ?idle_timeout ~reader ~on_line () =
  let read_line () =
    match clock, idle_timeout with
    | Some c, Some t -> Eio.Time.with_timeout_exn c t (fun () -> Eio.Buf_read.line reader)
    | _ -> Eio.Buf_read.line reader
  in
  let rec loop () =
    match read_line () with
    | "" -> loop ()
    | line ->
      on_line line;
      loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let inject_stream_param body_str =
  match Yojson.Safe.from_string body_str with
  | `Assoc fields -> Yojson.Safe.to_string (`Assoc (("stream", `Bool true) :: fields))
  | other -> Yojson.Safe.to_string other
  | exception Yojson.Json_error _ -> body_str
;;

[@@@coverage off]
(* ── catch_network tests ─────────────────────────────── *)

let%test "catch_network maps End_of_file to NetworkError with kind" =
  match catch_network (fun () -> raise End_of_file) with
  | Error (NetworkError { message; kind = End_of_file }) -> message = "End_of_file"
  | _ -> false
;;

let%test "catch_network maps Sys_error to NetworkError" =
  match catch_network (fun () -> raise (Sys_error "broken pipe")) with
  | Error (NetworkError { message; kind = Unknown }) ->
    has_substr (String.lowercase_ascii message) "broken pipe"
  | _ -> false
;;

let%test "catch_network classifies Unix ECONNREFUSED" =
  match
    catch_network (fun () -> raise (Unix.Unix_error (Unix.ECONNREFUSED, "connect", "")))
  with
  | Error (NetworkError { kind = Connection_refused; _ }) -> true
  | _ -> false
;;

let%test "catch_network classifies Unix ETIMEDOUT" =
  match
    catch_network (fun () -> raise (Unix.Unix_error (Unix.ETIMEDOUT, "connect", "")))
  with
  | Error (NetworkError { kind = Timeout; _ }) -> true
  | _ -> false
;;

(* ── classify_unix_error direct tests ──────────────── *)

let%test "classify_unix_error: EMFILE" =
  classify_unix_error Unix.EMFILE = Local_resource_exhaustion
;;

let%test "classify_unix_error: ENFILE" =
  classify_unix_error Unix.ENFILE = Local_resource_exhaustion
;;

let%test "classify_unix_error: ENOBUFS" =
  classify_unix_error Unix.ENOBUFS = Local_resource_exhaustion
;;

let%test "classify_unix_error: EADDRNOTAVAIL" =
  classify_unix_error Unix.EADDRNOTAVAIL = Local_resource_exhaustion
;;

let%test "classify_unix_error: catchall returns Unknown" =
  classify_unix_error Unix.EPIPE = Unknown
;;

let%test "classify_unix_error: ECONNRESET is Connection_refused" =
  classify_unix_error Unix.ECONNRESET = Connection_refused
;;

let%test "classify_unix_error: ENETUNREACH is Dns_failure" =
  classify_unix_error Unix.ENETUNREACH = Dns_failure
;;

let%test "classify_unix_error: EHOSTUNREACH is Dns_failure" =
  classify_unix_error Unix.EHOSTUNREACH = Dns_failure
;;

(* ── is_local_resource_exhaustion tests ──────────────── *)

let%test "resource exhaustion: EADDRNOTAVAIL via Eio" =
  is_local_resource_exhaustion
    (NetworkError
       { message =
           "Eio.Io Unix_error (Can't assign requested address, \"connect\", \"\"), \
            connecting to tcp:128.14.69.121:443"
       ; kind = Local_resource_exhaustion
       })
;;

let%test "resource exhaustion: too many open files" =
  is_local_resource_exhaustion
    (NetworkError { message = "Too many open files"; kind = Local_resource_exhaustion })
;;

let%test "resource exhaustion: EMFILE constant" =
  is_local_resource_exhaustion
    (NetworkError
       { message = "Unix.Unix_error(Unix.EMFILE, \"socket\", \"\")"
       ; kind = Local_resource_exhaustion
       })
;;

let%test "resource exhaustion: ENOBUFS" =
  is_local_resource_exhaustion
    (NetworkError
       { message = "No buffer space available"; kind = Local_resource_exhaustion })
;;

let%test "resource exhaustion: ENFILE constant" =
  is_local_resource_exhaustion
    (NetworkError
       { message = "Unix.Unix_error(Unix.ENFILE, \"socket\", \"\")"
       ; kind = Local_resource_exhaustion
       })
;;

let%test "resource exhaustion: normal connection refused is not" =
  not
    (is_local_resource_exhaustion
       (NetworkError { message = "Connection refused"; kind = Connection_refused }))
;;

let%test "resource exhaustion: HTTP error is not" =
  not (is_local_resource_exhaustion (HttpError { code = 500; body = "internal" }))
;;

let%test "resource exhaustion: DNS failure is not" =
  not
    (is_local_resource_exhaustion
       (NetworkError
          { message = "failed to resolve hostname: example.com"; kind = Dns_failure }))
;;

(* ── classify_by_message tests ───────────────────────── *)

let%test "classify_by_message: connection refused" =
  classify_by_message "Connection refused" = Connection_refused
;;

let%test "classify_by_message: connection refused via Eio" =
  classify_by_message
    "Eio.Io (Unix.Unix_error (Connection refused, connect, 127.0.0.1:443))"
  = Connection_refused
;;

let%test "classify_by_message: timeout" =
  classify_by_message "Connection timed out" = Timeout
;;

let%test "classify_by_message: DNS failure" =
  classify_by_message "failed to resolve hostname: api.example.com" = Dns_failure
;;

let%test "classify_by_message: DNS name or service" =
  classify_by_message "Name or service not known" = Dns_failure
;;

let%test "classify_by_message: TLS error" =
  classify_by_message "TLS handshake failed: certificate verify failed" = Tls_error
;;

let%test "classify_by_message: resource exhaustion" =
  classify_by_message "Too many open files" = Local_resource_exhaustion
;;

let%test "classify_by_message: unknown" = classify_by_message "broken pipe" = Unknown

let%test "classify_by_message: connection reset by peer" =
  classify_by_message "Connection reset by peer" = Connection_refused
;;

let%test "classify_by_message: network unreachable" =
  classify_by_message "Network is unreachable" = Dns_failure
;;

let%test "classify_by_message: host unreachable" =
  classify_by_message "Host is unreachable" = Dns_failure
;;

(* ── read_ndjson idle_timeout tests ──────────────────── *)

let%test "read_ndjson: no clock/idle_timeout preserves default behaviour" =
  Eio_main.run (fun _env ->
    let flow = Eio.Flow.string_source "{\"a\":1}\n{\"b\":2}\n" in
    let reader = Eio.Buf_read.of_flow ~max_size:1024 flow in
    let lines = ref [] in
    read_ndjson ~reader ~on_line:(fun l -> lines := l :: !lines) ();
    List.rev !lines = [ "{\"a\":1}"; "{\"b\":2}" ])
;;

let%test "read_ndjson: idle_timeout fires when stream stalls mid-read" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run
  @@ fun sw ->
  let source, sink = Eio_unix.pipe sw in
  (* Push one line and keep the sink open (never closed, never written
     again) — the second [Eio.Buf_read.line] call will hang. *)
  Eio.Flow.copy_string "{\"a\":1}\n" sink;
  let reader = Eio.Buf_read.of_flow ~max_size:1024 source in
  try
    read_ndjson ~clock ~idle_timeout:0.05 ~reader ~on_line:(fun _ -> ()) ();
    false
  with
  | Eio.Time.Timeout -> true
;;

(* ── read_sse idle_timeout tests ──────────────────────── *)

let%test "read_sse: no clock/idle_timeout preserves default behaviour" =
  Eio_main.run (fun _env ->
    let flow = Eio.Flow.string_source "data: hello\n\ndata: world\n\n" in
    let reader = Eio.Buf_read.of_flow ~max_size:1024 flow in
    let payloads = ref [] in
    read_sse ~reader ~on_data:(fun ~event_type:_ d -> payloads := d :: !payloads) ();
    List.rev !payloads = [ "hello"; "world" ])
;;

let%test "read_sse: idle_timeout fires when stream stalls mid-read" =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run
  @@ fun sw ->
  let source, sink = Eio_unix.pipe sw in
  Eio.Flow.copy_string "data: hello\n" sink;
  let reader = Eio.Buf_read.of_flow ~max_size:1024 source in
  try
    read_sse ~clock ~idle_timeout:0.05 ~reader ~on_data:(fun ~event_type:_ _ -> ()) ();
    false
  with
  | Eio.Time.Timeout -> true
;;
