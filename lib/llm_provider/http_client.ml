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

type stream_idle_state =
  | Awaiting_first_event
  | Awaiting_first_delta
  | Streaming_answer
  | Streaming_thinking
  | Streaming_tool_call
  | Streaming_heartbeat
  | Streaming_substrate
  | Streaming_done
  | Streaming_unknown
[@@deriving yojson, show]

type timeout_phase =
  | Admission
  | Queue
  | First_token
  | Wall_clock
  | Capacity_backpressure
  | Http_operation
  | Non_streaming_body
  | Stream_body
  | Stream_idle of stream_idle_state
  | Provider_step
  | Cli_stdout_idle
  | Caller_budget
  | Unknown_timeout
[@@deriving yojson, show]

(* Provider-internal terminal condition reported via structured exit
   (see .mli for the rationale and the @since note).  Adding a new
   variant rather than overloading [NetworkError] keeps callers from
   counting a provider's own [max_turns] hit as a flaky network. *)
type provider_terminal_kind =
  | Max_turns of
      { turns : int
      ; limit : int
      }
  | Other of string

type provider_failure_scope =
  | Failure_scope_model
  | Failure_scope_account
  | Failure_scope_region
  | Failure_scope_provider
  | Failure_scope_unknown

type provider_failure_kind =
  | Capacity_exhausted of
      { scope : provider_failure_scope
      ; retry_after : float option
      ; model : string option
      }
  | Hard_quota of { retry_after : float option }
  | Capability_mismatch of { capability : string option }
  | Cli_policy_invalid of
      { tool_name : string option
      ; rule : int option
      }
  | Cli_startup_failed of { reason : string }
  | Provider_parse_error of { parser : string option }
  | Unknown_provider_failure of { reason : string option }

type http_error =
  | HttpError of
      { code : int
      ; body : string
      }
  | NetworkError of
      { message : string
      ; kind : network_error_kind
      }
  | TimeoutError of
      { message : string
      ; phase : timeout_phase
      }
  | AcceptRejected of { reason : string }
  (* Signals that a provider kind requires a non-HTTP transport (e.g. a
     CLI subprocess transport for
     [Claude_code]/[Codex]/[Gemini]/[Kimi])
     but the caller did not wire one.  Distinct from [NetworkError] so
     callers can skip the candidate without counting it as a flaky
     network failure, and so callers see a clear "configuration/wiring
     bug" rather than a cohttp [Unknown scheme None]. *)
  | ProviderTerminal of
      { kind : provider_terminal_kind
      ; message : string
      }
  | ProviderFailure of
      { kind : provider_failure_kind
      ; message : string
      }

(* ── Internal helpers ──────────────────────────────────────── *)

let ( let* ) = Result.bind

let provider_failure_scope_to_string = function
  | Failure_scope_model -> "model"
  | Failure_scope_account -> "account"
  | Failure_scope_region -> "region"
  | Failure_scope_provider -> "provider"
  | Failure_scope_unknown -> "unknown"
;;

let provider_failure_kind_to_string = function
  | Capacity_exhausted { scope; _ } ->
    Printf.sprintf "capacity_exhausted:%s" (provider_failure_scope_to_string scope)
  | Hard_quota _ -> "hard_quota"
  | Capability_mismatch { capability = Some capability } ->
    Printf.sprintf "capability_mismatch:%s" capability
  | Capability_mismatch { capability = None } -> "capability_mismatch"
  | Cli_policy_invalid { tool_name = Some tool_name; rule = Some rule } ->
    Printf.sprintf "cli_policy_invalid:rule_%d:%s" rule tool_name
  | Cli_policy_invalid { tool_name = Some tool_name; rule = None } ->
    Printf.sprintf "cli_policy_invalid:%s" tool_name
  | Cli_policy_invalid { tool_name = None; rule = Some rule } ->
    Printf.sprintf "cli_policy_invalid:rule_%d" rule
  | Cli_policy_invalid { tool_name = None; rule = None } -> "cli_policy_invalid"
  | Cli_startup_failed _ -> "cli_startup_failed"
  | Provider_parse_error { parser = Some parser } ->
    Printf.sprintf "provider_parse_error:%s" parser
  | Provider_parse_error { parser = None } -> "provider_parse_error"
  | Unknown_provider_failure { reason = Some reason } ->
    Printf.sprintf "unknown_provider_failure:%s" reason
  | Unknown_provider_failure { reason = None } -> "unknown_provider_failure"
;;

let provider_failure_to_string ~kind ~message =
  let name = provider_failure_kind_to_string kind in
  if String.trim message = "" then name else Printf.sprintf "%s: %s" name message
;;

let stream_idle_state_to_label = function
  | Awaiting_first_event -> "awaiting_first_event"
  | Awaiting_first_delta -> "awaiting_first_delta"
  | Streaming_answer -> "streaming_answer"
  | Streaming_thinking -> "streaming_thinking"
  | Streaming_tool_call -> "streaming_tool_call"
  | Streaming_heartbeat -> "streaming_heartbeat"
  | Streaming_substrate -> "streaming_substrate"
  | Streaming_done -> "streaming_done"
  | Streaming_unknown -> "streaming_unknown"
;;

let timeout_phase_of_stream_idle_state = function
  | Awaiting_first_event | Awaiting_first_delta -> First_token
  | state -> Stream_idle state
;;

let timeout_phase_to_label = function
  | Admission -> "admission"
  | Queue -> "queue"
  | First_token -> "first_token"
  | Wall_clock -> "wall_clock"
  | Capacity_backpressure -> "capacity_backpressure"
  | Http_operation -> "http_operation"
  | Non_streaming_body -> "non_streaming_body"
  | Stream_body -> "stream_body"
  | Stream_idle state ->
    Printf.sprintf "stream_idle:%s" (stream_idle_state_to_label state)
  | Provider_step -> "provider_step"
  | Cli_stdout_idle -> "cli_stdout_idle"
  | Caller_budget -> "caller_budget"
  | Unknown_timeout -> "unknown_timeout"
;;

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
  | Unix.EPIPE -> End_of_file
  | Unix.ETIMEDOUT -> Timeout
  | Unix.ENETUNREACH -> Dns_failure
  | Unix.EHOSTUNREACH -> Dns_failure
  | Unix.EMFILE | Unix.ENFILE | Unix.ENOBUFS -> Local_resource_exhaustion
  | Unix.EADDRNOTAVAIL -> Local_resource_exhaustion
  | unclassified_unix_error ->
    let (_ : Unix.error) = unclassified_unix_error in
    Unknown
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

(* Empirically measured (2026-05-31) against RunPod's *.proxy.runpod.net edge:
   a single request header LINE >= 8192 bytes is rejected by the cloudflare edge
   with an opaque "400 Bad Request" (server: cloudflare, empty body, cf-ray)
   BEFORE the request reaches the origin. The binding limit is per-header-line,
   NOT the header total — 20 x 500B headers (10 KB total) passed, while one
   8192B header did not. Body size (up to 2 MB) and malformed header values did
   not reproduce it. *)
let cdn_per_header_limit_bytes = 8192

(* key + ": " + value + CRLF — the on-wire size of one header line. *)
let header_line_bytes (key, value) = String.length key + String.length value + 4

(* Request header size profile (name + on-wire bytes, largest first). VALUES ARE
   OMITTED: header values may carry credentials (Authorization, tokens), so only
   sizes are logged. *)
let header_size_profile headers =
  headers
  |> List.map (fun ((k, _) as h) -> k, header_line_bytes h)
  |> List.sort (fun (_, a) (_, b) -> compare b a)
  |> List.map (fun (k, n) -> `Assoc [ "name", `String k; "bytes", `Int n ])
;;

let max_single_header_bytes headers =
  List.fold_left (fun acc h -> max acc (header_line_bytes h)) 0 headers
;;

(* On a 4xx response, log the request's header size profile and the response's
   edge signature (server, cf-ray). A 4xx with an empty/opaque body and a
   "cloudflare" server indicates an edge rejection — commonly a single header
   line over [cdn_per_header_limit_bytes] — rather than an origin-level error.
   This names the offending header WHEN a real failure recurs: header contents
   are runtime-dependent and not knowable statically, so a pre-send size guess
   either never fires (small headers) or false-fires on benign many-small-header
   requests the edge accepts. *)
let profile_headers_on_client_error ~url ~code ~resp_headers request_headers =
  if code >= 400 && code < 500
  then (
    let server = Http.Header.get resp_headers "server" in
    let cf_ray = Http.Header.get resp_headers "cf-ray" in
    let opt = function
      | Some s -> `String s
      | None -> `Null
    in
    let total =
      List.fold_left (fun acc h -> acc + header_line_bytes h) 0 request_headers
    in
    let json =
      `Assoc
        [ "event", `String "http_client_4xx_request_header_profile"
        ; "url", `String url
        ; "status", `Int code
        ; "response_server", opt server
        ; "cf_ray", opt cf_ray
        ; "request_header_count", `Int (List.length request_headers)
        ; "total_request_header_bytes", `Int total
        ; "max_single_header_bytes", `Int (max_single_header_bytes request_headers)
        ; "cdn_per_header_limit_bytes", `Int cdn_per_header_limit_bytes
        ; "header_sizes", `List (header_size_profile request_headers)
        ; ( "note"
          , `String
              "4xx from an LLM endpoint. Header VALUES omitted (may carry credentials); \
               sizes only. A cloudflare/RunPod edge rejects a single header line over \
               cdn_per_header_limit_bytes with an opaque 400 before the origin — compare \
               max_single_header_bytes." )
        ]
    in
    Diag.warn "http_client" "%s" (Yojson.Safe.to_string json))
;;

let%test "header_line_bytes = key + value + 4 (\": \" + CRLF)" =
  (* "x-runtime-mcp" = 13, "abc" = 3, + 4 = 20 *)
  header_line_bytes ("x-runtime-mcp", "abc") = 20
;;

let%test "header_size_profile orders the largest header first" =
  match
    header_size_profile
      [ "small", "v"; "big", String.make 100 'x'; "mid", String.make 20 'y' ]
  with
  | `Assoc (("name", `String "big") :: _) :: _ -> true
  | _ -> false
;;

(* The edge checks per-header-line size, not the total. Many small headers whose
   total far exceeds the limit are accepted; only the per-header max matters. *)
let%test "max_single_header_bytes ignores the total, tracks the largest line" =
  let many_small =
    List.init 20 (fun i -> Printf.sprintf "x-h%d" i, String.make 500 'y')
  in
  max_single_header_bytes many_small < cdn_per_header_limit_bytes
;;

let%test "max_single_header_bytes flags a single oversized header line" =
  max_single_header_bytes [ "x-big", String.make 9000 'x' ] > cdn_per_header_limit_bytes
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
  else if has_substr m "connection closed by peer" || has_substr m "broken pipe"
  then End_of_file
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

let https_init_error_network_kind = function
  | Api_common.Ca_certs_unavailable msg ->
    let m = String.lowercase_ascii msg in
    (match classify_by_message msg with
     | Local_resource_exhaustion -> Local_resource_exhaustion
     | Connection_refused | Dns_failure | Tls_error | Timeout | End_of_file | Unknown ->
       if has_substr m "empty trust anchors" || has_substr m "no trust anchors"
       then Local_resource_exhaustion
       else Tls_error)
  | Api_common.Tls_config_unavailable _ -> Tls_error
;;

(** Classify a network/timeout exception into an [http_error]. A timeout
    is classified as [Http_operation] — accurate for the connect/headers
    phase; body-phase timeouts are intercepted before this function (see
    {!with_post_stream}) so the caller can attach a phase-accurate label. *)
let classify_network_exn (e : exn) =
  match e with
  | End_of_file -> Some (NetworkError { message = "End_of_file"; kind = End_of_file })
  | Eio.Time.Timeout ->
    Some
      (TimeoutError
         { message = "HTTP operation exceeded wall-clock timeout"
         ; phase = Http_operation
         })
  | Unix.Unix_error (code, _, _) as exn ->
    Some
      (NetworkError { message = Printexc.to_string exn; kind = classify_unix_error code })
  | Eio.Io _ as exn ->
    let msg = Printexc.to_string exn in
    Some (NetworkError { message = msg; kind = classify_by_message msg })
  | Sys_error msg -> Some (NetworkError { message = msg; kind = classify_by_message msg })
  | Failure msg -> Some (NetworkError { message = msg; kind = classify_by_message msg })
  | _ -> None
;;

let catch_network f =
  try f () with
  | exn ->
    (match classify_network_exn exn with
     | Some e -> Error e
     | None -> raise exn)
;;

(* ── classify_network_exn / phase-mapping invariants ─────────── *)

let%test "classify_network_exn: Eio.Time.Timeout is Http_operation" =
  (* A timeout classified HERE is a connect/headers-phase timeout
     ([catch_network] wraps only that phase in with_post_stream). Body-phase
     timeouts are intercepted before this point, so this stays accurate. *)
  match classify_network_exn Eio.Time.Timeout with
  | Some (TimeoutError { phase = Http_operation; _ }) -> true
  | _ -> false
;;

let%test "classify_network_exn: non-network exn is None (propagates)" =
  classify_network_exn Not_found = None
;;

let%test "timeout_phase_of_stream_idle_state: Awaiting_first_* -> First_token" =
  (* Prefill (no first chunk yet) must surface as [First_token], never
     [Http_operation]. Guards the phase-accuracy fix. *)
  timeout_phase_of_stream_idle_state Awaiting_first_event = First_token
  && timeout_phase_of_stream_idle_state Awaiting_first_delta = First_token
;;

(** Detect errors caused by local resource exhaustion (port/FD limits).
    Cascading to another provider cannot help — the local machine is
    the bottleneck, not the remote server. *)
let is_local_resource_exhaustion = function
  | NetworkError { kind = Local_resource_exhaustion; _ } -> true
  | TimeoutError _ -> false
  | AcceptRejected _ -> false
  | HttpError _ -> false
  | NetworkError _ -> false
  | ProviderTerminal _ -> false
  | ProviderFailure _ -> false
;;

(* ── Public API ────────────────────────────────────────────── *)

let add_connection_close headers = ("connection", "close") :: headers

(** Client wrapper that tracks the socket for explicit close.
    The caller provides the concrete URI so host resolution and TLS
    availability can be checked up front and reported as typed errors. *)
let make_closing_client ~sw ~net ~uri =
  let net = (net :> [ `Generic ] Eio.Net.ty Eio.Resource.t) in
  let https = Api_common.make_https_result () in
  let* host =
    match Uri.host uri with
    | Some host when String.trim host <> "" -> Ok host
    | Some _ | None ->
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
       | Ok wrap -> Ok (Some wrap)
       | Error reason ->
         Error
           (NetworkError
              { message =
                  Printf.sprintf
                    "HTTPS requested but TLS not available for %s: %s"
                    (Uri.to_string uri)
                    (Api_common.https_init_error_to_string reason)
              ; kind = https_init_error_network_kind reason
              }))
    | Some "http" | Some _ | None -> Ok None
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
         peer (e.g. Glm / Cloudflare-fronted endpoints) to interpret the
         half-close as "keep waiting" and hold the connection in
         CLOSE_WAIT indefinitely. *)
    let tracked_transports
      : [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t list Atomic.t
      =
      Atomic.make []
    in
    let connect ~sw:conn_sw _uri =
      let sock = Eio.Net.connect ~sw:conn_sw net addr in
      let transport : [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t =
        match tls_wrap with
        | Some wrap ->
          (wrap uri sock :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
        | None -> (sock :> [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t)
      in
      let rec push () =
        let prev = Atomic.get tracked_transports in
        if Atomic.compare_and_set tracked_transports prev (transport :: prev)
        then ()
        else push ()
      in
      push ();
      Diag.debug
        "http_client"
        "connect: new transport #%d for %s"
        (List.length (Atomic.get tracked_transports))
        (Uri.to_string uri);
      transport
    in
    let client = Cohttp_eio.Client.make_generic connect in
    Eio.Switch.on_release sw (fun () ->
      let transports = Atomic.exchange tracked_transports [] in
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

let drain_response_body ?clock ?(timeout_s = 30.0) resp_body =
  let buf = Cstruct.create 4096 in
  let rec drain () =
    let _ = Eio.Flow.single_read resp_body buf in
    drain ()
  in
  let drain_with_timeout () =
    match clock with
    | Some clk ->
      (try Eio.Time.with_timeout_exn clk timeout_s drain with
       | Eio.Time.Timeout -> ())
    | None -> drain ()
  in
  try drain_with_timeout () with
  | End_of_file -> ()
  | Eio.Time.Timeout -> ()
  | Unix.Unix_error (code, _, _) ->
    (match classify_unix_error code with
     | Connection_refused
     | Dns_failure
     | Tls_error
     | Timeout
     | Local_resource_exhaustion
     | End_of_file
     | Unknown -> ())
  | Eio.Io _ as e ->
    Diag.warn "http_client" "drain_response_body: %s" (Printexc.to_string e);
    ()
  | Sys_error msg ->
    Diag.warn "http_client" "drain_response_body: sys_error %s" msg;
    ()
  | Failure msg ->
    Diag.warn "http_client" "drain_response_body: failure %s" msg;
    ()
  | Invalid_argument msg ->
    Diag.warn "http_client" "drain_response_body: invalid_arg %s" msg;
    ()
  (* Re-raise cancellation so a fiber cancelled mid-drain unwinds instead of
     being absorbed by the catch-all below (structured concurrency). Mirrors the
     transport-close handler in this module. *)
  | Eio.Cancel.Cancelled _ as e -> raise e
  | drain_failure ->
    Diag.warn "http_client" "drain_response_body: %s" (Printexc.to_string drain_failure);
    ()
;;

let get_sync ?clock ?(timeout_s = default_http_timeout_s) ~sw ~net ~url ~headers () =
  catch_network (fun () ->
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
          drain_response_body ?clock resp_body;
          raise exn
      in
      Ok (code, body_str)))
;;

let post_sync ?clock ?(timeout_s = default_http_timeout_s) ~sw ~net ~url ~headers ~body ()
  =
  catch_network (fun () ->
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
      profile_headers_on_client_error
        ~url
        ~code
        ~resp_headers:(Cohttp.Response.headers resp)
        headers_with_length;
      let body_str =
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          drain_response_body ?clock resp_body;
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
      profile_headers_on_client_error
        ~url
        ~code
        ~resp_headers:(Cohttp.Response.headers resp)
        headers_with_length;
      let body_str =
        try
          Eio.Buf_read.(
            of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
        with
        | exn ->
          drain_response_body resp_body;
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
  Eio.Switch.run
  @@ fun sw ->
  (* Phase 1a: connect + post + response headers, bounded by
     [connect_timeout_s]. Cohttp_eio.Client.post returns once headers are
     parsed (body is a lazy flow), so wrapping only this stage in
     [catch_network] keeps a connect / header-phase stall as
     [TimeoutError { phase = Http_operation }] without absorbing body-phase
     timeouts (first-token / prefill wait, inter-chunk idle). *)
  let post_result =
    catch_network (fun () ->
      let* uri = parse_uri url in
      let* client = make_closing_client ~sw ~net ~uri in
      let headers_with_length =
        ("content-length", string_of_int (String.length body))
        :: add_connection_close headers
      in
      let hdr = Http.Header.of_list headers_with_length in
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
        Ok reader
      | status ->
        let code = Cohttp.Code.code_of_status status in
        profile_headers_on_client_error
          ~url
          ~code
          ~resp_headers:(Cohttp.Response.headers resp)
          headers_with_length;
        let body_str =
          try
            Eio.Buf_read.(
              of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
          with
          | exn ->
            drain_response_body resp_body;
            raise exn
        in
        Error (HttpError { code; body = body_str }))
  in
  (* Phase 1b: body consumption. Deliberately OUTSIDE [catch_network]: a
     body-phase [Eio.Time.Timeout] is phase-distinct from the connect /
     headers phase and must not be mislabelled [Http_operation]. [f] owns
     phase-aware timeout handling and must convert [Eio.Time.Timeout] into a
     typed [Error] (see [Complete_stream.body_logic] and the Streaming
     callers). A body-phase timeout that [f] lets propagate escapes this
     function as the raw exception. *)
  match post_result with
  | Error e -> Error e
  | Ok reader ->
    (* Body consumption. [Eio.Time.Timeout] propagates so the caller
        phases it (prefill → [First_token], inter-chunk → [Stream_idle]).
        Other network exceptions classify like {!catch_network} so a
        body-phase I/O failure still surfaces as a typed [NetworkError]
        instead of escaping as a raw exception. *)
    (try Ok (f reader) with
     | Eio.Time.Timeout ->
       (* Body-phase timeout. Stream-state-aware callers ([Complete_stream])
          catch this inside [f] and emit the precise [First_token] /
          [Stream_idle] phase. Callers that let it propagate (e.g.
          [Streaming]) get [Unknown_timeout] as a safe default rather
          than it being mislabelled [Http_operation] (the connect /
          headers phase, which a body-phase timeout is not). *)
       Error
         (TimeoutError
            { message = "stream body timed out (awaiting first token / inter-chunk idle)"
            ; phase = Unknown_timeout
            })
     | exn ->
       (match classify_network_exn exn with
        | Some e -> Error e
        | None -> raise exn))
;;

(* One W3C EventSource line, parsed per spec (§9.2.6 event stream
   interpretation):
   - empty line: event dispatch boundary
   - line starting with ':': comment (keepalive)
   - otherwise "name[:[ ]value]": a field; exactly one leading space is
     stripped from the value, and a line with no ':' is a field with an
     empty value.
   The previous implementation matched the literal prefixes "event: " /
   "data: " with index arithmetic, silently dropping spec-valid lines
   like "data:foo" (no space after the colon) — a provider or proxy
   that omits the optional space would make the whole stream vanish
   without a trace. *)
type sse_line =
  | Sse_blank
  | Sse_comment
  | Sse_field of string * string

let parse_sse_line line =
  if String.length line = 0
  then Sse_blank
  else (
    match String.index_opt line ':' with
    | Some 0 -> Sse_comment
    | None -> Sse_field (line, "")
    | Some i ->
      let value_start =
        if String.length line > i + 1 && line.[i + 1] = ' ' then i + 2 else i + 1
      in
      Sse_field
        ( String.sub line 0 i
        , String.sub line value_start (String.length line - value_start) ))
;;

let require_clock_when_idle ~site ~clock ~idle_timeout =
  match clock, idle_timeout with
  | None, Some _ ->
    (* Fail-loud contract: a configured idle deadline with no clock used
       to silently disarm and leave a stalled stream blocking forever
       (the read_sse idle-disarm bug family). Misconfiguration must fail
       at the call site, not at 3 a.m. as a hung fiber. *)
    invalid_arg
      (site
       ^ ": idle_timeout is set but no clock was supplied — the idle deadline would be \
          silently disarmed (pass ?clock, or drop ?idle_timeout)")
  | Some _, _ | None, None -> ()
;;

let read_sse ?clock ?idle_timeout ~reader ~on_data () =
  require_clock_when_idle ~site:"read_sse" ~clock ~idle_timeout;
  (* SSE keepalive comments carry no payload. Skipping them inside the
     SAME [with_timeout_exn] window preserves the idle deadline so a
     provider that emits only keepalives still trips [idle_timeout]
     when no real event arrives. *)
  let read_meaningful_line () =
    let rec inner () =
      match parse_sse_line (Eio.Buf_read.line reader) with
      | Sse_comment -> inner ()
      | (Sse_blank | Sse_field _) as parsed -> parsed
    in
    match clock, idle_timeout with
    | Some c, Some t -> Eio.Time.with_timeout_exn c t inner
    | Some _, None | None, None -> inner ()
    | None, Some _ ->
      (* Rejected by [require_clock_when_idle] above. *)
      assert false
  in
  let current_event_type = ref None in
  let rec loop () =
    match read_meaningful_line () with
    | Sse_blank ->
      current_event_type := None;
      loop ()
    | Sse_comment ->
      (* Filtered inside [read_meaningful_line]. *)
      loop ()
    | Sse_field ("event", value) ->
      current_event_type := Some value;
      loop ()
    | Sse_field ("data", value) ->
      (* Empty data is dispatched rather than dropped: the downstream
         accumulator surfaces unparsable payloads as SSEParseFailed
         events, which beats making protocol garbage invisible here. *)
      on_data ~event_type:!current_event_type value;
      loop ()
    | Sse_field (_, _) ->
      (* "id" / "retry" are valid EventSource fields this client
         deliberately does not use (no reconnect support); unknown
         field names are ignored per spec. *)
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
  require_clock_when_idle ~site:"read_ndjson" ~clock ~idle_timeout;
  let read_line () =
    match clock, idle_timeout with
    | Some c, Some t -> Eio.Time.with_timeout_exn c t (fun () -> Eio.Buf_read.line reader)
    | Some _, None | None, None -> Eio.Buf_read.line reader
    | None, Some _ ->
      (* Rejected by [require_clock_when_idle] above. *)
      assert false
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
  | `Assoc fields ->
    let without_existing = List.filter (fun (k, _) -> k <> "stream") fields in
    Yojson.Safe.to_string (`Assoc (("stream", `Bool true) :: without_existing))
  | other -> Yojson.Safe.to_string other
  | exception Yojson.Json_error _ -> body_str
;;

(* OpenAI streaming omits the [usage] object on every chunk unless the
   request sets [stream_options.include_usage = true], at which point the
   provider sends a final SSE chunk carrying [usage] with an empty
   [choices] array. Without this flag, OpenAI-compatible streaming turns
   report zero token usage. Anthropic/Ollama/Gemini carry usage natively
   and must NOT receive this field. Mirrors [inject_stream_param]'s
   JSON-manipulation style: drop any caller-supplied [stream_options]
   before re-adding so the flag cannot be double-injected, and leave a
   malformed/non-object body untouched. *)
let inject_stream_options_include_usage body_str =
  match Yojson.Safe.from_string body_str with
  | `Assoc fields ->
    let without_existing = List.filter (fun (k, _) -> k <> "stream_options") fields in
    Yojson.Safe.to_string
      (`Assoc
          (("stream_options", `Assoc [ "include_usage", `Bool true ]) :: without_existing))
  | other -> Yojson.Safe.to_string other
  | exception Yojson.Json_error _ -> body_str
;;

[@@@coverage off]
(* ── catch_network tests ─────────────────────────────── *)

let%test "catch_network maps End_of_file to NetworkError with kind" =
  match catch_network (fun () -> raise End_of_file) with
  | Error (NetworkError { message; kind = End_of_file }) -> message = "End_of_file"
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
;;

let%test "catch_network maps Sys_error to NetworkError" =
  match catch_network (fun () -> raise (Sys_error "broken pipe")) with
  | Error (NetworkError { message; kind = End_of_file }) ->
    has_substr (String.lowercase_ascii message) "broken pipe"
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
;;

let%test "catch_network classifies Unix ECONNREFUSED" =
  match
    catch_network (fun () -> raise (Unix.Unix_error (Unix.ECONNREFUSED, "connect", "")))
  with
  | Error (NetworkError { kind = Connection_refused; _ }) -> true
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
;;

let%test "catch_network classifies Unix ETIMEDOUT" =
  match
    catch_network (fun () -> raise (Unix.Unix_error (Unix.ETIMEDOUT, "connect", "")))
  with
  | Error (NetworkError { kind = Timeout; _ }) -> true
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
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

let%test "classify_unix_error: EPIPE is End_of_file" =
  classify_unix_error Unix.EPIPE = End_of_file
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

let%test "classify_by_message: broken pipe" =
  classify_by_message "broken pipe" = End_of_file
;;

let%test "classify_by_message: connection closed by peer" =
  classify_by_message "connection closed by peer" = End_of_file
;;

let%test "classify_by_message: connection reset by peer" =
  classify_by_message "Connection reset by peer" = Connection_refused
;;

let%test "classify_by_message: network unreachable" =
  classify_by_message "Network is unreachable" = Dns_failure
;;

let%test "classify_by_message: host unreachable" =
  classify_by_message "Host is unreachable" = Dns_failure
;;

let%test "https_init_error_network_kind: empty trust anchors are local" =
  https_init_error_network_kind
    (Api_common.Ca_certs_unavailable "ca-certs: empty trust anchors")
  = Local_resource_exhaustion
;;

let%test "https_init_error_network_kind: TLS config remains TLS" =
  https_init_error_network_kind (Api_common.Tls_config_unavailable "unsupported protocol")
  = Tls_error
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
