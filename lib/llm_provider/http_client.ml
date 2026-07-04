(** HTTP client for LLM provider endpoints.

    Wraps Eio + cohttp-eio with TLS. All network and HTTP-level errors
    are captured as {!http_error} so callers do not need [try/with].

    Each synchronous request without a [connection_cache] creates a one-shot
    client and explicitly closes the underlying TCP connection as soon as the
    response body is fully consumed. With a cache, connections are bound to
    the cache's switch and reused until eviction or switch release.

    @since 0.45.0 *)

open Result_syntax

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

let known_network_error_kind = function
  | Unknown -> None
  | ( Connection_refused
    | Dns_failure
    | Tls_error
    | Timeout
    | Local_resource_exhaustion
    | End_of_file ) as kind -> Some kind
;;

(* For composite errors, prefer kinds that should not be retried (local
   resource exhaustion and TLS errors) over transient network failures.
   This mirrors the severity ordering rather than the retry policy itself. *)
let network_error_kind_is_non_retryable = function
  | Local_resource_exhaustion | Tls_error -> true
  | Connection_refused | Dns_failure | Timeout | End_of_file | Unknown -> false
;;

let classify_eio_backend_error = function
  | Eio_unix.Unix_error (code, _, _) -> Some (classify_unix_error code)
  | _ ->
    (* Keep control flow on public typed backend constructors only. tls-eio's
       socket-closed backend is private in its .ml, so OAS cannot match it
       soundly; the surrounding [Connection_reset] fallback classifies that
       path as [End_of_file]. *)
    None
;;

let classify_eio_net_error = function
  | Eio.Net.Connection_reset backend ->
    Option.value (classify_eio_backend_error backend) ~default:End_of_file
  | Eio.Net.Connection_failure (Eio.Net.Refused backend) ->
    Option.value (classify_eio_backend_error backend) ~default:Connection_refused
  | Eio.Net.Connection_failure Eio.Net.Timeout -> Timeout
  | Eio.Net.Connection_failure Eio.Net.No_matching_addresses -> Dns_failure
;;

let rec classify_eio_error = function
  | Eio.Net.E net_error -> classify_eio_net_error net_error
  | Eio.Exn.X backend ->
    Option.value (classify_eio_backend_error backend) ~default:Unknown
  | Eio.Exn.Multiple_io errors ->
    let kinds =
      List.filter_map
        (fun (err, _, _) -> classify_eio_error err |> known_network_error_kind)
        errors
    in
    (match List.find_opt network_error_kind_is_non_retryable kinds with
     | Some kind -> kind
     | None ->
       (match kinds with
        | kind :: _ -> kind
        | [] -> Unknown))
  | _ -> Unknown
;;

let network_error_of_eio err exn =
  NetworkError { message = Printexc.to_string exn; kind = classify_eio_error err }
;;

let unknown_network_error msg = NetworkError { message = msg; kind = Unknown }

let https_init_error_network_kind = function
  | Api_common.Ca_certs_unavailable _ -> Tls_error
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
  | Eio.Io (err, _) as exn -> Some (network_error_of_eio err exn)
  | (Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _) as exn ->
    Some (NetworkError { message = Printexc.to_string exn; kind = Tls_error })
  | Sys_error msg -> Some (NetworkError { message = msg; kind = Unknown })
  | Failure msg -> Some (NetworkError { message = msg; kind = Unknown })
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

type connection = [ `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t

let add_connection_close headers = ("connection", "close") :: headers

let maybe_add_connection_close ?cache headers =
  match cache with
  | Some _ -> headers
  | None -> add_connection_close headers
;;

(* ── Connection cache ──────────────────────────────────────── *)

(** Host identity for connection reuse. The cache intentionally ignores
    path, query, and auth: a connection to the same origin can carry
    requests with different URLs and headers. *)
module Cache_key = struct
  type t =
    { scheme : string
    ; host : string
    ; port : int
    }

  let compare a b =
    match String.compare a.scheme b.scheme with
    | 0 ->
      (match String.compare a.host b.host with
       | 0 -> Int.compare a.port b.port
       | n -> n)
    | n -> n
  ;;

  let default_port_for_scheme scheme =
    match scheme with
    | "https" -> 443
    | _ -> 80
  ;;

  let of_uri uri =
    let scheme = Uri.scheme uri |> Option.value ~default:"http" in
    let host =
      match Uri.host uri with
      | Some "" | None -> "localhost"
      | Some h -> h
    in
    let port = Uri.port uri |> Option.value ~default:(default_port_for_scheme scheme) in
    { scheme; host; port }
  ;;

  let%test "Cache_key.of_uri defaults to https port 443" =
    let key = of_uri (Uri.of_string "https://example.com/path") in
    key.scheme = "https" && key.host = "example.com" && key.port = 443
  ;;

  let%test "Cache_key.of_uri defaults to http port 80" =
    let key = of_uri (Uri.of_string "http://example.com/path") in
    key.scheme = "http" && key.host = "example.com" && key.port = 80
  ;;

  let%test "Cache_key.of_uri preserves explicit port" =
    let key = of_uri (Uri.of_string "https://example.com:8443/path") in
    key.port = 8443
  ;;
end

module Cache_map = Map.Make (Cache_key)

type cache_entry =
  { connection : connection
  ; last_used_at : float
  }

type cache_stats =
  { idle_per_host : (string * int) list
  ; total_idle : int
  ; reuse_count_total : int
  ; create_count_total : int
  }

type cache =
  { sw : Eio.Switch.t
  ; mu : Eio.Mutex.t
  ; max_idle_per_host : int
  ; idle_ttl_seconds : float
  ; mutable entries : cache_entry list Cache_map.t
  ; reuse_count_total : int Atomic.t
  ; create_count_total : int Atomic.t
  ; stop : bool Atomic.t
  ; now : unit -> float
  }

let create_cache ~sw ?clock ?(max_idle_per_host = 8) ?(idle_ttl_seconds = 60.0) () : cache
  =
  if max_idle_per_host < 1
  then invalid_arg "Http_client.create_cache: max_idle_per_host must be >= 1";
  if idle_ttl_seconds <= 0.0
  then invalid_arg "Http_client.create_cache: idle_ttl_seconds must be > 0";
  let cache =
    let now =
      match clock with
      | Some clock -> fun () -> Eio.Time.now clock
      | None -> Unix.gettimeofday
    in
    { sw
    ; mu = Eio.Mutex.create ()
    ; max_idle_per_host
    ; idle_ttl_seconds
    ; entries = Cache_map.empty
    ; reuse_count_total = Atomic.make 0
    ; create_count_total = Atomic.make 0
    ; stop = Atomic.make false
    ; now
    }
  in
  Eio.Switch.on_release sw (fun () ->
    Atomic.set cache.stop true;
    let leftover =
      Eio.Mutex.use_rw ~protect:true cache.mu (fun () ->
        let all =
          Cache_map.fold
            (fun _ entries acc -> List.rev_append entries acc)
            cache.entries
            []
        in
        cache.entries <- Cache_map.empty;
        all)
    in
    Eio.Cancel.protect (fun () ->
      List.iter
        (fun e ->
           try Eio.Resource.close e.connection with
           | Eio.Cancel.Cancelled _ as exn -> raise exn
           | _ -> ())
        leftover));
  (* Eviction fiber: reap entries past [idle_ttl_seconds] when a clock
     is supplied. Without a clock the cache still works; stale entries
     are closed on switch release. *)
  (match clock with
   | Some clock ->
     Eio.Fiber.fork ~sw (fun () ->
       let rec loop () =
         if Atomic.get cache.stop
         then ()
         else (
           Eio.Time.sleep clock (cache.idle_ttl_seconds /. 2.0);
           let now = cache.now () in
           let expired =
             Eio.Mutex.use_rw ~protect:true cache.mu (fun () ->
               let expired = ref [] in
               let remaining =
                 Cache_map.map
                   (List.filter (fun e ->
                      if now -. e.last_used_at > cache.idle_ttl_seconds
                      then (
                        expired := e :: !expired;
                        false)
                      else true))
                   cache.entries
               in
               cache.entries <- remaining;
               !expired)
           in
           Eio.Cancel.protect (fun () ->
             List.iter
               (fun e ->
                  try Eio.Resource.close e.connection with
                  | Eio.Cancel.Cancelled _ as exn -> raise exn
                  | _ -> ())
               expired);
           loop ())
       in
       loop ())
   | None -> ());
  cache
;;

let cache_stats (cache : cache) : cache_stats =
  Eio.Mutex.use_ro cache.mu (fun () ->
    let idle_per_host =
      Cache_map.bindings cache.entries
      |> List.map (fun ({ Cache_key.scheme; host; port }, v) ->
        Printf.sprintf "%s://%s:%d" scheme host port, List.length v)
    in
    let total_idle = List.fold_left (fun acc (_, n) -> acc + n) 0 idle_per_host in
    { idle_per_host
    ; total_idle
    ; reuse_count_total = Atomic.get cache.reuse_count_total
    ; create_count_total = Atomic.get cache.create_count_total
    })
;;

(** Find a warm client for [uri] and remove it from the cache so it is
    owned by the caller. Returns [None] if no entry is available. *)
let cache_take (cache : cache) uri : cache_entry option =
  if Atomic.get cache.stop
  then None
  else (
    let key = Cache_key.of_uri uri in
    Eio.Mutex.use_rw ~protect:true cache.mu (fun () ->
      match Cache_map.find_opt key cache.entries with
      | Some (e :: rest) ->
        cache.entries <- Cache_map.add key rest cache.entries;
        Atomic.incr cache.reuse_count_total;
        Some e
      | _ -> None))
;;

(** Park a client back into the cache, or close it if the per-host cap
    is reached. [close] is the entry's own shutdown function. *)
let cache_return (cache : cache) uri (entry : cache_entry) : unit =
  if Atomic.get cache.stop
  then Eio.Resource.close entry.connection
  else (
    let key = Cache_key.of_uri uri in
    let now = cache.now () in
    let entry = { entry with last_used_at = now } in
    let parked =
      Eio.Mutex.use_rw ~protect:true cache.mu (fun () ->
        let existing = Cache_map.find_opt key cache.entries |> Option.value ~default:[] in
        if List.length existing < cache.max_idle_per_host
        then (
          cache.entries <- Cache_map.add key (entry :: existing) cache.entries;
          true)
        else false)
    in
    if not parked then Eio.Resource.close entry.connection)
;;

(** Resolve the origin for [uri] and prepare the TLS wrapper if needed.
    The result is reused by both one-shot clients and cached connections. *)
let resolve_origin net uri =
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
  let* addr =
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
    | Eio.Io (err, _) as exn -> Error (network_error_of_eio err exn)
    | Unix.Unix_error (code, _, _) as exn ->
      Error
        (NetworkError
           { message = Printexc.to_string exn; kind = classify_unix_error code })
    | Failure msg -> Error (unknown_network_error msg)
  and* tls_wrap =
    match Uri.scheme uri with
    | Some "https" ->
      let wrap_error reason =
        NetworkError
          { message =
              Printf.sprintf
                "HTTPS requested but TLS not available for %s: %s"
                (Uri.to_string uri)
                (Api_common.https_init_error_to_string reason)
          ; kind = https_init_error_network_kind reason
          }
      in
      let+ wrap = Result.map_error wrap_error https in
      Some wrap
    | Some "http" | Some _ | None -> Ok None
  in
  Ok (net, addr, tls_wrap)
;;

(** Build a reusable client with explicit lifetime control.
    Returns [Ok (client, close)] where [close] shuts down all transports
    created by this client. The client is NOT bound to any switch; the
    caller decides when to close it or park it in a cache. *)
let make_client ~net ~uri =
  let+ net, addr, tls_wrap = resolve_origin net uri in
  let tracked_transports : connection list Atomic.t = Atomic.make [] in
  let connect ~sw:conn_sw _uri =
    let sock = Eio.Net.connect ~sw:conn_sw net addr in
    let transport : connection =
      match tls_wrap with
      | Some wrap -> (wrap uri sock :> connection)
      | None -> (sock :> connection)
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
  let close () =
    let transports = Atomic.exchange tracked_transports [] in
    let n = List.length transports in
    if n > 0
    then
      Diag.debug
        "http_client"
        "close: closing %d transport(s) for %s"
        n
        (Uri.to_string uri);
    Eio.Cancel.protect (fun () ->
      List.iter
        (fun t ->
           try
             Eio.Resource.close t;
             Diag.debug "http_client" "transport closed for %s" (Uri.to_string uri)
           with
           | Eio.Cancel.Cancelled _ as e -> raise e
           | exn ->
             log_close_failure ~url:(Uri.to_string uri) ~message:(Printexc.to_string exn))
        transports)
  in
  client, close
;;

(** Create a single transport connection bound to [sw]. This is the unit
    stored in the connection cache and reused across requests. *)
let make_connection ~sw ~net ~uri : (connection, http_error) result =
  let* net, addr, tls_wrap = resolve_origin net uri in
  try
    let sock = Eio.Net.connect ~sw net addr in
    let conn : connection =
      match tls_wrap with
      | Some wrap -> (wrap uri sock :> connection)
      | None -> (sock :> connection)
    in
    Diag.debug "http_client" "make_connection: new connection for %s" (Uri.to_string uri);
    Ok conn
  with
  | Eio.Io (err, _) as exn -> Error (network_error_of_eio err exn)
  | Unix.Unix_error (code, _, _) as exn ->
    Error
      (NetworkError { message = Printexc.to_string exn; kind = classify_unix_error code })
  | Failure msg -> Error (unknown_network_error msg)
;;

(** Client wrapper that tracks the socket for explicit close.
    The caller provides the concrete URI so host resolution and TLS
    availability can be checked up front and reported as typed errors. *)
let make_closing_client ~sw ~net ~uri =
  let+ client, close = make_client ~net ~uri in
  Eio.Switch.on_release sw close;
  client
;;

(** Run [f client] with a client obtained either from [cache] or created
    for one request. When [cache] is supplied, a hit reuses a parked
    connection, a miss creates one and parks it on success, and any error
    evicts it. When [cache] is omitted the client is created for a single
    request and closed immediately after [f] returns.

    [f] receives the caller switch and must return an
    [('a, http_error) result]. The wrapper distinguishes [Ok] from [Error]
    for cache lifecycle decisions; exceptions are treated as fatal for the
    connection. *)
let with_client ?cache ~sw ~net ~uri f =
  match cache with
  | None ->
    let* client, close = make_client ~net ~uri in
    Fun.protect
      ~finally:(fun () ->
        try Eio.Cancel.protect close with
        | Eio.Cancel.Cancelled _ as e -> raise e
        | exn ->
          Diag.warn
            "http_client"
            "with_client one-shot close failed: %s"
            (Printexc.to_string exn))
      (fun () -> f ~sw client)
  | Some cache ->
    let* conn, was_cached =
      match cache_take cache uri with
      | Some e -> Ok (e.connection, true)
      | None ->
        let+ conn = make_connection ~sw:cache.sw ~net ~uri in
        Atomic.incr cache.create_count_total;
        conn, false
    in
    let client =
      Cohttp_eio.Client.make_generic (fun ~sw:_ _uri -> (conn :> _ Eio.Flow.two_way))
    in
    let ok = ref false in
    Fun.protect
      ~finally:(fun () ->
        try
          if !ok
          then cache_return cache uri { connection = conn; last_used_at = 0.0 }
          else Eio.Resource.close conn
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn ->
          Diag.warn
            "http_client"
            "with_client cleanup failed: %s"
            (Printexc.to_string exn))
      (fun () ->
         let* result = f ~sw client in
         ok := true;
         Ok result)
;;

let drain_response_body ?clock ?(timeout_s = 30.0) resp_body =
  let buf = Cstruct.create 4096 in
  let rec drain () =
    let _ = Eio.Flow.single_read resp_body buf in
    drain ()
  in
  let drain_with_timeout () =
    match clock with
    | Some clk -> Eio.Time.with_timeout_exn clk timeout_s drain
    | None -> drain ()
  in
  try drain_with_timeout () with
  | End_of_file ->
    Diag.debug "http_client" "drain_response_body: reached End_of_file";
    Ok ()
  | Eio.Time.Timeout ->
    Diag.debug "http_client" "drain_response_body: timed out after %.1fs" timeout_s;
    Error
      (TimeoutError
         { message = Printf.sprintf "response body drain timed out after %.1fs" timeout_s
         ; phase = Non_streaming_body
         })
  | Unix.Unix_error (code, _, _) as e ->
    let kind = classify_unix_error code in
    Diag.warn
      "http_client"
      "drain_response_body: Unix_error %s (kind %s)"
      (Printexc.to_string e)
      (match kind with
       | Connection_refused -> "connection_refused"
       | Dns_failure -> "dns_failure"
       | Tls_error -> "tls_error"
       | Timeout -> "timeout"
       | Local_resource_exhaustion -> "local_resource_exhaustion"
       | End_of_file -> "end_of_file"
       | Unknown -> "unknown");
    Error (NetworkError { message = Printexc.to_string e; kind })
  | Eio.Io (err, _) as e ->
    let message = Printexc.to_string e in
    Diag.warn "http_client" "drain_response_body: %s" message;
    Error (network_error_of_eio err e)
  | Sys_error msg ->
    Diag.warn "http_client" "drain_response_body: sys_error %s" msg;
    Error (unknown_network_error msg)
  | Failure msg ->
    Diag.warn "http_client" "drain_response_body: failure %s" msg;
    Error (unknown_network_error msg)
  | Invalid_argument msg ->
    Diag.warn "http_client" "drain_response_body: invalid_arg %s" msg;
    Error (NetworkError { message = msg; kind = Unknown })
  (* Re-raise cancellation so a fiber cancelled mid-drain unwinds instead of
     being absorbed by the catch-all below (structured concurrency). Mirrors the
     transport-close handler in this module. *)
  | Eio.Cancel.Cancelled _ as e -> raise e
  | drain_failure ->
    let message = Printexc.to_string drain_failure in
    Diag.warn "http_client" "drain_response_body: %s" message;
    Error (NetworkError { message; kind = Unknown })
;;

let read_response_body_or_drain_error ?clock resp_body =
  try
    Ok Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body resp_body |> take_all)
  with
  | exn ->
    (match drain_response_body ?clock resp_body with
     | Ok () -> raise exn
     | Error err -> Error err)
;;

let get_sync ?cache ?clock ?(timeout_s = default_http_timeout_s) ~sw ~net ~url ~headers ()
  =
  catch_network (fun () ->
    let* uri = parse_uri url in
    with_client ?cache ~sw ~net ~uri (fun ~sw client ->
      let hdr = Http.Header.of_list (maybe_add_connection_close ?cache headers) in
      with_optional_timeout ~clock ~timeout_s (fun () ->
        let resp, resp_body = Cohttp_eio.Client.get ~sw client ~headers:hdr uri in
        let code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
        let* body_str = read_response_body_or_drain_error ?clock resp_body in
        Ok (code, body_str))))
;;

let post_sync
      ?cache
      ?clock
      ?(timeout_s = default_http_timeout_s)
      ~sw
      ~net
      ~url
      ~headers
      ~body
      ()
  =
  catch_network (fun () ->
    let* uri = parse_uri url in
    with_client ?cache ~sw ~net ~uri (fun ~sw client ->
      (* Explicitly set Content-Length to prevent chunked transfer encoding.
         Ollama's yyjson parser rejects chunked bodies with
         "Value looks like object, but can't find closing '}' symbol". *)
      let headers_with_length =
        ("content-length", string_of_int (String.length body))
        :: maybe_add_connection_close ?cache headers
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
        let* body_str = read_response_body_or_drain_error ?clock resp_body in
        Ok (code, body_str))))
;;

let post_stream
      ?cache
      ?clock
      ?(connect_timeout_s = default_http_timeout_s)
      ~sw
      ~net
      ~url
      ~headers
      ~body
      ()
  =
  (* Cache is intentionally ignored for the streaming reader variant: the
     returned [Buf_read.t] outlives this function, so we cannot safely park
     the client until consumption finishes. Use [with_post_stream] for
     cache-aware streaming. *)
  ignore cache;
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
      let* body_str = read_response_body_or_drain_error ?clock resp_body in
      Error (HttpError { code; body = body_str }))
;;

let with_post_stream
      ?cache
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
  (* When a cache is active, bind the transport to the cache's long-lived
     switch so the connection can be reused across requests. Otherwise use
     the per-call switch for one-shot cleanup. *)
  let request_sw =
    match cache with
    | Some c -> c.sw
    | None -> sw
  in
  (* Phase 1a: connect + post + response headers, bounded by
     [connect_timeout_s]. Cohttp_eio.Client.post returns once headers are
     parsed (body is a lazy flow), so wrapping only this stage in
     [catch_network] keeps a connect / header-phase stall as
     [TimeoutError { phase = Http_operation }] without absorbing body-phase
     timeouts (first-token / prefill wait, inter-chunk idle).

     Streaming is handled manually rather than through [with_client] so the
     connection is NOT parked until [f] has fully consumed the reader. *)
  let post_result =
    catch_network (fun () ->
      let* uri = parse_uri url in
      let* conn =
        match cache with
        | None -> make_connection ~sw:request_sw ~net ~uri
        | Some cache ->
          (match cache_take cache uri with
           | Some e -> Ok e.connection
           | None ->
             let+ conn = make_connection ~sw:cache.sw ~net ~uri in
             Atomic.incr cache.create_count_total;
             conn)
      in
      let client =
        Cohttp_eio.Client.make_generic (fun ~sw:_ _uri -> (conn :> _ Eio.Flow.two_way))
      in
      let headers_with_length =
        ("content-length", string_of_int (String.length body))
        :: maybe_add_connection_close ?cache headers
      in
      let hdr = Http.Header.of_list headers_with_length in
      try
        let resp, resp_body =
          with_optional_timeout ~clock ~timeout_s:connect_timeout_s (fun () ->
            Cohttp_eio.Client.post
              ~sw:request_sw
              client
              ~headers:hdr
              ~body:(Cohttp_eio.Body.of_string body)
              uri)
        in
        match Cohttp.Response.status resp with
        | `OK ->
          Ok
            ( uri
            , conn
            , Eio.Buf_read.of_flow ~max_size:Api_common.max_response_body resp_body )
        | status ->
          let code = Cohttp.Code.code_of_status status in
          profile_headers_on_client_error
            ~url
            ~code
            ~resp_headers:(Cohttp.Response.headers resp)
            headers_with_length;
          (match read_response_body_or_drain_error ?clock resp_body with
           | Ok body_str ->
             Eio.Resource.close conn;
             Error (HttpError { code; body = body_str })
           | Error err ->
             Eio.Resource.close conn;
             Error err)
      with
      | exn ->
        Eio.Resource.close conn;
        (match classify_network_exn exn with
         | Some e -> Error e
         | None -> raise exn))
  in
  (* Phase 1b: body consumption. Deliberately OUTSIDE [catch_network]: a
     body-phase [Eio.Time.Timeout] is phase-distinct from the connect /
     headers phase and must not be mislabelled [Http_operation]. [f] owns
     phase-aware timeout handling and must convert [Eio.Time.Timeout] into a
     typed [Error] (see [Complete_stream.body_logic] and the Streaming
     callers). A body-phase timeout that [f] lets propagate escapes this
     function as the raw exception.

     The connection is parked back into the cache only after [f] returns
     successfully, ensuring the reader is no longer using the flow. *)
  let* uri, conn, reader = post_result in
  let body_result =
    try Ok (f reader) with
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
       | None ->
         (* Unclassified exceptions (including cancellation) escape, so close
              the connection before re-raising to avoid leaking a cached socket
              bound to the long-lived cache switch. *)
         Eio.Cancel.protect (fun () -> Eio.Resource.close conn);
         raise exn)
  in
  (match body_result, cache with
   | Ok _, Some cache -> cache_return cache uri { connection = conn; last_used_at = 0.0 }
   | Ok _, None -> Eio.Resource.close conn
   | Error _, _ -> Eio.Resource.close conn);
  body_result
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

let idle_timeout_without_clock site =
  invalid_arg
    (site
     ^ ": idle_timeout is set but no clock was supplied — the idle deadline would be \
        silently disarmed (pass ?clock, or drop ?idle_timeout)")
;;

let require_clock_when_idle ~site ~clock ~idle_timeout =
  match clock, idle_timeout with
  | None, Some _ ->
    (* Fail-loud contract: a configured idle deadline with no clock used
       to silently disarm and leave a stalled stream blocking forever
       (the read_sse idle-disarm bug family). Misconfiguration must fail
       at the call site, not at 3 a.m. as a hung fiber. *)
    idle_timeout_without_clock site
  | Some _, _ | None, None -> ()
;;

let read_sse ?clock ?idle_timeout ~reader ~on_data () =
  let site = "read_sse" in
  require_clock_when_idle ~site ~clock ~idle_timeout;
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
    | None, Some _ -> idle_timeout_without_clock site
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
  let site = "read_ndjson" in
  require_clock_when_idle ~site ~clock ~idle_timeout;
  let read_line () =
    match clock, idle_timeout with
    | Some c, Some t -> Eio.Time.with_timeout_exn c t (fun () -> Eio.Buf_read.line reader)
    | Some _, None | None, None -> Eio.Buf_read.line reader
    | None, Some _ -> idle_timeout_without_clock site
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

let inject_stream_and_options body_str =
  match Yojson.Safe.from_string body_str with
  | `Assoc fields ->
    let without_existing =
      List.filter (fun (k, _) -> k <> "stream" && k <> "stream_options") fields
    in
    Yojson.Safe.to_string
      (`Assoc
          (("stream_options", `Assoc [ "include_usage", `Bool true ])
           :: ("stream", `Bool true)
           :: without_existing))
  | other -> Yojson.Safe.to_string other
  | exception Yojson.Json_error _ -> body_str
;;

let%test "inject_stream_and_options matches chained param >> options" =
  (* Parity proof: the combined single-pass injector must be byte-identical to
     [inject_stream_param body |> inject_stream_options_include_usage] across
     Assoc variants, pre-existing stream/stream_options, non-json, array, empty. *)
  List.for_all
    (fun body ->
       inject_stream_and_options body
       = inject_stream_options_include_usage (inject_stream_param body))
    [ {|{"model":"glm-4"}|}
    ; {|{"model":"gpt-4","stream":false}|}
    ; {|{"messages":[],"stream_options":{"include_usage":false}}|}
    ; {|{"a":1,"stream":true,"stream_options":{"x":1}}|}
    ; "not json"
    ; {|[1,2,3]|}
    ; ""
    ]
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

let%test "catch_network maps text-only Sys_error to unknown NetworkError" =
  match catch_network (fun () -> raise (Sys_error "broken pipe")) with
  | Error (NetworkError { message = "broken pipe"; kind = Unknown }) -> true
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
;;

let%test "catch_network keeps text-only Sys_error resource exhaustion unknown" =
  match catch_network (fun () -> raise (Sys_error "Too many open files")) with
  | Error (NetworkError { kind = Unknown; _ }) -> true
  | Ok _
  | Error
      ( HttpError _
      | NetworkError _
      | TimeoutError _
      | AcceptRejected _
      | ProviderTerminal _
      | ProviderFailure _ ) -> false
;;

let%test "catch_network keeps text-only Failure resource exhaustion unknown" =
  match catch_network (fun () -> raise (Failure "EMFILE")) with
  | Error (NetworkError { kind = Unknown; _ }) -> true
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

(* ── drain_response_body tests ───────────────────────── *)

let%test "drain_response_body: complete source reports complete" =
  Result.is_ok (drain_response_body (Eio.Flow.string_source "abc"))
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

(* ── typed Eio classification tests ───────────────────── *)

let eio_exn err = Eio.Exn.create err

let%test "classify_network_exn: typed Eio refused" =
  match
    classify_network_exn
      (eio_exn
         (Eio.Net.E
            (Eio.Net.Connection_failure
               (Eio.Net.Refused (Eio_unix.Unix_error (Unix.ECONNREFUSED, "connect", ""))))))
  with
  | Some (NetworkError { kind = Connection_refused; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: typed Eio timeout" =
  match
    classify_network_exn
      (eio_exn (Eio.Net.E (Eio.Net.Connection_failure Eio.Net.Timeout)))
  with
  | Some (NetworkError { kind = Timeout; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: typed Eio no addresses" =
  match
    classify_network_exn
      (eio_exn (Eio.Net.E (Eio.Net.Connection_failure Eio.Net.No_matching_addresses)))
  with
  | Some (NetworkError { kind = Dns_failure; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: typed Eio Unix backend resource exhaustion" =
  match
    classify_network_exn
      (eio_exn (Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, "socket", ""))))
  with
  | Some (NetworkError { kind = Local_resource_exhaustion; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: text-only Sys_error is Unknown" =
  match classify_network_exn (Sys_error "Connection refused") with
  | Some (NetworkError { kind = Unknown; _ }) -> true
  | _ -> false
;;

let%test "classify_network_exn: message-only Failure stays Unknown" =
  match classify_network_exn (Failure "Connection refused") with
  | Some (NetworkError { kind = Unknown; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "https_init_error_network_kind: empty trust anchors are TLS" =
  https_init_error_network_kind
    (Api_common.Ca_certs_unavailable "ca-certs: empty trust anchors")
  = Tls_error
;;

let%test "https_init_error_network_kind: TLS config remains TLS" =
  https_init_error_network_kind (Api_common.Tls_config_unavailable "unsupported protocol")
  = Tls_error
;;

let%test "classify_network_exn: plain Tls_alert is Tls_error" =
  match classify_network_exn (Tls_eio.Tls_alert Tls.Packet.HANDSHAKE_FAILURE) with
  | Some (NetworkError { kind = Tls_error; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: plain Tls_failure is Tls_error" =
  match classify_network_exn (Tls_eio.Tls_failure (`Fatal `No_application_protocol)) with
  | Some (NetworkError { kind = Tls_error; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: backend printer text does not classify" =
  let module Test_backend = struct
    type Eio.Exn.Backend.t += Tls_socket_closed_test

    let () =
      Eio.Exn.Backend.register_pp (fun f -> function
        | Tls_socket_closed_test ->
          Format.pp_print_string f "TLS_socket_closed";
          true
        | _ -> false)
    ;;
  end
  in
  match
    classify_network_exn
      (eio_exn (Eio.Net.E (Eio.Net.Connection_reset Test_backend.Tls_socket_closed_test)))
  with
  | Some (NetworkError { kind = End_of_file; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let multiple_io_exn errs =
  let combine acc err =
    let exn = eio_exn err in
    let bt = Printexc.get_callstack 0 in
    Eio.Exn.combine acc (exn, bt)
  in
  match errs with
  | [] -> eio_exn (Eio.Exn.Multiple_io [])
  | err :: errs ->
    fst (List.fold_left combine (eio_exn err, Printexc.get_callstack 0) errs)
;;

let%test "classify_network_exn: Multiple_io prefers non-retryable kind" =
  match
    classify_network_exn
      (multiple_io_exn
         [ Eio.Net.E (Eio.Net.Connection_failure Eio.Net.Timeout)
         ; Eio.Exn.X (Eio_unix.Unix_error (Unix.EMFILE, "socket", ""))
         ])
  with
  | Some (NetworkError { kind = Local_resource_exhaustion; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
;;

let%test "classify_network_exn: Multiple_io falls back to first known kind" =
  match
    classify_network_exn
      (multiple_io_exn
         [ Eio.Exn.X (Eio_unix.Unix_error (Unix.EPIPE, "write", ""))
         ; Eio.Net.E (Eio.Net.Connection_failure Eio.Net.Timeout)
         ])
  with
  | Some (NetworkError { kind = End_of_file; _ }) -> true
  | Some (HttpError _ | NetworkError _ | TimeoutError _ | AcceptRejected _)
  | Some (ProviderTerminal _ | ProviderFailure _)
  | None -> false
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
