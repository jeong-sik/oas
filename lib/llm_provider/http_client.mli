(** HTTP client for LLM provider endpoints.

    Eio + cohttp-eio with TLS via {!Api_common.make_https}.
    Network and HTTP errors are captured as {!http_error},
    so callers do not need [try/with] around HTTP operations.

    @stability Internal
    @since 0.93.1 *)

(** Structured classification of network errors.
    Enables consumers to pattern-match on error kind instead of
    parsing message strings.

    @since 0.171.0 *)
type network_error_kind =
  | Connection_refused
      (** Remote endpoint actively refused the connection (ECONNREFUSED). *)
  | Dns_failure
      (** Hostname resolution failed or returned no results. *)
  | Tls_error
      (** TLS handshake or certificate validation failed. *)
  | Timeout
      (** Connection or read timed out (ETIMEDOUT). *)
  | Local_resource_exhaustion
      (** Local OS resource limits reached (EMFILE, ENFILE, ENOBUFS, EADDRNOTAVAIL). *)
  | End_of_file
      (** Peer closed the connection unexpectedly. *)
  | Unknown
      (** Unclassified network error. *)

(** Transport-level error. *)
type http_error =
  | HttpError of { code: int; body: string }
  | NetworkError of { message: string; kind: network_error_kind }
  | AcceptRejected of { reason: string }
  | CliTransportRequired of { kind: string }
      (** Provider kind requires a non-HTTP transport (CLI subprocess)
          but the caller did not inject one.  Distinct from
          {!NetworkError} so cascades can treat it as a configuration
          bug rather than a transient failure. *)

(** Default wall-clock timeout (seconds) applied to synchronous HTTP
    operations when a clock is supplied.  Streaming variants use this
    only to bound the connect + initial-response-headers phase. *)
val default_http_timeout_s : float

(** GET a URL synchronously, returning the full response.
    Returns [(status_code, body_string)] on success.

    When [clock] is supplied the entire operation (connect + response
    + body read) is bounded by [timeout_s] (default
    {!default_http_timeout_s}); a timeout surfaces as
    [NetworkError { kind = Timeout; _ }] which is classified as
    retryable by {!Retry.is_retryable}. *)
val get_sync :
  ?clock:_ Eio.Time.clock ->
  ?timeout_s:float ->
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  unit ->
  (int * string, http_error) result

(** POST JSON body synchronously, returning the full response.
    Returns [(status_code, body_string)] on success.

    When [clock] is supplied the entire operation is bounded by
    [timeout_s] (default {!default_http_timeout_s}); a timeout surfaces
    as [NetworkError { kind = Timeout; _ }]. *)
val post_sync :
  ?clock:_ Eio.Time.clock ->
  ?timeout_s:float ->
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  unit ->
  (int * string, http_error) result

(** POST JSON body for SSE/NDJSON streaming.
    Returns [Ok reader] on HTTP 200 (10 MB buffer).
    Returns [Error] on non-200 or network failure.

    The connection is bound to [sw]; prefer {!with_post_stream} to
    ensure the connection fd is released when the stream is consumed.

    When [clock] is supplied only the connect + initial response
    headers are bounded by [connect_timeout_s] (default
    {!default_http_timeout_s}); body consumption through the returned
    reader is the caller's responsibility to timebox. *)
val post_stream :
  ?clock:_ Eio.Time.clock ->
  ?connect_timeout_s:float ->
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  unit ->
  (Eio.Buf_read.t, http_error) result

(** Like {!post_stream} but manages connection lifetime internally.
    [f] receives the reader; when [f] returns the connection is closed
    and its fd is released immediately.

    [connect_timeout_s] bounds only the connect + initial response
    headers phase when [clock] is supplied. *)
val with_post_stream :
  ?clock:_ Eio.Time.clock ->
  ?connect_timeout_s:float ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  f:(Eio.Buf_read.t -> 'a) ->
  unit ->
  ('a, http_error) result

(** Read SSE-formatted lines from a reader.
    Strips [data: ] prefixes and passes the payload to [on_data].
    Tracks [event: ] lines and provides the current event type.
    Returns normally on [End_of_file]. *)
val read_sse :
  reader:Eio.Buf_read.t ->
  on_data:(event_type:string option -> string -> unit) ->
  unit -> unit

(** [true] when the error indicates local resource exhaustion
    (ephemeral port depletion, FD limit).  Cascading to another
    provider cannot help — the bottleneck is the local machine. *)
val is_local_resource_exhaustion : http_error -> bool

(** Inject ["stream": true] into a JSON body string. *)
val inject_stream_param : string -> string
