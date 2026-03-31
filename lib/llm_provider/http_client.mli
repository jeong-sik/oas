(** HTTP client for LLM provider endpoints.

    Eio + cohttp-eio with TLS via {!Api_common.make_https}.
    Network and HTTP errors are captured as {!http_error},
    so callers do not need [try/with] around HTTP operations.

    @stability Internal
    @since 0.93.1 *)

(** Transport-level error. *)
type http_error =
  | HttpError of { code: int; body: string }
  | NetworkError of { message: string }

(** GET a URL synchronously, returning the full response.
    Returns [(status_code, body_string)] on success. *)
val get_sync :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  (int * string, http_error) result

(** POST JSON body synchronously, returning the full response.
    Returns [(status_code, body_string)] on success. *)
val post_sync :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (int * string, http_error) result

(** POST JSON body for SSE/NDJSON streaming.
    Returns [Ok reader] on HTTP 200 (10 MB buffer).
    Returns [Error] on non-200 or network failure.

    The connection is bound to [sw]; prefer {!with_post_stream} to
    ensure the connection fd is released when the stream is consumed. *)
val post_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (Eio.Buf_read.t, http_error) result

(** Like {!post_stream} but manages connection lifetime internally.
    [f] receives the reader; when [f] returns the connection is closed
    and its fd is released immediately. *)
val with_post_stream :
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  f:(Eio.Buf_read.t -> 'a) ->
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
