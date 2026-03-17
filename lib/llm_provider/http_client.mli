(** HTTP client for LLM provider endpoints.

    Eio + cohttp-eio with TLS via {!Api_common.make_https}.
    Network and HTTP errors are captured as {!http_error},
    so callers do not need [try/with] around HTTP operations. *)

(** Transport-level error. *)
type http_error =
  | HttpError of { code: int; body: string }
  | NetworkError of { message: string }

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
    Returns [Error] on non-200 or network failure. *)
val post_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (Eio.Buf_read.t, http_error) result

(** Read SSE-formatted lines from a reader.
    Strips [data: ] prefixes and passes the payload to [on_data].
    Tracks [event: ] lines and provides the current event type.
    Returns normally on [End_of_file]. *)
val read_sse :
  reader:Eio.Buf_read.t ->
  on_data:(event_type:string option -> string -> unit) ->
  unit -> unit

(** Inject ["stream": true] into a JSON body string. *)
val inject_stream_param : string -> string
