(** SSE (Server-Sent Events) line-level parser.

    Follows the SSE spec: event/data/id/retry fields,
    blank line = dispatch.

    @stability Internal
    @since 0.93.1 *)

type raw_sse_event =
  { event_type : string option
  ; data : string
  ; id : string option
  ; retry : int option
  }

(** Parse SSE lines from a buffered reader, calling [on_event] for
    each complete event.  Returns when the stream ends. *)
val parse_lines : Eio.Buf_read.t -> on_event:(raw_sse_event -> unit) -> unit
