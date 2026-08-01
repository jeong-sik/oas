(** One payload unit (a joined SSE event, or a single line) exceeded the byte
    limit the reader was armed with. *)
val http_error_of_oversized_payload
  :  wire_format:Http_client.provider_wire_format
  -> actual_bytes:int option
  -> limit_bytes:int
  -> Http_client.http_error

val http_error_of_stream_error
  :  ?wire_format:Http_client.provider_wire_format
  -> Types.stream_error
  -> Http_client.http_error
