(** Abstract transport for LLM completions.

    Decouples the completion logic (cache, retry, cascade) from
    the underlying I/O mechanism (HTTP, subprocess, etc.).

    @since 0.78.0

    @stability Internal
    @since 0.93.0 *)

(** A completion request: everything needed to produce a response. *)
type completion_request = {
  config: Provider_config.t;
  messages: Types.message list;
  tools: Yojson.Safe.t list;
}

(** Result of a sync completion. *)
type sync_result = {
  response: (Types.api_response, Http_client.http_error) result;
  latency_ms: int;
}

(** Result of a streaming completion. *)
type stream_result = (Types.api_response, Http_client.http_error) result

(** Transport interface.

    Both [complete_sync] and [complete_stream] handle the full
    request → I/O → response pipeline for their transport kind.

    - HTTP transport: build request body, POST, parse response
    - Subprocess transport: write stdin, read stdout, parse output *)
type t = {
  complete_sync : completion_request -> sync_result;
  complete_stream :
    on_event:(Types.sse_event -> unit) ->
    completion_request ->
    stream_result;
}
