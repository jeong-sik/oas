(** Abstract transport for LLM completions.

    @since 0.78.0 *)

type completion_request = {
  config: Provider_config.t;
  messages: Types.message list;
  tools: Yojson.Safe.t list;
}

type sync_result = {
  response: (Types.api_response, Http_client.http_error) result;
  latency_ms: int;
}

type stream_result = (Types.api_response, Http_client.http_error) result

type t = {
  complete_sync : completion_request -> sync_result;
  complete_stream :
    on_event:(Types.sse_event -> unit) ->
    completion_request ->
    stream_result;
}
