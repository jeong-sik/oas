(** Standalone LLM completion: build request, HTTP POST, parse response.

    Self-contained in llm_provider — no agent_sdk dependency.
    Both OAS and MASC can call {!complete} directly.

    @since 0.46.0 *)

(** Execute a single LLM completion round-trip.
    Builds the request body based on {!Provider_config.t.kind},
    sends via {!Http_client.post_sync}, and parses the response.

    @return [Ok api_response] on success
    @return [Error http_error] on HTTP or network failure *)
val complete :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit ->
  (Types.api_response, Http_client.http_error) result
