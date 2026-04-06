(** OpenAI Chat Completions HTTP transport.

    Implements {!Llm_transport.t} for any OpenAI-compatible API endpoint:
    llama-server, GLM, OpenRouter, vLLM, Ollama, LiteLLM, etc.

    Thin wrapper around the HTTP completion pipeline in {!Complete}.
    Callers get a simplified config instead of constructing
    {!Provider_config.t} directly.

    @since 0.86.0

    @stability Internal
    @since 0.93.1 *)

(** Configuration for an OpenAI-compatible endpoint. *)
type config = {
  base_url: string;
    (** Base URL (e.g. {!Constants.Endpoints.default_url}). *)
  api_key: string;
    (** API key. Empty string when no auth is needed (local servers). *)
  model_id: string;
    (** Model identifier sent in the request body. *)
  request_path: string;
    (** Path appended to [base_url]. Default ["/v1/chat/completions"]. *)
  max_tokens: int;
    (** Maximum tokens in the response. Default [4096]. *)
  extra_headers: (string * string) list;
    (** Additional HTTP headers (e.g. [("HTTP-Referer", "...")]
        for OpenRouter). Default empty. *)
}

(** Default config for local llama-server on port 8085. *)
val default_config : config

(** Create an OpenAI-compatible HTTP transport.

    The returned {!Llm_transport.t} sends requests to the configured
    endpoint using the OpenAI Chat Completions wire format.

    Per-request overrides from {!Llm_transport.completion_request.config}
    (temperature, system_prompt, tools, etc.) are respected. The
    transport config provides base_url, api_key, and model_id defaults.

    @param sw  Eio switch for HTTP connection lifetime.
    @param net Eio network capability for outbound connections. *)
val create :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  config:config ->
  Llm_transport.t
