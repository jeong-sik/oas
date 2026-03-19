(** Cascade configuration: named provider profiles with JSON hot-reload
    and discovery-aware health filtering.

    Consumers (MASC, standalone agents) define named cascade profiles
    mapping to ordered lists of providers. This module handles:
    - Parsing "provider:model" strings into {!Provider_config.t}
    - Loading profiles from a JSON config file (mtime-based hot-reload)
    - Filtering providers by local endpoint health via {!Discovery}
    - Convenience cascade execution combining the above

    @since 0.59.0 *)

(** {1 Model String Parsing} *)

(** Known provider identifiers and their default configuration. *)
type provider_defaults = {
  kind: Provider_config.provider_kind;
  base_url: string;
  api_key_env: string;
  request_path: string;
}

(** Parse a "provider:model_id" string into a {!Provider_config.t}.

    Supported providers:
    - [llama:model]   — local llama-server (default http://127.0.0.1:8085)
    - [claude:model]  — Anthropic Messages API
    - [gemini:model]  — Google AI (OpenAI-compat gateway)
    - [glm:model]     — Z.ai ChatCompletions
    - [openrouter:model] — OpenRouter
    - [custom:model\@http://host:port] — arbitrary OpenAI-compat endpoint

    Returns [None] when the provider is unknown or the required API key
    env var is not set (provider is unavailable). *)
val parse_model_string :
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  string -> Provider_config.t option

(** Parse multiple model strings, skipping unavailable ones. *)
val parse_model_strings :
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  string list -> Provider_config.t list

(** {1 JSON Config Loading} *)

(** Load a named model list from a JSON config file.

    The JSON file maps "{name}_models" keys to string arrays:
    {[
      { "heartbeat_action_models": ["llama:qwen3.5", "glm:auto"],
        "sentinel_board_models":   ["llama:qwen3.5", "glm:glm-4.5"] }
    ]}

    Results are cached and hot-reloaded when the file mtime changes.
    Returns an empty list when the file is missing or the key is absent
    (caller provides defaults). *)
val load_profile :
  config_path:string ->
  name:string ->
  string list

(** {1 Discovery-Aware Health Filtering} *)

(** Filter a provider list by local endpoint health.

    Probes local (llama-server) endpoints via {!Discovery}. When all
    local endpoints are unhealthy, removes local providers from the list
    so cloud providers serve as fallback.

    When the list contains only local providers, passes through unchanged
    (let the provider return a connection error rather than an empty list).

    Cloud providers always pass through unfiltered. *)
val filter_healthy :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  Provider_config.t list ->
  Provider_config.t list

(** {1 Named Cascade Execution} *)

(** Extract the concatenated text content from an API response.
    Joins all {!Types.Text} blocks. Useful for accept validators. *)
val text_of_response : Types.api_response -> string

(** Execute a cascade completion using a named profile.

    1. Loads the profile from [config_path] (if provided)
    2. Falls back to [defaults] when the config key is missing
    3. Filters by local endpoint health
    4. Executes cascade: try each provider in order, advancing on failure
       or when [accept] returns [false]

    When [accept] is provided, a successful response that the validator
    rejects causes the cascade to try the next provider (same as a
    retryable failure). This enables retry-on-invalid-format patterns.

    @return [Ok api_response] on success
    @return [Error http_error] when all providers fail or are rejected *)
val complete_named :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?config_path:string ->
  name:string ->
  defaults:string list ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  ?accept:(Types.api_response -> bool) ->
  ?timeout_sec:int ->
  ?cache:Cache.t ->
  ?metrics:Metrics.t ->
  unit ->
  (Types.api_response, Http_client.http_error) result

(** {1 Named Streaming Cascade Execution} *)

(** Execute a streaming cascade completion using a named profile.

    Same resolution steps as {!complete_named}:
    1. Loads the profile from [config_path] (if provided)
    2. Falls back to [defaults] when the config key is missing
    3. Filters by local endpoint health
    4. Executes streaming cascade: try each provider in order

    Unlike {!complete_named}, does not accept an [accept] validator:
    once the SSE stream begins, events are emitted to [on_event]
    and the provider is committed. Failover only occurs on
    connection/HTTP errors before streaming starts.

    No caching (streaming responses are not cacheable).

    @since 0.61.0 *)
val complete_named_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?config_path:string ->
  name:string ->
  defaults:string list ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  ?timeout_sec:int ->
  ?metrics:Metrics.t ->
  on_event:(Types.sse_event -> unit) ->
  unit ->
  (Types.api_response, Http_client.http_error) result
