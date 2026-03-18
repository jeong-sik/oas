(** Bridge between legacy {!Provider.config} and {!Llm_provider.Provider_config.t}.

    Converts from the old 5-variant provider system (Anthropic, OpenAICompat,
    Ollama, Local, Custom_registered) to the new 2-kind standalone config
    (Anthropic, OpenAI_compat).

    @since 0.53.0 *)

(** Convert a single legacy provider config.
    Calls {!Provider.resolve} to obtain base_url, api_key, and headers.

    - [Anthropic] and [Local] map to [Anthropic] kind
    - [OpenAICompat], [Ollama], and [Custom_registered] map to [OpenAI_compat]

    @return [Error] if the provider cannot be resolved (e.g. missing env var) *)
val to_provider_config :
  Provider.config ->
  (Llm_provider.Provider_config.t, Error.sdk_error) result

(** Convert a legacy cascade to the new {!Llm_provider.Complete.cascade}.
    Fails if any provider in the cascade cannot be resolved. *)
val cascade_to_provider_config :
  Provider.cascade ->
  (Llm_provider.Complete.cascade, Error.sdk_error) result
