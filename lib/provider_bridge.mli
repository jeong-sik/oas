(** Bridge between legacy {!Provider.config} and {!Llm_provider.Provider_config.t}.

    Converts from the old 4-variant provider system (Provider_a, OpenAICompat,
    Local, Custom_registered) to the new 2-kind standalone config
    (Provider_a, Provider_d_compat).

    @since 0.53.0

    @stability Internal
    @since 0.93.1 *)

(** Convert a single legacy provider config.
    Calls {!Provider.resolve} to obtain base_url, api_key, and headers.

    - [Provider_a] and [Local] map to [Provider_a] kind
    - [OpenAICompat] and [Custom_registered] map to [Provider_d_compat]

    @return [Error] if the provider cannot be resolved (e.g. missing env var) *)
val to_provider_config
  :  Provider.config
  -> (Llm_provider.Provider_config.t, Error.sdk_error) result
