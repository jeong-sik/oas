(** Anthropic Claude API request building and response parsing.

    @stability Internal
    @since 0.93.1 *)

(** Parse Anthropic API response JSON. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Build request body assoc list for an Anthropic-compatible Messages API.
    [provider_kind] defaults to [Anthropic]; the built-in Kimi-compatible
    provider passes [Kimi] so its capability and thinking policy stay distinct.
    The Anthropic convenience path uses the public Anthropic default; Kimi is
    resolved from its provider-catalog declaration. Callers with an
    already-resolved endpoint should use
    {!build_body_assoc_result_for_resolved_config}. *)
val build_body_assoc
  :  config:Types.agent_state
  -> messages:Types.message list
  -> ?provider_kind:Llm_provider.Provider_config.provider_kind
  -> ?tools:Yojson.Safe.t list
  -> stream:bool
  -> unit
  -> (string * Yojson.Safe.t) list

val build_body_assoc_result_for_resolved_config
  :  resolved_config:Llm_provider.Provider_config.t
  -> cache_extended_ttl:bool
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> stream:bool
  -> unit
  -> ((string * Yojson.Safe.t) list, string) result
