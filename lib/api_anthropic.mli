(** Anthropic Claude API request building and response parsing.

    @stability Internal
    @since 0.93.1 *)

(** Parse Anthropic API response JSON. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Build request body assoc list for an Anthropic-compatible Messages API.
    [provider_kind] defaults to [Anthropic]; the built-in Kimi-compatible
    provider passes [Kimi] so its capability and thinking policy stay distinct. *)
val build_body_assoc
  :  config:Types.agent_state
  -> messages:Types.message list
  -> ?message_to_json:(Types.message -> Yojson.Safe.t)
  -> ?provider_kind:Llm_provider.Provider_config.provider_kind
  -> ?tools:Yojson.Safe.t list
  -> stream:bool
  -> unit
  -> (string * Yojson.Safe.t) list
