(** Synchronous provider-native input-token count transport.

    This module owns endpoint selection, request construction dispatch, and
    transport execution for input-token counting — the pieces that
    {!Input_token_count} deliberately keeps outside its decoding boundary.

    @stability Internal *)

(** Endpoint for the Anthropic Messages count-tokens call: inserts
    [/count_tokens] after the configured request path, preserving any query
    string carried by custom or proxy configurations. *)
val count_tokens_url : Provider_config.t -> string

(** Count one Anthropic Messages input through the provider's native
    [/v1/messages/count_tokens] endpoint.

    The request reuses {!Backend_anthropic}'s completion input projection.
    Non-Anthropic configs fail with [Unsupported] before any I/O.

    The call is bounded only when [timeout_s] is explicitly supplied;
    enforcing it also requires [clock], mirroring {!Http_client.post_sync}. *)
val count_anthropic
  :  ?connection_cache:Http_client.cache
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> (Input_token_count.count, Input_token_count.error) result
