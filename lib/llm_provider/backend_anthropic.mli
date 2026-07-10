(** Anthropic Claude API response parsing and request building.

    Pure functions operating on {!Llm_provider.Types}.

    @stability Internal
    @since 0.93.1 *)

val parse_response : Yojson.Safe.t -> Types.api_response

(** Provider-correct Claude thinking request field for a model family.
    Exposed so the legacy Agent SDK Anthropic builder can share the same
    manual-budget vs adaptive-thinking dispatch as this backend. *)
val thinking_config_for_config
  :  Capabilities.anthropic_thinking_control
  -> Provider_config.t
  -> Yojson.Safe.t option

(** Optional Claude [output_config], including adaptive [effort] and native
    JSON-schema format when requested. *)
val output_config_for_config
  :  Capabilities.anthropic_thinking_control
  -> Provider_config.t
  -> Yojson.Safe.t option

(** Resolve the output-token budget emitted on the Anthropic wire.  The
    caller override is clamped to the selected model capability, otherwise
    the catalog ceiling is used, with the call-time unknown-model fallback as
    the final layer.  This is shared with the legacy Agent SDK builder so it
    cannot invent a separate default. *)
val effective_max_output_tokens : Provider_config.t -> int

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string
