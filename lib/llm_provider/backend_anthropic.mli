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

(** Optional-envelope resolver re-exported from
    {!Backend_openai_request.effective_max_output_tokens}: caller override
    clamped to the model capability (one-shot WARN), [None] on caller
    [None] — the ceiling is never injected as a request value. *)
val effective_max_output_tokens : Provider_config.t -> int option

(** The Messages API requires [max_tokens] on every request, so this
    envelope carries an explicit OAS required-envelope policy: caller
    override clamped to a catalog or declared override ceiling; caller [None]
    falls back only to the catalog-declared model maximum (this is not a provider default); raises
    [Invalid_argument] naming the model when neither the caller nor the
    catalog declares a value — no second numeric policy is invented. *)
val required_max_output_tokens : Provider_config.t -> int

val required_output_token_receipt
  :  Provider_config.t
  -> (Types.output_token_receipt, Types.required_output_token_error) result

val required_output_token_receipt_exn : Provider_config.t -> Types.output_token_receipt

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

val build_request_with_receipt
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string Provider_request_artifact.t
