(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types
    for compile-time capability checking.

    @stability Evolving
    @since 0.93.1 *)

module type PROVIDER = sig
  type t

  val create_message
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Error.sdk_error) result
end

module type STREAMING_PROVIDER = sig
  include PROVIDER

  val create_message_stream
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> on_event:(Types.sse_event -> unit)
    -> unit
    -> (Types.api_response, Error.sdk_error) result
end

type provider_module = (module PROVIDER)
type streaming_provider_module = (module STREAMING_PROVIDER)

(** Resolve a provider config to a first-class PROVIDER module.
    Returns an error if provider configuration or credentials cannot be
    resolved (e.g. a required environment variable is missing). *)
val of_config
  :  ?on_output_token_receipt:(Llm_provider.Types.output_token_receipt -> unit)
  -> Provider.config
  -> (provider_module, Error.sdk_error) result

(** Check if a provider config supports native streaming. *)
val supports_streaming : Provider.config -> bool

(** Resolve to a STREAMING_PROVIDER if native streaming is supported.
    Returns [Ok None] when streaming is not supported for this provider.
    Returns an error if provider configuration or credentials cannot be
    resolved. *)
val of_config_streaming
  :  ?on_output_token_receipt:(Llm_provider.Types.output_token_receipt -> unit)
  -> Provider.config
  -> (streaming_provider_module option, Error.sdk_error) result
