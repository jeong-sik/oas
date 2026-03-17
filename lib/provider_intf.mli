(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types
    for compile-time capability checking. *)

module type PROVIDER = sig
  type t

  val create_message :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit ->
    (Types.api_response, Error.sdk_error) result
end

module type STREAMING_PROVIDER = sig
  include PROVIDER

  val create_message_stream :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    on_event:(Types.sse_event -> unit) ->
    unit ->
    (Types.api_response, Error.sdk_error) result
end

type provider_module = (module PROVIDER)
type streaming_provider_module = (module STREAMING_PROVIDER)

(** Resolve a provider config to a first-class PROVIDER module. *)
val of_config : Provider.config -> provider_module

(** Check if a provider config supports native streaming. *)
val supports_streaming : Provider.config -> bool

(** Resolve to a STREAMING_PROVIDER if native streaming is supported. *)
val of_config_streaming : Provider.config -> streaming_provider_module option
