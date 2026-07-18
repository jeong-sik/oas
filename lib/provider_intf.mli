(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types
    for compile-time capability checking.

    {2 Deprecated}

    This whole module is the legacy first-class-module dispatch island: the
    production path has converged on {!Llm_provider.Complete}. Every module
    type, type alias, and value here is retained for compatibility and will
    be removed in a future major release.

    @stability Evolving
    @since 0.93.1 *)

(* This signature cross-references its own deprecated items (module types in
   type aliases and value types); the recorded [@@deprecated] attributes
   still fire for external users. *)
[@@@alert "-deprecated"]

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
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

module type DETAILED_PROVIDER = sig
  include PROVIDER

  val create_message_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result
end
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

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
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

module type DETAILED_STREAMING_PROVIDER = sig
  include STREAMING_PROVIDER

  val create_message_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result

  val create_message_stream_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> on_event:(Types.sse_event -> unit)
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result
end
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

type provider_module = (module PROVIDER)
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

type detailed_provider_module = (module DETAILED_PROVIDER)
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

type streaming_provider_module = (module STREAMING_PROVIDER)
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

type detailed_streaming_provider_module = (module DETAILED_STREAMING_PROVIDER)
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

(** Resolve a provider config to a first-class PROVIDER module.
    Returns an error if provider configuration or credentials cannot be
    resolved (e.g. a required environment variable is missing). *)
val of_config : Provider.config -> (provider_module, Error.sdk_error) result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

val of_config_detailed
  :  Provider.config
  -> (detailed_provider_module, Provider_failure_attribution.detailed_error) result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

(** Check if a provider config supports native streaming. *)
val supports_streaming : Provider.config -> bool
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

(** Resolve to a STREAMING_PROVIDER if native streaming is supported.
    Returns [Ok None] when streaming is not supported for this provider.
    Returns an error if provider configuration or credentials cannot be
    resolved. *)
val of_config_streaming
  :  Provider.config
  -> (streaming_provider_module option, Error.sdk_error) result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

val of_config_streaming_detailed
  :  Provider.config
  -> ( detailed_streaming_provider_module option
       , Provider_failure_attribution.detailed_error )
       result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]
