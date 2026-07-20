(** API dispatch — routes requests to provider-specific backends.

    {2 Deprecated dispatch entry points}

    {!create_message} and {!create_message_detailed} are the legacy dispatch
    path: the production path has converged on {!Llm_provider.Complete}. They
    are retained for compatibility and will be removed in a future major
    release. The remaining values in this module — request body builders,
    response parsers, and JSON codecs such as {!content_block_to_json} and
    {!content_block_of_json} — are {b not} deprecated and remain supported
    helpers.

    @stability Evolving
    @since 0.93.1 *)

module Retry = Llm_provider.Retry

type response_accept = Types.api_response -> (unit, string) result

(** {1 Re-exports from Api_common} *)

val default_base_url : string
val api_version : string
val max_response_body : int
val string_is_blank : string -> bool
val text_blocks_to_string : Types.content_block list -> string
val json_of_string_or_raw : string -> Yojson.Safe.t
val content_block_to_json : Types.content_block -> Yojson.Safe.t
val content_block_of_json : Yojson.Safe.t -> Types.content_block option
val message_to_json : Types.message -> Yojson.Safe.t

val make_https
  :  unit
  -> (Uri.t -> [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t -> Tls_eio.t)
       option

(** {1 Re-exports from Api_anthropic} *)

val parse_response : Yojson.Safe.t -> Types.api_response

val build_body_assoc
  :  config:Types.agent_state
  -> messages:Types.message list
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

(** {1 Re-exports from Api_openai} *)

val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list

(** Result-returning OpenAI-compatible request body builder. Live request paths
    should use this form so unsupported provider contracts surface as typed
    errors before HTTP dispatch. *)
val build_openai_body_result
  :  ?provider_config:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (string, string) result

val build_openai_body_result_for_resolved_config
  :  resolved_config:Llm_provider.Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (string, string) result

val build_openai_body
  :  ?provider_config:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> string

(** Parse an OpenAI-compatible JSON response. See
    {!Llm_provider.Backend_openai_parse.parse_openai_response_result} for the
    [parse_error] contract (oas#2483: an all-empty 200 fails closed as
    [Empty_completion]). *)
val parse_openai_response_result
  :  string
  -> (Types.api_response, Llm_provider.Backend_openai_parse.parse_error) result

(** {1 Non-streaming request} *)

(** Send one request to the explicitly selected provider. A timeout is applied
    only when both [clock] and [request_timeout_s] are supplied. Supplying a
    timeout without a clock is an explicit configuration error. *)
val create_message_detailed
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> provider:Provider.config
  -> ?clock:_ Eio.Time.clock
  -> ?request_timeout_s:float
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (Types.api_response, Provider_failure_attribution.detailed_error) result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]

val create_message
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> provider:Provider.config
  -> ?clock:_ Eio.Time.clock
  -> ?request_timeout_s:float
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?slot_id:int
  -> unit
  -> (Types.api_response, Error.sdk_error) result
[@@deprecated
  "Use Llm_provider.Complete — this legacy dispatch path is retained for compatibility \
   and will be removed in a future major release."]
