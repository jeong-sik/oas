(** API dispatch — routes requests to provider-specific backends. *)

(** {1 Named cascade} *)

type named_cascade = {
  name : string;
  defaults : string list;
  config_path : string option;
}

val named_cascade :
  ?config_path:string -> name:string -> defaults:string list ->
  unit -> named_cascade

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
val make_https :
  unit ->
  (Uri.t ->
   [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t ->
   Tls_eio.t) option

(** {1 Re-exports from Api_anthropic} *)

val parse_response : Yojson.Safe.t -> Types.api_response
val build_body_assoc :
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  stream:bool ->
  unit -> (string * Yojson.Safe.t) list

(** {1 Re-exports from Api_openai} *)

val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val build_openai_body :
  ?provider_config:Provider.config ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit -> string
val parse_openai_response : string -> Types.api_response

(** {1 Non-streaming request} *)

val create_message :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  ?clock:_ Eio.Time.clock ->
  ?retry_config:Retry.retry_config ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit ->
  (Types.api_response, Error.sdk_error) result

(** {1 Cascade request} *)

val create_message_cascade :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  ?retry_config:Retry.retry_config ->
  cascade:Provider.cascade ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit ->
  (Types.api_response, Error.sdk_error) result

(** {1 Named cascade request} *)

val create_message_named :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  named_cascade:named_cascade ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  ?accept:(Types.api_response -> bool) ->
  ?timeout_sec:int ->
  ?metrics:Llm_provider.Metrics.t ->
  unit ->
  (Types.api_response, Error.sdk_error) result

val create_message_named_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?clock:_ Eio.Time.clock ->
  named_cascade:named_cascade ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  ?temperature:float ->
  ?max_tokens:int ->
  ?system_prompt:string ->
  ?timeout_sec:int ->
  ?metrics:Llm_provider.Metrics.t ->
  on_event:(Types.sse_event -> unit) ->
  unit ->
  (Types.api_response, Error.sdk_error) result
