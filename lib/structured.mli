(** Structured output via tool_use pattern.

    Uses tool_choice=Tool(name) to force the model to call a specific tool,
    then extracts the input JSON as structured output.

    @stability Evolving
    @since 0.93.1 *)

open Types

(** {1 Schema} *)

type 'a schema = {
  name: string;
  description: string;
  params: tool_param list;
  parse: Yojson.Safe.t -> ('a, string) result;
}

val schema_to_tool_json : _ schema -> Yojson.Safe.t
val extract_tool_input : schema:'a schema -> content_block list -> ('a, Error.sdk_error) result

(** {1 Direct extraction} *)

val extract :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  config:agent_config ->
  schema:'a schema ->
  string ->
  ('a, Error.sdk_error) result

(** {1 Extractors} *)

type 'a extractor = api_response -> ('a, string) result

val json_extractor : (Yojson.Safe.t -> 'a) -> 'a extractor
val text_extractor : (string -> 'a option) -> 'a extractor

(** {1 Agent-level structured output} *)

val run_structured :
  sw:Eio.Switch.t ->
  ?clock:float Eio.Time.clock_ty Eio.Resource.t ->
  Agent.t ->
  string ->
  extract:'a extractor ->
  ('a, Error.sdk_error) result

(** {1 Retry with validation feedback} *)

type 'a retry_result = {
  value: 'a;
  total_usage: api_usage option;
  attempts: int;
}

val extract_with_retry :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  ?clock:float Eio.Time.clock_ty Eio.Resource.t ->
  config:agent_config ->
  schema:'a schema ->
  ?max_retries:int ->
  ?on_validation_error:(int -> string -> unit) ->
  string ->
  ('a retry_result, Error.sdk_error) result

(** {1 Streaming extraction} *)

val extract_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  ?clock:float Eio.Time.clock_ty Eio.Resource.t ->
  config:agent_config ->
  schema:'a schema ->
  on_event:(sse_event -> unit) ->
  string ->
  ('a * api_response, Error.sdk_error) result
