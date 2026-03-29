(** OpenAI-compatible API response parsing and request building.

    Pure functions operating on {!Llm_provider.Types}.

    @stability Internal
    @since 0.93.1 *)

val tool_calls_to_openai_json : Types.content_block list -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val strip_json_markdown_fences : string -> string
val tool_choice_to_openai_json : Types.tool_choice -> Yojson.Safe.t
val build_openai_tool_json : Yojson.Safe.t -> Yojson.Safe.t

(** Parse an OpenAI-compatible JSON response.
    Returns [Ok api_response] on success, [Error msg] on API error. *)
val parse_openai_response_result : string -> (Types.api_response, string) result

val usage_of_openai_json : Yojson.Safe.t -> Types.api_usage option

val build_request :
  ?stream:bool ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit ->
  string
