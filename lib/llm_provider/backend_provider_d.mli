(** Provider_d-compatible API response parsing and request building.

    Pure functions operating on {!Llm_provider.Types}.

    @stability Internal
    @since 0.93.1 *)

val tool_calls_to_provider_d_json : Types.content_block list -> Yojson.Safe.t list
val provider_d_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val provider_d_messages_of_message : Types.message -> Yojson.Safe.t list
val strip_json_markdown_fences : string -> string
val tool_choice_to_provider_d_json : Types.tool_choice -> Yojson.Safe.t
val build_provider_d_tool_json : Yojson.Safe.t -> Yojson.Safe.t
val strip_orphaned_tool_results : Types.message list -> Types.message list
val response_format_to_provider_d_json : Types.response_format -> Yojson.Safe.t option

(** Parse an Provider_d-compatible JSON response.
    Returns [Ok api_response] on success, [Error msg] on API error. *)
val parse_provider_d_response_result : string -> (Types.api_response, string) result

val usage_of_provider_d_json : Yojson.Safe.t -> Types.api_usage option

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

(** Emit a one-shot stderr WARN the first time a capability-gated
    sampling field is dropped for a given [(model_id, field)] pair.

    Called from the silent-drop branches of the capability gates in
    {!build_request} and [Api_provider_d.build_provider_d_body] so operators
    who set a non-supported field (e.g. [min_p] on a Glm config) see
    exactly which field was stripped without the per-request WARN
    spam that would otherwise fire on every automated turn.

    Best-effort dedup via an internal [Hashtbl]; a race under Eio
    cooperative scheduling double-warns at most once per key, which
    is harmless.

    @since 0.123.0 *)
val warn_capability_drop : model_id:string -> field:string -> unit
