(** OpenAI-compatible request body building.

    This module owns provider-config -> Chat Completions JSON request
    construction. {!Backend_openai} re-exports the public surface for
    backwards compatibility while response parsing and message serialization
    stay in their existing modules.

    @stability Internal *)

val warn_capability_drop : model_id:string -> field:string -> unit
val effective_tool_choice : Provider_config.t -> Yojson.Safe.t option
val effective_tools : Provider_config.t -> Yojson.Safe.t list -> Yojson.Safe.t list
val structured_schema_of_config : Provider_config.t -> Yojson.Safe.t option
val capabilities_of_config : Provider_config.t -> Capabilities.capabilities
val openai_json_schema_payload : Yojson.Safe.t -> Yojson.Safe.t
val response_format_to_openai_json : Types.response_format -> Yojson.Safe.t option
val response_format_of_config : Provider_config.t -> Yojson.Safe.t option

(** [build_request_assoc] is {!build_request} before the final
    [Yojson.Safe.to_string]; sibling backends (e.g. {!Backend_glm}) mutate the
    Assoc directly instead of parsing the serialized string back. *)
val build_request_assoc
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> Yojson.Safe.t

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string
