(** OpenAI-compatible request serialization.

    @since 0.92.0 extracted from Backend_openai

    @stability Internal
    @since 0.93.0 *)

val tool_calls_to_openai_json : Types.content_block list -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val tool_choice_to_openai_json : Types.tool_choice -> Yojson.Safe.t
val build_openai_tool_json : Yojson.Safe.t -> Yojson.Safe.t
