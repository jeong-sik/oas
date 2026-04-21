(** OpenAI-compatible request serialization.

    @since 0.92.0 extracted from Backend_openai

    @stability Internal
    @since 0.93.1 *)

val tool_calls_to_openai_json : Types.content_block list -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val glm_messages_of_message : Types.message -> Yojson.Safe.t list
val ollama_messages_of_message : Types.message -> Yojson.Safe.t list
val tool_choice_to_openai_json : Types.tool_choice -> Yojson.Safe.t
val build_openai_tool_json : Yojson.Safe.t -> Yojson.Safe.t

val strip_orphaned_tool_results : Types.message list -> Types.message list
(** Remove ToolResult blocks whose tool_use_id has no matching ToolUse
    in any Assistant message. Call before serializing messages for
    OpenAI-compatible APIs to prevent orphaned tool_call_id errors
    after context compaction.  @since 0.103.0 *)
