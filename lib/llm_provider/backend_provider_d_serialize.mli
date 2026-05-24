(** Provider_d-compatible request serialization.

    @since 0.92.0 extracted from Backend_provider_d

    @stability Internal
    @since 0.93.1 *)

val tool_calls_to_provider_d_json : Types.content_block list -> Yojson.Safe.t list
val provider_d_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val provider_d_messages_of_message : Types.message -> Yojson.Safe.t list
val glm_messages_of_message : Types.message -> Yojson.Safe.t list
val ollama_messages_of_message : ?model_id:string -> Types.message -> Yojson.Safe.t list
val tool_choice_to_provider_d_json : Types.tool_choice -> Yojson.Safe.t
val build_provider_d_tool_json : Yojson.Safe.t -> Yojson.Safe.t

(** Remove ToolResult blocks whose tool_use_id has no matching ToolUse
    in any Assistant message. Call before serializing messages for
    Provider_d-compatible APIs to prevent orphaned tool_call_id errors
    after context compaction.  @since 0.103.0 *)
val strip_orphaned_tool_results : Types.message list -> Types.message list

(** Remove Thinking blocks from all messages. Provider_g-compatible APIs
    reject [reasoning_content] in request messages — it is response-only.
    Call before serializing messages for Provider_d-compatible APIs.
    @since 0.184.0 *)
val strip_thinking_blocks : Types.message list -> Types.message list
