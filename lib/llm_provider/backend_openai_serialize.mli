(** OpenAI-compatible request serialization.

    @since 0.92.0 extracted from Backend_openai

    @stability Internal
    @since 0.93.1 *)

val tool_calls_to_openai_json : Types.content_block list -> Yojson.Safe.t list
val openai_content_parts_of_blocks : Types.content_block list -> Yojson.Safe.t list
val openai_messages_of_message : Types.message -> Yojson.Safe.t list
val glm_messages_of_message : Types.message -> Yojson.Safe.t list

val dialect_messages_of_message
  :  ?assistant_tool_content_format:Capability_vocab.assistant_tool_content_format
  -> Reasoning_dialect.t
  -> Types.message
  -> Yojson.Safe.t list

val ollama_messages_of_message : ?model_id:string -> Types.message -> Yojson.Safe.t list
val tool_choice_to_openai_json : Types.tool_choice -> Yojson.Safe.t

(** [parallel_tool_calls_fields ~disable_parallel ~tools_present] returns the
    OpenAI-compatible [parallel_tool_calls] body field: [("parallel_tool_calls",
    `Bool false)] in a singleton list when parallel calls are disabled and tools
    are present, else the empty list. Shared by the low-level request builder and
    the agent-state-aware API layer so the wire shape is defined once. *)
val parallel_tool_calls_fields
  :  disable_parallel:bool
  -> tools_present:bool
  -> (string * Yojson.Safe.t) list

val build_openai_tool_json : Yojson.Safe.t -> Yojson.Safe.t

(** Remove ToolResult blocks outside the immediate result span following their
    Assistant ToolUse message. Kept for compatibility; provider request builders
    should normally call {!close_tool_message_pairs_for_request}. @since 0.103.0
*)
val strip_orphaned_tool_results : Types.message list -> Types.message list

val strip_orphaned_tool_results_with_report
  :  Types.message list
  -> Types.message list * Tool_message_pairs.repair_report

(** Normalize tool-call pairing before provider request serialization:
    orphan ToolResult blocks are removed and dangling Assistant ToolUse blocks
    receive synthetic error ToolResult messages. *)
val close_tool_message_pairs_for_request : Types.message list -> Types.message list

val close_tool_message_pairs_for_request_with_report
  :  Types.message list
  -> Types.message list * Tool_message_pairs.repair_report

(** Remove Thinking blocks from all messages. Deepseek-compatible APIs
    reject [reasoning_content] in request messages — it is response-only.
    Call before serializing messages for OpenAI-compatible APIs.
    @since 0.184.0 *)
val strip_thinking_blocks : Types.message list -> Types.message list
