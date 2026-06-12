(** Provider-agnostic ToolUse/ToolResult pairing repair.

    These helpers run at provider request serialization boundaries. They keep
    historical state untouched, but make the outbound message list satisfy the
    common provider contract that assistant tool calls are immediately followed
    by matching tool results. *)

val strip_orphaned_tool_results : Types.message list -> Types.message list

(** Insert synthetic error ToolResult messages for assistant ToolUse blocks that
    do not have a matching ToolResult in the immediate result span. *)
val repair_dangling_tool_calls : Types.message list -> Types.message list

(** Drop orphan ToolResult blocks, then close dangling ToolUse blocks. *)
val close_for_provider_request : Types.message list -> Types.message list
