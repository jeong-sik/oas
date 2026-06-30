(** Provider-agnostic ToolUse/ToolResult pairing repair.

    These helpers run at provider request serialization boundaries. They keep
    historical state untouched, but make the outbound message list satisfy the
    common provider contract that assistant tool calls are immediately followed
    by matching tool results. *)

type dropped_tool_result_reason =
  | Orphaned_tool_result
  | Duplicate_tool_result

type dropped_tool_result =
  { tool_use_id : string
  ; reason : dropped_tool_result_reason
  }

type repair_report =
  { dropped_tool_results : dropped_tool_result list
  ; synthesized_tool_result_ids : string list
  }

val empty_repair_report : repair_report
val strip_orphaned_tool_results : Types.message list -> Types.message list

val strip_orphaned_tool_results_with_report
  :  Types.message list
  -> Types.message list * repair_report

(** Insert synthetic error ToolResult messages for assistant ToolUse blocks that
    do not have a matching ToolResult in the immediate result span. *)
val repair_dangling_tool_calls : Types.message list -> Types.message list

val repair_dangling_tool_calls_with_report
  :  Types.message list
  -> Types.message list * repair_report

(** Drop orphan ToolResult blocks, then close dangling ToolUse blocks. *)
val close_for_provider_request : Types.message list -> Types.message list

val close_for_provider_request_with_report
  :  Types.message list
  -> Types.message list * repair_report
