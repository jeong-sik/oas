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

type tool_exchange =
  { tool_uses : Types.content_block list
  ; tool_results : Types.content_block list
  }

(** Return up to [count] latest assistant tool-use/result-span exchanges,
    newest first. Result spans use the same immediate multi-message boundary as
    provider pairing repair. An incomplete latest exchange is returned with an
    empty [tool_results] list so callers can fail explicitly.

    This is intended for one-time checkpoint restoration; live agent loops
    should retain the typed completed-round projection incrementally. *)
val latest_tool_exchanges : count:int -> Types.message list -> tool_exchange list

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
