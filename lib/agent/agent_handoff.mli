(** Handoff message helpers — pure functions for handoff detection
    and message manipulation.

    These functions operate only on [Types.message] lists and have no
    dependency on [Agent.t].

    @stability Internal
    @since 0.93.1 *)

(** {1 Handoff detection} *)

(** Find the most recent assistant message and extract the first handoff
    [ToolUse].  Returns [(tool_use_id, target_name, prompt)] or [None]. *)
val find_handoff_in_messages : Types.message list -> (string * string * string) option

(** {1 Message manipulation} *)

(** Replace the [ToolResult] for a specific [tool_id] in the most recent
    user message.  Used by handoff interception to overwrite the sentinel
    handler output with the delegated agent response. *)
val replace_tool_result
  :  Types.message list
  -> tool_id:string
  -> content:string
  -> is_error:bool
  -> Types.message list
