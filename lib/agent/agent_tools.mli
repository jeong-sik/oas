(** Tool execution helpers — lookup, hooks, event bus, and effect-aware
    Eio scheduling.

    These functions are parameterized by explicit fields rather than [Agent.t]
    to avoid circular module dependencies ([Agent_tools] is compiled before
    [Agent]).

    @stability Internal
    @since 0.93.1 *)

(** {1 Hook invocation} *)

(** Invoke a hook, recording the decision via optional [on_hook_invoked] callback
    and [tracer] span.  Returns the hook's decision. *)
val invoke_hook
  :  ?on_hook_invoked:
       (hook_name:string -> decision:Hooks.hook_decision -> detail:string option -> unit)
  -> tracer:Tracing.t
  -> agent_name:string
  -> turn_count:int
  -> hook_name:string
  -> (Hooks.hook_event -> Hooks.hook_decision) option
  -> Hooks.hook_event
  -> Hooks.hook_decision

(** {1 Single tool execution} *)

type tool_index

(** Build a stable lookup index for the current tool set.

    Exact tool-name lookups preserve first-match list semantics. The typed
    [Tool_id] key is also populated so normalized built-in/MCP names can share
    the same dispatch path without each caller scanning the full list. *)
val build_index : Tool.t list -> tool_index

(** [find_in_index index name] resolves a tool by its registered name. Falls
    back to [Tool_id.of_string] for canonical built-ins and MCP IDs only;
    user-defined tools (which [Tool_id.of_string] would map to [User _]) must
    match by [by_name] exactly to avoid case-variant cross-dispatch.

    Example: with only ["mytool"] registered, [find_in_index idx "MYTOOL"]
    returns [None] — not the lowercased neighbor — so approval and audit
    context cannot be applied to a tool the caller didn't actually name. *)
val find_in_index : tool_index -> string -> Tool.t option

type tool_failure_kind = Types.tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error

type tool_execution_result =
  { tool_use_id : string
  ; tool_name : string
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

(** Find a tool by name and execute it, invoking [PostToolUse] (and
    [PostToolUseFailure] on error) hooks.  Publishes [ToolCalled] and
    [ToolCompleted] events to the event bus. *)
val find_and_execute_tool
  :  context:Context.t
  -> tools:Tool.t list
  -> hooks:Hooks.hooks
  -> event_bus:Event_bus.t option
  -> tracer:Tracing.t
  -> agent_name:string
  -> turn_count:int
  -> ?correlation_id:string
  -> ?run_id:string
  -> ?on_hook_invoked:
       (hook_name:string -> decision:Hooks.hook_decision -> detail:string option -> unit)
  -> schedule:Hooks.tool_schedule
  -> string
  -> Yojson.Safe.t
  -> string
  -> tool_execution_result

(** {1 Tool scheduling and execution} *)

(** Execute tool-use content blocks using declared concurrency classes.

    Non-[ToolUse] blocks in the input list are filtered out before
    execution — only [ToolUse] blocks produce result triples.

    Scheduling is deterministic:
    - [Tool.Parallel_read] blocks are executed together in a parallel batch.
    - [Tool.Sequential_workspace] blocks run one-at-a-time in input order.
    - [Tool.Exclusive_external] blocks flush all pending batches before
      executing and run in complete isolation (no overlap with any other
      tool call).  The schedule metadata carries [batch_kind = "exclusive"]
      for audit purposes.
    - Tools without a declared descriptor default to sequential execution.

    For each [ToolUse] block, applies the [PreToolUse] hook before execution.
    Supports approval flow: if the hook returns [ApprovalRequired], the
    [approval] callback is invoked. If no callback is registered,
    [missing_approval_callback_policy] chooses fail-open compatibility or
    fail-closed rejection.

    Parallel batches catch exceptions per fiber to prevent one tool failure
    from canceling siblings (except [Out_of_memory], [Stack_overflow],
    [Sys.Break], and cancellation).

    [on_tool_execution_started] and [on_tool_execution_finished] are
    best-effort lifecycle observers. Non-fatal exceptions raised by these
    callbacks are logged and do not change the tool result.

    Returns one [tool_execution_result] per [ToolUse] block in the same
    relative order as the input. *)
val execute_tools
  :  context:Context.t
  -> tools:Tool.t list
  -> hooks:Hooks.hooks
  -> event_bus:Event_bus.t option
  -> ?journal:Durable_event.journal
  -> tracer:Tracing.t
  -> agent_name:string
  -> turn_count:int
  -> usage:Types.usage_stats
  -> approval:Hooks.approval_callback option
  -> missing_approval_callback_policy:Hooks.missing_approval_callback_policy
  -> ?correlation_id:string
  -> ?run_id:string
  -> ?on_tool_execution_started:
       (tool_use_id:string
        -> tool_name:string
        -> input:Yojson.Safe.t
        -> schedule:Hooks.tool_schedule
        -> unit)
  -> ?on_tool_execution_finished:
       (tool_use_id:string -> tool_name:string -> content:string -> is_error:bool -> unit)
  -> ?on_hook_invoked:
       (hook_name:string -> decision:Hooks.hook_decision -> detail:string option -> unit)
  -> Types.content_block list
  -> tool_execution_result list
