(** Tool execution helpers — lookup, hooks, event bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than [Agent.t]
    to avoid circular module dependencies ([Agent_tools] is compiled before
    [Agent]).

    @stability Internal
    @since 0.93.0 *)

(** {1 Hook invocation} *)

(** Invoke a hook, recording the decision via optional [on_hook_invoked] callback
    and [tracer] span.  Returns the hook's decision. *)
val invoke_hook :
  ?on_hook_invoked:(hook_name:string ->
    decision:Hooks.hook_decision ->
    detail:string option -> unit) ->
  tracer:Tracing.t ->
  agent_name:string ->
  turn_count:int ->
  hook_name:string ->
  (Hooks.hook_event -> Hooks.hook_decision) option ->
  Hooks.hook_event ->
  Hooks.hook_decision

(** {1 Single tool execution} *)

(** Find a tool by name and execute it, invoking [PostToolUse] (and
    [PostToolUseFailure] on error) hooks.  Publishes [ToolCalled] and
    [ToolCompleted] events to the event bus.
    Returns [(tool_use_id, content, is_error)]. *)
val find_and_execute_tool :
  context:Context.t ->
  tools:Tool.t list ->
  hooks:Hooks.hooks ->
  event_bus:Event_bus.t option ->
  tracer:Tracing.t ->
  agent_name:string ->
  turn_count:int ->
  ?on_hook_invoked:(hook_name:string ->
    decision:Hooks.hook_decision ->
    detail:string option -> unit) ->
  string -> Yojson.Safe.t -> string ->
  string * string * bool

(** {1 Parallel tool execution} *)

(** Execute tool-use content blocks in parallel using Eio fibers.

    Non-[ToolUse] blocks in the input list are filtered out before
    execution — only [ToolUse] blocks produce result triples.

    For each [ToolUse] block, applies the [PreToolUse] hook before execution.
    Supports approval flow: if the hook returns [ApprovalRequired], the
    [approval] callback is invoked.

    Each fiber catches exceptions to prevent one tool failure from
    canceling siblings (except [Out_of_memory], [Stack_overflow],
    [Sys.Break]).

    Returns [(tool_use_id, content, is_error)] triples, one per [ToolUse]
    block, in the same relative order as the input. *)
val execute_tools :
  context:Context.t ->
  tools:Tool.t list ->
  hooks:Hooks.hooks ->
  event_bus:Event_bus.t option ->
  tracer:Tracing.t ->
  agent_name:string ->
  turn_count:int ->
  usage:Types.usage_stats ->
  approval:Hooks.approval_callback option ->
  ?on_tool_execution_started:(tool_use_id:string ->
    tool_name:string -> input:Yojson.Safe.t -> unit) ->
  ?on_tool_execution_finished:(tool_use_id:string ->
    tool_name:string -> content:string -> is_error:bool -> unit) ->
  ?on_hook_invoked:(hook_name:string ->
    decision:Hooks.hook_decision ->
    detail:string option -> unit) ->
  Types.content_block list ->
  (string * string * bool) list
