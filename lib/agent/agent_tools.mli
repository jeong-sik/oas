(** Tool execution helpers — lookup, hooks, event bus, and declared Eio
    scheduling.

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

    Exact tool-name lookups preserve first-match list semantics. OAS does not
    classify or normalize names; any alias must be registered explicitly by
    the consumer. *)
val build_index : Tool.t list -> tool_index

(** [find_in_index index name] resolves only the exact registered name. *)
val find_in_index : tool_index -> string -> Tool.t option

type tool_failure_kind = Types.tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error
  | Reported_tool_error

type tool_execution_result =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
    (** Exact input received from the typed [ToolUse] block. Validation never
        rewrites this value. *)
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

(** Execute tool-use content blocks using the declared execution mode.

    Non-[ToolUse] blocks in the input list are filtered out before
    execution — only [ToolUse] blocks produce result triples.

    Scheduling is deterministic:
    - [Tool.Concurrent] calls in the same contiguous batch may overlap.
    - [Tool.Serial] calls run one-at-a-time in input order and separate
      concurrent batches.
    - Tools without a declared descriptor default to [Tool.Serial].

    For each [ToolUse] block, applies the [PreToolUse] hook before execution.
    Supports approval flow: if the hook returns [ApprovalRequired], the
    [approval] callback is invoked. If no callback is registered, the call
    becomes an explicit failed tool result. An always-allowed host installs a
    callback returning [Hooks.Approve].

    Concurrent batches catch exceptions per fiber to prevent one tool failure
    from canceling siblings (except [Out_of_memory], [Stack_overflow],
    [Sys.Break], and cancellation).

    [on_tool_execution_started] and [on_tool_execution_finished] are
    caller-owned lifecycle observers. Callback exceptions propagate to the
    caller; OAS never hides an observer failure.

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
