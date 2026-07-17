(** Tool execution helpers — lookup, hooks, event bus, and declared Eio
    scheduling.

    These functions are parameterized by explicit fields rather than [Agent.t]
    to avoid circular module dependencies ([Agent_tools] is compiled before
    [Agent]).

    @stability Internal
    @since 0.93.1 *)

(** {1 Hook invocation} *)

(** Invoke a hook, recording the decision via optional [on_hook_invoked] callback
    and [tracer] span. The callback receives the exact invocation for tool
    events and [None] for turn-level events. Returns the hook's decision. *)
val invoke_hook
  :  ?on_hook_invoked:
       (invocation:Tool.Invocation.t option
        -> hook_name:string
        -> decision:Hooks.hook_decision
        -> detail:string option
        -> unit)
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
  | Unattributed_tool_error

type tool_execution_result =
  { invocation : Tool.Invocation.t
  ; tool_name : string
  ; input : Yojson.Safe.t
    (** Exact input received from the typed [ToolUse] block. Validation never
        rewrites this value. *)
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

type execution_error =
  | Hook_execution_failed of
      { hook_name : string
      ; stage : Hooks.hook_stage
      ; tool_name : string
      ; invocation : Tool.Invocation.t
      ; detail : string
      }

(** Terminal cause observed while dispatching a scheduled tool batch.

    Ordinary observer failures are values at this boundary so every sibling
    already running in the same concurrent batch can finish.  The pipeline
    re-raises [Observer_failure] with the captured backtrace only after it has
    committed [completed_results]. If a hook failure happened first, a later
    observer failure remains the terminal cause so infrastructure faults are
    never hidden; the hook failure is logged as suppressed. *)
type execution_failure_cause =
  | Hook_failure of execution_error
  | Observer_failure of
      { invocation : Tool.Invocation.t
        (** Exact occurrence whose observer failed. *)
      ; exception_ : exn
      ; backtrace : Printexc.raw_backtrace
      }

(** A terminal batch failure together with every tool result that completed
    before, or concurrently with, that failure.  Results remain in original
    [ToolUse] order. *)
type execution_failure =
  { completed_results : tool_execution_result list
  ; cause : execution_failure_cause
  }

(** Find a tool by name and execute it, invoking [PostToolUse] (and
    [PostToolUseFailure] on error) hooks. Publishes [ToolCalled] and
    [ToolCompleted] events to the event bus. A failed post-execution hook is
    returned as [Hook_execution_failed] after the actual tool completion has
    been observed; it is never rewritten as a retryable tool result. An
    ordinary hook observer, tracer, or event-bus failure is re-raised with its
    original backtrace only after completion observers have run. *)
val find_and_execute_tool
  :  context:Context.t
  -> tools:Tool.t list
  -> hooks:Hooks.hooks
  -> event_bus:Event_bus.t option
  -> tracer:Tracing.t
  -> agent_name:string
  -> ?correlation_id:string
  -> ?run_id:string
  -> ?on_hook_invoked:
       (invocation:Tool.Invocation.t option
        -> hook_name:string
        -> decision:Hooks.hook_decision
        -> detail:string option
        -> unit)
  -> invocation:Tool.Invocation.t
  -> string
  -> Yojson.Safe.t
  -> (tool_execution_result, execution_error) result

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
    OAS does not adjudicate external effects; an embedding application must
    settle any such decision before the call reaches this function.

    A tool-handler exception is localized to that call as a non-retryable tool
    result. Typed hook failures and ordinary observer failures are returned as
    values after the current concurrent batch has joined, so they do not cancel
    sibling tool handlers. [Out_of_memory], [Stack_overflow], [Sys.Break], and
    cancellation still propagate through the surrounding Eio scope.

    [on_tool_execution_started] and [on_tool_execution_finished] are
    caller-owned lifecycle observers. Their failures, event-bus publication
    failures, journal projection failures, and tracer failures propagate to the
    caller through [Observer_failure]; OAS never hides an observer failure.

    [Block] produces a model-visible deterministic result without emitting
    tool-execution lifecycle callbacks or durable events, because no tool ran.

    On success, returns one [tool_execution_result] per [ToolUse] block in the
    same relative order as the input. On failure, [completed_results] retains
    every completed result in that order alongside the terminal cause. Serial
    work after a failure is not started; already-running concurrent siblings
    finish under their declared structured-concurrency batch. *)
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
  -> ?correlation_id:string
  -> ?run_id:string
  -> ?on_tool_execution_started:
       (invocation:Tool.Invocation.t -> tool_name:string -> input:Yojson.Safe.t -> unit)
  -> ?on_tool_execution_finished:
       (invocation:Tool.Invocation.t
        -> tool_name:string
        -> content:string
        -> is_error:bool
        -> unit)
  -> ?on_hook_invoked:
       (invocation:Tool.Invocation.t option
        -> hook_name:string
        -> decision:Hooks.hook_decision
        -> detail:string option
        -> unit)
  -> Types.content_block list
  -> (tool_execution_result list, execution_failure) result
