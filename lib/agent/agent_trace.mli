open Base
(** Raw-trace integration helpers for the Agent module.

    Wraps [Raw_trace] calls with lifecycle updates and hook invocation
    tracing.  These functions depend on [Agent_types.t] (mutable state).

    @stability Internal
    @since 0.93.1 *)

open Agent_types

(** {1 Hook recording} *)

(** Record a hook invocation in the active raw-trace run (no-op when
    [active_run] is [None]). *)
val record_hook_invocation
  :  Raw_trace.active_run option
  -> hook_name:string
  -> decision:Hooks.hook_decision
  -> ?detail:string
  -> unit
  -> unit

(** Invoke a hook within a tracing span, recording the decision. *)
val invoke_hook_with_trace
  :  t
  -> ?raw_trace_run:Raw_trace.active_run
  -> hook_name:string
  -> (Hooks.hook_event -> Hooks.hook_decision) option
  -> Hooks.hook_event
  -> Hooks.hook_decision

(** {1 Tool execution with tracing} *)

(** Execute tool-use blocks with full raw-trace recording (started/finished
    events, lifecycle updates).  Delegates to [Agent_tools.execute_tools]. *)
val execute_tools_with_trace
  :  t
  -> Raw_trace.active_run option
  -> Types.content_block list
  -> Agent_tools.tool_execution_result list

(** {1 Assistant block recording} *)

(** Record assistant content blocks in the active run. *)
val trace_assistant_blocks
  :  Raw_trace.active_run option
  -> Types.content_block list
  -> (unit, Error.sdk_error) result

(** {1 Run lifecycle} *)

(** Execute [f] within a raw-trace run, handling start/finish recording
    and lifecycle status updates.  [f] receives [Some active_run] when
    raw-trace is configured, [None] otherwise. *)
val with_raw_trace_run
  :  t
  -> string
  -> (Raw_trace.active_run option -> (Types.api_response, Error.sdk_error) result)
  -> (Types.api_response, Error.sdk_error) result
