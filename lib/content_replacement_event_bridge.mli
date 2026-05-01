(** Content_replacement_state ↔ Event_bus bridge.

    Opt-in wrappers around [Content_replacement_state.record_replacement]
    and [record_kept]. Each call delegates to the original mutator, then
    publishes a native content-replacement event on the supplied bus.

    The raw [Content_replacement_state] module itself stays free of any
    event-bus dependency so the "silent FSM" boundary between pure state
    and observability is preserved.

    {1 Native variants}

    - [ContentReplacementReplaced] carries [tool_use_id], preview detail,
      and [seen_count_after].
    - [ContentReplacementKept] carries [tool_use_id] and [seen_count_after].

    @since 0.154.0 *)

(** Wrap [Content_replacement_state.record_replacement]: record the
    replacement decision, then publish a [ContentReplacementReplaced] event.

    [correlation_id] / [run_id] default to empty strings, matching the
    convention used by the other event-bus wrappers for callers that do not
    yet have per-run identifiers wired.

    @raise Invalid_argument if the [tool_use_id] is already frozen; the
    event is not published in that case. *)
val record_replacement_with_events
  :  ?correlation_id:string
  -> ?run_id:string
  -> Event_bus.t
  -> Content_replacement_state.t
  -> Content_replacement_state.replacement
  -> unit

(** Wrap [Content_replacement_state.record_kept]: record the kept
    decision, then publish a [ContentReplacementKept] event.

    @raise Invalid_argument if the [tool_use_id] is already frozen; the
    event is not published in that case. *)
val record_kept_with_events
  :  ?correlation_id:string
  -> ?run_id:string
  -> Event_bus.t
  -> Content_replacement_state.t
  -> string
  -> unit
