(** Content_replacement_state ↔ Event_bus bridge.

    Opt-in wrappers around [Content_replacement_state.record_replacement]
    and [record_kept]. Each call delegates to the original mutator, then
    publishes a [Custom("content_replacement_frozen", ...)] event on the
    supplied bus.

    The raw [Content_replacement_state] module itself stays free of any
    event-bus dependency so the "silent FSM" boundary between pure state
    and observability is preserved.

    {1 Payload shape}

    {[
      {
        "tool_use_id": "toolu_xxx",
        "action":       "replaced" | "kept",
        "preview":      "..."     (* present only when action = "replaced" *),
        "original_chars": 9000    (* present only when action = "replaced" *),
        "seen_count_after": 3
      }
    ]}

    [action] is the discriminator that lets downstream subscribers
    separate the two freeze variants without looking for optional keys.

    @since 0.154.0 *)

(** Wrap [Content_replacement_state.record_replacement]: record the
    replacement decision, then publish a [Custom("content_replacement_frozen",
    {action = "replaced"; ...})] event.

    [correlation_id] / [run_id] default to empty strings, matching the
    {!Metrics_event_bridge} convention for callers that do not yet have
    per-run identifiers wired.

    @raise Invalid_argument if the [tool_use_id] is already frozen; the
    event is not published in that case. *)
val record_replacement_with_events :
  ?correlation_id:string ->
  ?run_id:string ->
  Event_bus.t ->
  Content_replacement_state.t ->
  Content_replacement_state.replacement ->
  unit

(** Wrap [Content_replacement_state.record_kept]: record the kept
    decision, then publish a [Custom("content_replacement_frozen",
    {action = "kept"; ...})] event.

    @raise Invalid_argument if the [tool_use_id] is already frozen; the
    event is not published in that case. *)
val record_kept_with_events :
  ?correlation_id:string ->
  ?run_id:string ->
  Event_bus.t ->
  Content_replacement_state.t ->
  string ->
  unit
