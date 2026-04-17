(** Content_replacement_state ↔ Event_bus bridge (LT-14b / observability).

    Lives in [agent_sdk] alongside {!Metrics_event_bridge}. Pattern is
    identical: wrap the mutator, delegate first (preserving the raise
    contract), then publish on success. On [Invalid_argument] the event
    is suppressed because no state transition occurred. *)

module S = Content_replacement_state

let event_topic = "content_replacement_frozen"

let publish_event ~bus ~correlation_id ~run_id payload =
  let event =
    Event_bus.mk_event
      ~correlation_id
      ~run_id
      (Event_bus.Custom (event_topic, payload))
  in
  Event_bus.publish bus event

let record_replacement_with_events
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (state : S.t)
    (r : S.replacement)
  : unit =
  S.record_replacement state r;
  let payload =
    `Assoc [
      "tool_use_id",      `String r.tool_use_id;
      "action",           `String "replaced";
      "preview",          `String r.preview;
      "original_chars",   `Int r.original_chars;
      "seen_count_after", `Int (S.seen_count state);
    ]
  in
  publish_event ~bus ~correlation_id ~run_id payload

let record_kept_with_events
    ?(correlation_id = "")
    ?(run_id = "")
    (bus : Event_bus.t)
    (state : S.t)
    (tool_use_id : string)
  : unit =
  S.record_kept state tool_use_id;
  let payload =
    `Assoc [
      "tool_use_id",      `String tool_use_id;
      "action",           `String "kept";
      "seen_count_after", `Int (S.seen_count state);
    ]
  in
  publish_event ~bus ~correlation_id ~run_id payload
