open Base
(** Content_replacement_state ↔ Event_bus bridge (LT-14b / observability).

    Lives in [agent_sdk] as an opt-in observability wrapper: delegate first
    (preserving the raise contract), then publish on success. On
    [Invalid_argument] the event is suppressed because no state transition
    occurred. *)

module S = Content_replacement_state

let publish_event ~bus ~correlation_id ~run_id payload =
  let event = Event_bus.mk_event ~correlation_id ~run_id payload in
  Event_bus.publish bus event
;;

let record_replacement_with_events
      ?(correlation_id = "")
      ?(run_id = "")
      (bus : Event_bus.t)
      (state : S.t)
      (r : S.replacement)
  : unit
  =
  S.record_replacement state r;
  publish_event
    ~bus
    ~correlation_id
    ~run_id
    (Event_bus.ContentReplacementReplaced
       { tool_use_id = r.tool_use_id
       ; preview = r.preview
       ; original_chars = r.original_chars
       ; seen_count_after = S.seen_count state
       })
;;

let record_kept_with_events
      ?(correlation_id = "")
      ?(run_id = "")
      (bus : Event_bus.t)
      (state : S.t)
      (tool_use_id : string)
  : unit
  =
  S.record_kept state tool_use_id;
  publish_event
    ~bus
    ~correlation_id
    ~run_id
    (Event_bus.ContentReplacementKept
       { tool_use_id; seen_count_after = S.seen_count state })
;;
