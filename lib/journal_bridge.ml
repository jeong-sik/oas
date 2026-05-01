open Base
(** Bridge Durable_event journal events to Event_bus.

    Provides an [on_append] callback factory that projects each
    {!Durable_event.event} onto {!Event_bus.Custom} with a stable
    [durable.<kind>] name.  This lets existing Event_bus subscribers
    observe the full journal stream without any Event_bus payload
    schema changes.

    Typical use:
    {[
      let journal =
        Durable_event.create ~on_append:(Journal_bridge.make ~bus ()) ()
      in
      ...
    ]}

    @since 0.133.0 *)

let projection_of_event (evt : Durable_event.event) : string * Yojson.Safe.t =
  match evt with
  | Turn_started { turn; timestamp } ->
    "durable.turn_started", `Assoc [ "turn", `Int turn; "timestamp", `Float timestamp ]
  | Llm_request { turn; model; input_tokens; timestamp } ->
    ( "durable.llm_request"
    , `Assoc
        [ "turn", `Int turn
        ; "model", `String model
        ; "input_tokens", `Int input_tokens
        ; "timestamp", `Float timestamp
        ] )
  | Llm_response { turn; output_tokens; stop_reason; duration_ms; timestamp } ->
    ( "durable.llm_response"
    , `Assoc
        [ "turn", `Int turn
        ; "output_tokens", `Int output_tokens
        ; "stop_reason", `String stop_reason
        ; "duration_ms", `Float duration_ms
        ; "timestamp", `Float timestamp
        ] )
  | Tool_called { turn; tool_name; idempotency_key; input_hash; timestamp } ->
    ( "durable.tool_called"
    , `Assoc
        [ "turn", `Int turn
        ; "tool_name", `String tool_name
        ; "idempotency_key", `String idempotency_key
        ; "input_hash", `String input_hash
        ; "timestamp", `Float timestamp
        ] )
  | Tool_completed
      { turn; tool_name; idempotency_key; output_json; is_error; duration_ms; timestamp }
    ->
    ( "durable.tool_completed"
    , `Assoc
        [ "turn", `Int turn
        ; "tool_name", `String tool_name
        ; "idempotency_key", `String idempotency_key
        ; "output_json", output_json
        ; "is_error", `Bool is_error
        ; "duration_ms", `Float duration_ms
        ; "timestamp", `Float timestamp
        ] )
  | State_transition { from_state; to_state; reason; timestamp } ->
    ( "durable.state_transition"
    , `Assoc
        [ "from_state", `String from_state
        ; "to_state", `String to_state
        ; "reason", `String reason
        ; "timestamp", `Float timestamp
        ] )
  | Checkpoint_saved { checkpoint_id; timestamp } ->
    ( "durable.checkpoint_saved"
    , `Assoc [ "checkpoint_id", `String checkpoint_id; "timestamp", `Float timestamp ] )
  | Error_occurred { turn; error_domain; detail; timestamp } ->
    ( "durable.error_occurred"
    , `Assoc
        [ "turn", `Int turn
        ; "error_domain", `String error_domain
        ; "detail", `String detail
        ; "timestamp", `Float timestamp
        ] )
;;

let make ~bus ?correlation_id ?run_id () : Durable_event.event -> unit =
  fun evt ->
  let name, payload = projection_of_event evt in
  Event_bus.publish
    bus
    (Event_bus.mk_event ?correlation_id ?run_id (Custom (name, payload)))
;;
