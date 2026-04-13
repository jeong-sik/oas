(** Bridge Durable_event journal events to Event_bus.

    Each journal event is projected onto {!Event_bus.Custom} with a
    stable [durable:<kind>] name (e.g. [durable:turn_started],
    [durable:llm_request]).  Subscribers observe these via the normal
    Event_bus subscription — no payload schema change required.

    @since 0.133.0 *)

(** Serialize a journal event into the [(name, payload)] shape used by
    {!Event_bus.Custom}.  Exposed for testing and custom relays. *)
val projection_of_event :
  Durable_event.event -> string * Yojson.Safe.t

(** Build an [on_append] callback suitable for {!Durable_event.create}
    that publishes every journal event to [bus] as an
    {!Event_bus.Custom} payload. *)
val make : bus:Event_bus.t -> Durable_event.event -> unit
