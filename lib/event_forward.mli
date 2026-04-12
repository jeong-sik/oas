(** Event Forwarding — deliver {!Event_bus} events to external targets.

    Supports file append and custom transports.
    Delivery is best-effort: failures log a warning but never block
    the agent.  Batching reduces HTTP round-trips.

    Runs in a separate Eio fiber (non-blocking to agent).

    @stability Evolving
    @since 0.93.1 *)

(** {1 Payload} *)

(** Serializable event payload for delivery. *)
type event_payload = {
  event_type: string;
  timestamp: float;
  agent_name: string option;
  correlation_id: string;
  run_id: string;
  data: Yojson.Safe.t;
}

(** Serialize a payload to JSON. *)
val payload_to_json : event_payload -> Yojson.Safe.t

(** Extract the event type name string from a bus event. *)
val event_type_name : Event_bus.event -> string

(** Extract the agent name from a bus event, if present. *)
val agent_name_of_event : Event_bus.event -> string option

(** Convert a bus event to a delivery payload. *)
val event_to_payload : Event_bus.event -> event_payload

(** {1 Targets} *)

(** Delivery target for forwarded events.
    For HTTP delivery, use {!Custom_target} with an HTTP client callback. *)
type target =
  | File_append of { path: string }
  | Custom_target of { name: string; deliver: event_payload -> unit }

(** {1 Forwarder} *)

(** Opaque forwarder state. *)
type t

(** Create a forwarder with the given targets and batching parameters.
    @param batch_size flush after this many events (default 10)
    @param flush_interval_s maximum time between flushes in seconds (default 1.0) *)
val create :
  targets:target list ->
  ?batch_size:int ->
  ?flush_interval_s:float ->
  unit -> t

(** {1 Lifecycle} *)

(** Start the forwarding fiber.  Subscribes to [bus] and begins
    delivering events to configured targets.  Idempotent: calling
    [start] on an already-running forwarder is a no-op. *)
val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  bus:Event_bus.t ->
  t -> unit

(** Signal the forwarding fiber to stop after draining remaining events. *)
val stop : t -> unit

(** {1 Counters} *)

(** Number of payloads delivered so far. *)
val delivered_count : t -> int

(** Number of payloads that failed delivery. *)
val failed_count : t -> int
