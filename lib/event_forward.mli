(** Event forwarding -- deliver {!Event_bus} events to external targets.

    Supports HTTP webhooks, file append, and custom transports.
    Delivery is best-effort: failures log a warning but do not block
    the agent.  Batching reduces HTTP round-trips. *)

(** {1 Payload} *)

type event_payload = {
  event_type: string;
  timestamp: float;
  agent_name: string option;
  data: Yojson.Safe.t;
}

val payload_to_json : event_payload -> Yojson.Safe.t

(** {1 Event conversion} *)

val event_type_name : Event_bus.event -> string
val agent_name_of_event : Event_bus.event -> string option
val event_to_payload : Event_bus.event -> event_payload

(** {1 Targets} *)

type target =
  | Webhook of {
      url: string;
      headers: (string * string) list;
      method_: [ `POST | `PUT ];
      timeout_s: float;
    }
  | File_append of { path: string }
  | Custom_target of { name: string; deliver: event_payload -> unit }

(** {1 Forwarder} *)

type t

val create :
  targets:target list ->
  ?batch_size:int ->
  ?flush_interval_s:float ->
  unit -> t

(** {1 Lifecycle} *)

(** Start forwarding events from [bus] in a separate Eio fiber. *)
val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  bus:Event_bus.t ->
  t -> unit

val stop : t -> unit

(** {1 Stats} *)

val delivered_count : t -> int
val failed_count : t -> int
