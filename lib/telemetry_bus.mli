(** Typed telemetry event bus.

    Wraps {!Event_bus.t} with a {!Llm_provider.Telemetry_event.t}-typed API.
    Events are serialized to JSON and published as
    [Event_bus.Custom("telemetry_event", json)] so downstream
    consumers can filter by topic.

    All state is internal to [t] — no globals.

    @since 0.189.0 *)

type t

(** Create a telemetry bus backed by lossless per-subscriber FIFOs. *)
val create : unit -> t

(** Wrap an existing {!Event_bus.t} as a telemetry bus.

    Events are published to the same underlying bus, so subscribers
    using {!Event_bus} directly will also receive them. *)
val of_event_bus : Event_bus.t -> t

(** Publish a telemetry event to all subscribers.

    The event is wrapped in an [Event_bus.event] with
    [Custom("telemetry_event", json)] payload and a fresh envelope. *)
val publish : t -> Llm_provider.Telemetry_event.t -> unit

(** {2 Subscription} *)

type subscription

(** Subscribe to telemetry events.

    - [?purpose] — free-form label surfaced in stats. *)
val subscribe : ?purpose:string -> t -> subscription

val unsubscribe : t -> subscription -> unit

(** {2 Drain} *)

(** Remove and return all queued events for a subscriber. *)
val drain : subscription -> Llm_provider.Telemetry_event.t list

(** {2 Queries} *)

val subscriber_count : t -> int
val stats : t -> Event_bus.bus_stats
