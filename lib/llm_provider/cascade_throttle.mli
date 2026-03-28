(** Per-endpoint admission throttle table.

    Shared throttle table mapping endpoint URLs to {!Provider_throttle.t}.
    Populated lazily from {!Discovery} slot data.

    @since 0.91.0
    @since 0.92.0 extracted from Cascade_config

    @stability Internal
    @since 0.93.0 *)

(** Update the throttle table from discovery probe results.
    Creates entries for healthy endpoints with slot data.
    Evicts entries for unhealthy endpoints. *)
val populate : Discovery.endpoint_status list -> unit

(** Lookup the throttle for a given endpoint URL. *)
val lookup : string -> Provider_throttle.t option

(** Clear all throttle entries (for testing). *)
val clear : unit -> unit

(** Number of throttle entries (for testing). *)
val length : unit -> int
