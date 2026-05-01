open Base
(** Request priority for LLM completion scheduling.

    Consumers attach a priority to each completion request.
    The provider layer orders concurrent requests by priority
    when capacity is constrained (via {!Slot_scheduler}).

    @since 0.95.0
    @stability Unstable *)

type t =
  | Resume
  (** P-1: resuming a yielded slot. Higher than Interactive to prevent starvation. *)
  | Interactive (** P0: user-facing chat, tool calls. *)
  | Proactive (** P1: agent turns, scheduled replies. *)
  | Background (** P2: heartbeat, status ticks. *)
  | Unspecified (** Caller did not set priority. Dispatched as Proactive with warning. *)
[@@deriving show]

(** [Background] — safe default that won't starve interactive requests. *)
val default : t

(** [resolve Unspecified] returns [Proactive] and logs a warning.
    All other values pass through unchanged. *)
val resolve : t -> t

val to_string : t -> string
val of_string : string -> t option

(** [Resume < Interactive < Proactive < Background < Unspecified] — lower = higher priority. *)
val compare : t -> t -> int

(** Numeric rank: -1=Resume, 0=Interactive, 1=Proactive, 2=Background, 3=Unspecified. *)
val to_int : t -> int

(** Product-neutral quota tier for schedulers that coordinate many agents.

    OAS only defines the vocabulary and deterministic allocation helper.
    Distributed counters, leases, backpressure propagation, billing, and
    product-specific policy remain downstream coordinator responsibilities. *)
type quota_tier =
  | P0_critical (** Critical/user-visible work. Suggested default share: 40%. *)
  | P1_standard
  (** Standard agent turns and scheduled work. Suggested default share: 40%. *)
  | P2_background
  (** Background maintenance, heartbeat, and status work. Suggested default
      share: 20%. *)
[@@deriving show]

type quota_allocation =
  { tier : quota_tier
  ; share_percent : int
  ; requests_per_minute : int
  }
[@@deriving show]

type quota_allocation_error = Invalid_total_requests_per_minute of int [@@deriving show]

val default_quota_requests_per_minute : int
val quota_tier_label : quota_tier -> string
val quota_tier_share_percent : quota_tier -> int

(** Map request scheduling priority to the Track9 P0/P1/P2 quota vocabulary. *)
val quota_tier_of_priority : t -> quota_tier

(** Deterministically split [total_requests_per_minute] into the default
    40/40/20 quota tiers while preserving the exact total after integer
    rounding. *)
val default_quota_allocations
  :  total_requests_per_minute:int
  -> (quota_allocation list, quota_allocation_error) result

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
