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

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
