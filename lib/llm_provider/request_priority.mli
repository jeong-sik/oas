(** Request priority for LLM completion scheduling.

    Consumers attach a priority to each completion request.
    The provider layer uses this to order concurrent requests
    when capacity is constrained.

    @since 0.95.0
    @stability Unstable *)

type t =
  | Interactive  (** P0: user-facing chat, tool calls. Dispatched immediately. *)
  | Proactive    (** P1: agent turns, board replies. Dispatched when slots available. *)
  | Background   (** P2: heartbeat, status ticks. Dispatched on remaining capacity. *)

val default : t
(** [Background] — safe default that won't starve interactive requests. *)

val to_string : t -> string
val of_string : string -> t option
val compare : t -> t -> int
(** [Interactive < Proactive < Background] — lower = higher priority. *)

val to_int : t -> int
(** Numeric rank: 0=Interactive, 1=Proactive, 2=Background. *)
