(** Request priority for LLM completion scheduling.

    Consumers attach a priority to each completion request.
    The provider layer will use this to order concurrent requests
    when capacity is constrained (not yet active — reserved for
    Phase 1 slot scheduler).

    @since 0.95.0
    @stability Unstable *)

type t =
  | Interactive  (** P0: user-facing chat, tool calls. *)
  | Proactive    (** P1: agent turns, board replies. *)
  | Background   (** P2: heartbeat, status ticks. *)

val default : t
(** [Background] — safe default that won't starve interactive requests. *)

val to_string : t -> string
val of_string : string -> t option
val compare : t -> t -> int
(** [Interactive < Proactive < Background] — lower = higher priority. *)

val to_int : t -> int
(** Numeric rank: 0=Interactive, 1=Proactive, 2=Background. *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result
