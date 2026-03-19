(** Agent selection strategies for swarm orchestration.

    @since 0.60.0 *)

type 'a strategy =
  | RoundRobin
  | Random
  | Custom of ('a list -> 'a option)

val select : strategy:'a strategy -> candidates:'a list -> 'a option
(** Select one candidate from the list using the given strategy. *)
