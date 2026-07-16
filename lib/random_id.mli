(** Fork-safe, restart-collision-resistant identifiers.

    Every identifier is sampled independently from 128 bits of operating-system
    entropy. Entropy failure is explicit and never falls back to clocks,
    counters, process IDs, paths, or content-derived identities. *)

val create : unit -> (string, string) result
