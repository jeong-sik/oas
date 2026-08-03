(** Fork-safe, restart-collision-resistant identifiers.

    Every identifier is sampled independently from operating-system entropy.
    Entropy failure is explicit and never falls back to clocks, counters,
    process IDs, paths, or content-derived identities. *)

(** [hex ~bytes] samples exactly [bytes] bytes from the operating-system
    entropy source and returns their lowercase hexadecimal encoding.  A
    non-positive byte count is rejected. *)
val hex : bytes:int -> (string, string) result

(** Equivalent to [hex ~bytes:16], yielding a 128-bit identifier. *)
val create : unit -> (string, string) result
