(** Fork-safe, restart-collision-resistant identifiers sampled from the
    operating-system entropy source. *)

val hex : bytes:int -> (string, string) result
(** [hex ~bytes] samples exactly [bytes] bytes and returns lowercase
    hexadecimal. A non-positive byte count is rejected. *)

val create : unit -> (string, string) result
(** Equivalent to [hex ~bytes:16], yielding a 128-bit identifier. *)
