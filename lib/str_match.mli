(** Thin wrapper around [Str.search_forward] for boolean matching. *)

val contains : Str.regexp -> string -> bool
(** [contains re str] returns [true] if [re] matches anywhere in [str].
    Equivalent to [try ignore (Str.search_forward re str 0); true
    with Not_found -> false]. *)
