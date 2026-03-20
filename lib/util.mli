(** Shared utility functions.

    Small helpers extracted from multiple modules to eliminate
    duplication. *)

(** Return [a] if [Some], otherwise [b]. *)
val first_some : 'a option -> 'a option -> 'a option

(** Check whether [haystack] contains [needle] as a substring. *)
val string_contains : needle:string -> string -> bool

(** Construct a {!Error.Serialization} [JsonParseError]. *)
val json_parse_error : string -> Error.sdk_error

(** Construct a {!Error.Io} [FileOpFailed] with [op = "read"]. *)
val file_read_error : path:string -> detail:string -> Error.sdk_error

(** Construct a {!Error.Io} [FileOpFailed] with [op = "write"]. *)
val file_write_error : path:string -> detail:string -> Error.sdk_error

(** Append a single element to the end of a list. *)
val snoc : 'a list -> 'a -> 'a list

(** Concatenate two lists (alias for [@]). *)
val snoc_list : 'a list -> 'a list -> 'a list

(** Traverse a list with [f], collecting [Ok] values.
    Short-circuits on first [Error]. *)
val result_traverse : f:('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
