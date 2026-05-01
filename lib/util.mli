(** Shared utility functions.

    Small helpers extracted from multiple modules to eliminate
    duplication.

    @stability Internal
    @since 0.93.1 *)

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

(** Truncate string to [max_len], appending "..." if truncated. *)
val clip : string -> int -> string

(** Safe substring: returns "" if start is past end or len is negative. *)
val safe_sub : string -> int -> int -> string

(** Case-insensitive substring search. *)
val contains_substring_ci : haystack:string -> needle:string -> bool

(** [regex_match re s] returns [true] if regex [re] matches anywhere in [s]. *)
val regex_match : Str.regexp -> string -> bool

(** Filter out empty strings from a list. *)
val filter_non_empty : string list -> string list

(** Split on [sep], trim each fragment, discard empty results. *)
val split_on_char_trim : char -> string -> string list
