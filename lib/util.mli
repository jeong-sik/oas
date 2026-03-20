(** Shared utility functions. *)

(** {1 Option helpers} *)

val first_some : 'a option -> 'a option -> 'a option

(** {1 String helpers} *)

val string_contains : needle:string -> string -> bool

(** {1 Error constructors} *)

val json_parse_error : string -> Error.sdk_error
val file_read_error : path:string -> detail:string -> Error.sdk_error
val file_write_error : path:string -> detail:string -> Error.sdk_error

(** {1 List helpers} *)

val snoc : 'a list -> 'a -> 'a list
val snoc_list : 'a list -> 'a list -> 'a list
