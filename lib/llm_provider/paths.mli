(** Path SSOT for the OAS SDK.

    Centralises the small number of ambient paths (cwd, user home, generic
    SDK config namespace) so that library code does not call [Sys.getcwd] or
    [Unix.getenv "HOME"] directly. *)

(** Current working directory at the time of the call.

    Raises [Sys_error] only if the cwd cannot be determined at all; callers
    that need a graceful fallback should catch and substitute ["."]. *)
val cwd : unit -> string

(** User home directory, if [HOME] is set and non-empty. *)
val home_dir : unit -> string option

(** [user_config_file filename] returns [$HOME/.config/oas/<filename>] when
    [HOME] is available, otherwise [None]. *)
val user_config_file : string -> string option
