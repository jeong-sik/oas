(** Result-based filesystem operations.

    All functions return [('a, Error.sdk_error) result] instead of raising.
    Catches [Eio.Io], [Unix.Unix_error], [Sys_error], and [Failure].
    Non-recoverable exceptions (e.g. [Out_of_memory]) are re-raised.

    @raises only truly non-recoverable exceptions

    @stability Evolving
    @since 0.93.0 *)

(** {1 Core I/O} *)

(** Read entire file contents. *)
val read_file : string -> (string, Error.sdk_error) result

(** Write file atomically via .tmp + rename. Creates parent dirs. *)
val write_file : string -> string -> (unit, Error.sdk_error) result

(** Append content to file. Creates parent dirs if needed. *)
val append_file : string -> string -> (unit, Error.sdk_error) result

(** {1 Directory operations} *)

(** Ensure directory exists (recursive). *)
val ensure_dir : string -> (unit, Error.sdk_error) result

(** List directory entries. *)
val read_dir : string -> (string list, Error.sdk_error) result

(** {1 Queries} *)

(** Check if path exists as a regular file. *)
val file_exists : string -> bool

(** {1 Deletion} *)

(** Remove a file. Returns [Ok ()] if file does not exist. *)
val remove_file : string -> (unit, Error.sdk_error) result

(** {1 Utilities} *)

(** Convert I/O exceptions to [Error.sdk_error].
    Re-raises non-recoverable exceptions.
    Extracted from checkpoint_store.ml for reuse. *)
val io_error_of_exn : op:string -> path:string -> exn -> (_, Error.sdk_error) result
