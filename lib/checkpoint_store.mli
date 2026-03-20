(** File-backed checkpoint persistence using [Eio.Path].

    Layout: [<base_dir>/<session_id>.json].
    Atomic writes via [.tmp] + rename. *)

(** {1 Types} *)

type t

(** {1 Lifecycle} *)

(** Create the store, ensuring [base_dir] exists. *)
val create : Eio.Fs.dir_ty Eio.Path.t -> (t, Error.sdk_error) result

(** {1 CRUD} *)

val save : t -> Checkpoint.t -> (unit, Error.sdk_error) result
val load : t -> string -> (Checkpoint.t, Error.sdk_error) result
val delete : t -> string -> (unit, Error.sdk_error) result
val exists : t -> string -> bool

(** {1 Query} *)

(** List all session IDs with saved checkpoints, sorted alphabetically. *)
val list : t -> (string list, Error.sdk_error) result

(** Load the checkpoint with the most recent [created_at] timestamp. *)
val latest : t -> (Checkpoint.t, Error.sdk_error) result
