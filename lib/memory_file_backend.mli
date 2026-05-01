open Base
(** File-system backed long-term memory persistence.

    Stores each key-value pair as a JSON file under a base directory.
    Atomic writes via temporary file + rename.
    Uses Eio.Path for structured concurrency compatibility.

    Layout: [<base_dir>/<hex_key>.json]
    where [hex_key] is a hex-encoded key for filesystem safety.

    Batch persist is best-effort: individual files are atomic but the
    batch as a whole is not transactional.
    Query by prefix is O(n) directory scan. For 1000+ entries, consider
    a database-backed backend.

    @stability Evolving
    @since 0.102.0 *)

type t

(** Create a file-backed memory store.
    Creates [base_dir] if it does not exist. *)
val create : Eio.Fs.dir_ty Eio.Path.t -> (t, Error.sdk_error) result

(** Get the {!Memory.long_term_backend} for use with {!Memory.create}. *)
val to_backend : t -> Memory.long_term_backend

(** Count stored entries. *)
val entry_count : t -> int

(** Remove all stored entries. *)
val clear : t -> (unit, Error.sdk_error) result

(** List all stored keys. *)
val keys : t -> string list
