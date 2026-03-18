(** File-backed A2A task persistence.

    Layout: [<base_dir>/<task_id>.json].
    Atomic writes via .tmp + rename. In-memory Hashtbl cache for fast lookups. *)

type t

val create : Eio.Fs.dir_ty Eio.Path.t -> (t, Error.sdk_error) result

val store_task : t -> A2a_task.task -> (unit, Error.sdk_error) result
val get_task : t -> A2a_task.task_id -> A2a_task.task option
val list_tasks : t -> A2a_task.task list
val delete_task : t -> A2a_task.task_id -> (unit, Error.sdk_error) result

val reload : t -> (unit, Error.sdk_error) result
val gc : ?max_age_s:float -> t -> (int, Error.sdk_error) result
