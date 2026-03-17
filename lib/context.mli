(** Cross-turn shared state container.
    Values are [Yojson.Safe.t] for flexibility while maintaining
    serializability. *)

type t
type scope =
  | App
  | User
  | Session
  | Temp
  | Custom of string

type diff = {
  added: (string * Yojson.Safe.t) list;
  removed: string list;
  changed: (string * Yojson.Safe.t) list;
}

val create : unit -> t
val get : t -> string -> Yojson.Safe.t option
val set : t -> string -> Yojson.Safe.t -> unit
val delete : t -> string -> unit
val keys : t -> string list
val snapshot : t -> (string * Yojson.Safe.t) list
val scoped_key : scope -> string -> string
val get_scoped : t -> scope -> string -> Yojson.Safe.t option
val set_scoped : t -> scope -> string -> Yojson.Safe.t -> unit
val delete_scoped : t -> scope -> string -> unit
val keys_in_scope : t -> scope -> string list
val merge : t -> (string * Yojson.Safe.t) list -> unit
val diff : t -> t -> diff
val to_json : t -> Yojson.Safe.t

(** Deserialize from JSON.  Returns an empty context if [json] is not
    a JSON object (i.e. not [`Assoc _]). *)
val of_json : Yojson.Safe.t -> t

(** Shallow-copy all entries into a fresh context.
    Values are [Yojson.Safe.t] (structurally immutable), so shallow copy
    is sufficient for full independence. *)
val copy : t -> t

(** Isolated scope for sub-agent delegation.
    Only specified keys propagate between parent and child contexts. *)
type isolated_scope = {
  parent: t;
  local: t;
  propagate_up: string list;
  propagate_down: string list;
}

val create_scope :
  parent:t -> propagate_down:string list -> propagate_up:string list ->
  isolated_scope

val merge_back : isolated_scope -> unit

(** {2 User data convenience API} *)

val set_user_data : t -> string -> Yojson.Safe.t -> unit
val get_user_data : t -> string -> Yojson.Safe.t option
val delete_user_data : t -> string -> unit

(** All key-value pairs in the [User] scope (keys without prefix). *)
val all_user_data : t -> (string * Yojson.Safe.t) list
