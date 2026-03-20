(** Runtime skill registry -- discover and manage skills at runtime.

    Wraps a hash table keyed by skill name.  Unlike {!Skill} which is
    pure data, the registry provides CRUD, bulk loading from a
    directory, and JSON round-trip for persistence / agent card export. *)

(** {1 Types} *)

type t

(** {1 CRUD} *)

val create : unit -> t
val register : t -> Skill.t -> unit
val find : t -> string -> Skill.t option
val remove : t -> string -> unit

(** {1 Query} *)

(** All registered skills, sorted by name. *)
val list : t -> Skill.t list

(** All registered skill names, sorted. *)
val names : t -> string list

val count : t -> int

(** {1 Bulk loading} *)

(** Load all skill files from [dir].  Returns the number loaded. *)
val load_from_dir :
  t -> ?scope:Skill.scope -> string ->
  (int, Error.sdk_error) result

(** {1 JSON round-trip} *)

val skill_to_json : Skill.t -> Yojson.Safe.t
val to_json : t -> Yojson.Safe.t
val skill_of_json : Yojson.Safe.t -> (Skill.t, Error.sdk_error) result
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
