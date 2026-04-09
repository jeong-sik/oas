(** Structured Logging -- level-based structured log system.

    Provides typed log records with key-value fields, composable sinks,
    and zero-cost filtering for disabled levels.

    - [sink] is [record -> unit] -- composable and lightweight.
    - [field] is a closed variant for schema enforcement at call sites.
    - Disabled levels skip record allocation entirely.

    @stability Internal
    @since 0.93.1 *)

(** {2 Level} *)

type level =
  | Debug
  | Info
  | Warn
  | Error

val level_to_string : level -> string
val level_of_string : string -> (level, string) result
val level_to_yojson : level -> Yojson.Safe.t
val level_of_yojson : Yojson.Safe.t -> (level, string) result
val pp_level : Format.formatter -> level -> unit
val show_level : level -> string

(** {2 Field} *)

type field =
  | S of string * string
  | I of string * int
  | F of string * float
  | B of string * bool
  | J of string * Yojson.Safe.t

val field_to_json : field -> string * Yojson.Safe.t

(** {2 Record} *)

type record = {
  ts: float;
  level: level;
  module_name: string;
  message: string;
  fields: field list;
  trace_id: string option;
  span_id: string option;
}

val record_to_json : record -> Yojson.Safe.t

(** {2 Sink} *)

type sink = record -> unit

(** {2 Global configuration}

    Underlying globals use [Atomic.t]. Concurrent sink registration is
    linearized with a CAS loop so [add_sink] does not lose updates
    across domains. [clear_sinks] publishes an empty sink set with a
    single atomic store, so a race between [add_sink] and [clear_sinks]
    resolves to whichever operation linearizes last. *)

val set_global_level : level -> unit
val add_sink : sink -> unit
val clear_sinks : unit -> unit

(** {2 Logger instance} *)

type t

val create : module_name:string -> unit -> t
val with_trace_id : t -> trace_id:string -> t
val with_span_id : t -> span_id:string -> t

(** {2 Logging} *)

val emit : t -> level -> string -> field list -> unit
val debug : t -> string -> field list -> unit
val info : t -> string -> field list -> unit
val warn : t -> string -> field list -> unit
val error : t -> string -> field list -> unit

(** {2 Built-in sinks} *)

val json_sink : _ Eio.Flow.sink -> sink
val stderr_sink : unit -> sink
val collector_sink : unit -> sink * (unit -> record list)
