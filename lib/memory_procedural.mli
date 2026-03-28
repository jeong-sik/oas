(** Procedural memory: learned action patterns with success/failure tracking.

    @since 0.75.0
    @since 0.92.0 extracted from Memory

    @stability Evolving
    @since 0.93.0 *)

type procedure = {
  id: string;
  pattern: string;
  action: string;
  success_count: int;
  failure_count: int;
  confidence: float;
  last_used: float;
  metadata: (string * Yojson.Safe.t) list;
}

val compute_confidence : success_count:int -> failure_count:int -> float
val procedure_to_json : procedure -> Yojson.Safe.t
val procedure_of_json : Yojson.Safe.t -> procedure option
val string_contains : needle:string -> string -> bool

val store : Context.t -> procedure -> unit
val all : Context.t -> procedure list

val matching :
  Context.t -> pattern:string -> ?min_confidence:float ->
  ?filter:(procedure -> bool) -> unit -> procedure list

val find :
  Context.t -> pattern:string -> ?min_confidence:float ->
  ?filter:(procedure -> bool) -> ?touch:bool -> unit -> procedure option

val best : Context.t -> pattern:string -> procedure option

val record_success : Context.t -> string -> unit
val record_failure : Context.t -> string -> unit
val forget : Context.t -> string -> unit
val count : Context.t -> int
