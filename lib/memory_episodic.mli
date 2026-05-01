(** Episodic memory: interaction history with time-decaying salience.

    @since 0.75.0
    @since 0.92.0 extracted from Memory

    @stability Evolving
    @since 0.93.1 *)

type outcome =
  | Success of string
  | Failure of string
  | Neutral

type episode =
  { id : string
  ; timestamp : float
  ; participants : string list
  ; action : string
  ; outcome : outcome
  ; salience : float
  ; metadata : (string * Yojson.Safe.t) list
  }

val outcome_to_json : outcome -> Yojson.Safe.t
val outcome_of_json : Yojson.Safe.t -> outcome
val episode_to_json : episode -> Yojson.Safe.t
val episode_of_json : Yojson.Safe.t -> episode option
val store : Context.t -> episode -> unit
val recall_one : Context.t -> string -> episode option
val all : Context.t -> episode list
val decayed_salience : now:float -> decay_rate:float -> episode -> float

val recall
  :  Context.t
  -> ?now:float
  -> ?decay_rate:float
  -> ?min_salience:float
  -> ?limit:int
  -> ?filter:(episode -> bool)
  -> unit
  -> episode list

val boost_salience : Context.t -> string -> float -> unit
val forget : Context.t -> string -> unit
val count : Context.t -> int
