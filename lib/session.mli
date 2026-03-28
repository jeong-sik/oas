(** Session lifecycle and metadata for agent executions.

    Tracks turn count, timestamps, and cross-turn state via
    {!Context.t}. Each session has a unique ID and optional
    resume lineage.

    @stability Evolving
    @since 0.93.0 *)

(** {1 Session type} *)

type t = {
  id: string;
  started_at: float;
  last_active_at: float;
  turn_count: int;
  resumed_from: string option;
  cwd: string option;
  metadata: Context.t;
}

(** {1 Lifecycle} *)

(** Generate a unique session ID. *)
val generate_id : unit -> string

(** Create a new session. *)
val create :
  ?id:string ->
  ?resumed_from:string ->
  ?cwd:string ->
  ?metadata:Context.t ->
  unit ->
  t

(** Record a completed turn: increments turn_count, updates last_active_at. *)
val record_turn : t -> t

(** Update last_active_at without incrementing turn_count. *)
val touch : t -> t

(** Elapsed seconds since session start. *)
val elapsed : t -> float

(** Resume a session from a checkpoint. *)
val resume_from : Checkpoint.t -> t

(** {1 Serialization} *)

(** Serialize session to JSON. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize session from JSON. *)
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
