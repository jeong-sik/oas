(** Working memory: 5-tier facade over {!Context.t}.

    - {b Scratchpad} — per-turn ephemeral data (cleared between turns).
    - {b Working} — cross-turn data (survives within session).
    - {b Episodic} — interaction history with timestamps and salience decay.
    - {b Procedural} — learned action patterns with success/failure tracking.
    - {b Long_term} — external persistence via callback.

    Episodic and Procedural tiers provide structured record storage
    beyond the generic key-value store of Scratchpad/Working/Long_term.

    @since 0.65.0 (3-tier)
    @since 0.75.0 (5-tier: Episodic + Procedural) *)

(** {1 Tiers} *)

type tier =
  | Scratchpad
  | Working
  | Episodic
  | Procedural
  | Long_term

(** {1 Long-term backend} *)

(** Callback for long-term memory persistence.

    [persist] and [remove] return [Ok ()] on success or [Error reason].
    [batch_persist] atomically stores multiple key-value pairs.
    [query] returns entries whose keys start with [prefix], up to [limit]. *)
type long_term_backend = {
  persist: key:string -> Yojson.Safe.t -> (unit, string) result;
  retrieve: key:string -> Yojson.Safe.t option;
  remove: key:string -> (unit, string) result;
  batch_persist: (string * Yojson.Safe.t) list -> (unit, string) result;
  query: prefix:string -> limit:int -> (string * Yojson.Safe.t) list;
}

(** Wrap legacy callbacks that return [unit] into a {!long_term_backend}
    where [persist]/[remove] always return [Ok ()],
    [batch_persist] iterates, and [query] returns [[]]. *)
val legacy_backend :
  persist:(key:string -> Yojson.Safe.t -> unit) ->
  retrieve:(key:string -> Yojson.Safe.t option) ->
  remove:(key:string -> unit) ->
  long_term_backend

(** {1 Abstract type} *)

type t

val create : ?ctx:Context.t -> ?long_term:long_term_backend -> unit -> t

(** Set or replace the long-term backend. *)
val set_long_term_backend : t -> long_term_backend -> unit

(** {1 Generic key-value operations (Scratchpad, Working, Long_term)} *)

(** Store a value at the given tier.  Long_term also invokes the backend.
    For Episodic/Procedural tiers, prefer {!store_episode}/{!store_procedure}. *)
val store : t -> tier:tier -> string -> Yojson.Safe.t -> unit

(** Recall a value.  Falls back through lower tiers:
    Scratchpad -> Working -> Long_term.
    Episodic and Procedural do not participate in fallback. *)
val recall : t -> tier:tier -> string -> Yojson.Safe.t option

(** Recall from a specific tier only, no fallback. *)
val recall_exact : t -> tier:tier -> string -> Yojson.Safe.t option

(** Remove a key from the given tier. *)
val forget : t -> tier:tier -> string -> unit

(** Promote a key from Scratchpad to Working.
    Returns [true] if the key existed in Scratchpad. *)
val promote : t -> string -> bool

(** All entries in the Working tier. *)
val working_entries : t -> (string * Yojson.Safe.t) list

(** All entries in the Scratchpad tier. *)
val scratchpad_entries : t -> (string * Yojson.Safe.t) list

(** Clear all Scratchpad entries (call between turns). *)
val clear_scratchpad : t -> unit

(** Count (scratchpad, working, episodic, procedural, long_term) entries. *)
val stats : t -> int * int * int * int * int

(** Access the underlying context. *)
val context : t -> Context.t

(** {1 Episodic memory}

    Stores interaction history with time-decaying salience.
    Use for "what happened" recall — past interactions,
    outcomes, participant involvement. *)

(** Outcome of an interaction. *)
type outcome =
  | Success of string
  | Failure of string
  | Neutral

(** A single episodic memory record. *)
type episode = {
  id: string;
  timestamp: float;
  participants: string list;
  action: string;
  outcome: outcome;
  salience: float;     (** 0.0–1.0, decays over time *)
  metadata: (string * Yojson.Safe.t) list;
}

(** Store an episode.  The episode's [id] is used as the storage key. *)
val store_episode : t -> episode -> unit

(** Recall episodes by salience (highest first), applying time decay.
    [decay_rate] controls exponential decay: [salience * exp(-rate * age)].
    Default decay_rate is [0.01] (slow decay).
    [min_salience] filters out episodes below threshold (default [0.1]).
    [filter] is applied after decay and before sorting/limit. *)
val recall_episodes :
  t -> ?now:float -> ?decay_rate:float -> ?min_salience:float ->
  ?limit:int -> ?filter:(episode -> bool) -> unit -> episode list

(** Recall a single episode by ID. *)
val recall_episode : t -> string -> episode option

(** Boost an episode's salience (e.g., on recall or relevance). *)
val boost_salience : t -> string -> float -> unit

(** Remove an episode. *)
val forget_episode : t -> string -> unit

(** Count episodes. *)
val episode_count : t -> int

(** {1 Procedural memory}

    Stores learned action patterns with success/failure tracking.
    Use for "how to do X" recall — reusable strategies,
    tool usage patterns, decision heuristics. *)

(** A learned procedure. *)
type procedure = {
  id: string;
  pattern: string;          (** What triggers this procedure *)
  action: string;           (** What to do *)
  success_count: int;
  failure_count: int;
  confidence: float;        (** success / (success + failure), 0.0–1.0 *)
  last_used: float;
  metadata: (string * Yojson.Safe.t) list;
}

(** Store or update a procedure. Uses [id] as storage key. *)
val store_procedure : t -> procedure -> unit

(** Find the best procedure matching a pattern substring.
    Returns the highest-confidence match. *)
val best_procedure : t -> pattern:string -> procedure option

(** Extended procedure lookup with optional confidence/filter criteria.
    [touch] updates the chosen procedure's [last_used] timestamp. *)
val find_procedure :
  t -> pattern:string -> ?min_confidence:float ->
  ?filter:(procedure -> bool) -> ?touch:bool -> unit -> procedure option

(** All procedures matching a pattern substring, sorted by confidence. *)
val matching_procedures :
  t -> pattern:string -> ?min_confidence:float ->
  ?filter:(procedure -> bool) -> unit -> procedure list

(** Record a success for a procedure (increments count, updates confidence). *)
val record_success : t -> string -> unit

(** Record a failure for a procedure (increments count, updates confidence). *)
val record_failure : t -> string -> unit

(** Remove a procedure. *)
val forget_procedure : t -> string -> unit

(** Count procedures. *)
val procedure_count : t -> int
