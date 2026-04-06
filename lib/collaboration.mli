(** Collaboration context for multi-agent coordination.

    Separates shared collaboration concerns from individual {!Session.t}
    (personal runtime tracking) and orchestration strategy.

    Design: immutable record updates; {!shared_context} is the only
    mutable component (it is a {!Context.t} reference, documented).

    @stability Evolving
    @since 0.93.1 *)

(** {1 Phase lifecycle} *)

type phase =
  | Bootstrapping
  | Active
  | Waiting_on_participants
  | Finalizing
  | Completed
  | Failed
  | Cancelled
[@@deriving yojson, show]

(** {1 Participant tracking} *)

type participant_state =
  | Planned
  | Joined
  | Working
  | Done
  | Failed_participant
  | Left
[@@deriving yojson, show]

type participant = {
  name: string;
  role: string option;
  state: participant_state;
  joined_at: float option;
  finished_at: float option;
  summary: string option;
}
[@@deriving yojson, show]

(** {1 Artifacts and contributions} *)

type artifact = {
  id: string;
  name: string;
  kind: string;
  producer: string;
  created_at: float;
}
[@@deriving yojson, show]

(** Generic contribution record.

    Replaces the domain-specific [vote] type.  Agent state (participants,
    phase, contributions) belongs in OAS; domain-specific semantics
    (governance votes, rulings) belong to downstream consumers.

    The [kind] field distinguishes contribution types:
    - ["vote"]   — a governance decision
    - ["review"] — a code/artifact review
    - ["idea"]   — a suggestion or proposal
    - any other consumer-defined string *)
type contribution = {
  agent: string;
  kind: string;
  content: string;
  created_at: float;
}
[@@deriving yojson, show]

(** {1 Collaboration record}

    12 fields, kept under 15 by design.
    [shared_context] is a mutable {!Context.t} reference shared
    across all record snapshots produced by update functions. *)

type t = {
  id: string;
  goal: string;
  phase: phase;
  participants: participant list;
  artifacts: artifact list;
  contributions: contribution list;
  shared_context: Context.t;
  created_at: float;
  updated_at: float;
  outcome: string option;
  max_participants: int option;
  metadata: (string * Yojson.Safe.t) list;
}

(** {1 Construction} *)

val create : ?id:string -> ?shared_context:Context.t -> goal:string -> unit -> t

(** {1 Participant operations}

    Participant names are expected to be unique.  The module does not
    enforce uniqueness; callers are responsible.  If duplicates exist:
    - [find_participant] returns the first match.
    - [update_participant] updates {e all} matches.
    - [remove_participant] removes {e all} matches. *)

val add_participant : t -> participant -> t
val update_participant : t -> string -> (participant -> participant) -> t
val remove_participant : t -> string -> t
val find_participant : t -> string -> participant option
val active_participants : t -> participant list

(** {1 Artifact and contribution operations} *)

val add_artifact : t -> artifact -> t
val add_contribution : t -> contribution -> t

(** {1 Phase and outcome} *)

val set_phase : t -> phase -> t
val set_outcome : t -> string -> t
val is_terminal : t -> bool

(** {1 Phase transition guards} *)

type phase_transition_error =
  | InvalidPhaseTransition of { from_phase: phase; to_phase: phase }
  | PhaseAlreadyTerminal of { phase: phase }

val valid_phase_transitions : phase -> phase list
val transition_phase : t -> phase -> (t, phase_transition_error) result
val phase_transition_error_to_string : phase_transition_error -> string

(** {1 Timestamp} *)

val touch : t -> t

(** {1 Serialization}

    [shared_context] is serialized via {!Context.to_json} / {!Context.of_json}.
    Note: [Context.of_json] silently returns an empty context if the JSON
    value is not an object — this is a known limitation of [Context], not
    specific to [Collaboration]. *)

val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
