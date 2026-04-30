(** Generic observation-driven collaboration contracts.

    This module names shared-state shapes that downstream coordinators can map
    onto CRDT documents, databases, event logs, or any other replicated store.
    OAS does not own distributed storage, task semantics, or coordinator
    policy here; it only exposes typed vocabulary for observation-driven
    coordination.

    @stability Evolving *)

type participant_id = string [@@deriving yojson, show]
type item_id = string [@@deriving yojson, show]
type state_key = string [@@deriving yojson, show]
type logical_clock = int [@@deriving yojson, show]

(** Principles expected from an observation-driven coordination substrate. *)
type principle =
  | Observable_updates
  | Deterministic_convergence
  | Monotonic_progress
[@@deriving yojson, show]

val principle_label : principle -> string

(** Generic shared-state roles. These are roles, not storage backends. *)
type shared_state_kind =
  | Claim_registry
  | Turn_queue
  | Blackboard
[@@deriving yojson, show]

val shared_state_kind_label : shared_state_kind -> string

(** A generic observation over shared state. [subject_id] is intentionally
    domain-neutral: consumers decide whether it names a CRDT key, queue ID,
    artifact, file range, or another downstream-owned object. *)
type observation =
  { observer_id : participant_id
  ; state_kind : shared_state_kind
  ; subject_id : string
  ; version : logical_clock
  ; observed_at : float option
  }
[@@deriving yojson, show]

(** {1 Claim protocol} *)

type claim_phase =
  | Open
  | Claimed
  | Closed
[@@deriving yojson, show]

type claim_snapshot =
  { item_id : item_id
  ; phase : claim_phase
  ; claimant : participant_id option
  ; logical_clock : logical_clock
  }
[@@deriving yojson, show]

type claim_verdict =
  | Claim_won
  | Claim_lost of participant_id
  | Claim_not_claimed
  | Claim_closed
[@@deriving yojson, show]

type merge_error =
  | Subject_mismatch of
      { left : string
      ; right : string
      }
[@@deriving yojson, show]

val open_claim : item_id -> claim_snapshot
val is_claimable : claim_snapshot -> bool

(** Optimistic write side of write-then-verify claim protocols.
    Non-open snapshots are returned unchanged. *)
val claim
  :  actor_id:participant_id
  -> logical_clock:logical_clock
  -> claim_snapshot
  -> claim_snapshot

(** Verify whether [actor_id] is the converged claimant. *)
val verify_claim : actor_id:participant_id -> claim_snapshot -> claim_verdict

(** Deterministic merge for two snapshots of the same item.
    [Closed] wins over non-closed phases to preserve monotonic progress.
    Otherwise the snapshot with the larger [(logical_clock, claimant)] wins. *)
val merge_claim_snapshot
  :  claim_snapshot
  -> claim_snapshot
  -> (claim_snapshot, merge_error) result

(** {1 Turn queue} *)

type turn_entry =
  { actor_id : participant_id
  ; ordinal : int
  ; priority : int option
  ; reason : string option
  }
[@@deriving yojson, show]

(** Deterministic queue ordering: higher priority first, then lower ordinal,
    then [actor_id] as a stable tie-breaker. *)
val compare_turn_entry : turn_entry -> turn_entry -> int

val normalize_turn_queue : turn_entry list -> turn_entry list
val next_turn : turn_entry list -> turn_entry option

(** {1 Shared state} *)

type state_entry =
  { key : state_key
  ; writer_id : participant_id
  ; value : Yojson.Safe.t
  ; logical_clock : logical_clock
  }
[@@deriving yojson, show]

(** Deterministic last-writer-wins merge for two entries with the same key.
    Ties are resolved by writer ID and value JSON, so merge order does not
    decide the winner. *)
val merge_state_entry : state_entry -> state_entry -> (state_entry, merge_error) result
