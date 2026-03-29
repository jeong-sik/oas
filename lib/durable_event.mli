(** Event-sourced agent loop journal.

    Records every agent loop action as an immutable event.
    On crash recovery, replays the journal to reconstruct exact state
    without re-executing side-effectful activities.

    Complements {!Durable} (step-level pipelines) with fine-grained
    agent-loop-level event sourcing inspired by Temporal workflows.

    Key properties:
    - Events are append-only (immutable journal)
    - Side-effectful activities use idempotency keys to skip on replay
    - Heartbeat lease detects stale/crashed agents
    - Full journal is JSON-serializable for persistence

    @since 0.89.0

    @stability Evolving
    @since 0.93.1 *)

(** {1 Event types} *)

(** An agent loop event. Each variant captures the data needed
    to replay the action without re-executing it. *)
type event =
  | Turn_started of { turn: int; timestamp: float }
  | Llm_request of {
      turn: int;
      model: string;
      input_tokens: int;
      timestamp: float;
    }
  | Llm_response of {
      turn: int;
      output_tokens: int;
      stop_reason: string;
      duration_ms: float;
      timestamp: float;
    }
  | Tool_called of {
      turn: int;
      tool_name: string;
      idempotency_key: string;
      input_hash: string;
      timestamp: float;
    }
  | Tool_completed of {
      turn: int;
      tool_name: string;
      idempotency_key: string;
      output_json: Yojson.Safe.t;
      is_error: bool;
      duration_ms: float;
      timestamp: float;
    }
  | State_transition of {
      from_state: string;
      to_state: string;
      reason: string;
      timestamp: float;
    }
  | Heartbeat of {
      agent_id: string;
      timestamp: float;
      context_tokens: int;
    }
  | Checkpoint_saved of {
      checkpoint_id: string;
      timestamp: float;
    }
  | Error_occurred of {
      turn: int;
      error_domain: string;
      detail: string;
      timestamp: float;
    }

(** {1 Journal} *)

type journal

(** Create an empty journal. *)
val create : unit -> journal

(** Append an event to the journal. *)
val append : journal -> event -> unit

(** All events in chronological order. *)
val events : journal -> event list

(** Number of events in the journal. *)
val length : journal -> int

(** {1 Idempotency} *)

(** Generate an idempotency key from tool name + input hash.
    Deterministic: same inputs always produce the same key. *)
val make_idempotency_key : tool_name:string -> input:Yojson.Safe.t -> string

(** Check if an activity with this key has already completed.
    Returns [Some output_json] if found, [None] if not. *)
val find_completed_activity : journal -> string -> Yojson.Safe.t option

(** {1 Heartbeat lease} *)

type lease_status =
  | Active of { last_heartbeat: float; age_s: float }
  | Expired of { last_heartbeat: float; age_s: float }
  | No_heartbeat

(** Check lease status.
    [timeout_s] is the maximum age of the last heartbeat (default: 60s). *)
val check_lease : journal -> ?timeout_s:float -> agent_id:string -> unit -> lease_status

(** {1 Replay} *)

(** Replay summary: extracted from a journal for state reconstruction. *)
type replay_summary = {
  last_turn: int;
  completed_tools: (string * Yojson.Safe.t) list;  (** idempotency_key -> output *)
  last_state: string;
  total_input_tokens: int;
  total_output_tokens: int;
  error_count: int;
}

(** Extract a replay summary from a journal.
    Used to reconstruct agent state after crash recovery. *)
val replay_summary : journal -> replay_summary

(** {1 Serialization} *)

val event_to_json : event -> Yojson.Safe.t
val event_of_json : Yojson.Safe.t -> (event, string) result
val journal_to_json : journal -> Yojson.Safe.t
val journal_of_json : Yojson.Safe.t -> (journal, string) result

(** {1 Queries} *)

(** Events for a specific turn. *)
val events_for_turn : journal -> int -> event list

(** Last event timestamp, or [None] for empty journals. *)
val last_timestamp : journal -> float option

(** All tool completions (for replay). *)
val tool_completions : journal -> (string * Yojson.Safe.t * bool) list
