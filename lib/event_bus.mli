(** Agent Event Bus -- typed publish/subscribe for agent lifecycle events.

    Each subscriber gets its own bounded {!Eio.Stream.t}; [publish]
    copies each event to every matching subscriber. All state is
    internal to [t] -- no globals.

    @stability Evolving
    @since 0.93.1
    @since 0.123.0 Envelope-based event structure *)

(** {2 Envelope} *)

(** Correlation metadata attached to every event. *)
type envelope = {
  correlation_id: string;  (** Session-level correlation (formerly session_id). *)
  run_id: string;          (** Per-run identifier (formerly worker_run_id). *)
  ts: float;               (** Event timestamp (Unix epoch). *)
}

(** {2 Payload types} *)

type payload =
  | AgentStarted of { agent_name: string; task_id: string }
  | AgentCompleted of { agent_name: string; task_id: string;
                        result: (Types.api_response, Error.sdk_error) result; elapsed: float }
  | AgentFailed of { agent_name: string; task_id: string;
                     error: Error.sdk_error; elapsed: float }
      (** Explicit failure variant companion to [AgentCompleted].
          Emitted when an orchestrator task ends with [Error _].
          Subscribers that only care about failures can filter on this
          variant directly instead of matching [AgentCompleted] with a
          [Result.is_error] check.
          @since 0.154.0 *)
  | ToolCalled of { agent_name: string; tool_name: string; input: Yojson.Safe.t }
  | ToolCompleted of { agent_name: string; tool_name: string;
                       output: Types.tool_result }
  | TurnStarted of { agent_name: string; turn: int }
  | TurnCompleted of { agent_name: string; turn: int }
  | HandoffRequested of { from_agent: string; to_agent: string; reason: string }
      (** Agent-to-agent handoff has been requested. Emitted when an
          agent delegates control to another agent (e.g. via a handoff
          tool). Mirrors OpenAI Agents SDK [handoff_requested].
          @since 0.154.0 *)
  | HandoffCompleted of { from_agent: string; to_agent: string; elapsed: float }
      (** Handoff target finished its run. Mirrors OpenAI Agents SDK
          [handoff_occurred].
          @since 0.154.0 *)
  | ElicitationCompleted of { agent_name: string; question: string;
                              response: Hooks.elicitation_response }
  | ContextCompacted of { agent_name: string; before_tokens: int;
                          after_tokens: int; phase: string }
  | ContextOverflowImminent of { agent_name: string;
                                  estimated_tokens: int; limit_tokens: int;
                                  ratio: float }
      (** Proactive warning: next turn is projected to exceed context budget.
          Emitted before compaction is attempted.
          @since 0.136.0 *)
  | ContextCompactStarted of { agent_name: string; trigger: string }
      (** Compaction has begun (before [ContextCompacted] which signals completion).
          [trigger] is one of ["proactive"], ["emergency"], ["operator"].
          @since 0.136.0 *)
  | Custom of string * Yojson.Safe.t

(** {2 Event} *)

(** An event is an envelope plus a payload. *)
type event = {
  meta: envelope;
  payload: payload;
}

(** {2 ID generation} *)

(** Generate a fresh unique identifier (pid-timestamp-counter). *)
val fresh_id : unit -> string

(** Create an envelope with optional correlation/run IDs (defaults to fresh). *)
val mk_envelope : ?correlation_id:string -> ?run_id:string -> unit -> envelope

(** Create an event by wrapping a payload in a fresh envelope. *)
val mk_event : ?correlation_id:string -> ?run_id:string -> payload -> event

(** {2 Bus} *)

type t

val create : ?buffer_size:int -> unit -> t

(** {2 Filters} *)

type filter = event -> bool

val accept_all : filter
val filter_agent : string -> filter
val filter_tools_only : filter
val filter_topic : string -> filter
val filter_correlation : string -> filter
val filter_run : string -> filter
val filter_any : filter list -> filter
val filter_all : filter list -> filter

(** {2 Subscription} *)

type subscription

val subscribe : ?filter:filter -> t -> subscription
val unsubscribe : t -> subscription -> unit

(** {2 Publish and drain} *)

val publish : t -> event -> unit
val drain : subscription -> event list

(** {2 Queries} *)

val subscriber_count : t -> int
