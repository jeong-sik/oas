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
  caused_by: string option;
    (** ID of the event that causally triggered this one. Distinct from
        [correlation_id] (same-session scoping): [caused_by] points at a
        specific prior [run_id] or [correlation_id] to reconstruct
        A→B→C cascades within a session. [None] means "no known parent
        event" (root of a causation chain). Convention:
        [caused_by = Some parent.run_id] when the trigger is a concrete
        event; [caused_by = Some parent.correlation_id] when the
        trigger is the session as a whole. Anthropic Multi-Agent
        Pattern 4 (Message Bus). @since 0.161.0 (#877) *)
}

(** {2 Payload types} *)

type slot_scheduler_state =
  | Idle
  | Queued
  | Saturated

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
  | ContentReplacementReplaced of {
      tool_use_id: string;
      preview: string;
      original_chars: int;
      seen_count_after: int;
    }
      (** Content replacement state froze a tool result by replacing the
          original output with a preview. Promoted from
          [Custom("content_replacement_frozen", ...)].
          @since 0.154.1 *)
  | ContentReplacementKept of { tool_use_id: string; seen_count_after: int }
      (** Content replacement state froze a tool result without replacement.
          Promoted from [Custom("content_replacement_frozen", ...)].
          @since 0.154.1 *)
  | SlotSchedulerObserved of {
      max_slots: int;
      active: int;
      available: int;
      queue_length: int;
      state: slot_scheduler_state;
    }
      (** Snapshot of the slot scheduler queue state. Promoted from
          [Custom("slot_scheduler_queue", ...)].
          @since 0.154.1 *)
  | Custom of string * Yojson.Safe.t
      (** Extension point.  [name] must be a dot-separated, lowercase,
          snake-case namespaced identifier (e.g. ["mylib.foo_happened"]).

          Reserved prefixes (OAS-internal):
          - [runtime.*] — bridged from [Runtime] session protocol.
          - [durable.*] — bridged from [Durable_event] journal.
          - [provider.*] — provider-specific escape hatch
            (e.g. [provider.anthropic.cache_hit],
            [provider.openai.reasoning_tokens]).
          - [oas.*] — reserved for future OAS use.

          External publishers should use their own [Event_bus.t] instance
          for domain-specific events rather than publishing onto OAS's
          bus.  Treating OAS's [Event_bus] as a general-purpose telemetry
          channel creates cross-layer coupling and makes OAS's public
          surface a dumping ground.  See [docs/EVENT-CATALOG.md] §2.3. *)

(** {2 Event} *)

(** An event is an envelope plus a payload. *)
type event = {
  meta: envelope;
  payload: payload;
}

(** {2 ID generation} *)

(** Generate a fresh unique identifier (pid-timestamp-counter). *)
val fresh_id : unit -> string

(** Create an envelope with optional correlation/run IDs (defaults to fresh)
    and an optional causation link.
    @since 0.161.0 [?caused_by] added. *)
val mk_envelope :
  ?correlation_id:string ->
  ?run_id:string ->
  ?caused_by:string ->
  unit ->
  envelope

(** Create an event by wrapping a payload in a fresh envelope.
    @since 0.161.0 [?caused_by] added. *)
val mk_event :
  ?correlation_id:string ->
  ?run_id:string ->
  ?caused_by:string ->
  payload ->
  event

(** {2 Bus} *)

type t

(** Backpressure policy applied when a subscriber's stream is full.

    - [Block] — [publish] blocks until the subscriber drains (current
      semantics; default). A single slow subscriber stalls every
      publisher sharing the bus.
    - [Drop_oldest] — evict the oldest queued event for the offending
      subscriber, enqueue the new one. Keeps [publish] non-blocking at
      the cost of losing the queue head. Increments [dropped_total].
    - [Drop_newest] — drop the event being published for that
      subscriber and leave the queue intact. Non-blocking; newest
      event is lost. Increments [dropped_total].

    The policy is bus-wide and affects every subscriber. It is set at
    {!create} time and cannot be changed afterwards. Publishers
    observe different behaviours via [Drop_*] only indirectly, through
    {!stats}: [Drop_*] never blocks, [Block] may accumulate time in
    [total_publish_blocked_seconds].

    @since 0.160.0 *)
type backpressure_policy =
  | Block
  | Drop_oldest
  | Drop_newest

(** Create a bus.

    - [?buffer_size] — per-subscriber stream capacity (default 256).
    - [?policy] — backpressure policy (default [Block] for backward
      compatibility).

    @since 0.160.0 [?policy] parameter added. *)
val create : ?buffer_size:int -> ?policy:backpressure_policy -> unit -> t

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

(** Subscribe to the bus.

    - [?filter] — event predicate (default: accept all).
    - [?purpose] — free-form label surfaced in {!subscription_stats}
      (e.g. ["sse_bridge"], ["keeper_turn"], ["eval_collector"]).
      Does not affect routing.

    @since 0.160.0 [?purpose] parameter added. *)
val subscribe : ?filter:filter -> ?purpose:string -> t -> subscription

val unsubscribe : t -> subscription -> unit

(** {2 Publish and drain} *)

val publish : t -> event -> unit
val drain : subscription -> event list

(** {2 Queries} *)

val subscriber_count : t -> int

(** Per-subscriber runtime statistics. Counters are monotonic; they
    reset only when the subscription is dropped.

    - [purpose] — label passed to {!subscribe} (if any).
    - [depth] — events currently queued in the subscriber's stream.
    - [published_total] — events that passed the filter and were
      offered to this subscriber (before any drop).
    - [drained_total] — events removed via {!drain}.
    - [dropped_total] — events discarded under [Drop_oldest] or
      [Drop_newest]. Always 0 under [Block].

    @since 0.160.0 *)
type subscription_stats = {
  purpose: string option;
  depth: int;
  published_total: int;
  drained_total: int;
  dropped_total: int;
}

(** Bus-wide runtime statistics.

    - [subscriber_count] — current subscriber count.
    - [subscriptions] — per-subscriber stats in subscription order
      (newest first, matching internal list order).
    - [total_publish_blocked_seconds] — wall-clock seconds that
      [publish] spent waiting on full subscriber streams (only
      accumulates under [Block]; always 0 for [Drop_*]).

    @since 0.160.0 *)
type bus_stats = {
  subscriber_count: int;
  subscriptions: subscription_stats list;
  total_publish_blocked_seconds: float;
}

(** Snapshot of bus-wide and per-subscriber runtime statistics.
    @since 0.160.0 *)
val stats : t -> bus_stats
