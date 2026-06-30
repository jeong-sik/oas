(** Context reducer: message windowing strategies.

    Reduces message lists before API calls while preserving the full
    history in agent state. All strategies respect turn boundaries
    so ToolUse/ToolResult pairs are never split.

    Token estimation is CJK-aware: ~4 chars/token for ASCII,
    ~2/3 token per character for multi-byte (CJK, emoji).

    @stability Stable
    @since 0.93.1 *)

open Types

(** {1 Strategy types} *)

(** Per-reduction memoization table for token estimates. Passed to
    [Dynamic] selectors so they can avoid re-estimating messages that
    will be estimated again by the selected strategy. *)
type estimate_cache

(** Create a fresh estimate cache for a single reduction. *)
val create_estimate_cache : unit -> estimate_cache

(** Windowing strategy for context reduction. *)
type strategy =
  | Keep_last_n of int
  | Token_budget of int
  | Prune_tool_outputs of { max_output_len : int }
  | Prune_tool_args of
      { max_arg_len : int
      ; keep_recent : int
      }
  | Repair_dangling_tool_calls
  | Repair_orphaned_tool_results
  | Merge_contiguous
  | Drop_thinking
  | Keep_first_and_last of
      { first_n : int
      ; last_n : int
      }
  | Prune_by_role of { drop_roles : role list }
  | Summarize_old of
      { keep_recent : int
      ; summarizer : message list -> string
      }
  | Clear_tool_results of { keep_recent : int }
  | Stub_tool_results of { keep_recent : int }
  | Cap_message_tokens of
      { max_tokens : int
      ; keep_recent : int
      }
  | Cache_alignment of { size : int }
  | Relocate_tool_results of
      { state : Content_replacement_state.t
      ; keep_recent : int
      }
  | Compose of strategy list
  | Custom of (message list -> message list)
  | Dynamic of (cache:estimate_cache -> turn:int -> messages:message list -> strategy)

(** A configured reducer wrapping a strategy. *)
type t = { strategy : strategy }

(** Score a message for importance-aware filtering.
    [index] is the zero-based position in the original list and [total]
    is the list length. Return a score in [0.0, 1.0]; out-of-range values
    are clamped. *)
type importance_scorer = index:int -> total:int -> message -> float

(** Optionally raise a message to a minimum importance score.
    Return [Some score] to boost a message, or [None] to leave the
    base score unchanged. Out-of-range values are clamped. *)
type importance_boost = message -> float option

(** {1 Token estimation} *)

(** CJK-aware character-level token estimation.
    ASCII: ~4 chars/token. Multi-byte (CJK, emoji): ~2/3 token/char.
    Returns at least 1. *)
val estimate_char_tokens : string -> int

(** Estimate tokens for a single content block. [cache] memoizes repeated
    estimates within one reduction. *)
val estimate_block_tokens : ?cache:estimate_cache -> content_block -> int

(** Estimate tokens for a message. [cache] memoizes repeated estimates
    within one reduction. *)
val estimate_message_tokens : ?cache:estimate_cache -> message -> int

(** {1 Overhead estimation} *)

(** Estimate the fixed-overhead tokens for the next turn: system prompt,
    tool descriptions, and output reserve.  This lets the caller project
    whether adding one more turn will exceed the context budget without
    actually building the prompt.

    @param system_prompt  System prompt text (if any).
    @param tools          Tool JSON descriptions sent to the provider.
    @param output_reserve Tokens reserved for model output (default: 4096).
    @return Estimated overhead in tokens.
    @since 0.136.0 *)
val estimate_next_turn_overhead
  :  ?system_prompt:string
  -> ?tools:Yojson.Safe.t list
  -> ?output_reserve:int
  -> unit
  -> int

(** {1 Turn grouping} *)

(** Group messages into turns.

    A turn starts with a User message and includes all following
    messages until the next User message. User messages containing
    ToolResult blocks belong to the preceding turn. *)
val group_into_turns : message list -> message list list

(** {1 Core reducer} *)

(** Reduce messages according to the configured strategy. *)
val reduce : t -> message list -> message list

type dangling_repair_report =
  { synthesized_tool_results : int
    (** Number of explicit synthetic ToolResult messages inserted for
        assistant ToolUse blocks that had no adjacent result span. *)
  }

(** Apply the same repair as {!repair_dangling_tool_calls}, returning
    counters so callers can log or meter synthetic ToolResult insertion.
    The inserted ToolResult messages carry [metadata] marking them as
    synthetic. *)
val repair_dangling_tool_calls_with_report
  :  message list
  -> message list * dangling_repair_report

(** {1 Convenience constructors} *)

val keep_last : int -> t
val token_budget : int -> t
val prune_tool_outputs : max_output_len:int -> t
val prune_tool_args : max_arg_len:int -> ?keep_recent:int -> unit -> t
val repair_dangling_tool_calls : t

(** Remove ToolResult blocks whose tool_use_id has no matching ToolUse.
    OpenAI-compatible APIs (Glm, Groq, etc.) reject orphaned ToolResults.
    Complement of [repair_dangling_tool_calls].
    @since 0.99.2 *)
val repair_orphaned_tool_results : t

val merge_contiguous : t
val drop_thinking : t
val keep_first_and_last : first_n:int -> last_n:int -> t
val prune_by_role : drop_roles:role list -> t
val summarize_old : keep_recent:int -> summarizer:(message list -> string) -> t
val clear_tool_results : keep_recent:int -> t

(** Replace tool result content in older turns with a structured stub
    that preserves tool name, line count, and error status.
    More informative than [clear_tool_results]: the stub format is
    [[tool: <name>, <N> lines, <ok|error>]].
    ToolUse/ToolResult pairing is preserved.
    @since 0.98.0 *)
val stub_tool_results : keep_recent:int -> t

(** Cap per-message token count by truncating oversized messages.
    Messages in the most recent [keep_recent] turns are not modified.
    For older messages exceeding [max_tokens], content blocks are kept
    from the front (60% budget) and back (30% budget), dropping the
    middle. A truncation marker is inserted at the splice point.
    ToolUse/ToolResult blocks at the boundaries are preserved intact
    to maintain API pairing invariants.
    @since 0.125.0 *)
val cap_message_tokens : max_tokens:int -> keep_recent:int -> t

(** Pad or slice messages to perfectly hit prompt cache alignment boundaries.
    @since 0.190.0 *)
val align_to_cache : size:int -> t

(** Re-apply frozen replacement decisions from
    {!Content_replacement_state} to tool result content in older turns.
    ToolResult blocks with a cached replacement get their content
    swapped to the preview.  Blocks that were "kept" or are fresh
    pass through unchanged.  Useful after checkpoint restore when
    messages may have been loaded with full content.
    @since 0.129.0 *)
val relocate_tool_results : state:Content_replacement_state.t -> keep_recent:int -> t

val compose : t list -> t
val custom : (message list -> message list) -> t

(** Return an equivalent reducer with implicit reasoning-erasing steps removed.
    This drops [Drop_thinking] and [Summarize_old] because the latter rewrites
    older turns into a text summary and cannot structurally preserve reasoning
    blocks. This is used when the agent/provider config explicitly requests
    historical thinking preservation; callers keep the same repair/windowing
    policy without silently deleting reasoning blocks from retained messages. *)
val preserve_thinking : t -> t

val importance_scored
  :  ?threshold:float
  -> ?boost:importance_boost
  -> scorer:importance_scorer
  -> unit
  -> t

(** Dynamic strategy: selects a strategy per turn based on
    conversation state. [cache] is the same estimate cache that will be
    passed to the selected strategy. *)
val dynamic : (cache:estimate_cache -> turn:int -> messages:message list -> strategy) -> t

(** {1 Capabilities integration} *)

(** Create a reducer from provider capabilities.
    Uses [max_context_tokens * margin] as the token budget (default 80%),
    composed with [drop_thinking], [repair_dangling_tool_calls],
    and [repair_orphaned_tool_results].
    Returns [None] if [max_context_tokens] is unknown. *)
val from_capabilities
  :  ?margin:float
  -> Llm_provider.Capabilities.capabilities
  -> t option

(** Create a reducer from an explicit context budget with configurable thresholds.
    Uses [max_tokens * compact_ratio] as the token budget. The budget ratio
    defaults to {!Types.default_context_compact_budget_ratio} (0.8) and is
    intentionally distinct from the proactive compaction watermark default
    ({!Types.default_context_compact_ratio}, 0.9).

    The reducer is composed with [drop_thinking], [repair_dangling_tool_calls],
    and [repair_orphaned_tool_results]. When [target_ratio] is set, the
    aggressive compaction path uses [max_tokens * target_ratio] as the ceiling
    budget, preserving the last [keep_recent_turns] turns. The [watermark]
    defaults to {!Types.default_context_compact_ratio} and triggers this
    aggressive path when context utilization exceeds it. Ratio arguments must
    be greater than 0.0 and less than 1.0.

    @since 0.79.0
    @since 0.185.0 — added target_ratio, watermark, keep_recent_turns *)
val from_context_config
  :  ?compact_ratio:float
  -> ?target_ratio:float
  -> ?watermark:float
  -> ?keep_recent_turns:int
  -> max_tokens:int
  -> unit
  -> t
