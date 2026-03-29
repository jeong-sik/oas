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

(** Windowing strategy for context reduction. *)
type strategy =
  | Keep_last_n of int
  | Token_budget of int
  | Prune_tool_outputs of { max_output_len: int }
  | Prune_tool_args of { max_arg_len: int; keep_recent: int }
  | Repair_dangling_tool_calls
  | Merge_contiguous
  | Drop_thinking
  | Keep_first_and_last of { first_n: int; last_n: int }
  | Prune_by_role of { drop_roles: role list }
  | Summarize_old of { keep_recent: int; summarizer: message list -> string }
  | Clear_tool_results of { keep_recent: int }
  | Compose of strategy list
  | Custom of (message list -> message list)
  | Dynamic of (turn:int -> messages:message list -> strategy)

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

(** Estimate tokens for a single content block. *)
val estimate_block_tokens : content_block -> int

(** Estimate tokens for a message. *)
val estimate_message_tokens : message -> int

(** {1 Turn grouping} *)

(** Group messages into turns.

    A turn starts with a User message and includes all following
    messages until the next User message. User messages containing
    ToolResult blocks belong to the preceding turn. *)
val group_into_turns : message list -> message list list

(** {1 Core reducer} *)

(** Reduce messages according to the configured strategy. *)
val reduce : t -> message list -> message list

(** {1 Convenience constructors} *)

val keep_last : int -> t
val token_budget : int -> t
val prune_tool_outputs : max_output_len:int -> t
val prune_tool_args : max_arg_len:int -> ?keep_recent:int -> unit -> t
val repair_dangling_tool_calls : t
val merge_contiguous : t
val drop_thinking : t
val keep_first_and_last : first_n:int -> last_n:int -> t
val prune_by_role : drop_roles:role list -> t
val summarize_old : keep_recent:int -> summarizer:(message list -> string) -> t
val clear_tool_results : keep_recent:int -> t
val compose : t list -> t
val custom : (message list -> message list) -> t
val importance_scored :
  ?threshold:float ->
  ?boost:importance_boost ->
  scorer:importance_scorer ->
  unit -> t

(** Dynamic strategy: selects a strategy per turn based on
    conversation state. *)
val dynamic : (turn:int -> messages:message list -> strategy) -> t

(** {1 Capabilities integration} *)

(** Create a reducer from provider capabilities.
    Uses [max_context_tokens * margin] as the token budget (default 80%),
    composed with [drop_thinking] and [repair_dangling_tool_calls].
    Returns [None] if [max_context_tokens] is unknown. *)
val from_capabilities :
  ?margin:float -> Llm_provider.Capabilities.capabilities -> t option

(** Create a reducer from an explicit context budget with configurable thresholds.
    Uses [max_tokens * compact_ratio] as the token budget (default 80%),
    composed with [drop_thinking] and [repair_dangling_tool_calls].

    @since 0.79.0 *)
val from_context_config :
  ?compact_ratio:float -> max_tokens:int -> unit -> t
