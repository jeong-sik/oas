(** Budget-aware context compression strategy.

    Maps token budget usage ratios to progressively aggressive
    context reduction strategies. Designed for integration with
    external budget managers.

    Phase thresholds:
    - 0.0-0.5  Full: no compression
    - 0.5-0.7  Compact: prune tool outputs
    - 0.7-0.85 Aggressive: prune + merge + drop thinking
    - 0.85+    Emergency: summarize old + all aggressive strategies

    @since 0.78.0

    @stability Evolving
    @since 0.93.1 *)

(** Compression phase, ordered from lightest to heaviest. *)
type compression_phase =
  | Full        (** No compression needed. *)
  | Compact     (** Light: prune tool outputs only. *)
  | Aggressive  (** Medium: prune outputs + drop thinking + merge contiguous. *)
  | Emergency   (** Heavy: summarize old + all aggressive strategies. *)

(** Default extractive summarizer.

    Joins the first [Text] block of each message, truncating each to
    100 chars and prefixing with role ([User]/[Assistant]/[System]/[Tool]).
    Used by [reduce_for_budget] and [strategies_for_phase] when no custom
    [summarizer] is supplied.

    Exported so downstream consumers can wrap it — for example, a
    domain-aware summarizer that scrubs application-specific markers
    from each [Text] block before delegating to this function. Without
    export, consumers had to re-implement the default to get the same
    output shape.

    @since 0.153.0 *)
val default_summarizer : Types.message list -> string

(** Map a compression phase to the appropriate reducer strategies.

    - [Full] returns an empty list (no reduction).
    - [Compact] returns [[PruneToolOutputs]].
    - [Aggressive] returns [[PruneToolOutputs; Drop_thinking; Merge_contiguous]].
    - [Emergency] returns [[Summarize_old; PruneToolOutputs; Drop_thinking; Merge_contiguous]].

    The [summarizer] parameter is used only for [Emergency] phase.
    If not provided, {!default_summarizer} is used. *)
val strategies_for_phase :
  ?summarizer:(Types.message list -> string) ->
  compression_phase ->
  Context_reducer.strategy list

(** Determine compression phase from budget usage ratio (0.0-1.0).

    - [0.0, 0.5)   -> [Full]
    - [0.5, 0.7)   -> [Compact]
    - [0.7, 0.85)  -> [Aggressive]
    - [0.85, 1.0+]  -> [Emergency]

    Values outside [0.0, 1.0] are clamped. *)
val phase_of_usage_ratio : float -> compression_phase

(** Convenience: determine phase from ratio, build composed reducer, apply.

    Equivalent to:
    {[
      let phase = phase_of_usage_ratio usage_ratio in
      let strategies = strategies_for_phase ?summarizer phase in
      (* apply Compose strategies to messages *)
    ]}

    Returns messages unchanged when phase is [Full]. *)
val reduce_for_budget :
  ?summarizer:(Types.message list -> string) ->
  usage_ratio:float ->
  messages:Types.message list ->
  unit ->
  Types.message list

(** String representation of a compression phase for logging. *)
val show_phase : compression_phase -> string

(** Aggregate context budget metrics.
    Convenience type combining ratio, phase, and limit proximity into
    a single value that downstream coordinators can consume without
    performing raw arithmetic on token counts.
    @since 0.105.0 *)
type context_metrics = {
  usage_ratio : float;       (** 0.0 to 1.0+. *)
  phase : compression_phase; (** Compression phase for this ratio. *)
  is_near_limit : bool;      (** [true] when [usage_ratio >= 0.85]. *)
  estimated_tokens : int;    (** Estimated tokens in context. *)
  context_window : int;      (** Total context window size. *)
}

(** Build context metrics from estimated token count and context window size.
    Returns a pre-computed [context_metrics] record with phase and limit
    proximity derived from the usage ratio.
    @since 0.105.0 *)
val context_metrics :
  estimated_tokens:int -> context_window:int -> context_metrics
