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

(** Map a compression phase to the appropriate reducer strategies.

    - [Full] returns an empty list (no reduction).
    - [Compact] returns [[PruneToolOutputs]].
    - [Aggressive] returns [[PruneToolOutputs; Drop_thinking; Merge_contiguous]].
    - [Emergency] returns [[Summarize_old; PruneToolOutputs; Drop_thinking; Merge_contiguous]].

    The [summarizer] parameter is used only for [Emergency] phase.
    If not provided, a default extractive summarizer is used. *)
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
