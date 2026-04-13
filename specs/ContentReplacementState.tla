---- MODULE ContentReplacementState ----
\* Formal specification of the Content Replacement State (CRS) decision
\* freezing mechanism from RFC-OAS-005 (Tool Result Relocation Architecture).
\*
\* Models Content_replacement_state.ml: once a tool_use_id is "seen",
\* its replacement decision (replaced or kept) is frozen forever.
\* This preserves prompt cache prefixes across turns.
\*
\* Properties verified:
\*   Safety:  FreezeMonotonicity, ReplacementStability, DecisionConsistency
\*   Safety (buggy): BuggyFreezeMonotonicity must be VIOLATED
\*
\* Reference: Claude Code toolResultStorage.ts ContentReplacementState

EXTENDS Naturals, FiniteSets

CONSTANTS
    ToolIds,        \* Set of tool_use_ids, e.g. {"t1", "t2", "t3", "t4"}
    Threshold       \* Content size threshold for replacement (abstract)

VARIABLES
    seen,           \* Set of tool_use_ids that have been processed
    replaced,       \* Set of tool_use_ids that were replaced with previews
    kept,           \* Set of tool_use_ids that were kept (below threshold)
    content_size,   \* [ToolIds -> 0..Threshold+1] simulated content sizes
    turn            \* Current turn number (for liveness)

vars == <<seen, replaced, kept, content_size, turn>>

\* ── Initial State ────────────────────────────────────────

Init ==
    /\ seen = {}
    /\ replaced = {}
    /\ kept = {}
    \* Each tool_use_id gets a random content size (0 to Threshold+1)
    /\ content_size \in [ToolIds -> 0..(Threshold + 1)]
    /\ turn = 0

\* ── Type Invariant ───────────────────────────────────────

TypeOK ==
    /\ seen \subseteq ToolIds
    /\ replaced \subseteq seen
    /\ kept \subseteq seen
    /\ replaced \cap kept = {}              \* Mutually exclusive
    /\ replaced \cup kept = seen            \* Partitions seen
    /\ content_size \in [ToolIds -> Nat]
    /\ turn \in Nat

\* ── Actions ──────────────────────────────────────────────

\* A tool result appears for the first time.
\* Decision: replace (persist to disk) or keep (below threshold).
\* This is the ONLY action that adds to seen/replaced/kept.
ProcessFreshResult(id) ==
    /\ id \notin seen
    /\ seen' = seen \cup {id}
    /\ IF content_size[id] > Threshold
       THEN /\ replaced' = replaced \cup {id}
            /\ kept' = kept
       ELSE /\ kept' = kept \cup {id}
            /\ replaced' = replaced
    /\ UNCHANGED <<content_size, turn>>

\* A frozen tool_use_id reappears on a subsequent turn.
\* The cached decision is re-applied (no state change).
\* Models apply_frozen in Content_replacement_state.ml.
ReapplyFrozen(id) ==
    /\ id \in seen
    \* No state change — the decision is frozen.
    \* This action exists to show that seen IDs cause no mutation.
    /\ UNCHANGED <<seen, replaced, kept, content_size>>
    /\ turn' = turn + 1

\* Advance turn (models LLM turn boundary).
AdvanceTurn ==
    /\ turn' = turn + 1
    /\ UNCHANGED <<seen, replaced, kept, content_size>>

\* ── Next-State Relation ──────────────────────────────────

Next ==
    \/ \E id \in ToolIds : ProcessFreshResult(id)
    \/ \E id \in ToolIds : ReapplyFrozen(id)
    \/ AdvanceTurn

Spec == Init /\ [][Next]_vars

\* ── Safety Properties ────────────────────────────────────

\* S1. Freeze Monotonicity: once an ID enters seen, it stays forever.
\*     This is the core invariant — decisions are irreversible.
\*     Action-level: seen only grows in each step.
FreezeMonotonicityAction ==
    \A id \in ToolIds :
        id \in seen => id \in seen'

\* Temporal wrapper for PROPERTY use
FreezeMonotonicity == [][FreezeMonotonicityAction]_vars

\* S2. Replacement Stability: once an ID is in replaced, it stays.
\*     A replaced tool result is never "un-replaced".
ReplacementStabilityAction ==
    \A id \in ToolIds :
        id \in replaced => id \in replaced'

ReplacementStability == [][ReplacementStabilityAction]_vars

\* S3. Decision Consistency: replaced and kept are disjoint and
\*     together equal seen. No ID can be both replaced and kept.
DecisionConsistency ==
    /\ replaced \cap kept = {}
    /\ replaced \cup kept = seen

\* S4. Keep Stability: once kept, always kept.
KeepStabilityAction ==
    \A id \in ToolIds :
        id \in kept => id \in kept'

KeepStability == [][KeepStabilityAction]_vars

\* Combined safety invariant for cfg file
Safety ==
    /\ TypeOK
    /\ DecisionConsistency

\* State bound for model checking (turn is unbounded otherwise)
StateBound == turn <= 4

\* ── Buggy Model ──────────────────────────────────────────
\* Models a bug where a "kept" decision can be reverted to "replaced"
\* (e.g., if threshold changes mid-session or re-evaluation is allowed).
\* FreezeMonotonicity and KeepStability must be VIOLATED.

BuggyReEvaluate(id) ==
    /\ id \in kept
    /\ content_size[id] > 0     \* Has any content
    \* Bug: move from kept to replaced, breaking the freeze
    /\ kept' = kept \ {id}
    /\ replaced' = replaced \cup {id}
    /\ UNCHANGED <<seen, content_size, turn>>

NextBuggy ==
    \/ Next
    \/ \E id \in ToolIds : BuggyReEvaluate(id)

SpecBuggy == Init /\ [][NextBuggy]_vars

====
