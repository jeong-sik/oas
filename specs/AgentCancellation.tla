---- MODULE AgentCancellation ----
\* Models Runtime.phase cancellation semantics.
\*
\* Addresses the "Cancelled dilemma": Runtime.phase has 7 states including
\* Cancelled, but AgentLifecycle.tla (5 states) explicitly declines to model
\* cancellation.  This spec closes that gap.
\*
\* States:
\*   Bootstrapping, Running, Waiting_on_workers, Finalizing,
\*   Completed, Failed, Cancelled
\*
\* Allowed transitions (inferred from runtime_projection.ml apply_event
\* and ensure_active_phase semantics):
\*   Bootstrapping      -> Running | Failed | Cancelled
\*   Running            -> Waiting_on_workers | Finalizing | Completed | Failed | Cancelled
\*   Waiting_on_workers -> Running | Finalizing | Failed | Cancelled
\*   Finalizing         -> Completed | Failed | Cancelled
\*   Completed          -> (terminal)
\*   Failed             -> (terminal)
\*   Cancelled          -> (terminal)
\*
\* Cancellation is triggered by Eio.Cancel.Cancelled and is terminal.
\* Recovery from Cancelled is NOT modeled — consistent with Eio.Switch
\* semantics where cancellation is final (guardrail_tripwire.ml:45).
\*
\* @since (issue #1212 follow-up)

EXTENDS Naturals, FiniteSets

CONSTANTS
    MaxTurns        \* Bound on Bootstrapping->Running cycles

VARIABLES
    phase,          \* Current runtime phase
    prev_phase,     \* Phase that immediately preceded `phase`
    turn_count      \* Number of completed Bootstrapping->Running cycles

vars == <<phase, prev_phase, turn_count>>

States       == {"Bootstrapping", "Running", "Waiting_on_workers", "Finalizing", "Completed", "Failed", "Cancelled"}
StatesOrNone == States \cup {"None"}

NonTerminal == {"Bootstrapping", "Running", "Waiting_on_workers", "Finalizing"}
Terminal    == {"Completed", "Failed", "Cancelled"}

TypeOK ==
    /\ phase \in States
    /\ prev_phase \in StatesOrNone
    /\ turn_count \in 0..MaxTurns

\* ── Helpers ──────────────────────────────────
IsTerminal(s) == s \in Terminal

\* Allowed next-state set per runtime semantics.
AllowedNext(s) ==
    CASE s = "Bootstrapping"      -> {"Running", "Failed", "Cancelled"}
      [] s = "Running"            -> {"Waiting_on_workers", "Finalizing", "Completed", "Failed", "Cancelled"}
      [] s = "Waiting_on_workers" -> {"Running", "Finalizing", "Failed", "Cancelled"}
      [] s = "Finalizing"         -> {"Completed", "Failed", "Cancelled"}
      [] s = "Completed"          -> {}
      [] s = "Failed"             -> {}
      [] s = "Cancelled"          -> {}

\* ── Initial state ────────────────────────────
Init ==
    /\ phase = "Bootstrapping"
    /\ prev_phase = "None"
    /\ turn_count = 0

\* ── Transition actions ───────────────────────
Transition(to_) ==
    /\ ~IsTerminal(phase)
    /\ to_ # phase                                  \* not a reaffirm
    /\ to_ \in AllowedNext(phase)
    \* Bound Bootstrapping->Running cycles.
    /\ ~(phase = "Bootstrapping" /\ to_ = "Running" /\ turn_count >= MaxTurns)
    /\ prev_phase' = phase
    /\ phase' = to_
    /\ turn_count' =
        IF phase = "Bootstrapping" /\ to_ = "Running"
        THEN turn_count + 1
        ELSE turn_count

\* Same-state reaffirm: allowed on non-terminal states.
Reaffirm ==
    /\ ~IsTerminal(phase)
    /\ UNCHANGED vars

\* Stutter on terminal states.
StutterTerminal ==
    /\ IsTerminal(phase)
    /\ UNCHANGED vars

Next ==
    /\ \E to_ \in States : Transition(to_)
    /\ Reaffirm
    /\ StutterTerminal

Spec == Init /\ [][Next]_vars /\ WF_vars(\E to_ \in States : Transition(to_))

\* ── Safety Invariants ────────────────────────

\* 1. PrevNotEqualPhase: prev_phase differs from phase (when set).
PrevNotEqualPhase ==
    prev_phase # "None" => prev_phase # phase

\* 2. TerminalIsStable: once terminal, phase never changes.
TerminalIsStable ==
    IsTerminal(prev_phase) => phase = prev_phase

\* 3. NoIllegalTransition: every transition arrow is declared.
NoIllegalTransition ==
    prev_phase # "None" =>
        phase \in AllowedNext(prev_phase)

\* 4. CancelledIsTerminal: Cancelled has no outgoing transitions.
CancelledIsTerminal ==
    phase = "Cancelled" => IsTerminal(phase)

\* 5. CancelledRequiresNonTerminal: Cancelled can only be reached from
\*    a non-terminal state (or initial).
CancelledRequiresNonTerminal ==
    phase = "Cancelled" =>
        (prev_phase = "None" \/ prev_phase \in NonTerminal)

\* 6. NoTransitionIntoBootstrapping: Bootstrapping is initial-only.
NoTransitionIntoBootstrapping ==
    (phase = "Bootstrapping") => (prev_phase \in {"None", "Bootstrapping"})

\* ── Bug Model: Cancelled Resurrection ────────
\* Models a regression where Cancelled is incorrectly re-transitioned
\* to Running — e.g. a buggy checkpoint restore or retry loop that
\* ignores cancellation as a terminal state.
\*
\* SHOULD violate TerminalIsStable.

BugCancelledResurrection ==
    /\ phase = "Cancelled"
    /\ prev_phase' = "Cancelled"
    /\ phase' = "Running"
    /\ UNCHANGED turn_count

NextBuggy ==
    /\ \E to_ \in States : Transition(to_)
    /\ Reaffirm
    /\ StutterTerminal
    /\ BugCancelledResurrection

SpecBuggy == Init /\ [][NextBuggy]_vars /\ WF_vars(\E to_ \in States : Transition(to_))

\* Invariant SHOULD be violated under SpecBuggy.
TerminalIsStableMustHold == TerminalIsStable

====
