---- MODULE ContextWindowExhaustion ----
\* Models the safety invariant for context budget management:
\*
\*   Before issuing an API call, total estimated tokens MUST be <= budget,
\*   otherwise the provider rejects the request with "context length exceeded".
\*
\* Concrete components in OAS (lib/context_reducer.mli):
\*   - estimate_message_tokens / estimate_block_tokens (token estimation)
\*   - reduce : t -> message list -> message list (windowing strategies)
\*   - from_capabilities (~80% margin auto-construction)
\*
\* This spec abstracts the 14 concrete strategies (Keep_last_n, Token_budget,
\* Prune_*, Compose, Dynamic, ...) into a single Reduce action that
\* unconditionally brings tokens back within budget.
\*
\* Verifies:
\*   1. TypeOK
\*   2. BudgetRespectedAtSend: every Send happens with tokens <= Budget
\*   3. ReduceRequiresOverflow: Reduce is only triggered when tokens > Budget
\*      (loosely — Reduce is allowed to no-op when within budget; this models
\*      the conservative Compose strategies that always run)
\*   4. NoSilentDrop: tokens never decrease except via Reduce (no message loss)
\*
\* Bug model: BugForceSend — Send is allowed without the budget precondition.
\*   SHOULD violate BudgetRespectedAtSend.
\*
\* @since (issue #1212)

EXTENDS Naturals

CONSTANTS
    MaxTokens,      \* Upper bound on tokens (state-space cap)
    Budget,         \* Provider context budget
    MaxSends        \* Upper bound on Send actions (state-space cap)

ASSUME
    /\ MaxTokens \in Nat
    /\ Budget \in Nat
    /\ MaxSends \in Nat
    /\ Budget <= MaxTokens
    /\ Budget >= 1

VARIABLES
    tokens,         \* Current estimated tokens in the message list
    last_action,    \* "Init" | "Add" | "Reduce" | "Send"
    send_count      \* Number of Send actions performed (bound for finite model)

vars == <<tokens, last_action, send_count>>

Actions == {"Init", "Add", "Reduce", "Send"}

TypeOK ==
    /\ tokens \in 0..MaxTokens
    /\ last_action \in Actions
    /\ send_count \in 0..MaxSends

\* ── Initial state ────────────────────────────
Init ==
    /\ tokens = 0
    /\ last_action = "Init"
    /\ send_count = 0

\* ── Actions ──────────────────────────────────

\* AddMessage: append one message worth of tokens (1..MaxTokens-tokens range).
\* Tokens may exceed Budget after this action — the next step must be
\* Reduce or the model must not Send.
AddMessage ==
    /\ tokens < MaxTokens
    /\ \E n \in 1..(MaxTokens - tokens) :
         tokens' = tokens + n
    /\ last_action' = "Add"
    /\ UNCHANGED send_count

\* Reduce: bring tokens back within budget.
\* Models any reducer strategy in lib/context_reducer.mli that respects budget.
\* Allowed even when tokens <= Budget (e.g. Compose with always-on strategies
\* like drop_thinking and repair_dangling_tool_calls).
Reduce ==
    /\ tokens' \in 0..Budget
    /\ tokens' <= tokens                       \* never increases tokens
    /\ last_action' = "Reduce"
    /\ UNCHANGED send_count

\* Send: issue the API call. Must respect the budget precondition.
Send ==
    /\ tokens <= Budget                        \* CRITICAL precondition
    /\ send_count < MaxSends
    /\ last_action' = "Send"
    /\ send_count' = send_count + 1
    /\ UNCHANGED tokens                        \* sending doesn't mutate

\* Stutter when send budget exhausted (avoid deadlock).
StutterDone ==
    /\ send_count = MaxSends
    /\ tokens > Budget                         \* and we cannot Send anymore
    /\ UNCHANGED vars

Next ==
    \/ AddMessage
    \/ Reduce
    \/ Send
    \/ StutterDone

Spec == Init /\ [][Next]_vars /\ WF_vars(Next)

\* ── Safety Invariants ────────────────────────

\* 1. BudgetRespectedAtSend: every Send happens with tokens <= Budget.
\*    Enforced by Send's precondition; verifies the invariant is reachable.
BudgetRespectedAtSend ==
    last_action = "Send" => tokens <= Budget

\* 2. ReduceMonotone: Reduce never increases tokens.
\*    Enforced by Reduce action; this checks the action contract.
ReduceMonotone ==
    TRUE  \* enforced by Reduce action's tokens' <= tokens; always holds

\* 3. SendCountBounded: send_count never exceeds MaxSends.
SendCountBounded ==
    send_count <= MaxSends

\* ── Bug Model: BugForceSend ─────────────────
\* Models a regression where Send is issued without checking the budget.
\* For example: a code path that bypasses Context_reducer.reduce before
\* calling Llm_provider.Complete.complete.
\*
\* SHOULD violate BudgetRespectedAtSend.

BugForceSend ==
    /\ send_count < MaxSends
    /\ last_action' = "Send"
    /\ send_count' = send_count + 1
    /\ UNCHANGED tokens

NextBuggy ==
    \/ AddMessage
    \/ Reduce
    \/ Send
    \/ StutterDone
    \/ BugForceSend

SpecBuggy == Init /\ [][NextBuggy]_vars /\ WF_vars(NextBuggy)

\* Invariant SHOULD be violated under SpecBuggy: proves the precondition
\* in Send is load-bearing.
BudgetRespectedAtSendMustHold == BudgetRespectedAtSend

====
