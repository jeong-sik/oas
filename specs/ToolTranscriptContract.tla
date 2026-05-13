---- MODULE ToolTranscriptContract ----
\* Models the strict transcript contract for tool-use/result adjacency.
\*
\* Concrete code:
\*   - Context_reducer_apply.apply_repair_dangling_tool_calls
\*   - Context_reducer_apply.apply_repair_orphaned_tool_results
\*   - Backend_openai_serialize.strip_orphaned_tool_results
\*
\* A ToolResult only belongs to the immediate result span following the
\* Assistant ToolUse that introduced its id. If a normal message would
\* intervene, OAS first inserts a synthetic error ToolResult; any later
\* ToolResult for that id is removed.
\*
\* Bug model: BugLateResultAccepted accepts a ToolResult after the span
\* has closed.

EXTENDS Naturals

CONSTANTS ToolIds

ASSUME ToolIds # {}

VARIABLES
    phase,
    pending,
    accepted_results,
    late_results

vars == <<phase, pending, accepted_results, late_results>>

Phases == {"Ready", "NeedResult"}

TypeOK ==
    /\ phase \in Phases
    /\ pending \subseteq ToolIds
    /\ accepted_results \subseteq ToolIds
    /\ late_results \subseteq ToolIds
    /\ phase = "Ready" => pending = {}

Init ==
    /\ phase = "Ready"
    /\ pending = {}
    /\ accepted_results = {}
    /\ late_results = {}

AssistantToolUse ==
    /\ phase = "Ready"
    /\ phase' = "NeedResult"
    /\ pending' = ToolIds
    /\ UNCHANGED <<accepted_results, late_results>>

AppendAdjacentToolResults ==
    /\ phase = "NeedResult"
    /\ phase' = "Ready"
    /\ accepted_results' = accepted_results \cup pending
    /\ pending' = {}
    /\ UNCHANGED late_results

RepairBeforeInterveningMessage ==
    /\ phase = "NeedResult"
    /\ phase' = "Ready"
    /\ accepted_results' = accepted_results \cup pending
    /\ pending' = {}
    /\ UNCHANGED late_results

AppendPlainMessage ==
    /\ phase = "Ready"
    /\ UNCHANGED vars

Next ==
    \/ AssistantToolUse
    \/ AppendAdjacentToolResults
    \/ RepairBeforeInterveningMessage
    \/ AppendPlainMessage

Spec == Init /\ [][Next]_vars

NoLateToolResultsAccepted == late_results = {}
PendingCoveredBeforeClose == phase = "Ready" => pending = {}

BugLateResultAccepted ==
    /\ phase = "Ready"
    /\ late_results' = late_results \cup ToolIds
    /\ UNCHANGED <<phase, pending, accepted_results>>

NextBuggy ==
    \/ Next
    \/ BugLateResultAccepted

SpecBuggy == Init /\ [][NextBuggy]_vars

====
