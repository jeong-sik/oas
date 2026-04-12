---- MODULE CascadeFSM_TTrace_1776014976 ----
EXTENDS CascadeFSM, Sequences, TLCExt, Toolbox, Naturals, TLC

_expression ==
    LET CascadeFSM_TEExpression == INSTANCE CascadeFSM_TEExpression
    IN CascadeFSM_TEExpression!expression
----

_trace ==
    LET CascadeFSM_TETrace == INSTANCE CascadeFSM_TETrace
    IN CascadeFSM_TETrace!trace
----

_inv ==
    ~(
        TLCGet("level") = Len(_TETrace)
        /\
        tried = ({1, 2, 3})
        /\
        decision = ("exhausted")
        /\
        outcomes = (<<"call_err_cascade", "call_err_cascade", "call_err_cascade">>)
        /\
        idx = (0)
    )
----

_init ==
    /\ tried = _TETrace[1].tried
    /\ idx = _TETrace[1].idx
    /\ outcomes = _TETrace[1].outcomes
    /\ decision = _TETrace[1].decision
----

_next ==
    /\ \E i,j \in DOMAIN _TETrace:
        /\ \/ /\ j = i + 1
              /\ i = TLCGet("level")
        /\ tried  = _TETrace[i].tried
        /\ tried' = _TETrace[j].tried
        /\ idx  = _TETrace[i].idx
        /\ idx' = _TETrace[j].idx
        /\ outcomes  = _TETrace[i].outcomes
        /\ outcomes' = _TETrace[j].outcomes
        /\ decision  = _TETrace[i].decision
        /\ decision' = _TETrace[j].decision

\* Uncomment the ASSUME below to write the states of the error trace
\* to the given file in Json format. Note that you can pass any tuple
\* to `JsonSerialize`. For example, a sub-sequence of _TETrace.
    \* ASSUME
    \*     LET J == INSTANCE Json
    \*         IN J!JsonSerialize("CascadeFSM_TTrace_1776014976.json", _TETrace)

=============================================================================

 Note that you can extract this module `CascadeFSM_TEExpression`
  to a dedicated file to reuse `expression` (the module in the 
  dedicated `CascadeFSM_TEExpression.tla` file takes precedence 
  over the module `CascadeFSM_TEExpression` below).

---- MODULE CascadeFSM_TEExpression ----
EXTENDS CascadeFSM, Sequences, TLCExt, Toolbox, Naturals, TLC

expression == 
    [
        \* To hide variables of the `CascadeFSM` spec from the error trace,
        \* remove the variables below.  The trace will be written in the order
        \* of the fields of this record.
        tried |-> tried
        ,idx |-> idx
        ,outcomes |-> outcomes
        ,decision |-> decision
        
        \* Put additional constant-, state-, and action-level expressions here:
        \* ,_stateNumber |-> _TEPosition
        \* ,_triedUnchanged |-> tried = tried'
        
        \* Format the `tried` variable as Json value.
        \* ,_triedJson |->
        \*     LET J == INSTANCE Json
        \*     IN J!ToJson(tried)
        
        \* Lastly, you may build expressions over arbitrary sets of states by
        \* leveraging the _TETrace operator.  For example, this is how to
        \* count the number of times a spec variable changed up to the current
        \* state in the trace.
        \* ,_triedModCount |->
        \*     LET F[s \in DOMAIN _TETrace] ==
        \*         IF s = 1 THEN 0
        \*         ELSE IF _TETrace[s].tried # _TETrace[s-1].tried
        \*             THEN 1 + F[s-1] ELSE F[s-1]
        \*     IN F[_TEPosition - 1]
    ]

=============================================================================



Parsing and semantic processing can take forever if the trace below is long.
 In this case, it is advised to uncomment the module below to deserialize the
 trace from a generated binary file.

\*
\*---- MODULE CascadeFSM_TETrace ----
\*EXTENDS CascadeFSM, IOUtils, TLC
\*
\*trace == IODeserialize("CascadeFSM_TTrace_1776014976.bin", TRUE)
\*
\*=============================================================================
\*

---- MODULE CascadeFSM_TETrace ----
EXTENDS CascadeFSM, TLC

trace == 
    <<
    ([tried |-> {},decision |-> "pending",outcomes |-> <<"pending", "pending", "pending">>,idx |-> 1]),
    ([tried |-> {1},decision |-> "try_next",outcomes |-> <<"call_err_cascade", "pending", "pending">>,idx |-> 2]),
    ([tried |-> {1, 2},decision |-> "try_next",outcomes |-> <<"call_err_cascade", "call_err_cascade", "pending">>,idx |-> 3]),
    ([tried |-> {1, 2, 3},decision |-> "exhausted",outcomes |-> <<"call_err_cascade", "call_err_cascade", "call_err_cascade">>,idx |-> 0])
    >>
----


=============================================================================

---- CONFIG CascadeFSM_TTrace_1776014976 ----
CONSTANTS
    NumProviders = 3
    AcceptOnExhaustion = TRUE

INVARIANT
    _inv

CHECK_DEADLOCK
    \* CHECK_DEADLOCK off because of PROPERTY or INVARIANT above.
    FALSE

INIT
    _init

NEXT
    _next

CONSTANT
    _TETrace <- _trace

ALIAS
    _expression
=============================================================================
\* Generated on Mon Apr 13 02:29:37 KST 2026