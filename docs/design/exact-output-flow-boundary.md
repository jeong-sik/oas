# Exact-output outer-flow boundary

Status: active pre-1.0 contract  
Effective: 2026-07-23

## Ownership

`Agent_sdk.Exact_output` is the single public surface for exact structured
output. OAS owns:

- immutable resolver snapshots and catalog-admitted opaque target handles;
- provider-wire admission and separately frozen candidate plans;
- one fresh outer-flow identity with immutable, preordered candidate visits;
- fresh, non-shared attempt identities;
- one-plan, at-most-one-dispatch `execute_once`;
- monotonic phase, dispatch-count, response, and provenance receipts;
- the ordered outer-flow transition to a predetermined successor;
- explicit scope-local last-good preference for future snapshots;
- an immutable monotonic success ordinal allocated when structural success is
  created;
- affine typed settlement of caller-owned domain validation.

The caller owns:

- domain input construction;
- domain schema, codec, and content validation;
- durable business-state binding, release, quarantine, and completion;
- operator configuration that names opaque candidate references;
- creation and lifetime of each process-local preference store and opaque flow
  scope, including its hard capacity and explicit scope removal;
- the domain-valid or domain-rejected disposition, but no freshness value.

The outer flow is generic. It contains no coordinator vocabulary and no
provider, model, tier, price, query-intent, or error-string policy.

## Immutable flow snapshot

An outer flow freezes one nonempty ordered candidate snapshot, its
catalog-admitted opaque target handles, and one immutable domain input. Catalog
membership and credential outcomes are frozen, but credentials remain
unselected. The caller supplies an explicit preference store and opaque
nonempty flow scope. OAS performs one exact scope lookup while creating the
snapshot and atomically reserves that scope under the caller-supplied hard
capacity. A new scope that would exceed the capacity fails with typed capacity
evidence. Each reservation has a private generation carried by the snapshot,
attempt, and success; removing and recreating the same textual scope cannot
allow an older success to write into the new generation. The store retains the
full successful candidate identity. OAS moves that candidate to the front only
when both its opaque caller slot identity and opaque target-identity fingerprint
still match. An absent slot or a slot rebound to another target is not promoted
and remains typed observation evidence. The relative order of every other
candidate is preserved.

The resulting order is immutable. A later success cannot change an existing
snapshot, and a preference recorded in one scope cannot affect another scope.
The store is caller-owned, bounded, and process-local. Explicit scope removal
frees one capacity slot. There is no singleton, implicit clock, environment
policy, expiry, eviction heuristic, persistence, or refresh daemon.

Starting the flow allocates one OAS-owned outer-flow identity and precomputes one
immutable visit `(flow ID, 1-based ordinal, candidate identity)` for every
frozen candidate. This performs no credential selection, request admission,
call identity allocation, callback, or network effect.

During execution, OAS resolves the frozen credential outcome and prepares only
the current candidate. Successful request admission freezes one ready plan and
receives one fresh non-shared attempt. A target-selection or request-admission
rejection receives no plan, call ID, or execution receipt.

## Execution

`execute_flow_once` is affine. A duplicate or concurrent invocation cannot
dispatch. For each predetermined candidate:

1. OAS resolves only the current precomputed visit's frozen target and admits
   its exact request contract.
2. A typed target-selection or request-admission rejection receives an
   immutable candidate-rejection receipt fixed at `Before_dispatch` and
   `dispatch_count = 0`; it does not enter
   `before_dispatch`.
3. For an admitted candidate, the caller's `before_dispatch` callback must
   confirm its durable binding before OAS invokes the unchanged single-plan
   `execute_once`.
4. OAS may select the next frozen candidate only after either that candidate
   rejection receipt or a first-use transport failure whose exact execution
   receipt is `Before_dispatch` with `dispatch_count = 0`.
5. The caller's `before_advance` callback receives the settled typed failure and
   the predetermined successor visit, and must durably confirm release before
   OAS prepares that successor.

Callbacks can stop a transition but cannot select, replace, or reorder a
candidate.

Advance eligibility is effect-based, not a classifier over rejection causes.
Every typed target-selection or request-admission rejection is pre-dispatch and
zero-dispatch, so it is eligible for the predetermined successor. OAS does not
infer that a credential, schema, or requirement rejection must behave
identically across opaque targets. Target-specific credential outcomes, schema
validators, and capability admission are evidence about the visited candidate,
not proof that an unvisited candidate must reject the contract.

The following outcomes are terminal:

- callback failure or exception;
- duplicate/replayed execution;
- cancellation;
- a missing execution prerequisite or frozen-request invariant failure;
- any receipt with `dispatch_count > 0`;
- response, partial, tool, structural-output, or normalization exposure;
- a final typed candidate rejection, reported as `Flow_candidates_exhausted`;
- structural success.

After structural success, the caller submits exactly one typed domain
disposition. `Domain_rejected` leaves the flow terminal and records no
preference. OAS allocates a monotonic immutable ordinal when it creates each
structural success. `Domain_valid` carries no caller-provided freshness and
atomically records the opaque successful candidate and its frozen ordinal for
future snapshots in the same scope reservation. A duplicate or concurrent
disposition returns a typed already-settled error. A domain-valid settlement
whose reservation was explicitly removed, including one whose textual scope
has since been recreated, returns a typed released-scope error and is still
consumed exactly once. Neither disposition can trigger another provider
dispatch.

An older or equal OAS-owned success ordinal cannot overwrite a newer
observation for the same scope reservation. OAS uses no wall clock, caller
timestamp, string, or target-specific tie-break. An older observation leaves
the installed preference unchanged and returns a typed superseded receipt
containing the retained candidate identity and ordinal.

The per-success settlement gate remains held until domain-valid preference
publication completes. A losing duplicate therefore cannot return
`Domain_already_settled` and immediately observe a pre-publication snapshot.
The only nested lock order is settlement gate then preference store; no path
acquires those locks in reverse.

No network or filesystem I/O occurs while the preference mutex is held.

## Evidence

Every flow evidence projection carries:

- the fresh outer-flow identity;
- the exact opaque flow scope;
- the caller-declared candidate identity snapshot;
- the frozen effective candidate identity snapshot;
- the single frozen preference observation: no record, applied, absent slot, or
  changed target binding, including the observed OAS-owned success ordinal when
  present;
- the monotonic typed candidate-visit count;
- ordered admission outcomes only for candidates reached so far;
- every non-shared execution receipt allocated for an admitted current
  candidate;

Terminal variants that settle a candidate additionally carry its exact
rejection, success, or execution failure. The `admitted_flow_candidate` and
`flow_attempt_receipt` wrappers embed the same precomputed visit; a rejection
embeds that visit without fabricating a plan, call ID, or execution receipt. A
new `start_flow` always allocates a new flow identity; restart resume is
unsupported here and belongs to the caller's authenticated lease or operation
journal.

Every admission rejection and allocated outer attempt receipt carries the same
opaque scope as the flow evidence. The scope, candidate identity, and one-shot
receipt therefore form one private immutable record binding; the caller cannot
construct or splice that join from coordinator or target strings.

After cancellation escapes, the same aggregate evidence remains queryable from
the opaque flow attempt.

The affine executor is the sole progress writer. Concurrent evidence readers
may observe the exact intermediate point after a successful request admission
is recorded and before that candidate receives its fresh attempt. Such a
snapshot is point-in-time evidence, not a fabricated attempt or a terminal
state.

## Explicit exclusions

- `execute_once` never retries and never changes plans.
- The outer flow performs no weighted, health, cost, or latency routing.
- Pricing is measurement evidence only and is absent from all decisions.
- Public flow rejections expose provider-neutral dispositions and capacity
  bounds only; provider, model, endpoint, credential environment, schema path,
  and raw serving-evidence source remain private to OAS.
- Resolver bootstrap and direct-execution diagnostics remain OAS integration
  surfaces, not MASC control inputs. MASC consumers may import only the outer
  flow's provider-neutral visit, disposition, receipt, and provenance
  projections.
- There is no legacy cascade API, compatibility wrapper, or persisted runtime
  JSON migration.
- A process-local last-good store is not a capability probe, health score,
  retry budget, or replacement for caller-owned durable state.
- Preference capacity is caller policy, but exhaustion, reservation
  invalidation, and success ordering are typed OAS transitions rather than
  TTL/LRU cleanup or caller-supplied timestamps.
- Before 1.0, obsolete runtime assets are reset rather than reconciled.

Archived cascade documents describe an earlier ownership decision and are not
the active exact-output contract.
