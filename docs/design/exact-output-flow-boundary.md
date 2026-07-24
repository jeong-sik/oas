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
- the ordered outer-flow transition to a predetermined successor.

The caller owns:

- domain input construction;
- domain schema, codec, and content validation;
- durable business-state binding, release, quarantine, and completion;
- operator configuration that names opaque candidate references.

The outer flow is generic. It contains no coordinator vocabulary and no
provider, model, tier, price, query-intent, or error-string policy.

## Immutable flow snapshot

An outer flow freezes one nonempty ordered candidate snapshot, its
catalog-admitted opaque target handles, and one immutable domain input. Catalog
membership and credential outcomes are frozen, but credentials remain
unselected. Starting the flow allocates one OAS-owned outer-flow identity and
precomputes one immutable visit `(flow ID, 1-based ordinal, candidate identity)`
for every frozen candidate. This performs no credential selection, request
admission, call identity allocation, callback, or network effect.

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
- success.

Domain validation occurs only after an OAS success. A caller-side domain
rejection therefore cannot trigger another provider dispatch.

## Evidence

Every flow evidence projection carries:

- the fresh outer-flow identity;
- the original immutable candidate identity snapshot;
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
- Before 1.0, obsolete runtime assets are reset rather than reconciled.

Archived cascade documents describe an earlier ownership decision and are not
the active exact-output contract.
