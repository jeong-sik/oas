# Exact-output outer-flow boundary

Status: active pre-1.0 contract  
Effective: 2026-07-23

## Ownership

`Agent_sdk.Exact_output` is the single public surface for exact structured
output. OAS owns:

- immutable resolver and target snapshots;
- provider-wire admission and separately frozen candidate plans;
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

An outer flow freezes one nonempty ordered candidate snapshot and one immutable
domain input. It does not prepare every candidate speculatively. Starting the
flow creates only the affine flow handle; it performs no target admission, call
identity allocation, or network effect.

During execution, OAS prepares only the current candidate. Successful
preparation freezes one ready plan and receives one fresh non-shared attempt.
Rejected candidates receive no plan, call ID, or execution receipt.

## Execution

`execute_flow_once` is affine. A duplicate or concurrent invocation cannot
dispatch. For each predetermined candidate:

1. OAS increments the typed candidate-attempt count and prepares the current
   exact target and contract.
2. A typed admission rejection receives an immutable admission receipt fixed at
   `Before_dispatch` and `dispatch_count = 0`; it does not enter
   `before_dispatch`.
3. For an admitted candidate, the caller's `before_dispatch` callback must
   confirm its durable binding before OAS invokes the unchanged single-plan
   `execute_once`.
4. OAS may select the next frozen candidate only after either that admission
   receipt or a first-use transport failure whose exact execution receipt is
   `Before_dispatch` with `dispatch_count = 0`.
5. The caller's `before_advance` callback receives the settled typed failure and
   the next frozen candidate identity, and must durably confirm release before
   OAS prepares that successor.

Callbacks can stop a transition but cannot select, replace, or reorder a
candidate.

Advance eligibility is effect-based, not a classifier over admission-error
causes. Every typed admission rejection is pre-dispatch and zero-dispatch, so
it is eligible for the predetermined successor. OAS does not infer that a
schema or requirement rejection must behave identically across opaque targets.
Target-specific schema validators and capability admission can emit the same
public error variant, so the cause alone is not proof that an unvisited
candidate must reject the contract.

The following outcomes are terminal:

- callback failure or exception;
- duplicate/replayed execution;
- cancellation;
- a missing execution prerequisite or frozen-request invariant failure;
- any receipt with `dispatch_count > 0`;
- response, partial, tool, structural-output, or normalization exposure;
- a final typed admission rejection;
- success.

Domain validation occurs only after an OAS success. A caller-side domain
rejection therefore cannot trigger another provider dispatch.

## Evidence

Every terminal result carries:

- the original immutable candidate identity snapshot;
- the monotonic typed candidate-attempt count;
- ordered admission outcomes only for candidates reached so far;
- every non-shared execution receipt allocated for an admitted current
  candidate;
- the exact admission rejection, success, or execution failure for the
  terminal candidate.

After cancellation escapes, the same aggregate evidence remains queryable from
the opaque flow attempt.

The affine executor is the sole progress writer. Concurrent evidence readers
may observe the exact intermediate point after admission is recorded and before
an admitted candidate receives its fresh attempt. Such a snapshot is
point-in-time evidence, not a fabricated attempt or a terminal state.

## Explicit exclusions

- `execute_once` never retries and never changes plans.
- The outer flow performs no weighted, health, cost, or latency routing.
- Pricing is measurement evidence only and is absent from all decisions.
- There is no legacy cascade API, compatibility wrapper, or persisted runtime
  JSON migration.
- Before 1.0, obsolete runtime assets are reset rather than reconciled.

Archived cascade documents describe an earlier ownership decision and are not
the active exact-output contract.
