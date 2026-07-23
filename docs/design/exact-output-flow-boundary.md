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

## Immutable admission

An outer flow is a nonempty ordered snapshot. OAS admits every candidate and
freezes every successful exact plan before it creates an attempt or performs a
network effect. The ordered admission evidence retains both successes and typed
rejections. A flow can start only when at least one candidate admitted.

Starting a ready flow creates one fresh exact attempt per admitted candidate.
Attempts are never shared between candidates or between two starts of the same
ready flow.

## Execution

`execute_flow_once` is affine. A duplicate or concurrent invocation cannot
dispatch. For each predetermined candidate:

1. The caller's `before_dispatch` callback must confirm its durable binding.
2. OAS invokes the unchanged single-plan `execute_once`.
3. OAS may select the next admitted candidate only for a first-use transport
   failure whose exact receipt is `Before_dispatch` with `dispatch_count = 0`.
4. The caller's `before_advance` callback must durably confirm release before
   OAS enters that already-selected successor.

Callbacks can stop a transition but cannot select, replace, or reorder a
candidate.

The following outcomes are terminal:

- callback failure or exception;
- duplicate/replayed execution;
- cancellation;
- a missing execution prerequisite or frozen-request invariant failure;
- any receipt with `dispatch_count > 0`;
- response, partial, tool, structural-output, or normalization exposure;
- success.

Domain validation occurs only after an OAS success. A caller-side domain
rejection therefore cannot trigger another provider dispatch.

## Evidence

Every terminal result carries:

- the original ordered admission outcomes;
- the immutable candidate identities and plan provenance;
- every non-shared attempt receipt, including unstarted successors;
- the exact success or execution failure for the terminal candidate.

After cancellation escapes, the same aggregate evidence remains queryable from
the opaque flow attempt.

## Explicit exclusions

- `execute_once` never retries and never changes plans.
- The outer flow performs no weighted, health, cost, or latency routing.
- Pricing is measurement evidence only and is absent from all decisions.
- There is no legacy cascade API, compatibility wrapper, or persisted runtime
  JSON migration.
- Before 1.0, obsolete runtime assets are reset rather than reconciled.

Archived cascade documents describe an earlier ownership decision and are not
the active exact-output contract.
