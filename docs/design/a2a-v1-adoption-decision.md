# A2A v1 Adoption Decision

Status: Draft
Related issues: `#523`, `#521`

## Problem

OAS already exposes an A2A surface, but that surface is still shaped around
legacy discovery and legacy JSON-RPC method names:

- discovery at `/.well-known/agent.json`
- JSON-RPC methods `tasks/send`, `tasks/get`, `tasks/cancel`
- a custom agent-card JSON shape
- task and message wire fields that do not match A2A v1 naming and enum rules

That was acceptable while the protocol was still moving. It is no longer a
good long-term default now that A2A v1 exists and external interop has a
clearer target.

At the same time, `oas` is an OCaml Agent SDK. It should not absorb every
product-level feature from the full A2A specification on day one.

## Decision

OAS will adopt A2A v1 as a **core interop subset**.

The first supported subset is:

- agent discovery and card exposure
- JSON-RPC core task methods
- wire adapters for agent cards, tasks, messages, and message parts

The adoption strategy is:

- keep internal OCaml runtime types as the canonical model
- add A2A v1 compatibility at the protocol boundary
- prefer v1 wire shapes first
- keep legacy fallback paths during the transition

This is a compatibility decision, not a full-spec commitment.

## Why This Belongs In OAS

The SDK should own reusable protocol primitives that consumers should not have
to rebuild:

- agent discovery
- task/message transport
- agent-card publication
- typed protocol adapters

Those are SDK-layer concerns. They fit the same boundary described in
[`AUTONOMY-PRIMITIVES-BOUNDARY.md`](../analysis/AUTONOMY-PRIMITIVES-BOUNDARY.md):
OAS owns reusable runtime and protocol primitives, while consumers own
product-specific orchestration and deployment choices.

## Explicit Non-goals

This decision does not commit OAS to full A2A v1 coverage.

Out of scope for the initial adoption:

- `SendStreamingMessage`
- `ListTasks`
- push notification configuration methods
- authenticated extended agent cards
- signature generation or verification
- full REST binding support
- full gRPC binding support
- strict v1-only cutover with no legacy compatibility

These may be added later, but they are not part of the initial adoption
promise.

## Phase Boundaries

### Phase 1: decision record

- Record that OAS will target A2A v1 core interop.
- Freeze the scope and non-goals before widening the public surface again.

Done condition:
- this document exists and is linked from the A2A backlog item

### Phase 2: core wire RFC

- Specify the exact v1 compatibility subset in implementation terms
- cover discovery, method names, wire enums, part shapes, and compatibility
  fallbacks

Done condition:
- a follow-up RFC defines the v1 core wire contract precisely enough to
  implement without reopening scope questions

### Phase 3: implementation

- implement the RFC against `#523`
- extend HTTP lifecycle coverage under `#521`

Done condition:
- a v1 JSON-RPC peer can complete discovery, send, get, and cancel against OAS

### Phase 4: broader protocol surface

- evaluate streaming, list APIs, push configuration, and extended cards as
  separate RFCs or issues

Done condition:
- each new surface is justified individually instead of being bundled into the
  initial v1 adoption

## Follow-up RFCs and Issues

- `#523`: A2A v1 protocol alignment gap
- `#521`: A2A HTTP lifecycle coverage
- [`agent-surface-hardening-backlog.md`](./agent-surface-hardening-backlog.md):
  backlog-level follow-ups and adjacent RFC candidates

The next design document after this decision record should be a focused
`A2A v1 core wire RFC`, not a full adoption rewrite.

## References

- A2A Protocol latest specification: <https://a2a-protocol.org/latest/specification/>
- A2A Protocol v1.0.0 release: <https://github.com/a2aproject/A2A/releases/tag/v1.0.0>
