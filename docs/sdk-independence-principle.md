# SDK Independence Principle

## Rule

OAS (OCaml Agent SDK) does not contain domain vocabulary from any specific
downstream coordinator. The dependency direction is strictly:

```
MCP Protocol SDK  <--  OAS (Agent SDK)  <--  External coordinator
```

OAS provides generic agent primitives (execution modes, risk contracts, proof
bundles, intent classification). Coordinators consume these primitives and add
their own orchestration semantics. OAS is not allowed to name, depend on, or
adapt to any specific coordinator.

## What this means in practice

1. **No coordinator keywords in heuristic classifiers.** Terms like "keeper",
   "room", "broadcast", or coordinator-specific role names must not appear in
   OAS keyword lists or prompt templates.

2. **No coordinator types imported.** OAS must never depend on a coordinator's
   opam package or reference coordinator-specific module names.

3. **Generic vocabulary only.** Where OAS needs coordination-related keywords
   (e.g. for intent classification), use domain-neutral terms: "assign",
   "route", "transfer", "coordinate", "sync", "notify", "actor", "group".

4. **Coordinator-specific aliases live downstream.** If a coordinator needs
   "handoff" to map to `Coordination` intent, it should wrap
   `Context_intent.intent_of_string` in its own adapter.

## Module ownership (this repository only)

This table is restricted to OAS-owned modules. Whatever a downstream
coordinator wraps these in is intentionally outside this document.

| Module | Owner |
|--------|-------|
| `Context_intent` | OAS |
| `Risk_contract`, `Cdal_proof` | OAS |
| `Execution_mode`, `Risk_class` | OAS |
| `Mode_enforcer`, `Proof_capture` | OAS |

If a coordinator builds its own contract module (for example a CDAL contract
or a CDAL judge), that module belongs to the coordinator's repository, not
to OAS, and is not tracked here.

## Enforcement

- CI: `grep -rn` for a maintained blocklist of coordinator-specific terms in `lib/`.
- Code review: any new keyword in `context_intent.ml` must be justified as
  domain-neutral.
- README and top-level docs must not mention any specific external coordinator
  by name.

## History

- 2026-03-29: Initial principle established. Removed "delegate", "handoff",
  "agent", "team" from `context_intent.ml` heuristic keywords. Replaced with
  "route", "transfer", "actor".
- 2026-04-17: Tightened. Owner/consumers table no longer names downstream
  coordinators; OAS docs do not depend on any specific consumer.
