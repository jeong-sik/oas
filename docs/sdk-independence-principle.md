# SDK Independence Principle

## Rule

OAS (OCaml Agent SDK) does not contain domain vocabulary from any specific
downstream coordinator. The dependency direction is strictly:

```
MCP Protocol SDK  <--  OAS (Agent SDK)  <--  Coordinator (e.g. MASC)
```

OAS provides generic agent primitives (execution modes, risk contracts, proof
bundles, intent classification). Coordinators consume these primitives and add
their own orchestration semantics.

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

## Module ownership

| Module | Owner | Consumers |
|--------|-------|-----------|
| `Context_intent` | OAS | MASC (via `Oas_worker`) |
| `Risk_contract`, `Cdal_proof` | OAS | MASC (`Keeper_cdal_contract`) |
| `Execution_mode`, `Risk_class` | OAS | MASC (`Keeper_cdal_contract`) |
| `Mode_enforcer`, `Proof_capture` | OAS | MASC (via hooks) |
| `Keeper_cdal_contract`, `Cdal_judge` | MASC | -- |

## Enforcement

- CI: `grep -rn` for a maintained blocklist of coordinator terms in `lib/`.
- Code review: any new keyword in `context_intent.ml` must be justified as
  domain-neutral.

## History

- 2026-03-29: Initial principle established. Removed "delegate", "handoff",
  "agent", "team" from `context_intent.ml` heuristic keywords. Replaced with
  "route", "transfer", "actor". Issue: jeong-sik/masc-mcp#3588.
