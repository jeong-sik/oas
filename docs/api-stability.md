# API Stability Tiers

OAS (OCaml Agent SDK) modules are classified into three stability tiers.
Each `.mli` file carries a `@stability` annotation in its top-level doc comment.

## Tiers

| Tier | Promise | Breaking change policy |
|------|---------|----------------------|
| **Stable** | Backward-compatible within a major version. | Requires major version bump + migration guide. |
| **Evolving** | May change with deprecation notice. | One minor-version deprecation window before removal. |
| **Internal** | Implementation detail. No stability promise. | May change in any release without notice. |

## Classification criteria

### Stable

- Types and functions that downstream consumers (external coordinators, embedding agents) import directly.
- Core abstractions: `Types`, `Error`, `Agent`, `Builder`, `Tool`, `Provider`, `Hooks`.
- Breaking a Stable module breaks all consumers.

### Evolving

- Modules under active development with external consumers.
- New features (autonomy, CDAL, harness) that have not yet settled.
- Swarm orchestration modules whose API is still being refined.

### Internal

- Modules not intended for direct external use.
- LLM provider backends, protocol internals, parse/serialize helpers.
- Sub-modules of agent/ that are implementation details of Agent.

## Annotation format

Every `.mli` file has a top-level doc comment with the `@stability` tag:

```ocaml
(** Context management for agent conversations.

    @stability Stable
    @since 0.93.1 *)
```

Rules:
- One `@stability` tag per `.mli` file, at the top-level module doc comment.
- `@since` indicates the version when the stability tier was assigned (not when the module was created).
- Modules without `@stability` are treated as **Internal** by default.
- Promotion (Internal -> Evolving -> Stable) does not require deprecation.
- Demotion (Stable -> Evolving) requires one minor version of deprecation.

## Current classification

As of v0.93.1, all 186 `.mli` files in `lib/` and `lib_swarm/` carry an
explicit stability tier:

- Stable: 14
- Evolving: 94
- Internal: 78

### Stable (14 modules)

Core types and interfaces that downstream consumers depend on.

| Module | File |
|--------|------|
| Types | `lib/types.mli` |
| Error | `lib/error.mli` |
| Agent | `lib/agent/agent.mli` |
| Builder | `lib/agent/builder.mli` |
| Tool | `lib/tool.mli` |
| Tool_set | `lib/tool_set.mli` |
| Hooks | `lib/hooks.mli` |
| Provider | `lib/provider.mli` |
| Guardrails | `lib/guardrails.mli` |
| Raw_trace | `lib/raw_trace.mli` |
| Checkpoint | `lib/checkpoint.mli` |
| Checkpoint_store | `lib/checkpoint_store.mli` |
| Context | `lib/context.mli` |
| Context_reducer | `lib/context_reducer.mli` |

### Evolving (94 modules)

Public modules with downstream consumers that are still settling. This tier
includes most top-level SDK surfaces outside the stable core and selected swarm
interfaces.

Representative modules:

| Module | File | Reason |
|--------|------|--------|
| Collaboration | `lib/collaboration.mli` | Shared collaboration state is still evolving |
| Memory | `lib/memory.mli` | Memory system is under active development |
| Policy | `lib/policy.mli` | Rule engine surface is still settling |
| Proof_store | `lib/proof_store.mli` | CDAL storage API is newly added |
| Swarm_types | `lib_swarm/swarm_types.mli` | Swarm API remains consumer-facing but not stable |
| Runner | `lib_swarm/runner.mli` | Orchestration behavior is still being refined |

### Internal (78 modules)

Implementation-detail modules with no compatibility promise. Most entries in
this tier live under internal subdirectories such as `lib/agent/`,
`lib/protocol/`, and `lib/llm_provider/`, plus parser/transport helpers that
external consumers should not depend on directly.

## Verification

```bash
# Count annotated files (should equal total .mli count)
rg '@stability' lib/ lib_swarm/ --glob '*.mli' -c | wc -l

# List by tier
rg '@stability Stable' lib/ lib_swarm/ --glob '*.mli' -l
rg '@stability Evolving' lib/ lib_swarm/ --glob '*.mli' -l
rg '@stability Internal' lib/ lib_swarm/ --glob '*.mli' -l
```
