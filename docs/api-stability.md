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

- Types and functions that downstream consumers (MASC, external agents) import directly.
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
    @since 0.93.0 *)
```

Rules:
- One `@stability` tag per `.mli` file, at the top-level module doc comment.
- `@since` indicates the version when the stability tier was assigned (not when the module was created).
- Modules without `@stability` are treated as **Internal** by default.
- Promotion (Internal -> Evolving -> Stable) does not require deprecation.
- Demotion (Stable -> Evolving) requires one minor version of deprecation.

## Current classification

As of v0.93.1, MASC imports 32 OAS modules. Classification:

### Stable (14 modules)

Core types and interfaces that MASC and external consumers depend on.

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

### Evolving (18 modules)

Actively developed modules with external consumers. API may change.

| Module | File | Reason |
|--------|------|--------|
| Collaboration | `lib/collaboration.mli` | Swarm collaboration, actively evolving |
| Memory | `lib/memory.mli` | Memory system under development |
| Lesson_memory | `lib/lesson_memory.mli` | Lesson extraction, new feature |
| Event_bus | `lib/event_bus.mli` | Event routing, evolving |
| Execution_mode | `lib/execution_mode.mli` | Execution modes, evolving |
| Harness | `lib/harness.mli` | Testing harness, evolving |
| Policy | `lib/policy.mli` | Policy system, evolving |
| Swarm_types | `lib_swarm/swarm_types.mli` | Swarm types, evolving |
| Runner | `lib_swarm/runner.mli` | Swarm runner, evolving |
| Autonomy_diff_guard | `lib/autonomy_diff_guard.mli` | Autonomy, new feature |
| Autonomy_exec | `lib/autonomy_exec.mli` | Autonomy, new feature |
| Cdal_proof | `lib/cdal_proof.mli` | CDAL proof system, new |
| Metric_contract | `lib/metric_contract.mli` | Metric contracts, new |
| Proof_store | `lib/proof_store.mli` | Proof storage, new |
| Risk_class | `lib/risk_class.mli` | Risk classification, new |
| Risk_contract | `lib/risk_contract.mli` | Risk contracts, new |
| Agent_checkpoint | `lib/agent/agent_checkpoint.mli` | Agent checkpoint, evolving |
| Consumer | `lib/consumer.mli` | Consumer bridge, evolving |

### Internal (154 modules)

All remaining `.mli` files. Implementation details of the SDK.
Consumers should use Stable or Evolving modules instead.

## Verification

```bash
# Count annotated files (should equal total .mli count)
rg '@stability' lib/ lib_swarm/ --glob '*.mli' -c | wc -l

# List by tier
rg '@stability Stable' lib/ lib_swarm/ --glob '*.mli' -l
rg '@stability Evolving' lib/ lib_swarm/ --glob '*.mli' -l
rg '@stability Internal' lib/ lib_swarm/ --glob '*.mli' -l
```
