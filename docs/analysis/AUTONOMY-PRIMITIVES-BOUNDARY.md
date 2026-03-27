# OAS Autonomy Primitives Boundary

## Why These Modules Belong in OAS

`oas` is an OCaml Agent SDK, not an AutoResearch product. The SDK can still
own the reusable building blocks that autonomy-oriented consumers need:

- argv-only external execution
- diff validation against explicit file boundaries
- strict metric output contracts
- memory-backed lesson retrieval for retry loops

These are consumer-agnostic primitives. They do not assume:

- a specific research loop
- a git ratchet policy
- a reviewer agent topology
- a long-running overnight orchestrator

## Why the Full Runner Stays Outside OAS

The full AutoResearch loop still belongs to the consumer layer because it
requires product-level choices that the SDK should not freeze:

- when to patch, revert, or checkpoint
- how many coder/reviewer/judge agents to run
- what counts as reward hacking
- which sandbox wrapper to use in production
- which memory records should become merge gates or reputation signals

Keeping the orchestration outside OAS preserves the existing boundary:

- OAS exposes typed execution/runtime primitives
- consumers compose those primitives into domain-specific workflows

## Composition Pattern

The intended shape is:

1. Use `Autonomy_exec` to run an isolated evaluator or code mutation command.
2. Parse the resulting metric with `Metric_contract`.
3. Validate candidate diffs with `Autonomy_diff_guard`.
4. On failure, store the lesson with `Lesson_memory`.
5. On the next attempt, inject retrieved lesson text via
   `Hooks.BeforeTurnParams.extra_system_context` or `Append_instruction`.

That gives consumers a safe autonomy substrate without changing `Agent`,
`Builder`, or `Swarm` semantics.
