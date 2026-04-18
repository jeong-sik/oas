# OAS coordination-leak audit — 2026-04-17

> Read-only audit. Each row is a candidate that may belong outside OAS.
> Methodology: Explore subagent walked `lib/`, `bin/`, `docs/rfc/`,
> and the `agent_sdk.opam` `depends` field, then cross-checked PR #976
> (which removed the named-coordinator vocabulary) for regressions.
> Disposition is advisory; actual delete/move PRs are tracked separately.

## Summary

OAS is structurally clean. The single-process / single-Eio-domain
invariant holds: no multi-process locks, no durable cross-process
state stores, no operator dashboards, no task queues spanning agents.
PR #976 removed the named-coordinator vocabulary from docs and
comments; no regression detected here.

Three low-risk candidates remain — all observability or example code,
not core runtime. They are listed below for follow-up consideration,
not as urgent removals.

## Leak candidates

| path | symbol/section | reason | action | risk |
|------|----------------|--------|--------|------|
| `bin/autonomy_smoke_cli.ml` | entire entry | Operator-facing diagnostics: analyzes agent autonomy / divergence across multiple runs. Not part of the agent runtime contract. | move-to-consumer | low |
| `lib/autonomy_trace_analyzer.ml` | entire module | Classifies agent behavior (`Autonomous` / `Scripted` / `Random`) from metrics. That's a policy/judgement, not a runtime primitive — belongs in whoever consumes the traces. | move-to-consumer | low |
| `bin/review_agent.ml` | entire entry | Example PR-review agent. Pulls from the GitHub API and posts comments. Useful as a demo, but `bin/` is normally for SDK-side runtime entry points, not example apps. | move-to-`examples/` or delete | low |

## Notes

### Already-clean areas (don't re-audit next time)

- `lib/agent/` — single-agent lifecycle, turns, tools. No cross-process state.
- `lib/sessions_store.ml` — file I/O only; no database, no multi-process semantics.
- `lib/runtime_server.ml` — stdin/stdout protocol, decoupled from any specific consumer.
- `lib/llm_provider/` — provider selection / failover policy is the consumer's responsibility (correctly externalized).
- `lib/checkpoint*.ml` — single-agent recovery; no durable cross-process semantics.

### False-positive scan list (looked suspicious, actually fine)

- `Durable_event` — single-agent crash-recovery journal, not coordinator state.
- `Runtime.session` — file-based artifacts only; no multi-process state machine.
- `Harness_runner` — single-run evaluation; not a coordinator test queue.
- `Raw_trace`, `Autonomy_trace_analyzer` (analyzer is observability, classifier portion of it is the leak — see table above).

### Regression check vs PR #976

PR #976 removed `MASC` (and similar named-coordinator) tokens from
seven docstrings and one comment, plus moved
`docs/design/cascade-boundary-analysis.md` to `docs/archive/2026-04/`.
A fresh `rg -in masc lib/ bin/ README.md` returns zero hits
on this branch. No regression.
