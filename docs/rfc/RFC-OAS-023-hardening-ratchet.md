# RFC-OAS-023: Production Hardening Ratchet

**Status**: Draft  
**Date**: 2026-06-27  
**Scope**: `.ci/hardening-baseline.json`, `scripts/hardening-ratchet.sh`, `.github/workflows/hardening-ratchet.yml`  
**One sentence**: Lock six production-hardening metrics against a stored baseline so that PRs can only decrease or hold the counts; increases are reported but do not block merge while the detector remains regex-based.

## Related Documents

- `docs/rfc/RFC-OAS-022-code-smell-monotone-decrease-ratchet.md` — sibling monotone-decrease ratchet; this RFC mirrors its interface (`--measure | --check | --rebaseline`) and waiver convention.
- `lib/llm_provider/http_client.ml` — contains the `classify_by_message` routine tracked by the `exception_message_classifiers` metric.

## 0. Summary

OAS runtime source currently carries several classes of production-hardening debt:

1. **local_workspace_path_literals** — string literals baking a developer workspace path into runtime source.
2. **direct_env_reads** — direct `Sys/Unix.getenv*` calls.
3. **direct_env_reads_outside_env_boundary** — direct `Sys/Unix.getenv*` calls outside config/env boundary modules.
4. **exception_message_classifiers** — exception-message substring classification such as `classify_by_message` / `has_substr`.
5. **stub_markers** — runtime stubs (`Not_implemented`, `failwith "...TODO..."`, etc.).
6. **wildcard_silent_defaults** — catch-all arms that collapse to permissive defaults.

This RFC introduces a baseline + monotone-decrease ratchet. PRs may *decrease* or *hold* any metric; a PR that *increases* any metric is flagged by CI but is **not** blocked while the detector uses line-oriented analysis. The workflow becomes required only after the detector graduates to AST/parsetree-based analysis.

## 1. Problem

Production-hardening debt tends to regress because the patterns above are easy to add and hard to audit:

- A new provider adds another `Sys.getenv_opt` in business logic.
- A retry loop adds another `has_substr msg "..."` classification instead of using typed errors.
- A catch-all arm silently absorbs a new variant.

Existing `dune -w +8` strict warnings and the code-smell ratchet do not cover these shapes. A dedicated ratchet gives each anti-pattern a visible, monotonically decreasing count.

## 2. Decision

Adopt a 6-metric monotone-decrease ratchet wired into CI as an **advisory** check.

- Baseline file: `.ci/hardening-baseline.json`.
- Gate script: `scripts/hardening-ratchet.sh` with three modes (`--measure | --check | --rebaseline`).
- Workflow: `.github/workflows/hardening-ratchet.yml` runs on `pull_request` and reports deltas without failing the merge.
- Escape hatch: `RATCHET-WAIVED: <reason>` line in PR body is recognized for consistency with RFC-OAS-022, though the check is currently advisory.

### Why advisory?

The current detector is line-oriented regex analysis with known limitations (see §9). Making it required would risk blocking legitimate code. It graduates to required only after:

- The scanner uses `compiler-libs`/parsetree/cmt or equivalent AST analysis.
- A stable true/false-positive test corpus is committed.
- The waiver path is exercised in at least one PR.

## 3. Measurement

The script `scripts/hardening-ratchet.sh` measures tracked `.ml`/`.mli` files under `lib/`, `bin/`, and `src/` (excluding paths containing `test`, `tests`, `fixture`, `fixtures`, `example`, or `examples`).

### 3.1 Metrics

| metric | what is counted |
|--------|-----------------|
| `local_workspace_path_literals` | string literals containing the repository root or `$HOME` |
| `direct_env_reads` | `Sys/Unix.getenv*` call sites |
| `direct_env_reads_outside_env_boundary` | `Sys/Unix.getenv*` call sites outside env/config boundary modules |
| `exception_message_classifiers` | `classify_by_message` definitions and `has_substr` call sites on message-like variables |
| `stub_markers` | `Not_implemented` and `failwith "...not implemented/TODO/stub..."` |
| `wildcard_silent_defaults` | line-leading `\| _ -> Ok/None/[]/()/true/false/""` arms |

### 3.2 Explicit exclusions

- `assert false` is **not** counted as a stub marker. It is the standard OCaml idiom for exhaustiveness proofs on impossible GADT/variant branches.
- Typed error-label serializers (e.g. `\| Http_client.Timeout -> "timeout"`) are **not** counted as exception-message classifiers.
- Workspace roots are derived from `git rev-parse --show-toplevel` and `$HOME`; no macOS- or repo-name-specific paths are baked into the detector.

### 3.3 Commands

```bash
scripts/hardening-ratchet.sh --measure     # print current JSON, no compare
scripts/hardening-ratchet.sh --check       # compare current vs baseline; exit 0 with report
scripts/hardening-ratchet.sh --rebaseline  # main-only: write current counts to baseline JSON
```

## 4. Baseline

Measured at the PR branch tip introducing this RFC.

| metric | count | removal target |
|--------|-------|----------------|
| local_workspace_path_literals | 0 | keep at 0 |
| direct_env_reads | TBD | RFC-OAS-024 or later (centralize env access) |
| direct_env_reads_outside_env_boundary | TBD | RFC-OAS-024 or later |
| exception_message_classifiers | TBD | 0 after oas#2174 merges and `classify_by_message` is removed |
| stub_markers | TBD | 0 after unreachable `assert false` arms are verified separately |
| wildcard_silent_defaults | TBD | audit each site; no blanket target |

The baseline is regenerated with `scripts/hardening-ratchet.sh --rebaseline` only on `main` (or with `ALLOW_REBASELINE_OFF_MAIN=1` for local testing).

## 5. Workflow Semantics

| Event | Behavior |
|-------|----------|
| `pull_request` | Run `--check`. Report deltas. Exit successfully so the check is advisory. |
| `push:main` | Not triggered. The ratchet is advisory until the detector is AST-based. |

## 6. Escape Hatch & Sunset Criteria

Escape hatch (for consistency with RFC-OAS-022):

```
RATCHET-WAIVED: <reason, e.g. "vendored upstream patch, new provider import">
```

Because the check is advisory, the waiver is informational today.

Sunset criteria for the advisory state:

- Detector rewritten to use `compiler-libs`/AST/cmt analysis.
- Committed true/false-positive test corpus.
- At least one intentional waiver exercised.

After these are met, the workflow can be made required with the same waiver semantics as RFC-OAS-022.

## 7. Non-Goals

- This RFC does not replace typed-error refactoring (oas#2174).
- It does not remove existing catch-all arms; it only prevents growth.
- It does not back-fill all env reads into a central config module.

## 8. Risks & Mitigations

- **Regex-based analysis is brittle.** Mitigated by making the check advisory and by excluding known false-positive shapes (`assert false`, typed serializers).
- **Baseline drift on legitimate refactors.** Mitigated by `RATCHET-WAIVED` and the advisory status; rebaseline only on `main`.
- **False sense of safety.** The ratchet only tracks count, not semantics. It is a pressure metric, not a proof.

## 9. Known Limitations

The current detector strips OCaml comments with a hand-rolled state machine and matches syntax with regexes. It does not fully handle:

- `(*` / `*)` inside string literals.
- Nested comments.
- Multi-line strings.

These limitations are acceptable while the check is advisory, and the detector must be replaced with AST-based analysis before graduation.

## 10. Compatibility with RFC-OAS-022

This RFC intentionally reuses the interface and conventions of RFC-OAS-022 (`--measure | --check | --rebaseline`, `RATCHET-WAIVED`, main-only rebaseline). The metrics are distinct because hardening debt has different signatures than general code smell.

Future work may extract a shared ratchet driver from the two scripts once both are stable.

## 11. Acceptance

- [x] `.ci/hardening-baseline.json` committed with current counts.
- [x] `scripts/hardening-ratchet.sh` with `--measure | --check | --rebaseline`.
- [x] `.github/workflows/hardening-ratchet.yml` wired to `pull_request` as advisory.
- [x] RFC-OAS-023 documenting metrics, waiver policy, and removal targets.
- [ ] Detector rewritten to AST/parsetree-based analysis (required before making the check required).
