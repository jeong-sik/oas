#!/usr/bin/env bash
# RFC-OAS-009 v2 PR-D — Core→CDAL boundary lint.
#
# Forbids OAS core (lib/agent/, lib/llm_provider/, lib/protocol/, lib/base/)
# from referencing CDAL modules (gold-standard self-tagged "Part of CDAL PoC-1"
# in their .mli plus the modules that depend on them).
#
# This script is the durable mechanism that prevents the layering violation
# RFC-OAS-009 v2 closed (PR-B #1481, PR-C #1482) from reappearing. Future
# PRs that introduce a Mode_enforcer/Cdal_proof/Risk_contract reference into
# core will trip this lint at CI time.
#
# Usage:
#   bash scripts/lint-core-cdal-boundary.sh
#
# Exit:
#   0 = boundary clean.
#   1 = at least one CDAL reference found in core.
#
# Tools: `rg` (ripgrep). Falls back to `grep -r` if rg is unavailable.

set -euo pipefail

# Resolve to repo root regardless of caller's cwd.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

# Core directories that must not reference CDAL.
CORE_DIRS=(
  lib/agent
  lib/llm_provider
  lib/protocol
  lib/base
)

# CDAL module prefixes to forbid in core.
#
# Original gold-standard set (RFC-OAS-009 v2 PR-D, 9 prefixes): the seven mli
# files that self-tag "Part of CDAL PoC-1" plus their direct dependents
# (mode_enforcer, contract_runner).
#
# Expanded set (RFC-OAS-011 OAS-E PR-1, +11 prefixes): the "Implied CDAL"
# modules that RFC-OAS-009 v2 §1.1.2 listed but the lint did NOT cover.
# Their omission allowed core → CDAL leakage that surfaced during
# OAS-E inventory:
#   - lib/agent/agent_turn.ml comment containing "Audit"      → diagnostic log
#   - lib/pipeline/pipeline.ml using Guardrails_async         → reclassified core
#   - lib/runtime_server_worker.ml using Runtime_evidence     → 15 call sites
#   - lib/sessions.mli include of Sessions_proof              → signature surface
#   - lib/execution_manifest.mli using Execution_mode/Risk_class → field types
#
# Reclassification (Guardrails are SDK core, not CDAL):
# Guardrail_llm / Guardrail_tripwire / Guardrails_async are part of the
# agent runtime surface (Anthropic-style safety hooks integral to the
# turn pipeline) — agent_types/builder both depend on Guardrails_async.t
# as a record field. They were over-classified during the MM-2 migration;
# the cdal_runtime copies are now redundant artifacts to be retired in
# RFC-OAS-013. Excluded from this lint pattern.
CDAL_PATTERN='\b(Cdal_proof|Mode_enforcer|Mode_resolver|Risk_contract|Risk_class|Execution_mode|Proof_capture|Proof_store|Contract_runner|Effect_evidence|Verified_output|Conformance|Cognitive_event|Direct_evidence|Audit|Autonomy_exec|Autonomy_diff_guard|Autonomy_trace_analyzer|Sessions_proof|Runtime_evidence)\b'

# Pick the search tool.
if command -v rg >/dev/null 2>&1; then
  search() { rg --no-heading -n -- "$CDAL_PATTERN" "$@"; }
else
  # Fallback: grep -E with same pattern, recursive.
  search() { grep -RnE -- "$CDAL_PATTERN" "$@" 2>/dev/null || true; }
fi

# Filter to only existing core dirs (defensive against future moves).
existing_dirs=()
for d in "${CORE_DIRS[@]}"; do
  [ -d "$d" ] && existing_dirs+=("$d")
done

if [ "${#existing_dirs[@]}" -eq 0 ]; then
  echo "lint-core-cdal-boundary: none of the configured core directories exist." >&2
  echo "  Configured: ${CORE_DIRS[*]}" >&2
  exit 1
fi

# Search and capture matches.
matches="$(search "${existing_dirs[@]}" || true)"

if [ -z "$matches" ]; then
  echo "✓ lint-core-cdal-boundary: no CDAL references in OAS core."
  echo "  Scanned: ${existing_dirs[*]}"
  echo "  Forbidden: Cdal_proof | Mode_enforcer | Mode_resolver | Risk_contract"
  echo "             | Risk_class | Execution_mode | Proof_capture | Proof_store"
  echo "             | Contract_runner | Effect_evidence | Verified_output"
  echo "             | Conformance | Cognitive_event | Direct_evidence | Audit"
  echo "             | Autonomy_exec | Autonomy_diff_guard"
  echo "             | Autonomy_trace_analyzer | Sessions_proof | Runtime_evidence"
  exit 0
fi

cat >&2 <<EOF
✗ lint-core-cdal-boundary: OAS core references CDAL modules.

RFC-OAS-009 v2 (Sever Core→CDAL Dependencies) forbids reverse dependencies
from agent_sdk core (lib/agent, lib/llm_provider, lib/protocol, lib/base)
into CDAL modules. The two original violations were:

  - lib/agent/agent_tools.ml:68     (removed in PR-B #1481, 39082f60)
  - lib/protocol/mcp_schema.ml:63   (removed in PR-C #1482, ffb8aff3)

After OAS-D this lint blocks new violations at CI time.

If your change needs CDAL-style classification on a Tool, supply
Tool.descriptor.mutation_class at construction (see
lib/contract_runner.ml:96-110 for the consumer-supplied pattern).

Per RFC-OAS-011 the CDAL modules will migrate to masc-mcp.cdal —
add new CDAL functionality there, not in OAS lib.

Found references:

EOF

echo "$matches" >&2
exit 1
