#!/usr/bin/env bash
# check-model-inventory-truth.sh
# Drift gate for the Gemini 3 model inventory (PR Cadd, 2026-04-16).
#
# Guards three invariants that can silently drift:
#   1. capabilities.ml retains the `gemini-3` prefix matcher
#      (without it, gemini-3-* models fall back to default_capabilities).
#   2. model_meta.ml retains the gemini-3 inline %test block
#      (proof that gemini-3 IDs actually resolve to 1M context + tool support).
#   3. Legacy gemini-2.x references in lib/ + test/ do not grow beyond the
#      baseline captured when this gate was introduced.
#
# The baseline is intentionally permissive (equal to the 2026-04-16 count).
# Follow-up PRs Cdeprecate and Cremove will lower it.
#
# Rationale: memory/feedback_policy-runtime-drift-gate.md
#   "정책 선언만으론 부족. 같은 PR에서 runtime 검증 스크립트를 함께 추가."

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
CAPS="$ROOT/lib/llm_provider/capabilities.ml"
META="$ROOT/lib/llm_provider/model_meta.ml"
BASELINE_LEGACY=53

fail=0

# ── Invariant 1: gemini-3 prefix matcher present ────────────────
if ! grep -Fq 'starts_with "gemini-3"' "$CAPS"; then
  echo "FAIL: gemini-3 prefix matcher missing in $CAPS" >&2
  fail=1
fi

# ── Invariant 2: gemini-3 inline tests present ──────────────────
for id in \
    "gemini-3-flash-preview" \
    "gemini-3.1-pro-preview" \
    "gemini-3.1-flash-lite-preview"; do
  if ! grep -Fq "$id" "$META"; then
    echo "FAIL: inline %test for $id missing in $META" >&2
    fail=1
  fi
done

# ── Invariant 3: legacy gemini-2.x count bounded ────────────────
legacy=$(rg -n 'gemini-2\.[05]' "$ROOT/lib" "$ROOT/test" 2>/dev/null | wc -l | tr -d ' ')
if [ "$legacy" -gt "$BASELINE_LEGACY" ]; then
  echo "FAIL: legacy gemini-2.x references grew ($legacy > baseline $BASELINE_LEGACY)" >&2
  echo "  Run: rg -n 'gemini-2\\.[05]' lib/ test/" >&2
  fail=1
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi

echo "OK: gemini-3 matcher present, inline tests present, legacy count $legacy ≤ baseline $BASELINE_LEGACY"
