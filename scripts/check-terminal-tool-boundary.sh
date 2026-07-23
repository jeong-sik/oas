#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PLANNER="$ROOT/lib/agent/agent_tool_batch_plan.ml"

fail() {
  printf 'terminal-tool boundary violation: %s\n' "$1" >&2
  exit 1
}

if rg -n -i \
  'provider|model|tier|pricing|price|parallel[_ -]?tool|parallel[_ -]?call' \
  "$PLANNER"; then
  fail "the admission planner must remain provider, pricing, and parallel-flag neutral"
fi

if rg -n \
  'String\.(equal|starts_with|ends_with)|Str\.|Re\.' \
  "$PLANNER"; then
  fail "the admission planner must not infer completion from tool-name strings"
fi

if rg -n \
  '(String\.(equal|starts_with|ends_with).*(terminal|Terminal)|(terminal|Terminal).*String\.(equal|starts_with|ends_with)|terminal.*parallel|parallel.*terminal)' \
  "$ROOT/lib/agent" "$ROOT/lib/pipeline"; then
  fail "terminal control flow must use typed metadata only"
fi

printf 'terminal-tool boundary: OK\n'
