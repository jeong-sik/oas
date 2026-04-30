#!/usr/bin/env bash
# Guard OAS against coordinator-specific vocabulary in public/runtime surfaces.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

TARGETS=(lib bin README.md)

if ! command -v rg >/dev/null 2>&1; then
  echo "SDK independence check failed: ripgrep (rg) is required" >&2
  exit 1
fi

patterns=(
  '\bmasc\b'
  'masc_'
  '\bkeeper\b'
  'keeper_'
  '\bboard\b'
  'board_'
  '\broom\b'
  'room_'
)

fail=0
for pattern in "${patterns[@]}"; do
  matches="$(rg -n -i -e "$pattern" "${TARGETS[@]}" || true)"
  if [[ -n "$matches" ]]; then
    echo "FAIL: coordinator-specific term matched pattern: $pattern" >&2
    echo "$matches" >&2
    fail=1
  fi
done

if [[ "$fail" -ne 0 ]]; then
  echo "SDK independence check failed" >&2
  exit 1
fi

echo "OK: SDK independence check passed"
