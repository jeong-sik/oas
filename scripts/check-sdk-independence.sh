#!/usr/bin/env bash
# Guard OAS against coordinator-specific vocabulary in public/runtime surfaces.
#
# Tiers:
#   strict  (default): lib/, bin/, README.md — any match fails the build.
#   warn    (--include-tests): also scan test/. Matches are reported but exit 0.
#                              Use --strict-tests to promote warn-tier matches to failures.
#
# Permanently excluded: docs/archive/, CHANGELOG.md, docs/rfc/, examples/
# (history records and design RFCs may legitimately mention prior coordinator names).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

include_tests=0
strict_tests=0
for arg in "$@"; do
  case "$arg" in
    --include-tests) include_tests=1 ;;
    --strict-tests)  include_tests=1; strict_tests=1 ;;
    -h|--help)
      sed -n '1,12p' "$0"
      exit 0
      ;;
    *)
      echo "unknown flag: $arg" >&2
      exit 2
      ;;
  esac
done

if ! command -v rg >/dev/null 2>&1; then
  echo "SDK independence check failed: ripgrep (rg) is required" >&2
  exit 1
fi

STRICT_TARGETS=(lib bin README.md)
WARN_TARGETS=(test)

# Strict tier: full coordinator vocabulary (lib/, bin/, README.md).
strict_patterns=(
  '\bmasc\b'
  'masc_'
  '\bkeeper\b'
  'keeper_'
  '\bboard\b'
  'board_'
  '\broom\b'
  'room_'
)

# Warn tier: narrowed to high-signal terms only.
# `room` and `board` collide with common test vocabulary (schema parameter
# names, JSON keys, "no room", "scoreboard", etc.) and produce too many
# false positives in test/. Keep them strict-only.
warn_patterns=(
  '\bmasc\b'
  'masc_'
  '\bkeeper\b'
  'keeper_'
)

# Filter out:
#   - lines explicitly tagged with `boundary-allow` (intentional historical references)
#   - OCaml comment lines that start with "(*", "*", or "(**"  (left-trimmed)
# Caveat: this is a line-level heuristic, not a full OCaml lexer. A code line
# containing an inline `(* ... *)` whose pattern sits inside the comment may
# slip through. Use `boundary-allow` for those edge cases.
filter_noise() {
  awk -F':' '
    {
      idx = index($0, ":")
      rest = substr($0, idx + 1)
      idx2 = index(rest, ":")
      content = substr(rest, idx2 + 1)
      # left-trim
      sub(/^[[:space:]]+/, "", content)
      # OCaml comment heuristics
      if (content ~ /^\*/) next
      if (content ~ /^\(\*/) next
      # explicit allow marker
      if ($0 ~ /boundary-allow/) next
      print $0
    }
  '
}

scan_tier() {
  local tier="$1"; shift
  local fail_on_match="$1"; shift
  # bash 3.2 (macOS default) lacks `local -n` nameref. Use indirect array
  # expansion via eval to read the patterns array by name.
  local patterns_var="$1"; shift
  local targets=("$@")
  local patterns_arr=()
  eval "patterns_arr=( \"\${${patterns_var}[@]}\" )"
  local tier_fail=0
  for pattern in "${patterns_arr[@]}"; do
    local matches
    matches="$(rg -n -i -e "$pattern" "${targets[@]}" 2>/dev/null | filter_noise || true)"
    if [[ -n "$matches" ]]; then
      if [[ "$fail_on_match" -eq 1 ]]; then
        echo "FAIL [$tier]: coordinator-specific term matched pattern: $pattern" >&2
      else
        echo "WARN [$tier]: coordinator-specific term matched pattern: $pattern" >&2
      fi
      echo "$matches" >&2
      tier_fail=1
    fi
  done
  return "$tier_fail"
}

overall_fail=0
if ! scan_tier "strict" 1 strict_patterns "${STRICT_TARGETS[@]}"; then
  overall_fail=1
fi

if [[ "$include_tests" -eq 1 ]]; then
  if ! scan_tier "test" "$strict_tests" warn_patterns "${WARN_TARGETS[@]}"; then
    if [[ "$strict_tests" -eq 1 ]]; then
      overall_fail=1
    fi
  fi
fi

if [[ "$overall_fail" -ne 0 ]]; then
  echo "SDK independence check failed" >&2
  exit 1
fi

echo "OK: SDK independence check passed"
