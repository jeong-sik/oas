#!/usr/bin/env bash
set -euo pipefail

SOURCE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
ROOT="${TERMINAL_BOUNDARY_ROOT:-$SOURCE_ROOT}"
PLANNER="$ROOT/lib/agent/agent_tool_batch_plan.ml"
BOUNDARY="$ROOT/lib/agent/agent_tool_terminal_boundary.ml"
PIPELINE="$ROOT/lib/pipeline/pipeline_terminal_tool.ml"
TOOL_MLI="$ROOT/lib/base/tool.mli"
EVENT="$ROOT/lib/execution_event.ml"

fail() {
  printf 'terminal-tool boundary violation: %s\n' "$1" >&2
  exit 1
}

require_pattern() {
  local pattern="$1"
  local file="$2"
  local detail="$3"
  rg -q --multiline "$pattern" "$file" || fail "$detail"
}

check_boundary() {
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

  require_pattern \
    'Terminal_after_success of failure_effect_disposition' \
    "$TOOL_MLI" \
    "terminal completion and failure-effect disposition must remain one closed value"
  require_pattern \
    'terminal_descriptor : failure_effect_disposition -> descriptor' \
    "$TOOL_MLI" \
    "terminal descriptors must require an explicit failure-effect disposition"
  require_pattern \
    'completion:completion[[:space:]]*->[[:space:]]*t' \
    "$TOOL_MLI" \
    "immutable invocations must require completion at construction"
  require_pattern \
    '; completion : Tool\.completion' \
    "$EVENT" \
    "Tool_invocation events must persist completion"
  require_pattern \
    'required:\[[^]]*"completion"' \
    "$EVENT" \
    "the current event decoder must require persisted completion"
  require_pattern \
    'schema_version_current = 2' \
    "$EVENT" \
    "execution events must reject pre-completion schema versions"
  require_pattern \
    'Tool\.Invocation\.completion invocation' \
    "$BOUNDARY" \
    "durable terminal recovery must use persisted invocation completion"

  if rg -n 'Tool_set|find_tool|Tool\.completion[[:space:]]' "$PIPELINE"; then
    fail "pipeline recovery must not reconstruct completion from the current tool catalog"
  fi

  if rg -n 'find_tool|Tool_set|tool_name.*completion|completion.*tool_name' "$BOUNDARY"; then
    fail "terminal recovery must remain catalog- and tool-name-independent"
  fi

  printf 'terminal-tool boundary: OK\n'
}

self_test() {
  local fixture
  fixture="$(mktemp -d)"
  trap "rm -rf '$fixture'" EXIT
  mkdir -p \
    "$fixture/lib/agent" \
    "$fixture/lib/pipeline" \
    "$fixture/lib/base"
  cp "$PLANNER" "$fixture/lib/agent/agent_tool_batch_plan.ml"
  cp "$BOUNDARY" "$fixture/lib/agent/agent_tool_terminal_boundary.ml"
  cp "$PIPELINE" "$fixture/lib/pipeline/pipeline_terminal_tool.ml"
  cp "$TOOL_MLI" "$fixture/lib/base/tool.mli"
  cp "$EVENT" "$fixture/lib/execution_event.ml"

  printf '\nlet _forbidden = Tool_set.to_list\n' \
    >> "$fixture/lib/pipeline/pipeline_terminal_tool.ml"
  if TERMINAL_BOUNDARY_ROOT="$fixture" "$0" --check >/dev/null 2>&1; then
    fail "negative self-test failed to detect current-catalog reconstruction"
  fi
  cp "$PIPELINE" "$fixture/lib/pipeline/pipeline_terminal_tool.ml"

  sed -i.bak '/; completion : Tool\.completion/d' "$fixture/lib/execution_event.ml"
  rm -f "$fixture/lib/execution_event.ml.bak"
  if TERMINAL_BOUNDARY_ROOT="$fixture" "$0" --check >/dev/null 2>&1; then
    fail "negative self-test failed to detect missing persisted completion"
  fi

  printf 'terminal-tool boundary self-test: OK\n'
}

case "${1:---check}" in
  --check) check_boundary ;;
  --self-test) self_test ;;
  *) fail "usage: $0 [--check|--self-test]" ;;
esac
