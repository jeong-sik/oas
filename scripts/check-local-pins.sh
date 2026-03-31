#!/usr/bin/env bash
# Detect opam pin drift: compare local pins against SSOT.
# Run before build to catch CI/local divergence early.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/mcp-sdk-pin.sh"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

drift=0

check_pin() {
  local pkg="$1" expected_sha="$2" label="$3"
  local local_info
  local_info="$(opam pin list 2>/dev/null | grep "^${pkg}\." || true)"

  if [[ -z "$local_info" ]]; then
    echo -e "${RED}[DRIFT]${NC} ${label}: not pinned locally"
    echo "  expected: ${expected_sha:0:12} (from scripts/mcp-sdk-pin.sh)"
    echo "  local:    opam default / no pin"
    echo "  fix: opam pin add ${pkg} \"git+${MCP_SDK_URL}#${expected_sha}\" --no-action --yes"
    drift=1
    return
  fi

  if echo "$local_info" | grep -q "$expected_sha"; then
    echo -e "${GREEN}[OK]${NC} ${label}: ${expected_sha:0:12}"
  else
    local actual
    actual="$(echo "$local_info" | grep -oE 'at [a-f0-9]+' | head -1 | sed 's/at //')"
    echo -e "${RED}[DRIFT]${NC} ${label}"
    echo "  expected: ${expected_sha:0:12} (from scripts/mcp-sdk-pin.sh)"
    echo "  local:    ${actual:-unknown}"
    echo "  fix: opam pin add ${pkg} \"git+${MCP_SDK_URL}#${expected_sha}\" --no-action --yes"
    drift=1
  fi
}

echo "Checking local opam pins against SSOT..."
check_pin "mcp_protocol" "$MCP_SDK_SHA" "mcp_protocol"

if [[ $drift -ne 0 ]]; then
  echo ""
  echo -e "${RED}Pin drift detected.${NC} Local build may behave differently from CI."
  echo "Run the fix commands above, then: dune clean && dune build"
  exit 1
fi

echo -e "${GREEN}All local pins match CI SSOT.${NC}"
