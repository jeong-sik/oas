#!/usr/bin/env bash
# check-tag-drift.sh — verify every CHANGELOG.md version entry has a matching
# git tag, making release-tag hygiene observable in CI.
#
# Relationship to peers:
#   release.sh             — one-shot "cut this release + push tag". Operator tool.
#   sync-version-truth.sh  — sync 3 in-repo surfaces (dune / opam / sdk_version.ml).
#   check-tag-drift.sh     — audit the 4th axis: released CHANGELOG entries ↔ git tags.
#
# Motivation: dune-project / agent_sdk.opam / lib/sdk_version.ml can be bumped,
# a CHANGELOG entry added, and the release PR merged, all without anyone ever
# running `release.sh --tag`. The drift is invisible until someone needs to pin
# by tag (e.g. downstream masc-mcp `oas-agent-sdk-pin.sh BASE=v0.169.0`). Scanning
# the repo on 2026-04-21 found v0.169.0, v0.164.0, v0.163.0, v0.161.0 (and more)
# documented as released in CHANGELOG but never tagged.
#
# Usage:
#   scripts/check-tag-drift.sh           # warn-only: list drift, always exit 0
#   scripts/check-tag-drift.sh --strict  # fail (exit 1) if any CHANGELOG version
#                                        # is missing a corresponding tag
#   scripts/check-tag-drift.sh --limit N # only consider the N most recent
#                                        # CHANGELOG entries (default: 10)
#
# Exit codes:
#   0   no drift, or drift present in warn-only mode
#   1   drift present in --strict mode
#   2   usage error / repo state error

set -euo pipefail

strict=false
limit=10

usage() {
  sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

while (($# > 0)); do
  case "$1" in
    --strict) strict=true ;;
    --limit) shift; limit="${1:-}" ;;
    --limit=*) limit="${1#--limit=}" ;;
    -h|--help) usage; exit 0 ;;
    *) usage >&2; exit 2 ;;
  esac
  shift
done

if ! [[ "$limit" =~ ^[0-9]+$ ]] || (( limit <= 0 )); then
  echo "check-tag-drift: --limit must be a positive integer" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

if [[ ! -f CHANGELOG.md ]]; then
  echo "check-tag-drift: CHANGELOG.md not found in $repo_root" >&2
  exit 2
fi

# Extract CHANGELOG versions in document order (newest first, by convention).
# A valid entry looks like: `## [0.169.0] - 2026-04-21` — skip unreleased /
# placeholder headers like `## [Unreleased]`.
versions=()
while IFS= read -r line; do
  versions+=("$line")
done < <(
  sed -n 's/^## \[\([0-9][0-9]*\(\.[0-9][0-9]*\)\{1,2\}\)\] .*/\1/p' CHANGELOG.md \
    | head -n "$limit"
)

if (( ${#versions[@]} == 0 )); then
  echo "check-tag-drift: no CHANGELOG versions found (regex: ^## \\[X.Y(.Z)\\] ...)" >&2
  exit 2
fi

missing=()
present=()

for v in "${versions[@]}"; do
  if git tag -l "v${v}" | grep -qx "v${v}"; then
    present+=("v${v}")
  else
    missing+=("v${v}")
  fi
done

echo "check-tag-drift: scanned ${#versions[@]} most recent CHANGELOG entries (limit=${limit})"
echo "  tagged:  ${#present[@]}"
echo "  missing: ${#missing[@]}"

if (( ${#missing[@]} > 0 )); then
  echo ""
  echo "Missing tags (CHANGELOG says released, git has no tag):"
  for t in "${missing[@]}"; do
    echo "  - ${t}"
  done
  echo ""
  echo "To fix for a specific version v<X>:"
  echo "  # find the release-cut commit (PR that edited dune-project to <X>)"
  echo "  git log --oneline --grep='cut <X>' -- dune-project"
  echo "  git tag -a v<X> <SHA> -m 'Release v<X>'"
  echo "  git push origin v<X>"

  if $strict; then
    exit 1
  fi
fi

exit 0
