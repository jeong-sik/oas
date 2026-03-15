#!/usr/bin/env bash
set -euo pipefail

# release.sh -- Verify version consistency and create a release tag.
#
# Usage:
#   ./scripts/release.sh          # dry-run (verify only)
#   ./scripts/release.sh --tag    # verify + create tag + push

TAG_MODE=false
if [[ "${1:-}" == "--tag" ]]; then
  TAG_MODE=true
fi

# Extract versions from canonical sources
DUNE_VER=$(grep '(version' dune-project | head -1 | sed 's/.*version \(.*\))/\1/')
SDK_VER=$(grep 'let version' lib/agent_sdk.ml | head -1 | sed 's/.*"\(.*\)".*/\1/')
LATEST_TAG=$(git tag -l 'v*' --sort=-v:refname | head -1)

echo "dune-project version: $DUNE_VER"
echo "agent_sdk.ml version: $SDK_VER"
echo "Latest git tag:       $LATEST_TAG"
echo ""

# Check 1: dune-project == agent_sdk.ml
if [[ "$DUNE_VER" != "$SDK_VER" ]]; then
  echo "ERROR: dune-project ($DUNE_VER) != agent_sdk.ml ($SDK_VER)"
  exit 1
fi

VERSION="$DUNE_VER"

# Check 2: tag doesn't already exist
if git tag -l "v${VERSION}" | grep -q .; then
  echo "ERROR: Tag v${VERSION} already exists."
  exit 1
fi

# Check 3: CHANGELOG has an entry for this version
if ! grep -q "\[${VERSION}\]" CHANGELOG.md; then
  echo "ERROR: No CHANGELOG.md entry found for [${VERSION}]."
  exit 1
fi

# Check 4: working tree is clean
if [[ -n "$(git status --porcelain)" ]]; then
  echo "WARNING: Working tree has uncommitted changes."
  if $TAG_MODE; then
    echo "ERROR: Cannot tag with dirty working tree."
    exit 1
  fi
fi

echo "All checks passed for v${VERSION}."

if $TAG_MODE; then
  echo ""
  echo "Creating tag v${VERSION}..."
  git tag -a "v${VERSION}" -m "v${VERSION}"
  echo "Pushing tag..."
  git push origin "v${VERSION}"
  echo "Released v${VERSION}."
else
  echo ""
  echo "Dry run complete. Run with --tag to create and push the tag."
fi
