# Contributing to agent_sdk

## Build from source

```bash
# Clone
git clone https://github.com/jeong-sik/oas.git
cd oas

# Pin fork dependencies (required for OCaml 5.4)
opam pin add bisect_ppx git+https://github.com/patricoferris/bisect_ppx.git#5.2 --no-action --yes
opam pin add mcp_protocol git+https://github.com/jeong-sik/mcp-protocol-sdk.git#main --no-action --yes
opam pin add mcp_protocol_eio git+https://github.com/jeong-sik/mcp-protocol-sdk.git#main --no-action --yes

# Install dependencies
opam install . --deps-only --with-test --yes

# Build everything
dune build @all

# Run tests
dune runtest

# Generate documentation
dune build @doc
```

## Code style

### .mli-first development

Public API changes start in `lib/agent_sdk.mli`. The .mli file is the contract.
Implementation follows the interface, not the other way around.

```
1. Edit lib/agent_sdk.mli   (define the interface)
2. Edit lib/<module>.ml      (implement it)
3. Edit test/test_<module>.ml (prove it works)
4. dune runtest               (verify)
```

### Naming conventions

| Item | Convention | Example |
|------|-----------|---------|
| Module | PascalCase | `Direct_evidence` |
| Type | snake_case | `agent_state` |
| Function | snake_case | `create_message_stream` |
| Variant | PascalCase | `EndTurn`, `StopToolUse` |

### Type safety

- Prefer `result` types over exceptions for expected failures.
- Use inline records for variants with 3+ fields (not positional tuples).
- Avoid `Obj.magic`. If the type system fights you, the design needs to change.
- `option` over sentinel values. `None` over empty string.

## Pull request expectations

1. **Tests required**: Every behavior change needs a test. `dune runtest` must pass.
2. **Interface first**: If you change public API, update `agent_sdk.mli` first.
3. **CHANGELOG**: Add an entry under `## [Unreleased]` in `CHANGELOG.md`.
4. **One concern per PR**: Keep PRs focused. Split unrelated changes.

## bisect_ppx fork

The upstream `bisect_ppx >= 2.8` does not build on OCaml 5.4.x.
We pin `patricoferris/bisect_ppx#5.2` which adds OCaml 5.4 support.
This pin is required for `--with-test` installation and coverage reporting.

## Versioning policy

We follow semver intent within the 0.x series:

- **0.x.0**: May contain breaking changes. Migration guide in CHANGELOG.
- **0.x.y** (y > 0): Additive features and bug fixes only.
- Target cadence: at most one minor release per week.

Version sources (must always match, enforced by CI):

| Source | Location |
|--------|----------|
| `dune-project` | `(version X.Y.Z)` |
| `lib/agent_sdk.ml` | `let version = "X.Y.Z"` |

### Release process

```bash
# 1. Verify version consistency (dry run)
./scripts/release.sh

# 2. Create and push tag
./scripts/release.sh --tag
```

The release script checks: version consistency, CHANGELOG entry exists,
tag doesn't already exist, clean working tree.

## Module stability tiers

Not all modules are equally stable. See README.md for the current tier list.

- **Stable**: Safe to depend on. Breaking changes get a minor version bump.
- **Evolving**: API may change between minor versions. Use with awareness.
- **Experimental**: May be removed or redesigned. Not recommended for production use.
