# agent-sdk

[![CI](https://github.com/jeong-sik/oas/actions/workflows/ci.yml/badge.svg)](https://github.com/jeong-sik/oas/actions/workflows/ci.yml)

> Personal project. No production SLA, no external support, no compatibility guarantees. The API surface changes on the author's schedule. Use at your own risk.
>
> 개인 프로젝트입니다. 프로덕션 SLA, 외부 지원, 호환성 보증 없음. 사용 시 자기 책임.

OCaml agent SDK on OCaml 5.x + Eio. Single-process, single-Eio-domain runtime. Talks to Anthropic Messages API and OpenAI-compatible chat endpoints (which covers many local servers, OpenRouter, and similar gateways), plus a local llama-server profile. See `lib/api_*.ml` for the actually wired endpoints.

- OCaml package: `agent_sdk` (in `lib/`)
- OCaml module: `Agent_sdk`
- Requires: OCaml >= 5.1, Dune >= 3.11
- Current version: source of truth is `lib/sdk_version.ml`

## Installation

```bash
opam pin add agent_sdk git+https://github.com/jeong-sik/oas.git#main --yes
```

For development (with test dependencies):

```bash
git clone https://github.com/jeong-sik/oas.git && cd oas

# Required fork pins (OCaml 5.4 compat)
source scripts/mcp-sdk-pin.sh
opam pin add bisect_ppx git+https://github.com/patricoferris/bisect_ppx.git#5.2 --no-action --yes
opam pin add mcp_protocol "git+${MCP_SDK_URL}#${MCP_SDK_SHA}" --no-action --yes

opam install . --deps-only --with-test --yes
dune build @all
```

## Non-interactive CLI transports and MCP defaults

OAS only owns the runtime policy for headless CLI subprocesses. Upstream CLI tools still have their own:

1. server registry layer (`claude mcp ...`, `codex mcp ...`, `gemini mcp ...`)
2. persistent config layer (user/project config files)
3. per-run execution policy layer (what OAS injects for this specific subprocess)

The recent bug lived in layer 3: a machine could have MCP servers registered, but a non-interactive OAS run was supposed to fail closed unless the caller opted back in.

Current headless defaults:

| Transport | Default runtime policy | Opt-back-in path |
|----------|------------------------|------------------|
| Claude Code | Always adds `--strict-mcp-config` | Supply `config.mcp_config` or `OAS_CLAUDE_MCP_CONFIG=/abs/path/mcp.json` |
| Codex CLI | Always prepends `-c mcp_servers={}` | Pass a non-empty `mcp_servers` TOML fragment through `OAS_CODEX_CONFIG` / caller config |
| Gemini CLI | Always sends an empty `--allowed-mcp-server-names` whitelist | Set `OAS_GEMINI_ALLOWED_MCP=name1,name2` |

Model selection remains request-scoped: Claude Code, Gemini CLI, and Codex CLI all honor a non-empty, non-`auto` `Provider_config.model_id` by passing the transport's model flag (`--model`). `auto` keeps the user's CLI default.

Why this can feel hidden:

- Claude exposes MCP registration and runtime policy as separate commands/flags.
- Codex exposes runtime policy through generic `-c key=value` overrides instead of a dedicated `--no-mcp` flag.
- Gemini is the clearest: runtime MCP exposure is controlled by `--allowed-mcp-server-names`.

### Codex sandbox cheat sheet

This is **Codex CLI's own sandbox flag**, not a downstream container sandbox.

Use these three levels:

- `OAS_CODEX_SANDBOX=read-only`: inspect / grep / diagnosis only
- `OAS_CODEX_SANDBOX=workspace-write`: normal local coding
- `OAS_CODEX_SANDBOX=danger-full-access`: last resort

Examples:

```bash
# safest explicit Codex run
OAS_CODEX_SANDBOX=read-only dune exec ./examples/review_agent.exe -- org/repo 123

# normal editing run
OAS_CODEX_SANDBOX=workspace-write dune exec ./examples/review_agent.exe -- org/repo 123
```

If `OAS_CODEX_SANDBOX` is unset, OAS does not add `-s`; only MCP-off
(`-c mcp_servers={}`) remains forced by default.

Examples:

```bash
# Claude Code: keep strict mode, but allow only the config you pass explicitly
OAS_CLAUDE_MCP_CONFIG=/abs/path/to/mcp.json dune exec ./examples/review_agent.exe -- org/repo 123

# Gemini CLI: allow only named MCP servers for this run
OAS_GEMINI_ALLOWED_MCP=filesystem,github dune exec ./examples/review_agent.exe -- org/repo 123

# Codex CLI: default remains MCP OFF unless the caller injects a non-empty
# mcp_servers TOML fragment via OAS_CODEX_CONFIG or equivalent caller wiring.
```

## Quickstart

```ocaml
open Agent_sdk

let () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run @@ fun sw ->
  let agent =
    Agent.create ~net
      ~config:
        { Types.default_config with
          name = "hello";
          system_prompt = Some "You are a helpful assistant. Be concise.";
        }
      ~tools:[] ()
  in
  match Agent.run ~sw agent "What is 2 + 2?" with
  | Ok response ->
      List.iter
        (function Types.Text t -> print_endline t | _ -> ())
        response.content
  | Error e -> prerr_endline (Error.to_string e)
```

This assumes a local LLM server (llama-server) on port 8085. For Anthropic API, configure a provider:

```ocaml
let agent =
  Agent.create ~net
    ~config:{ Types.default_config with name = "hello" }
    ~options:{ Agent.default_options with
      provider = Some (Provider.anthropic_sonnet ());
    }
    ~tools:[] ()
```

See `examples/` for more: `basic_agent.ml`, `tool_use.ml`, `streaming.ml`, `review_agent.ml`, `governance_demo.ml`, `plan_execute_demo.ml`, `autonomy_primitives_demo.ml`.

## Provider support

The provider variants actually wired in `lib/provider.ml` are:

| Variant | Constructor | Endpoint |
|---------|-------------|----------|
| `Local` (llama-server) | `Provider.Local { base_url }` / helper `Provider.local_llm ()` | `http://127.0.0.1:8085` (default) |
| `Anthropic` | `Provider.Anthropic` / helpers `anthropic_sonnet/haiku/opus ()` | `https://api.anthropic.com` |
| `OpenAICompat` | `Provider.OpenAICompat { base_url; ... }` / helper `Provider.openrouter ()` | Any `/chat/completions` host (OpenRouter, hosted gateways, local servers) |

There is no separate `Gemini` variant in the wired provider type — Gemini is reached through an OpenAI-compatible endpoint when one is available. If you need first-class Gemini support, that is currently a planned item, not a shipped one.

`Provider.resolve` returns `(base_url * api_key * headers, sdk_error) result`. Missing env vars produce `Error`, not silent fallback.

## Architecture

OAS ships one opam library in this repository:

```
Layer 1: agent_sdk  (lib/)
            |  Single-agent runtime: turn loop, tool dispatch, hooks.
            |
            +-- lib/agent/         Agent lifecycle, turns, tools, handoff, builder
            +-- lib/pipeline/      6-stage turn pipeline
            +-- lib/protocol/      A2A, MCP, Agent Card, Agent Registry
            +-- lib/llm_provider/  Shared LLM types, HTTP client, streaming
            +-- lib/*.ml           Context, Hooks, Guardrails, Orchestrator, etc.
```

Anything outside this repository — multi-process coordination, repo-wide task queues, dashboards, persistent shared state — is the responsibility of an external coordinator, not of OAS. OAS deliberately knows nothing about any specific coordinator.

### Layer 1: Agent Runtime (`agent_sdk`)

| Module | Role |
|--------|------|
| `Types` | Domain types: message, role, stop_reason, content_block, SSE events |
| `Provider` | LLM endpoint abstraction (Local / Anthropic / OpenAICompat / Ollama) |
| `Api` | HTTP client: `create_message` (sync) + `create_message_stream` (SSE) |
| `Agent` | Multi-turn agent loop with automatic tool_use handling (abstract `Agent.t`) |
| `Tool` / `Tool_set` | Tool definition, JSON Schema generation, O(1) lookup |
| `Builder` | Fluent API for agent construction with `build_safe` validation |
| `Hooks` | Lifecycle hooks: BeforeTurn, AfterTurn, PreToolUse, PostToolUse, OnStop |
| `Context` | Cross-turn shared state (scoped key-value store, `Yojson.Safe.t` values) |
| `Guardrails` | Tool filtering (AllowList/DenyList/Custom) + per-turn call limits |
| `Error` / `Error_domain` | 2-level structured errors: 7 domain variants + Internal, poly-variant mapping |
| `Orchestrator` | Multi-agent task orchestration (Sequential, Parallel, FanOut, Pipeline, Conditional) |
| `Log` | Structured logging with level filtering and composable sinks |
| `Mcp` | MCP client (NDJSON-over-stdio, server lifecycle, paginated tool listing) |
| `Streaming` | Multi-provider SSE parsing (Anthropic + OpenAI-compatible) |
| `Pipeline` | 6-stage turn pipeline with Provider_intf routing |
| `Contract` | Runtime contracts: instruction layers, triggers, tool grants |
| `Memory` | 5-tier memory: Scratchpad, Working, Episodic, Procedural, Long_term |
| `Memory_access` | Deny-by-default agent-scoped memory permissions |
| `Verified_output` | Phantom-typed compile-time output verification |
| `Policy` | Priority-ordered rule evaluation at decision points |
| `Audit` | Immutable log of policy decisions and agent actions |
| `Durable` | Typed step chains with execution journal for crash recovery |
| `Plan` | Goal decomposition with dependency DAG and re-planning |

## Module stability tiers

Not all modules are equally stable. Use this to gauge risk when depending on a module.
Every `.mli` now carries an explicit `@stability` tag and `@since` marker.
For the full 186-file classification, see `docs/api-stability.md`.

**Stable** -- safe to depend on, breaking changes only on minor version bumps:

`Types`, `Error`, `Agent`, `Builder`, `Tool`, `Tool_set`, `Hooks`, `Provider`, `Guardrails`, `Raw_trace`, `Checkpoint`, `Checkpoint_store`, `Context`, `Context_reducer`

**Evolving** -- API may change between minor versions:

`Streaming`, `Structured`, `Orchestrator`, `Collaboration`, `Memory`, `Policy`, `Proof_store`, `Cdal_proof`

**Internal** -- implementation details with no compatibility promise:

`lib/agent/*` submodules, `lib/protocol/*`, `lib/llm_provider/*` backends, parser/transport helpers, and other implementation-specific support modules

## Tool definition

```ocaml
(* Simple handler *)
let calc_tool =
  Tool.create ~name:"calculator"
    ~description:"Evaluate a math expression"
    ~parameters:[{
      Types.name = "expression";
      description = "The math expression";
      param_type = String; required = true;
    }]
    (fun args ->
      let open Yojson.Safe.Util in
      match args |> member "expression" |> to_string_option with
      | Some expr -> Ok (Printf.sprintf "Result: %s" expr)
      | None -> Error "missing expression")

(* Context-aware handler *)
let counter_tool =
  Tool.create_with_context ~name:"counter"
    ~description:"Increment and return counter"
    ~parameters:[]
    (fun ctx _input ->
      let n = match Context.get ctx "count" with
        | Some (`Int n) -> n + 1 | _ -> 1 in
      Context.set ctx "count" (`Int n);
      Ok (string_of_int n))
```

## Hooks and guardrails

```ocaml
let my_hooks = { Hooks.empty with
  pre_tool_use = Some (function
    | Hooks.PreToolUse { tool_name; _ } ->
        Printf.printf "Calling: %s\n" tool_name;
        Hooks.Continue
    | _ -> Hooks.Continue);
}

let guardrails = {
  Guardrails.tool_filter = Guardrails.AllowList ["calculator"];
  max_tool_calls_per_turn = Some 5;
}
```

## Build and test

```bash
dune build @all
dune runtest

# Coverage report
make coverage

# Harness datasets
oas eval record-trace --session /path/to/raw-trace.ndjson --out evals/replay.jsonl
oas eval run --dataset examples/evals/replay.jsonl --out _build/evals
oas eval run --config oas.json --dataset examples/evals/mixed.jsonl --out _build/evals
oas eval save-baseline --dataset examples/evals/replay.jsonl --out evals/replay-baseline.json
oas eval run --dataset examples/evals/replay.jsonl --baseline evals/replay-baseline.json --out _build/evals
oas eval run --config oas.json --dataset evals/replay.jsonl --out _build/evals

# `trace_replay` cases run offline, `fixture` cases use --config live in the same dataset
# Baseline compare/save currently target one evaluated case; use `--case <id>` when a dataset produces multiple runs

# Integration tests (requires local LLM server)
LLAMA_LIVE_TEST=1 dune exec ./test/test_local_llm.exe
LLAMA_LIVE_TEST=1 dune exec ./test/test_streaming_e2e.exe

# Run examples
dune exec examples/basic_agent.exe
dune exec examples/review_agent.exe -- jeong-sik/oas 123
```

## Constraints and trade-offs

- HTTP response body limit: 10MB.
- SSE streaming uses real-time event callbacks. Retry wraps outside the stream.
- tool_use loop returns the last response and stops when max_turns is exceeded.
- Runs on a single Eio domain. No multi-core parallelism.
- Prompt caching tracks `cache_creation_input_tokens` and `cache_read_input_tokens` in both streaming and non-streaming modes (since v0.4.0).

## Scope limitations

OAS is a single-process, single-Eio-domain agent runtime. The following concerns are explicitly out of scope and belong to a downstream consumer or a separate library.

| What | Owner | Why not here |
|------|-------|-------------|
| Worktree / filesystem coordination | External coordinator | Multi-process file locking and git worktree lifecycle are coordinator concerns, not SDK concerns. |
| MCP transport implementation | `mcp-protocol-sdk` | OAS consumes `mcp_protocol` as an opam dependency. Transport details (NDJSON framing, stdio management) live there. |
| Operator dashboard / visibility | External coordinator | Real-time agent status, workspace views, and supervisor dashboards belong to a layer that aggregates across processes. |
| Repo-level task and shared-state management | External coordinator | Task queues, claim semantics, and shared workspace state are coordination-plane concepts that span multiple agents and sessions. |
| Workflow scheduling / SaaS isolation | Embedding application | Cron triggers, tenant isolation, and multi-user access control are problems for the system that embeds OAS. |
| Long-term persistence / vector storage | Embedding application | Session state, memory backends, and embedding indexes are injected via callbacks, not owned by the SDK. |

If you find yourself pulling one of these responsibilities into `agent_sdk`, that is a signal the change belongs in a different repository. OAS does not name any specific downstream coordinator on purpose: anyone wiring OAS into a coordination layer should be free to make their own architectural choices.

## Versioning

The authoritative version string lives in `lib/sdk_version.ml` (mirrored in `dune-project` and `agent_sdk.opam`). Do not trust a number written into this README — it will drift.

We follow semver intent within the 0.x series:
- **0.x.0**: May contain breaking changes with migration guide in `CHANGELOG.md`.
- **0.x.y** (y > 0): Additive features and bug fixes only.

These are intent-level commitments on a personal project, not contractual guarantees.

See [CONTRIBUTING.md](CONTRIBUTING.md) for development setup and code style.
