# OAS RFC-0001 Big-bang vendor name purge (SDK boundary)

| | |
|---|---|
| Status | Draft |
| Related (masc-mcp) | RFC-0165 ~ RFC-0173 client-agnostic family |
| Scope | All `lib/`, `bin/`, `test/` OCaml files |
| Repo | yousleepwhen/oas |

## 1. Problem

masc-mcp RFC-0173 closed all vendor name references *except* 209 files that called into `agent_sdk.llm_provider` SDK closed-sum variant constructors. OAS is the SDK source. This RFC closes the SDK boundary.

User direction (from masc-mcp session):

> "OAS 가 그쪽이니까 같이 폭파에 동참하자"
> "전체 폭파 . 어차피 레거시 지원 안할거임"

OAS has one external consumer (masc-mcp), under the same operator's control. Fork-free renaming is feasible.

## 2. Decision

### Variant constructor renaming

| Original | New |
|----------|-----|
| `Anthropic` | `Provider_a` |
| `Moonshot` | `Provider_b` |
| `Kimi` | `Provider_c` |
| `OpenAI_compat` | `Provider_d_compat` |
| `OpenAI` (if any) | `Provider_d` |
| `Gemini` | `Provider_f` |
| `DeepSeek` | `Provider_g` |
| `DashScope` | `Provider_h` |
| `Glm` / `GLM` | `Provider_k` |
| `Claude_code` | `Cli_tool_d` |
| `Gemini_cli` | `Cli_tool_b` |
| `Kimi_cli` | `Cli_tool_c` |
| `Codex_cli` | `Cli_tool_a` |
| `Anthropic_messages` (request kind) | `Provider_a_messages` |
| `Gemini_3_1`, `Gemini_3`, `Gemini_2_5`, `Gemini_other` (model variants) | `Provider_f_3_1`, etc. |
| `Claude_opus_4`, `Claude_sonnet_4`, `Claude_haiku_4` | `Agent_llm_a_opus_4`, etc. |
| `Kimi_for_coding`, `Kimi_k2` | `Provider_c_for_coding`, `Provider_c_k2` |
| `Deepseek_v4_flash`, `Deepseek_v4_pro` | `Provider_g_v4_flash`, `Provider_g_v4_pro` |

### File renames

17 modules renamed via `git mv`:

- `api_anthropic.{ml,mli}` → `api_provider_a.{ml,mli}`
- `api_openai.{ml,mli}` → `api_provider_d.{ml,mli}`
- `backend_anthropic.{ml,mli}` → `backend_provider_a.{ml,mli}`
- `backend_openai{,_parse,_request,_serialize}.{ml,mli}` → `backend_provider_d{,_parse,_request,_serialize}.{ml,mli}`
- `backend_gemini.{ml,mli}` → `backend_provider_f.{ml,mli}`
- `backend_glm.{ml,mli}` → `backend_provider_k.{ml,mli}`
- `transport_codex_cli.{ml,mli}` → `transport_cli_tool_a.{ml,mli}`
- `transport_gemini_cli.{ml,mli}` → `transport_cli_tool_b.{ml,mli}`
- `transport_kimi_cli.{ml,mli}` → `transport_cli_tool_c.{ml,mli}`
- `transport_claude_code.{ml,mli}` → `transport_cli_tool_d.{ml,mli}`
- `transport_openai_compat.{ml,mli}` → `transport_provider_d_compat.{ml,mli}`
- `test_gemini_edge_cases.ml` → `test_provider_f_edge_cases.ml`
- `test_backend_gemini.ml` → `test_backend_provider_f.ml`
- `test_streaming_openai.ml` → `test_streaming_provider_d.ml`

Module references updated repo-wide; `test/dune` test list updated.

### Function/value renaming

- `glm_capabilities` → `provider_k_capabilities` (with `.mli` sync)

### Env var renaming (operator action required)

| Original | New |
|----------|-----|
| `ANTHROPIC_API_KEY`, `ANTHROPIC_DEFAULT_MODEL` | `PROVIDER_A_API_KEY`, `PROVIDER_A_DEFAULT_MODEL` |
| `GEMINI_API_KEY`, `GEMINI_DEFAULT_MODEL` | `PROVIDER_F_API_KEY`, `PROVIDER_F_DEFAULT_MODEL` |
| `KIMI_API_KEY`, `KIMI_BASE_URL`, `KIMI_DEFAULT_MODEL` | `PROVIDER_C_*` |
| `OPENAI_API_KEY`, `OPENAI_DEFAULT_MODEL` | `PROVIDER_D_*` |
| `MOONSHOT_API_KEY` | `PROVIDER_B_API_KEY` |
| `DEEPSEEK_API_KEY` | `PROVIDER_G_API_KEY` |
| `GLM_API_KEY` | `PROVIDER_K_API_KEY` |
| `DASHSCOPE_API_KEY` | `PROVIDER_H_API_KEY` |
| `CODEX_API_KEY` | `CLI_TOOL_A_API_KEY` |
| `OAS_ANTHROPIC_*`, `OAS_GEMINI_*`, `OAS_KIMI_*`, `OAS_OPENAI_*`, `OAS_CLAUDE_*`, `OAS_CODEX_*`, `OAS_DEEPSEEK_*`, `OAS_GLM_*`, `OAS_DASHSCOPE_*`, `OAS_MOONSHOT_*` | `OAS_PROVIDER_{A..K}_*`, `OAS_CLI_TOOL_{A..D}_*` |

**Operator action**: update `.zshenv` to re-export with new names. Old env vars are no longer read.

## 3. Intentionally preserved (RFC-0001 §3)

| Surface | Reason |
|---|---|
| `agent_llm_a_path = "claude"` (transport_cli_tool_d.ml) | External OS binary name. `claude` binary is the actual CLI tool installed on the operator's system. |
| `agent_code_path = "codex"` (transport_cli_tool_a.ml) | Same — external `codex` binary. |
| `provider_f_path = "gemini"` (transport_cli_tool_b.ml) | Same — external `gemini` binary. |
| `provider_c_path = "kimi"` (transport_cli_tool_c.ml) | Same — external `kimi` binary. |
| `~name:"claude"`, `~name:"codex"`, `~name:"gemini"`, `~name:"kimi"` (log labels in subprocess transport) | Same as binary path — operator-facing label that matches the actual binary. |
| `CODEX_COMPANION_SESSION_ID` env var (scrubbed) | External Codex CLI's own env var, scrubbed by transport before subprocess exec. Not OAS-defined. |
| `Ollama` variant (provider_kind) | LLM serving framework, not vendor — same rationale as masc-mcp RFC-0168~0173. |
| Comment references to original vendor names | None — all rewritten. |

These 4 binary path + 12 spawn-name occurrences are the technical boundary: the SDK calls external OS binaries that must be installed by name on the operator's system.

## 4. Verification

- `scripts/dune-local.sh build lib` clean.
- `scripts/dune-local.sh build bin` clean.
- `scripts/dune-local.sh build test` clean.
- `find lib bin test -type f \( -name '*.ml' -o -name '*.mli' \) | xargs grep -E 'anthropic|kimi|gemini|moonshot|codex|claude|dashscope'` returns only the §3 preserved occurrences (binary path + spawn names).

## 5. Downstream consumer migration

masc-mcp will receive a follow-up PR that:
1. Updates the opam pin or version in `dune-project` to consume this OAS release.
2. Rewrites the remaining 209 `Llm_provider.Provider_config.{Kimi, Claude_code, ...}` call sites to use the new `{Provider_c, Cli_tool_d, ...}` constructors.

That follow-up is RFC-0174 in masc-mcp's RFC sequence.

## 6. Workaround-rejection self-check

This RFC removes vendor namespace; it does not add code paths.

1. "makes X visible" without fixing — NO.
2. String/substring/prefix classifier added — NO.
3. "PR #N fixed K of M sites" — NO (sweeps the entire OAS surface in one PR; 16 occurrences in §3 are the external OS boundary).
4. catch-all `_ ->` added — NO.
5. cap / cooldown / dedup / repair — NO.
6. test backdoor — NO.
7. typo / off-by-one repeated — NO.

All 7 rejection signatures: NO.

## 7. Breaking change

This is a **major version bump** (0.196.x → 0.197.0 or 1.0.0 by operator preference). Every downstream consumer must migrate. Only masc-mcp is known to depend on `agent_sdk.llm_provider` — handled by RFC-0174.
