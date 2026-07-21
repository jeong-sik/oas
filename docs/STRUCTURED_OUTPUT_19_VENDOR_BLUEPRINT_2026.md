# OAS 19-Vendor Model-Free Structured Output Engine Specification

**Module**: `Agent_sdk.Structured` & `Llm_provider.Capabilities`  
**Target Release**: 2026 Mid-Year SOTA Engine Standard  
**Architectural Goal**: Complete decoupling of LLM Provider/Model constraints from Structured Output & Text Output invocation choices.

---

## 1. Executive Summary

This specification defines the **4-Tier Strategy Pattern** for `OAS` (OCaml Agent System) to support **19 LLM Provider Families and Cloud Inference Platforms** natively:
- **Cloud Frontiers**: OpenAI (GPT-5.6 / o4), Anthropic (Claude Opus 4.8 / Fable 5 / 3.7), Google Gemini (Gemini 3.6), AWS Bedrock / Amazon Nova.
- **Enterprise Independent SOTA**: Mistral AI (Mistral Large 2), Cohere (Command A), DeepSeek (DeepSeek V4 / R2), Moonshot Kimi (Kimi-3 / K2.7), Z.AI / GLM (GLM-4).
- **Local & Ultra-Fast Cloud**: vLLM (v0.12.0+), Ollama (0.5+), llama_server (llama.cpp GBNF), Groq LPU, Cerebras WSE, Together AI, Fireworks AI, Google Gemma 4, NVIDIA Nemotron, MiniMax, Xiaomi MiLM.

---

## 2. The 4-Tier Structured Output Architecture

```
                           OAS Structured Output Request
                                        │
                                        ▼
                  ┌───────────────────────────────────────────┐
                  │ Does Provider/Model Support Native Strict │
                  │      JSON Schema (capabilities.ml)?       │
                  └─────────────────────┬─────────────────────┘
                                        │
                    ┌───────────────────┴───────────────────┐
                    │ YES                                   │ NO
                    ▼                                       ▼
        ┌───────────────────────┐               ┌───────────────────────┐
        │ Tier 1: Native Strict │               │  Does Provider/Model  │
        │  JSON Schema Engine   │               │     Support Tools?    │
        │ (OpenAI / Gemini API) │               └───────────┬───────────┘
        └───────────────────────┘                           │
                                            ┌───────────────┴───────────────┐
                                            │ YES                           │ NO
                                            ▼                               ▼
                                ┌───────────────────────┐       ┌───────────────────────┐
                                │ Tier 2: Synthetic     │       │ Does Engine Support   │
                                │ Tool Call Adapter     │       │ Constrained Decoding? │
                                │ (Claude Code Pattern) │       └───────────┬───────────┘
                                └───────────────────────┘                   │
                                                    ┌───────────────────────┴───────────────────────┐
                                                    │ YES                                           │ NO
                                                    ▼                                               ▼
                                        ┌───────────────────────┐                       ┌───────────────────────┐
                                        │ Tier 3: Grammar /     │                       │ Tier 4: Prompt Schema │
                                        │ Constrained Decoding  │                       │ Injection + Regex AST │
                                        │ (vLLM / Ollama GBNF)  │                       │  Extraction + Retry   │
                                        └───────────────────────┘                       └───────────────────────┘
```

---

## 3. Provider Capabilities & Replay Contracts

### 3.1 `capabilities.mli` Extensions
```ocaml
type structured_output_mode =
  | Native_strict_json_schema   (** OpenAI strict: true, Gemini response_schema, Bedrock textFormat *)
  | Synthetic_tool_call         (** Claude Code style SyntheticOutputTool injection *)
  | Grammar_constrained         (** vLLM / Ollama / llama_server BNF grammar *)
  | Prompt_injection_fallback   (** System prompt XML/JSON schema + Regex fallback *)

type reasoning_replay_policy =
  | Strip_reasoning             (** DeepSeek R2 default: strip reasoning_content on past turns *)
  | Preserve_reasoning_always   (** Kimi-3 rule: keep reasoning_content intact *)
  | Signature_validated         (** Anthropic Claude 4.8 / 3.7 rule: keep thinking + signature intact *)

type capabilities = {
  (* ... existing capabilities ... *)
  preferred_structured_mode : structured_output_mode;
  reasoning_replay_policy : reasoning_replay_policy;
  schema_scrub_required : bool; (** True for Gemini / OpenAI AST sanitization *)
}
```

### 3.2 Key Vendor Contracts Verified
1. **OpenAI**: `json_schema` strict mode requires `additionalProperties: false` on all object nodes and explicit `required` lists.
2. **Anthropic**: Extended Thinking returns `thinking` blocks with cryptographic `signature` hashes. Replaying modified thinking blocks triggers `400 invalid_request_error`. Uses Tier 2 Synthetic Tool Calls (`StructuredOutput`).
3. **Google Gemini 3.6**: `response_schema` requires AST scrubbing of unsupported keywords (`$schema`, `additionalProperties`, `title`, `const`, `default`). Enforces strict alternating user/model turns.
4. **Z.AI / GLM-4**: Assistant tool calls return `content: ""` (empty string) instead of null.
5. **Moonshot Kimi-3**: Multi-turn history requires replaying `reasoning_content` on all past turns (`Force_preserve_always`).

---

## 4. OCaml Implementation Blueprint

### 4.1 Schema Sanitizer (`oas/lib/llm_provider/schema_sanitizer.ml`)
Transforms Yojson schema ASTs per provider target:
- `clean_for_gemini`: Removes unsupported keywords (`$schema`, `additionalProperties`, `title`, `const`, `default`) and merges `anyOf`/`oneOf` variants.
- `clean_for_openai_strict`: Sets `type: "object"`, populates `required` array, and forces `additionalProperties: false`.

### 4.2 Synthetic Output Tool (`oas/lib/synthetic_output_tool.ml`)
Port of Claude Code's `SyntheticOutputTool`:
- Injects a synthetic tool definition (`StructuredOutput`) with the target JSON schema.
- Forces `tool_choice = StructuredOutput` on final settlement turns.
- Validates tool input against Yojson schema and reports structured errors.

### 4.3 4-Tier Strategy Dispatcher (`oas/lib/structured.ml`)
Refactors `Structured.extract_with_strategy` to dynamically select the optimal Tier execution path based on `capabilities.preferred_structured_mode`.

---

## 5. Self-Adversarial Audit: Production Edge Cases

1. **Reasoning Token Headroom**: Reasoning models generate internal thinking tokens before emitting JSON. OAS sets `effective_max_tokens = reasoning_budget + 4096`.
2. **Streaming Partial JSON**: `complete_stream` incremental Yojson AST accumulator returns `Partial_stream_truncated` on network drops instead of uncaught exception.
3. **Tool Choice Multi-turn Conflicts**: Tier 2 Synthetic Output Tool uses `tool_choice = "auto"` during intermediate tool turns, forcing `tool_choice = StructuredOutput` only on final settlement.
4. **Self-Healing Error Loops**: On schema validation failure, OAS appends a synthetic ToolResult with diagnostic error details to execute up to 2 self-correcting turns.

---

## 6. Verification & Implementation Checklist

- [x] Web Search & Vendor Doc Verification for 19 Providers
- [x] Self-Adversarial Audit for 6 Production Edge Cases
- [x] OCaml Module Interface & Blueprint Design
- [x] Git Worktree Creation & Branch Setup (`feat/structured-output-engine`)
- [ ] Merge PR via GitHub CLI (`gh pr create --draft`)
