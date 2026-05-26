# RFC-OAS-023: Capability axis reshape — model × transport

| | |
|---|---|
| Status | Draft (skeleton) |
| Author | jeong-sik |
| Created | 2026-05-26 |
| Target | `agent_sdk` (oas) — `lib/llm_provider/`, `lib/provider.ml` |
| Supersedes (partial) | RFC-0001 vendor purge (naming policy portion) — see §7.1 |
| Supplements | RFC-OAS-018 catalog externalization (adds axis reshape on top of externalization) — see §7.2 |
| Sibling | RFC-OAS-009 (tool name ignorance), RFC-OAS-017 (coordinator-shape leak) |
| Boundary | masc-mcp depends on `Provider_kind.t` variant names (RFC-0174~0177) — see §7.3 |

## 0. Summary

OAS capability catalog가 두 가지 동시 결함을 가지고 있다.

1. **표기 결함** — RFC-0001 후 brand가 알파벳 부호로 1:1 치환되었다 (`Anthropic → Provider_a`, `Kimi → Provider_c`, `OpenAI_compat → Provider_d_compat` 등 14×N). [feedback_vendor_brand_substitution_is_encryption_not_abstraction] 메모리(2026-05-24)의 자기 비판이 그대로 적용된다 — abstraction이 아니라 encryption.
2. **축 결함** — capability의 1차 축은 *model*인데 catalog는 *provider* 부호로 분류되어 있다. 같은 모델(`kimi-k2.6`)이 여러 provider(Moonshot direct / Ollama Turbo cloud / OpenRouter / local quantization)로 갈 수 있고, capability는 `model_caps ∩ transport_caps`의 cross-product에서 결정되는데, 지금 catalog는 두 record를 한 record로 압축하여 *둘 다*를 잃었다.

본 RFC는 두 결함을 동시에 해결한다:

- **Naming policy (hybrid)**: model 축은 brand 그대로 (`kimi-k2.6`, `claude-opus-4`, `gpt-5`), transport 축은 wire-protocol (`messages_v1`, `chat_completions_v1`, `gemini_generate_v1`, `ollama_chat_v1`).
- **Two-record capability**: `effective_caps(model_id, provider) = model_caps(model_id).applyMask(transport_caps(provider))`.

## 1. Problem

### 1.1 RFC-0001 substitution = encryption

RFC-0001은 "OAS 가 그쪽이니까 같이 폭파에 동참하자", "전체 폭파 . 어차피 레거시 지원 안할거임" 지시로 14개 brand를 알파벳 부호로 1:1 치환했다. 결과:

```
Anthropic       → Provider_a
Moonshot        → Provider_b
Kimi            → Provider_c
OpenAI_compat   → Provider_d_compat
Gemini          → Provider_f
DeepSeek        → Provider_g
DashScope       → Provider_h
Mistral         → Provider_j
Glm/GLM         → Provider_k
Qwen            → Provider_h_3
Claude_code     → Cli_tool_d
Gemini_cli      → Cli_tool_b
Kimi_cli        → Cli_tool_c
Codex_cli       → Cli_tool_a
```

[feedback_vendor_brand_substitution_is_encryption_not_abstraction] (2026-05-24, masc-mcp RFC-0174~0177 sweep 후):

> Vendor brand → 알파벳 부호 1:1 substitution 은 abstraction 아닌 *암호화*. coupling 본질 보존 + 가독성 감소 + 매핑 테이블 외부 분산.

증상:

- 새 maintainer가 `base = "provider_d_chat"` manifest entry를 적으려면 `capabilities_for_provider_label` 매핑을 *역추적*해야 한다 ("끝말풀이").
- cascade.toml의 model id는 brand 그대로(`kimi-k2.6:cloud`, `deepseek-v4-pro:cloud`)인데 OAS catalog만 부호 — 양쪽이 만나는 모든 경계에서 매핑 테이블이 필요해진다.
- 부호의 *의미*는 코드 어디에도 명시되어 있지 않다 (`provider_d_chat`이 OpenAI chat completions wire를 가리킨다는 사실은 `backend_openai.ml` 파일명을 봐야 추정 가능).

### 1.2 Axis confusion — capability의 1차축이 model

`capabilities_for_provider_label : string -> capabilities option` 함수가 존재한다 (capabilities.ml:953). 이 함수는 provider label 단독으로 capability를 결정한다 — *모델 정보 없이*. 그러나 실제로 capability의 압도적 다수(context window, reasoning 지원, tool calling, image input, vision, audio)는 *모델 본체*의 속성이다. 같은 OpenAI chat completions wire를 통과한다고 해서 GPT-4와 GPT-5가 같은 capability를 갖지 않는다.

`for_model_id`가 이미 존재하지만 (capabilities.ml:1057), `provider.ml:288-296`의 fallback chain이 *miss 시 provider-default를 복원*함으로써 model 축의 진실성을 부정한다:

```ocaml
| OpenAICompat { base_url; _ } ->
  ...
  (match Llm_provider.Capabilities.for_model_id model_id with
   | Some caps -> caps
   | None -> default_provider_d_compat_capabilities model_id)  (* ← silent default *)
```

증상: 카탈로그에 없는 모델이 `provider_d_chat_capabilities` (reasoning=false)로 fallback → 실제 응답에 thinking block이 있어 `Thinking_returned_but_declared_unsupported` drift WARN. 이는 *catalog miss*를 *capability 사실*로 silent하게 변환한 결과.

### 1.3 Cross-product collapse — model × transport

같은 모델이 여러 provider로 갈 수 있다:

```
kimi-k2.6  →  Moonshot direct           (chat_completions_v1)
           →  Ollama Turbo cloud         (ollama_chat_v1)
           →  OpenRouter                 (chat_completions_v1, cache_control stripped)
           →  Local llama-server         (chat_completions_v1, context windowed down)
```

이들은 같은 모델이지만 *transport가 깎는 capability*가 다르다:

- Ollama Turbo: tool_choice 강제 불가, cache_control 미지원
- OpenRouter: cache_control 운반 못 함 (proxy), thinking_control_format 일부만 운반
- Local quantization: max_context_tokens가 quantization config에 의해 잘림
- Moonshot direct: 모든 모델 capability 운반 가능 (canonical)

지금 catalog는 이 cross-product를 *한 record로 압축*하고 있다 — `provider_d_chat_capabilities`는 "OpenAI 호환 chat completions wire를 통과한 어떤 가상 모델의 보수적 caps"라는 chimera. model도 transport도 아닌 중간 존재.

증상:

- 같은 `kimi-k2.6`을 다른 provider로 라우팅할 때 capability가 정확히 표현되지 않는다.
- `apply_manifest_entry`에서 `base_label` 인터페이스가 부자연스럽다 (base로 무엇을 골라야 할지 model/transport 어느 축에서 잡을지 모호).

## 2. Decision

### 2.1 Hybrid naming

| 축 | 명명 정책 | 예 |
|----|----------|----|
| **Model id** | Brand 그대로 (RFC-0001 substitution 역방향) | `kimi-k2.6`, `claude-opus-4`, `gpt-5`, `gemini-2.5-pro`, `glm-5.1`, `deepseek-v4-pro`, `qwen-3.5-35b` |
| **Transport / wire protocol** | Wire-protocol 명명 (brand 노출 없음) | `messages_v1`, `chat_completions_v1`, `gemini_generate_v1`, `ollama_chat_v1`, `glm_native_v1` |
| **Model family** (관용적 grouping) | Brand + size/version | `claude-4-family`, `gpt-5-family`, `k2-family` |

근거: capability는 model 축의 속성이고 model은 brand와 분리 불가능하다 (brand가 곧 model의 고유 식별자). transport는 wire protocol 그 자체이지 brand가 아니다 (chat completions wire는 OpenAI 외에 Ollama, OpenRouter, llama-server 등 다수가 *동일하게 구현*하므로 brand로 명명하면 잘못된다).

### 2.2 Two-record capability composition

```ocaml
(* Pseudo-code *)
type model_caps = {
  max_context_tokens : int option;
  max_output_tokens : int option;
  supports_reasoning : bool;
  supports_reasoning_budget : bool;
  supports_tools : bool;
  supports_image_input : bool;
  supports_video_input : bool;
  supports_audio_input : bool;
  ... (* 모델 본체의 속성만 *)
}

type transport_caps = {
  can_carry_cache_control : bool;
  can_carry_tool_choice : bool;
  supports_streaming : bool;
  thinking_control_format : Thinking_control_format.t;
  carries_reasoning_summary_blocks : bool;
  max_output_tokens_cap : int option;   (* provider-side quota cap *)
  ... (* wire 가 깎는 표면만 *)
}

val effective_caps : model_id:string -> provider:provider -> capabilities
(* Lookup: model_caps (brand catalog) + transport_caps (protocol catalog)
   Compose: intersection / mask. *)
```

`effective_caps`는 두 record의 *교집합*. transport가 *깎는* 표면만 transport_caps에 들어가고, 나머지는 model_caps 그대로 통과.

## 3. Architecture

### 3.1 Lookup 진입점

```
                      ┌─────────────────────────────┐
   call site ────────►│ effective_caps              │
                      │   ~model_id ~provider       │
                      └──┬───────────────────┬──────┘
                         ▼                   ▼
            ┌─────────────────────┐   ┌───────────────────────┐
            │ model_caps_for_id   │   │ transport_caps_for_   │
            │   string -> caps    │   │   provider/protocol   │
            │                     │   │                       │
            │ brand catalog       │   │ wire-protocol catalog │
            │ (manifest + static) │   │ (closed set)          │
            └─────────────────────┘   └───────────────────────┘
```

### 3.2 Unknown model 정책 — fail closed

`provider.ml:296`의 silent default fallback (`default_provider_d_compat_capabilities`)을 폐지한다. catalog miss는:

- (a) RFC-OAS-018에서 도입한 manifest layer로 *재시도* (operator escape hatch),
- (b) 그래도 miss면 `Capability_unknown { model_id }` 에러를 *큰소리로* 호출자에 전달.

이유: "가능한 모든 것의 카탈로그가 있어야 한다"는 사용자 입장에서, silent default는 *catalog의 부재를 가시화*하지 못한다. drift WARN은 사후 가시화이지 사전 차단이 아니다.

### 3.3 `capabilities_for_provider_label` 폐지

`capabilities_for_provider_label : string -> capabilities option` 함수는 의미상 model 축의 사실을 provider 축으로 *위장*시킨다. 폐지하고 다음 두 함수로 대체:

```ocaml
val transport_caps_for_protocol : Wire_protocol.t -> transport_caps
val model_family_default_caps : Model_family.t -> model_caps
  (* family-level default; per-model override는 catalog에서 *)
```

`apply_manifest_entry`의 `base_label` 인터페이스는 `Model_family.t` 또는 `Wire_protocol.t` 중 하나를 명시하도록 변경.

## 4. Naming sweep

### 4.1 Provider_kind.t variant 복원

| RFC-0001 (현행) | RFC-OAS-023 (목표) | 축 |
|----------------|---------------------|-----|
| `Provider_a` | `Anthropic` | model brand |
| `Provider_b` | `Moonshot` | model brand |
| `Provider_c` | `Kimi` | model brand |
| `Provider_d` | `OpenAI` | model brand |
| `Provider_d_compat` | `Chat_completions_v1` | **wire protocol** (OpenAI-호환 wire를 의미하므로) |
| `Provider_f` | `Gemini` | model brand |
| `Provider_g` | `DeepSeek` | model brand |
| `Provider_h` | `DashScope` | model brand |
| `Provider_h_3` (Qwen family) | `Qwen` | model brand |
| `Provider_j` | `Mistral` | model brand |
| `Provider_k` | `GLM` | model brand |
| `Cli_tool_a` | `Codex_cli` | tool brand |
| `Cli_tool_b` | `Gemini_cli` | tool brand |
| `Cli_tool_c` | `Kimi_cli` | tool brand |
| `Cli_tool_d` | `Claude_code_cli` | tool brand |

핵심 관찰: `Provider_d_compat`만 wire-protocol로 가는 이유 — 이 variant는 brand가 아니라 *protocol*을 의미했다 (Ollama, OpenRouter, llama-server, vLLM 모두 같은 chat completions wire를 구현). 나머지는 brand 그대로 복원.

### 4.2 함수/타입 명명 sweep

```
provider_a_capabilities             → anthropic_model_caps
provider_d_chat_capabilities        → chat_completions_v1_transport_caps + openai_model_default_caps (분리)
provider_d_chat_extended_capabilities → 폐지 (Provider_h가 wire가 아니라 model이므로 model_caps에 흡수)
provider_c_capabilities             → kimi_model_default_caps
provider_k_capabilities             → glm_model_default_caps
...
```

`provider_d_chat_extended_capabilities`의 사례가 axis confusion의 가장 선명한 증거 — "OpenAI 호환 wire의 *Qwen-3 확장*"이라는 명명 자체가 두 축을 한 record로 압축했다는 자기 진술.

## 5. Inventory

### 5.1 cascade.toml × OAS catalog 커버리지 (2026-05-26 audit)

`~/me/.masc/config/cascade.toml` 의 모든 `api-name` 을 OAS `Capabilities.for_model_id` 의 prefix table (capabilities.ml `String.starts_with` 30 prefixes) 과 대조한 결과:

| api-name (wire에 흐르는 그대로) | Brand | RFC-0001 cipher | OAS prefix가 기대하는 형태 | Match |
|---|---|---|---|---|
| `gpt-5.3-codex-spark` | OpenAI | Provider_d | `model-d-5*` | **MISS** |
| `gpt-4.1` | OpenAI | Provider_d | `model-d-4.1*` | **MISS** |
| `glm-5.1` | GLM | Provider_k | (no `provider_k-*` prefix) | **MISS** |
| `glm-5-turbo` | GLM | Provider_k | — | **MISS** |
| `glm-5` | GLM | Provider_k | — | **MISS** |
| `gemma4:e2b` | Gemma | Provider_f | `google/model-f-gemma-4*` | **MISS** |
| `glm-5.1:cloud` | GLM | Provider_k | — | **MISS** |
| `qwen3.5` | Qwen | Provider_h_3 | `provider_h-3*` | **MISS** |
| `kimi-k2.6:cloud` | Kimi | Provider_c | `provider_c-k2*` | **MISS** |
| `deepseek-v4-pro:cloud` | DeepSeek | Provider_g | `provider_g-v4-pro*` | **MISS** |
| `deepseek-v4-flash:cloud` | DeepSeek | Provider_g | `provider_g-v4-flash*` | **MISS** |
| `qwen` | Qwen | Provider_h_3 | `provider_h-3*` | **MISS** |
| `qwen-local-35b-a3b` | Qwen | Provider_h_3 | `provider_h-3*` | **MISS** |

**Hit rate: 0/13 (0%)**.

해석:

- RFC-0001 이 *source-level brand* 는 cipher 로 바꿨지만, *wire 에 들어오는 model_id 문자열* 은 brand 그대로다. cascade 가 `kimi-k2.6:cloud` 를 그대로 흘리는데 OAS catalog 의 prefix table 은 `provider_c-k2*` 를 기대 — 100% miss.
- 결과적으로 cascade.toml 의 모든 모델이 `default_provider_d_compat_capabilities` (≡ `provider_d_chat_capabilities`, reasoning=false) 보수적 default 로 fallback 중. 즉 cascade.toml 의 `[models.X.capabilities]` 블록에 사용자가 *명시적으로 적은* capability 선언이 OAS 가 보는 effective_caps 와 *전혀 일치하지 않는다*.
- drift WARN (`Thinking_returned_but_declared_unsupported`, `Tools_used_but_declared_unsupported` 등) 이 *전체 cascade 모델* 에서 발생 가능 상태. kimi-k2.6 만 본 것이 아니라 *시스템 전체* 가 catalog miss 영역에 있다.

평면 불일치 문제로서의 함의:

- catalog 에 새 entry 를 추가하는 식의 *etry-by-entry fix* 는 *원천적으로 불가능*. RFC-0001 정책상 새 entry 의 `id_prefix` 도 cipher 여야 하는데 wire 는 brand 를 그대로 흘리므로 매핑이 닫히지 않는다.
- RFC-OAS-018 catalog externalization 도 이 평면 불일치를 *해결하지 못함*. JSON manifest 로 빼내도 `id_prefix` 를 brand 로 쓸지 cipher 로 쓸지가 또 결정점이 된다.
- 즉 **RFC-OAS-023 (hybrid: model=brand 복원) 이 RFC-OAS-018 보다 *논리적으로 선행*** 한다. RFC-OAS-023 으로 catalog 의 key 평면을 wire 평면과 일치시킨 *후에* 만 RFC-OAS-018 의 externalization 이 의미 있는 데이터를 받을 수 있다.

### 5.2 RFC-OAS-018 inventory refresh

> RFC-OAS-018의 2026-05-12 inventory를 본 RFC merge 시점에 갱신. 14개 cipher token, 35 `starts_with` dispatcher, 11 closed-sum variants, 240 model literal — 현재 시점 수치 재측정.

## 6. Migration

### 6.1 Single big PR vs phased

[feedback_radical_improvement_over_diff_size] / [feedback_big_bang_refactor_preference]를 따라 *single big PR* 기본. 다만 다음 audit 의무를 PR body에 명시적으로 충족:

- [feedback_dead_export_sweep_2026_05_23_anti_pattern] — 5-prong dead-export audit (direct grep + module alias + re-export `let x = M.x` + `include Module` + paired test file)
- [feedback_module_name_grep_misses_open_bare_names] — bare function-name grep도 함께
- [feedback_partial_module_orphan_check_all_exports] — `.mli` 전체 exports 확인 후 부분 삭제 금지

대안 phased (B):

- Phase 1: `Provider_kind.t` variant rename 단독 (data 변경 없음, 이름만)
- Phase 2: `model_caps` / `transport_caps` 분리 (record 재구조화)
- Phase 3: `capabilities_for_provider_label` 폐지 + caller 수정
- Phase 4: silent default fallback 제거 + Unknown 정책 적용
- Phase 5: cascade.toml audit 기반 model catalog 채움

> **[DECISION NEEDED #1]** 사용자 결정점 — Single big PR vs Phased 5. 메모리는 big PR 선호. masc-mcp 의존성(§7.3) 때문에 phased가 더 안전할 수도 있음.

### 6.2 Audit obligations

각 변경마다:

```
1. rg "<old_name>" lib/ test/ bin/      # direct
2. rg "<short_function_name>" lib/      # opener-unqualified (open Module 후 bare call)
3. rg "include <Module>" lib/           # facade re-export
4. rg "module .* = <Module>" lib/       # alias
5. find . -name "test_<module>.ml"      # paired test
```

5가지 모두 0 이어야 삭제 안전. PR body에 audit 결과 인용 의무.

### 6.3 Test 운영

- Capability drift detector(`detect_drift`)가 PR 직후 *오히려 더 많은 WARN을 emit*할 수 있다 — silent default 제거로 인해. CI 임계는 PR 시점에 baseline reset.
- TLA+ spec이 capability 관련해서 존재한다면 검증 필요 (현재 OAS에 TLA+ spec 없음으로 추정).

## 7. Relations

### 7.1 RFC-0001 vendor purge — supersede partial

RFC-0001의 **명명 정책 부분**을 *역방향으로 supersede*. 즉 RFC-0001의 14×N 부호 매핑을 표 그대로 *반대 방향으로 sweep*. RFC-0001의 다른 동기(SDK 경계 폐쇄, masc-mcp와의 일관성)는 유효하나 substitution이 abstraction이 아니라 encryption이었다는 자기 평가에 따라 폐기.

RFC-0001 status를 Draft → Withdrawn (superseded by RFC-OAS-023)로 변경.

### 7.2 RFC-OAS-018 catalog externalization — supplement

RFC-OAS-018의 4-phase plan (closed-sum dispatcher → external catalog)은 *직교* 작업이며 본 RFC와 합쳐서 진행. 단 RFC-OAS-018의 *전제* (catalog의 1차축이 provider)를 본 RFC가 *재조정* — 1차축은 model. RFC-OAS-018 §2 Decision의 catalog schema에서 `provider_id` 1차 키를 `model_id` 1차 키로 변경.

### 7.3 masc-mcp RFC-0174~0177 boundary

masc-mcp는 OAS SDK의 `Provider_kind.t` variant 이름에 의존한다 (RFC-0174~0177 client-agnostic family에서 부호 매핑 적용). 본 RFC의 variant rename은 **breaking change**.

옵션:

- (a) masc-mcp에 *동시 PR*로 rename sweep — 두 repo cross-cut 작업.
- (b) OAS에서 `Provider_kind.t` variant alias 유지 (`Anthropic = Provider_a` deprecated alias) → masc-mcp는 점진 마이그레이션. 단 alias 자체가 cipher 잔존을 의미하므로 6.1의 phased 모드와 정합.

> **[DECISION NEEDED #2]** masc-mcp 동시 처리 정책. masc-mcp 쪽 RFC-0174~0177이 어떤 강도로 부호에 의존하는지 사전 측정 필요.

## 8. Non-goals

- cascade.toml과 OAS catalog 간 *자동 동기화* — OAS catalog가 진실원. cascade.toml은 routing 평면.
- Per-deployment quantization-specific capability tracking — model_caps는 *canonical model*의 사실. quantization-side cap은 transport_caps의 max_context_tokens_cap에 흡수.
- Pricing catalog (RFC-OAS-018 §pricing 별도).

## 9. Open questions / Decision points

| # | Decision | Status |
|---|----------|--------|
| 1 | Single big PR vs phased 5 | NEEDED |
| 2 | masc-mcp 동시 처리 — 동시 PR vs alias 유지 | NEEDED |
| 3 | `capabilities_for_provider_label` 완전 폐지 vs `transport_caps_for_protocol` 으로 재정의 | NEEDED |
| 4 | RFC-OAS-018 Phase 1-4 와의 작업 순서 — 본 RFC 먼저 vs RFC-OAS-018 Phase 1-2 먼저 | NEEDED |
| 5 | Unknown model 시 동작 — hard error vs fallback to model_family_default | NEEDED |
| 6 | Model id 명명 — `kimi-k2.6` (cascade alias 그대로) vs canonical (`moonshot-kimi-k2.6-instruct`) | NEEDED |

---

> Note: 본 RFC는 *skeleton*. §5 Inventory의 audit 결과, §6.3 Test 운영의 baseline 수치, §7.3 masc-mcp 의존 강도 측정이 차후 PR로 채워진다. Decision points는 사용자 검토 후 본문에 inline 결정.
