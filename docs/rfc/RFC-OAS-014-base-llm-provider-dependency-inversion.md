# RFC-OAS-014: base ↔ llm_provider 의존성 역전 (Foundation 추출)

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (with Claude analysis) |
| Created | 2026-06-28 |
| Target | `agent_sdk` (oas) |
| Supersedes | None |
| Sibling | `docs/rfc/sub-library-decomposition-rfc.md` (Phase 0 정밀화) |
| Related | RFC-OAS-015 (mutable cleanup), RFC-OAS-018 (provider/model catalog externalization) |
| Trigger PR | OAS #2212 (`perf(util): use Str literal search helpers`) — string_contains 3곳 중복의 SSOT 불가 |

## 0. Summary

`agent_sdk.base`(`lib/base`, public_name `agent_sdk.base`)가 `llm_provider`에 의존한다. "base"라는 이름이 최하위를 암시하지만 실제로는 `llm_provider` 없이 빌드/사용이 불가하다. 의존 방향과 모듈 이름이 모순되는 구조적 냄새이며, 그 증상이 `string_contains` 3곳 중복(`lib/base/util.ml`, `lib/llm_provider/cli_common_env.ml`, `lib/llm_provider/pricing.ml`)으로 발현한다. `llm_provider`는 base를 import할 수 없다(circular) → SSOT 불가 → 중복이 구조적 필연.

본 RFC는 **제3의 최하위 라이브러리 `agent_sdk.foundation`**을 신설하고, LLM-agnostic leaf 모듈(`Cli_common_env`, `Retry`, `Error.provider_error`, `Types`, `Request_priority`, `Pricing`)의 소유권을 `llm_provider`에서 `foundation`으로 옮겨 `base`와 `llm_provider` 모두 `foundation`에 의존하게 한다. 점진적 PR 분할이 가능하며 flip big-bang이 불필요하다.

`sub-library-decomposition-rfc.md` Phase 0("Extract `agent_sdk.base` — Types, Error, Result_syntax")의 정밀화:
- 기존 RFC는 types 추출을 제안하나 "no internal dependencies beyond `llm_provider`"라 명시해 **llm_provider 의존 잔존을 방치**. 본 RFC는 types와 함께 `Cli_common_env`/`Retry`/`Pricing`/`Request_priority`까지 foundation으로 옮겨 의존을 끊는다.
- 기존 RFC Risk("Types re-export: `include Llm_provider.Types` means sub-libraries need both `agent_sdk.types` and `llm_provider`")를 foundation 단일 의존으로 해소.

## 1. Problem Statement

### 1.1 측정된 사실 (main `83d89cda7`, 2026-06-28)

- `lib/base/dune`: `(libraries llm_provider yojson eio)` — **base → llm_provider** 단방향, 순환 없음.
- `lib/llm_provider/dune`: base 의존 0건. `llm_provider`는 base를 import할 수 없다.
- base → llm_provider 참조 전수:
  - `lib/base/types.ml:14` `include Llm_provider.Types`
  - `lib/base/types.ml:163` `priority : Llm_provider.Request_priority.t option`
  - `lib/base/error.ml:11` `module Retry = Llm_provider.Retry`
  - `lib/base/error.ml:17` `type provider_error = Llm_provider.Error.provider_error`
  - `lib/base/error.ml:216,230` `Llm_provider.Error.to_string`, `Llm_provider.Error.is_retryable`
  - `lib/base/util.ml:95,146` `Llm_provider.Cli_common_env.get`, `.int`
  - `lib/base/model_registry.ml:10` `Llm_provider.Cli_common_env.get "OAS_DEFAULT_MODEL"`
- llm_provider 내부 참조 통계: `Types` 22파일, `Cli_common_env` 21 call site.
- `string_contains` 3곳 중복: `lib/base/util.ml:13`, `lib/llm_provider/cli_common_env.ml:141`, `lib/llm_provider/pricing.ml:54`. #2212가 각 로컬 정의를 `Str` 교체했으나 SSOT 통합은 하지 않는다. SSOT 불가의 원인은 위 순환 제약.

### 1.2 왜 base로 옮기는 방향이 아닌가

"types를 base로 옮겨 역전" 방향은 **flip big-bang**이 된다. base가 llm_provider에 의존 중인 상태에서 모듈을 base로 옮기고 llm_provider가 base alias를 import하면 역순환이 발생해 dune 빌드가 불가하다. alias는 base→llm_provider 간선이 완전히 제거된 flip 순간에만 도입 가능하며, 그 flip은 base/llm_provider 양쪽 동시 변경의 단일 컷이 된다(Plan agent 검증, 본 RFC 부록 A).

제3의 `foundation` 라이브러리는 이 제약이 없다. 매 PR마다 한 모듈을 foundation으로 옮기고 base와 llm_provider가 동시에 foundation을 import → 같은 라이브러리 모듈이라 타입 identity 자동 보존, 순환 없음, 독립 머지 가능.

## 2. Target Architecture

```
agent_sdk.foundation  ← (LLM-agnostic leaf: Cli_common_env, Retry, Error, Types, Request_priority, Pricing)
    ↑
agent_sdk.base        (agent-specific: model_registry, agent_config, agent_state, Util, Hooks, Tool, Guardrails)
    ↑
agent_sdk.llm_provider (backend impl: anthropic/openai/ollama clients, streaming, cache)
    ↑
agent_sdk             (façade: re-export ABI)
```

`base`는 `foundation`과 `llm_provider` 모두에 의존 가능(점진적 전환 중). 최종 상태에서 `base`는 `foundation`만, `llm_provider`는 `foundation` + `base`에 의존. `base → llm_provider` 간선 제거.

## 3. ABI 보존 전략

`lib/agent_sdk.ml` 공개 재수출(`Types`, `Retry`, `Error`, `Util`, `Model_registry`, `Sse_parser` 등) 경로 유지. foundation으로 모듈이 옮겨도:

- `Agent_sdk_base.Types` → foundation에 정의, base가 `module Types = Foundation.Types` 재수출. `agent_sdk.ml: module Types = Agent_sdk_base.Types` unchanged.
- `Llm_provider.Types` → `module Types = Foundation.Types` alias. `agent_sdk.ml`이 `Llm_provider.Types`를 직접 쓰지 않으므로 외부 ABI 영향 0.
- OCaml module alias는 구조적 동일 타입을 가리키므로 `Agent_sdk.Types.message = Foundation.Types.message = Llm_provider.Types.message` identity 보존.

`agent_sdk.mli` 공개 표면 byte-identical 목표. 매 PR별 `dune build` + `@runtest` CI green.

## 4. 다단계 PR 분할 (점진적, flip 불필요)

각 PR은 foundation에 모듈을 추가 + base/llm_provider 양쪡 caller를 foundation import로 전환. 순환 없이 독립 머지.

### PR-1: foundation 신설 + `Cli_common_env` 이관

- 신규 `lib/foundation/` 라이브러리(`public_name agent_sdk.foundation`, `(wrapped true)`). `cli_common_env.ml/.mli`를 llm_provider에서 foundation으로 이동.
- `lib/base/dune`: `(libraries foundation llm_provider yojson eio)` 추가. `util.ml:95,146`, `model_registry.ml:10`을 `Foundation.Cli_common_env.*`로 전환.
- `lib/llm_provider/dune`: `(libraries ... foundation ...)` 추가. `cli_common_env.ml`을 `module Cli_common_env = Foundation.Cli_common_env` alias로 교체(또는 파일 제거 + caller를 `Foundation.Cli_common_env`로). llm_provider 내 21 call site는 alias로 자동 해결.
- 순환: foundation은 최하위, base/llm_provider 모두 foundation 의존. base→llm_provider 간선은 Cli_common_env 제외한 types/error 잔존. **순환 없음.**

### PR-2: `Retry` + `Error.provider_error` 이관

- `retry.ml/.mli`, `error.ml`의 `provider_error` 정의를 foundation으로.
- `lib/base/error.ml:11,17,216,230`을 `Foundation.Retry`, `Foundation.Error`로 전환.
- `lib/llm_provider/error.ml`은 foundation.Retry import. llm_provider 내부 Error 사용 0건(탐사 확정)이므로 회귀 좁음.
- base→llm_provider 간선은 types만 잔존. **순환 없음.**

### PR-3: `Types` + `Request_priority` 이관 (가장 큼)

- `lib/llm_provider/types.ml`의 role/message/content_block 정의를 `lib/foundation/types.ml`로 이동. `Request_priority`도 함께.
- `lib/base/types.ml:14` `include Llm_provider.Types` → `include Foundation.Types`. `:163` `Foundation.Request_priority.t`.
- `lib/llm_provider/types.ml` → `module Types = Foundation.Types` alias. llm_provider 내 22개 Types 참조 파일은 `Types.message` = `Llm_provider.Types.message` = `Foundation.Types.message` alias로 자동 해결(caller 수정 0건).
- base→llm_provider 간선 제거 완료. **순환 없음.** base는 foundation만 의존.

### PR-4: `Pricing` 이관 (flip 이후가 아닌 동일 패턴)

- `lib/llm_provider/pricing.ml`을 foundation으로. `lib/api.ml`, `lib/streaming.ml`, `lib/provider.ml`, `lib/llm_provider/complete.ml` caller는 `Foundation.Pricing` 또는 `Llm_provider.Pricing` alias로.
- Pricing은 foundation의 Types에 의존(동일 라이브러리). **순환 없음.**

### PR-5: `string_contains` SSOT 통합 (#2212 흡수)

- llm_provider가 `Foundation`(또는 base.Util)을 import 가능해진 상태에서 `cli_common_env.ml:141`, `pricing.ml:54` 로컬 `string_contains` 제거. `Foundation.Util.string_contains`(또는 base.Util) SSOT 사용.
- `lib/base/util.ml:13`의 정의를 foundation으로 올리거나 base.Util 유지 + llm_provider가 base.Util import. 설계 결정: foundation에 `Util` 최소 string 유틸 추가.
- `%test` 8개(pricing.ml:560-580) 이관.

## 5. 회귀 위험

- **PR-2**: base.Error.provider_error와 Llm_provider.Error.provider_error가 동일 foundation 타입이므로 분리 위험 없음(별도 라이브러리 방향의 이점). 단 base에서 api_error를 llm_provider로 넘기는 call path(`lib/api.ml`, `lib/agent*.ml`) 전수 조사 권장.
- **PR-3**: Types 이동 시 base.Types ↔ llm_provider.Types 전달 경로. foundation alias로 동일 identity이므로 파열 없음. 단 22개 참조 파일 컴파일 회귀 감시.
- **Pricing caller**: `lib/provider.ml:507` `type pricing = Llm_provider.Pricing.pricing =` 재 alias — foundation 이동 후 identity 보존 점검.

## 6. 비판적 위험 (명시)

- **비용/이득 비대칭**: foundation 추출은 5 PR + 신규 라이브러리. string_contains 하나의 이익을 초과하는 비용. 정당성은 의존 방향-이름 모순 해소 + 순환 잠재 제거 + base 독립 사용 가능에 있으나, 실사용 관점에서 base만 단독 링크하는 consumer가 거의 없으므로 런타임 이점은 제한적. 주 이득은 구조적 청결성 + 후속 sub-library decomposition Phase 언블록.
- **기존 RFC와 관계**: 본 RFC는 `sub-library-decomposition-rfc.md` Phase 0의 정밀화(sibling). 기존 RFC가 다루지 않은 Cli_common_env/Retry/Pricing/Request_priority를 보충하고, llm_provider 의존 잔존을 방치하지 않는다. 기존 RFC의 별도 라이브러리 방향을 계승.
- **대안 기각 근거**: string_contains만 SSOT로 족하다면 `agent_sdk.string_utils` 단일 라이브러리가 단일 PR 저비용. 그러나 façade 재수출 체계 전체(base가 llm_provider에 의존)가 구조적 냄새이므로 foundation 추출로 근본 해소. 단일 string_utils는 증상만 치료.

## 7. Verification

- 매 PR: `dune build` + `@runtest` CI green.
- `agent_sdk.mli` 공개 표면 byte-identical (`diff` 기반 회귀).
- PR-3 후 타입 identity 단위 테스트: `Agent_sdk.Types.message = Llm_provider.Types.message = Foundation.Types.message`.
- PR-5 후 `string_contains` 정의 단일화 확인: `rg "let string_contains" lib/` = 1건.
- 최종: `lib/base/dune`의 `(libraries ...)`에 `llm_provider` 부재 확인.

## 부록 A: "base로 이동" 방향의 flip big-bang 근거 (Plan agent 검증)

base→llm_provider 간선 잔존 시 llm_provider 측 `module M = Agent_sdk_base.M` alias는 항상 역간선을 만들어 dune 빌드 불가. alias는 flip 순간(base→llm_provider 간선 완전 제거)에만 도입 가능. PR-1~4 단독으로는 전부 순환 발생. 따라서 "base로 이동" 방향은 flip이 base/llm_provider 양쪽 동시 변경의 단일 컷(big-bang)이 되며 점진적 역전이 구조적으로 불가능. 본 RFC가 foundation 제3 라이브러리 방향을 채택한 근거.

## 참조

- 탐사 근거: `lib/base/dune`, `lib/llm_provider/dune`, `lib/base/types.ml:14,163`, `lib/base/error.ml:11,17,216,230`, `lib/base/util.ml:95,146`, `lib/base/model_registry.ml:10`, `lib/agent_sdk.ml:44-78`
- 기존 RFC: `docs/rfc/sub-library-decomposition-rfc.md` Phase 0
- Trigger: OAS #2212 (Str 교체), #2213 (call-time default model resolver)