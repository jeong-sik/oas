# RFC-OAS-035: OpenAI-compat chat-template thinking token injection + empty-completion fail-close

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (Claude Opus 4.8 조사·구현) |
| Created | 2026-07-08 |
| Target | `agent_sdk` (oas) — `lib/llm_provider/` (`backend_openai_serialize.ml`, `backend_openai_request.ml`, `backend_ollama.ml`, `backend_openai_parse.ml`, `http_client.ml`, `complete_sync.ml`, `error.ml`) |
| Supplements | RFC-OAS-029 §thinking/reasoning (thinking control), RFC-OAS-033 (typed-vs-string classifier), RFC-OAS-034 (capability는 model×transport) |
| Boundary | parse 계약 + openai-compat wire serialization 변경. OAS-side only; MASC는 typed outcome을 소비(RFC-OAS-029 §6: OAS는 MASC에 의존하지 않는다). |
| Triggering issue | oas#2483 (blank 200 → Ok content=[] → empty-turn storm), 2026-07-06 오류폭풍 근본 분석 |

## 0. Summary

`thinking_control_format = "chat_template_token"` + `kind = OpenAI_compat` 조합에서 thinking 토글이 **조용히 무시**됐다. `backend_ollama`는 catalog-declared 토큰을 system prompt에 주입해 invariant를 집행하는데, openai-compat request serializer는 동일 토큰을 **주입하지 않았다**(`request_control_fields`가 `Chat_template_token`에 대해 빈 JSON 필드를 반환 — 토큰은 system prompt에 있어야 하는데 그 주입이 빠짐). 결과: 같은 catalog row가 backend에 따라 예외적으로 동작하거나 silent no-op. 토큰이 없으면 모델이 blank-content 200을 합법 반환할 수 있고, 파서는 그것을 `Ok content=[]`로 수용해 downstream(masc)에서 `Response_shape.Empty` → "Provider returned an empty assistant turn" **empty-turn storm**으로 드러났다. runpod_rtxa6000.gemma4-coder-fable5-q4km에서 관측.

두 축으로 fail-closed 한다:

- **A (원인)**: chat-template 토큰 주입을 두 backend의 **공유 SSOT**로 만들어 openai-compat도 대칭 주입 → 토큰 드롭 근절.
- **B (안전망)**: all-empty completion(thinking·text·tool 전무)을 파싱 경계에서 **typed fail-closed outcome**으로 표면화 → 어떤 blank 200이든 silent `Ok content=[]` 대신 typed error.

## 1. Fix A — 대칭 토큰 주입 (SSOT)

`with_chat_template_thinking_token` / `thinking_requested` / `chat_template_thinking_active` / `system_prompt_with_thinking_token`를 `Backend_openai_serialize`로 승격(공유). `backend_openai_request.ml` build_request_assoc의 system-message 조립이 `system_prompt_with_thinking_token ~config ~caps`를 사용, `backend_ollama.ml`은 자체 복사본을 제거하고 공유본 호출.

- **caps-gated**: `caps.thinking_control_format = Chat_template_token _` 인 row만 주입. 비-token 모델(GLM/Kimi/DashScope/plain OpenAI)은 wire byte-identical.
- **think 조건**: `Some true`/`Some false`는 두 backend에서 동일하게 명시적이다. `None`의 기본값은 backend가 소유한다. Ollama만 `OAS_OLLAMA_THINK_DEFAULT`를 읽고, OpenAI-compatible은 다른 provider 이름의 env에 영향받지 않도록 기본 off다.
- **reject 안 함**: 이슈가 제안한 boot-reject(option b)는 채택하지 않음. 주입이 가능해지면 reject는 정상 config를 깨뜨리고, `validate_all`이 kind-agnostic이라 Ollama에도 오발동한다. RFC-OAS-023 DISABLE 가드(complete_common의 `validate_thinking_control_request`)는 직교이므로 유지.

## 2. Fix B — typed empty-completion (parse 경계 fail-closed)

`backend_openai_parse`에 typed 반환을 도입:

```
type empty_completion = { id; model; stop_reason; usage; telemetry }
type parse_error = Provider_error of string | Empty_completion of empty_completion
```

파서 wrapper: `content = thinking @ text @ tool` 가 `[]`이면 `Error (Empty_completion …)`, 아니면 `Ok`. 가드는 **`content=[]`만** — blank text + tool_calls는 `content=[ToolUse ..]`(non-empty)라 Ok 유지(회귀 테스트로 고정). 반환 타입이 `(api_response, string) result` → `(api_response, parse_error) result`로 바뀌며 모든 caller가 컴파일 강제로 새 outcome을 처리(N-of-M 우회 방지).

전파: `http_client.provider_failure_kind`에 `Empty_completion of { stop_reason }` 추가. `complete_sync`가 empty를 `ProviderFailure { Empty_completion }`로 매핑 → `retry_classify`의 `ProviderFailure _ -> None` 규칙으로 **non-retryable**(같은 binding 재시도는 또 empty). `error.of_provider_failure`는 이를 `ProviderUnavailable`(binding-health 성격)로 sdk_error에 투영.

**문자열 분류기 아님**: string sentinel(`Error "empty_completion:…"`)은 RFC-OAS-033/RFC-0042가 금하는 substring 분류기라 채택 안 함. typed variant로 닫음.

## 3. 범위에서 제외 (명시적 후속)

### 3.1 Streaming symmetry (구현됨 — oas#2483 streaming follow-up PR)

streaming 경로도 `Ok content=[]`를 낼 수 있어 non-streaming과 대칭으로 fail-close가 필요했다. 초기 시도는 `complete_stream_acc.finalize_stream_acc`(구조 조립기)에 직접 가드를 넣는 것이었는데, 이때 **21개 단위 테스트가 깨졌다** — 이들은 empty acc를 finalize해 usage/stop_reason plumbing만 검증하는 최소 fixture다. 이 실패가 **경계 오배치의 신호**였다.

**수정된 통찰**: deferral 당시 가설("empty-clean → Ok가 확립된 불변식이라 충돌")은 부정확했다. 그 불변식은 사실 **`finalize_stream_acc`가 순수 구조 조립기**라는 것이다 — 잘 닫힌 스트림을 `Ok content=[]`로 조립하는 건 구조적으로 정당하다(truncated 스트림만 이미 `stream_terminated_without_stop_reason`로 거부). "empty completion은 실행 불가"는 **의미 정책**이고, 이는 완성을 어시스턴트 턴으로 반환하는 **소비 경계**(`lib/streaming.ml` `map_stream_finalize_result`)에 속한다. non-streaming의 `parse_openai_response_result`(정책 wrapper) ↔ raw parser 분리와 정확히 대칭.

**구현**: `map_stream_finalize_result`의 `Ok (Ok resp) when resp.content = []` arm에서 `Stream_parse_failed{reason="empty_completion:<stop_reason>"}`로 fail-close(기존 truncated 거부와 동일 class·경로). 두 production 스트리밍 경로(Anthropic/OpenAI)가 이 어댑터를 공유하므로 단일 편집으로 커버되고, `finalize_stream_acc`는 순수하게 남아 **21개 단위 테스트가 회귀 0**. Custom provider는 sync fallback이라 §2의 non-streaming Fix B로 이미 커버. inline test(`map_stream_finalize_result fails closed on empty completion`) green.

**string 분류기 아님**: reason 문자열은 진단용이고 소비자(`http_error_of_stream_error`)는 이를 opaque하게 http_error로 매핑한다(제어분기 없음). truncated 케이스가 이미 같은 `Stream_parse_failed{reason=…}` idiom을 쓰는 선례.

### 3.2 MASC 소비 (B-full, 별도 repo/PR)

MASC는 OAS SHA를 pin하고 typed outcome을 소비한다(RFC-OAS-029 §6, 단방향). empty-completion을 runtime-binding-health 근거로 소비해 crash-count storm 대신 binding 관점 처리를 하는 것은 MASC PR의 몫이며, **본 OAS 변경이 main에 착지한 뒤** pin bump와 함께 진행한다. MASC-only cooldown을 OAS typed fix 없이 추가하면 CLAUDE.md 워크어라운드 거부 기준(cap/cooldown 증상억제)에 해당하므로, 순서는 OAS(A+B) → MASC(pin+consume)로 강제된다.

**상태 업데이트 (실측)**: A+B는 oas#2488(aad819bb1)로 main 착지, MASC는 pin bump #23682으로 이를 흡수했다. 다만 MASC는 OAS 완성을 `Agent.run`+**streaming**으로 소비하고 OAS 에러 타입을 exhaustive match 없이 **opaque하게**(`provider_failure_to_string`, `sdk_error` 라우팅) 처리하므로, 원래 구상한 "typed Empty_completion을 코드로 소비"(B-full)는 **대부분 불필요**했다 — pin bump가 컴파일을 깨지 않았고, §2의 Fix B는 sync 전용이라 MASC streaming hot path엔 §3.1의 streaming fail-close가 실제 보호막이다. 즉 MASC 측 실효 = pin bump(Fix A 원인수정) + §3.1(streaming empty→provider failure→기존 failover). 별도 MASC 코드 소비는 관측 필요가 확인될 때만 추가한다.

## 4. 검증

- `backend_openai_parse` 회귀: null-content 200 → `Empty_completion`(EndTurn), blank text + tool_calls → `Ok`(content 비어있지 않음). inline test green.
- 두 backend 대칭: openai-compat + Chat_template_token + `enable_thinking=Some true` → system turn이 토큰으로 시작; 비-token 모델 → byte-identical.
- 전 caller 컴파일 통과 + 기존 codec/api/cache 테스트 green (`Error msg` 사이트는 `parse_error_to_string`로 렌더).

## 5. Rollout

A+B는 하나의 OAS PR(#2483 참조)로 착지. release-please가 버전 bump. MASC는 그 main SHA를 `scripts/oas-agent-sdk-pin.sh`로 pin 후 B-full 소비 PR.
