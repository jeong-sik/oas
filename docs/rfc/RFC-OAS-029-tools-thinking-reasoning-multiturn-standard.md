# RFC-OAS-029: Tools / Thinking / Reasoning / Multi-turn usage standard

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (audit: adversarial multi-agent sweep, 2026-06-29) |
| Created | 2026-06-29 |
| Target | `agent_sdk` (oas) — `lib/llm_provider/`, `lib/api_*.ml`, `lib/*tool*.ml`, `lib/streaming.ml`, `docs/design/provider-reasoning-dialects.md` |
| Keystone dependency | RFC-OAS-023 (capability axis reshape) — the GLM/MiniMax dialect work lands there; see §5 |
| Supplements | RFC-OAS-008 (typed tool identification), RFC-OAS-009 (tool name ignorance), RFC-OAS-018 (catalog externalization), RFC-OAS-025 (forced-tool-use enforcement boundary) |
| Boundary | OAS exposes typed provider facts; MASC consumes them. OAS MUST NOT depend on MASC. See §6 |

## 0. Summary (요약)

OAS의 Tools / Thinking / Reasoning / Multi-turn 처리는 **코어는 견고하고 소비자 경계에서 부패**한다. typed dialect 아키텍처(`stop_reason_wire`, `Tool_id`, `Capabilities`/`Reasoning_dialect`의 closed sum types)는 적대적 기준을 통과한다. 위반은 전부 **typed 결정을 string/JSON 휴리스틱으로 재결정하거나 손으로 복제**한 지점에 집중된다.

본 RFC는 그 위반들을 근거로, 기여자(사람 또는 AI 에이전트)가 Tools/Thinking/Reasoning/Multi-turn 코드를 추가·수정할 때 **반드시 만족해야 하는 검증 가능한 불변식(invariant)** 을 확립한다. 각 규칙은 컴파일러 또는 비-vacuous 테스트 또는 CI grep gate로 강제된다. 아키텍처를 버리지 않는다 — 남은 call site를 기존 typed surface로 라우팅하고 dialect gap 2개(GLM, MiniMax)를 닫는 것이 목표다.

근거: 2026-06-29 적대적 다중 에이전트 감사 (5차원 코드 감사 + 6 provider 공식문서 currency + 회의주의 재검증). **확정 위반 23건, 반박/경계-수용 16건.** 발견별 file:line 증거는 §3.

## 1. Verdict — what is strong, what rots

### 1.1 Do not regress (검증된 모범)
- `stop_reason_wire.ml` — wire finish가 typed `wire_finish` + `has_tool_blocks:bool`를 거쳐 매핑되고, `reconcile`은 `Types.stop_reason`에 대해 total(새 variant는 컴파일 깨짐), unknown finish는 raw string을 보존한 채 typed `Unknown`으로 fail-closed. `#2222` infinite-Thinking P0가 cap/string workaround가 아니라 여기서 올바르게 고쳐졌다.
- `Tool_id` (`lib/base/tool_id.ml`) — closed-variant SSOT (RFC-OAS-008 구현). `typed_tool` / `typed_tool_safe` permission layering은 원칙적.
- `reasoning_dialect.ml` / `capabilities.ml`의 dialect 축 — `thinking_control_format`, `preserve_thinking_control_format`, `toggle_wire`, `gemini_family`, `anthropic_thinking_control`, `replay_policy` 모두 closed sum, 대체로 exhaustive. `#2228`은 loose toggle match를 typed preserve 축 추가로 *닫은* 진짜 hardening이다 (workaround 아님).
- Cost는 `annotate_response_cost`로 기록만 되고 `lib/`에서 동작을 gate하지 않는다 — budget 경계가 올바르다. (goal: budget/cost/turn은 집계만.)

### 1.2 Where it rots (위반의 형태)
- **이미 drift한 2중/3중 builder.** OpenAI-compat thinking-request body가 `lib/api_openai.ml`과 `lib/llm_provider/backend_openai_request.ml`에 각각 재구현돼 *실질적으로 갈라짐* (`clear_thinking`이 agent_sdk 경로에선 하드코딩 `true`, 다른 경로에선 config-driven). N-of-M workaround 시그니처가 물질화된 사례 — `#2228`이 preserve 축을 *양쪽 모두*에 추가(41+40줄)했지 통합하지 않았다.
- **typed kind로 1회 승격하지 않고 런타임에 재유도되는 string 분류기.** GLM-ness가 `String.starts_with ~prefix:"glm-"`로 모듈마다 재평가되어 serializer를 분기하고, typed `replay_policy`를 우회한다(GLM은 `No_replay`로 resolve되어 죽은 값).
- **typed 레이어가 제거한 휴리스틱을 다시 들여오는 lenient/repair shell.** `tool_use_recovery.ml`이 자유 텍스트에서 JSON tool call을 긁어내고 `Lenient_json` bracket/keyword completion 후 실행한다 (repair-on-read + JSON 휴리스틱).
- **closed sum이 가능한 자리의 string 판별 + silent `_ -> None` drop.** stream finalizer가 `content_type`을 raw string으로 match하고 catch-all로 unknown/multimodal output block을 조용히 버린다(`Ok` 반환).
- **같은 사실의 두 번째 SSOT.** 중복 stream accumulator(`lib/streaming.ml`, 자체 WORKAROUND 라벨, RFC/removal target 없음)가 reconcile + partial-tool-drop fix를 결여.
- **doc/typed surface가 배포 모델에 뒤처짐.** GLM이 Kimi `No_thinking_control`로 오모델링; MiniMax M2/M3 미커버; Claude 4.8/4.7 `thinking.display=omitted`와 `tool_choice`-forcing-400 미모델; `Reasoning_effort` enum에 stale `Minimal`, `none` 누락.

## 2. The Standard (검증 가능한 불변식)

각 규칙은 위반 시 `dune build` 실패(**Compiler**) / revert 시 red 되는 비-vacuous 테스트(**Test**) / CI grep(**Gate**) 중 하나로 강제되며, 일부 보조 규칙은 리뷰 가이드(**Advisory**)로 작용한다. 강제 경로가 명시된 핵심 규칙은 반드시 해당 메커니즘을 통과해야 한다.

### S1 — 새 model / dialect 추가
- **S1.1 (분류기는 하나).** model id → typed family/kind 변환은 parse 경계(`capabilities.ml`의 `*_of_id`, `provider_config.kind`)에서 **정확히 1회**. downstream은 variant로 switch한다. 그 단일 함수 밖에서 model/provider 이름에 `String.starts_with`/`String.equal` 금지. **Gate**: CI grep — classifier 밖의 `starts_with ~prefix:"glm`/`"gemini`/`"claude`/`"kimi`/`"deepseek`/`"qwen`/`"minimax` 금지.
- **S1.2 (per-model 사실의 SSOT는 catalog).** 수치 한계·pricing·thinking-control class는 `models.toml`/`Model_catalog`에 있고 코드의 prefix table에 없다. catalog에 모델을 추가하면 thinking 축이 catalog에서 나와야 하며 OCaml 편집이 필요 없어야 한다.
- **S1.3 (variant가 구분을 담는다).** sub-capability가 family를 가르면(예: 3.1-pro vs 3.1의 `supports_minimal`) variant에 인코딩한다(`Gemini_3_1 of { is_pro : bool }`). 미래의 분기가 컴파일 타임에 분류기를 깨야 한다. **Compiler**.
- **S1.4 (잘못된 default로 resolve 금지).** dialect 항목이 없는 reasoning 모델은 **fail closed**(`None`/`Unknown`)해야 하고 `No_replay`/`No_thinking_control`로 조용히 resolve되면 안 된다. **Compiler/Test**.

### S2 — Thinking-field 구성
- **S2.1 (builder는 하나).** wire field 이름(`thinking`, `reasoning_effort`, `enable_thinking`, `preserve_thinking`, `thinking_budget`, `clear_thinking`, `chat_template_kwargs`, `thinkingLevel`, `thinkingBudget`, `includeThoughts`)은 typed dialect로 keying된 **정확히 1개 함수**에만 존재한다. 새 format variant는 컴파일 site 1곳만 깨야 한다. Root: `thinking_request_fields : dialect -> Provider_config.t -> (string * Yojson.Safe.t) list`. **Compiler**.
- **S2.2 (budget→effort 매핑은 하나).** `2048`/`8192` 임계값과 `Reasoning_effort.of_budget` 매핑은 `reasoning_effort.ml`에만 산다. backend는 `Reasoning_effort.t`를 소비하고 `Reasoning_dialect.normalize_effort_value`로 wire string을 만든다. backend에 raw 임계 리터럴 금지. **Gate/Test**.
- **S2.3 (effort enum = 현재 wire 어휘).** `Reasoning_effort.t`는 대상 provider가 수락하는 모든 값을 표현하고, 거부하는 값을 표현하지 않아야 한다. 현재 stale `Minimal`을 들고 `None`(OpenAI)/`max`(Anthropic/DeepSeek/GLM)가 없다.

### S3 — Reasoning replay (multi-turn)
- **S3.1 (replay는 typed, 출처 하나).** "이 provider는 reasoning을 replay하는가?"는 `should_replay_reasoning`를 통한 `replay_policy`만이 답한다. serializer가 `~include_reasoning_content:true`를 하드코딩하거나 `config.kind=Glm`/`is_glm_request`로 분기 금지. **Test/Gate**.
- **S3.2 (mandatory-replay provider는 그것을 선언).** Kimi/MiniMax/GLM-preserve/Gemini-tool-signature는 capability record에서 `Preserve_always` 또는 `Drop_without_tool_preserve_with_tool`로 resolve돼야 한다. hard 400 규칙 포함(Anthropic unmodified-blocks, Gemini missing `thoughtSignature`, GLM clear_thinking ordering).
- **S3.3 (history 불변식, repair-on-read 아님).** tool-call/tool-result 인접성은 **append 시점에 강제**(parse-don't-validate)하여 orphan을 표현 불가능하게 만든다. request-shaping filter가 block을 drop해야 한다면 drop된 id를 caller에게 반환한다 — silent filter 금지, drop *counter*를 "fix"로 삼는 것 금지.

### S4 — Tool-call 탐지
- **S4.1 (typed, fail-closed).** "모델이 tool call을 냈는가?"는 `stop_reason_wire.of_finish` / native typed `ToolUse` block이 결정한다. 자유 텍스트 JSON 긁기를 *주* 결정으로 쓰는 것 금지.
- **S4.2 (recovery는 gated, 모호함은 거부).** 비준수 backend(GLM/Ollama)용 text→ToolUse fallback은 typed per-provider parse 경로이거나 명시적 capability flag 뒤에 gated돼야 하고, 모호(>1 candidate)하거나 *repair/truncate*된 JSON은 첫 객체를 승격하지 말고 거부해야 한다. Lenient bracket/keyword completion이 tool 인자를 날조하면 안 된다.
- **S4.3 (untyped == typed).** untyped handler는 typed parser에 위임하고 그 `Error`를 전파한다. "input 전체를 prompt로 직렬화"하는 fallback 금지.

### S5 — Forced tool use
- **S5.1 (forced-tool 제약은 provider별 typed).** `tool_choice` forcing capability는 provider에 대해 exhaustive한 capability 사실이다. 알려진 제약을 런타임 400으로 발견하지 말고 typed 사실로 노출: thinking active인 Anthropic은 `any`/`{tool,name}` 거부; Z.AI/GLM은 `auto`만; MiniMax는 `none`/`auto`만.
- **S5.2 (capability flag와 builder 일치).** `supports_tool_choice=false`면 request builder가 named/`required` `tool_choice`를 내면 안 된다. **Test**.

### S6 — Interleaved / streaming
- **S6.1 (block kind는 closed variant).** `content_type`은 SSE parse 경계에서 **1회** `content_block_kind` sum으로 변환되고 finalizer는 그 variant를 exhaustive match한다. unknown kind는 `SSEUnknownEventType` → `finalize`가 `Error` 반환, `_ -> None` 금지. **Compiler**.
- **S6.2 (accumulator는 하나).** stream accumulator는 `Complete_stream_acc` 하나. 보조 surface는 그것을 거친다(reconcile + partial-tool drop + reasoning visibility). 중복 `stream_acc`/`finalize_stream_acc` 금지.
- **S6.3 (parser가 dialect를 읽는다).** streamed reasoning delta field는 `dialect.streaming`(`Delta_field`/`Template_parser`)에서 읽는다, 하드코딩된 `reasoning_content`→`reasoning` 우선순위가 아니라. `streaming` 필드에 live reader가 있어야 한다.
- **S6.4 (interleave 충실도).** per-block stream index는 think→text→think 사이에서 reset돼야 interleaved block이 collapse/reorder되지 않는다.
- **S6.5 (signature는 전용 필드).** thinking signature는 `signature : string option`으로 block-subtype tag와 분리. no-signature default가 두 accumulator에서 동일하도록 finalize 공유.

### S7 — Multimodal
- **S7.1 (source kind는 closed sum).** `source_type`은 `Base64 | Url | File_id | …`; 모든 backend가 exhaustive match. backend가 지원 않는 source는 컴파일 gap 또는 명시적 `Error`, base64 가정 silent 금지. **Compiler**.
- **S7.2 (media→empty-text flatten 금지).** synthetic/stream surface는 충실한 media event를 내거나 media를 명시적으로 거부한다. `Image/Audio/Document`를 empty `text`로 relabel 금지.

### S8 — Unknown-input 처리 (교차 절단 기준)
- **S8.1.** unknown enum/schema/dialect 입력은 `Error`/`None`/`Unknown`으로 노출, 편의 default 금지. `unsupported_type, _ -> true` 금지.
- **S8.2.** contract상 required(`required:true`) tool 인자가 누락/malformed면 typed validation `Error`, magic default 금지.
- **S8.3.** unknown-but-named variant 분기는 warn(`warn_unknown_capability_value` 미러)하고 forward-compatible wire shape를 선호한다(예: Gemini deprecated `thinkingBudget`보다 `thinkingLevel`).

### S9 — Capabilities SSOT
- **S9.1 (사실당 registry 하나).** 같은 model 사실의 두 registry 금지(하나가 다른 하나에서 *증명 가능하게 유도*되지 않는 한). provider preset은 protocol/flag default만 보유 — model-version 수치 ceiling 금지(catalog에 있음; 부재 ⇒ `Unknown_limit`).
- **S9.2 (dead/duplicate typed path 금지).** 자체 테스트로만 살아있는 exported 함수 금지; 두 모듈의 byte-identical helper 금지.
- **S9.3 (precedence는 의도적·테스트됨).** capability source precedence(catalog vs host manifest vs preset)는 1회 결정, `.mli` 전반에 일관 문서화, 테스트로 고정.

### S10 — Observability / determinism
- **S10.1 (정직한 계약).** "Pure"로 문서화된 모듈은 pure여야 한다; 효과(wall-clock, mutable global)는 경계로 옮기거나 `.mli`에 문서화. recovered id는 결정론적(block index + content hash) 또는 주입된 generator로 유도.
- **S10.2 (데이터 손실은 관측되되, 관측이 fix는 아니다).** block을 drop하면 `repair_dangling_tool_calls`가 synthesized block에 태그하듯 태그한다. counter/log는 typed fix와 함께하는 *alarm*으로만 허용, fix 자체로는 금지(telemetry-as-fix = reject 시그니처).

## 3. Evidence — confirmed violations (23 confirmed, 16 refuted/boundary-acceptable)

| Sev | ID | Principle | File:line | Standard |
|---|---|---|---|---|
| P1 | D1-dup-thinking-builder-glm-drift | ssot | api_openai.ml:232-309; backend_openai_request.ml:300-358 | S2.1 |
| P1 | D1-glm-replay-hardcoded-heuristic | heuristic | backend_openai_serialize.ml:228-246; api_openai.ml:193-195; backend_openai_request.ml:209-223 | S3.1 |
| P1 | D3-finalize-content-type-string-catchall-silent-drop | string_match/silent | complete_stream_acc.ml:145-214 | S6.1 |
| P2 | D6-glm-identity-string-classifier-scattered | string_match | zai_catalog.ml:11-13; backend_glm.ml:92-136; backend_openai_request.ml:163-357 | S1.1, S9.2 |
| P2 | D2-budget-to-effort-triplicated | ssot/hardcode | reasoning_effort.ml:26-33; backend_anthropic.ml:48-53; backend_gemini.ml:45-50 | S2.2 |
| P2 | D5-anthropic-thinkmode-hardcoded-prefix-table | hardcode | capabilities.ml:182-221 | S1.2 |
| P2 | D4-provider-preset-stale-numeric-limits | hardcode | capabilities.ml:223-255; provider_registry.ml:408; builder.ml:256 | S9.1 |
| P2 | D2-streaming-reasoning-dialect-dead-and-field-guess | ssot | reasoning_dialect.ml:39-42; streaming.ml:331-335; backend_openai_parse.ml:208-298 | S6.3 |
| P2 | D4-duplicate-stream-accumulator-missing-reconcile | ssot | streaming.ml:16-215 | S6.2 |
| P2 | D-TOOLS-1-recovery-text-scrape-heuristic | heuristic | tool_use_recovery.ml:32-237 | S4.2 |
| P2 | D-TOOLS-6-agent_tool-untyped-silent-prompt-fallback | silent_failure | agent_tool.ml:149-161 | S4.3 |
| P2 | D-TOOLS-9-harness-unknown-schema-type-permissive | silent_failure | backend_tool_call_harness.ml:52-68 | S8.1 |
| P2 | D6-source-type-ignored-non-anthropic | string_match/silent | backend_openai_serialize.ml:60-82; backend_gemini.ml:161-174; backend_openai_responses.ml:121-137 | S7.1 |
| P2 | D5-synthetic-events-multimodal-silent-drop | silent_failure | streaming.ml:193-217 | S7.2 |
| P2 | D7-thinking-signature-overloaded-string | string_match | complete_stream_acc.ml:147-153; streaming.ml:151-155 | S6.5 |
| P2 | D4-dead-string-normalize-effort | string_match | reasoning_dialect.ml:271-280 (+mli:96) | S9.2 |
| P3 | D7-gemini-family-leaks-second-string-match | string_match | capabilities.ml:442-463 | S1.3 |
| P3 | D-TOOLS-8-recovery-impure-nondeterministic-id | mutable | tool_use_recovery.ml:11-12,149-157 | S10.1 |
| P3 | D3-tool-pair-silent-drop | silent_failure | tool_message_pairs.ml:55-111 | S3.3 |
| P3 | D4-budget-magic-defaults-silent | hardcode/silent | agent_turn_budget.ml:97-117 | S8.2 |
| P3 | D7-anthropic-prefix-list-literal-duplicates | hardcode | capabilities.ml:189-217 | S1.2 |
| P3 | D8-manifest-cannot-override-catalog-precedence | ssot | capabilities.ml:826-839 | S9.3 |

### 3b. Doc-currency drifts (official 2026-06-29 docs vs OAS)

| Sev | Provider / field | OAS now | Official | Standard |
|---|---|---|---|---|
| P1 | **GLM dialect** (recurs ×4 sources) | Kimi `No_thinking_control` | top-level `thinking:{type,clear_thinking}`, `reasoning_content` side-channel, GLM-5.2 `reasoning_effort` (default `max`), mandatory unmodified ordered replay when `clear_thinking=false` | S1.4, S3.2 |
| P1 | **MiniMax M2/M3** (recurs) | uncovered → `No_replay` default | always-on thinking, mandatory reasoning replay (`reasoning_details`/`thinking_blocks`/`<think>`), `reasoning_split` toggle, Anthropic-compat recommended; tool_choice none/auto; image+video in, no audio | S1.4 |
| P1 | **Anthropic `thinking.display`** | never emitted | default `omitted` on Opus 4.8/4.7/Fable5/Mythos5 (empty thinking, signature only); `summarized` needed for text | S8.3 |
| P1 | **Anthropic tool_choice vs thinking** | forced tool_choice unguarded | `any`/`{tool,name}` ⇒ 400 when thinking active | S5.1 |
| P1 | **OpenAI `reasoning_effort` enum** | `Minimal\|Low\|Medium\|High\|XHigh`, no `None` | GPT-5.5/5.1 = `none/low/medium/high/xhigh`; `minimal` removed | S2.3 |
| P1 | **OpenAI replay policy** | "No mandatory replay yet" | reasoning items MUST replay with tool-call outputs (Responses) or `previous_response_id` | S3.2 |
| P2 | Gemini `thoughtSignature` | "soft preserve", summaries/signatures conflated | hard 400 if not echoed; parallel = first part only; signatures ≠ summaries | S3.2, S6.5 |
| P2 | Gemini `thinkingLevel` matrix | `supports_minimal:bool` only | low/medium/high; medium absent on gemini-3-pro; minimal Flash-only | S1.3 |
| P2 | Qwen DashScope `preserve_thinking` scope | applied to all DashScope | allowlist only | S1.2 |
| P2 | Kimi visibility | `Provider_hidden`+`No_streaming_reasoning` | `reasoning_content` side-channel, streamed before content | S3.2 |
| P2 | OpenAI Responses `phase` | not modeled | `phase:commentary/final_answer` round-trips on stateless replay | S3.2 |

Full per-finding verify reasoning and source URLs: audit artifact `wf_ad6e7c0c-aff` (2026-06-29), 51 agents, 6 provider docs scans. (References: [Anthropic Tool Choice](https://docs.anthropic.com/en/docs/tool-use#tool-choice), [Gemini Reasoning](https://ai.google.dev/gemini-api/docs/reasoning), [OpenAI Reasoning](https://platform.openai.com/docs/guides/reasoning), [GLM Dev](https://open.bigmodel.cn/dev/api/normal-model/glm-4))

## 4. Enforcement (강제 방법)

표준을 사람의 선의에 맡기지 않는다. 메커니즘:

1. **Compiler** — S1.3/S1.4/S2.1/S6.1/S7.1은 closed sum + exhaustive match로 표현. 새 variant/format이 컴파일을 깨야 한다. (`_ -> ...` catch-all 추가는 CLAUDE.md 워크어라운드 체크리스트 4번에 걸린다.)
2. **CI grep gate** — S1.1/S2.2/S3.1: classifier 함수 밖의 model-name `String.starts_with`/raw threshold literal/`is_glm_request` 패턴을 거부하는 grep 단계를 `.github/workflows/ci.yml`에 추가. (이미 있는 `util-ci-substring-str` 작업과 정렬.)
3. **Non-vacuous test** — S5.2/S6.2/S9.3: revert 시 red 되는 테스트. (예: `supports_tool_choice=false`인 GLM 요청 body에 `tool_choice`가 없음을 단언.)
4. **Workaround-signature gate** — §5의 remediation을 그 workaround twin으로 구현하는 PR은 `scripts/ci/pr-rfc-check.sh`의 시그니처에 걸려 거부된다. counter-as-fix / string-classifier 보강 / N-of-M / cap-cooldown-dedup-repair 금지.

## 5. Remediation backlog + sequencing

RFC 컬럼: **RFC** = dialect/capability *type shape* 변경 또는 N-of-M reshape(workaround 게이트가 RFC 요구); **Direct** = 순수 삭제/dedup/위임(시그니처 트리거 없음). 키스톤은 **RFC-OAS-023**.

### 먼저 (keystone, 가장 많이 unblock)
1. **RFC-OAS-023 — GLM typed dialect reshape.** GLM-ness를 typed kind/capability로 1회 승격, `replay_policy`와 `Thinking_object`-style thinking-control variant 부여, 그 다음 2중/3중 thinking builder(S2.1)와 2중 `clear_thinking` helper(S9.2) 통합. 이 reshape 하나가 `D1-glm-replay-hardcoded-heuristic`(P1), `D6`(P2), GLM-row doc gap(P1×4), GLM effort/tool_choice/caps drift를 닫고 `is_glm_request` string fork를 제거한다. `D1-dup-thinking-builder-glm-drift`가 *re-drift 없이* 고쳐지려면 통합 builder가 string이 아니라 typed GLM dialect로 switch해야 하므로 이게 선행조건.
2. **thinking-request builder 통합 (D1-dup, P1)** — (1) 직후/내부. `thinking_request_fields` 하나로. GLM clear_thinking drift는 agent_sdk 경로의 live wire-byte 정합성 버그.
3. **content_type closed variant at stream boundary (D3-finalize, P1)** — GLM과 독립, streaming blast radius 최대(server-tool/multimodal output block의 silent drop을 `Ok`로 반환). `content_block_kind` 도입, 1회 변환, unknown fail closed.

### 다음 (배포/사용 surface의 정합성 drift)
4. OpenAI enum + replay drift (P1×2) — `none` 추가/`minimal` 제거, tool-turn mandatory replay. GPT-5.5 multi-turn tool loop에 현재 영향.
5. Anthropic `thinking.display` + `tool_choice`-400 (P1×2) — Opus 4.8/4.7 타깃이 빈 reasoning / forced tool에서 hard-400.
6. MiniMax provider 추가 (P1) — 현재 `No_replay` default가 interleaved thinking을 조용히 깨뜨림.
7. 중복 stream accumulator 제거 (D4, P2) — 삭제 후 `Complete_stream_acc`로 라우팅; RFC/removal target 없는 자체 WORKAROUND 라벨(상시 프로세스 위반).

### 미뤄도 안전 (latent, 현재 배포 모델 트리거 없음) — typed cleanup으로 batch
- `D2-budget-to-effort-triplicated`, `D5-anthropic-thinkmode-hardcoded-prefix-table`, `D4-provider-preset-stale-numeric-limits` (SSOT/hardcode 부채; 현재 값이 일치해 active break 없음 — catalog-field RFC로 fold).
- **Direct, RFC 불필요, 저위험 (언제든)**: `D-TOOLS-6`(agent_tool 위임), `D-TOOLS-9`(harness fail-closed), `D4-dead-string-normalize-effort`(삭제), `D7-anthropic-prefix-list-literal-duplicates`(dedupe), `D-TOOLS-8`(id 결정론), `D4-budget-magic-defaults-silent`, `D8-manifest-precedence` 문서/테스트, Kimi visibility 사실.
- `D7-gemini-family-leaks-second-string-match`(P3) + Gemini `supports_medium`/`thoughtSignature` strictness: 단일 Gemini variant reshape로 fold.

### Backlog 자체의 가드레일
여러 "root fix"는 그 workaround twin으로 구현하면 안 된다. `D3-tool-pair-silent-drop`은 append-time 불변식 또는 drop된 id 반환으로 고친다 — drop counter 아님(telemetry-as-fix = reject). `D-TOOLS-1`은 typed provider parse 경로로 고친다 — lenient repair 강화 아님.

## 6. Boundary note (OAS ↔ MASC)

경계는 대체로 올바르다: MASC는 `Llm_provider.Capabilities`를 typed로 직접 소비하고(`runtime_wire_overlay.ml: agent_capabilities_of_llm_capabilities`가 OAS variant를 verbatim 통과) model 이름 string-match로 reasoning을 결정하지 않는다. OAS는 MASC를 모른다.

단 하나의 경계 부채(MASC측, 정보용): `masc lib/runtime/runtime_schema.ml`이 자체 `thinking_control_format`를 **재선언(5/7 variant, `Thinking_object_only`/`Enable_thinking` 누락)** 한다. parse는 unknown에서 fail-closed(silent 아님)지만, OAS에 8번째 variant가 추가돼도 MASC 컴파일이 깨지지 않아 drift 무방비다. 게다가 그 필드는 wire 경로에서 읽히지 않아 운영자 TOML 설정이 inert no-op(의도-침묵)이다. **P2 SSOT 부채(데이터 경로는 안전).** 해결: 필드 삭제(OAS catalog가 단일 SSOT) 또는 OAS variant 집합에 대한 exhaustive drift 테스트. 이는 OAS 변경이 아니라 MASC 후속 작업이며, 본 RFC의 S9.1을 경계 너머로 확장한 사례로 기록한다.

## 7. Relationships
- **RFC-OAS-023** (capability axis reshape) — GLM/MiniMax dialect 작업과 model×transport two-record가 여기 land. 본 RFC는 그 작업이 만족해야 할 표준을 정의한다.
- **RFC-OAS-008/009** (typed tool id / tool name ignorance) — S4의 typed tool 기반.
- **RFC-OAS-018** (catalog externalization) — S1.2/S9.1의 catalog-as-SSOT 기반.
- **RFC-OAS-025** (forced-tool-use enforcement boundary) — S5의 기반.
- **CLAUDE.md 워크어라운드 거부 기준** — S10.2/§4.4의 enforcement 원천.
