# RFC-OAS-039: Structured output as a typed strategy, not a boolean

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (Claude Opus 4.8 조사·실측) |
| Created | 2026-07-22 |
| Target | `agent_sdk` (oas) — `lib/structured.ml`, `lib/llm_provider/{provider_config,capabilities,backend_*}.ml` |
| Supplements | RFC-OAS-023 (capability axis = model × transport), RFC-OAS-034 (capability provenance = 선언) |
| Related issues | #2746 (OpenAI provider identity 부재), #2747 (Gemini wire drift), #2748 (dead override), #2751 (추출 경계), #2752 (Cohere/Mistral/Kimi 선언 드리프트), #1925 (Kimi 게이트 모순), #2255 (GLM forced tool_choice) |
| Related PR | #2745 (OpenAI strict 투영 — 본 RFC 의 전제 조건) |

## 0. Summary

> `Structured.extract` 는 "provider 가 native json_schema 를 지원하면 쓰고, 아니면 실패한다" 는 이분법 위에 서 있다. 그 결과 native 를 지원하지 않는 provider (GLM, DeepSeek, Cohere, MiniMax, Ollama Cloud) 에서는 **구조화 출력을 얻을 수단이 아예 없다**. 이 RFC 는 이분법을 닫힌 합타입 **전략**으로 바꾼다. 전략은 선언된 capability 로부터 결정론적으로 *선택*되며(런타임 폴백 캐스케이드가 아니다), 어떤 전략이 쓰였는지는 receipt 로 관측 가능하고, 실패는 타입화된 3분법으로 보고된다.

## 1. Problem

### 1.1 현재 계약

```ocaml
(* lib/llm_provider/provider_config.ml:635-648 *)
let validate_output_schema_request (config : t) =
  match structured_schema_requested config with
  | false -> Ok ()
  | true ->
    (match config.kind with
     | Gemini | Anthropic | DashScope -> Ok ()          (* 무검사 통과 *)
     | Ollama | Kimi | OpenAI_compat ->
       validate_model_structured_output_capability config
     | Glm -> Error "Glm supports JSON mode (json_object) only; ...")
```

판정 정책이 provider kind 별로 세 갈래다 — 무검사 통과 / capability 검사 / 하드 리젝트. 이 셋의 차이를 정당화하는 것은 각 provider 의 *native 필드 유무* 하나뿐이며, native 가 없다는 사실이 곧 "구조화 출력 불가" 로 번역되어 있다. 그 번역이 틀렸다.

### 1.2 실측 — native 부재는 구조화 출력 불가와 동치가 아니다

2026-07-22 공식 문서 전수 재확인 결과:

| Provider | native json_schema | JSON mode | tools | forced named tool |
|---|---|---|---|---|
| OpenAI | ✅ `response_format.json_schema` / `text.format` | ✅ | ✅ | ✅ |
| Anthropic | ✅ `output_config.format` (GA, beta 헤더 불요) | ❌ | ✅ | ✅ |
| Gemini | ✅ (필드명 이전 중 — #2747) | ✅ | ✅ | ✅ |
| Ollama (local) | ✅ `format` | ✅ | ✅ | ❌ |
| xAI | ✅ (`strict` 는 no-op, `additionalProperties` 기본 반전) | ✅ | ✅ | ✅ |
| Mistral | ✅ (`strict` 기본 false — 명시 필요) | ✅ | ✅ | ✅ |
| Kimi (platform) | ✅ (`strict` 기본 **true**, MFJS 규격) | ✅ | ⚠️ | ⚠️ |
| Kimi (`/v1/messages` coding) | ❌ (필드 무시, 2026-07-03 측정) | ❌ | ✅ | ✅ |
| DashScope / Qwen | ⚠️ 확인 필요 | ✅ | ✅ | ✅ |
| **GLM / Z.AI** | ❌ (`response_format` enum 은 `[text, json_object]` 뿐) | ✅ | ✅ | ❌ (#2255) |
| **DeepSeek** | ❌ (`response_format` 은 `json_object` 만) | ✅ | ✅ + `function.strict` (Beta) | ✅ |
| **Cohere** | ❌ (스키마가 `json_object` **안에** 실림) | ✅ | ✅ (단, `response_format` 과 **동시 사용 금지**) | ❌ (`tool_choice` 없음) |
| **MiniMax** | ❌ (deprecated 엔드포인트 + Text-01 한정) | ⚠️ | ✅ | ⚠️ |
| **Ollama Cloud** | ❌ (*"Ollama's Cloud currently does not support structured outputs."*) | ✅ | ✅ | ❌ |

굵게 표시된 5개 provider 는 native 가 없지만 **전부 tools 를 지원한다**. 툴의 `input_schema` 는 곧 JSON Schema 이고, 대부분의 provider 는 툴 인자에 대해 native 출력과 동등하거나 더 강한 제약을 건다 (DeepSeek 은 `function.strict = true` 로 서버측 스키마 검증까지 한다). 즉 **구조화 출력을 얻는 경로는 존재하는데 OAS 가 그 경로를 쓰지 않는다.**

`lib/structured.ml` 은 그 경로의 코드를 이미 가지고 있다 — `extract_tool_input` (`structured.ml:46-71`). 그런데 `extract` / `extract_stream` 은 `tool_choice = None` 과 `~tools:[]` 로 요청을 만들기 때문에 **도달 불가능하다**. repo 전체에서 non-test 호출자가 0건이다.

### 1.3 다른 SDK 들의 수렴 (2026-07-22 확인)

| 구현 | 전략 열거 | 선택 방식 | 실패 시 |
|---|---|---|---|
| Vercel AI SDK v4 | `'auto' \| 'json' \| 'tool'` | 모델 어댑터의 `defaultObjectGenerationMode` 필드 (undefined = 지원 안 함) | 호출자 주입 `experimental_repairText` **1회**, 그 뒤 재-raise |
| LangChain | `'function_calling' \| 'json_mode' \| 'json_schema'` | provider 패키지별 `method` 인자, 기본값은 `function_calling` | `include_raw` 로 원문 노출 |
| Instructor | `TOOLS \| JSON \| MD_JSON \| JSON_SCHEMA` | Mode enum, provider 별 매핑 | reask 루프 |
| openclaw | 전략 없음 — **항상** 합성 툴 호출 | 유일한 게이트가 `supportsTools` | 교정 재시도 **1회** 후 타입화된 `schemaError` |

수렴하는 지점 넷:

1. **전략은 3개 안팎의 닫힌 열거다.** 2개(있다/없다)로는 부족하고, 그 이상은 아무도 쓰지 않는다.
2. **선택은 선언된 capability 기반이지 런타임 폴백 캐스케이드가 아니다.** 실패 후 다른 wire 형식으로 재시도하는 코드 경로를 가진 SDK 는 조사 대상 중 하나도 없다. 비용이 두 배가 되고 실패 원인이 가려진다.
3. **복구는 1회로 유계이고 타입화된다.** 무한 repair 루프도, 기본값으로의 조용한 강제 변환도 없다.
4. **툴 루프 안의 구조화 출력 = 종단 단계.** AI SDK 6 은 `generateObject` 를 폐기하고 구조화 출력을 툴 루프의 마지막 턴으로 접었다. 중간 턴에 스키마를 거는 것이 아니라, 툴 루프가 끝난 뒤 제약이 걸린 턴을 한 번 더 돈다.

4번은 OAS 의 실측과도 일치한다 (2026-07-22, gpt-5.5 및 로컬 glm-4.7-flash): schema 와 tools 를 함께 보내면 툴을 부르는 턴은 `content` 가 비고 `tool_calls` 만 나오며, 툴 결과가 들어온 다음 턴에서 스키마가 적용된 JSON 이 나온다. Anthropic 은 이 동작을 문서로 보증한다 — *"Grammar state resets between sections, allowing Claude to think freely while still producing structured output in the final response."* 즉 **제약을 루프 전 구간에 걸어두는 것이 올바르며**, 중간 턴은 자연히 제약을 소비하지 않는다.

## 2. 설계

### 2.1 전략 합타입

```ocaml
(* lib/llm_provider/structured_output_strategy.mli *)

type t =
  | Native_json_schema
    (** Provider 가 요청 필드로 디코딩을 스키마에 제약한다. 필드 shape 은
        provider dialect 가 결정한다 (response_format.json_schema /
        text.format / output_config.format / responseFormat.text / format). *)
  | Tool_call
    (** 스키마가 단일 툴의 [input_schema] 로 실린다. 모델이 그 툴을 호출하고,
        인자가 곧 구조화 출력이다. Native 가 없지만 tools 가 있는 provider
        전부에 적용된다. [supports_named_tool_choice] 가 참이면 그 툴을 강제
        선택하고, 거짓이면 툴을 하나만 노출하고 프롬프트로 지시한다 — 후자가
        openclaw 가 유일하게 쓰는 경로이며 GLM/Cohere 처럼 tool_choice 가
        없는 provider 를 덮는다. *)
  | Json_mode_with_prompt_schema
    (** [response_format = json_object] + 시스템 프롬프트에 스키마 본문.
        provider 는 JSON 문법만 보증하고 형태는 보증하지 않는다. tools 도
        native 도 없는 provider 의 마지막 경로. *)

(** 선언된 capability 로부터 전략을 결정한다. 순수 함수이며 네트워크·호스트·
    모델 id 문법을 보지 않는다 (RFC-OAS-034 §2). 어떤 전략도 성립하지 않으면
    [Error] — 조용한 성공 흉내를 내지 않는다. *)
val select
  :  capabilities:Capabilities.capabilities
  -> (t, unsupported_reason) result
```

선택 규칙 (우선순위 순, catch-all 없음):

| 조건 | 전략 |
|---|---|
| `supports_structured_output` | `Native_json_schema` |
| `supports_tools` | `Tool_call` |
| `supports_response_format_json` | `Json_mode_with_prompt_schema` |
| 그 외 | `Error No_structured_output_path` |

`supports_response_format_json` 과 `supports_structured_output` 두 플래그는 그대로 둔다. AI SDK 의 `defaultObjectGenerationMode` / `supportsStructuredOutputs` 2-플래그 분리와 같은 축이며, `docs/provider-capabilities-spec.md` 가 이미 그 의미를 정확히 기술하고 있다. **바꾸는 것은 플래그가 아니라 플래그를 읽는 코드다.**

### 2.2 호출자 정책

전략을 자동 선택만 하면 호출자는 자기가 받은 것이 보장인지 최선 노력인지 알 수 없다. 정책을 명시적으로 노출한다:

```ocaml
type policy =
  | Require_native      (** native 가 없으면 요청 전에 실패한다. 현재 동작. *)
  | Best_available      (** 위 표대로 선택한다. 새 기본값. *)
  | Pin of t            (** 전략을 못박는다. 성립하지 않으면 실패한다. *)
```

`Require_native` 를 남겨두는 이유는, 스키마 준수가 정확성 요건인 호출자(예: 판정 경계)가 최선 노력으로 조용히 강등되는 것을 막기 위해서다.

### 2.3 Receipt

`Types.output_token_receipt` 가 "어떤 선언이 이 값을 공급했는가" 를 보존하는 것과 같은 방식으로, 어떤 전략이 실제로 쓰였는지를 응답에 실어 보낸다:

```ocaml
type structured_output_receipt =
  { strategy : Structured_output_strategy.t
  ; strict_requested : bool          (* PR #2745 의 투영 결과 *)
  ; schema_degradations : Json_schema_strict.violation list
  }
```

이것이 없으면 §2.2 의 정책이 검증 불가능하다. 이는 텔레메트리-as-fix 가 아니다 — 동작은 전략 선택이 바꾸고, receipt 는 그 선택을 관측 가능하게 만든다.

### 2.4 추출 결과의 3분법

openclaw 와 AI SDK 가 공통으로 도달한 형태를 따른다 (#2751 참조):

```ocaml
type 'a extraction =
  | Structured of 'a
  | Unstructured of { text : string; reason : failure }
  | Never_produced of { stop_reason : Types.stop_reason }
and failure =
  | Refused of string
  | Truncated
  | Malformed_json of string
  | Schema_mismatch of string
```

`Never_produced` 는 툴콜 턴이나 refusal 처럼 **모델이 답변 텍스트를 내지 않은** 경우다. 현재는 이것이 `"structured output response did not contain text JSON"` 이라는 하나의 문자열로 뭉개져 있고, 그래서 에이전트 루프 안에서 정상 툴콜 턴과 provider 결함을 구분할 수 없다.

### 2.5 복구는 1회, 호출자 주입

```ocaml
val extract
  :  ?repair:(text:string -> failure:failure -> string option)
  -> ...
```

AI SDK 의 `experimental_repairText` 와 같은 경계다 — 순수 text→text 함수, 1회 호출, 실패하면 그대로 실패. OAS 는 모델에 재질의하지 않는다. 재질의는 비용과 지연을 호출자 모르게 두 배로 만들고, 호출자가 자기 예산 안에서 스스로 결정할 수 있는 일이다.

### 2.6 reasoning 채널 분리

Provider 별 실측·문서 확인 결과 reasoning 은 답변 채널을 오염시키지 않는다 (OpenAI Responses 는 별도 `reasoning` output item, Anthropic 은 `thinking` 블록, Ollama 는 `message.thinking`). 예외는 **reasoning 파서가 없는 OpenAI 호환 서버**로, `<think>...</think>` 를 `content` 에 인라인으로 낸다. Hermes 계열이 정확히 이 형태이며 Nous 의 chat template 자체가 최종 답변이 첫 `</think>` **이후**에 온다는 것을 보증한다.

따라서 추출 경계는:

1. `visible_text_of_response` 를 쓴다 (`ToolResult` 제외 — #2751).
2. 인라인 reasoning 을 내는 것으로 **선언된** 모델에 한해 첫 `</think>` 로 split 하고 접미부만 파싱한다. 태그 존재 여부로 추측하지 않는다 — 선언이 provenance 다 (RFC-OAS-034).

## 3. 이 RFC 가 하지 않는 것

- **런타임 폴백 캐스케이드를 도입하지 않는다.** native 로 400 을 맞으면 tool_call 로 재시도하지 않는다. 조사한 SDK 중 그렇게 하는 것이 없고, 실패 원인을 가린다.
- **없는 스키마 정보를 지어내지 않는다.** PR #2745 가 정한 원칙을 유지한다.
- **capability 플래그를 재설계하지 않는다.** 2-플래그 구조는 옳다.
- **`tool_param` 을 확장하지 않는다.** `Array` 의 `items` 와 중첩 `object` 를 표현할 수 없는 것은 별개의 실제 결함이지만(PR #2745 §근본원인 참조), `Provider_config.output_schema` 가 임의 JSON Schema 를 받으므로 본 RFC 의 전제는 아니다. 별도 RFC 로 다룬다.

## 4. 구현 순서

| 슬라이스 | 내용 | 선행 |
|---|---|---|
| S0 | OpenAI strict 투영 (`Json_schema_strict`) | — · **PR #2745 에 landed** |
| S1 | `#2746` provider entry `vendor_model_ids` 선언 | — · **PR #2745 에 landed** |
| S2 | `structured_output_strategy.ml` + `select` + 단위 테스트 (네트워크 없음) | — · **PR #2745 에 landed** |
| S3 | `Tool_call` 전략 배선: `extract` 가 스키마를 툴로 싣고 `extract_tool_input` 을 되살린다 | S2 · **PR #2745 에 landed** |
| S4 | receipt + 3분법 추출 결과 (#2751) | S2, S3 |
| S5 | `policy` 노출, 기본값을 `Best_available` 로 | S3, S4 |
| S6 | provider 선언 정정 (#2747, #2752) + `docs/provider-capabilities-spec.md` 날짜 갱신 | — · **DeepSeek·DashScope 는 PR #2745 에 landed**; Gemini(#2747) / Cohere·Mistral·Kimi(#2752) 잔여 |

각 슬라이스는 `test_structured_output_conformance.ml` (PR #2745) 에 해당 provider 케이스를 추가해 **실제 wire 로 증명한다**. capability 플래그를 바꾸는 것만으로는 이 RFC 의 어떤 주장도 증명되지 않는다.

S0+S1 이 실제로 그 기준을 통과했다 — `tool_param list` 로 만든 스키마가 실제 모델에 도달해 타입화된 값으로 돌아온다:

```
[openai] conformed: city=Seoul population_millions=9.4
[ollama-local] conformed: city=Seoul population_millions=9.7
[SKIP] anthropic — credential rejected by the provider
```

S3 도 같은 기준을 통과했다 — 로컬 `glm-4.7-flash` 는 `base = "glm"` 을 상속해 native 스키마 필드도 named tool_choice 도 없으므로 `Tool_call Model_choice` 로 라우팅되고, 그 wire 로 타입화된 값을 돌려줬다. GLM·Cohere 가 받는 것과 같은 wire 다.

S3 구현 중 라우팅 테스트가 실제 오선언을 잡았다: DeepSeek 4개 행이 `base = "openai_chat"` 을 상속해 native structured output 을 주장하고 있었고, 그대로면 documented enum 이 `[text, json_object]` 인 API 에 `json_schema` 를 보냈을 것이다. 행을 정정하고 tool 경로로 라우팅했다. §5 의 DashScope 항목도 같은 계열의 위험이며 아직 미해결이다.

또 하나 기록해 둘 것: 하네스가 값(value)에 대해 단언하면 안 된다. `population > 0` 을 요구하니 모델 품질의 flaky 판정자가 됐다. Gemini 문서가 긋는 선이 그것이다 — *"structured output guarantees syntactically correct JSON, it does not guarantee the values are semantically correct."* conformance 는 "요청한 형태로 파싱됐다" 이며, 타입화된 파서가 이미 그것을 강제한다.

S1 의 선택지는 두 가지였고 이슈 #2746 에 기록되어 있다. 채택하지 않은 쪽(gpt 모델마다 provider-scoped 행 추가)이 나쁜 이유는 provider-scoped 조회가 `id_prefix` **정확 일치**인 반면 bare row 는 prefix 매칭이라, dated snapshot 마다 행이 필요하고 카탈로그가 provider 릴리즈마다 뒤처지기 때문이다.

## 5. 미확인 사항

- ~~DashScope / Qwen 의 `response_format.json_schema` 지원 여부.~~ **해소됨 (PR #2745).** 2026-07-22 재확인 결과 미지원이 확정되었다 — OpenAI 호환 Chat Completions 레퍼런스, 네이티브 DashScope 레퍼런스, structured-output 가이드, Responses-API 호환 페이지 **네 곳 모두에서 `json_schema` 0회**이고 `response_format.type` enum 이 `{text, json_object}` 로 닫혀 있다. 가이드가 말하는 "JSON Schema format" 은 프롬프트 안의 산문이며, 서버측 보장이 없으므로 클라이언트 검증을 지시한다. `dashscope_capabilities` 가 `supports_structured_output = false` 를 선언하고 게이트의 무검사 통과 그룹에서 빠졌다. DashScope 의 구조화 출력은 tool 전략으로 간다.
- Anthropic 의 grammar-scope 문장(*"Grammar state resets between sections…"*) 은 검색 엔진 추출로만 확보했고 페이지 렌더에서 in-situ 확인에 실패했다. 인용 전 재확인 필요.
- Nous 호스티드 API 가 `response_format.json_schema` 를 수용하는지 미확정. Hermes 는 당분간 `Tool_call` 또는 `Json_mode_with_prompt_schema` 로 다룬다.

## 6. 근거

- 실측 (2026-07-22, `api.openai.com/v1/{chat/completions,responses}` + `gpt-5.5`, 로컬 Ollama `gemma4:31b-it-q4_K_M` / `glm-4.7-flash:q4_K_M`): strict 부분집합 위반 시 400, schema+tools 동시 사용 시 툴콜 턴의 빈 텍스트, 툴 결과 이후 턴의 스키마 준수, thinking 채널 분리.
- OpenAI Structured Outputs guide — https://developers.openai.com/api/docs/guides/structured-outputs
- Anthropic Structured outputs / Extended thinking — https://platform.claude.com/docs/en/build-with-claude/structured-outputs
- Gemini Structured output + REST GenerationConfig — https://ai.google.dev/api/generate-content
- Ollama Structured outputs — https://docs.ollama.com/capabilities/structured-outputs
- Z.AI struct-output + chat-completion reference — https://docs.z.ai/guides/capabilities/struct-output
- DeepSeek JSON Output + tool_calls — https://api-docs.deepseek.com/guides/json_mode
- Moonshot Kimi response_format — https://platform.kimi.ai/docs/guide/response_format
- Mistral custom structured output + openapi.yaml — https://docs.mistral.ai/studio-api/conversations/structured-output/custom
- xAI structured outputs — https://docs.x.ai/developers/model-capabilities/text/structured-outputs
- Cohere structured outputs + Chat API v2 — https://docs.cohere.com
- Vercel AI SDK `generateObject` reference — https://ai-sdk.dev
- LangChain `with_structured_output` — https://python.langchain.com
- openclaw — https://github.com/openclaw/openclaw (`packages/ai`)
- Nous Hermes model cards + chat templates — https://huggingface.co/NousResearch

전부 2026-07-22 확인. §5 에 열거한 세 항목은 confidence Medium, 나머지는 High.
