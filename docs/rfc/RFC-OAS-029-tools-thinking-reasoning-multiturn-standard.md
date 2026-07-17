# RFC-OAS-029: Tools / Thinking / Reasoning / Multi-turn usage standard

| | |
|---|---|
| Status | Draft |
| Author | jeong-sik (audit: adversarial multi-agent sweep, 2026-06-29) |
| Created | 2026-06-29 |
| Last verified | 2026-07-17 |
| Target | `agent_sdk` (oas) — `lib/llm_provider/`, `lib/api_*.ml`, `lib/*tool*.ml`, `lib/streaming.ml`, `docs/design/provider-reasoning-dialects.md` |
| Keystone dependency | RFC-OAS-023 (capability axis reshape) — the GLM/MiniMax dialect work lands there; see §5 |
| Supplements | RFC-OAS-018 (catalog externalization), RFC-OAS-025 (forced-tool-use enforcement boundary), RFC-OAS-034 (endpoint location is not capability provenance) |
| Boundary | OAS exposes typed provider facts; MASC consumes them. OAS MUST NOT depend on MASC. See §6 |

## 0. Summary (요약)

OAS의 Tools / Thinking / Reasoning / Multi-turn 처리는 **코어는 견고하고 소비자 경계에서 부패**한다. typed dialect 아키텍처(`stop_reason_wire`, `Capabilities`/`Reasoning_dialect`의 closed sum types)는 적대적 기준을 통과한다. Tool 이름은 OAS가 의미를 추론하지 않고 등록된 이름을 정확히 조회하며, alias는 consumer가 명시적으로 주입한다. 위반은 전부 **typed 결정을 string/JSON 휴리스틱으로 재결정하거나 손으로 복제**한 지점에 집중된다.

본 RFC는 그 위반들을 근거로, 기여자(사람 또는 AI 에이전트)가 Tools/Thinking/Reasoning/Multi-turn 코드를 추가·수정할 때 **반드시 만족해야 하는 검증 가능한 불변식(invariant)** 을 확립한다. 각 규칙은 컴파일러 또는 비-vacuous 테스트 또는 CI grep gate로 강제된다. 아키텍처를 버리지 않는다 — 남은 call site를 one typed wire contract로 라우팅하고, codec/path inference, provider-tuple gap, sync/stream grammar drift, malformed-frame loss, GLM/MiniMax/Ollama dialect gaps를 hard-cut하는 것이 목표다.

근거: 2026-06-29 적대적 다중 에이전트 감사의 historical inventory
(당시 확정 위반 22건, 반박/경계-수용 16건)와 2026-07-17 current-main
재검증. 해결된 항목과 새 transport P0를 §3에서 분리하며, historical 개수는
현재 open-backlog 개수로 사용하지 않는다.

## 1. Verdict — what is strong, what rots

### 1.1 Do not regress (검증된 모범)
- `stop_reason_wire.ml` — wire finish가 typed `wire_finish` + `has_tool_blocks:bool`를 거쳐 매핑되고, `reconcile`은 `Types.stop_reason`에 대해 total(새 variant는 컴파일 깨짐), unknown finish는 raw string을 보존한 채 typed `Unknown`으로 fail-closed. `#2222` infinite-Thinking P0가 cap/string workaround가 아니라 여기서 올바르게 고쳐졌다.
- `Agent_tools` tool lookup — 등록된 이름을 exact match하고 consumer-등록 alias만 별도 경계에서 해석한다. OAS core에 builtin tool catalog는 없다.
- `reasoning_dialect.ml` / `capabilities.ml`의 dialect 축 — `thinking_control_format`, `preserve_thinking_control_format`, `toggle_wire`, `gemini_family`, `anthropic_thinking_control`, `replay_policy` 모두 closed sum, 대체로 exhaustive. `#2228`은 loose toggle match를 typed preserve 축 추가로 *닫은* 진짜 hardening이다 (workaround 아님).
- Cost는 `annotate_response_cost`로 기록만 되고 `lib/`에서 동작을 gate하지 않는다 — budget 경계가 올바르다. (goal: budget/cost/turn은 집계만.)

### 1.2 Where it rots (위반의 형태)
- **공유 builder 뒤에 남은 복수 integration surface.** Current main routes
  both OpenAI-compatible request paths through canonical
  `Reasoning_dialect.request_control_fields` and the same clear-thinking
  resolver, so the former live wire-field drift is closed. The remaining risk
  is that two integration call sites still need proof that they consume the
  same exact binding contract; another field builder must not be added.
- **typed kind coverage와 provider tuple authority가 갈라짐.** Current main
  uses a typed `Provider_config.kind` branch for GLM; the old scattered raw
  GLM prefix classifier is no longer the live defect. The remaining gap is
  that an OpenAI-compatible GLM/Ollama binding can miss that kind coverage
  because provider identity, kind, codec, and path are independently assembled.
- **typed 레이어가 제거한 휴리스틱을 다시 들여오는 lenient/repair shell.** Historical: `tool_use_recovery.ml` scraped JSON tool calls from free text and ran `Lenient_json` bracket/keyword completion before execution. That recovery surface is now deleted without a replacement fallback: `Text` remains `Text`, and only a provider parser's typed `ToolUse` can reach dispatch.
- **typed block boundary 아래 남은 raw-frame silent drop.** Historical
  finalizer `content_type` catch-all is largely closed by `block_kind`,
  `Unknown_block`, and typed parse errors. Separately, current OpenAI-compatible
  raw SSE decode still conflates malformed JSON with no event, allowing later
  `[DONE]` to bless a partial attempt; S6.7 is the required total boundary.
- **같은 사실의 두 번째 SSOT는 삭제됨.** Historical:
  `lib/streaming.ml` carried a duplicate accumulator lacking reconcile and
  partial-Tool handling. Current code routes the supported
  `Agent_sdk.Streaming` façade through `Complete_stream_acc` and re-exports its
  narrowed canonical signature; the old accumulator is not a retained legacy
  path.
- **doc/typed surface가 배포 모델에 뒤처짐.** OpenAI-compatible GLM의
  provider-kind/codec/route catalog tuple이 typed GLM dialect까지 완전히
  연결되지 않음; MiniMax M2/M3 catalog rows exist but replay/tool-choice
  facts are under-sourced; audit-reported Anthropic `thinking.display` gap은
  공식 source refresh가 필요하다. Anthropic thinking 중 forced
  `tool_choice` 거부와 `Reasoning_effort.None_`/`Max` vocabulary는 current
  main에 이미 존재하므로 열린 결함으로 다시 세지 않는다.

## 2. The Standard (검증 가능한 불변식)

각 규칙은 위반 시 `dune build` 실패(**Compiler**) / revert 시 red 되는 비-vacuous 테스트(**Test**) / CI grep(**Gate**) 중 하나로 강제되며, 일부 보조 규칙은 리뷰 가이드(**Advisory**)로 작용한다. 강제 경로가 명시된 핵심 규칙은 반드시 해당 메커니즘을 통과해야 한다.

### S1 — 새 model / dialect 추가
- **S1.1 (이름 분류기는 없다).** Model/provider spelling is not capability
  evidence. A named model resolves by exact opaque catalog identity and catalog
  revision; an alias or arbitrary OpenAI-compatible deployment resolves only
  through an explicit checked binding/manifest row supplied by its owner.
  Unknown exact identity fails closed. No blessed function may use
  `String.starts_with`, substring/regex family matching, or semantic
  `String.equal` over model/provider text. Exact equality remains valid only
  for already typed protocol identities such as a catalog key, Tool wire name,
  or exact stream sentinel. Downstream consumes closed variants.
  **Compiler/Gate/Test**.
- **S1.2 (per-model 사실의 SSOT는 catalog).** 수치 한계·pricing·thinking-control class는 `models.toml`/`Model_catalog`에 있고 코드의 prefix table에 없다. catalog에 모델을 추가하면 thinking 축이 catalog에서 나와야 하며 OCaml 편집이 필요 없어야 한다.
- **S1.3 (variant가 구분을 담는다).** sub-capability가 family를 가르면(예: 3.1-pro vs 3.1의 `supports_minimal`) variant에 인코딩한다(`Gemini_3_1 of { is_pro : bool }`). 미래의 분기가 컴파일 타임에 분류기를 깨야 한다. **Compiler**.
- **S1.4 (잘못된 default로 resolve 금지).** dialect 항목이 없는 reasoning 모델은 **fail closed**(`None`/`Unknown`)해야 하고 `No_replay`/`No_thinking_control`로 조용히 resolve되면 안 된다. **Compiler/Test**.
- **S1.5 (transport-qualified resolution).** Effective reasoning, Tool,
  history, structured-output, and multimodal wire capabilities resolve from
  one opaque, revisioned binding row. Its checked identity is
  `Provider_binding_reference.t`; the row contains the exact typed
  `(provider identity, model identity, Provider_http_codec.t,
  Provider_endpoint_route.t, wire-evidence revision)` tuple.
  Provider/model identity alone MUST NOT select a wire dialect.
  `Provider_config.t` carries that opaque binding reference plus the codec and
  endpoint-route kind as typed declared fields. The reference is minted only
  by the provider/model catalog row or the checked custom-binding constructor;
  it is not an endpoint identity and is never derived from a URL, host,
  locality, or model spelling. The concrete HTTP path is emitted from that route
  contract (or retained as opaque transport data for an explicitly typed
  custom route); code never classifies its string. URL/model-name heuristics
  and a second provider identity for the same service are forbidden. Moving
  the same declared binding to another physical address therefore cannot
  change its capabilities. This is the RFC-OAS-034 boundary, not a second
  endpoint-capability SSOT.
  **Compiler/Test**.
- **S1.6 (one effective wire contract).** Binding construction resolves that
  tuple exactly once into one immutable `Provider_wire_contract.t`. Request
  builders, synchronous parsers, streaming parsers, whole-attempt finalizers,
  and continuation adapters all consume that same contract revision; none
  recomputes a capability from provider/model strings or payload shape. Every
  axis is a closed variant with an explicit `Unsupported`/`Unknown` outcome,
  and an unresolved required axis rejects binding construction. The contract
  is derived evidence, not a second model catalog. **Compiler/Test**.
- **S1.7 (declared tuple admissibility).** A named-provider catalog row declares
  the allowed provider-kind/codec/route/evidence-revision combinations for that
  exact provider/model binding. An arbitrary
  OpenAI-compatible endpoint instead uses one checked
  custom-binding constructor whose caller selects a closed codec and route
  variant explicitly; its path remains opaque transport data and cannot alter
  that selection. Both paths mint the same opaque revisioned binding
  reference. Binding construction proves the exact tuple came from one of
  these two authorities before capability lookup. Independently assembled but
  individually valid fields cannot form a binding, and there is no permissive
  cross-product default. Local Ollama and Ollama Cloud therefore use distinct
  explicitly declared binding rows when their verified wire facts differ;
  neither row is selected from its hostname. Conversely, relocating one row
  without changing its declared serving contract leaves the row unchanged.
  The current path-only `Provider.OpenAICompat`
  constructor is hard-cut in favor of this typed producer; no compatibility
  wrapper infers a codec from the path. **Compiler/Test**.
- **S1.8 (serving profile is evidence, not model inference).** For a generic
  OpenAI-compatible server, the binding evidence revision also identifies the
  exact deployed serving profile needed to interpret its wire behavior:
  server implementation/revision, selected chat template, Tool-call parser,
  reasoning parser, and other explicitly configured protocol adapters.
  vLLM-, SGLang-, llama.cpp-, proxy-, or Ollama-like behavior is never inferred
  from the host, model name, response field spelling, or a successful
  `/v1/models` call. A changed serving profile requires new evidence and a new
  binding revision before it can gain capabilities. Backend features marketed
  as repetition detection or a thinking-token budget are not enabled by OAS as
  recurrence fixes or defaults; repetition remains lossless evidence for an
  embedding LLM-policy boundary. **Test/Evidence**.

The effective contract keeps these compatibility axes independent:

| Axis | Required distinction |
|---|---|
| HTTP grammar | Chat Completions, Responses, native Ollama chat, and other native envelopes |
| Reasoning request | no control, boolean control, level control, effort value, or provider object; native Ollama boolean and GPT-OSS level forms are distinct |
| Reasoning output | non-stream carrier and stream-delta carrier, resolved independently |
| Continuation | full stateless replay, opaque provider state, or unsupported state handles |
| Tool call | mode-specific sync/stream arguments grammar, exact Tool name, closed Chat/Responses/name-order correlation fields, terminal mapping |
| Tool result | native Tool name carrier versus native call-ID carrier and adjacency rules |
| Tool choice | accepted subset of omitted/`auto`/`none`/`required`/named |
| Input truncation | unsupported, fail-before-loss, or caller-authorized lossy auto-truncation |
| Multimodal input | semantic media kind × source kind × codec carrier; image, document, audio, and video support are independent |
| Structured output | request syntax accepted versus schema-conformance guarantee |
| Stream envelope | framing (SSE or NDJSON), keep-alive/comment policy, typed data/usage/terminal/provider-error frames, termination rule, and premature-EOF handling |

No row is inferred from another row. In particular, accepting an OpenAI-shaped
request does not prove Responses statefulness, call-ID presence, a specific
reasoning delta field, strict schema enforcement, or OpenAI-identical stream
termination.

The target internal surface is one abstract resolved value, not parallel
boolean records:

```ocaml
module Provider_stream_content_contract : sig
  type field =
    | Output_text
    | Reasoning_text
    | Reasoning_summary_text
    | Refusal_text
    | Tool_arguments
    | Content_part

  type semantics =
    | Append_fragments
    | Append_fragments_then_validate_final_snapshot
    | Contract_declared_replacement_snapshots

  type t

  val semantics
    :  t
    -> field
    -> (semantics, Unsupported_stream_field.t) result
end

module Provider_input_truncation_contract : sig
  type request =
    | Fail_on_overflow
    | Lossy_auto

  type support =
    | Unsupported
    | Fail_on_overflow_only
    | Fail_or_lossy_auto

  type t

  val support : t -> support

  val validate
    :  t
    -> request
    -> (Provider_input_delivery_contract.t,
        Unsupported_input_truncation.t)
       result
end

module Provider_stream_completion_contract : sig
  type t =
    | Done_sentinel_required
    | Typed_terminal_event_required
    | Terminal_chunk_then_clean_eof
end

module Provider_in_band_failure_contract : sig
  type t =
    | No_in_band_failure
    | Sse_error_event
    | Ndjson_error_object
    | Responses_failed_event
end

module Provider_wire_contract : sig
  type t

  val resolve
    :  Provider_config.t
    -> (t, Provider_wire_contract_error.t) result

  val reference : t -> Provider_wire_contract_reference.t
  val binding : t -> Provider_binding_reference.t
  val http_codec : t -> Provider_http_codec.t
  val endpoint_route : t -> Provider_endpoint_route.t
  val reasoning_request : t -> Reasoning_request_wire.t
  val reasoning_output : t -> Reasoning_output_wire.t
  val reasoning_stream : t -> Reasoning_stream_wire.t
  val continuation : t -> Provider_continuation_wire.t
  val tool_call : t -> Provider_tool_call_wire.t
  val tool_result : t -> Provider_tool_result_wire.t
  val tool_choice : t -> Provider_tool_choice_contract.t
  val input_truncation : t -> Provider_input_truncation_contract.t
  val multimodal_input : t -> Provider_multimodal_input_wire.t
  val structured_output : t -> Provider_structured_output_contract.t
  val stream_envelope : t -> Provider_stream_envelope.t
  val stream_completion : t -> Provider_stream_completion_contract.t
  val in_band_failure : t -> Provider_in_band_failure_contract.t
  val stream_content : t -> Provider_stream_content_contract.t
end
```

`resolve` performs an exact catalog-identity lookup for a named model; it never
derives a family from `Provider_config.model_id` text. It reads the opaque
binding reference, codec, and endpoint route only from the config's typed
fields. It validates their closed variants
against the catalog/binding tuple; it does not call the
hard-cut-removed path classifier `Provider_http_codec.of_config`. Neither model
nor codec is a second free parameter that can disagree with provider
configuration. The returned value records the catalog/evidence revision used
to derive it. The listed return modules are closed variants, not strings or
open JSON. They remain Dune-private; ordinary OAS callers select a published
provider/model binding once and do not assemble this contract.
`stream_content` fixes whether each declared carrier is an append fragment, a
final validation snapshot, or a genuinely replacement-style snapshot.
Adapters never infer that choice from a common prefix, repeated text, payload
length, event name substring, provider/model spelling, or endpoint URL.
Changing any carrier semantics requires another binding/contract revision.

`Fail_on_overflow` is the required mode for an ordinary Tool continuation.
It either serializes the complete authorized conversation snapshot or returns
a typed pre-dispatch/provider-capacity failure while preserving that snapshot.
`Lossy_auto` is legal only when the embedding caller explicitly selected an
OAS-typed lossy input policy and the exact binding revision verified the wire
behavior. Its request fact records that policy and can never claim that every
selected source item was delivered. A provider's implicit/default
auto-truncation is rejected before serialization. OAS never enables it to make
a request fit, recover from a repeated Tool call, or satisfy a context/budget
heuristic.

### S2 — Thinking-field 구성
- **S2.1 (builder는 하나).** wire field 이름(`thinking`, `reasoning_effort`, `enable_thinking`, `preserve_thinking`, `thinking_budget`, `clear_thinking`, `chat_template_kwargs`, `thinkingLevel`, `thinkingBudget`, `includeThoughts`)은 typed dialect로 keying된 **정확히 1개 함수**에만 존재한다. 새 format variant는 컴파일 site 1곳만 깨야 한다. Current root is `Reasoning_dialect.request_control_fields`; all request integrations consume it. Closed sums make the target builder exhaustive, but they do **not** prove uniqueness while duplicate integration surfaces still exist; the remediation PR that closes D1 must add a grep/drift test proving those field names appear only in the canonical builder. **Compiler + Gate/Test**.
- **S2.2 (budget와 effort는 독립).** Numeric `thinking_budget` never implies
  `Reasoning_effort.t`; no threshold, ratio, default, or `of_budget` mapping is
  permitted. A provider-native numeric budget is serialized only when the
  caller explicitly selected that typed control and the exact wire contract
  accepts it. An effort value is likewise caller intent validated against the
  exact model/endpoint subset. Budget/cost/token observations never gate,
  pause, stop, or synthesize either control. **Compiler/Test**.
- **S2.3 (effort vocabulary + provider acceptance subset).** `Reasoning_effort.t`는 OAS가 직렬화할 수 있는 closed vocabulary이며, 모든 provider/model이 그 모든 값을 수락한다는 뜻이 아니다. provider/model capability record가 수락 subset과 omission/`none` 허용 여부를 선언하고, request builder는 그 subset을 검증해 fail-closed하거나 typed policy로 omit해야 한다. `none`/`max`/`xhigh` 등은 공식 근거와 catalog capability update가 있을 때만 vocabulary에 추가한다.
- **S2.4 (control/output/stream carrier independence).** Thinking request
  control, non-stream output carrier, and streaming-delta carrier are separate
  closed variants selected by S1.5. Native Ollama uses its typed `think`
  control and `message.thinking`. The contract declares its accepted-value
  subset rather than guessing from a model name: GPT-OSS requires an explicit
  `"low" | "medium" | "high"` caller value, returns typed `Level_required` when
  enabled without one, and returns typed unsupported when asked to disable
  thinking. Omission is allowed only when the exact contract declares provider
  defaulting. Other models may explicitly accept booleans, levels, or both.
  A boolean can therefore never become an accepted silent no-op. An
  OpenAI-compatible chat
  binding uses its
  declared reasoning control plus one exact declared delta carrier such as
  `reasoning_content` or `reasoning`. Unknown carriers fail closed. Field-order
  probing and provider-global fallback are forbidden. `enable_thinking=false`
  maps to a declared `none`/disabled value or returns a typed unsupported
  result; it is never a silent no-op. **Compiler/Test**.

### S3 — Reasoning replay (multi-turn)
- **S3.1 (replay는 typed, 출처 하나).** "이 provider는 reasoning을 replay하는가?"는 `should_replay_reasoning`를 통한 `replay_policy`만이 답한다. serializer가 `~include_reasoning_content:true`를 하드코딩하거나 `config.kind=Glm`/`is_glm_request`로 분기 금지. **Test/Gate**.
- **S3.2 (mandatory replay는 검증된 exact binding 사실이다).** An exact
  binding may resolve to `Preserve_always` or
  `Drop_without_tool_preserve_with_tool` only when its revision carries current
  official documentation or an exact live-probe evidence record proving that
  replay contract. Verified binding rows encode hard protocol requirements
  such as Anthropic unmodified blocks, Gemini missing `thoughtSignature`, or
  documented GLM `clear_thinking` ordering; generic code never infers them from
  a provider/model name. Kimi and MiniMax remain `Unknown` in this RFC until
  equivalent evidence is captured for their exact binding revisions.
  `Unknown` fails binding/continuation closed; it is not silently converted to
  `No_replay`, `Preserve_always`, or a guessed drop policy. **Compiler/Test/Gate**.
- **S3.3 (history 불변식, repair-on-read 아님).** tool-call/tool-result 인접성은 **append 시점에 강제**(parse-don't-validate)하여 orphan을 표현 불가능하게 만든다. request-shaping filter가 block을 drop해야 한다면 drop된 id를 caller에게 반환한다 — silent filter 금지, drop *counter*를 "fix"로 삼는 것 금지.
- **S3.4 (native Ollama Tool loop replay).** After a streamed native Ollama
  ToolUse, the next request replays the exact accumulated assistant
  `thinking`, `content`, and `tool_calls` before ToolResults. Native Ollama
  MUST NOT resolve to `No_replay`. Missing accumulated state is a typed history
  error, not reconstructed text. **Test**.
- **S3.5 (stateful continuation is a transport capability).** A transport
  that does not support stateful Responses continuation rejects
  `previous_response_id` and `conversation` before serialization. The caller
  supplies full authorized history instead. Provider identity cannot override
  this codec capability. **Test**.

### S4 — Tool-call 탐지
- **S4.1 (typed, fail-closed).** "모델이 tool call을 냈는가?"는 `stop_reason_wire.of_finish` / native typed `ToolUse` block이 결정한다. 자유 텍스트 JSON 긁기를 *주* 결정으로 쓰는 것 금지.
- **S4.2 (Text는 ToolUse가 아니다).** 비준수 backend의 자유 텍스트에서 JSON이나 도구 이름을 추출해 `ToolUse`로 승격하지 않는다. 도구 실행은 provider parser가 만든 native typed `ToolUse`에만 허용하며, 잘못된 tool 인자는 원문 그대로 typed validation failure로 반환한다.
- **S4.3 (untyped == typed).** untyped handler는 typed parser에 위임하고 그 `Error`를 전파한다. "input 전체를 prompt로 직렬화"하는 fallback 금지.
- **S4.4 (ToolResult identity is transport-owned).** Canonical ToolUse and
  ToolResult facts preserve exact provider-visible `tool_name` independently
  from grammar-specific correlation. Native Ollama serialization uses
  `tool_name`; OpenAI-compatible Chat preserves choice/tool indices plus its
  call ID; Responses preserves distinct response ID, item ID, output index, and
  call ID, using only the call ID in `function_call_output`. OAS MUST NOT
  synthesize a missing provider ID, collapse item ID into call ID, or replay
  either as another grammar's identity. The current required string
  `ToolUse.id`/`ToolResult.tool_use_id` pair is hard-cut into one typed
  reference containing OAS invocation identity, exact Tool name, and a closed
  native-correlation variant; ToolResult creation may not discard the name or
  identities and recover them later by history scan. **Compiler/Test**.

### S5 — Forced tool use
- **S5.1 (forced-tool 제약은 provider별 typed).** `tool_choice` forcing capability는 provider에 대해 exhaustive한 capability 사실이다. 알려진 제약을 런타임 400으로 발견하지 말고 typed 사실로 노출: thinking active인 Anthropic은 `any`/`{tool,name}` 거부; Z.AI/GLM은 `auto`만. MiniMax `none`/`auto` restriction is audit-reported here and requires official/live evidence before implementation authority.
- **S5.2 (capability flag와 builder 일치).** `supports_tool_choice=false`면 request builder가 named/`required` `tool_choice`를 내면 안 된다. **Test**.
- **S5.3 (Tool availability is not forced use).** Transport carriage of
  `tool_choice` and per-model acceptance of `auto`, `none`, `required`, or
  named choice are separate facts. Exposing Tools defaults to omitted/`auto`
  model judgment unless the caller explicitly requests a supported stronger
  mode. A provider capability never implies required ToolUse. **Test**.
- **S5.4 (documented carriage is not deployed behavior).** A Tool-choice mode
  enters a binding's accepted subset only after an end-to-end probe against
  that exact deployed server revision proves its semantic effect or its
  explicit typed rejection. Merely observing a `tool_choice` key in generated
  JSON, or a provider compatibility page listing the field, is insufficient.
  Omitted, `auto`, `none`, `required`, and named selection are independent
  probe rows. An unverified row is `Unsupported` and is rejected before
  dispatch; it is never sent in the hope that an OpenAI-compatible server will
  honor rather than ignore it. **Test/Evidence**.

### S6 — Interleaved / streaming
- **S6.1 (block kind는 closed variant).** `content_type`은 provider raw-frame parse 경계에서 **1회** `content_block_kind` sum으로 변환되고 finalizer는 그 variant를 exhaustive match한다. unknown kind는 typed unknown-event error → `finalize`가 `Error` 반환, `_ -> None` 금지. **Compiler**.
- **S6.2 (accumulator는 하나).** stream accumulator는 `Complete_stream_acc` 하나. 보조 surface는 그것을 거친다(reconcile + partial-tool drop + reasoning visibility). 중복 `stream_acc`/`finalize_stream_acc` 금지.
- **S6.3 (parser가 dialect를 읽는다).** streamed reasoning delta field는 `dialect.streaming`(`Delta_field`/`Template_parser`)에서 읽는다, 하드코딩된 `reasoning_content`→`reasoning` 우선순위가 아니라. `streaming` 필드에 live reader가 있어야 한다.
- **S6.4 (interleave 충실도).** per-block stream index는 think→text→think 사이에서 reset돼야 interleaved block이 collapse/reorder되지 않는다.
- **S6.5 (signature는 전용 필드).** thinking signature는 `signature : string option`으로 block-subtype tag와 분리. no-signature default가 두 accumulator에서 동일하도록 finalize 공유.
- **S6.6 (sync/stream semantic parity, mode-specific grammar).** Synchronous
  and streaming parsers consume the same `Provider_wire_contract.t`, which
  declares separate `sync_tool_call_wire` and `stream_tool_call_wire` closed
  grammars when the provider legitimately differs by mode. Each parser accepts
  only its declared grammar and reasoning carrier; neither probes alternate
  fields or consumes a broad union. Both must finalize to byte-equivalent
  canonical ToolUse/Reasoning semantics for equivalent native responses. An
  undeclared but recognizable carrier is a typed protocol error. **Test**.
- **S6.7 (total raw stream frame boundary).** `Provider_stream_envelope.t`
  declares framing (`SSE` or `NDJSON`), keep-alive/comment handling, and an
  exact termination rule (sentinel, typed finish chunk, or declared EOF).
  A decoded wire event is a checked record of independently present components:
  zero or more ordered data deltas, optional usage/timing, optional terminal,
  optional provider error, and keep-alive metadata. The exact contract rejects
  forbidden combinations; the generic layer does not force them into an
  exclusive `Data | Usage | Terminal` sum. This is required because a native
  Ollama final NDJSON object may carry the last content, `done:true`, and
  usage/timing together, while an OpenAI usage-only chunk may have
  `choices=[]`. A malformed frame fails the whole
  provider attempt even if a later `[DONE]` or native terminal chunk arrives.
- **S6.8 (SSE/NDJSON compatibility is grammar-exact).** An SSE contract
  recognizes CRLF, bare LF, and bare CR line endings; distinguishes `data`
  and `data:` empty fields; removes at most the one optional ASCII space after
  a field colon; ignores only grammar-declared comment/keep-alive lines; joins
  consecutive data fields with the specified newline; and dispatches an event
  only at its blank-line boundary. EOF discards an unterminated buffered event
  and then follows the contract's premature-EOF rule. A provider attempt never
  invokes browser-style SSE auto-reconnect, and a replacement HTTP request
  never concatenates a prior partial stream with the new stream. NDJSON uses
  its separately declared delimiter and terminal grammar. **Test**.
- **S6.9 (HTTP success does not imply stream success).** An HTTP 2xx stream may
  still carry a contract-declared provider failure after valid deltas (for
  example a native Ollama streaming error or a Responses
  `failed`/`incomplete` terminal). The adapter commits the prior ordered deltas
  as observations and closes the exact attempt with the typed declared
  failure. It publishes no successful selection and dispatches no Tool call
  from that attempt. The final data+terminal+usage object remains one total
  parsed event rather than mutually exclusive cases. **Test**.
- **S6.10 (delta and final snapshot are not both appended).** For each exact
  lane and semantic field, the binding declares append fragments, append then
  final-snapshot validation, or true replacement snapshots. A `.done` event
  carrying the complete final text/arguments validates the accumulated digest
  and is not appended a second time. If no prior delta exists, that final
  snapshot is adopted once. Mismatch is a protocol failure, never prefix
  repair, longest-common-prefix trimming, or repetition detection. **Test**.
  Premature EOF is success only for a contract that explicitly declares EOF
  termination. No malformed frame is represented as `None`, an empty event
  list, or a normally completed partial response. **Compiler/Test**.

### S7 — Multimodal
- **S7.1 (source kind는 closed sum).** `source_type`은 `Base64 | Url | File_id | …`; 모든 backend가 exhaustive match. backend가 지원 않는 source는 컴파일 gap 또는 명시적 `Error`, base64 가정 silent 금지. **Compiler**.
- **S7.2 (media→empty-text flatten 금지).** synthetic/stream surface는 충실한 media event를 내거나 media를 명시적으로 거부한다. `Image/Audio/Video/Document`를 empty `text`로 relabel 금지.
- **S7.3 (multimodal carrier is codec-owned).** Native Ollama uses its typed
  base64 `images[]` carrier; OpenAI-compatible chat uses typed content parts
  with the codec-supported base64 or image-URL forms. Effective vision support
  is the intersection of model capability and codec carrier. **Compiler/Test**.
- **S7.4 (media semantics are not source syntax).** Capability resolution is
  over semantic media kind × source kind × codec carrier. `Document` is not
  reclassified as `Image` merely because both can contain base64 or a URL;
  such conversion requires an explicit typed conversion result or fails
  before serialization. Image support alone proves no PDF/document support.
  **Compiler/Test**.

### S8 — Unknown-input 처리 (교차 절단 기준)
- **S8.1.** unknown enum/schema/dialect 입력은 `Error`/`None`/`Unknown`으로 노출, 편의 default 금지. `unsupported_type, _ -> true` 금지.
- **S8.2.** contract상 required(`required:true`) tool 인자가 누락/malformed면 typed validation `Error`, magic default 금지.
- **S8.3.** unknown-but-named variant 분기는 warn(`warn_unknown_capability_value` 미러)하고 forward-compatible wire shape를 선호한다(예: Gemini deprecated `thinkingBudget`보다 `thinkingLevel`).

### S9 — Capabilities SSOT
- **S9.1 (사실당 registry 하나).** 같은 model 사실의 두 registry 금지(하나가 다른 하나에서 *증명 가능하게 유도*되지 않는 한). "증명 가능"은 generated-file check 또는 drift-test 출력으로 source row/hash와 derived registry row/hash가 연결되는 상태를 뜻한다. 이 증명이 없으면 merge 불가다; reviewer waiver는 SSOT 증명을 대체하지 못한다. provider preset은 protocol/flag default만 보유 — model-version 수치 ceiling 금지(catalog에 있음; 부재 ⇒ `Unknown_limit`).
- **S9.2 (dead/duplicate typed path 금지).** 자체 테스트로만 살아있는 exported 함수 금지; 두 모듈의 byte-identical helper 금지.
- **S9.3 (precedence는 의도적·테스트됨).** capability source precedence(catalog vs host manifest vs preset)는 1회 결정, `.mli` 전반에 일관 문서화, 테스트로 고정.
- **S9.4 (evidence provenance is scoped).** A capability fact records official
  versus empirical provenance, `checked_at`, exact declared binding+codec,
  model, and a revalidation policy. A live probe also records the opaque
  physical endpoint instance it actually observed, but that address is
  evidence scope only and never selects a capability. An endpoint-local live
  probe MUST NOT become a provider-global model fact or mutate a binding at
  runtime. A contrary empirical result requires a reviewed new binding
  revision with evidence, not an edit to unrelated bindings or a host-derived
  override.
  **Test/Review gate**.
- **S9.5 (no dead override authority).** A capability fact has one typed field
  and one reader. The endpoint/codec's structured-output request carrier and
  the model's schema-conformance capability remain independent axes in
  `Provider_structured_output_contract.t`; neither is recomputed into the
  other. A current write-only boolean is deleted only after its endpoint
  acceptance information moves to that carrier axis. It is never retained as
  inert compatibility state and never absorbed into
  `model_capabilities_override`. **Compiler/Test**.

### S10 — Observability / determinism
- **S10.1 (정직한 계약).** "Pure"로 문서화된 모듈은 pure여야 한다; 효과(wall-clock, mutable global)는 경계로 옮기거나 `.mli`에 문서화. 새 identity가 필요하면 결정론적 입력 또는 주입된 generator에서 유도한다.
- **S10.2 (데이터 손실은 관측되되, 관측이 fix는 아니다).** OAS는 provider history block을 조용히 drop하거나 synthesized replacement로 고치지 않는다. 미래의 typed 변환이 block을 제거해야 한다면 제거된 identity를 결과로 반환해야 한다. counter/log는 typed fix와 함께하는 *alarm*일 뿐 fix 자체가 아니다(telemetry-as-fix = reject 시그니처).

## 3. Evidence — confirmed and historical violations

| Sev | ID | Principle | File:line | Standard |
|---|---|---|---|---|
| P0 | D-CODEC-request-path-classifier | string_match / ssot | provider_http_codec.ml:9-22; provider_config.ml:611-618 | S1.5 |
| P0 | D-PROVIDER-tuple-cross-product | ssot / silent misbinding | provider_config.ml:46-81,199-216; complete_common.ml:461-473; models.toml:2229-2237 | S1.7 |
| P0 | D-OLLAMA-transport-qualified-dialect-loss | ssot / silent data loss | provider_config.ml:199-216; models.toml:475-488,733-752,820-839; reasoning_dialect.ml:165-176,300-363; streaming.ml:477-516 | S1.5, S2.4, S6.3, S9.4 |
| P0 | D-OLLAMA-gpt-oss-think-level | silent no-op | backend_ollama.ml:100-112; models.toml:683-710 | S2.4 |
| P0 | D-OPENAI-sync-carrier-probe | payload-shape heuristic / parser drift | backend_openai_parse.ml:102-114,151-169,349-384; complete_sync.ml:180-201; streaming.ml:518-547 | S1.6, S6.6 |
| P0 | D-OPENAI-malformed-sse-drop | silent failure / partial success | streaming.ml:406-468,562-566,598-616; complete_stream.ml:713-727 | S6.7 |
| P1 | D-OLLAMA-native-tool-history-identity | replay / identity | reasoning_dialect.ml:65-74,165-171; types.mli:159-187; backend_ollama.ml:241-262; backend_openai_serialize.ml:313-326,652-676 | S3.4, S4.4 |
| P1 | D1-dup-thinking-call-surfaces | partial: shared canonical field builder, duplicate integration surfaces remain | api_openai.ml:336-358; backend_openai_request.ml:356-377 | S2.1 |
| P1 | D1-glm-compatible-tuple-gap | typed-kind coverage gap, not a remaining raw GLM string classifier | reasoning_dialect.ml:437-470 | S1.5, S1.7, S3.1 |
| P1 | D3-finalize-content-type-string-catchall-silent-drop | partial: policy gap after typed block-kind conversion | complete_stream_acc.ml:118-140,237-244,675-700,746-756 | S6.1 |
| P2 | D6-glm-identity-classification-residue | catalog/kind authority split | zai_catalog.ml:11-13; reasoning_dialect.ml:437-470 | S1.1, S9.2 |
| ~~P2~~ Resolved | D2-budget-to-effort-heuristic | historical heuristic; current config preserves numeric budget and explicit effort independently and has no `Reasoning_effort.of_budget` | test_provider_config.ml:1161-1172; reasoning_effort.ml:3-12 | S2.2 |
| P2 | D5-anthropic-thinkmode-hardcoded-prefix-table | hardcode | capabilities.ml:182-221 | S1.2 |
| P2 | D4-provider-preset-stale-numeric-limits | hardcode | capabilities.ml:223-255; provider_registry.ml:408; builder.ml:256 | S9.1 |
| P2 | D2-streaming-reasoning-dialect-dead-and-field-guess | ssot | reasoning_dialect.ml:39-42; streaming.ml:331-335; backend_openai_parse.ml:208-298 | S6.3 |
| P2 | D4-duplicate-stream-accumulator-missing-reconcile | resolved: supported public façade is a narrowed re-export of Complete_stream_acc, not a second accumulator | streaming.ml:16-35; streaming.mli:23 | S6.2 |
| ~~P2~~ Resolved | D-TOOLS-1-recovery-text-scrape-heuristic | historical heuristic; recovery surface deleted, no text-to-tool fallback remains | deleted `tool_use_recovery.ml`; current `agent_tools.ml` exact typed dispatch | S4.2 |
| ~~P2~~ Resolved | D-TOOLS-6-agent_tool-untyped-silent-prompt-fallback | historical; current untyped path delegates to typed handling | historical agent_tool.ml:149-161 | S4.3 |
| ~~P2~~ Resolved | D-TOOLS-9-harness-unknown-schema-type-permissive | historical; current harness rejects unknown schema types | historical backend_tool_call_harness.ml:52-68 | S8.1 |
| P2 | D6-source-type-ignored-non-anthropic | string_match/silent | backend_openai_serialize.ml:60-82; backend_gemini.ml:161-174; backend_openai_responses.ml:121-137 | S7.1 |
| P2 | D5-synthetic-events-multimodal-silent-drop | silent_failure | streaming.ml:193-217 | S7.2 |
| P2 | D7-thinking-signature-overloaded-string | string_match | complete_stream_acc.ml:147-153; streaming.ml:151-155 | S6.5 |
| P2 | D4-test-only-normalize-effort-wrapper | string_match | reasoning_dialect.ml:284-292 (+mli:96); test/test_thinking_control_dialects.ml | S9.2 |
| P3 | D7-gemini-family-leaks-second-string-match | string_match | capabilities.ml:442-463 | S1.3 |
| ~~P3~~ Resolved | D-TOOLS-8-recovery-impure-nondeterministic-id | historical mutable recovery identity; deleted with recovery surface | deleted `tool_use_recovery.ml` | S10.1 |
| ~~P3~~ Resolved | D3-tool-pair-silent-drop | historical standalone repair/filter surface; deleted rather than retained as a silent rewrite | deleted `tool_message_pairs.ml` | S3.3 |
| P3 | D7-anthropic-prefix-list-literal-duplicates | hardcode | capabilities.ml:189-217 | S1.2 |
| P3 | D8-manifest-cannot-override-catalog-precedence | ssot | capabilities.ml:826-839 | S9.3 |
| P2 | D9-structured-output-write-only-endpoint-acceptance | endpoint acceptance exists but is not consumed; migrate it to the endpoint-carrier axis rather than model capability | provider_config.mli:111-124; provider_config.ml:596-608; provider.ml:570-594 | S9.5 |

Status note: `D-TOOLS-1`, `D-TOOLS-6`, `D-TOOLS-9`, `D-TOOLS-8`, and
`D3-tool-pair-silent-drop` are historical confirmed violations but are already
closed in the current branch ancestry. The recovery and standalone tool-pair
repair modules were deleted; they are retained here only as historical evidence,
not as current files or open backlog items.

### 3b. Doc-currency drifts (official 2026-06-29 docs vs OAS)

| Sev | Provider / field | OAS now | Official | Standard |
|---|---|---|---|---|
| P0 | **Ollama provider/model row across OpenAI-compatible and native codecs** | provider/model lookup can select native `ollama_think` for an OpenAI-compatible codec; native replay defaults `No_replay`; ToolResult keeps call ID but not independent Tool name | OpenAI compatibility documents Tools and reasoning controls; native streaming requires accumulated thinking/content/tool-calls replay and native ToolResults carry `tool_name`; Responses compatibility is non-stateful | S1.5, S2.4, S3.4, S3.5, S4.4 |
| P1 | **GLM dialect** | native/provider-specific GLM rows exist, but the OpenAI-compatible GLM provider-kind/codec/route tuple is not completely connected to the same typed dialect | Z.AI docs show top-level `thinking:{type,clear_thinking}`, `reasoning_content`, GLM-5.2 `reasoning_effort` (default `max`), and ordered unmodified replay when `clear_thinking=false` | S1.4, S1.7, S3.2 |
| P1 | **MiniMax M2/M3** (recurs) | catalog rows exist, but replay/tool-choice semantics are not sourced row-by-row here | audit artifact claims always-on thinking/replay and restricted `tool_choice`, but this PR has no independent official source capture yet; treat implementation as blocked until refreshed | S1.4 |
| P1 | **Anthropic `thinking.display`** | never emitted | audit artifact reports default `omitted`/`summarized` drift; official source capture required before implementation | S8.3 |
| Resolved in current main | **Anthropic tool_choice vs thinking** | typed validation rejects forced Tool choice while thinking is enabled | `any`/`{tool,name}` ⇒ 400 when thinking active | S5.1 |
| Partial | **OpenAI `reasoning_effort` enum** | closed vocabulary includes `None_` and `Max`; exact accepted subset remains model/endpoint-scoped | official docs say accepted values are model-dependent and can include `none`, `minimal`, `low`, `medium`, `high`, `xhigh`; OAS needs vocabulary + model subset, not one provider-wide enum | S2.3 |
| P1 | **OpenAI replay policy** | Responses `previous_response_id`, encrypted reasoning-item replay, `function_call`, and `function_call_output` manual replay are implemented; Chat Completions vs Responses matrix remains incomplete | reasoning items MUST replay with tool-call outputs (Responses) or `previous_response_id` | S3.2 |
| P2 | Gemini `thoughtSignature` | "soft preserve", summaries/signatures conflated | hard 400 if not echoed; parallel = first part only; signatures ≠ summaries | S3.2, S6.5 |
| P2 | Gemini `thinkingLevel` matrix | `supports_minimal:bool` only | low/medium/high; medium absent on gemini-3-pro; minimal Flash-only | S1.3 |
| P2 | Qwen DashScope `preserve_thinking` scope | applied to all DashScope | audit artifact says allowlist-only; official source capture required before implementation | S1.2 |
| P2 | Kimi visibility | `Provider_hidden`+`No_streaming_reasoning` | audit artifact says `reasoning_content` side-channel streamed before content; official source capture required before implementation | S3.2 |
| P2 | OpenAI Responses `phase` | not modeled | `phase:commentary/final_answer` round-trips on stateless replay | S3.2 |

Full per-finding verify reasoning and source URLs: audit artifact `wf_ad6e7c0c-aff` (2026-06-29), 51 agents, 6 provider docs scans. Rows marked Low/`확인 필요` below are not implementation authority until an official source or live API probe is recorded.

### 3c. Provider-currency evidence (checked 2026-06-29 12:36 KST)

| Claim | Source / command | OAS mapping | Confidence / uncertainty |
|---|---|---|---|
| OpenAI `reasoning.effort` and Responses replay are model-dependent typed facts, not a provider-wide enum. | [OpenAI reasoning guide](https://developers.openai.com/api/docs/guides/reasoning) — checked by browser fetch on 2026-06-29 12:36 KST. | `Reasoning_effort.t`, replay policy, Responses `phase` handling. | High. Uncertainty: exact accepted effort subset remains model-specific and must live in catalog/capability metadata. |
| Anthropic forced-tool limitation and thinking-block replay are provider/model facts. | [Claude extended thinking guide](https://platform.claude.com/docs/en/build-with-claude/extended-thinking) — checked by browser fetch on 2026-06-29 12:36 KST. | forced `tool_choice` capability, replay policy. | High. Uncertainty: partner-platform differences still need catalog rows. |
| Anthropic `thinking.display` drift. | `확인 필요`: audit artifact only in this PR update; capture official docs or live probe before code changes. | `anthropic_thinking_control` visibility mode. | Low. Do not implement from audit artifact alone. |
| Gemini thought signatures and `thinking_level` matrix are model-specific and signatures must be preserved in stateless mode. | [Gemini thinking guide](https://ai.google.dev/gemini-api/docs/thinking) — checked by browser fetch on 2026-06-29 12:36 KST. | `gemini_family`, signature carrier, replay policy, streaming field mapping. | High. Page last updated 2026-06-24 UTC. |
| GLM/Z.AI thinking fields, `clear_thinking`, `reasoning_content`, GLM-5.2 effort, and `tool_choice=auto` are typed dialect facts. | [Z.AI chat completion API](https://docs.z.ai/api-reference/llm/chat-completion) — checked by browser fetch on 2026-06-29 12:36 KST. | GLM thinking dialect, replay policy, forced-tool capability. | High. Uncertainty: GLM-4.5 guide should be kept as secondary docs only; API reference is the authority. |
| MiniMax M2/M3 thinking/replay semantics. | `확인 필요`: no official MiniMax source was captured in this PR update. | MiniMax provider dialect and replay policy. | Low. Do not implement from audit artifact alone. |
| Qwen/Kimi thinking visibility and replay details. | `확인 필요`: audit artifact only in this PR update; capture official docs or live probe before code changes. | DashScope/Kimi dialect rows. | Low. Treat as backlog evidence gap, not implementation authority. |

### 3d. Ollama transport evidence (checked 2026-07-17 KST)

| Claim | Source / command | OAS mapping | Confidence / uncertainty |
|---|---|---|---|
| Ollama OpenAI compatibility covers chat-completions streaming, Tools, `tool_choice`, vision, and reasoning controls, but describes compatibility with only parts of the OpenAI API. | [Ollama OpenAI compatibility](https://docs.ollama.com/api/openai-compatibility) — checked 2026-07-17 KST. | S1.5/S2.4 exact HTTP-codec qualification; no provider-global native-dialect inheritance. | High for documented compatibility surface. |
| Ollama's compatibility page documents Responses `truncation`, while OpenAI defines automatic truncation as dropping older input items when the request exceeds context. | [Ollama OpenAI compatibility](https://docs.ollama.com/api/openai-compatibility) and [OpenAI Responses API reference](https://platform.openai.com/docs/api-reference/responses/create) — checked 2026-07-17 KST. | `Provider_input_truncation_contract`; ordinary Tool continuation is fail-before-loss and lossy auto requires an explicit caller policy fact. | High for the documented wire option and OpenAI semantics; deployed Ollama behavior still requires a binding-revision probe. |
| Ollama documents `tool_choice`, but documentation is not evidence that every deployed revision semantically enforces every OpenAI choice mode. | [Ollama OpenAI compatibility](https://docs.ollama.com/api/openai-compatibility) and [official Ollama OpenAI compatibility implementation](https://github.com/ollama/ollama/blob/main/openai/openai.go) — checked 2026-07-17 KST. | S5.4 exact deployed-revision behavioral probes; unverified modes remain `Unsupported`. | Medium. Documentation and current implementation surface need end-to-end reconciliation. |
| Native Ollama GPT-OSS requires `think` level values (`low`/`medium`/`high`) and cannot fully disable thinking; other thinking models may document other accepted controls. The current page does not establish that booleans are ignored. | [Ollama Thinking](https://docs.ollama.com/capabilities/thinking) — checked 2026-07-17 KST. | S2.4 distinct native level contract; boolean/disable are rejected unless the exact binding documents them. | High for the documented level/disable behavior. |
| Ollama native streamed Tool loops must accumulate and replay assistant `thinking`, `content`, and `tool_calls`, and native ToolResults carry `tool_name`. | [Ollama Tool calling](https://docs.ollama.com/capabilities/tool-calling) and [Streaming](https://docs.ollama.com/capabilities/streaming) — checked 2026-07-17 KST. | S3.4/S4.4 native replay and independent Tool name. | High. |
| Ollama's OpenAI-compatible Responses surface is non-stateful. The page also lists “Stateful requests” in a nearby feature list, but its explicit limitation and unsupported `previous_response_id`/`conversation` text are the selected authority until Ollama resolves that documentation contradiction. | [Ollama OpenAI compatibility](https://docs.ollama.com/api/openai-compatibility) — `/v1/responses`, checked 2026-07-17 KST. | S3.5 rejects state handles before serialization. | Medium-High because the page is internally inconsistent; negative contract tests remain required. |
| Ollama Cloud currently does not support structured outputs, while local/native and OpenAI-compatible surfaces document schema carriers separately. | [Ollama Structured Outputs](https://docs.ollama.com/capabilities/structured-outputs) — checked 2026-07-17 KST. | S1.5/S9.4 endpoint+codec scoped structured-output fact; no provider-global inference. | High. |
| Official Cloud direct API documentation names `https://ollama.com/api`; a direct Cloud `/v1` reasoning carrier is not documented there. | [Ollama API introduction](https://docs.ollama.com/api/introduction) — checked 2026-07-17 KST; external live endpoint/config probe recorded separately. | S9.4 scopes `/v1` carrier evidence to endpoint+codec+model instead of promoting it to a provider-global fact. | High for official `/api`; Medium for empirical direct `/v1` carrier shape. |
| A native Ollama streaming request can begin with HTTP 200 and later emit an error object in the response stream. | [Ollama API errors](https://docs.ollama.com/api/errors) and [Streaming](https://docs.ollama.com/capabilities/streaming) — checked 2026-07-17 KST. | S6.9 typed declared failure after prior deltas; no success selection or blind retry concatenation. | High. |

## 4. Enforcement (강제 방법)

표준을 사람의 선의에 맡기지 않는다. 메커니즘:

1. **Compiler** — S1.3/S1.4/S6.1/S7.1은 closed sum + exhaustive match로 표현. 새 variant/format이 컴파일을 깨야 한다. S2.1 also needs a uniqueness gate while duplicate builders remain. (`_ -> ...` catch-all 추가는 CLAUDE.md 워크어라운드 체크리스트 4번에 걸린다.)
2. **CI grep gate** — S1.1/S2.2/S3.1: every model/provider-name
   `String.starts_with`, substring/regex family matcher, budget-to-effort
   threshold/mapping, and `is_glm_request` pattern is rejected. Grep is only a
   regression backstop; durable proof is exact catalog/binding identity plus
   closed capability variants and independent budget/effort fields. The stale
   `scripts/check-reasoning-effort-ssot.sh` guidance that asks callers to use
   nonexistent `Reasoning_effort.of_budget` is deleted or rewritten as a
   no-mapping gate. S1.1/S3.1 gates remain required follow-ups.
3. **Non-vacuous test** — S1.5-S1.8/S2.4/S3.4/S3.5/S4.4/S5.2-S5.4/S6.2/S6.6-S6.10/S7.3/S7.4/S8.1/S9.3/S9.5: revert 시 red 되는 테스트. The minimum matrix is native Ollama/OpenAI-compatible Chat/OpenAI-compatible Responses × sync/stream × reasoning/Tool/multimodal/structured-output with positive and negative rows. It proves the typed codec is not inferred from path or model spelling; arbitrary Ollama aliases (including misleading `gpt-*` aliases) gain no capabilities without an exact binding; local Ollama and Ollama Cloud with otherwise equal provider/model/codec/route fields remain distinct explicitly declared binding references when their verified facts differ, while moving one binding between physical URLs leaves its capabilities invariant; undeclared provider-kind/codec/route/revision tuples fail; one binding revision's request/sync/stream/finalize/continuation paths consume the same contract identity; mode-specific raw Tool grammars produce the same canonical semantics while undeclared carriers fail; malformed SSE followed by `[DONE]` and malformed native NDJSON followed by a terminal chunk both fail the whole attempt; native NDJSON preserves a final chunk containing data + `done:true` + usage/timing; native GPT-OSS requires an explicit level and rejects boolean/disable controls; OpenAI-compatible `none`/`max` request vocabulary is accepted only for an exact model/codec subset and never copied into native GPT-OSS; native streamed Tool round-trip preserves the assistant triple and `tool_name`; missing required native call ID fails without synthesis; stateless Responses rejects state handles; `n > 1` is either explicitly unsupported before serialization or preserves typed choice identity with no `choices[0]` fallback; Document cannot become Image without typed conversion; local/native `format:\"json\"`, native schema `format`, OpenAI-compatible JSON mode/schema, and Cloud structured-output rejection are distinct binding/codec facts; an unknown JSON Schema `type` never becomes a permissive wildcard. Kimi and MiniMax binding revisions lacking official/live replay evidence resolve to `Unknown` and perform zero continuation requests; adding an exact evidence revision, not changing a model/provider spelling, is the only path that can enable a closed replay policy.

   The compatible-transport rows additionally prove: identical SSE events parse
   under LF, CRLF (including a split CR/LF transport boundary), and bare CR;
   a buffered event without the required blank-line terminator is not
   dispatched at EOF; terminal-before-data, duplicate terminal, and data after
   terminal fail; a valid-delta then HTTP-200 in-band error commits a declared
   failure with no selection/Tool dispatch, while the word `"error"` inside
   ordinary text does not; `.done` snapshots validate/adopt exactly once;
   unsupported Tool-choice modes perform zero HTTP requests; and accepted
   forced modes satisfy the exact binding's declared response obligation or
   fail protocol validation. Context overflow in `Fail_on_overflow` preserves
   the exact request snapshot, while `Lossy_auto` performs zero HTTP requests
   without an explicit application-policy witness and, with one, records
   `Provider_may_have_truncated` rather than exact delivery. OpenAI Chat SSE,
   Responses SSE, Ollama-compatible Chat/Responses, and native Ollama NDJSON
   each carry at least one terminal/error/truncation/Tool-choice negative row.
4. **Workaround-signature gate (planned)** — §5의 remediation을 그 workaround twin으로 구현하는 PR은 a future repo-versioned RFC gate에 걸려 거부돼야 한다. This RFC does **not** check in `scripts/ci/pr-rfc-check.sh`; until that lands, this rule is Advisory/reviewer-enforced. counter-as-fix / string-classifier 보강 / N-of-M / cap-cooldown-dedup-repair 금지.

## 5. Remediation backlog + sequencing

RFC 컬럼: **RFC** = dialect/capability *type shape* 변경 또는 N-of-M reshape(workaround gate planned; reviewer-enforced until it is versioned); **Direct** = 순수 삭제/dedup/위임(시그니처 트리거 없음). 키스톤은 **RFC-OAS-023**.

### 먼저 (keystone, 가장 많이 unblock)
1. **Ollama transport-qualified capability repair (P0).** Resolve the exact
   model capability/dialect from the revisioned
   `Provider_binding_reference.t` row containing
   `(provider, model, Provider_http_codec.t, Provider_endpoint_route.t,
   wire-evidence revision)`;
   make codec an explicit typed config field, delete request-path codec
   inference, validate the catalog-declared tuple, and keep one `ollama_cloud`
   provider identity. Add regressions for
   OpenAI-compatible DeepSeek `reasoning_content`, MiniMax `reasoning`, native
   boolean versus GPT-OSS level `think`, `message.thinking`, native full
   Tool-loop replay plus `tool_name`, stateless Responses rejection,
   Cloud-scoped structured-output rejection, and Document-not-Image handling.
   Do not add URL/model string branches or a second provider identity.
2. **Provider parser boundary repair (P0).** Pass the same contract to sync and
   stream parsing, remove alternate reasoning-carrier probing, declare
   mode-specific Tool-argument grammars with canonical semantic parity, and
   make both SSE and native NDJSON decoding total. The regressions
   `malformed SSE Tool frame -> later [DONE]` and
   `malformed NDJSON Tool frame -> later terminal chunk` must close the whole
   attempt as protocol failure with prior deltas preserved.
3. **RFC-OAS-023 — GLM typed dialect reshape.** GLM-ness를 typed kind/capability로 1회 승격, `replay_policy`와 `Thinking_object`-style thinking-control variant 부여, 그 다음 남은 복수 thinking 호출 surface(S2.1)를 통합한다. 핵심 잔여 결함은 raw string classifier 보강이 아니라 OpenAI-compatible GLM tuple이 typed kind coverage에서 빠지는 권한 gap이다.
4. **thinking-request integration 통합 (D1, P1)** — (3) 직후/내부. Canonical field builder는 이미 공유되므로 다시 만들지 않는다. 두 request integration surface가 그 한 contract/builder를 소비하는 drift test를 추가하고 중복 조립만 제거한다.
5. **content_type stream-boundary policy completion (D3-finalize, P1, partial)** — GLM과 독립, streaming blast radius 최대. Current branch ancestry already converts the wire `content_type` to `block_kind`, handles `Unknown_block` explicitly, and routes unknown-event/parse failures to typed stream errors. Remaining work is narrower: make every final parse/finalize path obey S6.1's fail-closed rule and test that unknown content-block kinds cannot be preserved as differently typed visible text or omitted as empty success.

### 다음 (배포/사용 surface의 정합성 drift)
6. OpenAI phase/replay matrix residual (P1/P2) — `none`/`minimal`/`xhigh` model-dependent vocabulary/subset, Responses `previous_response_id`, encrypted reasoning-item replay, and `function_call_output` manual replay are implemented in current branch ancestry. Remaining work is narrower: Chat Completions vs Responses replay-mode matrix, strict `json_schema` catalog facts, and `phase:commentary/final_answer` stateless replay modeling.
7. Anthropic thinking drift + `tool_choice`-400 (P1×2) — forced tool + thinking hard-400 is verified; `thinking.display` visibility drift needs official source refresh before code changes.
8. MiniMax replay/tool-choice evidence + catalog field fix (P1) — catalog rows already exist; do not add a duplicate provider. First capture official/live evidence, then update the existing capability/replay rows instead of relying on `No_replay` defaults that can silently break interleaved thinking.
9. ~~중복 stream accumulator 제거 (D4, P2)~~ — resolved at the execution
   authority boundary: supported `Agent_sdk.Streaming` routes to
   `Complete_stream_acc`. `lib/streaming.ml` still contains the narrowed public
   re-export record/functions and `test_stream_accumulator.ml` still verifies
   that façade; they are not a second accumulator implementation and are not
   falsely described as deleted.

### 미뤄도 안전 (latent, 현재 배포 모델 트리거 없음) — typed cleanup으로 batch
- `D5-anthropic-thinkmode-hardcoded-prefix-table` and
  `D4-provider-preset-stale-numeric-limits` remain SSOT/hardcode debt to fold
  into catalog-field work. `D2-budget-to-effort` is already hard-cut in current
  main; retain the independent-field regression and remove the stale gate
  instruction rather than reintroducing a central heuristic.
- **Partially closed before this RFC update (do not redo wholesale)**: `D3-finalize` already has the `block_kind` conversion, explicit `Unknown_block` handling, and typed `SSEUnknownEventType`/parse-error propagation in `Complete_stream_acc`; keep only the residual policy/test work listed above.
- **Closed before this RFC update (do not redo)**: `D-TOOLS-1` and `D-TOOLS-8` were removed with the entire recovery module; no provider-gated text-to-tool fallback remains. The standalone `D3-tool-pair-silent-drop` repair/filter module was also deleted. `D-TOOLS-6` now delegates through the typed agent-tool path, and `D-TOOLS-9` rejects unknown harness schema types. These rows remain historical evidence, not open backlog.
- **Direct, RFC 불필요, 저위험 (언제든)**: `D4-test-only-normalize-effort-wrapper` should be narrowed to any truly dead wrapper only; keep `Reasoning_dialect.normalize_effort_value`, which is a live backend dependency required by S2.2. Also: `D7-anthropic-prefix-list-literal-duplicates`(dedupe), `D8-manifest-precedence` 문서/테스트, Kimi visibility 사실.
- `D7-gemini-family-leaks-second-string-match`(P3) + Gemini `supports_medium`/`thoughtSignature` strictness: 단일 Gemini variant reshape로 fold.

### Backlog 자체의 가드레일
여러 "root fix"는 그 workaround twin으로 구현하면 안 된다. 삭제된
`tool_message_pairs` 필터를 drop counter로 복원하지 말고, 필요한 불변식은 typed
append/result 경계에서 보장한다. 삭제된 `tool_use_recovery` 대신 provider
telemetry gate나 lenient repair를 추가하지 말고, `Text` → `ToolUse` 승격 금지를
유지한다.

## 6. Boundary note (OAS ↔ MASC)

경계는 대체로 올바르다: MASC는 `Llm_provider.Capabilities`를 typed로 직접 소비하고(`runtime_wire_overlay.ml: agent_capabilities_of_llm_capabilities`가 OAS variant를 verbatim 통과) model 이름 string-match로 reasoning을 결정하지 않는다. OAS는 MASC를 모른다.

단 하나의 경계 부채(MASC측, 정보용): `masc lib/runtime/runtime_schema.ml`이 자체 `thinking_control_format`를 **재선언(5/9 variant, `Thinking_object_adaptive`/`Thinking_object_only`/`Enable_thinking`/`Ollama_think` 누락)** 한다. parse는 unknown에서 fail-closed(silent 아님)지만, OAS에 새 variant가 추가돼도 MASC 컴파일이 깨지지 않아 drift 무방비다. 게다가 그 필드는 wire 경로에서 읽히지 않아 운영자 TOML 설정이 inert no-op(의도-침묵)이다. **P2 SSOT 부채(데이터 경로는 안전).** 해결: 필드 삭제(OAS catalog가 단일 SSOT) 또는 OAS variant 집합에 대한 exhaustive drift 테스트. 이는 OAS 변경이 아니라 MASC 후속 작업이며, 본 RFC의 S9.1을 경계 너머로 확장한 사례로 기록한다. Tracking issue: [jeong-sik/masc#22654](https://github.com/jeong-sik/masc/issues/22654).

## 7. Relationships
- **RFC-OAS-023** (capability axis reshape) — GLM/MiniMax dialect 작업과 model×transport two-record가 여기 land. 본 RFC는 그 작업이 만족해야 할 표준을 정의한다.
- **`Agent_tools.find_in_index` contract** — S4의 exact registered-name 기반.
- **RFC-OAS-018** (catalog externalization) — S1.2/S9.1의 catalog-as-SSOT 기반.
- **RFC-OAS-025** (forced-tool-use enforcement boundary) — S5의 기반.
- **CLAUDE.md 워크어라운드 거부 기준** — S10.2/§4.4의 enforcement 원천.
