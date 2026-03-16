# Agent SDK OCaml - Roadmap

## Done (v0.3.0)

- [x] **Streaming (SSE) 지원**: `Cohttp_eio` 기반 SSE 파싱, `create_message_stream` + `on_event` 콜백. content block 누적 후 완성된 `api_response` 반환.
- [x] **Detailed Error Handling**: `Retry.api_error` 7-variant 타입 (RateLimited, Overloaded, ServerError, AuthError, InvalidRequest, NetworkError, Timeout) + exponential backoff with jitter.
- [x] **Multimodal 지원**: `Image`, `Document` content_block variant. base64 source 직렬화/역직렬화.
- [x] **Middleware/Hooks**: `Hooks` 모듈 -- BeforeTurn, AfterTurn, PreToolUse (Skip/Override), PostToolUse, OnStop lifecycle events.
- [x] **PR 상신 및 Merge**: v0.3.0 main 머지 완료.
- [x] **Property-based Testing**: `QCheck` 기반 도구 호출 시나리오 엣지 케이스 탐색 (`test/test_property.ml`).

## Remaining

- [ ] **Prompt Caching**: `cache_control` 파라미터 지원. system prompt에 `{"type":"ephemeral"}` 캐시 제어 + usage 통계에 cache_creation/cache_read 토큰 추출.
- [ ] **Eio Multicore 활용**: 여러 에이전트를 동시에 돌릴 때 멀티코어 성능을 극대화할 수 있도록 `Eio.Executor` 활용 검토.

---
*Last Updated: 2026-03-16*
