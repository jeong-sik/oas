# Agent SDK OCaml - 2026 Roadmap

최신 Anthropic Claude 3.7+ 스펙 반영 및 프로덕션 급 완성도를 위한 할 일 목록

## 🚀 High Priority (핵심 기능)
- [ ] **Streaming (SSE) 지원**: `Cohttp_eio`를 활용하여 `Thinking` 블록과 `Text` 블록을 실시간으로 스트리밍하는 기능 구현.
- [ ] **PR 상신 및 Merge**: 현재 `workspace`에 있는 코드를 `main` 브랜치로 안전하게 머지 (Worktree 활용 필수).
- [ ] **Detailed Error Handling**: API 에러 응답(429, 503 등)을 단순 문자열이 아닌 커스텀 Variant 타입으로 정의하여 재시도 로직과 연동.

## 🛠 Medium Priority (사용성 개선)
- [ ] **Multimodal 지원**: `ContentBlock`에 `Image` 및 `Document` (PDF 등) 타입 추가.
- [ ] **Prompt Caching**: `cache_control` 파라미터 지원 및 에이전트 레벨의 캐싱 전략 구현.
- [ ] **Middleware/Hooks**: 에이전트 실행 전후에 로깅이나 인시던트 트래킹을 할 수 있는 훅 시스템 도입.

## 🧪 Low Priority (최적화 및 테스트)
- [ ] **Eio Multicore 활용**: 여러 에이전트를 동시에 돌릴 때 멀티코어 성능을 극대화할 수 있도록 `Eio.Executor` 활용 검토.
- [ ] **Property-based Testing**: `Crowbar` 등을 사용하여 도구 호출 시나리오의 엣지 케이스 자동 탐색.

---
*Last Updated: 2026-02-20*
