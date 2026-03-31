# Agent Surface Hardening Backlog

This backlog captures issue-ready follow-ups discovered while tightening
tests and public surfaces in `agent_sdk`.

## Near-term issues

### `test: retire coverage-only suites with low behavioral signal`
- Remove tests that only exercise re-exports, `non-empty` strings, or
  "does not crash" paths without asserting user-visible behavior.
- Merge any valuable assertions into behavior-first suites that exercise
  the real runtime path.
- Add a contributor rule: new tests must fail for a real behavioral
  regression, not only for bisect coverage loss.

### `hooks: define a legal decision matrix for each hook stage`
- Make unsupported hook decisions fail closed everywhere.
- Document which decisions are legal for `before_turn`,
  `before_turn_params`, and `pre_tool_use`.
- Add regression tests for invalid decisions so hook semantics stay stable.

### `subagent: complete or remove prompt-only delegation metadata`
- Revisit `state_isolation`: either wire it to actual `Context` scoping
  or reduce it to explicit documentation-only metadata.
- Keep `Subagent` focused on features the runtime can enforce.
- Decide whether handoff children should share context by copy, by merge,
  or through an explicit propagation policy.

### `skills: separate discovery metadata from runtime prompt behavior`
- Keep `with_skill_registry` for agent-card export and registry discovery.
- Keep `with_skill` / `with_skills` for runtime prompt composition.
- Add docs and examples that make this distinction obvious.

### `a2a: expand real HTTP lifecycle coverage`
- Add more end-to-end tests around `A2a_server.start`, concurrent clients,
  and shutdown behavior.
- Decide whether `A2a_server` should expose a long-running serve API or
  stay as a lightweight embeddable helper.
- A2A v1 adoption scope is tracked separately in
  [`a2a-v1-adoption-decision.md`](./a2a-v1-adoption-decision.md) and `#523`.

## RFC candidates

### RFC: `AskUserQuestion` / resumable human-in-the-loop
- Current `ElicitInput` is synchronous callback plumbing.
- A fuller design would pause the run, surface pending questions or tool
  approvals, and resume from saved state.
- This should cover both direct questions and approval-gated tool calls.

### RFC: `agents as tools`
- OpenAI Agents SDK exposes specialized agents as callable tools without
  a full handoff.
- OAS currently has handoffs plus async agents, but not a first-class
  "agent as tool" abstraction with output extraction or approval gates.
- Candidate scope: agent wrapper tool, structured input schema, nested
  streaming callbacks, and custom output extraction.

### RFC: conditional handoff / tool enablement
- OpenAI handoffs and tool-agents support runtime `is_enabled` checks.
- OAS could add a typed capability gate for subagents or agent-backed tools
  so routing is explicit and testable.

### RFC: hosted or deferred tool search
- OpenAI Agents SDK supports hosted tool search to avoid loading large tool
  surfaces eagerly.
- OAS already has `Progressive_tools`; a deeper RFC could define dynamic
  tool discovery for local tools, MCP tools, and subagents.

### RFC: nested agent streaming events
- Nested agent runs in OpenAI's SDK can emit ordered streaming callbacks.
- OAS handoffs currently collapse child output to final text.
- A streaming child-event bridge would make multi-agent debugging and audit
  trails much stronger.

## External reference points

- OpenAI Agents SDK overview: <https://openai.github.io/openai-agents-python/agents/>
- OpenAI handoffs reference: <https://openai.github.io/openai-agents-python/ref/handoffs/>
- OpenAI tools guide: <https://openai.github.io/openai-agents-python/tools/>
- OpenAI guardrails reference: <https://openai.github.io/openai-agents-python/ref/guardrail/>
- OpenAI HITL guide: <https://openai.github.io/openai-agents-js/ja/guides/human-in-the-loop/>
