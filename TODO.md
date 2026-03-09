# `agent_sdk` Roadmap

## High Priority

- [ ] Add typed multimodal content blocks (`Image`, `Document`) and response parsing
- [ ] Add prompt caching primitives and cache-control helpers
- [ ] Add MCP resource / prompt surfaces so orchestration layers can mount MCP without flattening everything into tools
- [ ] Add richer permission adapters for interactive approval flows beyond hook-based allow/deny

## Medium Priority

- [ ] Add async/background hook adapters (command / HTTP / prompt) instead of raw callback-only hooks
- [ ] Add session persistence helpers for transcript snapshots and resume points
- [ ] Add first-class output artifact types instead of string-only tool results
- [ ] Add stronger frontmatter parsing or structured config loading for skills/subagents

## Low Priority

- [ ] Property-based tests for tool loop and permission transitions
- [ ] Better performance benchmarks for parallel tool execution and multi-agent delegation
- [ ] Compaction / memory summarization hooks for long-running sessions
