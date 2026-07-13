#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$repo_root"

forbidden='max_turns|max_idle_turns|idle_final_warning_at|max_tool_calls_per_turn|max_tool_calls|max_tool_rounds|Agent_turn_budget|MaxTurnsExceeded|IdleDetected|exit_condition|ExitConditionMet|budget_exhausted|turn_budget|tool_round_budget|OAS_MCP_OUTPUT_MAX_TOKENS|output_token_budget|truncate_output|default_max_tool_result_chars|max_result_chars|tool_result_relocation|ContentReplacement|call_time_pruner_keep_recent|call_time_pruner_keep_last|consecutive_idle_turns|set_consecutive_idle_turns|check_loop_guard|Tool_id|risk_classifier|risk_of_score|TurnCountAtMost|TurnCountBelow|turn_count_ceiling|Turn_count_at_most|trace_turn_count_at_most|missing_approval_callback_policy|Execute_without_callback|Reject_without_callback|parse_judge_response|Policy_channel|policy_channel|Tool_op|default_context_reducer|operator_policy|merge_operator_policy|policy_source|Budget_strategy|auto_context_overflow_retry|context_compact_ratio|context_prepare_ratio|context_handoff_ratio|with_context_thresholds|compact_attempts|proactive_compact|emergency_compact|from_context_config|PreCompact|PostCompact|OnContextCompacted|pre_compact|post_compact|on_context_compacted|ContextCompacted|ContextCompactStarted|ContextOverflowImminent|resolve_max_context_tokens|Agent_tool_name_alias|Tool_selector|Progressive_tools|disclosure_level|disclosure_resolver|Disclosure_resolver|schema_to_json_with_disclosure|Minimal_index|Agent_handoff|find_handoff_in_messages|replace_tool_result|handoff_prefix|target_name_of_tool|transfer_to_|mutation_class|Typed_tool_safe|typed_tool_safe|Tool\.permission|Tool\.(ReadOnly|Write|Destructive)|permission_to_string|is_read_only|Sdk_client_types|Runtime_server_control|runtime_mcp_policy|runtime_mcp_server|request_id = "legacy"|Guardrails\.|Agent_sdk_base\.Guardrails|tool_filter([^_[:alnum:]]|$)|AllowAll|AllowList|DenyList|tool_filter_override|effective_guardrails|Tool_set\.filter|with_guardrails([^_[:alnum:]]|$)|~guardrails([^_[:alnum:]]|$)|default_(attempt|request|http|connect|stream_idle)_timeout_s|complete_with_retry|complete_stream_with_retry|Retry_classify|Retry\.(with_retry|with_retry_map_error|calculate_delay|default_config)|Complete\.default_retry_config|type retry_config =|module Retry_common|module Structured_retry'

if rg -n --glob '*.ml' --glob '*.mli' -e "$forbidden" lib test examples; then
  printf '%s\n' \
    'agent unbounded contract violated: lifecycle caps, numeric sentinels, synchronous automatic retry, or implicit result rewriting re-entered lib/' \
    >&2
  exit 1
fi

if rg -n \
  -e '\?base_url([^_[:alnum:]]|$)' \
  -e '\?provider([^_[:alnum:]]|$)' \
  lib/api.ml lib/api.mli lib/streaming.ml lib/streaming.mli; then
  printf '%s\n' \
    'provider call contract violated: provider and endpoint ownership must be explicit, not optional compatibility arguments' \
    >&2
  exit 1
fi

if [[ -e lib/base/tool_id.ml \
   || -e lib/base/tool_id.mli \
   || -e lib/agent/agent_tool_name_alias.ml \
   || -e lib/agent/agent_tool_name_alias.mli \
   || -e lib/tool_selector.ml \
   || -e lib/tool_selector.mli \
   || -e lib/tool_index.ml \
   || -e lib/tool_index.mli \
   || -e lib/progressive_tools.ml \
   || -e lib/progressive_tools.mli \
   || -e lib/agent/disclosure_resolver.ml \
   || -e lib/agent/disclosure_resolver.mli \
   || -e lib/agent/agent_handoff.ml \
   || -e lib/agent/agent_handoff.mli \
   || -e lib/llm_provider/retry_classify.ml \
   || -e lib/llm_provider/retry_classify.mli \
   || -e lib/sdk_client_types.ml \
   || -e lib/sdk_client_types.mli \
   || -e lib/runtime_server_control.ml \
   || -e lib/typed_tool_safe.ml \
   || -e lib/typed_tool_safe.mli ]]; then
  printf '%s\n' \
    'agent boundary contract violated: OAS must not own a builtin tool-name catalog or tool permission taxonomy' \
    >&2
  exit 1
fi

if [[ -e lib/context/budget_strategy.ml \
   || -e lib/context/budget_strategy.mli ]]; then
  printf '%s\n' \
    'agent unbounded contract violated: automatic context budget strategy re-entered OAS' \
    >&2
  exit 1
fi

if ! rg -q -F 'let protocol_version = "oas-runtime-0.2"' lib/runtime.ml; then
  printf '%s\n' \
    'runtime boundary contract violated: the hard-cut wire must identify itself as oas-runtime-0.2' \
    >&2
  exit 1
fi

if rg -n \
  -e 'ask_permission|invoke_hook|permission_mode|approval_mode|policy_snapshot|requested_policy|control_response_timeout_s|Permission_request|Permission_response|Hook_request|Hook_response|Control_request_message|Control_response_message' \
  lib/runtime.ml \
  lib/runtime.mli \
  lib/runtime_projection.ml \
  lib/runtime_server.ml \
  lib/runtime_server.mli \
  lib/runtime_server_types.ml \
  lib/runtime_server_types.mli \
  lib/runtime_store.ml \
  lib/sessions_types.ml \
  lib/sessions_types.mli; then
  printf '%s\n' \
    'runtime boundary contract violated: implicit permission or hook control re-entered the runtime server' \
    >&2
  exit 1
fi

if rg -n -e 'if raw = "" then loop|exception Eio\.Io _ -> \(\)' lib/runtime_server.ml; then
  printf '%s\n' \
    'runtime observability contract violated: malformed input or I/O failure must not disappear silently' \
    >&2
  exit 1
fi

if [[ -e scripts/lint-core-cdal-boundary.sh ]]; then
  printf '%s\n' \
    'agent boundary contract violated: legacy product-governance lint re-entered OAS' \
    >&2
  exit 1
fi

if rg -n -e 'backpressure_policy|Drop_oldest|Drop_newest|dropped_total|total_publish_blocked_seconds|buffer_size' \
  lib/event_bus.ml lib/event_bus.mli lib/telemetry_bus.ml lib/telemetry_bus.mli; then
  printf '%s\n' \
    'agent event contract violated: subscribers must use lossless unbounded queues without publisher backpressure policy' \
    >&2
  exit 1
fi

if rg -n -e 'async_stream_capacity|OAS_WIRE_CAPTURE_QUEUE|OAS_WIRE_CAPTURE_MAX_BYTES|Eio\.Stream|Eio\.Switch\.on_release|take_nonblocking|queue full|dropping chunk|drops newest chunk|raw-stream\.jsonl|rotate_file|prune_over_cap|append_bounded|unlink_if_exists|\?sw:Eio\.Switch' \
  lib/llm_provider/wire_capture.ml lib/llm_provider/wire_capture.mli; then
  printf '%s\n' \
    'wire capture contract violated: exact-ID segments must be append-only, lossless, unbounded, and switch-scoped' \
    >&2
  exit 1
fi

if rg -n -e 'Eio\.Switch\.run \(fun wire_sw|~sw:wire_sw' \
  lib/llm_provider/complete_stream.ml; then
  printf '%s\n' \
    'wire capture contract violated: request teardown must not join observer I/O' \
    >&2
  exit 1
fi

if ! rg -q 'Wire_capture\.close wire_sink' lib/llm_provider/complete_stream.ml; then
  printf '%s\n' \
    'wire capture contract violated: every stream must close its request-local FIFO' \
    >&2
  exit 1
fi

if [[ -e lib/approval.ml \
   || -e lib/approval.mli \
   || -e lib/policy.ml \
   || -e lib/policy.mli \
   || -e lib/judge/judge.ml \
   || -e lib/judge/judge.mli \
   || -e lib/policy_channel.ml \
   || -e lib/policy_channel.mli \
   || -e lib/tool_op.ml \
   || -e lib/tool_op.mli ]]; then
  printf '%s\n' \
    'agent boundary contract violated: fixed-risk or priority governance re-entered OAS' \
    >&2
  exit 1
fi

if [[ -e lib/base/guardrails.ml || -e lib/base/guardrails.mli ]]; then
  printf '%s\n' \
    'agent boundary contract violated: synchronous name-based tool filtering re-entered OAS' \
    >&2
  exit 1
fi

if rg -n \
  -e 'default_http_timeout_s|with_optional_timeout|\?\(timeout_s[[:space:]]*=|\?\(connect_timeout_s[[:space:]]*=|drain_response_body.*timeout_s|drain_response_body[[:space:]]+\?clock' \
  lib/llm_provider/http_client.ml \
  lib/llm_provider/http_client.mli; then
  printf '%s\n' \
    'HTTP boundary contract violated: deadlines must be caller-supplied and clock-backed' \
    >&2
  exit 1
fi

printf '%s\n' 'agent unbounded contract passed'
