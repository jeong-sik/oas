# Coverage ratchet evidence - 2026-05-25

Scope: #1175 Stage C/D continuation. PR #1759 raises measured project coverage
above 77% with behavior-backed provider dispatch, provider_d codec, streaming,
pure-module, harness, and CLI transport tests, so the CI coverage floor can
move from 73% to 76%.

## Measurement

- Evidence: local clean full coverage run on PR #1759 after the Stage D
  client/pipeline/pure-module/harness/CLI transport follow-ups.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes EIO_BACKEND=posix BISECT_FILE=/tmp/oas_stage_d_full_after_ad_error_validation_coverage/bisect scripts/dune-local.sh runtest --force --instrument-with bisect_ppx`
- Summary command:
  `opam exec -- bisect-ppx-report summary --coverage-path=/tmp/oas_stage_d_full_after_ad_error_validation_coverage`
- Measured coverage: `77.72%` (`23104/29728`)
- Earlier Stage C clean measurement before the Stage D follow-ups:
  `76.02%` (`22598/29728`)
- Timestamp: 2026-05-25 KST
- Confidence: High for local measurement; GitHub full CI remains the final
  merge gate after the draft PR is marked ready.

## Ratchet decision

- Base threshold before this PR: `73`
- Intermediate threshold after the Stage C slice: `75`
- New threshold after the Stage D follow-ups: `76`
- Formula: `floor(77.72 - 1) = 76`
- Reason: preserve one percentage point of CI/runtime headroom while enforcing
  the measured recovery toward the 80% terminal goal.

## #1175 record

- Stage C target (`60 -> 75`): floor reaches 75.
- Stage D target (`75 -> 80`): in progress; floor now reaches 76 and still
  needs further measured slices before 80.
- Top 0% files removed in this PR: none; this slice targets low/mid coverage
  provider and streaming surfaces rather than zero-coverage files.
- Tests added in this PR:
  - `test/test_provider_intf.ml`: provider dispatch error/custom-provider paths.
  - `test/test_backend_provider_d_codec.ml`: provider_d serialization, schema,
    parse, telemetry, reasoning, fenced JSON, and malformed tool-call branches.
  - `test/test_streaming_coverage.ml`: stream accumulator/finalize/http-error
    edge branches.
  - `test/test_streaming_edge_cases.ml`: provider_a SSE, provider_d,
    provider_f, Ollama NDJSON, and synthetic-event edge branches.
  - `test/test_client_wrapper_coverage.ml`: public client wrapper show/default
    aliases.
  - `test/test_pipeline_common_coverage.ml`: pipeline strategy/outcome,
    completion-contract validation, event envelopes, and tiered-memory token
    counting.
  - `test/test_transport_cli_tool_a_coverage.ml`: Agent_code JSONL,
    runtime MCP env-token override, stdout recovery, stdin prompt routing, and
    parse-error paths.
  - `test/test_transport_cli_tool_d_coverage.ml`: Agent_llm_a stream-json,
    JSON output, runtime MCP args, max-turns terminal mapping, stdin prompt
    routing, and parse-error paths.
  - `test/test_llm_provider_error.ml`: remaining retry/http/provider-failure
    mapping variants and retryability matrix.
  - `test/test_tool_input_validation.ml`: scalar coercion edges, optional null,
    non-object input, JSON actual descriptions, and inline formatting fallback.

## Stage D focused follow-up

- Evidence: local focused coverage run on PR #1759 after adding the client and
  pipeline-common tests.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes BISECT_FILE=/tmp/oas_stage_d_focus_coverage/bisect scripts/dune-local.sh build --instrument-with bisect_ppx test/test_client_wrapper_coverage.exe test/test_pipeline_common_coverage.exe test/test_provider_intf.exe`
- Test binaries:
  `./_build/default/test/test_client_wrapper_coverage.exe`,
  `./_build/default/test/test_pipeline_common_coverage.exe`,
  `./_build/default/test/test_provider_intf.exe`
- Summary command:
  `opam exec -- bisect-ppx-report summary --per-file --coverage-path=/tmp/oas_stage_d_focus_coverage`
- Focused per-file results:
  - `lib/client.ml`: `74.47%` (`35/47`)
  - `lib/pipeline/pipeline_common.ml`: `100.00%` (`24/24`)
  - `lib/provider_intf.ml`: `53.70%` (`29/54`)
- Timestamp: 2026-05-25 KST
  Confidence: High for focused slice coverage; project-wide CI coverage is now
  governed by the latest `76` floor above.

## Stage D pure-module follow-up

- Evidence: local focused coverage run on PR #1759 after extending
  `test/test_tool_schema_gen.ml` and `test/test_memory_procedural.ml`.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes BISECT_FILE=/tmp/oas_stage_d_pure_coverage/bisect scripts/dune-local.sh build --instrument-with bisect_ppx test/test_tool_schema_gen.exe test/test_memory_procedural.exe`
- Test binaries:
  `./_build/default/test/test_tool_schema_gen.exe`,
  `./_build/default/test/test_memory_procedural.exe`
- Summary command:
  `opam exec -- bisect-ppx-report summary --per-file --coverage-path=/tmp/oas_stage_d_pure_coverage`
- Focused per-file results:
  - `lib/tool_schema_gen.ml`: `82.22%` (`74/90`)
  - `lib/memory_procedural.ml`: `83.78%` (`62/74`)
  - `lib/memory.ml`: `23.48%` (`85/362`)
  - `lib/tool_input_validation.ml`: `20.37%` (`22/108`)
- Timestamp: 2026-05-25 KST
  Confidence: High for focused pure-module coverage; project-wide CI coverage
  is now governed by the latest `76` floor above.

## Stage D harness and CLI transport follow-up

- Evidence: local focused coverage run on PR #1759 after extending
  `test/test_harness_runner.ml` and adding subprocess-backed
  `test/test_transport_cli_tool_b_coverage.ml` /
  `test/test_transport_cli_tool_c_coverage.ml`.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes BISECT_FILE=/tmp/oas_stage_d_cli_harness_coverage/bisect scripts/dune-local.sh build --instrument-with bisect_ppx test/test_harness_runner.exe test/test_transport_cli_tool_b_coverage.exe test/test_transport_cli_tool_c_coverage.exe`
- Test binaries:
  `./_build/default/test/test_harness_runner.exe`,
  `./_build/default/test/test_transport_cli_tool_b_coverage.exe`,
  `./_build/default/test/test_transport_cli_tool_c_coverage.exe`
- Summary command:
  `opam exec -- bisect-ppx-report summary --per-file --coverage-path=/tmp/oas_stage_d_cli_harness_coverage`
- Focused per-file results:
  - `lib/harness_runner.ml`: `77.13%` (`172/223`)
  - `lib/llm_provider/transport_cli_tool_b.ml`: `52.16%` (`133/255`)
  - `lib/llm_provider/transport_cli_tool_c.ml`: `63.10%` (`183/290`)
  - `lib/llm_provider/cli_common_subprocess.ml`: `47.43%` (`83/175`)
  - `lib/llm_provider/cli_common_synthetic_events.ml`: `55.00%` (`11/20`)
- Behaviors added:
  - harness response/trace/metric pass and fail grading, skipped cases, trace
    path validation, and execute-case failure shaping.
  - `cli_tool_b` Provider_f JSON parsing, synthetic stream replay,
    request-scoped MCP rejection, and terminal quota stderr classification.
  - `cli_tool_c` JSONL text/tool/tool-result restoration, stream event
    emission, subprocess-error surfacing, no-output parse failure, stdin prompt
    routing, and session delta reuse.
- Timestamp: 2026-05-25 KST
  Confidence: High for focused behavior coverage; project-wide CI coverage is
  now governed by the latest `76` floor above.

## Stage D CLI A/D and validation follow-up

- Evidence: local focused coverage run on PR #1759 after adding
  subprocess-backed `test/test_transport_cli_tool_a_coverage.ml` and
  `test/test_transport_cli_tool_d_coverage.ml`, then extending
  `test/test_llm_provider_error.ml` and `test/test_tool_input_validation.ml`.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes BISECT_FILE=/tmp/oas_stage_d_cli_ad_error_validation_coverage/bisect scripts/dune-local.sh build --instrument-with bisect_ppx test/test_transport_cli_tool_a_coverage.exe test/test_transport_cli_tool_d_coverage.exe test/test_llm_provider_error.exe test/test_tool_input_validation.exe`
- Test binaries:
  `./_build/default/test/test_transport_cli_tool_a_coverage.exe`,
  `./_build/default/test/test_transport_cli_tool_d_coverage.exe`,
  `./_build/default/test/test_llm_provider_error.exe`,
  `./_build/default/test/test_tool_input_validation.exe`
- Summary command:
  `opam exec -- bisect-ppx-report summary --per-file --coverage-path=/tmp/oas_stage_d_cli_ad_error_validation_coverage`
- Focused per-file results:
  - `lib/llm_provider/error.ml`: `81.95%` (`109/133`)
  - `lib/tool_input_validation.ml`: `87.04%` (`94/108`)
  - `lib/llm_provider/transport_cli_tool_a.ml`: `73.09%` (`277/379`)
  - `lib/llm_provider/transport_cli_tool_d.ml`: `69.40%` (`220/317`)
  - `lib/llm_provider/cli_common_subprocess.ml`: `48.57%` (`85/175`)
- Behaviors added:
  - `cli_tool_a` JSONL text/tool/tool-result restoration, telemetry for
    provider-internal command execution, stream events, nonzero-exit stdout
    recovery, runtime MCP bearer-token env indirection, stdin prompt routing,
    and empty-output parse failure.
  - `cli_tool_d` stream-json structured text/thinking/tool-use restoration,
    stream events, JSON-output fallback, internal max-turns terminal mapping,
    runtime MCP argv construction, stdin prompt routing, and empty-output parse
    failure.
  - provider error mapping for retry/auth/invalid/not-found/context/network/
    timeout variants, provider-failure subtypes, accept rejection, terminal
    `Other`, and retryability classification.
  - tool input validation for scalar coercion edge cases, JSON actual
    descriptions, optional null, non-object input, and inline path fallback.
- Timestamp: 2026-05-25 KST
  Confidence: High for focused behavior coverage; project-wide CI coverage is
  now governed by the latest `76` floor above.
