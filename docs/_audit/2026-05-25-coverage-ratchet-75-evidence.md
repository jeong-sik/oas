# Coverage ratchet evidence - 2026-05-25

Scope: #1175 Stage C continuation. PR #1759 raises measured project coverage
above 76% with behavior-backed provider dispatch, provider_d codec, and
streaming edge-case tests, so the CI coverage floor can move from 73% to 75%.

## Measurement

- Evidence: local clean full coverage run on PR #1759.
- Command:
  `env MASC_DUNE_THROTTLE=0 BISECT_ENABLE=yes EIO_BACKEND=posix OAS_RUNTIME_PATH=... BISECT_FILE=/tmp/oas_provider_intf_coverage_final/bisect scripts/dune-local.sh runtest --force --instrument-with bisect_ppx`
- Summary command:
  `opam exec -- bisect-ppx-report summary --coverage-path=/tmp/oas_provider_intf_coverage_final`
- Measured coverage: `76.02%` (`22598/29728`)
- Timestamp: 2026-05-25 KST
- Confidence: High for local measurement; GitHub full CI remains the final
  merge gate after the draft PR is marked ready.

## Ratchet decision

- Previous threshold: `73`
- New threshold: `75`
- Formula: `floor(76.02 - 1) = 75`
- Reason: preserve one percentage point of CI/runtime headroom while enforcing
  the measured recovery toward the 80% terminal goal.

## #1175 record

- Stage C target (`60 -> 75`): floor now reaches 75.
- Stage D target (`75 -> 80`): still open.
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
