# Checkpoint persistence migration: v5/v6 to v8

Checkpoint v8 remains the only in-memory and write schema. The reader accepts a
closed union of JSON shapes emitted by released v5 and v6 serializers. This is
necessary because the v5 wire shape changed during the v5 release line without
a checkpoint-version bump. Accepted v5 top-level shapes are exactly:

- pre-`preserve_thinking` with both retired token-cap fields;
- `preserve_thinking` with both retired token-cap fields;
- `preserve_thinking` with neither retired token-cap field.

Having only one token-cap field, or having neither `preserve_thinking` nor the
token-cap pair, matches no released shape and is rejected. Released v6 requires
`preserve_thinking` and has no token-cap fields. Every accepted document is
normalized to v8 before the strict v8 decoder runs and is subsequently written
as v8.

The one-way migration performs only these mechanical transformations:

- adds `reasoning_effort: null`;
- adds `preserve_thinking: null` only to the pre-`preserve_thinking` v5 shape;
- validates each retired token cap as `null` or an integer, then removes both.
  Token caps are not execution gates in the current unbounded contract;
- replaces `usage.unpriced_model` with `usage.pricing_gap`. The earliest v5
  usage shape had no `unpriced_model`; it becomes `pricing_gap: null` because
  the checkpoint contains no persisted pricing-gap attribution. A stored `null`
  remains `null`; the released `"<unknown>"` sentinel becomes
  `Model_identity_unavailable`; any other non-empty model string becomes
  `Pricing_unavailable`;
- preserves a v5/v6 failed `tool_result` that has no stored provenance as
  `Unattributed_tool_error` with no `error_class`; it does not claim that the
  error was provider-reported. A v5 document cannot contain provenance fields;
  a v6 document may contain only the provenance variants its released
  serializer knew;
- adds `http_base_url: null` and empty `http_headers` to the earliest persisted
  stdio MCP shape. An HTTP session in that shape is rejected because its
  reconnect URL was not persisted and cannot be reconstructed;
- removes the released MCP `env_policy` after first decoding the typed
  transport. For stdio, only `inherit` is equivalent to current behavior;
  `minimal` and `explicit` are rejected because reconnecting with the full
  parent environment would widen credential exposure. For HTTP, the released
  reconnect path never consulted subprocess environment policy, so its saved
  policy (including the `minimal` value emitted by HTTP capture) is mechanically
  removed without changing HTTP credentials or headers.

The nested release shapes are cross-checked rather than mixed freely. A
pre-`preserve_thinking` checkpoint without `usage.unpriced_model` accepts either
the pre-HTTP MCP record or the later HTTP-aware record. Once
`usage.unpriced_model` is present, the HTTP-aware MCP fields are required. The
`preserve_thinking` capped shape also requires those fields. The unbounded v5
shape and v6 require the later MCP record with `env_policy`. Unknown, partial,
and cross-era combinations are rejected, including a single checkpoint whose
MCP session list mixes record shapes from different release eras.

This is a finite persistence migration, not a compatibility API or a restored
legacy domain. Checkpoint versions 1-4, 7, and unknown future versions remain
rejected. A v5/v6 document is also rejected when its top-level or nested
released schema is missing required fields, contains unknown or duplicate
fields, uses a wrong JSON type, carries an invalid legacy `unpriced_model`
value, mixes release-era shapes, or requires a stdio MCP environment-policy
widening. The same shared nested validators run on direct v8 input, preventing
permissive downstream
decoders from silently normalizing malformed messages, tools, response formats,
contexts, or MCP sessions.

Operators can upgrade in place: load the existing v5 or v6 checkpoint through
`Checkpoint.of_json` or `Checkpoint.of_string`, then persist the returned value
with `Checkpoint.to_json` or `Checkpoint.to_string`. Keep a backup of the source
artifact until the rewritten v8 checkpoint has been validated.
