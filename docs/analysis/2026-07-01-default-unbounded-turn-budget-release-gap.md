# Default Unbounded Turn Budget Release Gap

Date: 2026-07-01

## Context

PR #2417 changed the OAS agent default so omitted `max_turns` resolves to
`0`, the documented no-turn-count-limit sentinel. The change is needed by
downstream MASC keeper runs because MASC delegates its default runtime
`max_turns` to `Agent_sdk.Types.default_config.max_turns`.

## Release Gap

PR #2417 merged with the squash title:

```text
Make default agent turn budget unbounded (#2417)
```

That title is not a conventional commit, so release-please skipped publishing a
new `agent_sdk` release for the already-merged behavior.

The relevant release-please log said:

```text
commit could not be parsed: 75b615ee784e460661ed4622317a891df158abb5
Make default agent turn budget unbounded (#2417)

No user facing commits found since 79260f042b476dda498952c71519195859ea3910
- skipping
```

## Downstream Impact

Until a release containing #2417 exists, downstream MASC cannot safely bump its
`agent_sdk` floor/pin to consume the default-unbounded behavior. The MASC-side
resume compatibility patch can preserve `max_turns = 0`, but it still needs an
OAS release where that is the SDK default.

## Intended Release Trigger

This note accompanies a conventional commit:

```text
fix(agent): release default unbounded turn budget
```

The commit has no runtime code delta beyond this documentation. Its purpose is
to let the normal release-please pipeline publish the already-merged #2417
behavior so downstream consumers can pin it normally.
