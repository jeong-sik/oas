# Zero-LLM Testing Guide

OAS supports testing agent logic without calling any LLM API.
This is the primary testing strategy — 28+ swarm tests run in <0.3s.

## How it works

`agent_entry.run` is a closure: `sw -> string -> (api_response, sdk_error) result`.
Replace the closure with a function that returns a fixed response.

```ocaml
(* Using Test_helpers from agent_sdk_swarm *)
open Agent_sdk_swarm

let entry = Test_helpers.mock_entry ~name:"worker" "task complete"
let config = Test_helpers.basic_config ~prompt:"do something" [entry]
```

## Mock patterns

### Fixed text response

```ocaml
let entry = Test_helpers.mock_entry ~name:"agent" "result text"
```

### Failing agent

```ocaml
let entry = Test_helpers.failing_entry ~name:"broken" "disk full"
```

### Counting calls

```ocaml
let (entry, get_count) = Test_helpers.counting_entry ~name:"w" "ok"
(* ... run swarm ... *)
assert (get_count () = 3)
```

### Custom behavior

```ocaml
let smart_mock ~sw:_ prompt =
  if String.length prompt > 100 then
    Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
         content = [Text "summarized"]; usage = None }
  else
    Error (Error.Internal "prompt too short")

let entry = { Swarm_types.name = "smart"; run = smart_mock; role = Execute }
```

## What you can test without LLM

| Capability | How |
|-----------|-----|
| Convergence loop | Mock metric callback, verify iteration count |
| Timeout behavior | Set `timeout_sec = Some 0.05`, verify graceful exit |
| N-worker parallel | 12 entries, Eio fiber concurrency |
| Pipeline mode | Ordered entries, verify sequential execution |
| Supervisor mode | Verify supervisor entry runs first |
| Partial failure | Mix mock_entry + failing_entry |
| Aggregate strategies | Best_score, Average, Majority_vote |
| Resource budget | Set budget limits, verify enforcement |

## Why this matters

- Tests run in <0.3s (no network, no API keys)
- Deterministic (no LLM variance)
- CI-friendly (no secrets needed)
- Type system guarantees mock = real (same `agent_entry` type)
