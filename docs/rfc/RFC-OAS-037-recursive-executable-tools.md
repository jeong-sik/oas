# RFC-OAS-037: Recursive Executables as Tools

| | |
|---|---|
| Status | Draft |
| Created | 2026-07-16 |
| Target | `agent_sdk` |
| Depends on | RFC-OAS-029, RFC-OAS-034, canonical JSON Schema, Execution Journal stack |
| Replaces | Untyped Tool dispatch and special-case Agent-as-Tool paths |

## 0. Decision

OAS will expose one typed recursive execution algebra:

```text
Executable as Tool
Executable[] as Tool
AsyncExecutable[] as Tool
```

`Any` in product-level descriptions means an existentially packed typed
executable. It does **not** mean `Obj.t`, `Obj.magic`, an unchecked JSON
handler, a string-classified runtime kind, or a provider transcript interpreted
as executable code.

OAS owns the generic finite mechanism:

- typed executable bindings and exact registration;
- provider-visible Tool adaptation;
- recursive synchronous execution;
- typed asynchronous acceptance, reconciliation, cancellation, and operation
  observation contracts;
- invocation, attempt, operation, and causal identity;
- hook ordering and typed failure propagation;
- durable, ordered, gap-explicit execution events and recovery facts.

The embedding application owns the meaning and long-lived policy of an
executable. OAS does not contain application-specific executable variants,
actor lifecycle, wake-up policy, scheduler policy, memory, or compaction.

This RFC is a hard cut. A typed facade over the existing untyped
`Yojson.Safe.t -> Types.tool_result` dispatch is not an acceptable
implementation.

### 0.1 Current implementation baseline

The execution foundation PRs are now merged in release `0.215.0`, but are not
proof that this RFC is wired:

- [#2608](https://github.com/jeong-sik/oas/pull/2608) merged the private
  recursive event and Journal foundation;
- [#2611](https://github.com/jeong-sik/oas/pull/2611) merged the crash-durable
  event-store and lane-writer foundation;
- [#2622](https://github.com/jeong-sik/oas/pull/2622) merged the shared
  canonical-codec executor and execution-runtime foundation;
- [#2631](https://github.com/jeong-sik/oas/pull/2631) exposed the live
  `Tool.Execution_env`/`Tool.Invocation` occurrence surface. That occurrence
  is not silently assumed to be this RFC's durable `Invocation.Id`; the
  implementation must bridge or hard-cut the live surface explicitly;
- [#2637](https://github.com/jeong-sik/oas/pull/2637) bound private Journal
  recursion to the exact live Tool attempt and rejects the retired store v1
  with typed `Unsupported_store_version`. It does not by itself wire
  `Agent.run`, provider continuation, recursive adapters, or the durable
  read model required here;
- [#2640](https://github.com/jeong-sik/oas/pull/2640) pinned rejection of the
  flattened `Tool_invocation -> Tool_invocation` and
  `Tool_attempt -> Tool_attempt` shapes plus the open child-run fence. It is
  negative topology coverage for #2637, not implementation of this RFC's
  executable/continuation/read contracts.

They remain useful only where their contracts conform to this RFC. None by
itself establishes typed recursive Tool adaptation, provider continuation,
durable async publication, or production dashboard projection.
Normal `Agent.run` still writes through `Durable_event`; provider, Tool,
checkpoint, event-bus, and dashboard paths have not completed a single-writer
hard cut to the new Journal. `Tool.Invocation.t`, private Journal node identity,
and this RFC's durable `Invocation.Id` are therefore inputs to one explicit
migration, not interchangeable aliases and not three authorities that may
remain live.

[근거] `git fetch origin main`, `git rev-list --left-right --count
HEAD...origin/main`, and `gh pr view` for #2608, #2611, #2622, #2631, #2637,
#2639, and #2640; checked 2026-07-17 12:21 KST at
`b2a9478ff328ac3334219721da4591d75c96c945` — confidence High.

## 1. Required invariants

### R1. One recursive execution boundary

Leaf tools, finite agents, composites, and asynchronous submission tools all
enter through the same invocation boundary. Adding another callable kind
requires an adapter to `Executable.t`, not another dispatch loop.

### R2. Typed packing, not dynamic guessing

Every heterogeneous package keeps its executable, stable revision, durable
codecs, and result types together. Runtime code never infers a kind from a name
prefix, substring, JSON shape, path, provider text, or output prose.

Exact lookup of an already parsed opaque identifier is allowed. Semantic
string classification is not.

### R3. One identity per entity, explicit edges between entities

Definition, exposure, occurrence, attempt, submission, operation, provider
call, and event identities are different entities:

| Entity | Authoritative identity |
|---|---|
| Executable definition | `Executable.Id.t` plus `Executable.Revision.t` |
| Provider Tool exposure | `Tool.Id.t` plus `Tool.Revision.t` |
| Async runtime configuration | `Async_runtime_config.Id.t` plus `Async_runtime_config.Revision.t` |
| Invocation occurrence | `Invocation.Id.t` |
| Execution attempt | `Attempt.Id.t` |
| Child Agent run | `Agent_run.Id.t` |
| Model exchange within an Agent turn | `Provider_exchange.Id.t` |
| Async submission | `Submission.Id.t` |
| Async operation | `Operation.Id.t` |
| Provider ToolUse | finalized `Provider_source_key.t` under exact adapter/attempt plus grammar-specific native correlation evidence |
| Journal event | `Execution_identity.Event_id.t` |

Each entity has one typed identity. Relationships are typed edges such as
`parent`, `caused_by`, `attempt_of`, and `originated_from`; one string is never
reused as several identities.

`Invocation.Id.t` is the journal identity of the invocation node. There is no
second dashboard-only or hook-only invocation ID.

All identity modules live in one dependency-leaf `Execution_identity` module.
Higher modules alias those types instead of referring to each other merely to
obtain an ID. This prevents an `Invocation -> Operation -> Executable ->
Invocation` compilation cycle.

```ocaml
module Execution_identity : sig
  module type Stable = sig
    type t

    val of_string : string -> (t, Parse_error.t) result
    val to_string : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  module type Occurrence = Stable

  module Executable_id : Stable
  module Executable_revision : Stable
  module Tool_id : Stable
  module Tool_revision : Stable
  module Operation_backend_id : Stable
  module Async_runtime_id : Stable
  module Async_runtime_revision : Stable
  module Context_factory_id : Stable
  module Context_factory_revision : Stable
  module Operation_scope_factory_id : Stable
  module Operation_scope_factory_revision : Stable
  module Effect_protocol_id : Stable
  module Effect_protocol_revision : Stable
  module Commit_action_id : Stable
  module Commit_action_revision : Stable
  module Provider_adapter_id : Stable
  module Provider_adapter_revision : Stable
  module Provider_binding_id : Stable
  module Provider_binding_revision : Stable
  module Provider_spool_id : Stable
  module Manifest_store_id : Stable
  module Edge_index_id : Stable

  module Invocation_id : Occurrence
  module Attempt_id : Occurrence
  module Agent_run_id : Occurrence
  module Agent_turn_id : Occurrence
  module Provider_exchange_id : Occurrence
  module Provider_attempt_id : Occurrence
  module Writer_owner_id : Occurrence
  module Claim_id : Occurrence
  module Claim_batch_id : Occurrence
  module Effect_id : Occurrence
  module Submission_id : Occurrence
  module Operation_id : Occurrence
  module Operation_execution_scope_id : Occurrence
  module Cancellation_request_id : Occurrence
  module Event_id : Occurrence
  module Event_stream_id : Occurrence
  module Correlation_id : Occurrence
  module Observation_clock_id : Occurrence
  module Manifest_id : Occurrence
  module Manifest_transaction_id : Occurrence
end

module Identity_source : sig
  type t

  val system : unit -> (t, Identity_source_error.t) result

  module Internal : sig
    type kind =
      | Invocation
      | Attempt
      | Agent_run
      | Agent_turn
      | Provider_exchange
      | Provider_attempt
      | Writer_owner
      | Claim
      | Claim_batch
      | Effect
      | Submission
      | Operation
      | Operation_execution_scope
      | Cancellation_request
      | Event
      | Event_stream
      | Correlation
      | Observation_clock
      | Manifest
      | Manifest_transaction

    val create
      :  next:(kind -> (string, Identity_source_error.t) result)
      -> t

    val invocation : t -> (Execution_identity.Invocation_id.t, Identity_error.t) result
    val attempt : t -> (Execution_identity.Attempt_id.t, Identity_error.t) result
    val agent_run : t -> (Execution_identity.Agent_run_id.t, Identity_error.t) result
    val agent_turn : t -> (Execution_identity.Agent_turn_id.t, Identity_error.t) result
    val provider_exchange
      :  t
      -> (Execution_identity.Provider_exchange_id.t, Identity_error.t) result
    val provider_attempt : t -> (Execution_identity.Provider_attempt_id.t, Identity_error.t) result
    val writer_owner : t -> (Execution_identity.Writer_owner_id.t, Identity_error.t) result
    val claim : t -> (Execution_identity.Claim_id.t, Identity_error.t) result
    val claim_batch : t -> (Execution_identity.Claim_batch_id.t, Identity_error.t) result
    val effect_id : t -> (Execution_identity.Effect_id.t, Identity_error.t) result
    val submission : t -> (Execution_identity.Submission_id.t, Identity_error.t) result
    val operation : t -> (Execution_identity.Operation_id.t, Identity_error.t) result
    val operation_execution_scope
      :  t
      -> (Execution_identity.Operation_execution_scope_id.t, Identity_error.t) result
    val cancellation_request
      :  t
      -> (Execution_identity.Cancellation_request_id.t, Identity_error.t) result
    val event : t -> (Execution_identity.Event_id.t, Identity_error.t) result
    val event_stream : t -> (Execution_identity.Event_stream_id.t, Identity_error.t) result
    val correlation : t -> (Execution_identity.Correlation_id.t, Identity_error.t) result
    val observation_clock
      :  t
      -> (Execution_identity.Observation_clock_id.t, Identity_error.t) result
    val manifest
      :  t
      -> (Execution_identity.Manifest_id.t, Identity_error.t) result
    val manifest_transaction
      :  t
      -> (Execution_identity.Manifest_transaction_id.t,
          Identity_error.t)
         result
  end
end

module Utc_timestamp : sig
  type t

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Observation_time : sig
  type t

  val clock : t -> Execution_identity.Observation_clock_id.t
  val monotonic_ns : t -> int64
  val wall : t -> Utc_timestamp.t option
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val compare_same_clock
      :  t
      -> t
      -> (int, Observation_clock_mismatch.t) result
  end
end

module Observation_duration : sig
  type t

  val between
    :  start:Observation_time.t
    -> finish:Observation_time.t
    -> (t, Observation_duration_error.t) result

  val nanoseconds : t -> int64
end

module Observation_source : sig
  type t

  val now : t -> (Observation_time.t, Observation_error.t) result

  module Internal : sig
    val create
      :  identity_source:Identity_source.t
      -> mono_clock:_ Eio.Time.Mono.t
      -> wall_clock:_ Eio.Time.clock
      -> (t, Construction_error.t) result
  end
end

module Executable_reference : sig
  module Id = Execution_identity.Executable_id
  module Revision = Execution_identity.Executable_revision

  type t =
    { id : Id.t
    ; revision : Revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Tool_exposure_reference : sig
  type t =
    { tool_id : Execution_identity.Tool_id.t
    ; tool_revision : Execution_identity.Tool_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end
```

Stable definition, revision, and occurrence modules cannot mint themselves.
One runtime-owned `Identity_source.t` is the only effect boundary that mints
occurrences. `system` uses OS entropy without mutable global state; deterministic
tests inject a closed-kind source through the Dune-private constructor. The
authority that opens an occurrence asks that source for the exact typed ID.
Writer-owner IDs are minted by the runtime/scope that owns them and are never
caller-supplied authority.

The runtime also constructs one `Observation_source.t` from that identity
source, an explicitly injected `Eio.Time.Mono.t`, and a separate wall clock.
The source mints one clock identity and
the sole lane writer samples it exactly once when linearizing each semantic
Journal event. Every fact ordinal in that event projects the same immutable
sample from the event envelope; the writer never samples once per fact, and a
later physical group flush never overwrites it. `Observation_time.t` is
display/measurement evidence, not semantic ordering authority. Monotonic values
may be compared or subtracted only when their clock identities are equal; an
optional UTC wall value is presentation evidence and never orders facts,
releases a barrier, admits work, or chooses a retry. A process restart creates a
new observation-clock identity, so a span crossing that boundary is explicitly
unavailable instead of being estimated from wall time. A generic
`Eio.Time.clock` can never populate `monotonic_ns`; it is used only to derive the
optional UTC display value.

`Executable_reference` and `Tool_exposure_reference` are dependency leaves
compiled before provider calls, invocation references, contexts, and the
executable algebra. `Executable.reference` and
`Executable.exposure_reference` below are aliases to these leaves. This cuts
the otherwise real
`Executable -> Execution_context -> Invocation_reference ->
Provider_tool_call_reference -> Executable` compilation cycle without a
recursive module.

`Execution_identity` is the hard-cut successor of the merged dependency-leaf
`Execution_id`; it is not a parallel identity implementation. Merged
`Execution_event.Event_id`, `Run_id`, `Node_id`, and `Correlation_id` are
rehomed or aliased through this one leaf during migration. In particular,
`Execution_event.Correlation_id` aliases
`Execution_identity.Correlation_id`; no second parser, generator, or codec is
retained. The generic merged `Node_id` is removed as a semantic identity once
the distinct invocation, attempt, turn, provider-attempt, and effect occurrence
IDs above cover every closed node kind. Event projection may expose a tagged
node reference, but cannot mint another node identity namespace.

```ocaml
module Attempt : sig
  module Id = Execution_identity.Attempt_id
end

module Agent_run : sig
  module Id = Execution_identity.Agent_run_id
end

module Agent_turn : sig
  module Id = Execution_identity.Agent_turn_id
end

module Provider_attempt : sig
  module Id = Execution_identity.Provider_attempt_id
end

module Provider_exchange : sig
  module Id = Execution_identity.Provider_exchange_id
end

module Operation_scope_factory_reference : sig
  type t =
    { id : Execution_identity.Operation_scope_factory_id.t
    ; revision : Execution_identity.Operation_scope_factory_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Execution_cursor : sig
  type t

  val make
    :  stream:Execution_identity.Event_stream_id.t
    -> sequence:int64
    -> (t, Construction_error.t) result

  val stream : t -> Execution_identity.Event_stream_id.t
  val sequence : t -> int64
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Execution_stream_reference : sig
  type operation_execution =
    { backend : Execution_identity.Operation_backend_id.t
    ; operation : Execution_identity.Operation_id.t
    ; invocation : Execution_identity.Invocation_id.t
    ; scope : Execution_identity.Operation_execution_scope_id.t
    ; scope_factory : Operation_scope_factory_reference.t
    }

  type kind =
    | Root_execution
    | Operation_backend of
        { backend : Execution_identity.Operation_backend_id.t
        }
    | Operation_execution of operation_execution

  type t

  val kind : t -> kind
  val stream : t -> Execution_identity.Event_stream_id.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val root
      :  stream:Execution_identity.Event_stream_id.t
      -> t

    val operation_backend
      :  backend:Execution_identity.Operation_backend_id.t
      -> stream:Execution_identity.Event_stream_id.t
      -> t

    val operation_execution
      :  operation_execution
      -> stream:Execution_identity.Event_stream_id.t
      -> t
  end
end

module Execution_fact_ref : sig
  type t

  val cursor : t -> Execution_cursor.t
  val stream_reference : t -> Execution_stream_reference.t
  val stream : t -> Execution_identity.Event_stream_id.t
  val event : t -> Execution_identity.Event_id.t
  val ordinal : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  stream:Execution_stream_reference.t
      -> cursor:Execution_cursor.t
      -> event:Execution_identity.Event_id.t
      -> ordinal:int
      -> (t, Construction_error.t) result
  end
end

module Execution_page_cursor : sig
  type position =
    | Beginning of Execution_stream_reference.t
    | After of Execution_fact_ref.t

  type t

  val position : t -> position
  val stream : t -> Execution_stream_reference.t
  val compare_same_stream
    :  t
    -> t
    -> (int, Cursor_stream_mismatch.t) result
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result

  val beginning : Execution_stream_reference.t -> t
  val after : Execution_fact_ref.t -> t
end

module Provider_source_key : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val compare : t -> t -> int
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_fact : Execution_fact_ref.t -> t
  end
end

module Provider_adapter_reference : sig
  type t =
    { id : Execution_identity.Provider_adapter_id.t
    ; revision : Execution_identity.Provider_adapter_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_binding_reference : sig
  type t =
    { id : Execution_identity.Provider_binding_id.t
    ; revision : Execution_identity.Provider_binding_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_wire_contract_reference : sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Byte_count : sig
  type t

  val zero : t
  val of_int64 : int64 -> (t, Construction_error.t) result
  val to_int64 : t -> int64
  val add : t -> t -> (t, Integer_overflow.t) result
end

module Positive_byte_count : sig
  type t

  val of_int64 : int64 -> (t, Construction_error.t) result
  val to_int64 : t -> int64
  val as_count : t -> Byte_count.t
end

module Immutable_byte_slice : sig
  type t

  val of_bytes_slice_copy
    :  capacity:Positive_byte_count.t
    -> source:bytes
    -> offset:int
    -> length:Positive_byte_count.t
    -> (t, Construction_error.t) result

  val of_string_slice_copy
    :  capacity:Positive_byte_count.t
    -> source:string
    -> offset:int
    -> length:Positive_byte_count.t
    -> (t, Construction_error.t) result

  val byte_count : t -> Positive_byte_count.t
  val to_string : t -> string
end

module Provider_native_scalar_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Provider_native_scalar : sig
  type staged
  type committed
  type 'lifecycle t
  type cursor

  val digest : 'lifecycle t -> Provider_native_scalar_digest.t
  val byte_count : 'lifecycle t -> Byte_count.t
  val beginning : 'lifecycle t -> cursor

  val encode_committed : committed t -> Canonical_json.t
  val decode_committed
    :  Canonical_json.t
    -> (committed t, Decode_error.t) result
end

module Provider_tool_call_id : sig
  type 'lifecycle t

  val scalar
    :  'lifecycle t
    -> 'lifecycle Provider_native_scalar.t

  module Internal : sig
    val of_scalar
      :  'lifecycle Provider_native_scalar.t
      -> 'lifecycle t
  end
end

module Provider_tool_name : sig
  type t

  val of_declared_string : string -> (t, Parse_error.t) result
  val to_declared_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_choice_id : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_tool_index : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_output_index : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_content_index : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_summary_index : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_tool_ordinal : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_item_ordinal : sig
  type t

  val of_int : int -> (t, Parse_error.t) result
  val to_int : t -> int
end

module Provider_response_id : sig
  type 'lifecycle t

  val scalar
    :  'lifecycle t
    -> 'lifecycle Provider_native_scalar.t

  module Internal : sig
    val of_scalar
      :  'lifecycle Provider_native_scalar.t
      -> 'lifecycle t
  end
end

module Provider_item_id : sig
  type 'lifecycle t

  val scalar
    :  'lifecycle t
    -> 'lifecycle Provider_native_scalar.t

  module Internal : sig
    val of_scalar
      :  'lifecycle Provider_native_scalar.t
      -> 'lifecycle t
  end
end

module Provider_native_tool_correlation : sig
  type 'lifecycle t =
    | Call_id of 'lifecycle Provider_tool_call_id.t
    | Chat_completions of
        { choice : Provider_choice_id.t
        ; tool_index : Provider_tool_index.t
        ; call_id : 'lifecycle Provider_tool_call_id.t
        }
    | Responses of
        { response_id : 'lifecycle Provider_response_id.t
        ; item_id : 'lifecycle Provider_item_id.t
        ; output_index : Provider_output_index.t
        ; call_id : 'lifecycle Provider_tool_call_id.t
        }
    | Name_ordered of
        { tool_ordinal : Provider_tool_ordinal.t
        }

  val encode_committed
    :  Provider_native_scalar.committed t
    -> Canonical_json.t
  val decode_committed
    :  Canonical_json.t
    -> (Provider_native_scalar.committed t, Decode_error.t) result
end

module Provider_stream_lane_correlation : sig
  type t =
    | Chat_choice of
        { choice : Provider_choice_id.t
        }
    | Chat_tool of
        { choice : Provider_choice_id.t
        ; tool_index : Provider_tool_index.t
        ; call_id :
            Provider_native_scalar.staged Provider_tool_call_id.t
        }
    | Responses_item_scalar of
        { response_id :
            Provider_native_scalar.staged Provider_response_id.t
        ; item_id :
            Provider_native_scalar.staged Provider_item_id.t
        ; output_index : Provider_output_index.t
        }
    | Responses_content of
        { response_id :
            Provider_native_scalar.staged Provider_response_id.t
        ; item_id :
            Provider_native_scalar.staged Provider_item_id.t
        ; output_index : Provider_output_index.t
        ; content_index : Provider_content_index.t
        }
    | Responses_summary of
        { response_id :
            Provider_native_scalar.staged Provider_response_id.t
        ; item_id :
            Provider_native_scalar.staged Provider_item_id.t
        ; output_index : Provider_output_index.t
        ; summary_index : Provider_summary_index.t
        }
    | Name_ordered_item of
        { item_ordinal : Provider_item_ordinal.t
        }
end

module Provider_tool_call_reference : sig
  type t

  val source_adapter : t -> Provider_adapter_reference.t
  val attempt : t -> Provider_attempt.Id.t
  val tool_use_source : t -> Provider_source_key.t
  val exposure : t -> Tool_exposure_reference.t
  val provider_tool_name : t -> Provider_tool_name.t
  val native_correlation
    :  t
    -> Provider_native_scalar.committed
         Provider_native_tool_correlation.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  source_adapter:Provider_adapter_reference.t
      -> attempt:Provider_attempt.Id.t
      -> tool_use_source:Provider_source_key.t
      -> exposure:Tool_exposure_reference.t
      -> provider_tool_name:Provider_tool_name.t
      -> native_correlation:
           Provider_native_scalar.committed
             Provider_native_tool_correlation.t
      -> t
  end
end

module Writer_owner : sig
  module Id = Execution_identity.Writer_owner_id
end

module Cancellation : sig
  module Request_id = Execution_identity.Cancellation_request_id

  type requester =
    | Caller
    | Parent_invocation of Execution_identity.Invocation_id.t
    | Execution_scope of Execution_identity.Event_stream_id.t
    | Async_operation of Execution_identity.Operation_id.t

  type cause

  val request : cause -> Request_id.t
  val requester : cause -> requester

  val encode_cause : cause -> Canonical_json.t
  val decode_cause
    :  Canonical_json.t
    -> (cause, Decode_error.t) result

  module Internal : sig
    val create
      :  identity_source:Identity_source.t
      -> requester:requester
      -> (cause, Identity_error.t) result
  end
end

module Predecessor_barrier : sig
  type terminal_requirement =
    | Any_terminal
    | Successful_terminal

  type t =
    | Awaiting of terminal_requirement
    | Predecessor_outcome_unknown of
        Execution_identity.Operation_id.t
    | Predecessor_recovery_failed of
        Execution_identity.Operation_id.t

  val equal : t -> t -> bool
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end
```

A cursor is an opaque `(stream, sequence)` capability. Callers cannot combine a
sequence from one stream with another stream identity.

`Provider_source_key.t` and dashboard `fact_id` likewise retain the exact
typed stream reference beside cursor, event, and ordinal. Even though
occurrence IDs are globally unique, uniqueness alone is not a read-routing
capability. `Execution_fact_ref.Internal.create` rejects a cursor whose stream
does not equal its `Execution_stream_reference`; an operation-execution
reference carries the backend, operation, invocation, scope, and exact factory
revision required to reopen that Journal. No caller must scan streams or infer
an anchor to locate the event that authorizes a provider item.

`Execution_page_cursor.t` is the lossless scan position and scan high-water.
`After fact` includes the event cursor and fact ordinal, so a bounded page may
stop inside one atomic event and resume at the next fact without skipping its
siblings. `Execution_cursor.t` remains only the physical transaction/event
position; it is neither a fact-page continuation token nor a scan high-water.

`Execution_cursor` is the hard-cut cursor authority over the merged physical
store contract. Merged `Execution_event_store.Scope_id` is rehomed as
`Execution_identity.Event_stream_id`, and both
`Execution_event_store.cursor` and `Execution_journal.cursor` become aliases of
`Execution_cursor.t`. The physical store retains authority to allocate its
stream identity and advance sequences; `Execution_cursor.make` is used only by
that representation-owning store and by canonical decode validation. The old
`make_cursor`, scope-ID parser/codec, and Journal cursor codec are deleted
rather than retained as parallel constructors.

Each role-specific provider ID is an opaque lifecycle wrapper over a
byte-paged `Provider_native_scalar.t`, not an OAS occurrence ID and never a
whole heap string. `Provider_native_tool_correlation.t` keeps each grammar's
distinct identity fields: Chat choice/tool indices and call ID, Responses
response/item/output indices and call ID, or an explicitly name/order-based
grammar. In particular, a Responses item `id` is never collapsed into its
different `call_id`. The parser incrementally stages every native scalar under
the provider-attempt spool lease. The success transaction publishes those
scalar roots with the finalized item and alone converts the correlation to
the committed lifecycle. A provider adapter writes the committed pages back
only when building that exact adapter's continuation. OAS never mints,
normalizes, flattens, classifies, or substitutes one field for another.
Digest equality is only an index candidate: exact equality and snapshot
resolution compare all bytes through bounded pages, including on digest
collision. Missing, duplicate, reordered, cross-role, cross-attempt, or
cross-linked required fields fail whole-attempt finalization; source/order
grammars use their explicit variant rather than `None`.

`Provider_tool_name.t` independently preserves the exact provider-visible Tool
name carried by that ToolUse. Whole-attempt finalization resolves it through
the exact immutable exposure snapshot committed with the provider request, and
the resulting call reference retains that `Executable.exposure_reference`.
It is not reconstructed later from a call ID or a current catalog. A
chat-completions adapter uses its `Chat_completions` correlation; a Responses
adapter uses the distinct `Responses.call_id` result carrier while preserving
the item ID for stream validation; a native Ollama adapter may serialize
`provider_tool_name`. OAS never synthesizes a missing identity field and then
replays it as provider-native identity.

Every cancellation request receives one durable occurrence identity. Its
closed requester says which authority initiated cancellation; arbitrary prose
is observation metadata and cannot change cancellation semantics. A parent,
scope, or operation cancellation therefore cannot be reconstructed from an
exception message. `Predecessor_barrier.t` likewise states the generic
dependency condition and the two non-terminal recovery blocks.
`Successful_terminal` is satisfied only by an exact referenced successful
execution terminal; cancellation, declared failure, outcome uncertainty, or
recovery failure cannot release it. Unknown outcome and recovery failure retain
the exact predecessor identity and cannot be confused with ordinary waiting.

### R4. No implicit behavioral budget

Cost, token usage, turn count, elapsed time, recursion depth, and observed
queue depth are measurements. They do not admit, reject, pause, compact, or
stop work.

OAS supplies no default timeout, recursion cap, turn cap, cost cap, semantic
payload policy, or queue-depth threshold.

Finite runtime resources are distinct from behavioral budgets. A runtime owner
must supply explicit executor, item, and byte capacities for the bounded
resources it enables. Temporary
saturation applies cooperative backpressure through the typed admission
capability; it does not reject, pause, or semantically stop the work. A closed
admission, cancelled wait, non-waiting CPU submission, or storage refusal is an
explicit typed capacity/infrastructure result. OAS never invents a numeric
default, drops work, runs it inline, or converts resource saturation into a
behavioral stop.

### R5. Domain failure is local; infrastructure failure is outer

An executable returning its declared failure is an ordinary outcome and does
not cancel siblings. Cancellation, reserved runtime exceptions, recorder
failure, and protocol corruption are infrastructure failures and retain
fail-closed structured-concurrency behavior.

### R6. No silent fallback

Failure to parse, submit, persist, reconcile, decode, execute, observe, encode,
or publish is typed. OAS never:

- runs inline after executor submission failure;
- replaces a durable path with volatile execution;
- drops a child result or accepted receipt;
- fabricates a successful aggregate;
- silently advances across an event cursor gap;
- automatically retries an externally visible effect whose outcome is unknown.

### R7. One authoritative execution writer

For an OAS-native execution scope, the Execution Journal is the sole execution
topology and recovery writer. Event buses, traces, checkpoints, dashboards,
and metrics are projections. There is no dual-write migration interval.

An operation backend remains authoritative for publication, readiness, claims,
cancellation-before-start, and the terminal-link state in its namespace. The
anchor-bound operation Execution Journal remains authoritative for invocation,
attempt, effect, recovery, and executable-terminal facts. The backend stores
only exact references to those execution facts; neither side duplicates the
other's mutable semantic state.

### R8. Provider transcript is a projection, not execution storage

Provider ToolUse and ToolResult blocks are exact protocol projections of an
execution fact. Internal recursive calls do not fabricate provider IDs or
provider content blocks in order to become durable.

### R9. Compaction and memory are outside OAS

OAS preserves exact provider and execution facts needed by callers. It does not
decide when or how to compact a conversation and does not own memory storage,
recall, consolidation, or forgetting.

## 2. Canonical value and schema prerequisites

```ocaml
module Execution_value_authority : sig
  type t
end
```

### 2.0 Runtime-owned immutable manifest staging

Large ordered structures share one finite mechanism rather than open-coded
lists or module-specific temp files:

```ocaml
module Execution_manifest_purpose : sig
  type agent_prelude
  type agent_definition
  type agent_checkpoint
  type execution_value
  type conversation_selection
  type provider_tool_exposure
  type tool_batch_members
  type executable_calls
  type executable_results
  type submission_operations
  type submission_receipts
  type provider_native_scalars
  type provider_observations
  type commit_action_repair
  type shutdown_report

  type _ t =
    | Agent_prelude : agent_prelude t
    | Agent_definition : agent_definition t
    | Agent_checkpoint : agent_checkpoint t
    | Execution_value : execution_value t
    | Conversation_selection : conversation_selection t
    | Provider_tool_exposure : provider_tool_exposure t
    | Tool_batch_members : tool_batch_members t
    | Executable_calls : executable_calls t
    | Executable_results : executable_results t
    | Submission_operations : submission_operations t
    | Submission_receipts : submission_receipts t
    | Provider_native_scalars : provider_native_scalars t
    | Provider_observations : provider_observations t
    | Commit_action_repair : commit_action_repair t
    | Shutdown_report : shutdown_report t

  type packed = Pack : 'purpose t -> packed
end

module Execution_manifest_owner : sig
  type t =
    | Runtime_definition of Execution_identity.Manifest_id.t
    | Execution_scope of Execution_stream_reference.t
    | Operation_scope of Execution_stream_reference.t
end

module Execution_manifest_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Execution_manifest_record : sig
  type 'purpose t
  type packed = Pack : 'purpose t -> packed

  val purpose
    :  'purpose t
    -> 'purpose Execution_manifest_purpose.t
  val byte_count : 'purpose t -> Positive_byte_count.t

  module Internal : sig
    type 'purpose encoder

    val begin_
      :  purpose:'purpose Execution_manifest_purpose.t
      -> capacity:Positive_byte_count.t
      -> 'purpose encoder

    val append
      :  'purpose encoder
      -> source:string
      -> offset:int
      -> length:Positive_byte_count.t
      -> (unit, Manifest_record_error.t) result

    val seal
      :  'purpose encoder
      -> ('purpose t, Manifest_record_error.t) result
  end
end

module Execution_manifest : sig
  type staged
  type committed
  type ('purpose, 'lifecycle) t
  type 'lifecycle packed =
    | Pack : ('purpose, 'lifecycle) t -> 'lifecycle packed

  val id
    :  ('purpose, 'lifecycle) t
    -> Execution_identity.Manifest_id.t
  val purpose
    :  ('purpose, 'lifecycle) t
    -> 'purpose Execution_manifest_purpose.t
  val count : ('purpose, 'lifecycle) t -> int64
  val encoded_bytes : ('purpose, 'lifecycle) t -> int64
  val digest
    :  ('purpose, 'lifecycle) t
    -> Execution_manifest_digest.t
end

module Execution_manifest_store_bootstrap : sig
  type t

  val create
    :  id:Execution_identity.Manifest_store_id.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Execution_manifest_store_recovery : sig
  type t

  val recovered_generation : t -> int64 option
  val discarded_manifests : t -> int64
  val discarded_bytes : t -> int64
end

module Execution_manifest_store : sig
  type t
  type cursor

  type page =
    { records : Execution_manifest_record.packed list
    ; next : cursor
    ; caught_up : bool
    }

  module Internal : sig
    val open_
      :  sw:Eio.Switch.t
      -> bootstrap:Execution_manifest_store_bootstrap.t
      -> global_byte_capacity:Positive_byte_count.t
      -> per_transaction_byte_capacity:Positive_byte_count.t
      -> record_byte_capacity:Positive_byte_count.t
      -> page_byte_capacity:Positive_byte_count.t
      -> (t * Execution_manifest_store_recovery.t,
          Manifest_store_open_error.t)
         result

    val close_and_await
      :  t
      -> (unit, Manifest_store_close_error.t) result
  end
end

module Execution_manifest_staging : sig
  type open_
  type sealed
  type 'state transaction
  type 'purpose member_builder

  type 'purpose page =
    { records : 'purpose Execution_manifest_record.t list
    ; next : Execution_manifest_store.cursor
    ; caught_up : bool
    }

  val begin_
    :  sw:Eio.Switch.t
    -> store:Execution_manifest_store.t
    -> id:Execution_identity.Manifest_transaction_id.t
    -> owner:Execution_manifest_owner.t
    -> byte_capacity:Positive_byte_count.t
    -> (open_ transaction, Manifest_store_error.t) result

  val begin_manifest
    :  open_ transaction
    -> id:Execution_identity.Manifest_id.t
    -> purpose:'purpose Execution_manifest_purpose.t
    -> byte_capacity:Positive_byte_count.t
    -> ('purpose member_builder, Manifest_store_error.t) result

  val append
    :  'purpose member_builder
    -> 'purpose Execution_manifest_record.t
    -> (unit, Manifest_store_error.t) result

  val seal_manifest
    :  'purpose member_builder
    -> (('purpose, Execution_manifest.staged) Execution_manifest.t,
        Manifest_store_error.t)
       result

  val seal
    :  open_ transaction
    -> (sealed transaction, Manifest_store_error.t) result

  val abort
    :  _ transaction
    -> (unit, Manifest_store_error.t) result

  val read
    :  Execution_manifest_store.t
    -> sealed transaction
    -> ('purpose, Execution_manifest.staged) Execution_manifest.t
    -> after:Execution_manifest_store.cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> ('purpose page, Read_error.t) result
end

module Execution_manifest_semantic_fact_builder : sig
  type 'fact t
  type ('fact, 'purpose) field

  (* There is no generic or public constructor. Each closed fact-specific
     module mints its own phantom [fact], required fields, and sealed builder. *)
end

module Execution_manifest_root_commit : sig
  type pending_roots
  type committed_set
  type pending_reference
  type attachment_builder
  type complete_transaction

  val begin_attachments : pending_roots -> attachment_builder

  val attach
    :  attachment_builder
    -> field:
         ('fact, 'purpose) Execution_manifest_semantic_fact_builder.field
    -> ('purpose, Execution_manifest.staged) Execution_manifest.t
    -> (unit, Manifest_root_attachment_error.t) result

  val seal_attachments
    :  attachment_builder
    -> fact:'fact Execution_manifest_semantic_fact_builder.t
    -> (complete_transaction, Manifest_root_attachment_error.t) result

  type error =
    | Rejected of Manifest_root_rejection.t
    | Uncertain of pending_reference

  val commit
    :  writer:Execution_journal.Writer.t
    -> store:Execution_manifest_store.t
    -> Execution_manifest_staging.sealed
         Execution_manifest_staging.transaction
    -> build:
         (pending_roots
          -> (complete_transaction, Manifest_root_build_error.t) result)
    -> (committed_set, error) result

  val committed_manifest
    :  committed_set
    -> ('purpose, Execution_manifest.staged) Execution_manifest.t
    -> (('purpose, Execution_manifest.committed) Execution_manifest.t,
        Manifest_root_lookup_error.t)
       result

  type reconciliation =
    | Committed of committed_set
    | Proven_aborted

  val reconcile
    :  writer:Execution_journal.Writer.t
    -> pending_reference
    -> (reconciliation, Manifest_reconciliation_error.t) result
end

module Execution_manifest_reader : sig
  type cursor

  type 'purpose page =
    { records : 'purpose Execution_manifest_record.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning
    :  ('purpose, Execution_manifest.committed) Execution_manifest.t
    -> cursor

  val read
    :  Execution_journal.Reader.t
    -> ('purpose, Execution_manifest.committed) Execution_manifest.t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> ('purpose page, Read_error.t) result
end
```

The store is staging only. It is exclusively opened from an explicit
base-path-derived bootstrap and owns one exact generation.
`Execution_manifest_staging.begin_` fairly acquires one aggregate lease for
the complete semantic transaction before any child value or parent manifest
exists. `begin_manifest` only partitions that already-owned lease; it never
waits for or acquires more global capacity. It registers each member under its
GADT purpose witness before append; a builder for one purpose cannot accept a
record of another purpose. Append copies one size-checked immutable record
immediately, charges exact encoded bytes, and never retains the caller's list
or JSON tree. A purpose-specific private codec
streams fixed/ref metadata through `Execution_manifest_record.Internal`;
there is no raw `Canonical_json.t` record constructor or generic application
JSON bag.
Record count and bytes are
tracked incrementally with checked integer arithmetic. `seal_manifest`
publishes only a small transaction-bound root
`(transaction, id, purpose, count, bytes, digest)` inside the staging store.
A member append/seal failure poisons the aggregate; thereafter only whole
transaction abort is legal.

There is deliberately no standalone manifest/value/input commit API.
`Execution_manifest_root_commit` transfers all sealed members into the exact
Journal pending namespace `(stream, manifest_transaction_id)` and invokes one
closed typed semantic-fact builder. Every fact constructor exposes only
abstract `('fact, 'purpose)
Execution_manifest_semantic_fact_builder.field` slots with its allowed purpose
witness. Each closed fact-specific module owns one private phantom `fact`,
constructs its `Typed_transaction`, exposes exact named field accessors, and
seals to `fact Execution_manifest_semantic_fact_builder.t`. That opaque value
contains the transaction, complete required-slot set, and a fresh builder
instance token. There is no public/generic fact-builder constructor,
purpose-to-field conversion, transaction accessor, or function that can obtain
a field from `pending_roots` alone.
The Dune-private
`Execution_manifest_semantic_fact_catalog` is that exhaustive producer. Its
closed family GADT and field GADT name every allowed root slot for all fifteen
manifest purposes. The multi-owner `Execution_value` purpose is additionally
closed by `execution_value_owner_kind`; it has no text, native-kind, or
catch-all constructor. `Internal.seal` inspects the typed transaction's exact
semantic fact kind and required-slot set against the selected family before it
mints a fresh builder-instance token. `field` tags every named field with that
same token. The provider-terminal family additionally carries the sealed,
paged `Provider_semantic_item_slot_source`: each non-Tool item contributes one
checked `(attempt, item ordinal)` slot bound to that item's exact
`Execution_value.prepared`, while ToolUse contributes none.
`Provider_terminal_semantic slot` is repeatable only for members of that exact
source. It returns a token tagged by both builder instance and slot; duplicate,
missing, cross-attempt, cross-item, or source-external slots fail typed.
`seal_attachments` compares the consumed slot count/digest with the sealed
source. The slot builder writes through the existing attempt spool lease and
the source is count/byte paged, so arbitrary cardinality never becomes an
in-memory field list or another storage authority.
Supplying a family whose transaction fact kind or slot set differs is
`Manifest_root_build_error`; it is never relabeled into the requested family.
`attach` cannot compile for a root of another purpose; it consumes each
transaction-bound slot at most once. `seal_attachments` checks that all and
only the sealed fact's required slots and pending roots are consumed exactly
once, rejects another manifest transaction and another instance of the same
phantom fact type, and returns one opaque `complete_transaction` containing
the typed Journal transaction plus its complete proof. The commit callback can
return only that opaque value; there is no raw-transaction callback or
`Typed_transaction.t * proof` overload. Before publication the reducer
revalidates its transaction token and attachment proof. The fact roots and all
committed manifest roots become visible under one Journal CAS. A committed
handle is returned only from the durable receipt or exact reconciliation.
Therefore a manifest cannot commit without its semantic owner, and a staged
reference cannot survive restart or appear in a read model.

Cancellation, append/seal failure, explicit abort, or switch exit releases the
whole aggregate and its exact charged bytes. A definite commit transfers byte
lineage to committed Journal pages and releases staging/pending ownership; a
definite abort releases both; an uncertain result forbids a fresh commit and
can proceed only through its `pending_reference`. Startup enumerates the exact
pending index and distinguishes committed, proven-aborted, corrupt, and still
uncertain entries before readiness. It never guesses from age, filename,
newest entry, or a substring. Prior-generation staging roots are digest-checked
and removed only after that reconciliation. Missing/corrupt pages or cleanup
failure is typed. Runtime close rejects new transactions, wakes blocked
acquisition, drains/aborts existing transactions, and reports every cleanup
failure.

The global, per-transaction, per-record, and page byte capacities are explicit
runtime-owner resource decisions. They are not turn/tool/count budgets.
`begin_` fairly and atomically reserves one whole aggregate lease from the
global admission before returning; two semantic transactions can never retain
partial global prefixes while each waits for child or parent capacity. A
transaction that cannot fit a newly declared member in its aggregate fails
immediately and aborts as a whole; it does not wait while retaining prior
children. Manifest declarations themselves are charged. FIFO waiters
are grouped by the exact runtime-definition/root/operation owner. Ready owners
advance round-robin and FIFO within an owner; a hot Keeper/reacquirer cannot
bypass an older satisfiable owner. This uses no age, weight, payload-size, or
priority heuristic. Cancellation
or close removes the exact waiter.
Staging capacity
refusal is a typed infrastructure failure; OAS never truncates a manifest or
declares the work semantically complete. Committed pages contain only
fixed/ref-form records that individually passed `record_byte_capacity`; large
payload bytes live in separately byte-paged content sources. Therefore a page
can always advance with at least one record when its byte request is at least
the runtime record capacity. A page never returns empty with
`caught_up = false`.

### 2.1 Canonical execution value

Durable executable codecs target one canonical typed value algebra:

```ocaml
module Execution_value : sig
  module Inline_bytes : module type of Immutable_byte_slice

  module Canonical_json_event : sig
    type t =
      | Null
      | Bool of bool
      | Begin_number
      | Number_chunk of Inline_bytes.t
      | End_number
      | Begin_string
      | String_chunk of Inline_bytes.t
      | End_string
      | Begin_array
      | End_array
      | Begin_object
      | Begin_object_name
      | Object_name_chunk of Inline_bytes.t
      | End_object_name
      | End_object
    end

  module Content_digest : sig
    type t

    val equal : t -> t -> bool
    val encode : t -> string
    val decode : string -> (t, Decode_error.t) result
  end

  module Content : sig
    type staged
    type committed
    type 'lifecycle t

    type format =
      | Utf8_text
      | Canonical_json
      | Binary

    val format : 'lifecycle t -> format
    val byte_count : 'lifecycle t -> Byte_count.t
    val digest : 'lifecycle t -> Content_digest.t
  end

  module Content_cursor : sig
    type t

    val beginning : t
  end

  module Content_page : sig
    type t

    val bytes : t -> Inline_bytes.t option
    val next : t -> Content_cursor.t
    val caught_up : t -> bool
  end

  module Citation_coordinate : sig
    type t =
      | Utf8_byte
      | Unicode_scalar
      | Utf16_code_unit
  end

  module Citation_index : sig
    type t

    val of_int64 : int64 -> (t, Construction_error.t) result
    val to_int64 : t -> int64
  end

  module Annotation_source : sig
    type staged
    type committed
    type 'lifecycle t
    type cursor

    type annotation =
      | Url_citation of
          { url : Content.committed Content.t
          ; title : Content.committed Content.t option
          ; coordinate : Citation_coordinate.t option
          ; start_index : Citation_index.t option
          ; end_index : Citation_index.t option
          }
      | File_citation of
          { filename : Content.committed Content.t option
          ; content : Blob_ref.t option
          }
      | Native_extension of
          { adapter : Provider_adapter_reference.t
          ; payload : Content.committed Content.t
          }

    type page =
      { annotations : annotation list
      ; next : cursor
      ; caught_up : bool
      }

    val beginning : committed t -> cursor
    val read
      :  Execution_journal.Reader.t
      -> committed t
      -> after:cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (page, Read_error.t) result
  end

  module Prepared_annotation : sig
    type t

    val url_citation
      :  url:Content.staged Content.t
      -> title:Content.staged Content.t option
      -> coordinate:Citation_coordinate.t option
      -> start_index:Citation_index.t option
      -> end_index:Citation_index.t option
      -> (t, Construction_error.t) result

    val file_citation
      :  filename:Content.staged Content.t option
      -> content:Blob_ref.t option
      -> (t, Construction_error.t) result

    val native_extension
      :  adapter:Provider_adapter_reference.t
      -> payload:Content.staged Content.t
      -> t
  end

  module Media_kind : sig
    type t =
      | Image
      | Audio
      | Video
      | Document
  end

  type media_source =
    | Inline of Content.committed Content.t
    | Blob of Blob_ref.t

  type item_view =
    | Text of
        { content : Content.committed Content.t
        ; annotations : Annotation_source.committed Annotation_source.t
        }
    | Json of Content.committed Content.t
    | Image of
        { media_type : Content.committed Content.t
        ; source : media_source
        }
    | Audio of
        { media_type : Content.committed Content.t
        ; source : media_source
        }
    | Video of
        { media_type : Content.committed Content.t
        ; source : media_source
        }
    | Document of
        { media_type : Content.committed Content.t
        ; source : media_source
        }
    | Resource of
        { reference : Blob_ref.t
        ; source_uri : Content.committed Content.t option
        ; media_type : Content.committed Content.t option
        }

  type prepared
  type item
  type t
  type cursor

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
  val manifest
    :  t
    -> (Execution_manifest_purpose.execution_value,
        Execution_manifest.committed)
         Execution_manifest.t
  val view_item : item -> item_view
  val beginning : t -> cursor

  type page =
    { items : item list
    ; next : cursor
    ; caught_up : bool
    }

  val read_items
    :  Execution_journal.Reader.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val read_content
    :  Execution_journal.Reader.t
    -> Content.committed Content.t
    -> after:Content_cursor.t
    -> max_bytes:Positive_byte_count.t
    -> (Content_page.t, Read_error.t) result

  module Builder : sig
    type t
    type utf8_writer
    type canonical_writer
    type binary_writer
    type annotation_builder

    val append_text
      :  t
      -> content:Content.staged Content.t
      -> annotations:Annotation_source.staged Annotation_source.t
      -> (unit, Execution_value_error.t) result

    val empty_annotations
      :  t
      -> (Annotation_source.staged Annotation_source.t,
          Execution_value_error.t)
         result

    val append_json
      :  t
      -> Content.staged Content.t
      -> (unit, Execution_value_error.t) result

    val append_media
      :  t
      -> kind:Media_kind.t
      -> media_type:Content.staged Content.t
      -> source:[ `Inline of Content.staged Content.t | `Blob of Blob_ref.t ]
      -> (unit, Execution_value_error.t) result

    val append_resource
      :  t
      -> reference:Blob_ref.t
      -> source_uri:Content.staged Content.t option
      -> media_type:Content.staged Content.t option
      -> (unit, Execution_value_error.t) result

    val begin_utf8
      :  t
      -> (utf8_writer, Execution_value_error.t) result
    val append_utf8
      :  utf8_writer
      -> Inline_bytes.t
      -> (unit, Execution_value_error.t) result
    val seal_utf8
      :  utf8_writer
      -> (Content.staged Content.t, Execution_value_error.t) result

    val begin_canonical
      :  t
      -> (canonical_writer, Execution_value_error.t) result
    val append_canonical
      :  canonical_writer
      -> Canonical_json_event.t
      -> (unit, Execution_value_error.t) result
    val seal_canonical
      :  canonical_writer
      -> (Content.staged Content.t, Execution_value_error.t) result

    val begin_binary
      :  t
      -> (binary_writer, Execution_value_error.t) result
    val append_binary
      :  binary_writer
      -> Inline_bytes.t
      -> (unit, Execution_value_error.t) result
    val seal_binary
      :  binary_writer
      -> (Content.staged Content.t, Execution_value_error.t) result

    val seal : t -> (prepared, Execution_value_error.t) result
    val abort : t -> (unit, Execution_value_error.t) result

    module Internal : sig
      val begin_
        :  sw:Eio.Switch.t
        -> transaction:
             (Execution_manifest_staging.open_
                Execution_manifest_staging.transaction)
        -> id:Execution_identity.Manifest_id.t
        -> (t, Execution_value_error.t) result

      val begin_annotations
        :  t
        -> (annotation_builder, Execution_value_error.t) result
      val append_annotation
        :  annotation_builder
        -> Prepared_annotation.t
        -> (unit, Execution_value_error.t) result
      val seal_annotations
        :  annotation_builder
        -> (Annotation_source.staged Annotation_source.t,
            Execution_value_error.t)
           result
    end
  end

  val begin_
    :  sw:Eio.Switch.t
    -> Execution_value_authority.t
    -> (Builder.t, Execution_value_error.t) result

  type value = t

  module Decoder : sig
    type t

    val value : t -> value
    val beginning : t -> cursor
    val read_items
      :  t
      -> after:cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (page, Read_error.t) result
    val read_content
      :  t
      -> Content.committed Content.t
      -> after:Content_cursor.t
      -> max_bytes:Positive_byte_count.t
      -> (Content_page.t, Read_error.t) result

    module Internal : sig
      val create
        :  reader:Execution_journal.Reader.t
        -> value
        -> t
    end
  end

end
```

`Execution_value.t` is an opaque committed manifest, never `content list`.
Its page records contain only closed item metadata and content/blob references;
text, JSON, and inline media bytes are separate content-addressed byte-paged
sources. `prepared` and staged content references cannot be encoded in facts or
returned by the read model. The Journal transaction that owns an input,
terminal, hook decision, or provider item incrementally copies its prepared
manifest/content and publishes the small committed root atomically.

The following properties are normative:

- text, structured JSON, image, audio, video, document, and resource values remain
  distinguishable;
- JSON object ordering has one canonical byte encoding;
- media source kind is a closed variant;
- no media value is flattened to empty text;
- no parallel `content`, `json`, and `content_blocks` fields compete as
  authorities.

An `Inline_bytes.t` is only one capacity-checked nonempty page/chunk.
`of_bytes_slice_copy` and `of_string_slice_copy` validate offset, length, and
explicit capacity, then own and charge exactly the requested bytes. They never
retain the caller's larger backing `bytes` or `string`, and cannot represent a
complete unbounded payload. Empty content is represented by a sealed zero-byte
`Content.t`, not by a fabricated chunk. The builder incrementally validates
UTF-8 or canonical JSON while copying chunks and hashes them once. No durable
identity, provider frame, or prepared value retains caller-owned `bytes`,
whole text, a `Canonical_json.t` tree, or a content-item list.

Every manifest page obeys both count and encoded-metadata byte bounds. Because
an item contains only fixed metadata/references, a single 131K-class text,
multimodal document, or Tool output is read through `read_content` without
inflating the item page. Codec decode similarly receives a paged reader over
the committed value; it does not reconstruct the full manifest first.
Text annotations/citations are a separate committed metadata manifest.
Provider-controlled URL, title, and filename bytes are content references, not
inline strings in a page record. Citation offsets are nonnegative typed values
with an exact adapter-declared coordinate system; construction rejects mixed
coordinate presence and `start > end`. Known URL/file citations use closed
variants. Other lossless metadata remains an adapter-revision-tagged native
extension content reference, so the dashboard can page it without parsing the
provider envelope or dropping it.
Media types and source URIs likewise enter as staged UTF-8 content sources and
become committed references only after incremental media-type/URI validation;
they are never unbounded heap strings inside item metadata. The item
constructor, not a dashboard or serializer, performs that validation and
rejects the wrong content format. No guessed byte ceiling or URI substring
classifier is used; byte admission remains the enclosing transaction's
explicit infrastructure capacity.

`Blob_ref.t` contains an immutable content digest and stable backend namespace.
The embedding runtime supplies the resolver. Expiring provider file IDs and
mutable URIs are projection evidence, not canonical durable content identity.

### 2.2 Canonical JSON Schema

The existing flat `Types.tool_param list` surface cannot express nested
objects, array item schemas, discriminated unions, or `oneOf`. It must be
replaced by one canonical `Json_schema.t` shared by:

- provider schema serialization;
- input decoding;
- validation errors;
- heterogeneous batch schema generation;
- MCP schema conversion.

```ocaml
module Finite_number : sig
  type t

  val of_float : float -> (t, Non_finite_number.t) result
  val to_float : t -> float
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Tool_argument_source : sig
  type t
  type cursor

  type page =
    { events : Execution_value.Canonical_json_event.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Decode_error.t) result
end

module Tool_decode_node_count : sig
  type t

  val zero : t
  val of_int64 : int64 -> (t, Construction_error.t) result
  val equal : t -> t -> bool
  val add : t -> t -> (t, Integer_overflow.t) result
end

module Positive_tool_decode_node_count : sig
  type t

  val of_node_count
    :  Tool_decode_node_count.t
    -> (t, Construction_error.t) result
  val to_node_count : t -> Tool_decode_node_count.t
end

module Tool_decode_allocation : sig
  type t

  val zero : t
  val of_components
    :  value_nodes:Tool_decode_node_count.t
    -> payload_bytes:Byte_count.t
    -> t
  val value_nodes : t -> Tool_decode_node_count.t
  val payload_bytes : t -> Byte_count.t
  val add
    :  t
    -> t
    -> (t, Integer_overflow.t) result
end

module Tool_decode_admission : sig
  type t
  type lease

  val value_node_capacity : t -> Positive_tool_decode_node_count.t
  val payload_byte_capacity : t -> Positive_byte_count.t

  val acquire_exact_or_await
    :  sw:Eio.Switch.t
    -> t
    -> Tool_decode_allocation.t
    -> (lease, Tool_decode_capacity_error.t) result

  val release
    :  lease
    -> (unit, Tool_decode_capacity_error.t) result

  module Internal : sig
    val create
      :  value_node_capacity:Positive_tool_decode_node_count.t
      -> payload_byte_capacity:Positive_byte_count.t
      -> t

    val close : t -> unit

    val close_and_await
      :  t
      -> (unit, Tool_decode_admission_close_error.t) result
  end
end

module Tool_decode_lifetime : sig
  type t

  (* Opaque, generation-tagged ownership of one admitted materialized input.
     Only Tool dispatch may transfer or release it. *)
end

module Tool_decode_context : sig
  type t

  val arguments : t -> Tool_argument_source.t
  val value_authority : t -> Execution_value_authority.t
  val decode_admission : t -> Tool_decode_admission.t
  val binding : t -> Executable_reference.t
  val exposure : t -> Tool_exposure_reference.t

  module Internal : sig
    val create
      :  sw:Eio.Switch.t
      -> value_authority:Execution_value_authority.t
      -> decode_admission:Tool_decode_admission.t
      -> arguments:Tool_argument_source.t
      -> binding:Executable_reference.t
      -> exposure:Tool_exposure_reference.t
      -> (t, Construction_error.t) result
  end
end

module Json_schema : sig
  type t

  module Codec : sig
    type 'a t
    type 'a codec = 'a t
    type 'a case
    type discriminator

    module Case_catalog : sig
      type 'a builder
      type 'a t

      val begin_ : unit -> 'a builder
      val append
        :  'a builder
        -> 'a case
        -> (unit, Construction_error.t) result
      val seal
        :  'a builder
        -> ('a t, Construction_error.t) result
    end

    module Object : sig
      type 'a t

      val empty : unit t
      val required
        :  name:string
        -> 'a codec
        -> 'b t
        -> ('a * 'b) t
      val optional
        :  name:string
        -> 'a codec
        -> 'b t
        -> ('a option * 'b) t
    end

    val string : string t
    val bool : bool t
    val int64 : int64 t
    val number : Finite_number.t t
    val list : 'a t -> 'a list t
    val nonempty_list : 'a t -> ('a * 'a list) t
    val const
      :  'a t
      -> value:'a
      -> ('a t, Construction_error.t) result
    val enum
      :  'a t
      -> values:'a list
      -> ('a t, Construction_error.t) result
    val object_
      :  'a Object.t
      -> ('a t, Construction_error.t) result
    val discriminator
      :  fields:string list
      -> (discriminator, Construction_error.t) result
    val case
      :  discriminator:discriminator
      -> codec:'a t
      -> inject:('a -> ('b, Decode_error.t) result)
      -> project:('b -> ('a option, Encode_error.t) result)
      -> ('b case, Construction_error.t) result
    val one_of
      :  discriminator:discriminator
      -> 'a Case_catalog.t
      -> ('a t, Construction_error.t) result
    val iso
      :  'a t
      -> to_representation:('b -> ('a, Encode_error.t) result)
      -> of_representation:('a -> ('b, Decode_error.t) result)
      -> 'b t

    val schema : 'a t -> Json_schema.t
    val decode
      :  Tool_decode_context.t
      -> 'a t
      -> ('a, Decode_error.t) result
    val encode
      :  Execution_value.Builder.t
      -> 'a t
      -> 'a
      -> (unit, Encode_error.t) result
  end
end
```

Schema and decoder are one value. A schema declaration and an unrelated
hand-written parser cannot be registered as one Tool case. JSON `number` uses
the abstract `Finite_number.t`; `NaN`, positive infinity, and negative infinity
fail `Finite_number.of_float` and therefore cannot reach the total canonical
encoder. Migrated Tool implementations may explicitly convert a decoded
finite value with `to_float`, but no compatibility codec silently accepts a
non-finite OCaml `float`.
Decode consumes `Tool_argument_source` pages through one invocation-bound
`Tool_decode_context`; there is no `Yojson.Safe.t` overload. An ordinary
application codec may materialize its chosen OCaml representation, including
an OCaml list, only after a first bounded validation pass computes the exact
framework-owned allocation vector—logical value nodes and copied payload
bytes—from its closed codec structure and the source events, and
`Tool_decode_admission` atomically grants the whole vector. These are
independent exact units; OAS never multiplies nodes by a guessed heap-size
factor.
The second pass materializes while that switch-bound lease remains held.
There is no partial-lease hold-and-wait and OAS supplies no numeric default.
Exhaustion is a typed infrastructure/decode failure and does not truncate or
reinterpret the input. Application `iso` callbacks and the Tool runner remain
trusted application code, just like any user callback; their allocations are
not mislabeled as framework-accounted bytes. Collection adapters do not use
the materializing list combinator: their private streaming decoder appends
each validated member directly to a prepared call manifest.
The runtime owns one admission with independent positive node and copied-byte
capacities. `acquire_exact_or_await` compares the complete vector with both
capacities before queueing, acquires both dimensions atomically, and never
retains one dimension while waiting for the other. Successful decode transfers
the exact generation-tagged lease into `Tool_internal.decoded`.
`run_decoded` consumes that one-shot value and releases the lease after the
last PreTool/handler/observer use of the materialized input, before the
already-externalized terminal preparation enters Journal commit. Decode,
binding, handler, hook, projection, infrastructure, and cancellation exits all
settle it exactly once. A terminal-commit failure cannot retain the input
because commit never receives the decode lifetime. Double, stale, foreign, or
second-run release is a typed capacity error. Runtime close rejects new
acquisitions, wakes queued decoders with a typed stopping result, and waits for
every admitted lease to settle.
`Object.required`/`optional` preserve every declared field occurrence;
`object_` is the single checked closing boundary and rejects duplicate names,
required/optional collisions, and discriminator collisions. It never resolves
them by last-write-wins, exception, or schema/decoder divergence.
`const` and `enum` refine the same typed codec used by encode/decode; they do
not attach an unrelated schema fragment. `nonempty_list` emits `minItems: 1`
and decodes to a head/tail value, so an empty list is unrepresentable.
`discriminator ~fields` requires a nonempty, duplicate-free ordered field set.
Every case must expose exact `const` values for all those fields, and the tuple
must be unique across cases. The two fields of an executable exposure
(`tool_id`, `tool_revision`) are therefore compared structurally; no compound
string, concatenation, substring, or display label selects a case.
`case` and `iso` conversion functions return typed results in both directions.
The codec boundary catches any non-cancellation exception raised by an
application conversion and returns it as `Encode_error`/`Decode_error`;
reserved cancellation is re-raised unchanged. Canonical encode therefore has
an explicit error channel and cannot silently escape through a partial
projection.

### 2.3 Acyclic compilation spine

The signatures in this RFC map to ordinary, acyclic OCaml compilation units.
They do not rely on forward module references or recursive modules.

The existential representation-sharing core is one Dune-private
`Execution_algebra` compilation unit with logical submodules
`Executable`, `Executable_registry`, `Tool`, `Executable_internal`, and
`Tool_internal`. Those submodules share private GADT representations without
`Obj.magic`; they are not five compilation units pretending that OCaml
`private` or abstract types are constructible across `.mli` boundaries.
`Agent_sdk` re-exports only narrowed public signatures for `Executable` and
`Tool`. Catalog, plans, async execution, and read projections remain separate
acyclic compilation units depending on that core.

The narrow `Execution_journal.Reader`/`Writer` capabilities are part of the
pre-existing Journal foundation and compile before this algebra. Their payload
boundary is canonical journal facts and identities, not `Executable.t` or
`Agent.t`; higher codecs construct those facts. The same representation-owning
Journal unit also creates one checked `Execution_journal_access.t`; callers
cannot assemble an unrelated reader and writer into a runtime. This permits
`Child_agent_run_authority` to bind the one Journal authority without a
Journal-to-Agent or Journal-to-Tool dependency.

The dependency leaf below is compiled before `Executable`:

```ocaml
module Composite_recovery : sig
  type t

  val structural_children : unit -> t
end

module Commit_action : sig
  type reference =
    { id : Execution_identity.Commit_action_id.t
    ; revision : Execution_identity.Commit_action_revision.t
    }

  type t

  val reference : t -> reference
  val payload : t -> Execution_value.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  reference:reference
      -> payload:Execution_value.t
      -> t
  end
end

module Durable_codec : sig
  type 'a t

  val create
    :  version:Codec_version.t
    -> encode:
         (Execution_value.Builder.t
          -> 'a
          -> (unit, Encode_error.t) result)
    -> decode:
         (Execution_value.Decoder.t
          -> ('a, Decode_error.t) result)
    -> 'a t

  val version : 'a t -> Codec_version.t
  val encode
    :  'a t
    -> into:Execution_value.Builder.t
    -> 'a
    -> (unit, Encode_error.t) result
  val decode
    :  'a t
    -> Execution_value.Decoder.t
    -> ('a, Decode_error.t) result
end

(* Every encode path is result-bearing; prose below defines exception
   normalization and cancellation preservation. *)
module Submission_request_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Backend_semantic_rejection : sig
  type t

  type kind =
    | Invalid_request
    | Unsupported_contract
    | Idempotency_conflict
    | Authorization_denied
    | Definitively_refused

  val kind : t -> kind
  val detail : t -> Execution_value.t

  val create
    :  kind:kind
    -> detail:Execution_value.t
    -> t
end

module Backend_rejection_witness : sig
  type t

  val submission : t -> Execution_identity.Submission_id.t
  val digest : t -> Submission_request_digest.t
  val backend : t -> Execution_identity.Operation_backend_id.t
  val reason : t -> Backend_semantic_rejection.t
end

module Backend_absence_witness : sig
  type t

  val submission : t -> Execution_identity.Submission_id.t
  val digest : t -> Submission_request_digest.t
  val backend : t -> Execution_identity.Operation_backend_id.t
end

module Execution_value_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result

  module Internal : sig
    val compute : Execution_value.t -> t
  end
end

module Writer_epoch : sig
  type t

  val of_int64 : int64 -> (t, Decode_error.t) result
  val to_int64 : t -> int64
  val successor : t -> (t, Epoch_exhausted.t) result
  val compare : t -> t -> int
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Backend_supervisor_fence : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t
  val owner : t -> Writer_owner.Id.t
  val epoch : t -> Writer_epoch.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_fence : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t
  val operation : t -> Execution_identity.Operation_id.t
  val owner : t -> Writer_owner.Id.t
  val epoch : t -> Writer_epoch.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_fence_takeover : sig
  type t

  val supervisor : t -> Backend_supervisor_fence.t
  val operation : t -> Execution_identity.Operation_id.t
  val previous : t -> Operation_fence.t option
  val current : t -> Operation_fence.t
  val established_by : t -> Execution_fact_ref.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_backend_protocol : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t

  val fresh_claim_id
    :  t
    -> (Execution_identity.Claim_id.t, Identity_error.t) result

  val fresh_claim_batch_id
    :  t
    -> (Execution_identity.Claim_batch_id.t, Identity_error.t) result

  val fresh_event_id
    :  t
    -> (Execution_identity.Event_id.t, Identity_error.t) result

  val fresh_event_stream_id
    :  t
    -> (Execution_identity.Event_stream_id.t, Identity_error.t) result

  val observation_now
    :  t
    -> (Observation_time.t, Observation_error.t) result

  val rejection_witness
    :  t
    -> submission:Execution_identity.Submission_id.t
    -> digest:Submission_request_digest.t
    -> reason:Backend_semantic_rejection.t
    -> Backend_rejection_witness.t

  val absence_witness
    :  t
    -> submission:Execution_identity.Submission_id.t
    -> digest:Submission_request_digest.t
    -> Backend_absence_witness.t

  val fact_ref
    :  t
    -> stream:Execution_stream_reference.t
    -> cursor:Execution_cursor.t
    -> event:Execution_identity.Event_id.t
    -> ordinal:int
    -> (Execution_fact_ref.t, Construction_error.t) result

  val supervisor_fence
    :  t
    -> backend:Execution_identity.Operation_backend_id.t
    -> owner:Writer_owner.Id.t
    -> epoch:Writer_epoch.t
    -> (Backend_supervisor_fence.t, Construction_error.t) result

  val operation_fence
    :  t
    -> backend:Execution_identity.Operation_backend_id.t
    -> operation:Execution_identity.Operation_id.t
    -> owner:Writer_owner.Id.t
    -> epoch:Writer_epoch.t
    -> (Operation_fence.t, Construction_error.t) result

  val fence_takeover
    :  t
    -> supervisor:Backend_supervisor_fence.t
    -> operation:Execution_identity.Operation_id.t
    -> previous:Operation_fence.t option
    -> current:Operation_fence.t
    -> established_by:Execution_fact_ref.t
    -> (Operation_fence_takeover.t, Construction_error.t) result

  module Internal : sig
    val create
      :  identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> backend:Execution_identity.Operation_backend_id.t
      -> t
  end
end

module Execution_journal_access : sig
  type t
  type scope_claim

  val reader : t -> Execution_journal.Reader.t
  val writer : t -> Execution_journal.Writer.t

  val claim_scope
    :  sw:Eio.Switch.t
    -> t
    -> (scope_claim, Scope_already_claimed.t) result

  module Internal : sig
    val create
      :  reader:Execution_journal.Reader.t
      -> writer:Execution_journal.Writer.t
      -> (t, Construction_error.t) result

    val same_authority : t -> t -> bool
    val release_scope : scope_claim -> unit
  end
end

module Execution_fence : sig
  type source =
    | Journal_invocation of Execution_identity.Invocation_id.t
    | Operation of
        { backend : Execution_identity.Operation_backend_id.t
        ; operation : Execution_identity.Operation_id.t
        }

  type t

  val source : t -> source
  val epoch : t -> Writer_epoch.t
  val compare_same_source
    :  t
    -> t
    -> (int, Fence_source_mismatch.t) result
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  source:source
      -> epoch:Writer_epoch.t
      -> (t, Construction_error.t) result
  end
end

module Fencing_token : sig
  type t

  val execution_fence : t -> Execution_fence.t
  val invocation : t -> Execution_identity.Invocation_id.t
  val attempt : t -> Execution_identity.Attempt_id.t
  val effect_id : t -> Execution_identity.Effect_id.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  execution_fence:Execution_fence.t
      -> invocation:Execution_identity.Invocation_id.t
      -> attempt:Execution_identity.Attempt_id.t
      -> effect_id:Execution_identity.Effect_id.t
      -> t
  end
end

module Effect_idempotency_key : sig
  type t

  val effect_id : t -> Execution_identity.Effect_id.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_effect_id : Execution_identity.Effect_id.t -> t
  end
end

module Effect_protocol_reference : sig
  type t =
    { id : Execution_identity.Effect_protocol_id.t
    ; revision : Execution_identity.Effect_protocol_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Effect_open_uncertainty_cause : sig
  type t =
    | Execution_outcome_unavailable
    | Receipt_commit_unconfirmed
    | Reconciliation_still_unknown
    | Reconciliation_failed of Effect_recovery_error.t

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Effect_uncertainty_cause : sig
  type t =
    | Open_effect of Effect_open_uncertainty_cause.t
    | Post_effect_continuation_unavailable

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Effect_fact : sig
  module Entry : sig
    type t

    val id : t -> Execution_identity.Effect_id.t
    val invocation_id : t -> Execution_identity.Invocation_id.t
    val attempt_id : t -> Execution_identity.Attempt_id.t
    val protocol : t -> Effect_protocol_reference.t
    val request : t -> Execution_value.t
    val fence : t -> Fencing_token.t
    val idempotency_key : t -> Effect_idempotency_key.t
    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result

    module Internal : sig
      val create
        :  effect_id:Execution_identity.Effect_id.t
        -> invocation_id:Execution_identity.Invocation_id.t
        -> attempt_id:Execution_identity.Attempt_id.t
        -> protocol:Effect_protocol_reference.t
        -> request:Execution_value.t
        -> fence:Fencing_token.t
        -> idempotency_key:Effect_idempotency_key.t
        -> (t, Construction_error.t) result
    end
  end

  module Receipt : sig
    type outcome =
      | Completed of Execution_value.t
      | Failed of Execution_value.t

    type t

    val effect_id : t -> Execution_identity.Effect_id.t
    val protocol : t -> Effect_protocol_reference.t
    val entry_fact : t -> Execution_fact_ref.t
    val outcome : t -> outcome
    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result

    module Internal : sig
      val create
        :  entry:Entry.t
        -> entry_fact:Execution_fact_ref.t
        -> outcome:outcome
        -> (t, Construction_error.t) result
    end
  end

  module Unknown : sig
    type evidence =
      | Open_entry of
          { entry : Entry.t
          ; entry_fact : Execution_fact_ref.t
          }
      | Settled_receipt of
          { entry : Entry.t
          ; entry_fact : Execution_fact_ref.t
          ; receipt : Receipt.t
          ; receipt_fact : Execution_fact_ref.t
          }

    type t

    val effect_id : t -> Execution_identity.Effect_id.t
    val protocol : t -> Effect_protocol_reference.t
    val entry_fact : t -> Execution_fact_ref.t
    val evidence : t -> evidence
    val cause : t -> Effect_uncertainty_cause.t
    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result

    module Internal : sig
      val create_open
        :  entry:Entry.t
        -> entry_fact:Execution_fact_ref.t
        -> cause:Effect_open_uncertainty_cause.t
        -> (t, Construction_error.t) result

      val create_settled
        :  entry:Entry.t
        -> entry_fact:Execution_fact_ref.t
        -> receipt:Receipt.t
        -> receipt_fact:Execution_fact_ref.t
        -> (t, Construction_error.t) result
    end
  end
end

module Effect_entry = Effect_fact.Entry
module Effect_receipt = Effect_fact.Receipt
module Effect_unknown = Effect_fact.Unknown

module Effect_protocol : sig
  module Id = Execution_identity.Effect_protocol_id
  module Revision = Execution_identity.Effect_protocol_revision

  type ('receipt, 'failure) reconciliation =
    | Absent
    | Completed of 'receipt
    | Failed of 'failure
    | Still_unknown

  type ('request, 'receipt, 'failure) operation

  type reference = Effect_protocol_reference.t

  type packed =
    | Operation :
        ('request, 'receipt, 'failure) operation
        -> packed

  type t

  val create_operation
    :  id:Id.t
    -> revision:Revision.t
    -> request:'request Durable_codec.t
    -> receipt:'receipt Durable_codec.t
    -> failure:'failure Durable_codec.t
    -> reconcile:
         (fence:Fencing_token.t
          -> idempotency_key:Effect_idempotency_key.t
          -> 'request
          -> (('receipt, 'failure) reconciliation,
              Effect_recovery_error.t)
             result)
    -> ('request, 'receipt, 'failure) operation

  val set : packed list -> (t, Construction_error.t) result
  val reference : (_, _, _) operation -> reference
  val find : t -> reference -> (packed, Lookup_error.t) result

  module Internal : sig
    type reconciliation_fact =
      | Absent
      | Completed of Execution_value.t
      | Failed of Execution_value.t
      | Still_unknown

    val reconcile_encoded
      :  t
      -> reference:reference
      -> fence:Fencing_token.t
      -> idempotency_key:Effect_idempotency_key.t
      -> request:Execution_value.t
      -> (reconciliation_fact, Effect_recovery_error.t) result

    val encode_request
      :  ('request, _, _) operation
      -> 'request
      -> (Execution_value.t, Encode_error.t) result

    val encode_receipt
      :  (_, 'receipt, _) operation
      -> 'receipt
      -> (Execution_value.t, Encode_error.t) result

    val encode_failure
      :  (_, _, 'failure) operation
      -> 'failure
      -> (Execution_value.t, Encode_error.t) result

    val decode_receipt
      :  (_, 'receipt, _) operation
      -> Execution_value.t
      -> ('receipt, Decode_error.t) result

    val decode_failure
      :  (_, _, 'failure) operation
      -> Execution_value.t
      -> ('failure, Decode_error.t) result
  end
end

module Recovery_policy : sig
  type ('input, 'output, 'failure) t =
    | Replay_safe
    | External_effect of Effect_protocol.t
    | Journal_resumable_composite of Composite_recovery.t
end

(* Recovery semantics are closed by constructor:
   - Replay_safe may abandon the old attempt and return the operation to Ready.
   - Journal_resumable_composite may resume the same attempt from durable child
     checkpoints.
   - External_effect never resumes an arbitrary OCaml continuation. If no
     effect fact exists in the complete takeover-high-water ledger fold, it may
     return to Ready. If that complete fold contains exactly one open entry,
     reconciliation proves the effect Absent, and no earlier settled effect
     exists, it may also return to Ready under a newer fence. A completed,
     failed, or still-unknown open effect, any earlier settled effect, more than
     one open entry, an invalid entry/receipt pairing, or a fully settled ledger
     without an already committed executable terminal becomes typed corruption
     or Outcome_unknown with the exact durable evidence. No latest-fact
     shortcut participates in this decision. *)

module Effect_driver : sig
  type t

  type encoded_execute =
    fence:Fencing_token.t
    -> idempotency_key:Effect_idempotency_key.t
    -> (Execution_value.t, Execution_value.t) result

  val perform_encoded
    :  t
    -> protocol:Effect_protocol.reference
    -> request:Execution_value.t
    -> execute:encoded_execute
    -> ((Execution_value.t, Execution_value.t) result,
        Effect_execution_error.t)
       result

  module Internal : sig
    val create
      :  perform:
           (protocol:Effect_protocol.reference
            -> request:Execution_value.t
            -> execute:encoded_execute
            -> ((Execution_value.t, Execution_value.t) result,
                Effect_execution_error.t)
               result)
      -> t
  end
end

module Effect_boundary : sig
  type t

  val perform
    :  t
    -> protocol:('request, 'receipt, 'failure) Effect_protocol.operation
    -> request:'request
    -> execute:
         (fence:Fencing_token.t
          -> idempotency_key:Effect_idempotency_key.t
          -> 'request
          -> ('receipt, 'failure) result)
    -> (('receipt, 'failure) result, Effect_execution_error.t) result

  module Internal : sig
    val create
      :  driver:Effect_driver.t
      -> allowed:Effect_protocol.t
      -> executable:Executable_reference.t
      -> attempt:Invocation_attempt_reference.t
      -> t
  end
end

module Context_factory_reference : sig
  type t =
    { id : Execution_identity.Context_factory_id.t
    ; revision : Execution_identity.Context_factory_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Async_runtime_reference : sig
  type t =
    { id : Execution_identity.Async_runtime_id.t
    ; revision : Execution_identity.Async_runtime_revision.t
    }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Invocation_reference : sig
  type origin =
    | Provider_tool_call of Provider_tool_call_reference.t
    | Programmatic_child
    | Async_operation of Execution_identity.Operation_id.t

  type t

  val id : t -> Execution_identity.Invocation_id.t
  val origin : t -> origin
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  id:Execution_identity.Invocation_id.t
      -> origin:origin
      -> t
  end
end

module Invocation_open_reference : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val invocation : t -> Invocation_reference.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_committed_open
      :  Execution_journal.Committed_invocation_opened.t
      -> t
  end
end

module Invocation_attempt_reference : sig
  type t

  val invocation : t -> Invocation_open_reference.t
  val attempt : t -> Attempt.Id.t
  val opened_fact : t -> Execution_fact_ref.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_committed_attempt
      :  Execution_journal.Committed_attempt_opened.t
      -> t
  end
end

module Execution_runtime_authority : sig
  type t

  module Internal : sig
    val create : unit -> t
    val equal : t -> t -> bool
  end
end

module Submission_backend_request_route_authority : sig
  type t
end

module Execution_scope_authority : sig
  type t

  module Internal : sig
    val create : unit -> t
    val equal : t -> t -> bool
  end
end

module Conversation_selection_authority : sig
  type t
end

module Agent_prelude_authority : sig
  type t
end

module Async_submission_resolver : sig
  type t

  type prepared =
    { receipt : Execution_value.t
    ; after_commit : Commit_action.t
    }

  type result =
    | Prepared of prepared
    | Rejected of Async_prepare_error.t
    | Reconciliation_required of
        { submission_id : Execution_identity.Submission_id.t
        ; request_digest : Submission_request_digest.t
        }

  val prepare
    :  t
    -> runtime:Async_runtime_reference.t
    -> submission_id:Execution_identity.Submission_id.t
    -> caused_by:Invocation_attempt_reference.t
    -> request:Execution_value.t
    -> result

  module Internal : sig
    val create
      :  runtime_authority:Execution_runtime_authority.t
      -> journal:Execution_journal_access.t
      -> prepare:
           (runtime:Async_runtime_reference.t
            -> submission_id:Execution_identity.Submission_id.t
            -> caused_by:Invocation_attempt_reference.t
            -> request:Execution_value.t
            -> result)
      -> t

    val runtime_authority : t -> Execution_runtime_authority.t
    val journal : t -> Execution_journal_access.t
  end
end

module Async_submission_authority : sig
  type t

  val prepare
    :  t
    -> runtime:Async_runtime_reference.t
    -> request:Execution_value.t
    -> Async_submission_resolver.result

  module Internal : sig
    type view =
      { journal : Execution_journal_access.t
      ; resolver : Async_submission_resolver.t
      ; owner : Invocation_attempt_reference.t
      }

    val create
      :  journal:Execution_journal_access.t
      -> resolver:Async_submission_resolver.t
      -> owner:Invocation_attempt_reference.t
      -> (t, Construction_error.t) result

    val view : t -> view
  end
end

module Media_stabilizer : sig
  type t

  val create
    :  stabilize_media:
         (media_type:string
          -> source_type:Types.media_source_kind
          -> data:string
          -> (Execution_value.media_source, Media_stabilization_error.t) result)
    -> t

  module Internal : sig
    val stabilize
      :  t
      -> media_type:string
      -> source_type:Types.media_source_kind
      -> data:string
      -> (Execution_value.media_source, Media_stabilization_error.t) result
  end
end

module Cpu_cancellation : sig
  type t

  val check : t -> (unit, Cpu_cancelled.t) result
end

module Cpu_executor : sig
  type t

  module Snapshot : sig
    type t

    val admitted : t -> int
    val queued : t -> int
    val running : t -> int
    val cancellation_requested : t -> int
    val reentrant_inline : t -> int64
  end

  val submit
    :  t
    -> sw:Eio.Switch.t
    -> (Cpu_cancellation.t -> 'a)
    -> ('a, Cpu_execution_error.t) result

  val snapshot : t -> Snapshot.t

  module Internal : sig
    val create
      :  sw:Eio.Switch.t
      -> domain_mgr:_ Eio.Domain_manager.t
      -> workers:Positive_int.t
      -> admission_capacity:Positive_int.t
      -> t

    val close_and_await
      :  t
      -> (unit, Cpu_executor_close_error.t) result
  end
end

module Execution_admission : sig
  type t
  type slot

  type owner =
    | Agent_run of Agent_run.Id.t
    | Invocation_attempt of Invocation_attempt_reference.t

  module Internal : sig
    val create : capacity:Positive_int.t -> t

    val acquire_or_await
      :  sw:Eio.Switch.t
      -> t
      -> limit:Positive_int.t
      -> owner:owner
      -> (slot list, Execution_admission_error.t) result

    val handoff
      :  parent:slot
      -> from:owner
      -> to_:owner
      -> (slot, Execution_admission_error.t) result

    val return
      :  child:slot
      -> parent:slot
      -> from:owner
      -> to_:owner
      -> (slot, Execution_admission_error.t) result

    val release
      :  slot
      -> owner:owner
      -> (unit, Execution_admission_error.t) result

    val close : t -> unit
  end
end

module Read_admission : sig
  type t

  module Internal : sig
    val create
      :  stream_capacity:Positive_int.t
      -> batch_size:Positive_int.t
      -> page_byte_capacity:Positive_byte_count.t
      -> t

    val with_stream
      :  sw:Eio.Switch.t
      -> t
      -> (unit -> ('a, Read_error.t) result)
      -> ('a, Read_error.t) result

    val batch_size : t -> Positive_int.t
    val page_byte_capacity : t -> Positive_byte_count.t

    val close_and_await
      :  t
      -> (unit, Read_admission_close_error.t) result
  end
end

module Projection_context : sig
  type t

  val decoder
    :  t
    -> Execution_value.t
    -> Execution_value.Decoder.t

  module Internal : sig
    val create
      :  sw:Eio.Switch.t
      -> reader:Execution_journal.Reader.t
      -> admission:Read_admission.t
      -> t
  end
end

module Execution_journal_bootstrap : sig
  type t
  type opened
  type read_opened
  type quiescent
  type closed
  type exclusive_reopen_closed

  type durability =
    | Volatile
    | Durable

  type opening =
    | Volatile_created
    | Durable_created of Execution_journal_initialization.t
    | Durable_opened of Execution_journal_recovery.t

  val volatile
    :  ?correlation:Execution_identity.Correlation_id.t
    -> unit
    -> t

  val durable_create
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> ?correlation:Execution_identity.Correlation_id.t
    -> unit
    -> t

  val durable_open
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> t

  val durability : t -> durability
  val access : opened -> Execution_journal_access.t
  val opening : opened -> opening
  val reader : read_opened -> Execution_journal.Reader.t

  module Internal : sig
    val open_
      :  sw:Eio.Switch.t
      -> cpu_executor:Cpu_executor.t
      -> identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> append_admission:Journal_append_admission.t
      -> t
      -> (opened, Execution_journal_open_error.t) result

    val open_reader
      :  sw:Eio.Switch.t
      -> cpu_executor:Cpu_executor.t
      -> t
      -> (read_opened, Execution_journal_open_error.t) result

    val quiesce_and_await
      :  opened
      -> (quiescent, Execution_journal_quiesce_error.t) result

    val quiescent_reader : quiescent -> Execution_journal.Reader.t

    val close_and_await
      :  quiescent
      -> (closed, Execution_journal_close_error.t) result

    val close_exclusive_reopen
      :  opened
      -> Execution_journal_access.scope_claim
      -> (exclusive_reopen_closed, Execution_journal_close_error.t) result

    val close_reader
      :  read_opened
      -> (unit, Execution_journal_close_error.t) result
  end
end

module Provider_stream_byte_admission : sig
  type t
  type attempt_lease

  module Internal : sig
    val create
      :  global_capacity:Positive_byte_count.t
      -> t

    val acquire_attempt_or_await
      :  sw:Eio.Switch.t
      -> t
      -> capacity:Positive_byte_count.t
      -> (attempt_lease, Provider_stream_admission_error.t) result

    val release_attempt
      :  attempt_lease
      -> (unit, Provider_stream_admission_error.t) result

    val close_and_await
      :  t
      -> (unit, Provider_stream_admission_close_error.t) result
  end
end

module Execution_scope_services : sig
  type t

  val runtime_authority : t -> Execution_runtime_authority.t
  val scope_authority : t -> Execution_scope_authority.t

  module Internal : sig
    type view =
      { runtime_authority : Execution_runtime_authority.t
      ; scope_authority : Execution_scope_authority.t
      ; journal : Execution_journal_access.t
      ; cpu_executor : Cpu_executor.t
      ; execution_admission : Execution_admission.t
      ; provider_stream_bytes : Provider_stream_byte_admission.t
      ; provider_stream_queue_frame_capacity : Positive_int.t
      ; provider_stream_per_attempt_byte_capacity : Positive_byte_count.t
      ; provider_transport_read_chunk_capacity : Positive_byte_count.t
      ; provider_spool : Provider_spool_store.t
      ; provider_spool_page_byte_capacity : Positive_byte_count.t
      ; tool_decode_admission : Tool_decode_admission.t
      ; media_stabilizer : Media_stabilizer.t
      ; async_resolver : Async_submission_resolver.t
      }

    val create
      :  runtime_authority:Execution_runtime_authority.t
      -> scope_authority:Execution_scope_authority.t
      -> journal:Execution_journal_access.t
      -> cpu_executor:Cpu_executor.t
      -> execution_admission:Execution_admission.t
      -> provider_stream_bytes:Provider_stream_byte_admission.t
      -> provider_stream_queue_frame_capacity:Positive_int.t
      -> provider_stream_per_attempt_byte_capacity:Positive_byte_count.t
      -> provider_transport_read_chunk_capacity:Positive_byte_count.t
      -> provider_spool:Provider_spool_store.t
      -> provider_spool_page_byte_capacity:Positive_byte_count.t
      -> tool_decode_admission:Tool_decode_admission.t
      -> media_stabilizer:Media_stabilizer.t
      -> async_resolver:Async_submission_resolver.t
      -> (t, Construction_error.t) result

    val view : t -> view
  end
end

module Child_agent_run_authority : sig
  type t

  module Internal : sig
    type view =
      | View :
          { services : Execution_scope_services.t
          ; owner : Invocation_attempt_reference.t
          ; execution_slot : Execution_admission.slot
          ; switch : Eio.Switch.t
          ; clock : 'clock Eio.Time.clock
          }
          -> view

    val create
      :  services:Execution_scope_services.t
      -> owner:Invocation_attempt_reference.t
      -> execution_slot:Execution_admission.slot
      -> switch:Eio.Switch.t
      -> clock:_ Eio.Time.clock
      -> t

    val view : t -> view
  end
end

module Execution_context : sig
  type t

  val invocation : t -> Invocation_reference.t
  val attempt : t -> Execution_identity.Attempt_id.t
  val switch : t -> Eio.Switch.t
  val state : t -> Context.t
  val effects : t -> Effect_boundary.t

  module Internal : sig
    val create
      :  attempt:Invocation_attempt_reference.t
      -> execution_slot:Execution_admission.slot
      -> switch:Eio.Switch.t
      -> state:Context.t
      -> effects:Effect_boundary.t
      -> async_submissions:Async_submission_authority.t
      -> child_agent_runs:Child_agent_run_authority.t
      -> services:Execution_scope_services.t
      -> (t, Construction_error.t) result

    val async_submissions : t -> Async_submission_authority.t
    val child_agent_runs : t -> Child_agent_run_authority.t
    val services : t -> Execution_scope_services.t
    val cpu_executor : t -> Cpu_executor.t
    val execution_value_authority : t -> Execution_value_authority.t
    val tool_decode_admission : t -> Tool_decode_admission.t
    val projection_context : t -> Projection_context.t
    val attempt_reference : t -> Invocation_attempt_reference.t
    val execution_slot : t -> Execution_admission.slot
  end
end
```

`Durable_codec` is a total typed boundary, not an infallible application
callback disguised as one. `create` installs the same exception firewall as
the schema codec: a returned `Error` and every non-cancellation exception
become `Encode_error.t`; `Eio.Cancel.Cancelled` is re-raised unchanged. Input,
success, failure, effect request/receipt/failure, Agent checkpoint, and async
digest material all use this one result-bearing path. An encode failure after a
handler domain result commits a typed infrastructure terminal that preserves
the native domain outcome when it was already known; it never drops the open
attempt or escapes as an untyped exception.
Encoding receives the runtime-owned incremental `Execution_value.Builder.t`
and decoding receives a paged `Execution_value.Decoder.t`; neither callback
can demand a whole `Execution_value.t` tree/list/string. The runtime seals and
commits the builder only after the callback succeeds and aborts it on every
error/cancellation path. Schema-derived codecs implement the same sink/source
contract, so a hand-written durable codec cannot bypass manifest/content byte
admission merely because its OCaml value is programmatic.

`Composite_recovery.t` is an opaque runtime-owned policy. It never contains an
application callback. It instructs recovery to read the existing child plan
and child terminals from the Journal, retain their original invocation
identities, and resume only children whose exact recovery policy permits it.

`Commit_action.t` is a durable, versioned protocol envelope, not a closure.
Its payload is canonical `Execution_value.t`, and its exact
`(Commit_action_id, revision)` selects a registered private decoder/executor.
The Journal stores the envelope beside the terminal result before reporting the
result committed. A runtime cannot infer an action from Tool name, output text,
or payload shape.
`Commit_action.Internal.create` is a submodule of the representation-owning
compilation unit and is omitted by the public `Agent_sdk` signature.

An `Effect_protocol.operation` likewise carries one stable protocol reference
and the request, receipt, and failure codecs bound to that revision.
`Effect_protocol.set` rejects duplicate references. Recovery resolves the exact
reference before decoding or reconciling an effect; an unavailable revision is
a typed recovery failure rather than a replay with a newer protocol.
The typed reconciler returns `result`: `Still_unknown` means a successful query
established an ambiguous external state, while query, transport, or decode
failure remains `Error Effect_recovery_error.t` and is preserved as
`Effect_open_uncertainty_cause.Reconciliation_failed`.
The representation-owning `Effect_protocol.Internal` submodule opens the
existential operation, decodes the exact request type, invokes its typed
reconciler, and re-encodes the request, receipt, or failure. `Effect_fact`
owns entry, receipt, and uncertainty representations before
`Effect_boundary` is compiled. `Effect_boundary.Internal.create` accepts only
the private typed driver used by a Journal attempt or operation runner and the
exact protocol set declared by that executable's `Recovery_policy`. A
`Replay_safe` or composite-only handler therefore receives no external-effect
grant, and another protocol revision is a typed refusal before entry commit or
external I/O; a closure cannot smuggle an undeclared effect around durability
readiness. There
is no later compilation unit constructing an earlier abstract type. Driver
entry/receipt commit failure and commit uncertainty are returned through
`Effect_execution_error.t`, never converted to a domain failure or dropped.
The public signature omits these eliminators and constructors.

The public `Invocation` module in §4 aliases `Invocation_reference.t` and
`Execution_context.t`. `Executable` depends only on the leaf modules above;
`Operation` depends on `Executable`; neither dependency points back.

`Async_submission_resolver.t` is a process-local capability installed by final
Agent/runtime construction. Its boundary is canonical `Execution_value.t`
under the exact async-runtime revision, not JSON or a name-classified handler.
It is never exposed directly to a Tool invocation.
`Async_submission_authority.t` binds that resolver to the checked Journal
access and the exact owning invocation/attempt. Construction rejects a resolver
whose `Async_submission_resolver.Internal.journal` is not the same checked
authority. `Async_submission_authority.prepare` performs only the occurrence
compare-and-set that gets or creates one `Submission_id` for the owning
invocation, then calls the resolver with that identity and the exact
`Invocation_attempt_reference.t`. The reference is minted only after both the
invocation-open and owning attempt-open facts commit and carries their exact
stream-qualified fact references; a bare invocation occurrence or invocation
without an owning attempt cannot become an async cause. The
resolver is the single owner of decoding the
runtime-specific request, incrementally sealing its canonical
`Submission_operation_source`, computing the typed request digest from that
root, committing the exact `Submission_intent`, and only then crossing the
backend preparation boundary. The authority never interprets generic JSON,
payload shape, or strings. A retry of the same invocation receives the same
submission identity, operation root, and typed
`Submission_request_digest.t`; no hidden ID is encoded into the generic
request value.
The generated async adapter owns the request and receipt durable codecs and
fails if the resolved revision returns a value that does not decode. Ordinary
application code cannot obtain this capability because `Execution_context`
is re-exported through a public signature that omits `Internal`.
`Async_submission_resolver.Internal.create` is implemented in the same
representation-owning compilation unit, allowing the later runtime-registry
unit to bind the checked Journal and construct the capability without a reverse
dependency or `Obj.magic`. Its private representation stores both the exact
application-runtime authority and checked Journal access.

`Execution_scope_services.t` is the acyclic process-local bundle containing the
application-runtime authority, finite-scope authority, checked Journal access,
shared CPU executor, shared recursive-execution admission, shared media
stabilizer, and scope-bound async resolver. It has no global lookup. Direct
Agents, child Agents, and operation runners receive this exact value; both
authority tokens remain equality witnesses rather than service locators.
`Execution_scope_services.Internal.create` rejects an async resolver bound to a
different checked Journal authority or application-runtime authority.
`Execution_context.Internal.create` consumes the exact committed
`Invocation_attempt_reference.t` and active `Execution_admission.slot`; it
compares the authority's Journal/resolver, attempt owner, slot owner, switch,
and runtime-service token with its explicit arguments and returns a
construction error on any cross-wiring.

`Child_agent_run_authority.t` is another process-local capability, created only
after the current executable attempt has opened. Its private representation
binds that exact committed owning attempt, its active execution-admission slot,
exact runtime services, and the exact `Execution_context.switch` together. It
has no
public constructor or accessor;
its representation-owning module compiles after the Journal access and before
the execution algebra, exposes `Internal` only to the private runtime, and the
later Agent integration layer consumes the bound `view`. Consequently no
Agent adapter can pair a parent attempt from one occurrence with a detached
slot, switch, or different Journal authority.

`Execution_journal_access.Internal.create` is callable only from the
representation-owning Journal implementation and proves that reader and writer
name the same store authority and namespace. `claim_scope ~sw` installs one
process-local exclusive owner keyed by that underlying authority, not by
physical equality of the access wrapper, until that switch releases it.
Creating a second `Execution_scope.t` over the same Journal authority while
the first claim is live is a typed construction failure, so duplicate lane
writers and repair supervisors cannot be hidden behind two access or scope
values. This claim has no ownership relation to the shared CPU executor.

`Cpu_executor.t` is the one application-runtime capability over a reusable
`Eio.Executor_pool`. Its worker and admission capacities are explicit positive
owner inputs. The `Cpu` runner is structurally a full-core, non-yielding OCaml
job; it does not expose fractional weight. The executor submits it with Eio's
documented full-worker weight. Supporting a genuinely fractional offload would
require a separate typed runner contract with explicit utilization semantics;
it cannot be guessed from a float on a CPU-only callback. `submit` acquires one
non-blocking admission token before calling `Eio.Executor_pool.submit` from a
runtime-owned waiter fiber. That
waiter, its result promise, and its admission token are owned by the
application-lifetime CPU executor switch, not the submitting Tool switch. The
caller waits on the promise. Closing or cancelling the caller switch marks the
job's `Cpu_cancellation.t` and may end the caller wait, but the bounded waiter
retains admission until the pool job actually returns or fails and then
settles/discards the result exactly once. At most the explicit admission
capacity of these waiters can exist, so queued plus running CPU jobs remain
bounded; no dispatcher can drain a bounded front queue into unbounded
pool-waiting fibers. Cancellation before body entry marks the bounded job
cancelled, so its body is skipped when dequeued. A running job receives
`Cpu_cancellation.t` and must check it at its declared cooperative checkpoints;
OCaml domains are not forcibly preempted. If a runner ignores cancellation, its
worker and admission remain occupied until that runner returns. The immutable
`Cpu_executor.snapshot` reports exact non-negative admitted, queued, running,
and cancellation-requested counts plus a monotonic reentrant-inline
observation; those counts are observation only and never admission or
termination policy.

There is one closed structural exception to fresh pool submission:
`Reentrant_current_worker`. A call made from a job already running in this same
`Cpu_executor` runs inline on that worker with the current job's
`Cpu_cancellation.t`. It acquires no second admission token, creates no waiter,
and does not increment queued, running, or admitted; it increments only
`reentrant_inline`. This preserves the merged full-pool nested-submit
deadlock prevention without weakening admission. A call from any other domain
is `Fresh_submission` and follows the admission/pool path above. Detection uses
an executor-owned domain-local capability, not call-stack text, Tool identity,
elapsed time, or a catch-and-inline fallback.

For fresh submissions the invariant is
`admitted = queued + running`,
`admitted <= cpu_admission_capacity`,
`running <= cpu_workers`, and
`cancellation_requested <= admitted`. Thus admission capacity bounds the total
accepted jobs, not only the waiting queue; there is no alternate
“workers plus a separate waiting limit” interpretation. Capacity or worker failure is
typed; an Eio server domain never waits for queue space. CPU runners receive no
`Execution_context.t`, so code that performs Eio I/O cannot be silently moved
to a domain by runtime guesswork.

`Cpu_executor.Internal.close_and_await` first closes admission, settles every
admitted waiter, joins every pool worker through the executor-owned switch, and
returns all shutdown failures as one typed error. It is idempotent. Submission
after close is a typed `Cpu_execution_error`; runtime shutdown never relies on
parent-switch cancellation being reported as successful drain.

## 3. Typed executable algebra

### 3.1 Stable binding

An executable binding contains everything required to run and recover one
version of a typed executable:

```ocaml
module Invocation_terminal_preparation : sig
  type t
end

module Executable : sig
  module Id = Executable_reference.Id
  module Revision = Executable_reference.Revision

  type ('input, 'output, 'failure) t
  type ('input, 'output, 'failure) binding

  type reference = Executable_reference.t
  type exposure_reference = Tool_exposure_reference.t

  type call

  type ('output, 'failure) run_result = private
    | Domain_succeeded of
        { output : 'output
        ; after_commit : Commit_action.t option
        }
    | Domain_failed of 'failure
    | Terminal_prepared of Invocation_terminal_preparation.t
    | Infrastructure_failed of Infrastructure_error.t

  type ('input, 'output, 'failure) runner =
    | Cooperative of
        (Execution_context.t
         -> 'input
         -> ('output, 'failure) run_result)
    | Cpu of
        (Cpu_cancellation.t
         -> 'input
         -> ('output, 'failure) run_result)

  val domain_succeeded : 'output -> ('output, 'failure) run_result
  val domain_failed : 'failure -> ('output, 'failure) run_result

  type outcome =
    | Succeeded :
        { invocation_id : Execution_identity.Invocation_id.t
        ; binding : ('input, 'output, 'failure) binding
        ; exposure : exposure_reference option
        ; output : 'output
        ; after_commit : Commit_action.t option
        }
        -> outcome
    | Failed :
        { invocation_id : Execution_identity.Invocation_id.t
        ; binding : ('input, 'output, 'failure) binding
        ; exposure : exposure_reference option
        ; failure : 'failure
        }
        -> outcome

  val create
    :  runner:('input, 'output, 'failure) runner
    -> ('input, 'output, 'failure) t

  val bind
    :  id:Id.t
    -> revision:Revision.t
    -> input:'input Durable_codec.t
    -> output:'output Durable_codec.t
    -> failure:'failure Durable_codec.t
    -> recovery:('input, 'output, 'failure) Recovery_policy.t
    -> executable:('input, 'output, 'failure) t
    -> ('input, 'output, 'failure) binding

  val reference : (_, _, _) binding -> reference

  module Internal : sig
    val infrastructure_failed
      :  Infrastructure_error.t
      -> ('output, 'failure) run_result

    val domain_succeeded_after_commit
      :  output:'output
      -> Commit_action.t
      -> ('output, 'failure) run_result

    val terminal_prepared
      :  Invocation_terminal_preparation.t
      -> ('output, 'failure) run_result
  end
end
```

`Revision.t` identifies the executable contract and durable codec version, not
a deployment version guessed from source control. Duplicate `(Id, Revision)`
registration is rejected.
Runner placement (`Cooperative` or `Cpu`) is part of that revisioned contract;
changing it requires a new executable revision.

A recovered call is dispatched only to the exact registered binding revision.
An unavailable revision is a typed recovery failure; OAS does not silently run
the newest revision.

`Domain_failed` is an ordinary executable outcome. `Infrastructure_failed`
retains the outer fail-closed path used by recursive composites. Reserved
runtime exceptions and Eio cancellation are recorded and then retain their
exception semantics; an application handler cannot relabel them as a declared
domain failure. The variant is private: public callers construct only domain
success/failure through the two functions above; the Dune-private recursive
runtime owns infrastructure construction. `Internal` is implemented as a
submodule of the same `executable.ml` compilation unit that owns the concrete
variant, so it can construct the private cases without violating OCaml private
constructor rules. The public `Agent_sdk` signature re-exports `Executable`
through a narrower signature that omits `Internal`.

`Executable_internal.run_registered` executes `Cooperative` in the current Eio
fiber and submits `Cpu` through `Execution_context.Internal.cpu_executor`.
Queue refusal and worker failure become typed infrastructure failures; declared
domain results keep their original type. A CPU runner cannot call Eio through
the runner signature, and a cooperative runner is never silently offloaded.

### 3.2 Exact executable registry

Stable executable identity is owned by a registry distinct from Tool exposure:

```ocaml
module Executable_registry : sig
  type t

  type ('input, 'output, 'failure) registered

  type packed =
    | Registered :
        ('input, 'output, 'failure) registered
        -> packed

  val empty : t

  val register
    :  t
    -> ('input, 'output, 'failure) Executable.binding
    -> ((t * ('input, 'output, 'failure) registered),
        Registration_error.t)
       result

  val merge
    :  t
    -> t
    -> (t, Registration_error.t) result

  val reference
    :  (_, _, _) registered
    -> Executable.reference

  val fragment
    :  (_, _, _) registered
    -> t

  val call
    :  ('input, 'output, 'failure) registered
    -> 'input
    -> Executable.call

  val find
    :  t
    -> Executable.reference
    -> (packed, Lookup_error.t) result
end
```

Registration is immutable and rejects a second binding with the same
`(Executable.Id, Revision)`. A `registered` value is a private typed witness
minted only by that registry. Tool exposure and programmatic calls accept this
witness, not an independently constructed binding carrying the same strings.
Registry merge preserves an already shared witness and rejects two independently
created witnesses under one stable key; it never compares handler closures.
The private witness carries a process-local capability token used only for this
exact equality check. That token is not durable identity and is never serialized
or used for recovery lookup.
`fragment` returns the minimal immutable registry fragment containing that
witness; Tool construction carries the fragment to the single catalog
finalization step.

Multiple Tool exposures may reference the same `registered` executable.
Recovery, the synchronous runtime, and the asynchronous runtime all receive the
same registry value.

### 3.3 Tool exposure is also existential

`Tool.t` becomes an abstract existential package. Its untyped public record and
`handler_kind` disappear.

```ocaml
module Execution_mode : sig
  type t =
    | Serial
    | Concurrent
end

module Provider_sibling_schedule : sig
  type t =
    | Must_serialize
    | May_overlap
end

module Tool_input_codec : sig
  type 'a t

  val create
    :  schema:'a Json_schema.Codec.t
    -> durable:'a Durable_codec.t
    -> 'a t
end

module Tool_success_projection : sig
  type 'a t

  val pure
    :  ('a -> (Tool_output.t, Projection_error.t) result)
    -> 'a t

  val apply
    :  'a t
    -> Projection_context.t
    -> 'a
    -> (Tool_output.t, Projection_error.t) result

  module Internal : sig
    val paged
      :  (Projection_context.t
          -> 'a
          -> (Tool_output.t, Projection_error.t) result)
      -> 'a t
  end
end

module Tool_failure_projection : sig
  type 'a t

  val pure
    :  ('a -> (Tool_failure.t, Projection_error.t) result)
    -> 'a t

  val apply
    :  'a t
    -> Projection_context.t
    -> 'a
    -> (Tool_failure.t, Projection_error.t) result

  module Internal : sig
    val paged
      :  (Projection_context.t
          -> 'a
          -> (Tool_failure.t, Projection_error.t) result)
      -> 'a t
  end
end

module Tool : sig
  module Id = Execution_identity.Tool_id
  module Revision = Execution_identity.Tool_revision

  type t

  val create
    :  executable_id:Executable.Id.t
    -> executable_revision:Executable.Revision.t
    -> tool_id:Id.t
    -> tool_revision:Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> input:'input Tool_input_codec.t
    -> output:'output Durable_codec.t
    -> failure:'failure Durable_codec.t
    -> recovery:('input, 'output, 'failure) Recovery_policy.t
    -> run:
         (Execution_context.t
          -> 'input
          -> ('output, 'failure) result)
    -> disclose_success:
         ('output -> (Tool_output.t, Projection_error.t) result)
    -> disclose_failure:
         ('failure -> (Tool_failure.t, Projection_error.t) result)
    -> unit
    -> (t, Construction_error.t) result

  val create_cpu
    :  executable_id:Executable.Id.t
    -> executable_revision:Executable.Revision.t
    -> tool_id:Id.t
    -> tool_revision:Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> input:'input Tool_input_codec.t
    -> output:'output Durable_codec.t
    -> failure:'failure Durable_codec.t
    -> recovery:('input, 'output, 'failure) Recovery_policy.t
    -> run:
         (Cpu_cancellation.t
          -> 'input
          -> ('output, 'failure) result)
    -> disclose_success:
         ('output -> (Tool_output.t, Projection_error.t) result)
    -> disclose_failure:
         ('failure -> (Tool_failure.t, Projection_error.t) result)
    -> unit
    -> (t, Construction_error.t) result

  val reexpose
    :  source:t
    -> id:Id.t
    -> revision:Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> t

  val expose
    :  id:Id.t
    -> revision:Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> input:'input Json_schema.Codec.t
    -> success_to_tool_output:
         ('output -> (Tool_output.t, Projection_error.t) result)
    -> failure_to_tool_error:
         ('failure -> (Tool_failure.t, Projection_error.t) result)
    -> executable:
         ('input, 'output, 'failure) Executable_registry.registered
    -> t
end
```

Collection construction is incremental rather than a whole-list overload:

```ocaml
module Tool_member_catalog : sig
  type builder
  type t
  type cursor

  type page =
    { members : Tool.t list
    ; next : cursor
    ; caught_up : bool
    }

  val begin_ : unit -> builder
  val append
    :  builder
    -> Tool.t
    -> (unit, Construction_error.t) result
  val seal : builder -> (t, Construction_error.t) result
  val count : t -> int64
  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
end
```

`Tool.batch` and `Tool.batch_async` list compatibility wrappers do not exist.
`Tool_batch.expose` and `Async_tool_batch.expose` consume this sealed catalog.
Append checks the exact Tool/executable witnesses immediately; seal freezes
one ordered `Tool_batch_members` definition source without retaining the
caller's list.

`Tool_input_codec.t` is one typed bundle containing the provider schema decoder
and the durable input codec for the same OCaml type. It does not permit a schema
for one type to be paired with another runner by later registry lookup.

The same executable may have multiple explicit Tool exposures. Therefore
`Executable.Id.t` and `Tool.Id.t` are not interchangeable.
An exposure revision binds its name, description, sibling schedule, schema
codec, success disclosure projection, failure projection, and registered
executable. Changing any of them requires a new exposure revision.

The binding's durable output codec is the sole stored success representation.
The Tool exposure owns a versioned pure disclosure projection from decoded
`'output` to provider-neutral `Tool_output.t`. That projection may redact,
summarize, or select multimodal content, but it cannot alter the committed
execution result and does not produce another durable authority.
For ordinary users `Tool.create` keeps the compact pure callback surface and
immediately wraps it in `Tool_success_projection.pure` or
`Tool_failure_projection.pure`. Dune-private adapters for Agent/media results
may install a paged projection that receives only `Projection_context.t`,
whose reader and read admission are bound to the same execution runtime.
Neither path receives a Journal writer or can mutate the committed native
result. Large nested output is therefore disclosed by references/pages without
forcing every external Tool author to manage Journal plumbing.

There is no `Typed_tool.to_untyped` bridge in the final design. Dispatch opens
the existential package and retains the type equality through decode, execute,
durable encode, hook observation, and ToolResult projection.

### 3.4 Exact catalog

The Tool catalog is an immutable exact-lookup map from
`(Tool.Id.t, Tool.Revision.t)` and declared active wire name to `Tool.t`.
Construction rejects:

- duplicate Tool `(Id, Revision)` key;
- duplicate provider-visible name in one exposure set;
- a schema/decoder mismatch, which is structurally impossible when
  `Json_schema.Codec.t` is used.

There is no last-writer-wins registration.
Older Tool revisions may remain registered for recovery without being included
in the active provider exposure set.
Every Tool reference must resolve in the supplied `Executable_registry`.
Several exposures may resolve to the same registered entry; the Tool catalog
does not become a second executable registry and never compares closures.

```ocaml
module Tool_catalog : sig
  type t
  type builder

  type built =
    { catalog : t
    ; executables : Executable_registry.t
    }

  val begin_ : unit -> builder
  val append
    :  builder
    -> Tool.t
    -> (unit, Construction_error.t) result
  val seal : builder -> (built, Construction_error.t) result

  val find
    :  t
    -> Tool.Id.t
    -> Tool.Revision.t
    -> (Tool.t, Lookup_error.t) result

  val find_active_wire_name
    :  t
    -> string
    -> (Tool.t, Lookup_error.t) result
end

module Tool_exposure_registry : sig
  type t

  val empty : t

  val extend_one
    :  t
    -> Tool.t
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Tool.Id.t
    -> Tool.Revision.t
    -> (Tool.t, Lookup_error.t) result
end

module Durable_call : sig
  type t

  val binding : t -> Executable.reference
  val exposure : t -> Executable.exposure_reference option
  val input : t -> Execution_value.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  binding:Executable.reference
      -> exposure:Executable.exposure_reference option
      -> input:Execution_value.t
      -> t
  end
end
```

`Tool_exposure_registry.t` is the immutable application-runtime authority for
exposure revisions. Extending it with an existing `(Tool.Id, Tool.Revision)`
is accepted only when the new value carries the same unforgeable construction
authority as the already registered `Tool.t`. Independently constructing a
second exposure under the same stable key is a conflict even when its rendered
name or schema bytes happen to match. Per-definition `Tool_catalog.t` chooses
the active wire names; it does not weaken this runtime-wide identity rule.

`Durable_call.t` is the total canonical envelope produced from the same
existential `Executable.call` that is executed. Its input is encoded by that
exact binding revision before the existential type is hidden. Decoding the
envelope never chooses a newer binding and never reconstructs a Tool from a
wire name.

The abstract public types have Dune-private eliminators used by the runtime:

```ocaml
module Executable_internal : sig
  type ('output, 'failure) registered_outcome =
    | Registered_succeeded of
        { invocation_id : Execution_identity.Invocation_id.t
        ; output : 'output
        ; after_commit : Commit_action.t option
        }
    | Registered_failed of
        { invocation_id : Execution_identity.Invocation_id.t
        ; failure : 'failure
        }

  type call_view =
    | Call :
        { executable :
            ('input, 'output, 'failure) Executable_registry.registered
        ; exposure : Executable.exposure_reference option
        ; input : 'input
        }
        -> call_view

  val call_with_exposure
    :  exposure:Executable.exposure_reference
    -> ('input, 'output, 'failure) Executable_registry.registered
    -> 'input
    -> Executable.call

  val view_call : Executable.call -> call_view
  val encode_call
    :  Executable.call
    -> (Durable_call.t, Encode_error.t) result

  val decode_registered_input
    :  ('input, 'output, 'failure) Executable_registry.registered
    -> Execution_value.Decoder.t
    -> ('input, Decode_error.t) result

  val encode_registered_output
    :  ('input, 'output, 'failure) Executable_registry.registered
    -> into:Execution_value.Builder.t
    -> 'output
    -> (unit, Encode_error.t) result

  val encode_registered_failure
    :  ('input, 'output, 'failure) Executable_registry.registered
    -> into:Execution_value.Builder.t
    -> 'failure
    -> (unit, Encode_error.t) result

  val run_registered
    :  Execution_context.t
    -> ('input, 'output, 'failure) Executable_registry.registered
    -> 'input
    -> (('output, 'failure) registered_outcome,
        Infrastructure_error.t)
       result

  val run
    :  Execution_context.t
    -> Executable.call
    -> (Executable.outcome, Infrastructure_error.t) result
end

module Tool_decode_request : sig
  type t

  val exposure : t -> Executable.exposure_reference

  module Internal : sig
    val create
      :  context:Tool_decode_context.t
      -> tool:Tool.t
      -> (t, Tool_decode_binding_error.t) result
  end
end

module Tool_internal : sig
  type view =
    | Tool :
        { id : Tool.Id.t
        ; revision : Tool.Revision.t
        ; name : string
        ; sibling_schedule : Provider_sibling_schedule.t
        ; input : 'input Json_schema.Codec.t
        ; success_to_tool_output :
            'output Tool_success_projection.t
        ; failure_to_tool_error :
            'failure Tool_failure_projection.t
        ; executable :
            ('input, 'output, 'failure) Executable_registry.registered
        ; registry_fragment : Executable_registry.t
        ; async_runtime_dependencies : Async_runtime_reference.t list
        ; execution_runtime_dependencies :
            Execution_runtime_authority.t list
        }
        -> view

  type decoded =
    | Decoded :
        { exposure : Executable.exposure_reference
        ; executable :
            ('input, 'output, 'failure) Executable_registry.registered
        ; input : 'input
        ; success_to_tool_output :
            'output Tool_success_projection.t
        ; failure_to_tool_error :
            'failure Tool_failure_projection.t
        ; decode_lifetime : Tool_decode_lifetime.t
        }
        -> decoded

  type terminal_for_commit =
    | Success_ready of
        { binding : Executable.reference
        ; exposure : Executable.exposure_reference
        ; durable_output : Execution_value.prepared
        ; tool_output : Tool_output.t
        ; after_commit : Commit_action.t option
        }
    | Failure_ready of
        { binding : Executable.reference
        ; exposure : Executable.exposure_reference
        ; durable_failure : Execution_value.prepared
        ; tool_failure : Tool_failure.t
        }
    | Success_projection_failed of
        { binding : Executable.reference
        ; exposure : Executable.exposure_reference
        ; durable_output : Execution_value.prepared
        ; error : Projection_error.t
        }
    | Failure_projection_failed of
        { binding : Executable.reference
        ; exposure : Executable.exposure_reference
        ; durable_failure : Execution_value.prepared
        ; error : Projection_error.t
        }

  val view : Tool.t -> view

  val add_execution_runtime_dependency
    :  Tool.t
    -> Execution_runtime_authority.t
    -> Tool.t

  val decode
    :  Tool_decode_request.t
    -> (decoded, Decode_error.t) result

  val run_decoded
    :  Execution_context.t
    -> decoded
    -> (terminal_for_commit, Infrastructure_error.t) result
end

```

These modules are not re-exported from `Agent_sdk`. Catalog, batch generation,
and execution live in the same private library layer and do not expose a
general caller-controlled unpacking capability.

`async_runtime_dependencies` is immutable construction metadata, not a runtime
lookup by Tool name. Ordinary Tools carry `[]`, an async adapter carries its
exact runtime reference, and a composite carries the exact deduplicated union
of its members. Agent finalization validates every reference before exposing
the catalog. The private async client accepts only the adapter-owned
`Async_submission_authority.t`; `Execution_context`'s public signature exposes
no conversion to that authority. Therefore `[]` means that asynchronous
submission is structurally unavailable, not merely unchecked at runtime.
`execution_runtime_dependencies` is the analogous process-local authority set.
Only higher adapters such as Agent-as-Tool attach it through the Dune-private
helper, and re-exposure/composition preserves its exact union. It is never
serialized or used as durable recovery identity.

`Tool_decode_request` binds one `Tool.t` to the invocation's exact executable
binding and exposure before decoding. `Tool_internal.decode` consumes only that
request and opens the Tool existential once; there is no overload accepting an
independent `Tool.t` or codec. Its successful existential owns the decode
lifetime. `run_decoded` keeps the same registered witness adjacent to the
decoded input, durable codecs, and both disclosure functions through every
input consumer, then releases that lifetime exactly once before returning
`terminal_for_commit`. A second run of the same decoded token fails typed. It
never converts an exposure ID comparison into an OCaml type equality. The generic
`Executable.outcome` path is therefore not used to recover a Tool disclosure
projector after the witness has been erased.

A success or failure disclosure failure occurs after the corresponding native
domain result is known. The runtime commits `durable_output` or
`durable_failure` as the executable terminal and commits an infrastructure
ToolResult carrying `Projection_error.t`; it does not erase or reclassify the
native result. Both disclosure callbacks use result-bearing exception
firewalls: a non-cancellation exception becomes the typed projection error,
while `Eio.Cancel.Cancelled` retains cancellation semantics. In a collection,
already settled child facts stay in the Journal and the parent follows the
outer infrastructure-failure path.

### 3.5 Ordinary public façade

The algebra above is the implementation contract, not the minimum user
experience. Ordinary users define a typed Tool in one call and keep the
existing simple `Agent_sdk.Builder.with_tool` (`Builder.with_tool`) shape:

```ocaml
let make_weather_agent ~sw ~runtime ~journal ~net ~binding =
  let* weather =
    Tool.create
      ~executable_id:Weather.executable_id
      ~executable_revision:Weather.executable_revision
      ~tool_id:Weather.tool_id
      ~tool_revision:Weather.tool_revision
      ~name:"weather"
      ~description:"Read current weather"
      ~sibling_schedule:Provider_sibling_schedule.Must_serialize
      ~input:Weather.input
      ~output:Weather.output
      ~failure:Weather.failure
      ~recovery:Recovery_policy.Replay_safe
      ~run:Weather.run
      ~disclose_success:Weather.disclose_success
      ~disclose_failure:Weather.disclose_failure
      ()
  in
  let builder =
    Builder.create ~net ~binding
    |> Builder.with_tool weather
  in
  Execution_scope.build_direct
    ~sw
    ~runtime
    ~journal
    ~async_runtimes:[]
    builder
```

`Tool.create` constructs the binding, private registry witness, and exposure as
one checked definition. `Tool.reexpose` creates another exposure from the same
private witness while retaining the exact input codec and disclosure functions;
changing any typed component requires another checked `Tool.create`. Agent
construction merges those witnesses into one immutable registry and rejects
conflicting stable keys. `Execution_scope.build_direct` is the ordinary
one-call façade after application-runtime bootstrap; it performs the same
definition finalization, finite-scope construction/readiness, and fresh
instantiation as the advanced path, returning both scope and Agent.
`Tool.create` is the cooperative Eio runner and receives
`Execution_context.t`. `Tool.create_cpu` is the explicit CPU-only constructor;
its runner receives no Eio context and is submitted through the application
runtime's bounded reusable executor. OAS never chooses placement from elapsed
time, Tool name, input size, or observed CPU usage.
The façade maps the handler's ordinary `Ok`/`Error` result to the two public
domain constructors; it does not expose the infrastructure constructor.

Advanced callers may construct an explicit registry or operation backend.
Ordinary Tool and Agent users do not receive Journal writers, WAL handles,
backend reconciliation values, raw executor pools, or existential eliminators.
`Tool_catalog.build` is the single per-definition finalization point used by
`Execution_runtime.build_agent`:
it merges every private registry fragment, validates every exposure against the
merged registry, and returns the catalog and registry together.

### 3.6 Agent adapter is a typed executable, not another dispatcher

An Agent is exposed through the same algebra. The adapter does not retain a
mutable `Agent.t`, accept a free-form runner closure, or reduce input and output
to strings:

```ocaml
module Agent_input : sig
  type prepared
  type t

  val value : t -> Execution_value.t

  module Builder : sig
    type t

    val append_text
      :  t
      -> Execution_value.Content.staged Execution_value.Content.t
      -> (unit, Agent_input_error.t) result

    val append_media
      :  t
      -> kind:Execution_value.Media_kind.t
      -> media_type:
           (Execution_value.Content.staged Execution_value.Content.t)
      -> source:
           [ `Inline of
               Execution_value.Content.staged Execution_value.Content.t
           | `Blob of Blob_ref.t
           ]
      -> (unit, Agent_input_error.t) result

    val seal : t -> (prepared, Agent_input_error.t) result
    val abort : t -> (unit, Agent_input_error.t) result
  end

  val begin_
    :  sw:Eio.Switch.t
    -> Execution_value_authority.t
    -> (Builder.t, Agent_input_error.t) result

  module Internal : sig
    val begin_
      :  sw:Eio.Switch.t
      -> transaction:
           (Execution_manifest_staging.open_
              Execution_manifest_staging.transaction)
      -> id:Execution_identity.Manifest_id.t
      -> (Builder.t, Agent_input_error.t) result

    val codec : t Tool_input_codec.t
  end
end

module Agent_response : sig
  type t

  val agent_run : t -> Agent_run.Id.t
  val terminal_fact : t -> Execution_fact_ref.t
  val selected_attempt_fact : t -> Execution_fact_ref.t
  val item_manifest
    :  t
    -> (Execution_manifest_purpose.executable_results,
        Execution_manifest.committed)
         Execution_manifest.t

  module Internal : sig
    val of_committed_terminal
      :  agent_run:Agent_run.Id.t
      -> terminal_fact:Execution_fact_ref.t
      -> selected_attempt_fact:Execution_fact_ref.t
      -> item_manifest:
           ((Execution_manifest_purpose.executable_results,
             Execution_manifest.committed)
              Execution_manifest.t)
      -> (t, Agent_response_error.t) result

    val codec : t Durable_codec.t
    val disclose
      :  Projection_context.t
      -> t
      -> (Tool_output.t, Projection_error.t) result
  end
end

module Execution_durability_requirement : sig
  type reason =
    | Async_submission of Async_runtime_reference.t
    | External_effect of Executable.reference
    | Resumable_composite of Executable.reference

  type t =
    | Volatile_safe
    | Durable_required of reason list
end

module Agent_prelude : sig
  type role =
    | System
    | User
    | Application_context

  type builder
  type prepared
  type t

  val begin_
    :  sw:Eio.Switch.t
    -> Agent_prelude_authority.t
    -> (builder, Agent_prelude_error.t) result

  val append
    :  builder
    -> role:role
    -> content:Execution_value.prepared
    -> (unit, Agent_prelude_error.t) result

  val value_authority : builder -> Execution_value_authority.t
  val seal : builder -> (t, Agent_prelude_error.t) result
  val abort : builder -> (unit, Agent_prelude_error.t) result
  val digest : t -> Execution_manifest_digest.t

  module Internal : sig
    val stage_for_run
      :  transaction:
           (Execution_manifest_staging.open_
              Execution_manifest_staging.transaction)
      -> t
      -> (prepared, Agent_prelude_error.t) result
  end
end

module Agent_definition_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Agent_definition_manifest : sig
  type t

  val digest : t -> Agent_definition_digest.t

  module Internal : sig
    val stage_for_run
      :  transaction:
           (Execution_manifest_staging.open_
              Execution_manifest_staging.transaction)
      -> t
      -> ((Execution_manifest_purpose.agent_definition,
           Execution_manifest.staged)
            Execution_manifest.t,
          Agent_definition_error.t)
         result
  end
end

module Agent_run_open_preparation : sig
  type fresh
  type consumed
  type 'state t
  type receipt

  val prepare
    :  sw:Eio.Switch.t
    -> runtime:Execution_runtime_authority.t
    -> agent_run:Agent_run.Id.t
    -> definition:Agent_definition_manifest.t
    -> prelude:Agent_prelude.t option
    -> (fresh t, Agent_run_open_error.t) result

  val commit
    :  writer:Execution_journal.Writer.t
    -> fresh t
    -> ((consumed t * receipt),
        Agent_run_open_error.t)
       result

  val abort
    :  fresh t
    -> (unit, Agent_run_open_error.t) result
end

module Agent_checkpoint_frontier : sig
  type t

  val agent_run : t -> Agent_run.Id.t
  val definition : t -> Agent_definition_digest.t
  val stream : t -> Execution_stream_reference.t
  val observed_through : t -> Execution_page_cursor.t
  val frontier_manifest
    :  t
    -> (Execution_manifest_purpose.agent_checkpoint,
        Execution_manifest.committed)
         Execution_manifest.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Builder : sig
  type t

  val create
    :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> binding:Provider_binding.t
    -> t

  val with_tool : Tool.t -> t -> t
  val with_provider_binding : Provider_binding.t -> t -> t

  module Resource : sig
    type shared
    type per_run

    val network : _ Eio.Net.t -> shared
    val model_catalog : Model_catalog.t -> shared
    val prelude : Agent_prelude.t -> per_run
  end

  module Internal : sig
    type finalized
    type view =
      { primary_provider : Provider_binding.reference
      ; provider_bindings : Provider_binding.t list
      ; tools : Tool_catalog.t
      ; shared : Resource.shared list
      ; per_run : Resource.per_run list
      ; definition_manifest : Agent_definition_manifest.t
      ; durability_requirement : Execution_durability_requirement.t
      }

    val finalize
      :  t
      -> (finalized, Construction_error.t) result

    val view : finalized -> view
  end
end

module Agent : sig
  type definition
  type t
  type detailed_error = Provider_failure_attribution.detailed_error

  val conversation_selection_authority
    :  t
    -> Conversation_selection_authority.t

  val definition_digest : definition -> Agent_definition_digest.t

  module Internal : sig
    type checkpoint = Agent_checkpoint_frontier.t

    type definition_view =
      { runtime : Execution_runtime_authority.t
      ; finalized : Builder.Internal.finalized
      ; primary_provider : Provider_binding.t
      ; provider_bindings : Provider_binding_registry.t
      ; provider_adapters : Provider_continuation_registry.t
      ; catalog : Tool_catalog.t
      ; executables : Executable_registry.t
      ; definition_manifest : Agent_definition_manifest.t
      ; durability_requirement : Execution_durability_requirement.t
      }

    val create_definition
      :  runtime:Execution_runtime_authority.t
      -> finalized:Builder.Internal.finalized
      -> primary_provider:Provider_binding.t
      -> provider_bindings:Provider_binding_registry.t
      -> provider_adapters:Provider_continuation_registry.t
      -> catalog:Tool_catalog.t
      -> executables:Executable_registry.t
      -> definition_manifest:Agent_definition_manifest.t
      -> durability_requirement:Execution_durability_requirement.t
      -> definition

    val view_definition : definition -> definition_view
    val runtime_authority : definition -> Execution_runtime_authority.t

    val instantiate
      :  services:Execution_scope_services.t
      -> definition
      -> (t, Construction_error.t) result

    val resume
      :  services:Execution_scope_services.t
      -> definition
      -> checkpoint
      -> (t, Construction_error.t) result

    val encode_checkpoint : checkpoint -> Canonical_json.t
    val decode_checkpoint
      :  Canonical_json.t
      -> (checkpoint, Decode_error.t) result
    val snapshot_checkpoint
      :  t
      -> (checkpoint, Agent_checkpoint_error.t) result

    val failure_codec : detailed_error Durable_codec.t
    val disclose_failure
      :  detailed_error
      -> (Tool_failure.t, Projection_error.t) result
  end
end

module Agent_tool : sig
  val create
    :  executable_id:Executable.Id.t
    -> executable_revision:Executable.Revision.t
    -> tool_id:Tool.Id.t
    -> tool_revision:Tool.Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> agent:Agent.definition
    -> unit
    -> (Tool.t, Construction_error.t) result
end

module Agent_child_recovery : sig
  type key = Invocation_attempt_reference.t

  type terminal =
    | Succeeded of Agent_response.t
    | Declared_failed of Agent.detailed_error
    | Cancelled of Cancellation.cause
    | Infrastructure_failed of Infrastructure_error.t

  type existing =
    | Open of
        { agent_run : Execution_identity.Agent_run_id.t
        ; checkpoint : Agent.Internal.checkpoint
        ; checkpoint_fact : Execution_fact_ref.t
        }
    | Terminal of
        { agent_run : Execution_identity.Agent_run_id.t
        ; terminal_fact : Execution_fact_ref.t
        ; terminal : terminal
        }

  type state =
    | Absent
    | Existing of existing

  type create_result =
    | Created of
        { agent_run : Execution_identity.Agent_run_id.t
        ; checkpoint_fact : Execution_fact_ref.t
        }
    | Existing of existing

  val inspect
    :  Execution_journal.Reader.t
    -> key
    -> (state, Read_error.t) result

  val create_if_absent
    :  Execution_journal.Writer.t
    -> key
    -> initial_checkpoint:Agent.Internal.checkpoint
    -> (create_result, Journal_commit_error.t) result

  val append_checkpoint
    :  Execution_journal.Writer.t
    -> key:key
    -> agent_run:Execution_identity.Agent_run_id.t
    -> expected_previous:Execution_fact_ref.t option
    -> Agent.Internal.checkpoint
    -> (Execution_fact_ref.t, Journal_commit_error.t) result

  val commit_terminal
    :  Execution_journal.Writer.t
    -> key:key
    -> agent_run:Execution_identity.Agent_run_id.t
    -> expected_checkpoint:Execution_fact_ref.t option
    -> terminal
    -> (Execution_fact_ref.t, Journal_commit_error.t) result
end

module Agent_child_runtime : sig
  type authority = Child_agent_run_authority.t

  val of_context : Execution_context.t -> authority

  val start_or_resume
    :  authority
    -> definition:Agent.definition
    -> input:Agent_input.t
    -> ((Agent_response.t, Agent.detailed_error) result,
        Infrastructure_error.t)
       result
end

```

`Agent.definition` is the immutable result of application-runtime Builder
finalization. It contains finalized configuration, the initial
conversation/context snapshot, exact primary provider-binding reference, Tool catalog, and
executable registry. It contains no current messages, turn count, usage,
lifecycle, live `Eio.Switch.t`, per-run callback state, or second journal
writer. `Builder.Resource` is a closed construction algebra: public Builder
functions such as `create ~net ~binding`, `with_provider_binding`, and
`with_tool` place each accepted
value into a specific private shared-capability or per-run descriptor; ordinary
users do not wrap them manually. Finalization never attempts to introspect an
arbitrary OCaml closure or guess whether a captured value is mutable.
It also derives one closed durability requirement from exact Tool metadata.
Any async submission dependency, `External_effect`, or
`Journal_resumable_composite` binding contributes its typed reason. A volatile
scope is legal only for `Volatile_safe`; callers cannot waive or hide a reason.
The primary binding is automatically included in the closed binding set.
Fallback candidates are added explicitly as `Provider_binding.t` values; a
`Model_catalog.t` entry may reference only one of those exact binding
revisions. A loose provider configuration plus model string cannot compete
with this binding authority.
The definition manifest is the single durable description of that immutable
graph. Its domain-separated digest binds the exact primary and ordered
fallback binding references, provider-adapter revisions, Tool exposure
catalog root, executable registry root, prelude digest, durability reasons,
and every typed shared/per-run configuration reference. It never hashes an
OCaml closure address, physical endpoint path, environment snapshot, or
rendered description. Reordering or changing any bound revision changes the
digest. Checkpoints store only that digest plus bounded frontier/root
references and a Journal high-water; they never copy the cumulative
conversation or definition manifest at every turn.
`Builder.Internal.finalize`,
`Agent.Internal.create_definition/view_definition`, and
`Agent.Internal.instantiate/resume` are logical submodules of one Dune-private
`Agent_core` representation-sharing compilation unit and hidden from
`Agent_sdk`. `Builder.Internal.view` is the closed eliminator used by
`Execution_runtime.build_agent`; it cannot expose a second Tool registry
authority.
`Execution_runtime.build_agent` below is the only caller of the definition
constructor. There is no standalone `Builder.build_safe`: direct Agents and
Agent-as-Tool both pass through the same runtime authority, Journal access,
registries, and resource ownership.

Every adapter invocation creates a fresh `Agent.t` from that definition.
Concurrent or repeated calls therefore share the immutable definition and
stable executable revision, not mutable conversation state.
Each labeled public Builder entry converts its accepted value to the
corresponding closed `Builder.Resource` descriptor internally. Mutable conversation, provider stream,
event-writer, checkpoint sink, callback accumulator, and live switch values
have no shared constructor; their legacy capture methods are deleted or
replaced by typed per-run descriptors. Finalization validates the closed
descriptor graph and never introspects closure environments.

The legacy `Builder.Resource.initial_messages : Types.message list` surface is
deleted. An ordinary caller obtains the runtime's narrow
`Agent_prelude_authority`, incrementally appends only
`System | User | Application_context` plus canonical `Execution_value.t`, and
seals one bounded immutable `Agent_prelude.t`. The prelude builder exposes a
narrow value authority so every nested prepared value borrows the same
aggregate construction lease; it never waits for an independent child lease
while retaining earlier items. Seal copies the completed template into the
runtime-definition source, returns its content digest, and releases all
transient staging. The reusable `Agent.definition` and prelude therefore never
pin a staged manifest.

For each AgentRun, `Agent_run_open_preparation.prepare` materializes the exact
definition manifest, optional prelude, and initial input into one fresh
one-shot aggregate. `commit` publishes those roots with the AgentRun-open fact
or publishes nothing. It is consumable once; a second commit returns
`Already_consumed`. OAS commits each prelude entry through
`Conversation_input_writer` exactly once at AgentRun open, keyed by
`(agent_run, prelude_digest, ordinal)`. Every turn and every same-turn
Tool-continuation exchange automatically references that committed prelude
base and cannot omit, duplicate, or restage it. Resume loads those roots and
requires the exact definition/prelude digest; changing either requires a new
AgentRun rather than a repair or legacy fallback. Assistant/provider output,
Thinking, ToolUse, and ToolResult history cannot enter through this surface.
Importing existing provider history, when needed, is a separate checked
Journal-import operation which validates exact adapter/correlation facts; it
is not a Builder compatibility path.

The fixed `Agent_input` type can represent only user-authored `Text`, `Image`,
`Document`, and `Audio` blocks whose media is already `Inline` or content
addressed by `Blob_ref.t`. Provider URL and file-ID inputs must be ingested by
the caller's `Media_stabilizer.t` before `Agent_input.t` exists.
`Thinking`, `ReasoningDetails`, `RedactedThinking`, `ToolUse`, and
`ToolResult` are not constructors of the input type, so rejection is structural
rather than a string or runtime-kind check. Exact provider wire input already
belongs to the invocation fact; there is no duplicate `raw_input` correlation
field.

The child runtime never converts a complete legacy `Types.api_response` into
another response tree. Provider item finalization stabilizes every URL/file-ID
media item before its success root commits. `Agent_response.t` is then a small
opaque reference manifest containing the child AgentRun terminal fact, exact
selected-attempt fact, and committed finalized-item manifest. Its durable codec
encodes those references, not a second copy of text/reasoning/multimodal
payload. `disclose` pages the referenced provider-neutral semantic content and
builds a Tool projection from the same content references. Missing media,
selection, item, digest, or adapter revision is an infrastructure projection
failure before the parent terminal; it never falls back to a reconstructed
string. Nested Agent[], including 131K-class outputs, therefore copy bounded
metadata rather than O(history) response bytes.
`Agent_tool.create` copies the definition's process-local runtime authority
into the Tool's private construction dependencies. A parent definition can be
finalized only by that same runtime; cross-runtime nesting fails construction
rather than discovering a different Journal/pool/backend authority after the
child has started.

`Agent_child_runtime` is Dune-private. `of_context` retrieves the already-bound
authority; `start_or_resume` accepts neither a switch nor a parent ID and first
verifies that the definition's execution-runtime authority equals the
context's. It reads the Journal by the owning invocation and attempt before
allocating an occurrence. `Absent` atomically creates one `Agent_run_id`;
`Existing (Open ...)` resumes that same occurrence from its exact committed
Agent checkpoint and structural child state; `Existing (Terminal ...)` reuses
the recorded typed native result.
Before that compare-and-set, the adapter instantiates fresh state and obtains
its initial value through `Agent.Internal.snapshot_checkpoint`.
`Agent_child_recovery.create_if_absent` commits the new ID and that initial
checkpoint in one Journal compare-and-set on the closed key, so there is no
durable “created without checkpoint” state. Concurrent retries return
`Existing` and an open occurrence can never cause a second child ID. The
checkpoint contains exact provider binding,
continuation source keys, completed child identities, and the next legal
transition; it is not a serialized closure.
`append_checkpoint` and `commit_terminal` compare the exact previous checkpoint
event, so two resumptions cannot advance one AgentRun. `Agent.Internal.resume`
is the representation-owning checkpoint eliminator. Resume refuses a missing
checkpoint or unavailable exact provider/Tool revision as a typed recovery
failure rather than restarting the Agent.
At every durable turn boundary, the adapter snapshots the live Agent and
commits that checkpoint before beginning the next provider or Tool effect.

After start-or-resume, the adapter instantiates fresh per-run state and executes
the multimodal detailed Agent path with the authority's exact switch, runtime
services, and clock. Success commits the complete stable
`Agent_response.t`; declared failure commits the complete
`Agent.detailed_error`, including provider-failure attribution. Journal, codec,
media stabilization, and topology failures commit
`Infrastructure_failed`; cancellation commits `Cancelled` under a protected
cleanup region and then re-raises the original `Eio.Cancel.Cancelled` rather
than recasting it as an SDK failure. If that cancellation-terminal append also
fails, the exact append failure is published to the scope's typed failure
aggregate before the original cancellation propagates; the still-open durable
AgentRun remains recoverable rather than being reported closed. Every other
terminal-append failure returns a composite infrastructure error retaining both
the original outcome and append cause. Thus the parent can prove the structural
child terminal before closing, or itself remains nonterminal/recoverable.
This integration unit compiles after both Agent and Execution Journal. The
lower execution algebra refers only to abstract
`Child_agent_run_authority.t`, so Agent never becomes a reverse dependency of
`Executable` and no recursive module is required.

The adapter uses `Journal_resumable_composite` recovery and the fixed codecs
above. `Agent_response.Internal.codec/disclose` owns the response
representation and is the one versioned pure
projection from the stable native response to a validated `Tool_output.t`.
Assistant-visible text and supported multimodal resources may be disclosed;
reasoning and thinking remain typed child facts and are never copied into a
parent provider ToolResult. `Agent.Internal.failure_codec/disclose_failure`
preserves the structured failure attribution. Neither projector may replace
the native response with concatenated text or a summarizer. The parent attempt
cannot close while its structural child AgentRun is open.

```ocaml
let make_research_tool ~runtime ~child_builder =
  let* child_definition =
    Execution_runtime.build_agent runtime ~async_runtimes:[] child_builder
  in
  Agent_tool.create
    ~executable_id:Ids.research_agent
    ~executable_revision:Revisions.research_agent_v1
    ~tool_id:Ids.research_tool
    ~tool_revision:Revisions.research_tool_v1
    ~name:"research"
    ~description:"Run the research agent"
    ~sibling_schedule:Provider_sibling_schedule.May_overlap
    ~agent:child_definition
    ()
```

There are no Agent-array dispatch paths:

- Agent as Tool is `Agent_tool.create`;
- Agent array as Tool seals those Tools into a `Tool_member_catalog` and calls
  `Tool_batch.expose`;
- asynchronous Agent array as Tool calls `Async_tool_batch.expose` with the
  same sealed catalog.

The old public `agent_runner`, `raw_input`, `output_summarizer`, `create_simple`,
`create_typed`, `create_typed_untyped`, and `Typed_tool.to_untyped` paths are
deleted in the hard cut.

## 4. Invocation authority and context

`Invocation.context` is abstract and can only be created by the execution
runtime:

```ocaml
module Invocation : sig
  module Id = Execution_identity.Invocation_id

  type origin = Invocation_reference.origin =
    | Provider_tool_call of Provider_tool_call_reference.t
    | Programmatic_child
    | Async_operation of Execution_identity.Operation_id.t

  type reference = Invocation_reference.t
  type context = Execution_context.t

  val id : reference -> Id.t
  val origin : reference -> origin
  val reference : context -> reference
  val switch : context -> Eio.Switch.t
  val state : context -> Context.t
  val effects : context -> Effect_boundary.t
end
```

Only `reference` crosses a durable or asynchronous boundary. The private
`context` representation also carries the live switch, state, journal mutation
capability, typed effect boundary, and attempt ancestry. The raw journal
capability is not exposed to application code or retained by background
operations.

Passing the current structured-concurrency switch through this context is
mandatory. Capturing an unrelated switch in an Agent-as-Tool closure is
forbidden.

## 5. Recursive execution topology

The target journal topology is execution-native:

```text
AgentRun
└─ AgentTurn
   └─ ProviderExchange
      ├─ ProviderAttempt (retry/fallback candidates)
      └─ selected ProviderAttempt
         └─ ExecutableInvocation
            └─ ExecutableAttempt
               ├─ ExecutableInvocation
               │  └─ ExecutableAttempt
               ├─ AgentRun
               └─ ExecutableInvocation
                  └─ ExecutableAttempt
```

An invocation records:

- executable identity and revision;
- Tool exposure identity when one exists;
- exact provider wire input when the origin is a provider ToolUse;
- canonical decoded input after successful decode, or canonical programmatic
  input when no provider decode boundary exists;
- origin and parent/causal edges;
- schedule metadata;
- exact terminal success or declared failure.

`Provider_tool_call` carries the provider-owned call reference: source adapter
revision, provider attempt, finalized ToolUse source key, and its exact
grammar-specific native correlation.
For a provider-origin invocation,
`Invocation_reference.origin = Provider_tool_call call` is the sole full-call
and exposure authority. `Invocation_opened_fact.exposure` derives
`Provider_tool_call_reference.exposure call`, and `provider_wire` stores only
the schedule and raw arguments that are not in the call reference. It never
repeats the call or exposure. Programmatic origins carry their optional
exposure exactly once in the checked opened fact. Construction/decoding rejects
any provider-origin opened fact whose binding does not resolve from that one
exposure.

A blocked or decode-failed provider invocation therefore retains its exact
wire input and typed rejection without fabricating a decoded canonical input.

An attempt records:

- handler entry;
- progress;
- effect-entry and effect-receipt facts;
- cancellation and infrastructure failure.

Provider ToolUse/ToolResult values are projections attached to a provider
origin. Programmatic and nested calls store canonical execution input/result
without fabricating provider content.

The current private journal foundation must be reshaped so that:

- an attempt can own nested executable invocations and child Agent runs;
- a child cannot be attached directly to an invocation while bypassing its
  attempt;
- input/result snapshots are execution-native;
- provider correlation remains optional evidence;
- a parent cannot reach terminal state while structural synchronous children
  are open.

## 6. Synchronous one/many execution

```ocaml
module Executable_call_source : sig
  type builder
  type prepared
  type t
  type cursor

  type page =
    { calls : Durable_call.t list
    ; next : cursor
    ; caught_up : bool
    }

  val begin_
    :  sw:Eio.Switch.t
    -> Execution_context.t
    -> (builder, Executable_call_source_error.t) result
  val append
    :  builder
    -> Executable.call
    -> (unit, Executable_call_source_error.t) result
  val seal
    :  builder
    -> (prepared, Executable_call_source_error.t) result
  val abort
    :  builder
    -> (unit, Executable_call_source_error.t) result
  val beginning : t -> cursor
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
end

module Executable_result_source : sig
  type prepared
  type t
  type cursor

  type state =
    | Settled of
        { ordinal : int64
        ; invocation : Invocation.Id.t
        ; terminal_fact : Execution_fact_ref.t
        }
    | Running_unfinished of
        { ordinal : int64
        ; invocation : Invocation.Id.t
        ; opened_fact : Execution_fact_ref.t
        }
    | Not_started of { ordinal : int64 }

  type page =
    { states : state list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
end

module Executable_plan : sig
  type mode = Execution_mode.t =
    | Serial
    | Concurrent

  type execution_failure =
    { cause : Infrastructure_error.t
    ; states : Executable_result_source.t
    }

  type t =
    | One of Executable.call
    | Many of
        { mode : mode
        ; calls : Executable_call_source.prepared
        }

  module Internal : sig
    val run
      :  Invocation.context
      -> t
      -> (Executable_result_source.t, execution_failure) result
  end
end
```

`Executable_plan.Internal.run` and the context eliminator it consumes are
Dune-private. A cooperative Tool handler cannot capture an independently
registered executable and dynamically run it outside its definition's exact
registry, recovery, and durability graph. Public one/many composition uses the
incremental `Executable_call_source`/`Tool_member_catalog` builders followed by
`Tool_batch.expose`; asynchronous composition uses the same catalog plus
`Async_tool_batch.expose`. Those constructors compute the immutable member
registry and typed dependency union before Agent finalization. There is no
closure introspection and no runtime fallback from an undeclared child.

Result order is always input order. On infrastructure failure the result
source retains ordered `Settled`, `Running_unfinished`, and `Not_started`
records. Serial calls that never started have only an ordinal and no fabricated
invocation identity. The journal remains the authoritative detailed record;
the API does not erase partial completion.

### Serial

- Start one child after the previous child reaches a terminal domain outcome.
- Preserve exact input order.
- A declared child failure is an outcome; later children still run.

### Concurrent

- Borrow application-runtime `Execution_admission` leases before opening child
  fibers; do not materialize one waiting fiber per input member.
- Start at most the leased frontier under the invocation’s structured Eio
  switch, refill it as children settle, and preserve every not-yet-started
  ordinal durably.
- Every synchronous structural call, not only a composite, hands its active
  lease from the exact parent AgentRun/invocation-attempt owner to the exact
  child owner before awaiting that child. The old generation becomes unusable;
  child settlement returns a fresh-generation lease to the suspended parent.
  A composite transfers that lease to its first runnable descendant before
  requesting leases for additional siblings. A parent coordinator never owns
  a valid lease while waiting for descendants, so Agent→Tool, Tool→Tool,
  Agent→Agent, and nested composite calls cannot saturate the frontier with
  waiting parents.
- Join every declared child outcome.
- Preserve input order in the returned aggregate.
- A declared child failure does not cancel siblings.
- Parent cancellation cancels unfinished children.
- Reserved runtime exceptions and journal failure abort the structured scope.

Reserved runtime exceptions and Eio cancellation retain exception semantics
after the journal records the abort. They are not converted into ordinary
declared child failures. Lease transfer, release, or admission-close failure is
itself a typed infrastructure failure and cannot silently increase capacity.
The opaque admission implementation verifies the full owner and generation on
every `handoff`, `return`, and `release`; stale parent handles and a child
returning a sibling's lease are typed corruption. This mutable ownership cell
is resource authority only, never execution-domain or scheduling-policy SSOT.

Programmatic `Many []` returns an exact empty aggregate. A provider-visible
batch Tool uses `minItems: 1`, because an empty provider request carries no
call intent.

The plan-open fact commits only the ordered call-manifest root. It allocates no
child occurrence identities. The reducer mints an invocation identity in the
ordinal's `Member_started` CAS immediately before the child open fact, and
`Member_settled` links its exact terminal fact. Consequently an open-plan crash
has zero phantom children, a serial never-started ordinal has no identity, and
recovery cannot infer start state from array position or missing log text.

The existing Tool descriptor that controls whether sibling provider ToolUse
calls may overlap is a separate axis from this composite’s internal
`Serial | Concurrent` mode. The two modes must not share one field or be
inferred from each other.

The private Tool collection executor is typed separately:

```ocaml
module Tool_batch_argument_codec : sig
  type t

  val create
    :  members:Tool_member_catalog.t
    -> (t, Construction_error.t) result
  val schema : t -> Json_schema.t
  val decode_calls
    :  Tool_decode_context.t
    -> t
    -> (Executable_call_source.prepared, Decode_error.t) result
end

module Tool_plan_internal : sig
  type execution_failure =
    { cause : Infrastructure_error.t
    ; states : Executable_result_source.t
    }

  val run
    :  Invocation.context
    -> mode:Executable_plan.mode
    -> calls:Executable_call_source.prepared
    -> (Executable_result_source.t, execution_failure) result
end
```

The call manifest contains canonical `Durable_call.t` records, never erased
decoded OCaml packages. Each page resolves the exact executable/exposure
witness, decodes under that binding, executes, and appends only ordinal plus
committed lifecycle references to the result source. Each child therefore
retains its own input/output/failure witness through disclosure without a
whole heterogeneous heap list.
`Executable_plan` remains the provider-neutral programmatic API; Tool batches
do not try to reconstruct a member projector from its generic outcome.

## 7. Tool adaptation of collections

A collection Tool is generated from explicit member Tool cases:

```ocaml
module Tool_batch : sig
  val expose
    :  id:Tool.Id.t
    -> revision:Tool.Revision.t
    -> executable_id:Executable.Id.t
    -> executable_revision:Executable.Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> mode:Executable_plan.mode
    -> members:Tool_member_catalog.t
    -> (Tool.t, Construction_error.t) result
end
```

Construction rejects an empty member catalog and duplicate full exposure
identities `(Tool.Id, Tool.Revision)`.
The caller supplies the composite executable identity and revision. A revision
binds one immutable ordered member catalog and scheduling mode; changing either
requires a new revision. Recovery never decodes an old composite call against a
new member set.

Its input schema is a discriminated union:

```json
{
  "type": "object",
  "additionalProperties": false,
  "required": ["calls"],
  "properties": {
    "calls": {
      "type": "array",
      "minItems": 1,
      "items": {
        "oneOf": [
          {
            "type": "object",
            "additionalProperties": false,
            "required": ["tool_id", "tool_revision", "input"],
            "properties": {
              "tool_id": { "const": "member-a" },
              "tool_revision": { "const": "v1" },
              "input": { "...": "member-a schema" }
            }
          },
          {
            "type": "object",
            "additionalProperties": false,
            "required": ["tool_id", "tool_revision", "input"],
            "properties": {
              "tool_id": { "const": "member-b" },
              "tool_revision": { "const": "v3" },
              "input": { "...": "member-b schema" }
            }
          }
        ]
      }
    }
  }
}
```

`Tool_batch_argument_codec` builds its discriminated schema by paging the
sealed member catalog and its incremental case catalog. It does not call
`Json_schema.Codec.list`/`nonempty_list` or retain a `case list`. During
dispatch it pages canonical argument events, parses each opaque `tool_id` and
`tool_revision` once as one `Executable.exposure_reference`, performs exact
catalog lookup, invokes the paired typed decoder under the invocation's
explicit input-byte admission, and immediately appends the resulting durable
call to `Executable_call_source`. The complete array is never a
`Yojson.Safe.t`, OCaml call list, or heterogeneous decoded heap list.
Asynchronous batches page that prepared call source into
`Submission_operation_source` before submission preparation. The same exact
lookup rule applies; neither path selects a runner from free text nor repairs
malformed input.

The generated composite binding has one canonical aggregate result:

```ocaml
module Tool_batch_result : sig
  type t

  val mode : t -> Executable_plan.mode
  val calls : t -> Executable_call_source.t
  val results : t -> Executable_result_source.t
end
```

The paired manifests are ordered by input ordinal. The child invocation
identity is generated only when that ordinal actually starts and is the
correlation key used by hooks, journal readers, and dashboard projection.
Array position is ordering metadata, not identity. A bounded projector
lockstep-pages calls, terminal facts, and exact Tool witnesses to produce
provider aggregate pages; `Tool_batch_result.t` never stores the projected
items as a whole list.

The parent ToolResult is an ordered aggregate containing every child’s typed
success or declared failure. A declared child failure does not turn the parent
into an infrastructure failure.

Only `Tool_internal.Success_ready` and `Failure_ready` become projected member
results.
`Success_projection_failed` preserves the child's committed native success in
the exact terminal fact and takes the parent infrastructure-failure path;
it is never squeezed into a declared member failure.

The generated binding is always registered with
`Journal_resumable_composite`. Recovery reopens the existing parent invocation,
loads the persisted child plan, retains every child invocation identity and
terminal, and resumes only not-started or reconcilable incomplete children.
It never creates a new child for an already terminal index and never replays
the entire collection as a fresh parent attempt.

If the composite itself fails at the infrastructure boundary, its single
ToolResult is an infrastructure error that includes the exact settled indices
and running invocation identities plus never-started indices. It never claims
the entire batch failed before start and never drops already committed child
outcomes.

## 8. Hook contract

Every actual Tool invocation uses this lifecycle:

```text
invocation opened durably
PreToolUse
├─ Block
│  ├─ no attempt opens
│  ├─ blocked ToolResult commits durably
│  └─ provider projection may continue
└─ Continue
   ├─ decode provider input with the exposure's schema-bound codec
   ├─ Decode failure
   │  ├─ no attempt opens
   │  ├─ typed validation ToolResult commits durably
   │  ├─ PostToolUse observes the terminal result
   │  └─ PostToolUseFailure additionally observes it
   └─ Decode success
      ├─ attempt opened durably
      ├─ handler/composite executes
      ├─ native terminal, authoritative invocation result, and optional
      │  Commit_action_pending commit in one journal transaction
      ├─ the runtime executes or durably marks the commit action reconcilable
      └─ closed outcome matrix below selects the exact post observers
```

This deliberately makes the post hooks post-commit observers. The existing
pre-commit post-hook ordering must be replaced.

Hook context carries canonical identities:

```ocaml
type tool_hook_input_evidence =
  | Provider_wire of
      (Provider_tool_arguments.committed Provider_tool_arguments.t)
  | Canonical_input of Execution_value.t

type tool_hook_context =
  { invocation_id : Invocation.Id.t
  ; attempt_id : Attempt.Id.t option
  ; exposure : Executable.exposure_reference
  ; input_evidence : tool_hook_input_evidence
  ; decoded_input : Execution_value.t option
  ; schedule : Hooks.tool_schedule
  }
```

The runtime constructs `Provider_wire` only from a provider-origin invocation
and `Canonical_input` only from a programmatic/nested invocation. The checked
constructor rejects an origin/evidence mismatch. Hooks never fabricate an
empty provider object merely to satisfy their schema, and canonical decoded
input remains independently optional for a provider decode failure.

Hook implementations are deterministic, side-effect-free transformations over
that immutable context and already committed outcome. They receive no
`Eio.Switch`, network/filesystem handle, `Effect_boundary`, submission
authority, or arbitrary runtime service. `PreToolUse` may return only its typed
continue/block decision; post hooks return only typed observation metadata.
Any action that can escape the process, require retry/reconciliation, or alter
application state is modeled as an `Executable`, declared `Effect_protocol`,
or commit action with its own Journal lifecycle. OAS never reruns an
effectful callback while pretending it is an observer, and recovery never
guesses whether a hook side effect happened.

Normative post-observer matrix over the one provider-neutral
`Invocation_result.outcome` authority:

| Outcome | `PostToolUse` | `PostToolUseFailure` |
|---|---:|---:|
| `Succeeded` | once | no |
| `Declared_failure` | once | once, after `PostToolUse` |
| `Blocked` | once | once, after `PostToolUse` |
| `Invalid_input` | once | once, after `PostToolUse` |
| `Cancelled` | once | once, after `PostToolUse` |
| `Infrastructure_failed` | once | once, after `PostToolUse` |

There is no separate prose rule for decode, projection, cancellation, or
PreTool observer failure. Each path first commits exactly one row of this
closed outcome algebra, then uses the table. A PreTool observer infrastructure
failure is not `Blocked`; it commits `Infrastructure_failed` and follows that
row.

- One post-observer instance runs at most once for its exact
  `(invocation, phase, ordinal)`.
- One post-hook failure does not suppress later observers.
- Every observer failure is committed and linked to the invocation.
- A post-hook failure cannot rewrite, erase, or reclassify the committed Tool
  result.
- A commit action is runtime protocol work, not a hook. It runs only after the
  result transaction returns its actual publication event identity and before
  post observers. For provider origin that event is the ToolResult; for a
  programmatic or nested origin it is the corresponding generic result fact.
- Commit-action completion or reconciliation state is committed before post
  observers. Failure or reply loss cannot rewrite the ToolResult.
- Crash recovery enumerates `Commit_action_pending` facts by journal identity
  and resumes the exact idempotent action revision; it does not rediscover work
  from output text.
- A PreToolUse hook failure is not a Block and fails closed.
- The invocation lifecycle closes after all post observers settle; provider
  projection reads the already committed ToolResult and never treats a
  post-hook event as result content.

For a synchronous collection, parent and child hooks form a real hierarchy.
Concurrent child lifecycles may interleave, but each child retains a complete
identity and hook sequence.

## 9. Provider continuation law

```ocaml
module Provider_content_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_content_cursor : sig
  type t

  val beginning : t
end

module Provider_content : sig
  type staged
  type committed
  type 'lifecycle t

  type format =
    | Canonical_json
    | Utf8_text
    | Binary

  val digest : 'lifecycle t -> Provider_content_digest.t
  val byte_count : 'lifecycle t -> Byte_count.t
  val format : 'lifecycle t -> format

  type page =
    { bytes : Execution_value.Inline_bytes.t option
    ; next : Provider_content_cursor.t
    ; caught_up : bool
    }

  val read_committed
    :  Execution_journal.Reader.t
    -> committed t
    -> after:Provider_content_cursor.t
    -> max_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val encode_committed : committed t -> Canonical_json.t
  val decode_committed
    :  Canonical_json.t
    -> (committed t, Decode_error.t) result
end

module Provider_native_evidence : sig
  type completeness =
    | Complete
    | Capacity_exceeded of
        { limit : Positive_byte_count.t
        ; received_at_least : Byte_count.t
        }

  type decoder_phase =
    | Http_body
    | Content_decoding
    | Stream_framing
    | Utf8
    | Json
    | Wire_contract
    | Attempt_finalization

  type staged
  type committed
  type 'lifecycle t

  val attempt : 'lifecycle t -> Provider_attempt.Id.t
  val contract : 'lifecycle t -> Provider_wire_contract_reference.t
  val frame_sequence : 'lifecycle t -> int64 option
  val byte_offset : 'lifecycle t -> Byte_count.t
  val media_type : 'lifecycle t -> 'lifecycle Provider_content.t option
  val phase : 'lifecycle t -> decoder_phase
  val retained : 'lifecycle t -> 'lifecycle Provider_content.t
  val completeness : 'lifecycle t -> completeness

  module Internal : sig
    val create_staged
      :  attempt:Provider_attempt.Id.t
      -> contract:Provider_wire_contract_reference.t
      -> frame_sequence:int64 option
      -> byte_offset:Byte_count.t
      -> media_type:Provider_content.staged Provider_content.t option
      -> phase:decoder_phase
      -> retained:Provider_content.staged Provider_content.t
      -> completeness:completeness
      -> (staged t, Construction_error.t) result
  end
end

module Provider_protocol_error : sig
  type staged
  type committed
  type 'lifecycle t

  val evidence
    :  'lifecycle t
    -> 'lifecycle Provider_native_evidence.t
  val detail : 'lifecycle t -> Provider_protocol_failure.t

  module Internal : sig
    val create_staged
      :  evidence:Provider_native_evidence.staged Provider_native_evidence.t
      -> detail:Provider_protocol_failure.t
      -> staged t
  end
end

module Provider_capacity_failure : sig
  type resource =
    | Transport_read_chunk
    | Attempt_stream_bytes
    | Global_stream_bytes
    | Provider_spool_page_bytes
    | Provider_spool_attempt_bytes
    | Provider_spool_global_bytes
    | Evidence_storage

  type staged
  type committed
  type 'lifecycle t

  val resource : 'lifecycle t -> resource
  val evidence
    :  'lifecycle t
    -> 'lifecycle Provider_native_evidence.t

  module Internal : sig
    val create_staged
      :  resource:resource
      -> evidence:Provider_native_evidence.staged Provider_native_evidence.t
      -> staged t
  end
end

module Provider_stream_frame : sig
  type t
  type cursor

  type page =
    { bytes : Execution_value.Inline_bytes.t option
    ; next : cursor
    ; caught_up : bool
    }

  val attempt : t -> Provider_attempt.Id.t
  val contract : t -> Provider_wire_contract_reference.t
  val sequence : t -> int64
  val byte_count : t -> Byte_count.t
end

module Provider_stream_queue : sig
  type t
  type ingress
  type taken_frame
  type retained_frame

  type terminal =
    | End_of_stream
    | Cancelled of Cancellation.cause
    | Transport_failed of Provider_failure_attribution.detailed_error
    | Protocol_failed of
        Provider_protocol_error.staged Provider_protocol_error.t
    | Capacity_failed of
        Provider_capacity_failure.staged Provider_capacity_failure.t
    | Runtime_failed of Provider_runtime_error.t

  type item =
    | Frame of taken_frame
    | Terminal of terminal

  type consumer_failure =
    | Consumer_protocol_failed of
        Provider_protocol_error.staged Provider_protocol_error.t
    | Consumer_capacity_failed of
        Provider_capacity_failure.staged Provider_capacity_failure.t
    | Consumer_runtime_failed of Provider_runtime_error.t

  val frame : taken_frame -> Provider_stream_frame.t
    val retained_frame : retained_frame -> Provider_stream_frame.t

    val payload_beginning
      :  taken_frame
      -> Provider_stream_frame.cursor

    val retained_payload_beginning
      :  retained_frame
      -> Provider_stream_frame.cursor

    val read_payload
      :  taken_frame
      -> after:Provider_stream_frame.cursor
      -> max_bytes:Positive_byte_count.t
      -> (Provider_stream_frame.page, Provider_stream_queue_error.t) result

    val read_retained_payload
      :  retained_frame
      -> after:Provider_stream_frame.cursor
      -> max_bytes:Positive_byte_count.t
      -> (Provider_stream_frame.page, Provider_stream_queue_error.t) result

  module Internal : sig
    val create
      :  frame_capacity:Positive_int.t
      -> per_attempt_byte_capacity:Positive_byte_count.t
      -> attempt_bytes:Provider_stream_byte_admission.attempt_lease
      -> attempt:Provider_attempt.Id.t
      -> contract:Provider_wire_contract_reference.t
      -> t

    val begin_ingress
      :  t
      -> sequence:int64
      -> (ingress, Provider_stream_queue_error.t) result

    val reserve_and_append
      :  sw:Eio.Switch.t
      -> ingress
      -> source:string
      -> offset:int
      -> length:Positive_byte_count.t
      -> (unit, Provider_stream_queue_error.t) result

    val seal_and_push_or_await
      :  sw:Eio.Switch.t
      -> ingress
      -> (unit, Provider_stream_queue_error.t) result

    val abort_ingress
      :  ingress
      -> (unit, Provider_stream_queue_error.t) result

    val close_from_producer
      :  t
      -> terminal
      -> (unit, Provider_stream_queue_error.t) result

    val abort_from_consumer
      :  t
      -> consumer_failure
      -> (unit, Provider_stream_queue_error.t) result

    val take_or_await
      :  sw:Eio.Switch.t
      -> t
      -> (item, Provider_stream_queue_error.t) result

    val finish_frame
      :  taken_frame
      -> (unit, Provider_stream_queue_error.t) result

    val retain_for_assembly
      :  taken_frame
      -> (retained_frame, Provider_stream_queue_error.t) result

    val finish_retained
      :  retained_frame
      -> (unit, Provider_stream_queue_error.t) result
  end
end
```

Each provider attempt owns exactly one bounded queue created from
`Runtime_capacity.provider_stream_queue_frame_capacity` and
`provider_stream_per_attempt_byte_capacity`, backed by one opaque
whole-attempt lease from the runtime-global
`Provider_stream_byte_admission` created from
`provider_stream_global_byte_capacity`. All three are explicit runtime-owner
capacities with no OAS default. The bounded transport reader never supplies a
source chunk larger than `provider_transport_read_chunk_capacity`.
Before network dispatch, `acquire_attempt_or_await` fairly reserves the entire
per-attempt capacity atomically from the global admission. It never grants a
partial attempt lease, so two streams cannot each retain a prefix while waiting
for the other's bytes. `reserve_and_append` acquires from that one lease before
copying bytes into its opaque ingress buffer; streaming decompression
must write through the same API. Thus non-stream error-body accumulation,
SSE/NDJSON line accumulation, JSON parsing, and partial Tool-argument assembly
cannot allocate an unaccounted giant intermediate first. The network producer
blocks cooperatively when either
admission is full; it cannot drop, coalesce, silently truncate, or reorder a
frame. If the configured storage cannot accept a valid frame, the attempt
closes with typed `Provider_capacity_failure.t`, carrying the same bounded
native evidence, not a provider protocol error or behavioral-budget decision.
The queue
validates exact attempt, contract, contiguous sequence, and measured bytes.
A frame is an opaque cursor-bound byte source, not one flattened
`Inline_bytes.t`. The framer consumes it incrementally under the taken or
retained permit; one SSE/NDJSON event may span arbitrarily many admitted
transport reads without an intermediate concatenation. `None` bytes are
returned only for a caught-up zero/finished source with an unchanged terminal
cursor; `None, false` is impossible. A cursor from another frame, a read after
finish, or a stale retained permit is a typed error. Sealing an ingress with no
append publishes a real zero-byte sequenced frame. `reserve_and_append` remains
positive-length, so empty bytes are never fabricated.
A `taken_frame` retains its byte permit until the exact adapter finishes,
commits, or fails parsing it. Bytes needed across frames for a partial Tool
argument, reasoning item, or text item transfer to `retained_frame`; they are
released only after the accumulator is durably externalized/finalized or its
typed failure evidence commits. The adapter cannot release a frame and retain
an unaccounted copy. Cancellation and every failure path release each lineage
exactly once. Stale, duplicate, or wrong-queue release is typed. The
whole-attempt lease is released only after the queue terminal and every
taken/retained frame settle.
`Terminal` is delivered only after every accepted frame, and close,
cancellation, or transport failure wakes both producer and consumer with typed
state. The exact adapter consumes the frames and commits parsed delta facts
through the ordinary Journal lane; this queue is not a second transcript or
semantic Journal authority. Framing, finish, and malformed-input semantics come
only from the exact wire contract. No timer, inferred payload size, or
provider-name branch changes queue behavior.

Transport reads, queue waits, and spool/page I/O remain Eio fiber work. For
every bounded page, the pure UTF-8/JSON/envelope/hash/canonicalization step is
submitted to the application runtime's one `Cpu_executor`; the returned parser
state is then advanced by the Eio consumer. There is no payload-size threshold,
“small parse inline” branch, provider exception, or adaptive offload heuristic.
A CPU job receives immutable page bytes and pure parser state only—never a
network handle, switch-owned I/O resource, Journal writer, or blocking wait.
This prevents a hot compatible stream from monopolizing the Keeper/server Eio
domain while also preventing CPU workers from becoming I/O executors.

When the consumer detects a protocol, capacity/spool, or runtime failure,
`abort_from_consumer` atomically maps its closed `consumer_failure` to the
matching terminal, rejects later ingress/pushes, and wakes a producer blocked
on either admission. A consumer cannot construct `End_of_stream` or claim a
producer transport failure. The terminal follows the already accepted frames.
Those frames are drained into bounded evidence or ordinary parsed facts before
the terminal is consumed; they are never silently discarded. Repeating the
same abort is idempotent, while a different terminal cause is a typed conflict.

Every protocol failure durably commits its exact attempt/contract, decoder
phase, optional frame sequence, byte offset, and immutable native evidence
before it can be reported as a completed failure. Invalid UTF-8, duplicate JSON
keys, and malformed JSON are therefore representable without forcing the bytes
through `Canonical_json.t`. Evidence within capacity is complete. Capacity
overflow retains the exact admitted prefix plus
`Capacity_exceeded { limit; received_at_least }`; this is explicit bounded
evidence, never a claim that the rejected suffix was retained. If evidence
persistence itself fails, the attempt returns a composite
protocol-plus-recording failure and cannot collapse to a generic timeout.
Native evidence is lifecycle typed and backed by a zero-capable, binary,
byte-paged `Provider_content.t`; it is never one nonempty chunk or a whole
malformed body. Queue/parser failures hold only staged evidence. The failure
root transaction incrementally publishes that binary content and alone mints
committed evidence/error wrappers; staged wrappers have no encoder and cannot
enter a read-model fact. Invalid UTF-8 therefore remains byte-exact. Immediate
EOF or a header-only failure has a valid zero-byte source, while an overflow
records the exact admitted prefix plus checked `received_at_least`.

For every provider ToolUse identity, the journal contains at most one
authoritative matching ToolResult fact. A provider continuation request
projects that fact exactly once within that request before the conversation
continues.

The transport request itself may be retried. This is logical result
cardinality, not a claim of exactly-once network delivery. If the authoritative
ToolResult cannot be committed, provider continuation is forbidden.

### 9.1 Continuation is rebuilt from facts

OAS never forms the next request by mutating the previous serialized request or
concatenating dashboard text. It builds a fresh provider-specific continuation
plan from an immutable conversation snapshot and committed execution facts:

```ocaml
module Provider_item_kind : sig
  type t =
    | Thinking
    | Reasoning
    | Reasoning_summary
    | Text
    | Refusal
    | Multimodal
    | Tool_use of Provider_tool_call_reference.t
end

module Provider_item_classification : sig
  type tool_use =
    { provider_tool_name : Provider_tool_name.t
    ; native_correlation :
        Provider_native_scalar.staged Provider_native_tool_correlation.t
    ; exposure : Executable.exposure_reference
    }

  type t =
    | Thinking
    | Reasoning
    | Reasoning_summary
    | Text
    | Refusal
    | Multimodal
    | Tool_use of tool_use
end

module Canonical_json_stream : sig
  module Chunk : sig
    type t = Execution_value.Inline_bytes.t

    val of_string_slice_copy
      :  capacity:Positive_byte_count.t
      -> source:string
      -> offset:int
      -> length:Positive_byte_count.t
      -> (t, Construction_error.t) result

    val byte_count : t -> Positive_byte_count.t
    val bytes : t -> Execution_value.Inline_bytes.t
  end

  module Event : sig
    type t = Execution_value.Canonical_json_event.t =
      | Null
      | Bool of bool
      | Begin_number
      | Number_chunk of Chunk.t
      | End_number
      | Begin_string
      | String_chunk of Chunk.t
      | End_string
      | Begin_array
      | End_array
      | Begin_object
      | Begin_object_name
      | Object_name_chunk of Chunk.t
      | End_object_name
      | End_object
  end

  module Sink : sig
    type t
  end
end

module Provider_semantic_slice : sig
  type t

  val byte_count : t -> Byte_count.t
end

module Provider_parsed_stream_carrier : sig
  type t

  type phase =
    | Delta
    | Done_snapshot
    | Replacement_snapshot

  val lane : t -> Provider_stream_lane_correlation.t
  val field : t -> Provider_stream_content_contract.field
  val phase : t -> phase
  val native_event_ordinal : t -> int64
  val semantic : t -> Provider_semantic_slice.t
  val evidence
    :  t
    -> Provider_native_evidence.staged Provider_native_evidence.t
end

module Provider_stream_snapshot_proof : sig
  type t

  val lane : t -> Provider_stream_lane_correlation.t
  val field : t -> Provider_stream_content_contract.field
  val accumulated : t -> Provider_content_digest.t
  val snapshot : t -> Provider_content_digest.t
end

module Provider_stream_content_progress : sig
  type t

  val lane : t -> Provider_stream_lane_correlation.t
  val field : t -> Provider_stream_content_contract.field
  val encoded_bytes : t -> Byte_count.t
end

module Provider_stream_content_update : sig
  type field = Provider_stream_content_contract.field =
    | Output_text
    | Reasoning_text
    | Reasoning_summary_text
    | Refusal_text
    | Tool_arguments
    | Content_part

  type application =
    | Fragment_appended of Provider_stream_content_progress.t
    | Final_snapshot_validated of Provider_stream_snapshot_proof.t
    | Final_snapshot_adopted of Provider_stream_content_progress.t
    | Snapshot_replaced of Provider_stream_content_progress.t

  type t

  val lane : t -> Provider_stream_lane_correlation.t
  val field : t -> field
  val application : t -> application
end

module Provider_stream_content_error : sig
  type t =
    | Protocol of
        Provider_protocol_error.staged Provider_protocol_error.t
    | Capacity of
        Provider_capacity_failure.staged Provider_capacity_failure.t
    | Spool of Provider_spool_error.t
    | Cancelled of Cancellation.cause
end

module Provider_spool_bootstrap : sig
  type t

  val create
    :  id:Execution_identity.Provider_spool_id.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Provider_spool_recovery : sig
  type t

  val recovered_generation : t -> int64 option
  val discarded_attempts : t -> int64
  val discarded_bytes : t -> int64
end

module Provider_spool_store : sig
  type t
  type attempt_lease
  type canonical_writer
  type utf8_writer
  type binary_writer
  type native_scalar_writer

  module Internal : sig
    val open_
      :  sw:Eio.Switch.t
      -> bootstrap:Provider_spool_bootstrap.t
      -> global_byte_capacity:Positive_byte_count.t
      -> per_attempt_byte_capacity:Positive_byte_count.t
      -> page_byte_capacity:Positive_byte_count.t
      -> (t * Provider_spool_recovery.t,
          Provider_spool_open_error.t)
         result

    val acquire_attempt_or_await
      :  sw:Eio.Switch.t
      -> t
      -> attempt:Provider_attempt.Id.t
      -> (attempt_lease, Provider_spool_error.t) result

    val begin_canonical
      :  attempt_lease
      -> (canonical_writer, Provider_spool_error.t) result

    val append_canonical_event
      :  canonical_writer
      -> Canonical_json_stream.Event.t
      -> (unit, Provider_spool_error.t) result

    val seal_canonical
      :  canonical_writer
      -> (Provider_content.staged Provider_content.t,
          Provider_spool_error.t)
         result

    val begin_utf8
      :  attempt_lease
      -> (utf8_writer, Provider_spool_error.t) result

    val append_utf8
      :  utf8_writer
      -> Canonical_json_stream.Chunk.t
      -> (unit, Provider_spool_error.t) result

    val seal_utf8
      :  utf8_writer
      -> (Provider_content.staged Provider_content.t,
          Provider_spool_error.t)
         result

    val begin_binary
      :  attempt_lease
      -> (binary_writer, Provider_spool_error.t) result

    val append_binary
      :  binary_writer
      -> Execution_value.Inline_bytes.t
      -> (unit, Provider_spool_error.t) result

    val seal_binary
      :  binary_writer
      -> (Provider_content.staged Provider_content.t,
          Provider_spool_error.t)
         result

    val begin_native_scalar
      :  attempt_lease
      -> (native_scalar_writer, Provider_spool_error.t) result

    val append_native_scalar
      :  native_scalar_writer
      -> Canonical_json_stream.Chunk.t
      -> (unit, Provider_spool_error.t) result

    val seal_native_scalar
      :  native_scalar_writer
      -> (Provider_native_scalar.staged Provider_native_scalar.t,
          Provider_spool_error.t)
         result

    val read_staged
      :  attempt_lease
      -> Provider_content.staged Provider_content.t
      -> after:Provider_content_cursor.t
      -> max_bytes:Positive_byte_count.t
      -> (Provider_content.page, Read_error.t) result

    val discard_attempt
      :  attempt_lease
      -> (unit, Provider_spool_error.t) result

    val close_and_await
      :  t
      -> (unit, Provider_spool_close_error.t) result
  end
end

module Provider_native_scalar_source : sig
  type page =
    { bytes : Immutable_byte_slice.t option
    ; next : Provider_native_scalar.cursor
    ; caught_up : bool
    }

  val read_staged
    :  Provider_spool_store.attempt_lease
    -> Provider_native_scalar.staged Provider_native_scalar.t
    -> after:Provider_native_scalar.cursor
    -> max_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val read_committed
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> Provider_native_scalar.committed Provider_native_scalar.t
    -> after:Provider_native_scalar.cursor
    -> max_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val equal_staged
    :  Provider_spool_store.attempt_lease
    -> Provider_native_scalar.staged Provider_native_scalar.t
    -> Provider_native_scalar.staged Provider_native_scalar.t
    -> (bool, Read_error.t) result

  val equal_committed
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> Provider_native_scalar.committed Provider_native_scalar.t
    -> Provider_native_scalar.committed Provider_native_scalar.t
    -> (bool, Read_error.t) result

  val write_committed_json_string
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> Provider_native_scalar.committed Provider_native_scalar.t
    -> sink:Canonical_json_stream.Sink.t
    -> (unit, Encode_error.t) result
end

module Provider_stream_content_accumulator : sig
  type t

  module Internal : sig
    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> contract:Provider_wire_contract.t
      -> attempt:Provider_attempt.Id.t
      -> (t, Provider_spool_error.t) result

    val apply
      :  t
      -> Provider_parsed_stream_carrier.t
      -> (Provider_stream_content_update.t,
          Provider_stream_content_error.t)
         result

    val seal_field
      :  t
      -> lane:Provider_stream_lane_correlation.t
      -> field:Provider_stream_content_contract.field
      -> (Provider_content.staged Provider_content.t,
          Provider_stream_content_error.t)
         result

    val abort
      :  t
      -> (unit, Provider_spool_error.t) result
  end
end

```

`Canonical_json_stream.Event.t` is a closed incremental representation, not a
second JSON tree. Strings, object names, and number lexemes are bracketed
chunked scalars; a `Chunk.t` is copied only after its slice length has passed
the explicit byte capacity. `append_canonical_event` validates incremental
UTF-8, number grammar, object/array nesting, exactly one root,
object-name/value alternation, and duplicate object names while bytes enter
the lease. Duplicate-name detection retains a digest and spooled key bytes,
then performs byte equality on a digest collision; it never retains all key
strings in heap. `seal` rejects an incomplete scalar or document. It never
accepts raw pre-rendered JSON or calls `Canonical_json.t` on the whole
document. `Canonical_json.t` remains the semantic codec SSOT; the streaming
encoder is its byte-for-byte incremental implementation and a drift test
compares both encoders over the same bounded fixture corpus.

Provider-native call, response, and item identifiers use the separate
`Provider_native_scalar` representation. The JSON parser writes their decoded
UTF-8 chunks through `begin_native_scalar`/`append_native_scalar`; it never
calls a whole-string constructor. The role-specific wrappers prevent a
response ID, item ID, and call ID from being exchanged even when their bytes
are equal. `Provider_stream_lane_correlation.t` is a Dune-private staged value
with no encoder or decoder. Exact adapters compare staged scalars by checked
byte count and digest followed by page-wise byte equality, and a continuation
serializer writes a committed scalar only through
`write_committed_json_string`. There is no standalone staged-to-committed
promotion: only the provider-success root builder can attach staged scalar
roots and the finalized native item to the same semantic transaction. Failure
or cancellation discards every staged scalar with the attempt lease.

`Provider_content.t` is a content-addressed immutable canonical byte source.
Its closed format distinguishes canonical JSON, incrementally validated UTF-8
text, and lossless binary evidence. A complete provider Tool argument is UTF-8
evidence even when its text is not valid JSON. All writers may seal an exact
zero-byte source; page reads represent that as `None, caught_up=true` rather
than an invented nonempty chunk. `staged` references cannot be encoded as durable facts. Only the Journal
success transaction can copy their bytes incrementally into its physical event
store and return `committed` references; a committed reference includes the
store namespace, digest, and byte count and is verified on every open. Neither
lifecycle exposes a filesystem path. Content reads return no more than the
explicit byte request and never materialize a complete item.

The provider spool is pure staging and no committed root ever references it.
`open_` exclusively validates the stable store ID, format version, and prior
generation manifests; it enumerates them in bounded pages, verifies their
digests, and removes every prior-generation attempt before readiness. Missing
pages, corrupt manifests, delete failure, or an already-open generation is a
typed startup error, not silent cleanup. The returned
`Provider_spool_recovery.t` records exact discarded attempts/bytes for
observation only. There is no age/TTL/orphan-name heuristic. Journal-internal
pending chunks/root-CAS uncertainty are recovered by the Journal's own commit
ID protocol and are never inferred from spool files.
`acquire_attempt_or_await` reserves the complete per-attempt spool lease
atomically. Waiters are FIFO within the exact execution owner and ready owners
advance round-robin; a hot provider attempt cannot release/reacquire ahead of
an older satisfiable Keeper. No attempt holds a partial lease while waiting
for the remainder, and cancellation/close removes its exact waiter. This
fairness uses no age score, payload estimate, model priority, cost, or turn
budget.

The exact grammar parser first mints one opaque
`Provider_parsed_stream_carrier.t` from its closed native event variant. The
wire contract and that carrier derive the field and
`Provider_stream_content_update.application`; the adapter never supplies
either as a free argument. The accumulator key is the pair of full typed
stream lane and semantic field, so interleaved Chat choices/tools, multiple
fields on one item, and Responses
item/output/content indices cannot merge. A delta carrier cannot be relabelled
as done, nor can a done carrier be sent through append.
`Fragment_appended` extends that lane's one accumulator-owned open
writer/rope and returns only bounded progress metadata; it does not seal or
copy the accumulated value per delta.
`Final_snapshot_validated` never appends again: while terminal semantic chunks
arrive it compares the accumulated
byte count and digest to the terminal snapshot, performs byte comparison when
digests match, and either emits the small equality proof or fails the whole
attempt. The comparison sink discards terminal semantic chunks after
comparison; it does not spool a second complete semantic value. The full
native done/item envelope remains once as bounded native evidence.
A final snapshot without earlier deltas is valid only when that exact contract
permits the done-only form. In that case `Final_snapshot_adopted` streams the
snapshot once into the lane's writer, rather than comparing and discarding the
only semantic value. `seal_field ~lane ~field` is the single operation that returns the
staged immutable content for replay/finalization. Duplicate done/snapshot
events, data after done,
and disagreement between a field-done snapshot, an item-done full object, and
the response terminal are protocol failures even if a convenient prefix could
be salvaged. An item-done full item is validation evidence for the already
assembled semantic item, not another item to append.

`Snapshot_replaced` is constructible only when the selected binding's immutable
`Provider_stream_content_contract` explicitly declares replacement snapshots
for that field. The adapter never discovers cumulative output by prefix,
substring, equality-after-trimming, payload length, repeated prose, or model
name. Output text, reasoning text, reasoning summary, refusal text, content
parts, and Tool arguments follow the same closed rule independently; accepting
one carrier says nothing about another.
Replacement atomically installs the new lane writer/root and releases the
superseded byte lineage exactly once; it never repeatedly copies the entire
prefix. `abort` releases every open lane writer and comparison cursor.
The per-field state machine also validates native event ordinal, added/delta/
done/item-done/response-terminal order, and uniqueness before producing an
update. Mutation tests swapping a delta/done carrier or its field must fail;
event-name string matching is not an alternate parser.
`begin_` freezes the attempt and exact wire-contract reference. Before any
writer, lane map, digest, or event frontier mutates, `apply` compares both to
the parsed carrier's evidence and rejects a cross-attempt, cross-contract, or
stale-lane-generation carrier. Routing a valid parsed carrier to the wrong
accumulator therefore cannot corrupt either attempt.
For a full object such as Responses `content_part.done`, the exact parser
extracts and compares the contract-declared semantic subvalue (for example the
text scalar) for the same typed lane while preserving annotations and other
native metadata in the envelope. It never byte-compares an entire object to a
text delta or discards metadata merely to make equality pass.

The JSON envelope and a Tool argument string also have separate parsers.
Duplicate keys in an outer response/event object reject the provider attempt.
A duplicate key inside a complete `function.arguments` UTF-8 string is instead
an invocation `Invalid_input` result with its matching ToolResult, because the
provider grammar completed the ToolUse even though the Tool input did not
decode. This distinction is structural, never substring-based.
Measured content uses nonnegative `Byte_count.t`; only capacities, requested
read sizes, and copied nonempty chunks use `Positive_byte_count.t`. Empty text,
an empty delta, and a complete empty Tool-argument string are therefore
representable without fabricating a byte or dropping the event. Empty
metadata-only events either advance their exact grammar state with no semantic
append or seal a zero-byte source as declared. Every lane/event/manifest record
still consumes separately measured nonzero metadata capacity, so a flood of
zero-byte carriers cannot bypass admission.

```ocaml
module Provider_tool_arguments : sig
  type staged
  type committed
  type 'lifecycle t

  val content : 'lifecycle t -> 'lifecycle Provider_content.t

  module Internal : sig
    val of_complete_utf8_staged
      :  Provider_content.staged Provider_content.t
      -> (staged t, Construction_error.t) result

    val of_committed
      :  Provider_content.committed Provider_content.t
      -> (committed t, Construction_error.t) result
  end
end

module Provider_finalized_item : sig
  type replay_eligibility =
    | Replayable
    | Observation_only

  type t
  type view =
    | Non_tool of
        { kind : Provider_item_kind.t
        ; semantic : Execution_value.t
        }
    | Tool_use of
        { call : Provider_tool_call_reference.t
        ; arguments :
            Provider_tool_arguments.committed Provider_tool_arguments.t
        }

  val source_key : t -> Provider_source_key.t
  val source_adapter : t -> Provider_adapter_reference.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val ordinal : t -> int
  val kind : t -> Provider_item_kind.t
  val view : t -> view
  val replay_eligibility : t -> replay_eligibility
  val native : t -> Provider_content.committed Provider_content.t
  val supersedes_delta_through : t -> int option
  val tool_arguments
    :  t
    -> Provider_tool_arguments.committed Provider_tool_arguments.t option

  module Internal : sig
    val create_non_tool
      :  source_key:Provider_source_key.t
      -> source_adapter:Provider_adapter_reference.t
      -> turn:Agent_turn.Id.t
      -> exchange:Provider_exchange.Id.t
      -> attempt:Provider_attempt.Id.t
      -> ordinal:int
      -> kind:Provider_item_kind.t
      -> semantic:Execution_value.t
      -> replay_eligibility:replay_eligibility
      -> native:Provider_content.committed Provider_content.t
      -> supersedes_delta_through:int option
      -> (t, Construction_error.t) result

    val create_tool_use
      :  source_key:Provider_source_key.t
      -> source_adapter:Provider_adapter_reference.t
      -> turn:Agent_turn.Id.t
      -> exchange:Provider_exchange.Id.t
      -> attempt:Provider_attempt.Id.t
      -> ordinal:int
      -> call:Provider_tool_call_reference.t
      -> arguments:
           Provider_tool_arguments.committed Provider_tool_arguments.t
      -> native:Provider_content.committed Provider_content.t
      -> supersedes_delta_through:int option
      -> t
  end
end

module Conversation_input_role : sig
  type t = Agent_prelude.role =
    | System
    | User
    | Application_context
end

module Committed_conversation_input : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val source_key : t -> Provider_source_key.t
  val role : t -> Conversation_input_role.t
  val content : t -> Execution_value.t

  module Internal : sig
    val of_committed_fact
      :  Execution_journal.Committed_conversation_input.t
      -> t
  end
end

module Conversation_input_writer : sig
  val append
    :  writer:Execution_journal.Writer.t
    -> role:Conversation_input_role.t
    -> content:Execution_value.t
    -> (Committed_conversation_input.t, Conversation_input_write_error.t) result
end

module Conversation_snapshot : sig
  type input_role = Conversation_input_role.t

  type input = Committed_conversation_input.t
  type item =
    | Input of input
    | Finalized_provider_output of Provider_finalized_item.t

  module Selection : sig
    type builder
    type t

    val begin_
      :  sw:Eio.Switch.t
      -> Conversation_selection_authority.t
      -> (builder, Conversation_selection_error.t) result

    val append_input
      :  builder
      -> Committed_conversation_input.t
      -> (unit, Conversation_selection_error.t) result

    val append_provider_output
      :  builder
      -> Provider_finalized_item.t
      -> (unit, Conversation_selection_error.t) result

    val seal
      :  builder
      -> (t, Conversation_selection_error.t) result

    val abort
      :  builder
      -> (unit, Conversation_selection_error.t) result
  end

  type t
  type cursor

  type page =
    { items : item list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val committed_by : t -> Execution_fact_ref.t
  val selected_through : t -> Execution_page_cursor.t
  val read_items
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
  val input_source_key : input -> Provider_source_key.t
  val input_role : input -> input_role
  val input_content : input -> Execution_value.t

  module Internal : sig
    val commit_for_exchange
      :  writer:Execution_journal.Writer.t
      -> turn:Agent_turn.Id.t
      -> exchange:Provider_exchange.Id.t
      -> selected_through:Execution_page_cursor.t
      -> Selection.t
      -> (t, Conversation_selection_commit_error.t) result

    val open_committed
      :  reader:Execution_journal.Reader.t
      -> t
      -> (t, Read_error.t) result
  end
end

module Provider_input_truncation_selection : sig
  type t =
    | Fail_on_overflow
    | Explicit_lossy_auto

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_input_delivery : sig
  type t =
    | Exact_by_contract
    | Provider_may_have_truncated

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_declared_failure : sig
  type kind =
    | Failed
    | Incomplete

  type staged
  type committed
  type 'lifecycle t

  val kind : 'lifecycle t -> kind
  val detail : 'lifecycle t -> Provider_failure_attribution.detailed_error
  val evidence
    :  'lifecycle t
    -> 'lifecycle Provider_native_evidence.t

  module Internal : sig
    val create_staged
      :  kind:kind
      -> detail:Provider_failure_attribution.detailed_error
      -> evidence:Provider_native_evidence.staged Provider_native_evidence.t
      -> staged t
  end
end

module Provider_attempt_failure : sig
  type cause =
    | Transport of Provider_failure_attribution.detailed_error
    | Declared_provider_failure of
        Provider_declared_failure.committed Provider_declared_failure.t
    | Protocol of
        Provider_protocol_error.committed Provider_protocol_error.t
    | Capacity of
        Provider_capacity_failure.committed Provider_capacity_failure.t
    | Spool of Provider_spool_error.t
    | Cancelled of Cancellation.cause
    | Runtime of Provider_runtime_error.t

  type t

  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val cause : t -> cause
  val terminal_event : t -> Execution_fact_ref.t

  module Internal : sig
    val of_committed_failure
      :  binding:Provider_binding_reference.t
      -> adapter:Provider_adapter_reference.t
      -> turn:Agent_turn.Id.t
      -> exchange:Provider_exchange.Id.t
      -> attempt:Provider_attempt.Id.t
      -> cause:cause
      -> terminal_event:Execution_fact_ref.t
      -> t
  end
end

module Provider_adapter_attempt_error : sig
  type t =
    | Declared_provider_failure of
        Provider_declared_failure.staged Provider_declared_failure.t
    | Protocol_failure of
        Provider_protocol_error.staged Provider_protocol_error.t
    | Capacity_failure of
        Provider_capacity_failure.staged Provider_capacity_failure.t
    | Spool_failure of Provider_spool_error.t
    | Cancelled of Cancellation.cause
    | Runtime_failure of Provider_runtime_error.t
end

module Token_count : sig
  type t

  val of_int64 : int64 -> (t, Construction_error.t) result
  val to_int64 : t -> int64
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_observation_metric : sig
  type unit_ =
    | Count
    | Duration_ns

  type core =
    | Input_tokens
    | Output_tokens
    | Total_tokens
    | Cached_input_tokens
    | Reasoning_tokens
    | Audio_input_tokens
    | Audio_output_tokens
    | Accepted_prediction_tokens
    | Rejected_prediction_tokens
    | Total_duration
    | Load_duration
    | Prompt_eval_duration
    | Eval_duration

  type kind =
    | Core of core
    | Native_extension of
        { adapter : Provider_adapter_reference.t
        ; name : Provider_content.committed Provider_content.t
        ; unit_ : unit_
        }

  type t

  val kind : t -> kind
  val value : t -> int64

  module Internal : sig
    val create
      :  kind:kind
      -> value:int64
      -> (t, Construction_error.t) result
  end
end

module Provider_observation_metric_source : sig
  type builder
  type prepared
  type t
  type cursor

  type page =
    { metrics : Provider_observation_metric.t list
    ; next : cursor
    ; caught_up : bool
    }

  val manifest
    :  t
    -> (Execution_manifest_purpose.provider_observations,
        Execution_manifest.committed)
         Execution_manifest.t
  val beginning : t -> cursor
  val read
    :  Execution_journal.Reader.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  transaction:
           (Execution_manifest_staging.open_
              Execution_manifest_staging.transaction)
      -> (builder, Provider_observation_error.t) result
    val append
      :  builder
      -> Provider_observation_metric.t
      -> (unit, Provider_observation_error.t) result
    val seal
      :  builder
      -> (prepared, Provider_observation_error.t) result
  end
end

module Provider_usage_observation : sig
  type count =
    | Unreported
    | Reported of Token_count.t

  type t

  val input_tokens : t -> count
  val output_tokens : t -> count
  val total_tokens : t -> count
  val cached_input_tokens : t -> count
  val reasoning_tokens : t -> count

  module Internal : sig
    val derive
      :  Execution_journal.Reader.t
      -> Provider_observation_metric_source.t
      -> (t, Read_error.t) result
  end
end

module Provider_finish_observation : sig
  type kind =
    | Completed
    | Tool_calls
    | Length_limit
    | Content_filtered
    | Native_other

  type t =
    | Unreported
    | Reported of kind

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_duration_ns : sig
  type t

  val of_int64 : int64 -> (t, Construction_error.t) result
  val to_int64 : t -> int64
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Provider_timing_observation : sig
  type duration =
    | Unreported
    | Reported of Provider_duration_ns.t

  type t

  val generation : t -> duration

  module Internal : sig
    val derive
      :  Execution_journal.Reader.t
      -> Provider_observation_metric_source.t
      -> (t, Read_error.t) result
  end
end

module Provider_terminal_observation : sig
  type prepared
  type t

  val metrics : t -> Provider_observation_metric_source.t
  val finish : t -> Provider_finish_observation.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create_prepared
      :  metrics:Provider_observation_metric_source.prepared
      -> finish:Provider_finish_observation.t
      -> prepared
  end
end

module Execution_observation_metric : sig
  type unavailable =
    | Output_tokens_unreported
    | Provider_generation_duration_unreported
    | Observation_clock_mismatch
    | Nonpositive_duration

  type basis =
    | Provider_reported_generation
    | Oas_observed_end_to_end

  type rate

  val basis : rate -> basis
  val output_tokens : rate -> Token_count.t
  val elapsed_ns : rate -> int64
  val tokens_per_second : rate -> Finite_number.t

  val provider_reported_generation_output_rate
    :  reader:Execution_journal.Reader.t
    -> Provider_terminal_observation.t
    -> (rate, unavailable) result

  val oas_observed_end_to_end_output_rate
    :  reader:Execution_journal.Reader.t
    -> opened_at:Observation_time.t
    -> completed_at:Observation_time.t
    -> Provider_terminal_observation.t
    -> (rate, unavailable) result
end

module Provider_attempt_success : sig
  type t

  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val input_delivery : t -> Provider_input_delivery.t
  val observation : t -> Provider_terminal_observation.t
  val terminal_native
    :  t
    -> Provider_content.committed Provider_content.t
  val terminal_event : t -> Execution_fact_ref.t

  module Internal : sig
    val of_committed_success
      :  binding:Provider_binding_reference.t
      -> adapter:Provider_adapter_reference.t
      -> turn:Agent_turn.Id.t
      -> exchange:Provider_exchange.Id.t
      -> attempt:Provider_attempt.Id.t
      -> input_delivery:Provider_input_delivery.t
      -> observation:Provider_terminal_observation.t
      -> terminal_native:Provider_content.committed Provider_content.t
      -> terminal_event:Execution_fact_ref.t
      -> t
  end
end

module Provider_attempt_selected : sig
  type t

  val success : t -> Provider_attempt_success.t
  val selection_event : t -> Execution_fact_ref.t

  module Internal : sig
    val of_committed_selection
      :  success:Provider_attempt_success.t
      -> selection_event:Execution_fact_ref.t
      -> t
  end
end

module Provider_attempt_selection : sig
  type t

  val find
    :  t
    -> Provider_exchange.Id.t
    -> (Provider_attempt_selected.t option, Read_error.t) result

  module Internal : sig
    val of_journal
      :  reader:Execution_journal.Reader.t
      -> stream:Execution_stream_reference.t
      -> t
  end
end

module Invocation_opened_fact : sig
  type ancestry =
    | Root
    | Structural_child of Invocation_attempt_reference.t
    | Caused_by of Invocation_attempt_reference.t

  type provider_wire =
    { schedule : Hooks.tool_schedule
    ; arguments :
        Provider_tool_arguments.committed Provider_tool_arguments.t
    }

  type t

  val invocation : t -> Invocation_reference.t
  val ancestry : t -> ancestry
  val binding : t -> Executable.reference
  val exposure : t -> Executable.exposure_reference option
  val provider_wire : t -> provider_wire option
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Invocation_result : sig
  type outcome =
    | Succeeded of Execution_value.t
    | Declared_failure of Execution_value.t
    | Blocked of Execution_value.t
    | Invalid_input of Execution_value.t
    | Cancelled of Cancellation.cause
    | Infrastructure_failed of Execution_value.t

  module Committed : sig
    type t

    val fact : t -> Execution_fact_ref.t
    val opened : t -> Invocation_opened_fact.t
    val outcome : t -> outcome
  end
end

module Provider_semantic_item_slot : sig
  type t

  val attempt : t -> Provider_attempt.Id.t
  val ordinal : t -> Provider_item_ordinal.t
end

module Provider_semantic_item_slot_source : sig
  type builder
  type prepared
  type cursor

  type page =
    { slots : Provider_semantic_item_slot.t list
    ; next : cursor
    ; caught_up : bool
    }

  val attempt : prepared -> Provider_attempt.Id.t
  val count : prepared -> int64
  val digest : prepared -> Execution_manifest_digest.t
  val beginning : prepared -> cursor
  val read
    :  prepared
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> attempt:Provider_attempt.Id.t
      -> builder

    val append
      :  builder
      -> ordinal:Provider_item_ordinal.t
      -> semantic:Execution_value.prepared
      -> (Provider_semantic_item_slot.t, Construction_error.t) result

    val seal
      :  builder
      -> (prepared, Construction_error.t) result
  end
end

module Execution_manifest_semantic_fact_catalog : sig
  type agent_run_open_without_prelude
  type agent_run_open_with_prelude
  type agent_checkpoint
  type execution_value_owner
  type provider_exchange_open
  type provider_attempt_open
  type tool_batch_definition
  type executable_plan_open
  type executable_plan_terminal
  type submission_intent
  type submission_publication
  type provider_attempt_terminal
  type commit_action_repair
  type runtime_shutdown

  type execution_value_owner_kind =
    | Agent_run_terminal
    | Conversation_input
    | Invocation_input
    | Invocation_success
    | Invocation_declared_failure
    | Invocation_blocked
    | Invocation_invalid_input
    | Invocation_infrastructure_failure
    | Hook_block_reason
    | Hook_observer_failure
    | Effect_request
    | Effect_receipt
    | Effect_failure
    | Operation_recovery_failure
    | Commit_action_payload

  type _ family =
    | Agent_run_open_without_prelude :
        agent_run_open_without_prelude family
    | Agent_run_open_with_prelude :
        agent_run_open_with_prelude family
    | Agent_checkpoint : agent_checkpoint family
    | Execution_value_owner :
        execution_value_owner_kind
        -> execution_value_owner family
    | Provider_exchange_open : provider_exchange_open family
    | Provider_attempt_open : provider_attempt_open family
    | Tool_batch_definition : tool_batch_definition family
    | Executable_plan_open : executable_plan_open family
    | Executable_plan_terminal : executable_plan_terminal family
    | Submission_intent : submission_intent family
    | Submission_publication : submission_publication family
    | Provider_attempt_terminal :
        Provider_semantic_item_slot_source.prepared
        -> provider_attempt_terminal family
    | Commit_action_repair : commit_action_repair family
    | Runtime_shutdown : runtime_shutdown family

  type (_, _) field =
    | Agent_run_definition_without_prelude :
        (agent_run_open_without_prelude,
         Execution_manifest_purpose.agent_definition)
          field
    | Agent_run_input_without_prelude :
        (agent_run_open_without_prelude,
         Execution_manifest_purpose.execution_value)
          field
    | Agent_run_definition_with_prelude :
        (agent_run_open_with_prelude,
         Execution_manifest_purpose.agent_definition)
          field
    | Agent_run_prelude :
        (agent_run_open_with_prelude,
         Execution_manifest_purpose.agent_prelude)
          field
    | Agent_run_input_with_prelude :
        (agent_run_open_with_prelude,
         Execution_manifest_purpose.execution_value)
          field
    | Agent_checkpoint_frontier :
        (agent_checkpoint,
         Execution_manifest_purpose.agent_checkpoint)
          field
    | Execution_value :
        (execution_value_owner,
         Execution_manifest_purpose.execution_value)
          field
    | Conversation_selection :
        (provider_exchange_open,
         Execution_manifest_purpose.conversation_selection)
          field
    | Provider_tool_exposure :
        (provider_attempt_open,
         Execution_manifest_purpose.provider_tool_exposure)
          field
    | Tool_batch_members :
        (tool_batch_definition,
         Execution_manifest_purpose.tool_batch_members)
          field
    | Executable_calls :
        (executable_plan_open,
         Execution_manifest_purpose.executable_calls)
          field
    | Executable_results :
        (executable_plan_terminal,
         Execution_manifest_purpose.executable_results)
          field
    | Submission_operations :
        (submission_intent,
         Execution_manifest_purpose.submission_operations)
          field
    | Submission_receipts :
        (submission_publication,
         Execution_manifest_purpose.submission_receipts)
          field
    | Provider_native_scalars :
        (provider_attempt_terminal,
         Execution_manifest_purpose.provider_native_scalars)
          field
    | Provider_observations :
        (provider_attempt_terminal,
         Execution_manifest_purpose.provider_observations)
          field
    | Provider_terminal_semantic :
        Provider_semantic_item_slot.t
        ->
        (provider_attempt_terminal,
         Execution_manifest_purpose.execution_value)
          field
    | Commit_action_repair_source :
        (commit_action_repair,
         Execution_manifest_purpose.commit_action_repair)
          field
    | Shutdown_report :
        (runtime_shutdown,
         Execution_manifest_purpose.shutdown_report)
          field

  val field
    :  'fact Execution_manifest_semantic_fact_builder.t
    -> ('fact, 'purpose) field
    -> (('fact, 'purpose)
          Execution_manifest_semantic_fact_builder.field,
        Manifest_root_attachment_error.t)
       result

  module Internal : sig
    val seal
      :  family:'fact family
      -> transaction:Execution_journal.Typed_transaction.t
      -> ('fact Execution_manifest_semantic_fact_builder.t,
          Manifest_root_build_error.t)
         result
  end
end

module Provider_tool_result : sig
  type t

  val source_key : t -> Provider_source_key.t
  val invocation : t -> Invocation.Id.t
  val call : t -> Provider_tool_call_reference.t
  val outcome : t -> Invocation_result.outcome

  module Internal : sig
    val of_committed_fact
      :  Invocation_result.Committed.t
      -> (t, Provider_tool_result_load_error.t) result
  end
end

module Provider_source_fragment : sig
  type t

  val create
    :  source_key:Provider_source_key.t
    -> fragment_ordinal:int
    -> (t, Construction_error.t) result

  val source_key : t -> Provider_source_key.t
  val fragment_ordinal : t -> int
  val compare : t -> t -> int
end

module Provider_source_fragment_source : sig
  type t
  type builder
  type cursor

  type page =
    { fragments : Provider_source_fragment.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> (builder, Provider_spool_error.t) result

    val append
      :  builder
      -> Provider_source_fragment.t
      -> (unit, Provider_spool_error.t) result

    val seal
      :  builder
      -> (t, Provider_spool_error.t) result
  end
end

module Provider_projected_item : sig
  type t

  val create
    :  attributions:Provider_source_fragment_source.t
    -> canonical:Provider_content.staged Provider_content.t
    -> (t, Construction_error.t) result

  val attributions : t -> Provider_source_fragment_source.t
  val canonical : t -> Provider_content.staged Provider_content.t
end

module Provider_projected_item_source : sig
  type t
  type builder
  type cursor

  type page =
    { items : Provider_projected_item.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> (builder, Provider_spool_error.t) result

    val append
      :  builder
      -> Provider_projected_item.t
      -> (unit, Provider_spool_error.t) result

    val seal
      :  builder
      -> (t, Provider_spool_error.t) result
  end
end

module Provider_tool_result_source : sig
  type t
  type cursor

  type page =
    { results : Provider_tool_result.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val for_conversation
      :  reader:Execution_journal.Reader.t
      -> conversation:Conversation_snapshot.t
      -> high_water:Execution_page_cursor.t
      -> (t, Read_error.t) result
  end
end

module Provider_item_finalization : sig
  type t
  type view =
    | Non_tool of
        { classification : Provider_item_classification.t
        ; semantic : Execution_value.prepared
        }
    | Tool_use of
        { classification : Provider_item_classification.tool_use
        ; arguments :
            Provider_tool_arguments.staged Provider_tool_arguments.t
        }

  val classification : t -> Provider_item_classification.t
  val view : t -> view
  val replay_eligibility
    :  t
    -> Provider_finalized_item.replay_eligibility
  val native : t -> Provider_content.staged Provider_content.t
  val tool_arguments
    :  t
    -> Provider_tool_arguments.staged Provider_tool_arguments.t option
  val supersedes_delta_through : t -> int option

  module Internal : sig
    val create_non_tool
      :  classification:Provider_item_classification.t
      -> semantic:Execution_value.prepared
      -> replay_eligibility:Provider_finalized_item.replay_eligibility
      -> native:Provider_content.staged Provider_content.t
      -> supersedes_delta_through:int option
      -> (t, Construction_error.t) result

    val create_tool_use
      :  classification:Provider_item_classification.tool_use
      -> arguments:
           Provider_tool_arguments.staged Provider_tool_arguments.t
      -> native:Provider_content.staged Provider_content.t
      -> supersedes_delta_through:int option
      -> t
  end
end

module Provider_native_attempt_item : sig
  type t

  val ordinal : t -> int
  val native : t -> Provider_content.staged Provider_content.t

  module Internal : sig
    val create
      :  ordinal:int
      -> native:Provider_content.staged Provider_content.t
      -> (t, Construction_error.t) result
  end
end

module Provider_attempt_item_cursor : sig
  type t

  val beginning : t
end

module Provider_native_attempt_source : sig
  type t
  type builder

  type page =
    { items : Provider_native_attempt_item.t list
    ; next : Provider_attempt_item_cursor.t
    ; caught_up : bool
    }

  val read
    :  t
    -> after:Provider_attempt_item_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> attempt:Provider_attempt.Id.t
      -> contract:Provider_wire_contract_reference.t
      -> (builder, Construction_error.t) result

    val append
      :  builder
      -> Provider_native_attempt_item.t
      -> (unit, Provider_spool_error.t) result

    val seal
      :  builder
      -> (t, Provider_spool_error.t) result
  end
end

module Provider_attempt_finalization : sig
  type item
  type t

  val item_ordinal : item -> int
  val item_finalization : item -> Provider_item_finalization.t
  val terminal_observation
    :  t
    -> Provider_terminal_observation.prepared
  val terminal_native
    :  t
    -> Provider_content.staged Provider_content.t
  val semantic_slots
    :  t
    -> Provider_semantic_item_slot_source.prepared

  type page =
    { items : item list
    ; next : Provider_attempt_item_cursor.t
    ; caught_up : bool
    }

  val read_items
    :  t
    -> after:Provider_attempt_item_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    type builder

    val begin_
      :  spool:Provider_spool_store.attempt_lease
      -> source:Provider_native_attempt_source.t
      -> (builder, Construction_error.t) result

    val item
      :  ordinal:int
      -> finalization:Provider_item_finalization.t
      -> (item, Construction_error.t) result

    val append
      :  builder
      -> item
      -> (unit, Encode_error.t) result

    val seal
      :  builder
      -> terminal_observation:Provider_terminal_observation.prepared
      -> terminal_native:Provider_content.staged Provider_content.t
      -> (t, Construction_error.t) result
  end
end

module Provider_validated_attempt : sig
  type t

  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val finalization : t -> Provider_attempt_finalization.t
  val terminal_observation
    :  t
    -> Provider_terminal_observation.prepared
  val terminal_native
    :  t
    -> Provider_content.staged Provider_content.t
end

module Provider_committed_attempt : sig
  type t

  type page =
    { items : Provider_finalized_item.t list
    ; next : Provider_attempt_item_cursor.t
    ; caught_up : bool
    }

  val read_items
    :  t
    -> after:Provider_attempt_item_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
  val terminal_observation : t -> Provider_terminal_observation.t
  val terminal_native
    :  t
    -> Provider_content.committed Provider_content.t
  val success : t -> Provider_attempt_success.t
end

module Provider_attempt_commit : sig
  type outcome =
    | Selected of
        { attempt : Provider_committed_attempt.t
        ; selection : Provider_attempt_selected.t
        }
    | Existing_selection of Provider_attempt_selected.t

  val commit_success
    :  writer:Execution_journal.Writer.t
    -> Provider_validated_attempt.t
    -> (outcome,
        Provider_attempt_commit_error.t)
       result
end

module Provider_binding : sig
  type t
  type reference = Provider_binding_reference.t

  val create
    :  provider:Llm_provider.Provider_config.t
    -> (t, Provider_binding_error.t) result

  val reference : t -> reference
  val provider : t -> Llm_provider.Provider_config.t
  val model : t -> Types.model
  val http_codec : t -> Provider_http_codec.t
  val wire_contract : t -> Provider_wire_contract.t
  val adapter : t -> Provider_adapter_reference.t

  module Internal : sig
    val create_with_adapter
      :  provider:Llm_provider.Provider_config.t
      -> adapter:Provider_adapter_reference.t
      -> (t, Provider_binding_error.t) result

    val same_authority : t -> t -> bool
  end
end

module Provider_tool_exposure_snapshot : sig
  type entry
  type prepared
  type t
  type cursor

  val provider_tool_name : entry -> Provider_tool_name.t
  val exposure : entry -> Executable.exposure_reference
  val manifest
    :  t
    -> (Execution_manifest_purpose.provider_tool_exposure,
        Execution_manifest.committed)
         Execution_manifest.t
  val beginning : t -> cursor
  type page =
    { entries : entry list
    ; next : cursor
    ; caught_up : bool
    }
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
  val find
    :  reader:Execution_journal.Reader.t
    -> t
    -> Provider_tool_name.t
    -> (Executable.exposure_reference, Lookup_error.t) result
  val resolve_native_name
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> spool:Provider_spool_store.attempt_lease
    -> t
    -> Provider_native_scalar.staged Provider_native_scalar.t
    -> (entry, Lookup_error.t) result
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

end

module Provider_tool_wire_definition_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Provider_tool_offer_plan : sig
  type prepared
  type t

  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val wire_contract : t -> Provider_wire_contract_reference.t
  val exposures : t -> Provider_tool_exposure_snapshot.t
  val wire_definition_digest
    :  t
    -> Provider_tool_wire_definition_digest.t

end

module Provider_tool_offer_projection : sig
  type t

  val plan : t -> Provider_tool_offer_plan.t
  val canonical
    :  t
    -> Provider_content.staged Provider_content.t
  val wire_definition_digest
    :  t
    -> Provider_tool_wire_definition_digest.t
end

module Committed_provider_attempt_opened : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val wire_contract : t -> Provider_wire_contract_reference.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val conversation : t -> Conversation_snapshot.t
  val input_truncation : t -> Provider_input_truncation_selection.t
  val tool_offer : t -> Provider_tool_offer_plan.t
end

module Provider_attempt_context : sig
  type t

  val opened_fact : t -> Execution_fact_ref.t
  val binding : t -> Provider_binding_reference.t
  val adapter : t -> Provider_adapter_reference.t
  val wire_contract : t -> Provider_wire_contract.t
  val turn : t -> Agent_turn.Id.t
  val exchange : t -> Provider_exchange.Id.t
  val attempt : t -> Provider_attempt.Id.t
  val conversation : t -> Conversation_snapshot.t
  val input_truncation : t -> Provider_input_truncation_selection.t
  val tool_offer : t -> Provider_tool_offer_plan.t
  val exposures : t -> Provider_tool_exposure_snapshot.t
  val semantic_value_authority : t -> Execution_value_authority.t
  val begin_observation_metrics
    :  sw:Eio.Switch.t
    -> t
    -> (Provider_observation_metric_source.builder,
        Provider_observation_error.t)
       result

  module Internal : sig
    val of_committed_open
      :  semantic_value_authority:Execution_value_authority.t
      -> binding:Provider_binding.t
      -> exposure_registry:Tool_exposure_registry.t
      -> Committed_provider_attempt_opened.t
      -> (t, Provider_attempt_context_error.t) result
  end
end

module Provider_tool_decode_request : sig
  val open_
    :  sw:Eio.Switch.t
    -> execution:Execution_context.t
    -> attempt:Provider_attempt_context.t
    -> opened:Invocation_opened_fact.t
    -> tool:Tool.t
    -> (Tool_decode_request.t, Provider_tool_decode_error.t) result
end

module Provider_attempt_failure_commit : sig
  val commit
    :  writer:Execution_journal.Writer.t
    -> context:Provider_attempt_context.t
    -> Provider_adapter_attempt_error.t
    -> (Provider_attempt_failure.t,
        Provider_attempt_failure_commit_error.t)
       result
end

module Provider_attempt_repair : sig
  type scan
  type open_attempt
  type cursor

  type page =
    { attempts : open_attempt list
    ; next : cursor
    ; caught_up : bool
    }

  val begin_scan
    :  Execution_journal.Reader.t
    -> (scan, Read_error.t) result
  val read_open
    :  Execution_journal.Reader.t
    -> scan:scan
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
  val recover
    :  writer:Execution_journal.Writer.t
    -> open_attempt
    -> (Provider_attempt_failure.t,
        Provider_attempt_failure_commit_error.t)
       result
end

module Provider_continuation_adapter : sig
  module Id = Execution_identity.Provider_adapter_id
  module Revision = Execution_identity.Provider_adapter_revision

  type reference = Provider_adapter_reference.t

  module type S = sig
    type state

    val reference : reference

    val validate_contract
      :  state
      -> Provider_wire_contract.t
      -> (unit, Provider_adapter_contract_error.t) result

    val finalize_attempt
      :  state
      -> context:Provider_attempt_context.t
      -> items:Provider_native_attempt_source.t
      -> terminal_native:Provider_content.staged Provider_content.t
      -> (Provider_attempt_finalization.t,
          Provider_adapter_attempt_error.t)
         result

    val build_sequence
      :  state
      -> spool:Provider_spool_store.attempt_lease
      -> contract:Provider_wire_contract.t
      -> conversation:Conversation_snapshot.t
      -> selected_attempts:Provider_attempt_selection.t
      -> committed_tool_results:Provider_tool_result_source.t
      -> (Provider_projected_item_source.t, Continuation_error.t) result
  end

  type t =
    | Adapter :
        { implementation : (module S with type state = 'state)
        ; state : 'state
        }
        -> t

  val pack
    :  (module S with type state = 'state)
    -> state:'state
    -> t

  val reference : t -> reference
end

module Provider_continuation_registry : sig
  type t

  val build
    :  Provider_continuation_adapter.t list
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Provider_continuation_adapter.reference
    -> (Provider_continuation_adapter.t, Lookup_error.t) result
end

module Provider_bound_adapter : sig
  type t

  val resolve
    :  adapters:Provider_continuation_registry.t
    -> Provider_binding.t
    -> (t, Provider_adapter_contract_error.t) result

  val binding : t -> Provider_binding.t
  val adapter : t -> Provider_adapter_reference.t

  val prepare_tool_offer
    :  t
    -> transaction:
         (Execution_manifest_staging.open_
            Execution_manifest_staging.transaction)
    -> catalog:Tool_catalog.t
    -> (Provider_tool_offer_plan.prepared, Construction_error.t) result

  val project_tool_offer
    :  t
    -> spool:Provider_spool_store.attempt_lease
    -> context:Provider_attempt_context.t
    -> (Provider_tool_offer_projection.t,
        Provider_request_projection_error.t)
       result

  val finalize_attempt
    :  t
    -> context:Provider_attempt_context.t
    -> items:Provider_native_attempt_source.t
    -> terminal_native:Provider_content.staged Provider_content.t
    -> (Provider_validated_attempt.t,
        Provider_adapter_attempt_error.t)
       result

  val build_sequence
    :  t
    -> spool:Provider_spool_store.attempt_lease
    -> context:Provider_attempt_context.t
    -> selected_attempts:Provider_attempt_selection.t
    -> committed_tool_results:Provider_tool_result_source.t
    -> (Provider_projected_item_source.t, Continuation_error.t) result
end

module Provider_attempt_open_preparation : sig
  type fresh
  type consumed
  type 'state t

  val prepare
    :  transaction:
         (Execution_manifest_staging.open_
            Execution_manifest_staging.transaction)
    -> offer:Provider_tool_offer_plan.prepared
    -> turn:Agent_turn.Id.t
    -> exchange:Provider_exchange.Id.t
    -> attempt:Provider_attempt.Id.t
    -> conversation:Conversation_snapshot.t
    -> input_truncation:Provider_input_truncation_selection.t
    -> (fresh t, Provider_attempt_open_error.t) result

  val commit
    :  writer:Execution_journal.Writer.t
    -> store:Execution_manifest_store.t
    -> fresh t
    -> ((consumed t * Committed_provider_attempt_opened.t),
        Provider_attempt_open_error.t)
       result
end

module Provider_binding_registry : sig
  type t

  val empty : t

  val extend
    :  t
    -> Provider_binding.t list
    -> adapters:Provider_continuation_registry.t
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Provider_binding.reference
    -> (Provider_bound_adapter.t, Lookup_error.t) result
end

module Provider_request_item : sig
  type t

  val adapter : t -> Provider_continuation_adapter.reference
  val canonical : t -> Provider_content.staged Provider_content.t

  module Internal : sig
    val create
      :  adapter:Provider_continuation_adapter.reference
      -> canonical:Provider_content.staged Provider_content.t
      -> t
  end
end

module Provider_continuation_plan : sig
  type t
  type item
  type cursor

  val build
    :  spool:Provider_spool_store.attempt_lease
    -> bound:Provider_bound_adapter.t
    -> context:Provider_attempt_context.t
    -> selected_attempts:Provider_attempt_selection.t
    -> committed_tool_results:Provider_tool_result_source.t
    -> (t, Continuation_error.t) result

  type page =
    { items : item list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read_items
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
  val attributions : item -> Provider_source_fragment_source.t
  val request_item : item -> Provider_request_item.t
  val read_request_content
    :  t
    -> Provider_request_item.t
    -> after:Provider_content_cursor.t
    -> max_bytes:Positive_byte_count.t
    -> (Provider_content.page, Read_error.t) result
end

module Provider_wire_request : sig
  type t
  type cursor

  val binding : t -> Provider_binding_reference.t
  val wire_contract : t -> Provider_wire_contract_reference.t
  val tool_offer : t -> Provider_tool_offer_plan.t
  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> max_bytes:Positive_byte_count.t
    -> (Provider_content.page, Read_error.t) result

  module Internal : sig
    val seal
      :  context:Provider_attempt_context.t
      -> tool_offer:Provider_tool_offer_projection.t
      -> items:Provider_projected_item_source.t
      -> (t, Provider_request_projection_error.t) result
  end
end
```

`Provider_finalized_item`, `Provider_item_classification`,
`Provider_item_finalization`, `Provider_attempt_finalization`,
`Provider_validated_attempt`, `Provider_committed_attempt`,
`Provider_attempt_commit`, `Provider_attempt_context`,
`Provider_continuation_adapter`, and `Provider_bound_adapter` share one
representation-owning private compilation unit. Provider response integration
can call only `Provider_bound_adapter.finalize_attempt`; it cannot call
`Provider_finalized_item.Internal.create` directly or finalize one ToolUse in
isolation. The exact adapter revision therefore owns both whole-attempt native
response validation and later continuation assembly.
`Provider_continuation_adapter.S`, `pack`, raw finalization constructors, and
`Execution_runtime.Internal.create_with_extensions` is Dune-private
OAS implementation/test surfaces and are not re-exported by `Agent_sdk`.
Ordinary runtime construction installs the built-in exact adapters from the
OAS provider/codec catalog. An OpenAI-compatible endpoint selects a declared
contract under that existing adapter; callers never implement a raw
finalization SPI. Adding a genuinely new wire grammar is an OAS adapter
implementation change with its checked constructor tests, not an application
callback or JSON plugin.

The transport incrementally externalizes accepted native items into one
attempt-owned immutable spool and seals it as
`Provider_native_attempt_source.t` plus exact terminal-native metadata and one
typed `Provider_terminal_observation.t`. The spool retains ordinal order and
content digests, reads only bounded pages, and is not a second semantic
transcript: before success it is staging. On success, content and item records
are copied incrementally in bounded Journal-internal staging transactions.
Those chunks are addressable only by digest under the pending commit ID; they
are not facts, selections, replay inputs, or dashboard rows. One final small
Journal root transaction atomically publishes the sealed item-manifest
root/count/digest, terminal reference, successful attempt, and unique per-turn
selection. A finalized item fact reference is the root event plus its checked
manifest ordinal, so the reader can page logical sibling facts without the
root transaction encoding every item. No committed fact points back into the
provider staging directory. The attempt acquires the spool lease before the stream lease and
releases it if stream acquisition fails, so two admissions cannot hold partial
resources in opposite order. A frame/accumulator permit is released only after
its byte lineage has transferred to the already-accounted spool or durably
committed as fact/evidence; the spool lease remains charged until success
commit, typed abort cleanup, cancellation cleanup, or runtime close completes.
Neither a single native item nor finalization nor success commit materializes
the whole payload as `Canonical_json.t` or an item list. Metadata and content
are separate: ordinal metadata is read in item-and-byte-bounded pages, while a
large text/Tool-argument/native envelope is a digest-checked
`Provider_content.t` read in byte pages. The exact adapter/contract decoder
projects declared usage counters, provider-reported generation duration, and
finish semantics while it still owns the wire grammar. A missing provider field
remains `Unreported`; OAS never estimates
tokens from text, derives one counter from another, parses dashboard JSON, or
classifies an unknown finish string generically. `Native_other` means only that
the exact adapter accepted a contract-declared native terminal kind whose
generic semantics are intentionally not stronger; the unmodified terminal
envelope remains the evidence. These
pre-commit items carry only a zero-based ordinal and native payload; a
`Provider_source_key.t` does not exist until a Journal fact commits. Streaming
deltas cannot construct a source key, finalized item, ToolUse reference, or
successful attempt.

A Tool-argument stream has two distinct validity boundaries. Missing/changing
call identity, illegal event order, an unterminated argument string, or a
terminal snapshot inconsistent with its deltas is an attempt-wide provider
protocol failure. A complete UTF-8 argument string with valid call
identity/name is instead finalized even when its JSON syntax, duplicate-key
check, or Tool schema is invalid. The selected ToolUse carries its immutable
`Provider_tool_arguments` source into `Invocation_opened_fact`; the dispatch
decoder streams it once and commits `Invalid_input` plus the matching
ToolResult without opening a handler attempt. PreTool receives the same raw
source reference, while `decoded_input` remains `None`. This is not a provider
retry and does not lose the call ID needed for continuation.
`Provider_tool_decode_request.open_` accepts that checked
`Invocation_opened_fact.t`, not a loose argument source. It verifies that the
execution context names the same invocation, that the
origin is exactly `Provider_tool_call`, and that the call's provider attempt,
adapter, ToolUse source, exposure, and declared Tool name belong to the supplied
committed provider-attempt context and its frozen offer plan. Only then does it
derives the `provider_wire.arguments` source and runtime-owned decode admission,
resolves the supplied opaque Tool against the exact immutable exposure and
executable binding, and returns one `Tool_decode_request.t`. A programmatic
origin, absent provider wire, another invocation's arguments, an independently
chosen Tool/codec, or any cross-attempt/call/exposure/name substitution is a
typed decode-request binding error before reading one argument event.
Every successfully finalized `Tool_use` is therefore `Replayable` by
construction. Its two private constructors do not accept a replay-eligibility
argument. Only non-Tool provider items may be
`Observation_only`. A provider-internal call-like object which its exact
grammar declares non-dispatchable is a distinct closed non-Tool
classification or an attempt protocol failure; it can never be represented as
an observation-only ToolUse. Invalid JSON/schema affects the invocation
outcome, not whether the provider's already complete ToolUse participates in
continuation.

Every finalized non-Tool item also carries one provider-neutral semantic
`Execution_value.t` produced incrementally by the same exact adapter from the
validated lane state. Thinking, reasoning, text, refusal, and multimodal block
boundaries remain closed typed items; the semantic value is not dashboard
prose. Journal success commits that immutable semantic manifest, its native
evidence reference, source adapter, and classification in the same item root.
While finalization pages items, it also appends every non-Tool semantic
preparation to one attempt-owned `Provider_semantic_item_slot_source` under
the same ordinal; it appends no slot for a ToolUse. Sealing verifies exact
one-to-one coverage against all finalization pages and fixes count/digest.
Provider success root construction uses that sealed source as the
`Provider_attempt_terminal` catalog family and attaches each semantic manifest
through its ordinal-keyed token. It never re-enumerates semantics into a list
or uses one unkeyed field for the whole attempt.
The native envelope is protocol/audit SSOT while the semantic manifest is its
revision-bound display/replay projection; neither may be changed
independently. A mismatch fails finalization. MASC and other consumers page the
semantic value and never parse provider JSON, while diagnostic views may page
the native evidence separately.

`S.finalize_attempt` validates the provider's complete response grammar by
paging that sealed source: required grammar-specific correlation identities,
duplicate/cross-linked identities, argument completion, terminal
reason versus returned content, reasoning or thought-signature artifacts,
closed item kinds, replay eligibility, and provider-required adjacency. It
resolves every ToolUse name through the exact
`Provider_tool_exposure_snapshot.t` committed with that provider request; an
unoffered or currently unresolvable exposure is an attempt-wide protocol
failure, not a lookup against a newer catalog. It
appends one pre-commit finalization for every supplied ordinal in the same
order to a bounded finalization spool. The generic wrapper rejects a negative,
missing, duplicated, invented,
or reordered ordinal; stamps binding/adapter/turn/attempt provenance; and
returns one opaque `Provider_validated_attempt.t`. This witness contains no
committed fact identity. Each item also declares either no provisional-delta
coverage or one exact last contiguous `delta_sequence` for that same
attempt/item ordinal. The Journal reducer verifies that every claimed sequence
exists under that item with no gap before success commit, that `None` is valid
iff no delta exists for that item, and that `Some n` equals the exact highest
committed contiguous sequence at the success transaction's read high-water.
An adapter cannot leave a later delta provisionally visible, supersede another
item's deltas, or name a sequence that was never committed.

A documented terminal failure carrier is not a malformed stream. The exact
adapter maps native Ollama's in-stream `{"error": ...}`, Responses
`response.failed`, and contract-declared incomplete terminals to
`Declared_provider_failure` with exact bounded evidence. Earlier deltas remain
observable, but no validated success, item manifest, exchange selection, or
Tool dispatch authority is minted. Protocol violations, transport errors,
capacity/spool failures, cancellation, and runtime failures remain distinct
constructors. The transport layer never parses a provider payload to make this
decision.

`Provider_attempt_commit.commit_success` streams the finalization pages into
the Journal's invisible content/item staging namespace, verifies their digests,
then uses one reducer CAS on the
`(Agent_turn.Id, Provider_exchange.Id)` selection key to publish the small root
transaction. That root derives each logical item fact reference and
`Provider_source_key.t`, converts every
`Provider_item_classification.Tool_use` into a
`Provider_tool_call_reference.t` naming that ToolUse's own committed source key
and exact exposure reference, and publishes terminal-native metadata,
successful attempt, finalized-item manifest, and selection atomically.
`Selected` returns a bounded committed-item source, success, and selection only
after that root is durable. If an earlier ambiguous commit already selected
the same exchange/request lineage, the CAS publishes none of the new candidate and
returns `Existing_selection`; its staged chunks are discarded by exact pending
commit ID. There is no durable unselected success and no “choose the first
success after restart” policy. A ToolUse can be dispatched only from that
selected root; a validated but uncommitted candidate has no executable
identity. An attempt-wide protocol error instead commits
`Provider_attempt_failure (Protocol ...)`; no earlier item from the rejected
candidate becomes replayable or executable.
If a chunk write or root transaction aborts, no finalized item, source key,
terminal, success witness, selection, or dispatch authority is returned. If
the root commit result is uncertain, recovery queries its exact pending commit
ID/exchange CAS rather than uploading again or selecting heuristically. Invisible
chunks absent from a committed root are enumerated and reclaimed by exact
reachability, never age. Failure recording is attempted only after the root
outcome is resolved; recording failure returns both causes and never converts
the candidate into success.
`Provider_attempt_success.Internal.of_committed_success` and
`Provider_finalized_item.Internal.create` are called only by that commit path
from the transaction's immutable fact references.
The successful-attempt witness retains the exact terminal-native metadata
(provider terminal envelope) and the adapter-derived typed
`Provider_terminal_observation.t` in the same success transaction. A dashboard
therefore never reparses provider JSON or joins an unrelated log to obtain
finish/usage. The typed projection is observation-only: it cannot select a
provider, admit or terminate work, control continuation, or create a
token/cost/turn budget.
Its metric manifest is the only usage/timing SSOT. Core metric kinds cover
token, cached/reasoning/audio/prediction counts and native Ollama-style
total/load/prompt-eval/eval durations without pretending every provider
reports every value. Adapter-specific additions retain an exact adapter
revision, paged UTF-8 field-name content reference, declared unit, and
nonnegative checked value. `Provider_usage_observation` and
`Provider_timing_observation` are derived read views over that manifest, not
separately encoded records. Missing, duplicate, wrong-unit, negative, or
overflowed metrics are typed; they are never filled with zero. Large extension
sets are read through byte/count-bounded pages. No metric—especially token,
cost, rate, or turn count—is an execution admission or stop condition.
`Provider_attempt_selected.Internal.of_committed_selection` is minted only from
the same Journal root that publishes its successful attempt and wraps that
success witness. The exchange-keyed reducer rejects a second selection for
that exchange; no
separate selection writer or crash window exists. Selection APIs accept these
durable witnesses, not bare occurrence IDs or an arbitrary list of successes.
During planning, every `Replayable` finalized provider
item must name the selected witness's exact attempt and source adapter
revision. A missing or mismatched selection is a typed continuation error; it
is never silently downgraded to observation-only.
`Provider_tool_result.Internal.of_committed_fact` accepts no loose
source-key/invocation/call/outcome fields. It consumes the Journal's opaque
`Invocation_result.Committed.t`, derives the source key from its exact fact
reference, and verifies
that the invocation origin carries the same full
`Provider_tool_call_reference`, and only then returns the opaque value.
Wrong fact kind, cross-stream identity, or correlation mismatch is a typed
projection error.
`Conversation_snapshot.Input` accepts only an opaque
`Committed_conversation_input.t`; it cannot pair an existing source key with a
different role or rewritten content. New user/system/application context is
first appended through `Conversation_input_writer.append`, which returns the
immutable fact-derived witness. An application-owned compactor may append its
result as explicit `Application_context`, but OAS neither computes nor silently
substitutes that result.
The embedding application, not OAS, chooses the exact conversation subset
through the public OAS-owned `Conversation_snapshot.Selection.builder`. It
pushes opaque committed input/provider-output witnesses one at a time; there
is no application callback, module packing, raw content, bare source key, or
list-returning SPI. The builder is obtained from the Agent's narrow opaque
`Conversation_selection_authority.t` and its switch, so OAS writes directly
through that runtime's bounded spool and cleanup authority without creating an
`Agent`/provider module cycle. The authority, Agent representation, and
selection builder share one private representation-owning unit; no application
can construct or retarget the token. `begin_` acquires a private provisional
selection-staging lease before returning the builder; every `append_*` writes
the witness reference immediately under that lease rather than retaining a
list. `seal` consumes the builder and returns its already sealed immutable
manifest without a whole-selection copy. `abort`, append/seal failure, switch
cancellation, and a builder that leaves scope all discard that exact
provisional lease with typed cleanup; no provider-attempt identity or durable
turn exists yet.
Before any provider effect,
OAS mints a `Provider_exchange.Id` and calls
`Conversation_snapshot.Internal.commit_for_exchange`. The exchange-open
transaction owns that sealed selection manifest, its digest/count, and the
exact Journal high-water once. Selection failure opens neither the exchange
nor a provider attempt. The exchange-keyed reducer accepts only one manifest;
a byte-different selection for the same exchange is a typed conflict.

Initial, retry, and fallback provider attempts for that exchange store the same
opaque `Conversation_snapshot.t` witness in `Provider_attempt_opened`. They may
encode that source set through another exact provider adapter, but cannot
rerun an application compactor, select newer facts, or replace the manifest.
`open_committed` freezes the stored high-water, verifies every selected fact,
and rejects duplicate source keys while pages are read. Crash recovery loads
the exchange witness before considering another attempt. Thus a transport
failure cannot turn a changed prompt into an implicit retry.

A selected ToolUse exchange commits its ToolResults and then opens a **new**
exchange under the same AgentTurn. That continuation exchange derives its
immutable source frontier from the preceding selected witness, all matching
committed ToolResults, and the preceding authorized snapshot; it does not ask
the embedding application to rebuild history. Any further ToolUse repeats this
typed edge, so an arbitrary finite Tool loop has
`Exchange₀ → tools → Exchange₁ → …` rather than appending a response to its own
input or exhausting a per-turn selection singleton. A selected final non-Tool
exchange closes the AgentTurn. There is no exchange-count or turn budget.
The continuation-open root atomically publishes one
`Continuation_of_exchange` edge plus one ordinal
`Tool_result_causes_exchange` edge for every result in its committed manifest.
The reducer lockstep-verifies exact selected ToolUse coverage, distinct
invocations, contiguous result ordinals, and equality between the edge set and
manifest root before the exchange becomes visible. Missing, duplicate,
cross-turn, or extra results reject the whole root; the dashboard never
reconstructs this hierarchy by temporal proximity.
Compaction/memory policy remains outside OAS; a compactor's new
`Application_context` can participate only in a separately opened
application-selected turn frontier, not mutate an in-flight Tool loop.

Before calling an adapter, `Provider_continuation_plan.build` accepts the
committed `Provider_attempt_context.t`, not a loose snapshot. It verifies that
the bound adapter/binding equals that context and opens only
`Provider_attempt_context.conversation`. The private adapter receives that
derived snapshot; no caller can pair an opened attempt with another manifest.
The builder then lockstep-pages that snapshot and the exact ToolResults
required by its selected ToolUse
references. `Provider_tool_result_source.Internal.for_conversation` cannot
return an unrelated result from the same long-lived stream. The plan rejects
every cross-source collision and duplicate ToolResult source key, writes
projected request items and attribution manifests to the request attempt's
spool, and returns only a paged plan. The HTTP serializer reads item metadata
and staged canonical content in bounded pages; it never calls `items` or builds
the complete request tree. Source coverage is therefore over distinct facts,
not a set that can alias two inputs onto one key.

The same context is the adapter's sole semantic-value construction authority.
It binds `Execution_value.begin_` to the exact
`(stream, turn, exchange, attempt)` and runtime manifest transaction; adapter
state never captures a process-global store or mints its own authority.
`Provider_bound_adapter.finalize_attempt` rejects every prepared semantic
value whose owner differs from that context before it can enter a validated
attempt. Cancellation or failed finalization aborts those builders through the
same attempt transaction. Thus an adapter remains easy to implement and test
without acquiring storage-lifetime authority, and a value prepared for attempt
A cannot be committed under attempt B.

Each provider implementation packages one exact
`Provider_continuation_adapter.reference`. The application runtime builds one
immutable `Provider_continuation_registry` and one immutable
`Provider_binding_registry`. A binding revision freezes the process-local
provider configuration and exact adapter revision. Public
`Provider_binding.create`
derives `Types.model` only from `Provider_config.model_id`, derives
`Provider_http_codec.t` and endpoint-route kind only from the config's explicit
typed fields, validates their closed variants and the catalog-declared
provider-kind-codec-route tuple without classifying a path string,
resolves the one built-in adapter revision from that exact catalog row, and
derives the one immutable `Provider_wire_contract.t` required by RFC-OAS-029
S1.6 from that full typed tuple. `Provider_binding.reference` is exactly
`Provider_wire_contract.binding` from that resolved contract, which is itself
the opaque reference already carried by `Provider_config.t`; `create` accepts
no second binding ID or revision. Construction rejects any inequality among
those three views before a `Provider_binding.t` exists, and
`same_authority` can succeed only for that same checked reference and
process-local config authority. Neither model nor codec is accepted as a
second caller-supplied value, and an unknown required axis rejects binding
construction. Only Dune-private provider implementation tests may call
`Provider_binding.Internal.create_with_adapter`; ordinary callers never know,
parse, or hardcode a provider-adapter ID/revision, and that helper may replace
only the implementation adapter after the same config/contract/reference
derivation succeeds.
`Provider_bound_adapter.resolve` performs the exact registry lookup, requires
adapter-reference equality, calls the adapter's closed contract validator, and
returns one opaque checked package. `Provider_binding_registry.extend`
resolves and stores that package for every binding before publishing its
immutable snapshot, and `find` returns the checked package rather than a loose
binding, so an Ollama-native adapter cannot be paired with an OpenAI-chat
contract later.
Request,
synchronous parse, streaming parse, whole-attempt finalization, and
continuation all receive the same frozen contract through that package or the
committed `Provider_attempt_context`; provider identity alone cannot select a
dialect or let those paths resolve different contracts. A committed attempt
context must carry the package's exact binding and adapter references, and all
offered exposure references must resolve in the immutable runtime exposure
registry before response bytes are accepted. Reusing its stable binding key is
accepted only for the same unforgeable binding authority;
lookup never falls forward to a newer binding or adapter. Its `build_sequence`
pages the frozen typed snapshot, exchange-keyed Journal selection index, and
committed `Invocation_result.outcome` source under one request-spool lease. The
adapter is therefore the only unit allowed to interpret its provider-native
canonical payload, correlate exact provider ToolUse references, preserve native
reasoning/signature fields, place native function results, and construct the
provider-specific adjacency/order. Generic continuation code sees closed item
kinds, occurrence identities, and `Provider_request_item.t`; it never branches
on provider names, JSON tags, result-status strings, or rendered text.

The offered-Tool authority is one contract-qualified
`Provider_tool_offer_plan`, not an entries list or a whole JSON object.
`Provider_bound_adapter` prepares it once from the exact binding, adapter,
wire-contract revision, and sealed Tool catalog. `prepare_tool_offer` derives
all three references from the already checked bound package; no loose
binding/adapter/contract arguments or snapshot-level staging constructor
exists. The attempt-open integration calls it under the attempt's one manifest
transaction and passes the resulting abstract `prepared` plan to the only
declared consumer, `Provider_attempt_open_preparation.prepare`. That consumer
verifies the plan's staging-transaction token, derives its prepared exposure
root plus binding/adapter/contract/digest, constructs the closed
`Provider_attempt_open` semantic fact family, and attaches its exact prepared
exposure field. `commit` consumes the preparation once and root-commits the
exposure manifest, wire-definition digest/index, provider-attempt-open fact,
and the committed `Provider_tool_offer_plan.t` atomically. Failure exposes none
of them; reconciliation returns the same committed opened witness. Thus a
`prepared` plan has one declared promotion path and cannot be projected,
resolved, or dispatched before its owning attempt commits. Its attempt-open transaction
atomically publishes the `Provider_tool_exposure` manifest root, the exact
wire-definition digest, and an exact
`(snapshot_id, provider_tool_name)` unique projection index derived from those
same records. The same bound adapter pages that committed plan into
`Provider_tool_offer_projection`; `Provider_wire_request.Internal.seal`
accepts no catalog or ad-hoc Tool definitions and verifies the projection's
plan/digest against the opened attempt before returning the only request value
accepted by HTTP dispatch. Thus “offered”, “serialized”, and “eligible for
response resolution” cannot drift into three authorities.
`find` uses the snapshot index and then verifies the referenced manifest
record; a native response name is first staged as a paged scalar and
`resolve_native_name` performs exact streamed comparison. Neither path scans
every ToolUse nor accepts prefix/case/substring aliases. Paged reads are
bounded by count and encoded bytes. A missing/corrupt page, duplicate name
across a page boundary, wrong-purpose root, projection/digest mismatch, or
index/root disagreement fails before request dispatch or response acceptance;
no empty exposure fallback is constructed.
Every finalized item, successful-attempt witness, and ToolUse reference carries
its source adapter revision. A target adapter must explicitly accept and
translate that tagged source revision or return a typed incompatibility error;
fallback/model changes never infer the source provider from opaque JSON shape.

An OpenAI-compatible service, including an Ollama Cloud `/v1` binding,
uses the ordinary exact OpenAI-compatible adapter revision when it satisfies
that revision's response grammar. OAS does not branch on endpoint hostname,
provider name, model-name substring, or an `ollama` marker. Exact extension
fields admitted by that adapter revision, such as provider-returned reasoning
content, remain provider-native finalized data. Missing call IDs, incomplete
streamed arguments, incompatible terminal reasons, or an unrecognized wire
shape are typed protocol failures with exact bounded native evidence; overflow
is an explicit marker, never silent truncation. If a service later requires a
genuinely different wire grammar, the
catalog registers another explicit adapter revision; it is never selected by
catch-and-retry or payload-shape guessing.

Every exchange-open fact also freezes one
`Provider_input_truncation_selection.t`. The ordinary Agent and Tool
continuation constructors select `Fail_on_overflow`; there is no runtime
default to `Explicit_lossy_auto`. The latter can arise only from the
embedding application's explicit lossy-policy entry point and is validated
against the exact binding's RFC-OAS-029 truncation contract before any HTTP
effect. Unsupported selection performs zero requests. A failed
`Fail_on_overflow` attempt retains the exact immutable conversation snapshot,
so retry/fallback cannot silently shorten the prompt. A successful
`Explicit_lossy_auto` attempt commits
`Provider_may_have_truncated`; it cannot mint an exact-delivery claim even
when the returned response looks complete. Token, cost, turn, payload-size,
or repeated-Tool observations never select this mode.

The normative wire-carrier, replay, ToolResult-identity, forced-Tool, and
evidence-provenance rules are owned by RFC-OAS-029 S1.5, S2.4, S3.4-S3.5,
S4.4, S5.3, S7.3, and S9.4. This RFC adds no second provider capability matrix.
It requires only that the exact binding and adapter package selected under
those rules remain unchanged through transport, whole-attempt finalization,
Tool dispatch, durable facts, and continuation projection.

`Conversation_snapshot.t` is the exact caller-authorized context with provenance.
The embedding application may supply a compacted or memory-augmented snapshot,
but OAS neither chooses nor performs that policy.

Each ordered `item` packages one native request item together with its nonempty
`Provider_source_fragment.t` attributions; attribution never lives in a
parallel list. This permits one provider message/content node to group several
facts, and permits one fact to contribute several explicitly numbered native
fragments, without inventing another semantic source identity.
After adapter assembly, generic plan construction verifies that every
authorized input item, every `Replayable` finalized item from an exactly
selected durable attempt witness, and every supplied committed ToolResult has a
nonempty attribution partition, that fragment ordinals for each source are
unique and contiguous from zero, and that no unauthorized source key was
invented. The same `(source_key, fragment_ordinal)` cannot occur twice across
the plan. `Observation_only` finalized items are forbidden from adapter output.
The adapter rejects unresolved ToolUse references, more than one result for a
ToolUse, and provider-invalid ordering. Rebuilding twice from the same snapshot
and facts yields
byte-identical canonical request items. A transport retry reuses that plan; it
does not append its contents to a copy of itself.

Provider output attempts have explicit durable selection state. Only items from
the uniquely selected successful attempt enter later model input. Failed/retried attempt
payloads remain observable Journal facts but are not replayed as additional
assistant turns.

The AgentTurn, exact caller input, exchange-open snapshot, and
`Provider_attempt_opened` fact commit before provider transport begins.
Streaming facts append beneath that attempt.
A caller-supplied transport deadline, connection loss, cancellation, premature
stream end, or provider-protocol rejection closes the same attempt with one
typed `Provider_attempt_failure`; it cannot roll back the turn input or delete
already committed deltas. OAS supplies no default deadline. If committing the
terminal failure itself fails, the call returns a composite
`Provider_attempt_recording_error` containing both the primary provider failure
and Journal failure; it never reports an empty timeout, a successful turn, or a
missing message. A retry opens a fresh provider-attempt occurrence under the
same exchange. Only that exchange's separately committed selection witness can
make one successful attempt replayable. A post-Tool model call is a new
exchange, not a retry and not a second selection under the first exchange.

Startup repair takes a frozen high-water and pages every opened provider
attempt lacking a terminal. It first resolves any exact pending success/failure
root commit; committed outcomes are reused byte-for-byte. If the process lost
the attempt's transport before any terminal root existed, repair commits the
closed runtime-interruption failure for that same attempt and preserves all
prior deltas/evidence. It never waits for a watchdog, marks the attempt
successful, deletes its input, or concatenates it with a replacement request.
Only after that terminal is durable may policy open a new retry occurrence
under the unchanged exchange snapshot. Corrupt/missing pages or an uncertain
root that cannot be reconciled block readiness with the exact typed cause,
not a generic timeout or silent open-attempt leak.

Streaming deltas, partial thinking text, token counters, Tool argument deltas,
and dashboard-rendered summaries are observation-only facts. Only finalized
provider-native items admitted by the typed adapter are replayable:

- OpenAI reasoning items and function-call items retain their native identity
  and are returned with matching function-call outputs when required;
- Anthropic ToolUse/ToolResult blocks preserve exact correlation and adjacency;
- Gemini function calls/results preserve call identity and multimodal parts;
- Qwen reasoning content, function calls, and function results retain the
  adapter-declared turn structure;
- a provider that does not return replayable reasoning receives no fabricated
  chain-of-thought item.

This contract prevents framework-created repetition. If a model itself emits
semantically repeated finalized content, OAS preserves it as distinct evidence;
whether to intervene is an embedding LLM-policy decision, never a substring,
count, elapsed-time, cost, or turn heuristic.

### Synchronous Tool

The ToolResult is projected only after the exact execution result is durable.

### Asynchronous submission Tool

The outer ToolResult contains durable submission/operation receipts. It does
not wait for background operations to finish.

Every submission outcome closes the original ToolUse with one structured
result:

- `Accepted receipt` becomes a successful receipt ToolResult.
- `Rejected_before_commit rejection` becomes a typed failed ToolResult naming
  the durable rejection fact.
- `Reconciliation_required reference` becomes an explicit uncertainty
  ToolResult carrying the same submission identity and request digest.

The uncertainty result instructs the caller to reconcile that identity. It
never invites a blind retry with a new identity.

Because submission intent commits before any backend call, a definitive
backend absence or rejection is closed by
`Submission_rejected_before_backend_commit` in the same Journal transaction as
the caller-visible rejection result. That fact binds the exact submission ID,
request digest, typed reason, and backend absence/definitive-rejection proof.
The open-intent reducer treats it as terminal. A crash or reply loss after this
fact can only reload the same rejection; restart repair cannot prepare or
activate the request again. A rejection whose absence proof cannot be committed
is not returned as definitive and remains an explicit reconciliation failure.

`Reconciliation_required` is chosen only while the authoritative publication
outcome is still unknown. Once an accepted publication fact is committed, the
submission result is permanently `Accepted`. A lost or failed activation reply
does not rewrite it to uncertainty; it leaves the accepted operations visibly
`Pending_publication` in the backend until idempotent activation repair
succeeds, while OAS reconciliation reports
`Publication_committed_activation_pending`.

If later reconciliation proves that a previously uncertain backend preparation
committed, OAS appends a typed `Submission_reconciled` causal fact and activates
the same operations. It does not replace the uncertainty ToolResult or append a
second ToolResult to the closed ToolUse. If reconciliation proves absence, the
absence is likewise a later operation fact.

Later operation completion:

- is not appended as a second result for the original provider ToolUse;
- does not mutate a closed provider turn;
- does not invoke the outer submission Tool’s post hooks a second time;
- is exposed as a typed operation event;
- may cause a new turn only when the embedding application explicitly decides
  to wake or enqueue work.

Accepted background children use operation lifecycle events
(`Accepted`, `Started`, `Cancelling`, `Settled`). If a child later invokes a
Tool, that new invocation receives its own Tool hook lifecycle.

Provider continuation projection preserves the provider’s typed requirements,
including tool-call/result correlation, ordered content blocks, reasoning
items, thinking signatures, and multimodal results. Dashboard display strings
are never replay input.

## 10. Durable asynchronous collections

### 10.1 Atomic submission identity

Every submission has a caller-visible idempotency identity:

```ocaml
module Operation_execution_anchor : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t
  val operation : t -> Execution_identity.Operation_id.t
  val invocation : t -> Execution_identity.Invocation_id.t
  val scope
    :  t
    -> Execution_identity.Operation_execution_scope_id.t
  val scope_factory : t -> Operation_scope_factory_reference.t
  val stream : t -> Execution_identity.Event_stream_id.t
  val stream_reference : t -> Execution_stream_reference.t
  val equal : t -> t -> bool
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  backend:Execution_identity.Operation_backend_id.t
      -> operation:Execution_identity.Operation_id.t
      -> invocation:Execution_identity.Invocation_id.t
      -> scope:Execution_identity.Operation_execution_scope_id.t
      -> scope_factory:Operation_scope_factory_reference.t
      -> stream:Execution_identity.Event_stream_id.t
      -> (t, Construction_error.t) result
  end
end

module Operation_receipt : sig
  type t =
    { execution : Operation_execution_anchor.t
    ; ordinal : int
    ; binding : Executable.reference
    ; exposure : Executable.exposure_reference option
    ; context_factory : Context_factory_reference.t
    ; backend_event_stream : Execution_identity.Event_stream_id.t
    ; accepted_cursor : Execution_cursor.t
    ; caused_by : Invocation_attempt_reference.t
    }
end

module Submission_operation_source : sig
  type builder
  type prepared
  type t
  type cursor
  type operation

  type page =
    { operations : operation list
    ; next : cursor
    ; caught_up : bool
    }

  val begin_
    :  sw:Eio.Switch.t
    -> authority:Async_submission_authority.t
    -> mode:Executable_plan.mode
    -> (builder, Submission_prepare_error.t) result
  val append_call
    :  builder
    -> Executable.call
    -> (unit, Submission_prepare_error.t) result
  val seal
    :  builder
    -> (prepared, Submission_prepare_error.t) result
  val abort
    :  builder
    -> (unit, Submission_prepare_error.t) result
  val ordinal : operation -> int64
  val operation_id
    :  operation
    -> Execution_identity.Operation_id.t
  val invocation_id : operation -> Invocation.Id.t
  val execution : operation -> Operation_execution_anchor.t
  val binding : operation -> Executable.reference
  val exposure
    :  operation
    -> Executable.exposure_reference option
  val context_factory
    :  operation
    -> Context_factory_reference.t
  val caused_by : operation -> Invocation_attempt_reference.t
  val input : operation -> Execution_value.t
  val beginning : t -> cursor
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Reader : sig
    type source = t
    type t

    val source : t -> source
    val beginning : t -> cursor
    val read
      :  t
      -> after:cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (page, Read_error.t) result
  end

  module Internal : sig
    val bind_reader
      :  reader:Execution_journal.Reader.t
      -> admission:Read_admission.t
      -> t
      -> (Reader.t, Construction_error.t) result
  end
end

module Submission_receipt_source : sig
  type builder
  type prepared
  type t
  type cursor

  type page =
    { receipts : Operation_receipt.t list
    ; next : cursor
    ; caught_up : bool
    }

  val beginning : t -> cursor
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  module Internal : sig
    val begin_
      :  sw:Eio.Switch.t
      -> protocol:Operation_backend_protocol.t
      -> submission:Execution_identity.Submission_id.t
      -> request_digest:Submission_request_digest.t
      -> operations:Submission_operation_source.t
      -> (builder, Backend_receipt_error.t) result
    val append
      :  builder
      -> Operation_receipt.t
      -> (unit, Backend_receipt_error.t) result
    val seal
      :  builder
      -> accepted_cursor:Execution_cursor.t
      -> (prepared, Backend_receipt_error.t) result
  end
end

module Submission : sig
  module Id = Execution_identity.Submission_id
  module Request_digest = Submission_request_digest

  type reconciliation_ref =
    { submission_id : Id.t
    ; request_digest : Request_digest.t
    }

  type prepared_request
  type durable_request
  type prepared_receipt = Submission_receipt_source.prepared
  type receipt

  val submission_id : durable_request -> Id.t
  val intent_fact : durable_request -> Execution_fact_ref.t
  val request_digest : prepared_request -> Request_digest.t
  val durable_request_digest : durable_request -> Request_digest.t
  val runtime : durable_request -> Async_runtime_reference.t
  val mode : durable_request -> Executable_plan.mode
  val caused_by
    :  durable_request
    -> Invocation_attempt_reference.t
  val operations : durable_request -> Submission_operation_source.t
  val receipt_operations : receipt -> Submission_receipt_source.t

  type backend_rejection_witness = Backend_rejection_witness.t
  type backend_absence_witness = Backend_absence_witness.t

  val backend_rejection_submission : backend_rejection_witness -> Id.t
  val backend_rejection_digest
    :  backend_rejection_witness
    -> Request_digest.t
  val backend_rejection_backend
    :  backend_rejection_witness
    -> Execution_identity.Operation_backend_id.t
  val backend_rejection_reason
    :  backend_rejection_witness
    -> Backend_semantic_rejection.t

  val backend_absence_submission : backend_absence_witness -> Id.t
  val backend_absence_digest : backend_absence_witness -> Request_digest.t
  val backend_absence_backend
    :  backend_absence_witness
    -> Execution_identity.Operation_backend_id.t

  type backend_prepare_outcome =
    | Backend_prepared of prepared_receipt
    | Backend_rejected_before_commit of backend_rejection_witness

  type prepared_rejection
  type definitive_rejection

  val prepared_rejection_submission : prepared_rejection -> Id.t
  val prepared_rejection_digest : prepared_rejection -> Request_digest.t
  val prepared_rejection_reason
    :  prepared_rejection
    -> Backend_semantic_rejection.t
  val rejection_submission : definitive_rejection -> Id.t
  val rejection_digest : definitive_rejection -> Request_digest.t
  val rejection_reason
    :  definitive_rejection
    -> Backend_semantic_rejection.t
  val rejection_witness
    :  definitive_rejection
    -> backend_rejection_witness
  val rejection_prepared : definitive_rejection -> prepared_rejection
  val rejection_fact
    :  definitive_rejection
    -> Execution_fact_ref.t

  type published_receipt =
    { receipt : receipt
    ; publication_event : Execution_fact_ref.t
    }

  type result =
    | Accepted of receipt
    | Rejected_before_commit of definitive_rejection
    | Reconciliation_required of reconciliation_ref

  type reconciliation =
    | Absent_before_commit
    | Backend_prepared_publication_pending of receipt
    | Publication_committed_activation_pending of published_receipt
    | Accepted_prior of published_receipt
    | Rejected_prior of definitive_rejection
    | Publication_uncertain of reconciliation_ref
    | Corrupt of Reconcile_error.t

  type backend_reconciliation =
    | Backend_absent of backend_absence_witness
    | Backend_prepared of prepared_receipt
    | Backend_activated of
        { prepared_receipt : prepared_receipt
        ; publication_event : Execution_fact_ref.t
        }
    | Backend_publication_uncertain of reconciliation_ref
    | Backend_corrupt of Reconcile_error.t

  module Internal : sig
    val prepare_request
      :  runtime:Async_runtime_reference.t
      -> mode:Executable_plan.mode
      -> caused_by:Invocation_attempt_reference.t
      -> operations:Submission_operation_source.prepared
      -> (prepared_request, Submission_prepare_error.t) result

    val validate_backend_receipt
      :  request:durable_request
      -> backend:Execution_identity.Operation_backend_id.t
      -> prepared_receipt
      -> (prepared_receipt, Backend_receipt_error.t) result

    val validate_backend_rejection
      :  request:durable_request
      -> backend:Execution_identity.Operation_backend_id.t
      -> backend_rejection_witness
      -> (prepared_rejection, Backend_rejection_witness_error.t) result

    val validate_backend_absence
      :  request:durable_request
      -> backend:Execution_identity.Operation_backend_id.t
      -> backend_absence_witness
      -> (backend_absence_witness, Backend_absence_witness_error.t) result

    val terminal_preparation
      :  opened:Invocation_opened_fact.t
      -> prepared_rejection
      -> (Invocation_terminal_preparation.t,
          Submission_rejection_binding_error.t)
         result
  end
end

module Submission_backend_request : sig
  type t
  type request = t
  type receipt_builder

  module Durable_header : sig
    type t

    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result
  end

  module Durable_operation : sig
    type t

    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result
  end

  val backend : t -> Execution_identity.Operation_backend_id.t
  val submission_id : t -> Submission.Id.t
  val request_digest : t -> Submission.Request_digest.t
  val runtime : t -> Async_runtime_reference.t
  val mode : t -> Executable_plan.mode
  val caused_by : t -> Invocation_attempt_reference.t
  val intent_fact : t -> Execution_fact_ref.t
  val operations : t -> Submission_operation_source.Reader.t
  val durable_header : t -> Durable_header.t
  val durable_operation
    :  t
    -> Submission_operation_source.operation
    -> Durable_operation.t
  val bind_header
    :  t
    -> Durable_header.t
    -> (unit, Backend_request_binding_error.t) result
  val bind_operation
    :  t
    -> Durable_operation.t
    -> (Submission_operation_source.operation,
        Backend_request_binding_error.t)
       result
  val input_decoder
    :  t
    -> Submission_operation_source.operation
    -> (Execution_value.Decoder.t, Read_error.t) result

  val write_header
    :  t
    -> sink:Canonical_json_stream.Sink.t
    -> (unit, Encode_error.t) result

  val write_operation
    :  t
    -> Submission_operation_source.operation
    -> sink:Canonical_json_stream.Sink.t
    -> (unit, Encode_error.t) result

  val begin_receipt
    :  sw:Eio.Switch.t
    -> protocol:Operation_backend_protocol.t
    -> t
    -> (receipt_builder, Backend_receipt_error.t) result

  val append_receipt
    :  receipt_builder
    -> operation:Submission_operation_source.operation
    -> backend_event_stream:Execution_identity.Event_stream_id.t
    -> accepted_cursor:Execution_cursor.t
    -> (unit, Backend_receipt_error.t) result

  val seal_receipt
    :  receipt_builder
    -> (Submission.prepared_receipt, Backend_receipt_error.t) result

  module Reopener : sig
    type t

    val with_reopened
      :  sw:Eio.Switch.t
      -> t
      -> expected_runtime:Async_runtime_reference.t
      -> expected:Submission.reconciliation_ref
      -> header:Durable_header.t
      -> use:(request -> ('a, 'use_error) result)
      -> (('a, 'use_error) result,
          Backend_request_reopen_error.t)
         result
  end

  module Internal : sig
    val for_backend
      :  reader:Execution_journal.Reader.t
      -> admission:Read_admission.t
      -> protocol:Operation_backend_protocol.t
      -> Submission.durable_request
      -> (t, Construction_error.t) result
  end
end

module Programmatic_submission_rejection_commit : sig
  val commit
    :  writer:Execution_journal.Writer.t
    -> request:Submission.durable_request
    -> Submission.prepared_rejection
    -> (Submission.definitive_rejection,
        Submission_rejection_commit_error.t)
       result
end

module Invocation_terminal_commit : sig
  type committed_preparation

  val invocation_result
    :  committed_preparation
    -> Invocation_result.Committed.t
  val submission_rejection
    :  committed_preparation
    -> Submission.definitive_rejection

  val commit_prepared
    :  writer:Execution_journal.Writer.t
    -> opened:Invocation_opened_fact.t
    -> Invocation_terminal_preparation.t
    -> (committed_preparation,
        Invocation_terminal_commit_error.t)
       result
end

module Commit_action_fact : sig
  type pending =
    { invocation_id : Execution_identity.Invocation_id.t
    ; publication_event : Execution_fact_ref.t
    ; action : Commit_action.t
    }

  type progress =
    | Reconciliation_pending of Submission.reconciliation

  type terminal =
    | Completed
    | Failed of Commit_action_error.t

  type t =
    | Pending of pending
    | Progress of
        { pending_fact : Execution_fact_ref.t
        ; state : progress
        }
    | Terminal of
        { pending_fact : Execution_fact_ref.t
        ; outcome : terminal
        }

  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Commit_action_index_digest : sig
  type t

  val equal : t -> t -> bool
  val encode : t -> string
  val decode : string -> (t, Decode_error.t) result
end

module Commit_action_fact_source : sig
  type t
  type cursor
  type index_generation

  val stream : t -> Execution_stream_reference.t
  val index_generation : t -> index_generation
  val count : t -> int64
  val digest : t -> Commit_action_index_digest.t
  val observed_through : t -> Execution_page_cursor.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
  val beginning : t -> cursor

  type page =
    { facts : Execution_fact_ref.t list
    ; next : cursor
    ; caught_up : bool
    }

  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
end

module Commit_action_quiescent_scan : sig
  type t

  val stream : t -> Execution_stream_reference.t
  val observed_through : t -> Execution_page_cursor.t
  val open_pending_facts : t -> Commit_action_fact_source.t

  module Internal : sig
    val inspect
      :  quiescent:Execution_journal_bootstrap.quiescent
      -> page_size:Positive_int.t
      -> (t, Commit_action_final_scan_error.t) result
  end
end

module Commit_action_repair_requirement : sig
  type t

  val stream : t -> Execution_stream_reference.t
  val observed_through : t -> Execution_page_cursor.t
  val open_pending_facts : t -> Commit_action_fact_source.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_quiescent_scan : Commit_action_quiescent_scan.t -> t
  end
end

module Commit_action_repair_completion : sig
  type t

  val requirement : t -> Commit_action_repair_requirement.t
  val terminal_facts : t -> Commit_action_fact_source.t
  val completed_through : t -> Execution_page_cursor.t
end

module Commit_action_repair_handoff : sig
  type t

  val requirement : t -> Commit_action_repair_requirement.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val prepare
      :  Commit_action_quiescent_scan.t
      -> (t, Construction_error.t) result
  end
end

module Commit_action_repair_release : sig
  type t

  val handoff : t -> Commit_action_repair_handoff.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val after_close
      :  Commit_action_repair_handoff.t
      -> Execution_journal_bootstrap.closed
      -> (t, Construction_error.t) result

    val after_exclusive_reopen
      :  Commit_action_repair_handoff.t
      -> Execution_journal_bootstrap.exclusive_reopen_closed
      -> (t, Construction_error.t) result
  end
end

module Commit_action_terminal_disposition : sig
  type view =
    | No_open_actions of Commit_action_repair_requirement.t
    | Handoff_prepared of Commit_action_repair_handoff.t

  type t

  val view : t -> view
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val no_open_actions
      :  Commit_action_quiescent_scan.t
      -> (t, Construction_error.t) result

    val handoff_prepared
      :  scan:Commit_action_quiescent_scan.t
      -> Commit_action_repair_handoff.t
      -> (t, Construction_error.t) result
  end
end
```

`Commit_action_fact_source.t` is a paged view of the Journal's existing
immutable open-action index at one exact `(stream, index_generation,
observed_through)` snapshot. Quiescence closes producers and fixes that
snapshot atomically; inspection does not try to commit a new manifest after
the writer is quiesced. The source records checked count and index digest, and
each bounded page is verified against both. Requirement, handoff, and
completion retain only these source roots and cursors. Completion lockstep
pages the required open facts and their terminal facts and rejects omission,
duplication, reordering, another generation, or a terminal for an unlisted
pending action. It never rematerializes either side as a list.

The caller-visible `receipt` is the backend receipt; it contains no journal
event that does not exist yet. `published_receipt` is an internal reconciliation
proof pairing that immutable receipt with the actual committed publication
event. This split removes any need to predict, reserve heuristically, or
back-patch a ToolResult event ID.

The Journal writer allocates the authoritative invocation-result event identity
as the occurrence authority, then commits that result and matching
`Commit_action_fact.Pending` in one append transaction. For provider origin the
result is the ToolResult. An uncommitted allocated identity has no fact and
confers no acceptance. Recovery runs only committed pending facts lacking a
linked terminal fact, using the newest linked `Progress` fact as reconciliation
state. `Reconciliation_pending` is deliberately nonterminal.

The authoritative operation backend prepares one submission record and all
child operation intents in one transaction. Prepared operations are durable
but are not claimable until the authoritative OAS publication fact is committed
and the backend receives its exact publication proof.

Before that transaction, the invocation-bound submission authority allocates
each operation ID and its distinct executable invocation ID exactly once, then
converts every in-process call to a durable registered call containing only
those identities, executable identity/revision, ordinal, and canonical encoded
input. Retry reads that same intent; it never reallocates operation or
invocation identities. No closure or process address enters the store.

- `Accepted` proves every listed operation is durable and its authoritative OAS
  publication fact is committed.
- `Rejected_before_commit` proves no operation was accepted.
- If the caller cannot know whether commit happened, the result is
  `Reconciliation_required`; it is never a generic error.
- Retrying uses the same `Submission.Id.t` and reconciles exact prior state.
- A new submission identity must not be generated to “retry” an ambiguous
  submission.
- Reusing a submission identity with a different canonical request digest is
  `Idempotency_conflict`; it never returns the old receipt or overwrites it.

This contract prevents loss of already durable child handles.

Cross-authority publication uses an idempotent prepare/publish/activate
handshake:

```text
OAS Journal commits Submission_intent(durable_request)
backend.prepare_submission(request)
  -> Backend_prepared_publication_pending prepared_receipt_root
OAS Journal commits authoritative publication fact
  -> publication_event
backend.activate_submission(submission_id, digest, publication_event)
  -> operations become claimable
```

The backend transaction stores operations as `Pending_publication`. A worker
cannot claim them before activation. If OAS publication fails, recovery repairs
the same publication from the backend receipt. If activation fails or its reply
is lost, `Publication_committed_activation_pending` drives an idempotent
activation repair. No operation can run in the window where the external
backend has committed but the original provider ToolUse has no durable OAS
result.

The `Submission_intent` commit precedes every backend call. It stores the exact
runtime reference, causal invocation, digest material, and generated operation
identities, giving recovery an exact enumerable set of open submissions and
the runtime/backend revision required to reconcile them. The backend SPI needs
no heuristic orphan scan. If intent commit fails, backend preparation is
forbidden.

For a programmatic submission, the publication fact is a generic
`Submission_accepted` Journal event. For an asynchronous provider Tool, the
publication fact is that invocation's authoritative ToolResult. The generic
executor never invents a ToolResult for a programmatic call.

The request digest is computed incrementally from the canonical durable request
(exact async-runtime ID/revision, `mode`, causal invocation identity, ordered
binding/exposure references, operation/invocation identities, context-factory
references, and canonical inputs). The runtime revision binds the one backend
namespace, so backend identity is derived and validated from that exact
runtime rather than duplicated as another request authority.
The domain-separated digest covers the fixed header plus the operation
manifest's count, encoded-byte count, and digest; no whole request JSON or
operations list is materialized. It is used only for exact idempotency
equality, never for semantic deduplication or scheduling.

`Operation_backend.S.prepare_submission` receives a scoped
`Submission_backend_request.t`, not a raw Journal reader and not only an
unreadable manifest root. It directly exposes the exact submission ID, digest,
runtime revision, mode, causal attempt, committed intent fact, and the backend
ID derived by the runtime-bound protocol. `write_header` emits their stable
bounded canonical representation plus the exact operation-source root for
backend persistence. The encoded backend ID is a checked derivative of the
runtime revision's registered backend package, not a second request authority.
Its bound
`operations` reader pages the exact durable request under runtime read
admission; every opaque operation exposes its ordinal, generated identities,
anchor, binding/exposure, context-factory, causal attempt, and committed input
through typed accessors. `Durable_header` and `Durable_operation` are the
stable OAS-owned codecs for backend persistence. `bind_header` is a
same-callback equality check; it is not a restart constructor and cannot be
called without an already live request.

After restart the only entry is
`Submission_backend_request.Reopener.with_reopened`. The backend supplies its
decoded opaque header, the expected exact runtime revision, and the stored
submission ID/request digest. The reopener is bound to one frozen backend
package and the application runtime's narrow
`Submission_backend_request_route_authority`; it verifies, in order, that the
runtime revision is still registered to the same frozen packed backend
authority and byte-equal durable backend ID,
the header's derived backend/runtime/submission/digest fields match, the intent
fact belongs to the header's exact root stream, and that root route resolves to
the same registered runtime authority. It then point-reads that exact
`Submission_lifecycle_fact.Intent`, verifies its request digest and operation
source root/count/encoded-byte-count/digest, and creates one scoped request
from that committed request. It never scans a Journal or chooses a route by
path, current runtime revision, backend ID substring, or the newest matching
submission.

The route authority is a narrow façade over the runtime's one existing root
route registry; the reopener factory combines it with one explicit immutable
async-runtime/backend registry snapshot. It is not another route table or
persistence authority. It exposes neither
`Execution_journal.Reader.t` nor a filesystem/bootstrap handle to the backend.
The authority and factory are Dune-private runtime wiring; the advanced
backend receives only the already narrowed `Reopener.t`.
Only durable state reconstruction, `reconcile_submission`, ready-claim paging,
and recovery paging receive the reopener, because only those SPI paths may need
to turn persisted request locators back into canonical input. Inside the bracket the backend calls
`bind_operation`, which revalidates source root, ordinal, generated identities,
anchor, and request digest before `input_decoder` can page that operation's
input. `write_operation` emits the same stable operation/root representation
without whole-value materialization. On every success, callback error,
cancellation, route failure, or decode failure the scoped request is closed;
retained operation readers and input decoders fail typed. A missing, closing,
volatile, corrupt, or mismatched route is a typed reopen error and never an
empty request or a backend-local reconstructed `Execution_value.t`. The receipt
builder opens only from that request and derives all immutable fields from the
operation value; a backend supplies only its event-stream/cursor evidence.
The bracket retains only scoped route/reader metadata; each bounded operation
or input page acquires and releases the shared read admission independently, so
a backend cannot pin a global read lease while waiting for its next page.
This makes an external backend restartable without granting general Journal
authority or allowing it to substitute another request source.

The backend returns an untrusted closed
`Submission.backend_prepare_outcome`. A prepared branch carries a backend
receipt source; before OAS may publish, activate, or return it,
`Submission.Internal.validate_backend_receipt` compares it against the exact
durable request and frozen backend identity by lockstep bounded pages.
Submission/digest, operation count and order, ordinals,
operation/invocation/anchor identities, binding,
optional exposure, context-factory, causal attempt, backend event stream, and
cursor/stream consistency must all match; operation IDs and anchors must be
unique. Publication atomically commits that staged receipt manifest, the
submission publication fact, and the caller-visible receipt root; only then is
`Submission.receipt` minted. Any mismatch rejects the whole preparation before
publication, records the typed backend protocol failure, and exposes no
partially validated operation or receipt prefix.

The semantic-rejection branch carries an opaque backend-minted witness bound
to the exact backend, submission ID, request digest, and typed reason. The
reason is `Backend_semantic_rejection.t`, whose closed kind cannot represent a
transport error, storage error, uncertain outcome, protocol corruption, or
temporary admission saturation. Those remain `Backend_prepare_error.t` and can
never be wrapped in a non-commit witness.
authoritative backend obtains it through
`Operation_backend_protocol.rejection_witness` only after its own prepare
transaction has definitively taken the non-commit branch. That protocol value
is frozen to the packed backend ID/revision, so an implementation cannot
supply another backend ID or proof JSON. The parallel
`Operation_backend_protocol.absence_witness` is used only after the
authoritative lookup proves the exact request absent. Both constructors are
public only through the narrow protocol capability, making an external
`Operation_backend.S` implementable without exposing identity minting or
Journal authority.

`Submission.Internal.validate_backend_rejection` first binds the witness to
the durable request and returns an opaque `prepared_rejection`; it does not yet
prove an OAS terminal fact. A programmatic submission passes that value to
`Programmatic_submission_rejection_commit.commit`, which atomically appends
`Submission_rejected_before_backend_commit` and returns the resulting
`definitive_rejection`. A provider-origin async Tool instead converts the same
prepared value to `Invocation_terminal_preparation.t` and the sole Tool
terminal writer calls `Invocation_terminal_commit.commit_prepared`. That one
Journal transaction appends the rejection lifecycle fact, authoritative
invocation result, and matching ToolResult and only then returns both the
committed invocation-result witness and `definitive_rejection`. There is no
interval in which the rejection is durable but the ToolResult is missing, or
vice versa.
`Submission.Internal.terminal_preparation ~opened` additionally proves that
the opened invocation/attempt is byte-equal to
`durable_request.material.caused_by`, belongs to the same Journal stream, and
has the exact provider ToolUse origin carried by the prepared request. A
prepared rejection for another invocation, attempt, stream, or provider call
is a typed binding error before commit; the terminal writer never correlates
two loose opaque values by caller assertion.

A prepare transport/storage `Error`, an invalid witness, or failure or
uncertainty of either Journal commit never yields `Rejected_before_commit`.
It yields the same `Reconciliation_required` identity and inspects the
authoritative backend plus exact pending Journal commit. Later
`Backend_absent` reconciliation carries its own request-bound absence witness;
a bare absence variant cannot erase proof. If the exact rejection transaction
was already committed but its reply was lost, the reducer returns
`Rejected_prior` with that same definitive fact; it does not collapse it to
backend absence or mint another rejection.

### 10.2 Operation receipts and terminal states

```ocaml
module Operation_claim : sig
  type t

  val make
    :  claim_id:Execution_identity.Claim_id.t
    -> receipt:Operation_receipt.t
    -> input:Execution_value.t
    -> fence:Operation_fence.t
    -> (t, Construction_error.t) result

  val id : t -> Execution_identity.Claim_id.t
  val receipt : t -> Operation_receipt.t
  val input : t -> Execution_value.t
  val fence : t -> Operation_fence.t
  val equal : t -> t -> bool
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_execution_start_intent : sig
  type t

  val claim : t -> Operation_claim.t
  val attempt : t -> Attempt.Id.t
  val input_digest : t -> Execution_value_digest.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  claim:Operation_claim.t
      -> attempt:Attempt.Id.t
      -> t
  end
end

module Committed_executable_terminal : sig
  type kind =
    | Succeeded
    | Declared_failure
    | Cancelled
    | Outcome_unknown
    | Recovery_failed

  type t

  val fact : t -> Execution_fact_ref.t
  val invocation : t -> Invocation_reference.t
  val attempt : t -> Attempt.Id.t
  val binding : t -> Executable.reference
  val kind : t -> kind
  val operation_fence : t -> Operation_fence.t
end

module Operation_execution_binding : sig
  type t

  val bind
    :  access:Execution_journal_access.t
    -> Operation_execution_anchor.t
    -> (t, Operation_execution_bind_error.t) result

  val anchor : t -> Operation_execution_anchor.t
end

module Committed_executable_attempt_opened : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val invocation : t -> Invocation_reference.t
  val attempt : t -> Attempt.Id.t
  val binding : t -> Executable.reference
  val claim : t -> Execution_identity.Claim_id.t
  val operation_fence : t -> Operation_fence.t
  val input_fact : t -> Execution_fact_ref.t
  val input_digest : t -> Execution_value_digest.t
end

module Operation_execution_start_reference : sig
  type t

  val anchor : t -> Operation_execution_anchor.t
  val invocation : t -> Invocation_reference.t
  val attempt : t -> Attempt.Id.t
  val binding : t -> Executable.reference
  val claim : t -> Execution_identity.Claim_id.t
  val operation_fence : t -> Operation_fence.t
  val input_fact : t -> Execution_fact_ref.t
  val input_digest : t -> Execution_value_digest.t
  val opened_fact : t -> Execution_fact_ref.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_committed_attempt
      :  scope:Operation_execution_binding.t
      -> intent:Operation_execution_start_intent.t
      -> Committed_executable_attempt_opened.t
      -> (t, Operation_execution_start_error.t) result
  end
end

module Operation_execution_writer_authority : sig
  type t

  val execution : t -> Operation_execution_start_reference.t
  val current_fence : t -> Operation_fence.t
  val established_by : t -> Execution_fact_ref.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val initial
      :  Operation_execution_start_reference.t
      -> t

    val restore
      :  scope:Operation_execution_binding.t
      -> execution:Operation_execution_start_reference.t
      -> current_fence:Operation_fence.t
      -> established_by:Execution_fact_ref.t
      -> (t, Operation_execution_writer_restore_error.t) result
  end
end

module Operation_execution_effect_state : sig
  type t =
    | Absent
    | Open of
        { entry : Effect_entry.t
        ; entry_fact : Execution_fact_ref.t
        }
    | Settled of
        { entry : Effect_entry.t
        ; entry_fact : Execution_fact_ref.t
        ; receipt : Effect_receipt.t
        ; receipt_fact : Execution_fact_ref.t
        }
end

module Operation_execution_terminal_reference : sig
  type kind =
    | Succeeded
    | Declared_failure
    | Cancelled
    | Outcome_unknown
    | Recovery_failed

  type t

  val anchor : t -> Operation_execution_anchor.t
  val attempt : t -> Attempt.Id.t
  val kind : t -> kind
  val operation_fence : t -> Operation_fence.t
  val fact : t -> Execution_fact_ref.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val of_committed_fact
      :  scope:Operation_execution_binding.t
      -> writer:Operation_execution_writer_authority.t
      -> Committed_executable_terminal.t
      -> (t, Terminal_reference_error.t) result
  end
end

module Operation_execution_effect_source : sig
  type t
  type cursor
  type complete_fold

  type entry_evidence =
    { entry : Effect_entry.t
    ; entry_fact : Execution_fact_ref.t
    }

  type settled_evidence =
    { entry : Effect_entry.t
    ; entry_fact : Execution_fact_ref.t
    ; receipt : Effect_receipt.t
    ; receipt_fact : Execution_fact_ref.t
    }

  type recovery_view =
    | No_effects
    | One_open of entry_evidence
    | One_open_after_settled of
        { open_entry : entry_evidence
        ; last_settled : settled_evidence
        ; settled_count : int64
        }
    | Fully_settled of
        { settled_count : int64
        ; last_settled : settled_evidence
        }
    | Outcome_already_unknown of Effect_unknown.t

  type state =
    | Entered of entry_evidence
    | Receipt_committed of settled_evidence
    | Outcome_unknown of Effect_unknown.t

  type page =
    { states : state list
    ; next : cursor
    ; observed_through : Execution_page_cursor.t
    ; caught_up : bool
    }

  val attempt : t -> Attempt.Id.t
  val high_water : t -> Execution_page_cursor.t
  val beginning : t -> cursor
  val read
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Operation_execution_read_error.t) result

  val fold_complete
    :  reader:Execution_journal.Reader.t
    -> admission:Read_admission.t
    -> t
    -> (complete_fold, Operation_execution_read_error.t) result

  val recovery_view : complete_fold -> recovery_view
end

module Operation_execution_journal : sig
  type bound = Operation_execution_binding.t
  type takeover
  type start_absence

  type start_lookup =
    | Absent of start_absence
    | Existing of Operation_execution_start_reference.t

  type takeover_state =
    | Terminal_before_takeover
        of Operation_execution_terminal_reference.t
    | Incomplete_before_takeover

  val inspect_or_seal_start
    :  bound
    -> intent:Operation_execution_start_intent.t
    -> backend_takeover:Operation_fence_takeover.t
    -> (start_lookup, Operation_execution_start_error.t)
       result

  val absence_intent
    :  start_absence
    -> Operation_execution_start_intent.t
  val absence_fence : start_absence -> Operation_fence.t
  val absence_fact : start_absence -> Execution_fact_ref.t

  val inspect_terminal
    :  bound
    -> attempt:Attempt.Id.t
    -> (Operation_execution_terminal_reference.t option,
        Operation_execution_read_error.t)
       result

  val effects
    :  takeover
    -> (Operation_execution_effect_source.t,
        Operation_execution_read_error.t)
       result

  val begin_takeover
    :  bound
    -> previous:Operation_execution_writer_authority.t
    -> current_fence:Operation_fence.t
    -> (takeover, Operation_execution_takeover_error.t) result

  val takeover_cursor : takeover -> Execution_cursor.t
  val takeover_high_water : takeover -> Execution_page_cursor.t
  val takeover_fact : takeover -> Execution_fact_ref.t
  val writer_authority : takeover -> Operation_execution_writer_authority.t
  val takeover_state : takeover -> takeover_state
end

module Operation_terminal_resolver : sig
  type t

  val resolve
    :  t
    -> Operation_execution_terminal_reference.t
    -> (Execution_value.t, Operation_terminal_resolve_error.t) result

  module Internal : sig
    val create
      :  resolve:
           (Operation_execution_terminal_reference.t
            -> (Execution_value.t,
                Operation_terminal_resolve_error.t)
               result)
      -> t
  end
end

module Operation : sig
  module Id = Execution_identity.Operation_id

  type receipt = Operation_receipt.t =
    { execution : Operation_execution_anchor.t
    ; ordinal : int
    ; binding : Executable.reference
    ; exposure : Executable.exposure_reference option
    ; context_factory : Context_factory_reference.t
    ; backend_event_stream : Execution_identity.Event_stream_id.t
    ; accepted_cursor : Execution_cursor.t
    ; caused_by : Invocation_attempt_reference.t
    }

  type handle =
    | Handle :
        { receipt : receipt
        ; executable :
            ('input, 'output, 'failure) Executable_registry.registered
        }
        -> handle

  type decoded_terminal =
    | Succeeded :
        { executable :
            ('input, 'output, 'failure) Executable_registry.registered
        ; output : 'output
        }
        -> decoded_terminal
    | Failed :
        { executable :
            ('input, 'output, 'failure) Executable_registry.registered
        ; failure : 'failure
        }
        -> decoded_terminal
    | Cancelled of Cancellation.cause
    | Outcome_unknown of Effect_unknown.t
    | Recovery_failed of Recovery_error.t

  type terminal =
    | Execution_terminal of Operation_execution_terminal_reference.t
    | Cancelled_before_start of Cancellation.cause

  type worker =
    { claim_id : Execution_identity.Claim_id.t
    ; attempt_id : Attempt.Id.t
    }

  type status =
    | Pending_publication
    | Accepted
    | Waiting_on_predecessor of
        { predecessor : Id.t
        ; reason : Predecessor_barrier.t
        }
    | Claimed of Execution_identity.Claim_id.t
    | Starting of
        { claim_id : Execution_identity.Claim_id.t
        ; attempt_id : Attempt.Id.t
        }
    | Running of
        { claim_id : Execution_identity.Claim_id.t
        ; attempt_id : Attempt.Id.t
        }
    | Cancelling of
        { cause : Cancellation.cause
        ; worker : worker
        }
    | Terminal of terminal

  type state_transition

  val transition_from : state_transition -> status
  val transition_to : state_transition -> status
  val transition_subject : state_transition -> Id.t

  type event_kind =
    | State_transition of state_transition
    | Supervisor_fence_established of Backend_supervisor_fence.t
    | Operation_fence_established of Operation_fence_takeover.t

  type event

  val event_fact : event -> Execution_fact_ref.t
  val event_cursor : event -> Execution_cursor.t
  val event_observed_at : event -> Observation_time.t
  val event_kind : event -> event_kind
  val encode_event : event -> Canonical_json.t
  val decode_event : Canonical_json.t -> (event, Decode_error.t) result

  val event_of_stored
    :  fact:Execution_fact_ref.t
    -> observed_at:Observation_time.t
    -> kind:event_kind
    -> (event, Decode_error.t) result

  type cursor_gap =
    { requested : Execution_page_cursor.t
    ; earliest_available : Execution_page_cursor.t
    }

  type event_page

  val event_page
    :  after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> events:event list
    -> next:Execution_page_cursor.t
    -> earliest_available:Execution_page_cursor.t
    -> caught_up:bool
    -> (event_page, Construction_error.t) result

  val page_events : event_page -> event list
  val page_next : event_page -> Execution_page_cursor.t
  val page_earliest_available : event_page -> Execution_page_cursor.t
  val page_caught_up : event_page -> bool

  type cancellation_result =
    | Cancelled_before_start of
        { operation_id : Id.t
        ; cursor : Execution_cursor.t
        }
    | Cancellation_requested of
        { operation_id : Id.t
        ; cursor : Execution_cursor.t
        }
    | Cancellation_committed_signal_failed of
        { operation_id : Id.t
        ; cursor : Execution_cursor.t
        ; error : Worker_signal_error.t
        }
    | Already_terminal of terminal
    | Worker_ownership_unknown of status
    | Cancellation_reconciliation_required of
        { operation_id : Id.t
        ; cursor : Execution_cursor.t option
        }

  val decode_terminal
    :  resolver:Operation_terminal_resolver.t
    -> handle
    -> terminal
    -> (decoded_terminal, Operation_terminal_load_error.t) result
end
```

The durable constructors cannot represent `Succeeded (Error ...)` or
`Failed (Ok ...)`. `decode_terminal` resolves the exact referenced execution
Journal terminal through its anchor and fact reference, verifies
operation/scope/invocation/attempt/binding/kind and cursor/event identity, then
uses only that binding revision's codec. It never
decodes a fact with the newest available binding.

`Operation_execution_terminal_reference.Internal.of_committed_fact` accepts a
Journal binding, the exact current
`Operation_execution_writer_authority.t`, and one opaque committed terminal
witness. The writer authority keeps the immutable execution-start identity
separate from the fence currently authorized to append to that attempt.
`Internal.initial` is derived from the committed start itself; every later
authority is returned only by a committed takeover marker. Anchor, invocation,
attempt, binding, current fence, kind, stream, cursor, event, and ordinal are
derived and cross-checked from those authorities; callers cannot assemble them
as independent fields. A terminal under a takeover fence is therefore valid
without rewriting the original start fact, while a terminal under any
superseded fence is rejected. `Operation_execution_binding.bind` fails if the
Journal is already bound to another anchor or its physical stream differs from
`Operation_execution_anchor.stream`.

Receipts are canonical serializable facts and never contain closures.
Programmatic callers may retain an existential `handle`, while durable storage
and provider ToolResult projection use the receipt’s executable identity,
revision, two explicit stream identities, and canonical encoded input. The
receipt’s `backend_event_stream` must equal
`Execution_cursor.stream accepted_cursor`. Its
`Operation_execution_anchor.stream` is the distinct execution-Journal stream
allocated and stored with the operation intent in the same backend
transaction. There is no compatibility `event_stream` alias whose meaning
depends on call site.

### 10.3 Facade and backend ownership

`Async_executor` is a typed protocol facade over one injected
`Operation_backend.t`:

```ocaml
module Operation_state : sig
  type expected_publication =
    { submission_id : Submission.Id.t
    ; request_digest : Submission.Request_digest.t
    }

  type activation_proof =
    { submission_id : Submission.Id.t
    ; request_digest : Submission.Request_digest.t
    ; publication_event : Execution_fact_ref.t
    }

  type publication =
    | Publication_pending of expected_publication
    | Publication_activated of activation_proof

  type linked_worker =
    { claim : Operation_claim.t
    ; execution : Operation_execution_start_reference.t
    ; writer : Operation_execution_writer_authority.t
    }

  type active_worker =
    | Start_pending of Operation_execution_start_intent.t
    | Start_linked of linked_worker

  type execution =
    | Ready
    | Waiting_on_predecessor of
        { predecessor : Operation.Id.t
        ; reason : Predecessor_barrier.t
        }
    | Claimed of Operation_claim.t
    | Starting of Operation_execution_start_intent.t
    | Running of linked_worker
    | Cancelling of
        { cause : Cancellation.cause
        ; worker : active_worker
        }
    | Terminal of Operation.terminal

  type repair =
    | No_open_commit_actions
    | Commit_action_handoff_prepared of Commit_action_repair_handoff.t
    | Commit_action_repair_released of Commit_action_repair_release.t
    | Commit_action_repair_completed of Commit_action_repair_completion.t

  type initial_execution =
    | Initially_ready
    | Initially_waiting_on_predecessor of
        { predecessor : Operation.Id.t
        ; reason : Predecessor_barrier.t
        }

  type t

  val prepare
    :  expected_publication:expected_publication
    -> receipt:Operation.receipt
    -> input:Execution_value.t
    -> initial_execution:initial_execution
    -> (t, Construction_error.t) result

  val receipt : t -> Operation.receipt
  val input : t -> Execution_value.t
  val publication : t -> publication
  val execution : t -> execution
  val repair : t -> repair
  val status : t -> Operation.status
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_recovery_subject : sig
  type t =
    | Claimed_subject of Operation_claim.t
    | Starting_subject of Operation_execution_start_intent.t
    | Running_subject of Operation_state.linked_worker
    | Cancelling_subject of
        { cause : Cancellation.cause
        ; worker : Operation_state.active_worker
        }
    | Terminal_repair_subject of
        { terminal : Operation.terminal
        ; handoff : Commit_action_repair_handoff.t
        ; release : Commit_action_repair_release.t option
        }
end

module Operation_recovery_resolution : sig
  type t =
    | Reclaimed of Operation_claim.t
    | Started of
        { replacement : Operation_claim.t
        ; execution : Operation_execution_start_reference.t
        ; writer : Operation_execution_writer_authority.t
        }
    | Replay_ready
    | Resumed of
        { replacement : Operation_claim.t
        ; execution : Operation_execution_start_reference.t
        ; writer : Operation_execution_writer_authority.t
        }
    | Terminal_without_journal of Operation.terminal
    | Terminal_with_journal of
        { terminal : Operation.terminal
        ; disposition : Commit_action_terminal_disposition.t
        }
    | Repair_completed of Commit_action_repair_completion.t
end

module Operation_recovery_decision : sig
  type t

  val subject : t -> Operation_recovery_subject.t
  val backend_takeover : t -> Operation_fence_takeover.t
  val resolution : t -> Operation_recovery_resolution.t

  module Internal : sig
    val claimed_without_start_intent
      :  backend_takeover:Operation_fence_takeover.t
      -> subject:Operation_recovery_subject.t
      -> resolution:Operation_recovery_resolution.t
      -> (t, Operation_recovery_decision_error.t) result

    val after_start_absence
      :  backend_takeover:Operation_fence_takeover.t
      -> subject:Operation_recovery_subject.t
      -> absence:Operation_execution_journal.start_absence
      -> resolution:Operation_recovery_resolution.t
      -> (t, Operation_recovery_decision_error.t) result

    val after_journal_takeover
      :  backend_takeover:Operation_fence_takeover.t
      -> journal_takeover:Operation_execution_journal.takeover
      -> subject:Operation_recovery_subject.t
      -> resolution:Operation_recovery_resolution.t
      -> (t, Operation_recovery_decision_error.t) result

    val after_commit_action_repair
      :  backend_takeover:Operation_fence_takeover.t
      -> subject:Operation_recovery_subject.t
      -> completion:Commit_action_repair_completion.t
      -> (t, Operation_recovery_decision_error.t) result
  end
end

module Operation_transition : sig
  module Idempotency_key : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val encode : t -> Canonical_json.t
    val decode : Canonical_json.t -> (t, Decode_error.t) result
  end

  type t

  val publication_activated
    :  submission_id:Submission.Id.t
    -> request_digest:Submission.Request_digest.t
    -> receipt:Operation.receipt
    -> publication_event:Execution_fact_ref.t
    -> t

  val claimed : Operation_claim.t -> t

  val claim_superseded
    :  previous:Operation_claim.t
    -> replacement:Operation_claim.t
    -> t

  val execution_starting
    :  Operation_execution_start_intent.t
    -> t

  val execution_started
    :  intent:Operation_execution_start_intent.t
    -> Operation_execution_start_reference.t
    -> t

  val predecessor_released
    :  operation_id:Operation.Id.t
    -> predecessor:Operation.Id.t
    -> predecessor_terminal_event:Execution_fact_ref.t
    -> t

  val cancellation_requested
    :  operation_id:Operation.Id.t
    -> fence:Operation_fence.t
    -> Cancellation.cause
    -> t

  val terminal_committed
    :  worker:Operation_state.linked_worker
    -> terminal:Operation.terminal
    -> disposition:Commit_action_terminal_disposition.t
    -> t

  val commit_action_repair_source_released
    :  operation_id:Operation.Id.t
    -> fence:Operation_fence.t
    -> Commit_action_repair_release.t
    -> t

  val recovery_resolved
    :  Operation_recovery_decision.t
    -> t

  val operation_id : t -> Operation.Id.t
  val expected_fence : t -> Operation_fence.t option
  val idempotency_key : t -> Idempotency_key.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result
end

module Operation_reducer : sig
  val apply
    :  current:Operation_state.t
    -> cursor:Execution_cursor.t
    -> Operation_transition.t
    -> (Operation_state.t, Transition_error.t) result
end

module Operation_ready_scan : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t
  val beginning : t -> Execution_page_cursor.t
  val high_water : t -> Execution_page_cursor.t

  val create
    :  backend:Execution_identity.Operation_backend_id.t
    -> beginning:Execution_page_cursor.t
    -> high_water:Execution_page_cursor.t
    -> (t, Construction_error.t) result
end

module Operation_claim_batch_request : sig
  type source =
    | Fixed_scan of Operation_ready_scan.t
    | Tail

  type t

  val id : t -> Execution_identity.Claim_batch_id.t
  val supervisor : t -> Backend_supervisor_fence.t
  val source : t -> source
  val after : t -> Execution_page_cursor.t
  val requested : t -> Positive_int.t
  val max_encoded_bytes : t -> Positive_byte_count.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  module Internal : sig
    val create
      :  id:Execution_identity.Claim_batch_id.t
      -> supervisor:Backend_supervisor_fence.t
      -> source:source
      -> after:Execution_page_cursor.t
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (t, Construction_error.t) result
  end
end

module Operation_claim_page : sig
  type t
  type entry

  val entry_claim : entry -> Operation_claim.t
  val entry_fact : entry -> Execution_fact_ref.t

  val create
    :  request:Operation_claim_batch_request.t
    -> claims:(Operation_claim.t * Execution_fact_ref.t) list
    -> next:Execution_page_cursor.t
    -> observed_through:Execution_page_cursor.t
    -> caught_up:bool
    -> (t, Construction_error.t) result

  val request : t -> Operation_claim_batch_request.t
  val claims : t -> entry list
  val next : t -> Execution_page_cursor.t
  val observed_through : t -> Execution_page_cursor.t
  val caught_up : t -> bool
end

module Operation_recovery_scan : sig
  type t

  val backend : t -> Execution_identity.Operation_backend_id.t
  val beginning : t -> Execution_page_cursor.t
  val high_water : t -> Execution_page_cursor.t

  val create
    :  backend:Execution_identity.Operation_backend_id.t
    -> beginning:Execution_page_cursor.t
    -> high_water:Execution_page_cursor.t
    -> (t, Construction_error.t) result
end

module Operation_recovery_candidate : sig
  type state = Operation_recovery_subject.t

  type t

  val operation : t -> Operation.Id.t
  val state : t -> state
  val observed_fact : t -> Execution_fact_ref.t

  val create
    :  operation:Operation.Id.t
    -> state:Operation_recovery_subject.t
    -> observed_fact:Execution_fact_ref.t
    -> (t, Construction_error.t) result
end

module Operation_recovery_page : sig
  type source =
    | Fixed_scan of Operation_recovery_scan.t
    | Tail

  type t

  val create
    :  source:source
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> candidates:Operation_recovery_candidate.t list
    -> next:Execution_page_cursor.t
    -> observed_through:Execution_page_cursor.t
    -> caught_up:bool
    -> (t, Construction_error.t) result

  val candidates : t -> Operation_recovery_candidate.t list
  val next : t -> Execution_page_cursor.t
  val observed_through : t -> Execution_page_cursor.t
  val caught_up : t -> bool
end

module Operation_backend : sig
  module type S = sig
    type state

    val id
      :  state
      -> Execution_identity.Operation_backend_id.t

    val prepare_submission
      :  state
      -> protocol:Operation_backend_protocol.t
      -> Submission_backend_request.t
      -> (Submission.backend_prepare_outcome, Backend_prepare_error.t) result

    val activate_submission
      :  state
      -> protocol:Operation_backend_protocol.t
      -> submission_id:Submission.Id.t
      -> request_digest:Submission.Request_digest.t
      -> publication_event:Execution_fact_ref.t
      -> (unit, Backend_activation_error.t) result

    val reconcile_submission
      :  state
      -> protocol:Operation_backend_protocol.t
      -> request_reopener:Submission_backend_request.Reopener.t
      -> Submission.reconciliation_ref
      -> (Submission.backend_reconciliation, Backend_reconcile_error.t) result

    val read_state
      :  state
      -> request_reopener:Submission_backend_request.Reopener.t
      -> Operation.Id.t
      -> (Operation_state.t, Lookup_error.t) result

    val request_cancel
      :  state
      -> protocol:Operation_backend_protocol.t
      -> Operation.Id.t
      -> Cancellation.cause
      -> (Operation.cancellation_result, Cancel_error.t) result

    val read_events
      :  state
      -> after:Execution_page_cursor.t
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (Operation.event_page, Read_error.t) result

    val acquire_supervisor_fence
      :  state
      -> protocol:Operation_backend_protocol.t
      -> Writer_owner.Id.t
      -> (Backend_supervisor_fence.t, Fence_error.t) result

    val begin_ready_scan
      :  state
      -> (Operation_ready_scan.t, Read_error.t) result

    val claim_ready
      :  state
      -> protocol:Operation_backend_protocol.t
      -> request_reopener:Submission_backend_request.Reopener.t
      -> request:Operation_claim_batch_request.t
      -> (Operation_claim_page.t, Claim_error.t) result

    val claim_ready_tail
      :  state
      -> protocol:Operation_backend_protocol.t
      -> request_reopener:Submission_backend_request.Reopener.t
      -> request:Operation_claim_batch_request.t
      -> (Operation_claim_page.t, Claim_error.t) result

    val await_ready_after
      :  state
      -> sw:Eio.Switch.t
      -> after:Execution_page_cursor.t
      -> (Execution_page_cursor.t, Read_error.t) result

    val begin_recovery_scan
      :  state
      -> supervisor:Backend_supervisor_fence.t
      -> (Operation_recovery_scan.t, Read_error.t) result

    val read_recovery_candidates
      :  state
      -> request_reopener:Submission_backend_request.Reopener.t
      -> scan:Operation_recovery_scan.t
      -> after:Execution_page_cursor.t
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (Operation_recovery_page.t, Read_error.t) result

    val read_recovery_tail
      :  state
      -> request_reopener:Submission_backend_request.Reopener.t
      -> after:Execution_page_cursor.t
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (Operation_recovery_page.t, Read_error.t) result

    val await_recovery_after
      :  state
      -> sw:Eio.Switch.t
      -> after:Execution_page_cursor.t
      -> (Execution_page_cursor.t, Read_error.t) result

    val take_operation_fence
      :  state
      -> protocol:Operation_backend_protocol.t
      -> supervisor:Backend_supervisor_fence.t
      -> operation:Operation.Id.t
      -> previous:Operation_fence.t option
      -> (Operation_fence_takeover.t, Fence_error.t) result

    val commit_transition
      :  state
      -> protocol:Operation_backend_protocol.t
      -> fence:Operation_fence.t
      -> Operation_transition.t
      -> (Operation.event, Transition_error.t) result
  end

  type t

  val id : t -> Execution_identity.Operation_backend_id.t

  val pack
    :  (module S with type state = 'state)
    -> 'state
    -> t

  module Internal : sig
    val same_authority : t -> t -> bool
    val bind_protocol
      :  identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> t
      -> t
  end
end

module Operation_context_factory : sig
  module Id = Execution_identity.Context_factory_id
  module Revision = Execution_identity.Context_factory_revision

  module type S = sig
    type state

    val create_context
      :  state
      -> Operation.receipt
      -> (Context.t, Context_recovery_error.t) result
  end

  type t =
    | Context_factory :
        { reference : Context_factory_reference.t
        ; implementation : (module S with type state = 'state)
        ; state : 'state
        }
        -> t

  val create
    :  id:Id.t
    -> revision:Revision.t
    -> (module S with type state = 'state)
    -> state:'state
    -> t

  val of_function
    :  id:Id.t
    -> revision:Revision.t
    -> create_context:
         (Operation.receipt -> (Context.t, Context_recovery_error.t) result)
    -> t

  val reference : t -> Context_factory_reference.t
end

module Operation_context_factory_registry : sig
  type t

  val build
    :  Operation_context_factory.t list
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Context_factory_reference.t
    -> (Operation_context_factory.t, Lookup_error.t) result
end

module Operation_scope_factory : sig
  module Id = Execution_identity.Operation_scope_factory_id
  module Revision = Execution_identity.Operation_scope_factory_revision

  module type S = sig
    type state

    val open_journal
      :  state
      -> Operation_execution_anchor.t
      -> (Execution_journal_bootstrap.t, Operation_scope_open_error.t) result
  end

  type t =
    | Scope_factory :
        { reference : Operation_scope_factory_reference.t
        ; implementation : (module S with type state = 'state)
        ; state : 'state
        }
        -> t

  val create
    :  id:Id.t
    -> revision:Revision.t
    -> (module S with type state = 'state)
    -> state:'state
    -> t

  val of_function
    :  id:Id.t
    -> revision:Revision.t
    -> open_journal:
         (Operation_execution_anchor.t
          -> (Execution_journal_bootstrap.t, Operation_scope_open_error.t) result)
    -> t

  val reference : t -> Operation_scope_factory_reference.t
end

module Operation_scope_factory_registry : sig
  type t

  val build
    :  Operation_scope_factory.t list
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Operation_scope_factory_reference.t
    -> (Operation_scope_factory.t, Lookup_error.t) result
end

module Operation_journal_bootstrap : sig
  type t
  type opened

  val durable_create
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> t

  val durable_open
    :  dir:Eio.Fs.dir_ty Eio.Path.t
    -> t

  val equal : t -> t -> bool

  module Internal : sig
    val open_
      :  sw:Eio.Switch.t
      -> cpu_executor:Cpu_executor.t
      -> identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> append_admission:Journal_append_admission.t
      -> backend_id:Execution_identity.Operation_backend_id.t
      -> t
      -> (opened, Operation_backend_open_error.t) result

    val backend : opened -> Operation_backend.t
    val scope_factory : opened -> Operation_scope_factory.t

    val close_and_await
      :  opened
      -> (unit, Operation_backend_close_error.t) result
  end
end

module Async_runtime_config : sig
  module Id = Execution_identity.Async_runtime_id
  module Revision = Execution_identity.Async_runtime_revision

  type t
  type reference = Async_runtime_reference.t
  type worker_ownership =
    | Oas_supervised
    | Embedding_supervised

  val create
    :  id:Id.t
    -> revision:Revision.t
    -> backend:Operation_backend.t
    -> context_factory:Operation_context_factory.t
    -> scope_factory:Operation_scope_factory.t
    -> worker_ownership:worker_ownership
    -> t

  val create_journal
    :  id:Id.t
    -> revision:Revision.t
    -> backend_id:Execution_identity.Operation_backend_id.t
    -> storage:Operation_journal_bootstrap.t
    -> context_factory:Operation_context_factory.t
    -> (t, Construction_error.t) result

  val add_recovery_context_factory
    :  t
    -> Operation_context_factory.t
    -> (t, Construction_error.t) result

  val add_recovery_scope_factory
    :  t
    -> Operation_scope_factory.t
    -> (t, Construction_error.t) result

  val reference : t -> reference
  val id : reference -> Id.t
  val revision : reference -> Revision.t
  val encode_reference : reference -> Canonical_json.t
  val decode_reference
    :  Canonical_json.t
    -> (reference, Decode_error.t) result

  module Internal : sig
    type backend_source =
      | Existing of Operation_backend.t
      | Journal of
          { backend_id : Execution_identity.Operation_backend_id.t
          ; storage : Operation_journal_bootstrap.t
          }

    val backend_source : t -> backend_source
    val worker_ownership : t -> worker_ownership
    val context_factories : t -> Operation_context_factory.t list
    val scope_factories : t -> Operation_scope_factory.t list
  end
end

module Registered_async_runtime : sig
  type t

  val reference : t -> Async_runtime_config.reference

  module Internal : sig
    type backend_ownership =
      | Borrowed
      | Runtime_owned of Operation_journal_bootstrap.opened

    val create
      :  reference:Async_runtime_config.reference
      -> backend:Operation_backend.t
      -> backend_ownership:backend_ownership
      -> worker_ownership:Async_runtime_config.worker_ownership
      -> context_factories:Operation_context_factory.t list
      -> scope_factories:Operation_scope_factory.t list
      -> (t, Construction_error.t) result

    val backend : t -> Operation_backend.t
    val backend_ownership : t -> backend_ownership
    val worker_ownership : t -> Async_runtime_config.worker_ownership
    val context_factories : t -> Operation_context_factory.t list
    val scope_factories : t -> Operation_scope_factory.t list
  end
end

module Recovery_dependencies : sig
  type t

  val empty : t

  val create
    :  tools:Tool_catalog.t
    -> provider_bindings:Provider_binding_registry.t
    -> (t, Construction_error.t) result

  val merge
    :  t
    -> t
    -> (t, Construction_error.t) result

  module Internal : sig
    type view =
      { executables : Executable_registry.t
      ; tool_exposures : Tool_exposure_registry.t
      ; provider_bindings : Provider_binding_registry.t
      }

    val view : t -> view
  end
end

module Execution_registry_snapshot : sig
  type t

  type view =
    { generation : int64
    ; executables : Executable_registry.t
    ; tool_exposures : Tool_exposure_registry.t
    ; provider_adapters : Provider_continuation_registry.t
    ; provider_bindings : Provider_binding_registry.t
    ; async_runtimes : Registered_async_runtime.t list
    ; context_factories : Operation_context_factory_registry.t
    ; scope_factories : Operation_scope_factory_registry.t
    }

  val generation : t -> int64

  module Internal : sig
    val create
      :  generation:int64
      -> executables:Executable_registry.t
      -> tool_exposures:Tool_exposure_registry.t
      -> provider_adapters:Provider_continuation_registry.t
      -> provider_bindings:Provider_binding_registry.t
      -> async_runtimes:Registered_async_runtime.t list
      -> context_factories:Operation_context_factory_registry.t
      -> scope_factories:Operation_scope_factory_registry.t
      -> t

    val view : t -> view
  end
end

module Execution_registry_authority : sig
  type t
  type prepared

  val current : t -> Execution_registry_snapshot.t

  module Internal : sig
    val create : Execution_registry_snapshot.t -> t

    val prepare
      :  t
      -> Execution_registry_snapshot.t
      -> (prepared, Construction_error.t) result

    val publish : prepared -> unit
    val discard : prepared -> unit
  end
end

module Submission_backend_request_reopener_factory : sig
  val create
    :  runtime_authority:Execution_runtime_authority.t
    -> routes:Submission_backend_request_route_authority.t
    -> read_admission:Read_admission.t
    -> registry:Execution_registry_snapshot.t
    -> backend:Operation_backend.t
    -> (Submission_backend_request.Reopener.t,
        Construction_error.t)
       result
end
```

`Async_runtime_config.create_journal` is the ordinary OAS path. It constructs
one application-lifetime Journal-backed operation backend from the explicit
durable backend ID and pure `Operation_journal_bootstrap.t`. This operation
store is not a root execution scope's causal Journal and cannot be supplied as
an `Execution_journal_access.t`. During atomic runtime registration OAS opens
the backend under the application-runtime switch, using the shared CPU
executor only for declared CPU codec work, and retains one packed backend
authority per durable backend ID.

`Operation_journal_bootstrap.Internal.open_` returns one runtime-owned opened
handle containing the packed backend and its private operation-scope factory.
`Registered_async_runtime` retains that ownership handle so application
shutdown can call `close_and_await` after every worker supervisor has joined.
An advanced `Async_runtime_config.create ~backend` registration is `Borrowed`;
OAS never closes the embedding-owned backend. Failed atomic registration closes
every newly opened runtime-owned handle before publishing a new registry
snapshot and reports both primary and cleanup failures.

`create_journal` is always `Oas_supervised`. The advanced `create` call must
choose `Oas_supervised` or `Embedding_supervised` explicitly. OAS starts no
worker for the latter. Its embedding owner obtains one checked
`Embedding_operation_driver.t` from the application runtime and exact registered
async-runtime reference. That façade owns the narrowed operation host,
backend protocol, supervisor fence, identity minting, ordinal-aware scans, and
calls to `Operation_runner`; the embedding never constructs an Internal host,
supplies a claim-batch ID, or uses ambient identity state. `recover_startup`
reaches the fixed scan high-water before returning, then retains a structured
recovery-tail fiber under the supplied switch using
`read_recovery_tail/await_recovery_after`. `run_ready` is legal only while that
tail is live, and `stop` closes and joins it; a fixed startup scan alone is not
the embedding-supervised liveness contract. This ownership
choice is typed configuration, never inferred from backend type, path, or
whether a notification arrived.

The Journal-backed backend also installs one private exact
`Operation_scope_factory` revision. Submission preparation allocates each
operation's `Operation_execution_anchor`, including its dedicated stream, and
persists the backend-owned mapping to its future execution-Journal location in
the same transaction as the operation intent. The factory accepts only that
anchor and resolves only that mapping; it never derives
a directory by concatenating or substring-matching an operation ID. Recovery
reopens the same scope identity and store. The advanced
`Async_runtime_config.create ~backend` path requires an explicit typed scope
factory and retains every old revision referenced by durable receipts.

Two configurations naming the same backend ID must carry equal bootstrap
descriptors or the exact same advanced packed backend authority; a mismatch is
a typed registration conflict. Bootstrap equality is an unforgeable
process-local descriptor-authority comparison. Independently constructing two
descriptors for textually equal paths does not make them equal; revisions that
share one backend must reuse the same abstract bootstrap value. OAS therefore
does not compare, normalize, or substring-match paths to infer backend
identity. The bootstrap supplies no hidden worker count,
polling interval, timeout, scheduling rule, or recovery policy. The application
still names every durable ID/revision and supplies a fresh context function
through `Operation_context_factory.of_function`. `Async_runtime_config.create
~backend` and `Operation_backend.S` remain the advanced embedding path for an
application-owned durable backend.

`Operation_transition.t` is a closed OAS variant for publication activation,
claim ownership, execution-Journal start linkage, cancellation, terminal
linkage, and recovery resolution. Its constructors are private to OAS, while
the canonical codec, accessors, durable `Operation_state`, and pure
`Operation_reducer` are public to backend implementers. The backend persists
only transitions accepted by that reducer under exact current
`Operation_fence.t` equality; it cannot interpret free-form transition names.
`Operation_claim.t` carries the durable receipt, canonical input, fresh claim
identity, and full operation fence but no executable closure.
The advanced backend package also exports the narrowed
`Operation_backend_protocol` construction kit and checked `create` functions
for ready/recovery scans, candidates, and claim pages. These validate backend,
operation, cursor-stream, fence lineage, requested-count, and page-bound
invariants before returning opaque values. They are not re-exported through
the ordinary Agent/Tool façade, but an external `Operation_backend.S`
implementation never needs private record construction or `Obj.magic`.
Every SPI method that may append or reconcile backend state receives that
runtime-bound protocol explicitly: prepare, activate, reconcile, cancel,
fence/claim, and transition commit. `Operation_backend.pack` cannot smuggle it
into existential state after construction, and an implementation may not mint
event/claim identities or observation samples from ambient globals.
`Submission_backend_request_reopener_factory.create` binds the separate route
capability to that packed value's frozen process authority and durable ID plus
one immutable execution-registry snapshot; no constructor accepting only a
backend string/ID is exposed. Construction also proves the route authority and
read admission belong to the supplied runtime authority and that every runtime
revision the reopener accepts maps to the same packed backend in that snapshot;
a reference mapped to another backend is rejected before route access. A
cross-runtime capability mix is a typed construction error.
Restart input access is intentionally not added to
`Operation_backend_protocol`: that would introduce the forbidden
`Operation_backend_protocol -> Submission_backend_request ->
Operation_backend_protocol` module cycle and would grant input access to every
mutating method. The separately injected request reopener is available only to
reconciliation, claim, and recovery reads and mints no backend identity, fact,
fence, or observation.

`Backend_supervisor_fence.t` and `Operation_fence.t` are deliberately
different authorities. The former authorizes fixed-high-water scans and the
atomic allocation or takeover of one operation fence. It never invalidates a
sibling operation already running. Every claim and worker transition carries
the latter, whose source includes the exact backend and operation. Taking a
new fence for operation A changes no compare-and-set authority for operation B.
`take_operation_fence` commits and returns one opaque
`Operation_fence_takeover.t`. Its idempotency key is the exact
`(supervisor fence, operation, previous operation fence option)` tuple.
Commit-success/reply-loss retry with that tuple returns the byte-equal
takeover, including its establishing backend fact; a different current fence
or previous value is a typed conflict. No caller observes only “fence advance
uncertain” and strands the operation without a recovery key.

The backend aggregate is the sole authority for publication, ready/claim
ownership, cancellation-before-start, and links to operation execution. The
operation's `Execution_journal` is the sole authority for invocation, attempt,
effect entry/receipt, recovery marker, and executable terminal facts. The
backend never copies an effect ledger, executable result, or recovery evidence.
`Operation_state.t` therefore retains the expected submission/digest and later
activation proof independently from a small execution-link axis.
`Operation.status` is a read projection only; it is never fed back to the
reducer or stored as a competing authority.
Projection precedence is closed: any execution `Terminal` is terminal
regardless of publication state; every other publication-pending aggregate
projects `Pending_publication`; after activation, `Cancelling`, `Running`,
`Starting`, `Claimed`, and predecessor waiting project their exact execution
state, while `Ready` projects `Accepted`. Thus cancel-before-activate is
displayed terminal both before and after the later proof.
`claim_ready` allocates that claim identity and commits the corresponding
`Ready -> Claimed claim` transition in the same backend transaction that
removes the operation from the ready set. It uses the current supervisor fence
to allocate a fresh per-operation fence; the claim contains that operation
fence, not the supervisor epoch. A predecessor barrier must first
commit `Waiting_on_predecessor -> Ready`; a waiting operation never consumes a
claim. Returning a claim without its durable transition is forbidden.
`predecessor_released` is the only barrier-release transition. The backend
validates, in the same authority namespace, that its exact
`predecessor_terminal_event` is the committed terminal for the stored
predecessor before applying it; no status string or polling observation can
release a barrier.

Every ready-claim page is keyed by one opaque
`Operation_claim_batch_request.t`, including its fresh occurrence ID, exact
supervisor fence, fixed-scan-or-tail source, ordinal-aware page cursor, and
requested count.
`claim_ready` and `claim_ready_tail` commit the claims and canonical page under
that key atomically. Commit-success/reply-loss retry of the same request
returns the byte-equal claims/page even though those operations are no longer
ready; reusing the ID with changed fields is corruption. The supervisor retains
the request until the reply is resolved, so an uncertain claim call is retried
or enters typed supervisor recovery rather than silently abandoning claimed
work. Each returned entry pairs its claim with the exact committed claim fact;
`next` may therefore stop between ordinals of one backend transaction, while
`observed_through` separately proves the event high-water completely examined
by that page. Process crash remains covered by the fixed startup recovery scan.

`publication_activated` is the only transition with no worker epoch. It changes
`Pending_publication` to `Accepted` using the exact submission identity, request
digest, and committed publication event already validated by
`activate_submission`. The backend applies it atomically to every operation in
that submission. Replay of the same proof is idempotent. If cancellation
already changed a `Pending_publication` operation to terminal
`Cancelled_before_start`, replay of that same activation proof is a successful
no-op for that operation and cannot resurrect it; a different proof is
corruption. Claims, execution links, cancellation, recovery, and terminals all require
`Some expected_fence`. `commit_transition` rejects a transition unless that
full backend/operation/owner/epoch value equals the currently installed fence
for that exact operation; comparing only epoch integers or a backend-wide
epoch is forbidden. Supervisor-fence equality is checked only by scan, claim,
and `take_operation_fence` operations and is never substituted for this
worker-fence comparison.
Within one backend namespace, activation and cancellation are linearizable.
Activation first validates the proof for every operation in the submission and
then applies all state changes atomically; a validation/store failure leaves
the whole submission unchanged. In the cancellation-first order activation
advances only the publication axis of an already terminal operation. In the
activation-first order later cancellation advances only the execution axis.

The reducer admits the closed ordinary worker sequence
`Ready -> Claimed claim -> Starting intent ->
Running { claim; execution; writer } -> Terminal`, plus the explicitly typed
cancellation and recovery transitions below. `Operation_state` stores the
complete immutable claim, not only its ID, and the reducer compares its
canonical fields.

Before opening the operation Journal attempt, the worker allocates the attempt
identity and commits `execution_starting` with one
`Operation_execution_start_intent.t`. That intent is derived from the exact
claim and contains its canonical input digest. Only then may the Journal commit
the invocation-open, single input, and attempt-open facts for that same intent.
`Operation_execution_start_reference.Internal.of_committed_attempt` consumes
the intent and committed witness, and verifies anchor, invocation, attempt,
binding, claim, operation fence, input fact, and input digest. The backend
`execution_started` transition is legal only from the matching `Starting`
state. It derives the initial
`Operation_execution_writer_authority.t` from that start and stores all three
parts of the linked worker; the start reference is immutable evidence while
the writer authority is the replaceable append capability.

This durable handshake closes both cross-store start races. A crash before the
Journal start leaves an exact `Starting` intent whose lookup returns absent; a
crash after Journal start but before backend linkage returns the one matching
start reference. Neither case can be mistaken for an operation that never
allocated an attempt.

A terminal transition must carry the active claim, exact execution-start
reference, current writer authority, and exact committed Journal terminal
reference. It also carries one checked
`Commit_action_terminal_disposition.t`: `No_open_actions` is constructible only
from a quiescent fixed scan whose open-pending set is empty, while
`Handoff_prepared` names the nonempty set from that same scan. A bare `None` or
caller assertion cannot close the repair axis. The identical disposition is
required when recovery adopts a terminal that committed before backend linkage,
so that crash window cannot orphan nested async publication. Outside recovery,
the writer authority and terminal reference's
operation fence must equal the active claim fence.
A duplicate intent, reused attempt under another claim, changed input digest,
second start reference, stale operation fence, and terminal from another
anchor or attempt are typed transition failures.

An operation-fence takeover does not depend on elapsed time. If a worker dies
while `Claimed`, a supervisor uses `take_operation_fence` for only that
operation and applies `Reclaimed` with a replacement claim carrying identical
receipt/input and a fresh claim identity. If it dies while `Starting`, recovery
must resolve the exact start intent before choosing `Reclaimed` or `Started`;
it cannot use the plain pre-attempt shortcut. The old operation fence becomes
stale immediately without affecting sibling claims.

A running attempt is never directly re-claimed. After acquiring a strictly
newer backend supervisor fence, a supervisor takes a fixed-high-water
`Operation_recovery_scan` over materialized `Claimed`, `Starting`, `Running`,
`Cancelling`, and terminal commit-action-repair states. Each candidate carries
the exact state fact that placed it in the index. The scan pages by explicit
request size and an ordinal-aware position, then re-reads each candidate and
atomically takes a newer fence for only that operation before acting. This is an
indexed state scan, not event-history search, elapsed-time selection, or
orphan-path discovery.

For a `Starting` subject, recovery calls
`inspect_or_seal_start ~intent ~backend_takeover`. In one Journal CAS this
either returns the existing start or commits an opaque no-start tombstone bound
to the exact intent and new operation fence. Every start-append transition
checks that tombstone, so an old worker cannot commit a start after absence was
observed. A sealed absent start permits `Reclaimed` only when cancellation is
not pending; an existing
start is first converted to its initial writer authority and passed through
`begin_takeover` under the new operation fence. A terminal found by that
takeover is adopted immediately; otherwise its returned writer authority
produces `Started { replacement; execution; writer }` without opening another
attempt. If cancellation is pending, absence becomes
`Cancelled_before_start`, while an existing start remains a cancelling active
worker and must commit its cancellation terminal in the Journal.

For `Running` or start-linked `Cancelling`,
`Operation_runner.recover` reopens the exact
anchor and calls `Operation_execution_journal.begin_takeover` before applying
the binding recovery policy, supplying the linked worker's current writer
authority rather than only its original start. That one Journal transaction
commits the new
execution-fence marker and decides whether an executable terminal existed
before it. Once committed, an append under the previous fence is rejected.
If `Terminal_before_takeover` is returned, recovery links that exact terminal
into the backend and does not invoke the handler, effect reconciler, or a newer
binding. This closes the crash window in which the Journal terminal committed
but the backend terminal-link transition did not.
`begin_takeover` is idempotent on
`(anchor, previous_writer.established_by, current_operation_fence)`.
Commit-success/reply-loss retry returns the same marker cursor and the same
terminal snapshot and writer authority. Reusing that key with a different
prior writer lineage or fence is corruption; it never appends a second marker
or re-runs reconciliation.

Only `Incomplete_before_takeover` reaches the exact
`Recovery_policy`. `Replay_safe` may commit a Journal recovery decision that
returns the backend to `Replay_ready`; a later ordinary claim opens a fresh
attempt under the same anchor. `Journal_resumable_composite` may commit a
replacement claim linked to the same structural execution start and the
takeover's current writer authority.
`External_effect` obtains `Operation_execution_journal.effects` from the
opaque takeover itself. The returned source is fixed to
`takeover_high_water`; `fold_complete` must reach that exact boundary,
validates every effect occurrence and terminal pairing, and returns the sole
opaque recovery view consumed by policy. A live tail, caller-supplied attempt,
or partially read page cannot construct that proof. It never asks for a
destructive “latest” record that could hide earlier effects or infer openness
from sequence proximity. More than one open effect is corruption because the
attempt contract permits at most one. Every entered and settled fold element
retains its exact entry/receipt fact references. `One_open_after_settled` and
`Fully_settled` retain the complete last settled evidence rather than a bare
receipt; policy can therefore construct `Effect_unknown` or link a reconciled
terminal without rescanning, guessing a fact by position, or losing the
durable evidence required by its constructor. Effect reconciliation and any resulting receipt, `Outcome_unknown`, or
`Recovery_failed` terminal commit occur only in the execution Journal. The
backend receives only the resulting
`Operation_execution_terminal_reference.t`; it never stores the receipt or
uncertainty evidence itself.

`Operation_transition.recovery_resolved` accepts only an opaque
`Operation_recovery_decision.t`. A decision without a Journal attempt permits
only the closed pre-attempt resolutions appropriate to its exact subject.
`Terminal_without_journal` is restricted to
`Cancelled_before_start` after `after_start_absence` consumes the no-start
tombstone for the matching Starting/cancelling intent and takeover fence; it
carries no fabricated Journal disposition.
Every `Started`, `Resumed`, `Replay_ready`, or Journal-terminal resolution is
minted by `after_journal_takeover`, which verifies that the backend takeover's
current fence equals the Journal takeover writer authority and that both name
the same operation, prior writer lineage, start, and subject. Thus the backend
cannot encode “recovery complete” from a loose terminal reference or enum
without the terminal-first Journal decision.
`Terminal_with_journal` additionally requires the checked quiescent
commit-action disposition before the terminal link can close.
`Repair_completed` has exactly one construction path:
`after_commit_action_repair`. It accepts only a
`Terminal_repair_subject` whose release is present, verifies that release names
the subject's handoff and that completion closes the same requirement and every
named pending fact, and binds the current backend takeover fence. The reducer
consumes that decision through `recovery_resolved`; there is no parallel
repair-completed transition or idempotency key.

The reducer rejects a resumed claim whose receipt/input/backend differs from
the previous claim or whose operation fence is not the installed current
fence.
`Resumed` is legal only for `Journal_resumable_composite`.
`External_effect` cannot reconstruct a plain OCaml continuation. No effect
fact, or an open entry reconciled as `Absent` with no earlier settled receipt,
may lead to `Replay_ready`. A completed, failed, still-unknown, or
reconciliation-failed effect produces the exact Journal facts and a terminal
reference; settled evidence is never re-executed or treated as absent.

Cancellation does not require an operation claim. The authoritative backend
commits `cancellation_requested` under the current fence for that operation
before signalling any worker. Cancellation of `Pending_publication`,
`Accepted`, or `Waiting_on_predecessor` commits terminal
`Cancelled_before_start` directly and returns it without fabricating a claim.
Cancellation of `Claimed` also commits terminal `Cancelled_before_start`
atomically because no start intent exists.

Cancellation of `Starting` never claims “before start” from backend state
alone. It enters `Cancelling { worker = Start_pending intent }`; the worker or
recovery owner performs the exact Journal start lookup described above.
Cancellation of `Running` enters
`Cancelling { worker = Start_linked { claim; execution; writer } }`. In both
cases a committed Journal start requires a Journal cancellation terminal and
backend reference link. The original worker may settle only while its
operation fence and writer authority remain current. If a newer operation
fence has taken authority,
`terminal_committed` from the old claim is stale and the new owner first uses
the Journal takeover protocol, then links its exact cancellation terminal.
The caller-facing cancellation API does not guess worker ownership or Journal
state from elapsed time.

The checked fence and claim constructors verify backend identity, operation
identity, the receipt's exact execution anchor, claim identity,
backend-stream/cursor consistency, and epoch shape.
`Effect_entry.Internal.create` additionally proves that the invocation, attempt, and
effect identities in `Fencing_token.t` equal the supplied entry fields and
that an async effect's invocation equals the immutable
`Operation_execution_anchor.invocation`, and that its `Execution_fence` equals
the active operation-Journal attempt fence whose source is the anchor's
`(backend, operation)` and whose epoch equals the active claim's exact
operation-fence epoch, while
`Effect_idempotency_key.t` names that same effect.
`Effect_receipt.Internal.create`
copies the exact entry protocol and effect identity; the execution Journal
accepts it only when its `entry_fact` names the committed entry, including its
ordinal, in the
anchor's stream. At most one external effect is open in one attempt. A second
open entry, a receipt without its entry, and an ordinary success/failure
terminal while an effect is open are rejected by the Journal reducer. All
effect and terminal references use `Execution_fact_ref.t`, so another stream's
event/cursor cannot be assembled into evidence.
Digest, epoch, transition, effect, and event codecs are stable durable codecs;
an embedding backend can recover every value it is required to return without
`Obj.magic`, private record construction, or string parsing.

Every transition has a typed idempotency key derived from its closed occurrence
identities, never from a string tag. Recommitting the same canonical transition
returns its original cursor without another event; the same key with different
canonical payload is corruption. Implementations use an indexed unique key,
not a linear history scan.

The backend contract implements atomic durable request preparation,
idempotency conflict detection, publication activation, reconciliation, status
transitions, cancellation, event paging, claims, and fencing. `Async_executor`
owns typed call encoding, exact executable-registry resolution, result
decoding, provider Tool adaptation, and causal/ToolResult journal projection.
Backend `read_state` returns the one durable aggregate; the caller-facing
`status` API is only `Operation_state.status`, never another mutable field.
Backend reconciliation reports only backend facts. A transport, store, or
protocol read failure returns `Error Backend_reconcile_error.t`; it cannot be
collapsed into `Backend_publication_uncertain`, which is reserved for a
successfully read durable state that is itself ambiguous. `Async_executor`
joins a successful backend result with the OAS Journal by exact submission
identity, digest, and publication event to produce
`Submission.reconciliation`; neither side guesses the other store's state.
Failure to read either authority returns
`Error Submission_reconcile_error.t` and appends or emits an exact
`Reconciliation_attempt_failed` observation linked to the submission. It does
not fabricate `Absent_before_commit`, mutate the submission state, or escape
as an untyped exception.

Backend SPI calls are fiber-cooperative. An implementation backed by blocking
system calls must own an explicit offload capability and typed admission
failure; OAS does not infer blocking behavior or submit the whole backend call
to the CPU pool. This keeps database/network waits off CPU workers and prevents
one backend from blocking the server's Eio domain.

OAS may provide a journal-backed backend for OAS-native finite executables. An
embedding application may inject a backend for application-owned long-lived
operations. Exactly one backend is authoritative for an operation namespace;
the root/submission causal Journal does not become a second operation writer.
Every executable operation instead has one exact anchor-bound execution
Journal. The backend owns queue/link state and that Journal owns execution
facts, as defined above.
`Operation_backend.pack` mints an unforgeable process-local authority token in
the abstract package. `Operation_backend.Internal.same_authority` compares only
that token. `pack` also reads `S.id state` exactly once and freezes the value in
the abstract package; every later `Operation_backend.id` and fence check uses
that frozen value, never mutable implementation state. Runtime
revisions that name the same `Operation_backend_id` must reuse the same packed
backend value; independently packing another state value under that durable ID
is rejected during registry construction. The token is never serialized and
never substitutes for exact durable backend identity.
Agent/runtime construction gives every async executor the one checked
`Execution_journal_access.t`; the executor derives its narrow reader and writer
capabilities internally and cannot be constructed from either half alone.
The writer capability commits
submission intent, OAS publication, reconciliation, and causal facts only; it
cannot mutate an embedding backend's operation state or an operation execution
Journal. Ordinary Tool
users never supply or receive the writer.

```ocaml
module Async_executor : sig
  type t

  val create
    :  journal:Execution_journal_access.t
    -> runtime:Registered_async_runtime.t
    -> request_reopener:Submission_backend_request.Reopener.t
    -> executables:Executable_registry.t
    -> t

  val submit
    :  t
    -> mode:Executable_plan.mode
    -> caused_by:Invocation_attempt_reference.t
    -> Submission_operation_source.prepared
    -> Submission.result

  val reconcile_submission
    :  t
    -> Submission.reconciliation_ref
    -> (Submission.reconciliation, Submission_reconcile_error.t) result

  val status
    :  t
    -> Operation.Id.t
    -> (Operation.status, Lookup_error.t) result

  val request_cancel
    :  t
    -> Operation.Id.t
    -> requester:Cancellation.requester
    -> (Operation.cancellation_result, Cancel_error.t) result

  val decode_terminal
    :  t
    -> Operation.handle
    -> Operation.terminal
    -> (Operation.decoded_terminal, Operation_terminal_load_error.t) result

  val read_events
    :  t
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (Operation.event_page, Read_error.t) result
end

module Async_runtime_registry : sig
  type t

  val find
    :  t
    -> Async_runtime_config.reference
    -> (Async_executor.t, Lookup_error.t) result

  val submission_resolver : t -> Async_submission_resolver.t

  val validate_tools
    :  t
    -> Tool_catalog.t
    -> (unit, Construction_error.t) result

  module Internal : sig
    val build
      :  runtime_authority:Execution_runtime_authority.t
      -> journal:Execution_journal_access.t
      -> request_routes:Submission_backend_request_route_authority.t
      -> read_admission:Read_admission.t
      -> snapshot:Execution_registry_snapshot.t
      -> (t, Construction_error.t) result
  end
end

module Async_submission_client : sig
  type t
  type builder
  type prepared

  module Internal : sig
    val create
      :  authority:Async_submission_authority.t
      -> runtime:Async_runtime_config.reference
      -> (t, Construction_error.t) result
  end

  val begin_submission
    :  sw:Eio.Switch.t
    -> t
    -> mode:Executable_plan.mode
    -> (builder, Async_prepare_error.t) result

  val append
    :  builder
    -> Executable.call
    -> (unit, Async_prepare_error.t) result

  val seal
    :  builder
    -> (prepared, Async_prepare_error.t) result

  val abort
    :  builder
    -> (unit, Async_prepare_error.t) result

  val submit_prepared : t -> prepared -> Submission.result

  val reconcile_submission
    :  t
    -> Submission.reconciliation_ref
    -> (Submission.reconciliation, Submission_reconcile_error.t) result

  val status
    :  t
    -> Operation.Id.t
    -> (Operation.status, Lookup_error.t) result

  val request_cancel
    :  t
    -> Operation.Id.t
    -> (Operation.cancellation_result, Cancel_error.t) result

  val decode_terminal
    :  t
    -> Operation.handle
    -> Operation.terminal
    -> (Operation.decoded_terminal, Operation_terminal_load_error.t) result
end

module Submission_intent_repair : sig
  type scan
  type open_intent

  val fact : open_intent -> Execution_fact_ref.t
  val request : open_intent -> Submission.durable_request

  type page =
    { intents : open_intent list
    ; next : Execution_page_cursor.t
    ; observed_through : Execution_page_cursor.t
    ; caught_up : bool
    }

  val begin_scan
    :  Execution_journal.Reader.t
    -> (scan, Read_error.t) result

  val high_water : scan -> Execution_page_cursor.t

  val read_open
    :  Execution_journal.Reader.t
    -> scan:scan
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val tail_open
    :  Execution_journal.Reader.t
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val await_after
    :  sw:Eio.Switch.t
    -> Execution_journal.Reader.t
    -> after:Execution_page_cursor.t
    -> (Execution_page_cursor.t, Read_error.t) result

  val resume
    :  writer:Execution_journal.Writer.t
    -> async_runtimes:Async_runtime_registry.t
    -> open_intent
    -> (Submission.reconciliation, Submission_reconcile_error.t) result
end

module Async_executor_internal : sig
  type tool_preparation =
    | Prepared of
        { receipt : Submission.receipt
        ; after_commit : Commit_action.t
        }
    | Rejected of Submission.prepared_rejection
    | Reconciliation_required of Submission.reconciliation_ref

  val prepare_for_tool
    :  authority:Async_submission_authority.t
    -> runtime:Async_runtime_config.reference
    -> mode:Executable_plan.mode
    -> Submission_operation_source.prepared
    -> tool_preparation
end

module Commit_action_handler : sig
  type outcome =
    | Completed
    | Reconciliation_pending of Submission.reconciliation

  type t
  type dependency =
    | No_async_runtime
    | One_async_runtime of Async_runtime_reference.t

  val reference : t -> Commit_action.reference
  val dependencies
    :  t
    -> payload:Execution_value.t
    -> (dependency, Commit_action_error.t) result

  module Internal : sig
    val create
      :  reference:Commit_action.reference
      -> dependencies:
           (payload:Execution_value.t
            -> (dependency, Commit_action_error.t) result)
      -> run:
            (async_runtimes:Async_runtime_registry.t
            -> publication_event:Execution_fact_ref.t
            -> payload:Execution_value.t
            -> (outcome, Commit_action_error.t) result)
      -> t
  end
end

module Commit_action_registry : sig
  type t

  val build
    :  Commit_action_handler.t list
    -> (t, Construction_error.t) result

  val find
    :  t
    -> Commit_action.reference
    -> (Commit_action_handler.t, Lookup_error.t) result
end

module Async_submission_commit_action : sig
  val reference : Commit_action.reference
  val handler : Commit_action_handler.t

  val create
    :  runtime:Async_runtime_config.reference
    -> receipt:Submission.receipt
    -> Commit_action.t
end

module Commit_action_executor : sig
  val run
    :  handlers:Commit_action_registry.t
    -> async_runtimes:Async_runtime_registry.t
    -> publication_event:Execution_fact_ref.t
    -> Commit_action.t
    -> (Commit_action_handler.outcome, Commit_action_error.t) result
end

module Commit_action_repair_claim : sig
  type t

  val pending_fact : t -> Execution_fact_ref.t
  val owner : t -> Writer_owner.Id.t
  val epoch : t -> Writer_epoch.t
  val encode : t -> Canonical_json.t
  val decode : Canonical_json.t -> (t, Decode_error.t) result

  val read_current
    :  Execution_journal.Reader.t
    -> pending_fact:Execution_fact_ref.t
    -> (t option, Read_error.t) result

  val acquire
    :  writer:Execution_journal.Writer.t
    -> pending_fact:Execution_fact_ref.t
    -> owner:Writer_owner.Id.t
    -> (t, Repair_claim_error.t) result

  val supersede
    :  writer:Execution_journal.Writer.t
    -> previous:t
    -> owner:Writer_owner.Id.t
    -> (t, Repair_claim_error.t) result
end

module Commit_action_repair : sig
  type scan

  val begin_scan
    :  Execution_journal.Reader.t
    -> (scan, Read_error.t) result

  val high_water : scan -> Execution_page_cursor.t

  type open_action =
    { pending_fact : Execution_fact_ref.t
    ; pending : Commit_action_fact.pending
    ; latest_progress : Commit_action_fact.progress option
    ; current_claim : Commit_action_repair_claim.t option
    }

  type page =
    { actions : open_action list
    ; next : Execution_page_cursor.t
    ; observed_through : Execution_page_cursor.t
    ; caught_up : bool
    }

  type requirement =
    { pending_fact : Execution_fact_ref.t
    ; action : Commit_action.reference
    ; payload : Execution_value.t
    }

  type requirement_page =
    { requirements : requirement list
    ; next : Execution_page_cursor.t
    ; observed_through : Execution_page_cursor.t
    ; caught_up : bool
    }

  val read_open
    :  Execution_journal.Reader.t
    -> scan:scan
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val read_requirements
    :  Execution_journal.Reader.t
    -> scan:scan
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (requirement_page, Read_error.t) result

  val tail_open
    :  Execution_journal.Reader.t
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result

  val await_after
    :  sw:Eio.Switch.t
    -> Execution_journal.Reader.t
    -> after:Execution_page_cursor.t
    -> (Execution_page_cursor.t, Read_error.t) result

  val resume
    :  writer:Execution_journal.Writer.t
    -> claim:Commit_action_repair_claim.t
    -> handlers:Commit_action_registry.t
    -> async_runtimes:Async_runtime_registry.t
    -> open_action
    -> (unit, Commit_action_error.t) result
end

module Runtime_capacity : sig
  type t

  val create
    :  cpu_workers:Positive_int.t
    -> cpu_admission_capacity:Positive_int.t
    -> journal_append_admission_capacity:Positive_int.t
    -> journal_lane_queue_capacity:Positive_int.t
    -> journal_group_max_transactions:Positive_int.t
    -> journal_transaction_byte_capacity:Positive_byte_count.t
    -> journal_group_byte_capacity:Positive_byte_count.t
    -> manifest_staging_global_byte_capacity:Positive_byte_count.t
    -> manifest_staging_per_transaction_byte_capacity:Positive_byte_count.t
    -> manifest_record_byte_capacity:Positive_byte_count.t
    -> manifest_page_byte_capacity:Positive_byte_count.t
    -> provider_stream_queue_frame_capacity:Positive_int.t
    -> provider_stream_global_byte_capacity:Positive_byte_count.t
    -> provider_stream_per_attempt_byte_capacity:Positive_byte_count.t
    -> provider_transport_read_chunk_capacity:Positive_byte_count.t
    -> provider_spool_global_byte_capacity:Positive_byte_count.t
    -> provider_spool_per_attempt_byte_capacity:Positive_byte_count.t
    -> provider_spool_page_byte_capacity:Positive_byte_count.t
    -> tool_decode_value_node_capacity:Positive_tool_decode_node_count.t
    -> tool_decode_payload_byte_capacity:Positive_byte_count.t
    -> execution_concurrency:Positive_int.t
    -> operation_concurrency:Positive_int.t
    -> operation_claim_page_size:Positive_int.t
    -> operation_recovery_page_size:Positive_int.t
    -> repair_concurrency:Positive_int.t
    -> repair_page_size:Positive_int.t
    -> read_stream_concurrency:Positive_int.t
    -> read_batch_size:Positive_int.t
    -> read_page_byte_capacity:Positive_byte_count.t
    -> (t, Runtime_capacity_error.t) result

  val cpu_workers : t -> Positive_int.t
  val cpu_admission_capacity : t -> Positive_int.t
  val journal_append_admission_capacity : t -> Positive_int.t
  val journal_lane_queue_capacity : t -> Positive_int.t
  val journal_group_max_transactions : t -> Positive_int.t
  val journal_transaction_byte_capacity : t -> Positive_byte_count.t
  val journal_group_byte_capacity : t -> Positive_byte_count.t
  val manifest_staging_global_byte_capacity
    :  t
    -> Positive_byte_count.t
  val manifest_staging_per_transaction_byte_capacity
    :  t
    -> Positive_byte_count.t
  val manifest_record_byte_capacity : t -> Positive_byte_count.t
  val manifest_page_byte_capacity : t -> Positive_byte_count.t
  val provider_stream_queue_frame_capacity : t -> Positive_int.t
  val provider_stream_global_byte_capacity : t -> Positive_byte_count.t
  val provider_stream_per_attempt_byte_capacity
    :  t
    -> Positive_byte_count.t
  val provider_transport_read_chunk_capacity
    :  t
    -> Positive_byte_count.t
  val provider_spool_global_byte_capacity
    :  t
    -> Positive_byte_count.t
  val provider_spool_per_attempt_byte_capacity
    :  t
    -> Positive_byte_count.t
  val provider_spool_page_byte_capacity
    :  t
    -> Positive_byte_count.t
  val tool_decode_value_node_capacity
    :  t
    -> Positive_tool_decode_node_count.t
  val tool_decode_payload_byte_capacity
    :  t
    -> Positive_byte_count.t
  val execution_concurrency : t -> Positive_int.t
  val operation_concurrency : t -> Positive_int.t
  val operation_claim_page_size : t -> Positive_int.t
  val operation_recovery_page_size : t -> Positive_int.t
  val repair_concurrency : t -> Positive_int.t
  val repair_page_size : t -> Positive_int.t
  val read_stream_concurrency : t -> Positive_int.t
  val read_batch_size : t -> Positive_int.t
  val read_page_byte_capacity : t -> Positive_byte_count.t
end

module Journal_append_admission : sig
  type t
  type lane
  type permit

  val max_transactions_per_group : t -> Positive_int.t
  val transaction_byte_capacity : t -> Positive_byte_count.t
  val group_byte_capacity : t -> Positive_byte_count.t

  module Internal : sig
    val create
      :  sw:Eio.Switch.t
      -> global_capacity:Positive_int.t
      -> lane_capacity:Positive_int.t
      -> max_transactions_per_group:Positive_int.t
      -> transaction_byte_capacity:Positive_byte_count.t
      -> group_byte_capacity:Positive_byte_count.t
      -> t

    val register_lane
      :  t
      -> Execution_stream_reference.t
      -> (lane, Construction_error.t) result

    val acquire_or_await
      :  sw:Eio.Switch.t
      -> lane
      -> (permit, Journal_append_admission_error.t) result

    val release
      :  permit
      -> (unit, Journal_append_admission_error.t) result

    val close_lane_and_await
      :  lane
      -> (unit, Journal_append_lane_close_error.t) result

    val close_and_await
      :  t
      -> (unit, Journal_append_admission_close_error.t) result
  end
end

module Repair_admission : sig
  type t

  module Internal : sig
    val create : capacity:Positive_int.t -> t

    val with_slot
      :  sw:Eio.Switch.t
      -> t
      -> (unit -> ('a, 'failure) result)
      -> (('a, 'failure) result, Repair_admission_error.t) result

    val close : t -> unit
  end
end

module Commit_action_repair_registry : sig
  type t
  type root_prepared
  type root_handoff

  val prepare_root
    :  t
    -> reference:Execution_stream_reference.t
    -> bootstrap:Execution_journal_bootstrap.t
    -> quiescent:Execution_journal_bootstrap.quiescent
    -> handoff:Commit_action_repair_handoff.t
    -> (root_prepared, Commit_action_repair_handoff_error.t) result

  val activate_root
    :  root_prepared
    -> release:Commit_action_repair_release.t
    -> (root_handoff, Commit_action_repair_handoff_error.t) result

  val abort_root
    :  root_prepared
    -> (unit, Commit_action_repair_handoff_error.t) result

  val register_reopened_root
    :  t
    -> reference:Execution_stream_reference.t
    -> bootstrap:Execution_journal_bootstrap.t
    -> (root_handoff option, Commit_action_repair_handoff_error.t) result

  val await_root_completion
    :  root_handoff
    -> (Commit_action_repair_completion.t, Commit_action_error.t) result

  val release_root
    :  root_handoff
    -> completion:Commit_action_repair_completion.t
    -> (unit, Commit_action_repair_handoff_error.t) result

  val repair_operation
    :  t
    -> backend:Operation_backend.t
    -> scope_factory:Operation_scope_factory.t
    -> anchor:Operation_execution_anchor.t
    -> release:Commit_action_repair_release.t
    -> (Commit_action_repair_completion.t, Commit_action_error.t) result

  module Internal : sig
    val create
      :  sw:Eio.Switch.t
      -> identity_source:Identity_source.t
      -> cpu_executor:Cpu_executor.t
      -> repair_admission:Repair_admission.t
      -> registry_authority:Execution_registry_authority.t
      -> handlers:Commit_action_registry.t
      -> page_size:Positive_int.t
      -> page_byte_capacity:Positive_byte_count.t
      -> t

    val close_and_await
      :  t
      -> (unit, Commit_action_repair_close_error.t) result
  end
end

module Operation_admission : sig
  type t
  type lease

  module Internal : sig
    val create : capacity:Positive_int.t -> t

    val acquire_or_await
      :  sw:Eio.Switch.t
      -> t
      -> limit:Positive_int.t
      -> (lease list, Operation_admission_error.t) result

    val release
      :  lease
      -> (unit, Operation_admission_error.t) result
    val close : t -> unit

    val run
      :  sw:Eio.Switch.t
      -> t
      -> lease
      -> (unit -> ('a, 'failure) result)
      -> (('a, 'failure) result, Operation_admission_error.t) result
  end
end

module Runtime_shutdown_report : sig
  type t
  type cursor

  type page =
    { failures : Execution_scope_stop_error.t list
    ; next : cursor
    ; caught_up : bool
    }

  val failure_count : t -> int64
  val beginning : t -> cursor
  val read
    :  t
    -> after:cursor
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (page, Read_error.t) result
end

module Runtime_scope_registry : sig
  type t
  type reservation
  type registration

  val create : unit -> t

  val reserve
    :  t
    -> (reservation, Runtime_stopping.t) result

  val install_stop
    :  reservation
    -> scope:Execution_scope_authority.t
    -> stop:(unit -> (unit, Execution_scope_stop_error.t) result)
    -> (registration, Scope_registration_error.t) result

  val abandon : reservation -> unit
  val deregister : registration -> unit

  val stop_all
    :  t
    -> (Runtime_shutdown_report.t, Runtime_stop_error.t) result
end

module Execution_scope_host : sig
  type t
  type registration
  type clock = Clock : 'clock Eio.Time.clock -> clock

  val runtime_authority : t -> Execution_runtime_authority.t

  module Internal : sig
    val create
      :  runtime_authority:Execution_runtime_authority.t
      -> identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> cpu_executor:Cpu_executor.t
      -> journal_append_admission:Journal_append_admission.t
      -> manifest_store:Execution_manifest_store.t
      -> execution_admission:Execution_admission.t
      -> provider_stream_bytes:Provider_stream_byte_admission.t
      -> provider_stream_queue_frame_capacity:Positive_int.t
      -> provider_stream_per_attempt_byte_capacity:Positive_byte_count.t
      -> provider_transport_read_chunk_capacity:Positive_byte_count.t
      -> provider_spool:Provider_spool_store.t
      -> provider_spool_page_byte_capacity:Positive_byte_count.t
      -> tool_decode_admission:Tool_decode_admission.t
      -> read_admission:Read_admission.t
      -> submission_request_routes:
           Submission_backend_request_route_authority.t
      -> media_stabilizer:Media_stabilizer.t
      -> registry_snapshot:Execution_registry_snapshot.t
      -> repair_admission:Repair_admission.t
      -> commit_action_repairs:Commit_action_repair_registry.t
      -> clock:clock
      -> reservation:Runtime_scope_registry.reservation
      -> t

    val cpu_executor : t -> Cpu_executor.t
    val identity_source : t -> Identity_source.t
    val observation_source : t -> Observation_source.t
    val journal_append_admission : t -> Journal_append_admission.t
    val manifest_store : t -> Execution_manifest_store.t
    val execution_admission : t -> Execution_admission.t
    val provider_stream_bytes : t -> Provider_stream_byte_admission.t
    val provider_stream_queue_frame_capacity : t -> Positive_int.t
    val provider_stream_per_attempt_byte_capacity
      :  t
      -> Positive_byte_count.t
    val provider_transport_read_chunk_capacity
      :  t
      -> Positive_byte_count.t
    val provider_spool : t -> Provider_spool_store.t
    val provider_spool_page_byte_capacity
      :  t
      -> Positive_byte_count.t
    val tool_decode_admission : t -> Tool_decode_admission.t
    val read_admission : t -> Read_admission.t
    val submission_request_routes
      :  t
      -> Submission_backend_request_route_authority.t
    val media_stabilizer : t -> Media_stabilizer.t
    val registry_snapshot : t -> Execution_registry_snapshot.t
    val repair_admission : t -> Repair_admission.t
    val commit_action_repairs : t -> Commit_action_repair_registry.t
    val clock : t -> clock

    val install_stop
      :  t
      -> scope:Execution_scope_authority.t
      -> stop:
           (unit -> (unit, Execution_scope_stop_error.t) result)
      -> (registration, Scope_registration_error.t) result

    val abandon : t -> unit
    val deregister : registration -> unit
  end
end

module Execution_operation_host : sig
  type t

  val runtime_authority : t -> Execution_runtime_authority.t
  val registry_snapshot : t -> Execution_registry_snapshot.t

  module Internal : sig
    val create
      :  runtime_authority:Execution_runtime_authority.t
      -> identity_source:Identity_source.t
      -> observation_source:Observation_source.t
      -> cpu_executor:Cpu_executor.t
      -> journal_append_admission:Journal_append_admission.t
      -> manifest_store:Execution_manifest_store.t
      -> execution_admission:Execution_admission.t
      -> provider_stream_bytes:Provider_stream_byte_admission.t
      -> provider_stream_queue_frame_capacity:Positive_int.t
      -> provider_stream_per_attempt_byte_capacity:Positive_byte_count.t
      -> provider_transport_read_chunk_capacity:Positive_byte_count.t
      -> provider_spool:Provider_spool_store.t
      -> provider_spool_page_byte_capacity:Positive_byte_count.t
      -> tool_decode_admission:Tool_decode_admission.t
      -> operation_admission:Operation_admission.t
      -> read_admission:Read_admission.t
      -> submission_request_routes:
           Submission_backend_request_route_authority.t
      -> media_stabilizer:Media_stabilizer.t
      -> registry_authority:Execution_registry_authority.t
      -> repair_admission:Repair_admission.t
      -> commit_action_repairs:Commit_action_repair_registry.t
      -> scope_registry:Runtime_scope_registry.t
      -> clock:Execution_scope_host.clock
      -> t

    val reserve_scope
      :  t
      -> (Execution_scope_host.t, Runtime_stopping.t) result

    val identity_source : t -> Identity_source.t
    val manifest_store : t -> Execution_manifest_store.t
    val operation_admission : t -> Operation_admission.t
    val provider_spool : t -> Provider_spool_store.t
    val provider_spool_page_byte_capacity
      :  t
      -> Positive_byte_count.t
    val tool_decode_admission : t -> Tool_decode_admission.t
    val submission_request_reopener
      :  t
      -> registry:Execution_registry_snapshot.t
      -> backend:Operation_backend.t
      -> (Submission_backend_request.Reopener.t,
          Construction_error.t)
         result
  end
end

module Repair_trigger : sig
  type t =
    | Startup
    | Pending_action_committed of Execution_fact_ref.t
    | Backend_notification of Execution_identity.Operation_backend_id.t
    | Explicit_reconciliation of Submission.reconciliation_ref
end

module Execution_edge_index_bootstrap : sig
  type t

  val create
    :  id:Execution_identity.Edge_index_id.t
    -> dir:Eio.Fs.dir_ty Eio.Path.t
    -> t
end

module Execution_runtime : sig
  type t
  type root_history_registration

  val create
    :  sw:Eio.Switch.t
    -> clock:_ Eio.Time.clock
    -> mono_clock:_ Eio.Time.Mono.t
    -> domain_mgr:_ Eio.Domain_manager.t
    -> media_stabilizer:Media_stabilizer.t
    -> edge_index:Execution_edge_index_bootstrap.t
    -> manifest_store:Execution_manifest_store_bootstrap.t
    -> provider_spool:Provider_spool_bootstrap.t
    -> capacity:Runtime_capacity.t
    -> (t, Construction_error.t) result

  val build_agent
    :  t
    -> ?recovery_dependencies:Recovery_dependencies.t
    -> async_runtimes:Async_runtime_config.t list
    -> Builder.t
    -> (Agent.definition, Construction_error.t) result

  val register_root_history
    :  t
    -> reference:Execution_stream_reference.t
    -> bootstrap:Execution_journal_bootstrap.t
    -> (root_history_registration, Root_read_route_error.t) result

  val unregister_root_history
    :  root_history_registration
    -> (unit, Root_read_route_error.t) result

  val stop : t -> (unit, Runtime_stop_error.t) result
  val authority : t -> Execution_runtime_authority.t
  val execution_value_authority : t -> Execution_value_authority.t
  val agent_prelude_authority : t -> Agent_prelude_authority.t

  module Internal : sig
    type root_read_route
    type root_read_route_closing

    val reserve_scope
      :  t
      -> (Execution_scope_host.t, Runtime_stopping.t) result

    val cpu_executor : t -> Cpu_executor.t
    val read_admission : t -> Read_admission.t
    val submission_request_routes
      :  t
      -> Submission_backend_request_route_authority.t

    val create_with_extensions
      :  sw:Eio.Switch.t
      -> clock:_ Eio.Time.clock
      -> mono_clock:_ Eio.Time.Mono.t
      -> domain_mgr:_ Eio.Domain_manager.t
      -> media_stabilizer:Media_stabilizer.t
      -> edge_index:Execution_edge_index_bootstrap.t
      -> manifest_store:Execution_manifest_store_bootstrap.t
      -> provider_spool:Provider_spool_bootstrap.t
      -> capacity:Runtime_capacity.t
      -> identity_source:Identity_source.t
      -> provider_adapters:Provider_continuation_adapter.t list
      -> commit_action_handlers:Commit_action_handler.t list
      -> (t, Construction_error.t) result

    val register_root_read_route
      :  t
      -> reference:Execution_stream_reference.t
      -> access:Execution_journal_access.t
      -> bootstrap:Execution_journal_bootstrap.t
      -> (root_read_route, Construction_error.t) result

    val begin_root_scope_close
      :  root_read_route
      -> (root_read_route_closing, Root_read_route_error.t) result

    val await_root_live_readers_drained
      :  root_read_route_closing
      -> (unit, Root_read_route_error.t) result

    val finish_root_scope_close
      :  root_read_route_closing
      -> closed:Execution_journal_bootstrap.closed
      -> (unit, Root_read_route_error.t) result

    val fail_root_scope_close
      :  root_read_route_closing
      -> journal_error:Execution_journal_close_error.t
      -> (unit, Root_read_route_error.t) result
  end
end

module Execution_scope : sig
  type t

  val create
    :  sw:Eio.Switch.t
    -> runtime:Execution_runtime.t
    -> definition:Agent.definition
    -> journal:Execution_journal_bootstrap.t
    -> (t, Execution_scope_create_error.t) result

  val start
    :  t
    -> (unit, Scope_readiness_error.t) result

  val instantiate
    :  t
    -> (Agent.t, Construction_error.t) result

  val build_direct
    :  sw:Eio.Switch.t
    -> runtime:Execution_runtime.t
    -> journal:Execution_journal_bootstrap.t
    -> async_runtimes:Async_runtime_config.t list
    -> Builder.t
    -> (t * Agent.t, Execution_scope_create_error.t) result

  val trigger_repair
    :  t
    -> Repair_trigger.t
    -> (unit, Repair_trigger_error.t) result

  val stop : t -> (unit, Execution_scope_stop_error.t) result

  val runtime_authority : t -> Execution_runtime_authority.t
  val authority : t -> Execution_scope_authority.t
  val journal_opening : t -> Execution_journal_bootstrap.opening
  val stream_reference : t -> Execution_stream_reference.t

  module Internal : sig
    val services : t -> Execution_scope_services.t

    val create_operation
      :  sw:Eio.Switch.t
      -> host:Execution_scope_host.t
      -> claim:Operation_claim.t
      -> journal:Execution_journal_bootstrap.t
      -> (t, Execution_scope_create_error.t) result

    val resume
      :  t
      -> Agent.Internal.checkpoint
      -> (Agent.t, Construction_error.t) result
  end
end

module Operation_runner : sig
  val run_claim
    :  host:Execution_operation_host.t
    -> backend:Operation_backend.t
    -> switch:Eio.Switch.t
    -> Operation_claim.t
    -> (unit, Operation_run_error.t) result

  val recover
    :  host:Execution_operation_host.t
    -> backend:Operation_backend.t
    -> switch:Eio.Switch.t
    -> takeover:Operation_fence_takeover.t
    -> Operation.Id.t
    -> (unit, Operation_recovery_error.t) result
end

module Operation_recovery_trigger : sig
  type cause =
    | Worker_infrastructure_failed
    | Start_link_uncertain
    | Terminal_link_uncertain
    | Commit_action_handoff_prepared of Execution_fact_ref.t
    | Commit_action_source_released of Execution_fact_ref.t
    | Backend_notification
    | Explicit_reconciliation

  type t =
    { operation : Operation.Id.t
    ; cause : cause
    }
end

module Operation_worker_supervisor : sig
  type t
  type staged

  val prepare
    :  sw:Eio.Switch.t
    -> host:Execution_operation_host.t
    -> candidate:Execution_registry_snapshot.t
    -> backend:Operation_backend.t
    -> claim_page_size:Positive_int.t
    -> recovery_page_size:Positive_int.t
    -> page_byte_capacity:Positive_byte_count.t
    -> (staged, Operation_supervisor_start_error.t) result

  val activate_after_publish : staged -> t

  val discard
    :  staged
    -> (unit, Operation_supervisor_stop_error.t) result

  val stop
    :  t
    -> (unit, Operation_supervisor_stop_error.t) result

  val trigger_recovery
    :  t
    -> Operation_recovery_trigger.t
    -> (unit, Operation_recovery_trigger_error.t) result
end

module Embedding_operation_driver : sig
  type t

  type ready_batch

  val operations : ready_batch -> Operation.Id.t list
  val next : ready_batch -> Execution_page_cursor.t
  val caught_up : ready_batch -> bool

  val create
    :  runtime:Execution_runtime.t
    -> async_runtime:Async_runtime_config.reference
    -> (t, Construction_error.t) result

  val recover_startup
    :  t
    -> sw:Eio.Switch.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (unit, Operation_recovery_error.t) result

  val run_ready
    :  t
    -> sw:Eio.Switch.t
    -> after:Execution_page_cursor.t
    -> requested:Positive_int.t
    -> max_encoded_bytes:Positive_byte_count.t
    -> (ready_batch, Operation_run_error.t) result

  val await_ready_after
    :  t
    -> sw:Eio.Switch.t
    -> after:Execution_page_cursor.t
    -> (Execution_page_cursor.t, Read_error.t) result

  val stop
    :  t
    -> (unit, Operation_supervisor_stop_error.t) result
end
```

`read_page_byte_capacity` is the single application-runtime authority for the
encoded metadata bytes returned by operation claim/recovery, commit-action
repair, submission-intent repair, execution read-model, and edge-index pages.
`Execution_runtime` passes it once into `Read_admission`, operation
supervisors, and the repair registry; internal callers obtain the value from
those capabilities and never supply a literal. It is independent from the
Journal transaction capacity, provider stream capacity, and provider-spool
page capacity: those resources have different ownership and failure
semantics, so reusing any of them as a read limit would create an accidental
second policy authority. Item count and encoded metadata bytes are independent
saturation dimensions and every internal page call supplies both.

`Async_submission_client`, `Executable_plan.Internal.run`, and the context
eliminators they consume are Dune-private implementation capabilities. They
are omitted from `Agent_sdk`; an ordinary `Tool.create` handler cannot capture
an independently registered executable, obtain an async client, name an
undeclared async runtime, or prepare backend work. Public synchronous and
asynchronous composition incrementally seal one `Tool_member_catalog` and pass
it to `Tool_batch.expose` or `Async_tool_batch.expose ~runtime`. They compute
the immutable member registry, recovery-policy union, and exact async-runtime
dependency union before Agent finalization. An `Async_tool_batch` dependency contributes
`Durable_required (Async_submission ...)` and must resolve from the scope's
captured registry before readiness. Thus a volatile scope, unknown runtime
revision, or dynamically smuggled child fails before any handler/provider/
backend call; no closure introspection or runtime fallback is involved.

`Execution_runtime.t` is the application-lifetime SSOT. It owns exactly one
runtime-local `Identity_source`, one `Observation_source`, one `Cpu_executor`,
one fair `Journal_append_admission`, the exact
executable/Tool-exposure/provider-adapter/
provider-binding/async-runtime/context-factory/backend authority registries,
the commit-action handler and repair registries, one durable edge projection
index opened from the explicit base-path bootstrap, shared media
stabilization, one global recursive-execution admission, one global operation
admission, one global repair admission, one global read admission, one exact
root-route registry shared by the read model and the narrowed submission
request route authority, and one
global provider-stream byte admission plus immutable per-attempt frame/byte
and transport-read capacities. It owns one atomic Tool-decode admission with
independent exact framework-value-node and copied-payload-byte capacities. It
also owns one typed immutable-manifest
staging store for preludes, selections, recursive plans/results, submissions,
exposure catalogs, repair reports, and shutdown reports, plus one staging-only provider spool
opened from the explicit base-path bootstrap, with separate global,
per-attempt, and page byte capacities. It owns one
worker supervisor per exact OAS-supervised backend
authority. It owns no root-run Journal,
lane writer, execution cursor, top-level Agent run, or per-scope repair scan.
One failed or stopped scope therefore cannot stop or corrupt another scope.

`Execution_runtime.create` validates capacities, exclusively opens the
declared manifest-store and provider-spool IDs/directories, creates its runtime-local
system identity/observation sources, and installs exact provider-adapter revisions and
commit-action handlers from the built-in OAS catalog,
creates the one CPU pool plus global Journal-append,
execution/operation/repair/read and Tool-decode admissions under its inner
switch,
and returns only after that shared host runtime is ready. `build_agent` calls
`Builder.Internal.finalize/view`, takes the exact Tool list from that closed
view, merges its active executable fragments, Tool-exposure authorities,
provider-binding authorities, explicit recovery-only dependencies, and async
configurations into the runtime's
monotonic immutable registries, validates every Tool and provider dependency
against the same runtime authority, resolves the primary provider binding and
its exact adapter package once, then calls `Agent.Internal.create_definition`
with that immutable provider registry view. A recovery bundle registers exact
retired executable, Tool-exposure, and provider-binding revisions for
decode/resume only. Those exposures do not enter the current definition's
`Tool_catalog`, schema, or wire-name set, so retaining an old revision never
re-exposes it or creates an active name collision. Registration is atomic: conflicts
change no registry and do not disturb existing definitions or scopes.
For each candidate async-runtime/backend pair the runtime constructs a scoped
`Submission_backend_request.Reopener.t` from the same packed backend authority,
that unpublished immutable registry snapshot, root-route registry, and read
admission for the staged supervisor. After publication,
`Async_runtime_registry.Internal.build` creates each scope-bound executor's
reopener from that scope's captured immutable snapshot. `Registered_async_runtime`
does not persist a capability that would create a snapshot-construction cycle.
A reused supervisor constructs the next reopener from
`Execution_registry_authority.current` at operation start. Runtime revisions
sharing one packed backend therefore share the same root-route authority but
validate against the exact snapshot used for that call; they do not create
another request or route registry.
An Agent attempt selects one exact `Provider_binding.t`, resolves its adapter
from the definition's captured immutable registry, and retains both packages
through transport, whole-attempt finalization, Journal commit, and
continuation construction. No later phase re-resolves by model text, endpoint,
or provider name.
For every newly introduced `Oas_supervised` backend authority, finalization
uses an `Execution_operation_host.t` backed by the runtime's monotonic
`Execution_registry_authority.t` and calls
`Operation_worker_supervisor.prepare` with the unpublished candidate snapshot.
Preparation performs
fixed ready/recovery scans and validates every durable
`Claimed`/`Starting`/`Running`/`Cancelling` dependency against that candidate,
including reopening every persisted request locator needed by those pages.
After process restart the embedding host must therefore call
`Execution_runtime.register_root_history` for each explicitly retained durable
root before publishing a backend whose recovery index references it. A missing
or mismatched route fails supervisor preparation typed; it is never deferred as
an empty input or repaired from a backend-local copy.
Its preallocated fibers remain behind a closed activation gate and cannot
claim or open a scope. Under one runtime registry transaction OAS publishes the
candidate snapshot and staged-supervisor registrations, then opens their gates;
`activate_after_publish` is infallible and performs no allocation. Preparation
failure discards staged supervisors, closes newly opened runtime-owned
backends, and leaves the old snapshot unchanged. Another runtime revision
sharing that backend reuses the same supervisor; `Embedding_supervised` starts
none. The staged host also cuts the forbidden
`Execution_runtime -> Operation_worker_supervisor -> Execution_runtime`
compilation cycle. A reused supervisor resolves each new claim/recovery against
one atomic `Execution_registry_authority.current` snapshot at operation start;
it never retains the snapshot from its own construction. Registry publication
only adds exact revisions, so an in-flight operation may retain its captured
snapshot while later operations see the new generation.

`Execution_scope.t` is one finite root-Agent execution scope. It owns exactly
one checked Journal access, one semantic lane-writer actor, one scope
authority, one runtime-minted repair owner, and one repair cursor. It borrows the application
runtime's CPU executor, recursive-execution admission, read admission, and
immutable registry snapshot; it cannot close, resize, or replace them. An
embedding application's infinite actor, scheduler,
wake-up loop, and cross-scope timeline remain outside OAS. Such an
application opens a fresh OAS scope for each finite root execution and links
that scope into its own lane with typed causal metadata.

`Execution_scope.create` first verifies that the definition belongs to the
supplied application-runtime authority and captures one immutable registry
snapshot containing every exact dependency visible to that definition.
Before opening the scope, it compares the definition's durability requirement
with `Execution_journal_bootstrap.durability`. A `Durable_required` definition
with a volatile bootstrap fails typed readiness before any provider request,
Tool handler, effect entry, submission intent, or backend preparation can run.
This prevents a prepared operation or external-effect uncertainty from losing
its only enumerable causal authority on process crash.
`Execution_runtime.Internal.reserve_scope` atomically rejects a stopping host
or returns one opaque `Execution_scope_host.t` reservation. That reservation
contains borrowed shared capabilities and immediately counts as an active
construction, so application shutdown cannot pass it unnoticed. It
then owns an inner scope switch and passes the shared `Cpu_executor` to
`Execution_journal_bootstrap.Internal.open_`. The bootstrap opens or creates
the physical store, constructs the semantic Journal reducer and its sole
lane-writer actor, and mints one checked `Execution_journal_access.t`. The
scope acquires that access's exclusive `scope_claim`, constructs the
scope-bound async executors/resolver from the captured host registry, builds
`Execution_scope_services.t` with the borrowed runtime Tool-decode admission,
registers the exact root read route with both
the live checked access and immutable bootstrap, installs its typed stop
operation into the host reservation, and answers its ready handshake. Failure
before publication calls
`Execution_scope_host.Internal.abandon`; successful scope stop deregisters its
registration exactly once.

That one route registration serves both hierarchical inspection and
submission-request reopening. The read-model façade and
`Submission_backend_request_route_authority` are two narrowed capabilities
over the same route entry and close/drain state; registering, closing, or
unregistering one root cannot diverge between them.

The two construction DAGs are:

```text
Runtime_capacity
  -> Identity_source + Observation_source + Cpu_executor
  -> global Journal-append/execution/operation/repair/read/Tool-decode admissions
  -> root-route registry + narrowed submission-request route authority
  -> commit-action repair registry + immutable execution registry authority
  -> Execution_runtime

Execution_runtime + Agent.definition + Execution_journal_bootstrap
  -> Execution_scope_host with narrowed identity/observation/append capabilities
  -> physical Execution_event_store
  -> semantic Execution_journal + Execution_lane_writer
  -> Execution_journal_access + scope_claim
  -> scope-bound Async_runtime_registry
  -> Execution_scope_services
  -> Execution_scope

candidate Execution_registry_snapshot + narrowed shared capabilities
  -> Execution_operation_host
  -> exact-backend Submission_backend_request.Reopener
  -> staged Operation_worker_supervisor
  -> atomic registry/supervisor publication

Execution_registry_authority + exact async-runtime reference
  -> Embedding_operation_driver
  -> structured startup scan + durable recovery tail
```

There is no Journal-to-application-runtime edge and no temporary or second CPU
pool. `Execution_journal_bootstrap.t` contains only the explicit source mode,
directory capability, and optional exact correlation identity; it cannot hold
an already-open store, reader, writer, executor, or callback. The bootstrap is
an internal construction boundary, not another persistence authority.

A scope-construction failure unwinds only that scope's inner switch in reverse
DAG order and returns the exact typed open or construction failure. It does not
close the shared CPU executor or mutate host registries.
`Execution_scope.stop` uses a checked two-phase route/store handshake:

1. `begin_root_scope_close` atomically moves the route from `Live` to
   `Closing`, rejects new live borrows with typed `Root_transitioning`, and
   returns an opaque closing token.
2. `await_root_live_readers_drained` waits only for readers that already hold
   that exact live authority.
3. The scope stops and joins every semantic producer and repair fiber, then
   `quiesce_and_await` closes the lane writer's producer admission, wakes queued
   submitters with typed stopping results, drains every already accepted append,
   and returns an opaque quiescent proof. No new pending action can commit after
   that proof.
4. Using only `quiescent_reader`, the scope performs one fixed open-action scan
   through its exact high-water. It constructs either the checked empty
   disposition or one handoff for the complete nonempty fact set. A durable root
   calls `prepare_root` with that same quiescent proof. Only then does
   `close_and_await` accept the proof, close the store, and return an opaque
   `closed` proof bound to the opened Journal.
5. Any prepared repair handoff is activated with a release derived from that
   same closed proof. `finish_root_scope_close` verifies the proof matches the route and publishes
   either the exact durable read-only bootstrap or typed volatile-unavailable
   state.

If Journal close fails, `fail_root_scope_close` publishes a typed
`Close_failed` route containing that exact cause; it never leaves `Closing` or
points at a closed reader. Route-drain failure likewise transitions to an
explicit failed state before returning. Cleanup failure remains a typed
scope-stop failure and is never replaced by generic cancellation.

`Execution_runtime.stop` is explicit application shutdown. It atomically enters
`Stopping` so new scopes, provider attempts, routes, and inspector sessions are
rejected. It closes operation/repair/execution admissions to wake queued work
with typed stopping errors, calls the nonblocking
`Tool_decode_admission.Internal.close` to wake every queued decoder with a
typed stopping result, cancels scope-owned provider stream queues so both
network and parser fibers wake, and stops every operation worker supervisor.
It calls
`Read_admission.close_and_await` so queued read sessions wake and admitted
reads drain. It then requests every registered scope to perform the route/store
handshake above, waits for its lane to quiesce and close, and unregisters that
lane only after every append permit/ticket settles. Once all scope-owned
decode handlers have released their exact leases, it calls
`Tool_decode_admission.Internal.close_and_await` and verifies both accounted
dimensions are zero. It next drains and closes
the detached `Commit_action_repair_registry` and runtime-owned operation
backends. Only after all scope/backend lanes are gone does it call
`Journal_append_admission.close_and_await`; closing that admission earlier is
forbidden because it could prevent an already accepted scope drain. After every
provider queue, ingress reservation, taken frame, and retained accumulator has
settled, it first calls
`Provider_spool_store.Internal.close_and_await`; this wakes selection/request/
finalization waiters, joins every lease cleanup, removes only this store ID's
uncommitted staging generations, and proves that no committed fact references
the staging directory. It then calls
`Provider_stream_byte_admission.Internal.close_and_await`; this wakes any
remaining byte waiter and proves that the global byte count is zero. The CPU
durable edge index then closes and the CPU executor closes last. Spool open
validates its stable ID/manifest under an exclusive lock; it never discovers a
path from environment variables or provider names. Every stage
contributes to one typed
aggregate; a failure in one scope never short-circuits cleanup of another, and
caller-owned switches are not relied on to wake runtime admission waiters.

The registered stop function is an OAS-private process-local lifecycle
capability created by `Execution_scope`; it is not a user callback, durable
fact, Tool handler, or registry extension point. `Execution_runtime` stores it
behind the abstract registration type and never depends on
`Execution_scope.t`, keeping the compilation graph acyclic.

`Execution_scope.journal_opening` exposes the exact typed
create/open/recovery evidence; durable recovery is never collapsed into a
boolean, log string, or silent success. Reader and writer are always derived
from the one checked Journal access. Scope creation does not start operation or
commit-action recovery.

The application runtime stores immutable async-runtime configuration and
backend authority values, not Journal-bound executors. At scope construction,
`Async_runtime_registry.Internal.build` receives the captured exact host
snapshot and checked Journal access. It constructs only scope-bound executor
facades, derives the writer passed to each `Async_executor`, and binds that same
access into the scope's one `Async_submission_resolver`; a writer alone can
never reconstruct or claim to represent the checked reader/writer authority.
It rejects duplicate `(runtime ID, revision)` implementations while retaining
explicit older revisions required by durable facts. It also groups
configurations by `Operation_backend.id` and rejects any group whose packed
values fail `Operation_backend.Internal.same_authority`; multiple revisions may
share a namespace only by sharing its one application-runtime authority
package.

`find` is exact typed-reference lookup; an async Tool cannot capture a
pre-finalization executor or silently fall back to another runtime.
`validate_tools` reads the private immutable dependency metadata and rejects
every missing runtime revision before a scope can start. A definition
registered after scope construction is not silently injected into that live
scope; instantiating it there is an exact authority/snapshot-generation error.
A new finite scope must be opened instead.

`Execution_scope.start` first drains
`Submission_intent_repair.begin_scan/read_open` to its immutable high-water,
arms its tail, and fails readiness on any gap, unknown runtime revision, or
unresolved reconciliation error. It then calls
`Commit_action_repair.begin_scan` once and
traverses `read_requirements` to that scan's immutable `high_water` with
`Runtime_capacity.repair_page_size`. It fails readiness on a cursor gap or read
error. For every requirement it resolves the exact action handler and invokes
that handler's typed `dependencies` decoder on the payload, then verifies every
returned async-runtime revision. It launches the scope repair supervisor from
the scan's `high_water`. `tail_open ~after:high_water` reads facts
strictly after that fact position and returns bounded pages with the same monotonic `next` and
`caught_up` law as the fixed scan, but against the reader's current durable
tail. After a page reports `caught_up`,
`await_after ~after:page.observed_through`
cooperatively blocks until the Journal's durable high-water is strictly greater
than that page position, including commits made by another process. Its implementation
must recheck durable high-water when arming the notification so a commit cannot
fall between the check and wait. A retained-history gap or notification/store
failure is a typed read failure, never an empty page or elapsed-time retry.
Actions committed during readiness are therefore consumed without an
unscanned window. Missing configuration is a readiness failure; it does not
append terminal failure to an open action. All scope repair work passes through
the application runtime's one `repair_concurrency` admission capability, so
adding scopes does not multiply server-wide repair concurrency.
`instantiate`, `resume`, and `trigger_repair` reject a scope that has not passed
readiness.

This readiness law applies equally to root scopes and
`Execution_scope.Internal.create_operation`; an operation runner cannot execute
the outer binding until its Journal's existing commit actions have been
scanned and the repair tail is armed. Before any scope closes, it first obtains
the quiescent proof described above and then performs one fixed-high-water
open-action scan. If none remain, the checked terminal disposition proves that
fact. If open actions remain, the current owner creates one
`Commit_action_repair_handoff.t` against the quiescent scan:

- a durable root calls `prepare_root`, which records the handoff but cannot
  open another reader/writer. Only after the original lane writer drains, its
  scope claim is released, and `close_and_await` returns the matching closed
  proof does `activate_root` accept
  `Commit_action_repair_release.after_close` and reopen the repair lane. A
  volatile root cannot be detached and its close returns a typed
  unresolved-repair failure;
- an operation scope includes the prepared handoff in
  `Operation_transition.terminal_committed` through its checked disposition.
  Recovery that adopts an already committed Journal terminal must perform the
  same quiescent scan and supply the same disposition. The backend may expose terminal
  status to callers, but its recovery index retains
  `Terminal_repair_subject`. After the original operation scope closes, the
  runner commits the fenced `commit_action_repair_source_released` transition
  and sends an exact wake trigger;
- if the process crashes between physical close and the release transition,
  restart recovery may prove release by successfully taking the exact
  Journal's exclusive open/claim once and closing that proof handle completely.
  Only the resulting `exclusive_reopen_closed` witness can construct the
  release; recovery then commits that same idempotent
  release transition. In the live process an already-claimed result waits for
  the explicit close signal; it is not retried by time;
- only a released subject reaches
  `Commit_action_repair_registry.repair_operation`, which reopens the exact
  anchor through its stored scope-factory revision and runs only the bounded
  repair lane. It holds no operation-execution or recursive-execution slot;
- `register_root_history` scans the reopened durable root and registers any
  open requirement before publishing the history route, so restart does not
  depend on an in-memory handoff.

Completing every named pending fact produces a checked
`Commit_action_repair_completion.t`. It has no exposed constructor: the repair
registry creates it only while holding the exact release and reopened reader,
after a verified closed scan proves a one-to-one terminal link for every fact in
the handoff requirement. A root handle can be released only with that matching
completion. The backend consumes it through the
sole fenced
`Operation_recovery_decision.after_commit_action_repair` path and
`recovery_resolved` transition, sets
`repair = Commit_action_repair_completed completion`, and removes that exact
recovery-index entry.
The root registry likewise releases its exact handle. A terminal outer operation therefore
cannot orphan a nested `Async_tool_batch` activation. Crash after nested
pending commit, reply loss, outer terminal, scope close, and process restart
all converge on the same pending fact and activate it at most once. This is
durable recursive-async ownership, not a timer, lease, polling guess, or reason
to keep the outer Keeper blocked.

`Execution_scope.build_direct` is the ordinary façade: it calls
`Execution_runtime.build_agent`, creates one scope from the resulting
definition, starts that scope, and instantiates the Agent. It returns both the
scope and Agent so the caller cannot lose the lifetime handle. No Agent Builder
constructs its own async registry, executor pool, backend authority table,
Journal access, lane writer, or recovery scan.

`Async_submission_commit_action.create` is Dune-private. It canonical-encodes
the runtime reference, submission identity, request digest, and backend receipt
under the single typed `Async_submission_commit_action.reference` definition;
the ID/revision is not repeated as a free string literal.
`Commit_action_registry.build` rejects duplicate exact references and retains
every handler revision still named by an open durable action.
`Commit_action_executor.run` resolves only the envelope's exact reference,
invokes that handler, resolves the finalized runtime, and performs idempotent
activation with the supplied committed publication event. It never selects the
newest handler. An unknown action reference, unavailable runtime, or malformed
payload is a typed recovery failure. No Tool name, output text, or payload-shape
classification participates.

The application-lifetime OAS recovery supervisor pages
`Commit_action_repair.read_open` under a fixed scan high-water with the same
explicit page-size owner input on startup, then continues from that high-water
with `tail_open`. Every local append transaction that creates
`Commit_action_fact.Pending` sends
`Repair_trigger.Pending_action_committed pending_fact` only after the commit
returns the exact fact reference, including its ordinal; the trigger wakes the
same supervisor and drains from its last durable page cursor. This local trigger is
a latency optimization;
`await_after` is the cross-process, lost-wakeup-safe source, so the supervisor
neither busy-spins after `caught_up` nor misses a remote append. Trigger
delivery is not completion authority: the pending fact remains the SSOT,
duplicate triggers join the same in-flight repair, a crash is covered by the
next fixed startup scan, and trigger failure is recorded as an observable
runtime error rather than dropping the fact. The supervisor may also call
`resume` after an explicit backend notification or reconciliation request.
Before invoking a handler it reads the exact current durable claim. `acquire`
is idempotent for the same owner and fails for a different current owner;
explicit takeover passes the decoded current claim to `supersede`, which
commits a strictly greater epoch. The resulting claim is passed to `resume`.
`resume` appends `Progress` while activation remains
uncertain and appends exactly one terminal fact after completion or
unrecoverable protocol corruption. OAS supplies no poll interval, lease
duration, or elapsed-time liveness guess; an embedding runtime may schedule
reconciliation without blocking unrelated execution.
Deploying a new commit-action revision adds its handler to the application
registry; it does not remove an older handler while an open pending/progress
fact references that revision.

The supervisor admits at most one local active repair per exact `pending_fact`
and
bounds distinct repairs by `Runtime_capacity.repair_concurrency`. Concurrent
startup, backend-notification, and explicit triggers join that same in-flight
repair rather than issuing duplicate backend calls. Across processes, the
Journal claim is authoritative: a second owner cannot execute while the claim
is current. Takeover is an explicit `supersede` with a strictly greater durable
epoch and exact previous claim, never a time lease. Every progress or terminal
append compares the complete repair claim. Journal append also enforces a
unique terminal link per exact `pending_fact`; identical terminal replay returns
the existing fact, while a different terminal is corruption. Missing
handler/runtime configuration is not terminal protocol corruption: readiness
fails and the action remains open so a corrected binary can resume it.

`Async_runtime_config.create` installs one default context-factory revision for
new operations. `add_recovery_context_factory` retains older exact revisions
needed by already durable receipts. Finalization builds one immutable factory
registry and rejects duplicate `(id, revision)` implementations. A missing
factory revision is `Recovery_failed`; the newest factory is never substituted.
The async runtime revision binds the operation-backend namespace, default
context-factory reference, and executable-resolution contract. Changing any of
those requires a new runtime revision. The submission activation action uses
the single exact global `Async_submission_commit_action.reference`; it is not a
second configurable identity inside each runtime revision. Executor capacity
and other owner-supplied resource settings are not durable behavioral identity.

`Operation_runner.run_claim` is the only generic bridge from a durable
`Operation_claim.t` to an executable implementation. It borrows a fresh scope
reservation from the supplied narrow `Execution_operation_host.t`, resolves
the exact binding, context-factory, and operation-scope-factory revisions from
that host's one immutable staged/published registry snapshot, and asks the
scope factory to open the receipt's exact
`Operation_execution_anchor`. It then calls
`Execution_scope.Internal.create_operation`, which validates the claim,
anchor, narrow scope host authority, and bootstrap before binding the resulting Journal
and minting a checked
operation-local `Execution_scope_services.t`. It reconstructs
the root `Invocation_reference.t` from the anchor with
`origin = Async_operation operation_id`; the cross-scope
relationship to `receipt.caused_by` is a typed causal edge, not a false
structural parent.

Before opening the executable attempt, the runner allocates its attempt ID,
derives one `Operation_execution_start_intent.t` from the claim, and commits
backend `execution_starting` under the claim's operation fence. The operation
Journal then commits the invocation-open fact, single canonical input fact,
and executable-attempt-open fact for that exact intent, and mints an
`Operation_execution_start_reference.t`. Only after that durable start exists
does the backend commit `execution_started`, which installs the initial writer
authority derived from the start. Failure or reply uncertainty at one of these
boundaries leaves an exact `Claimed` or `Starting` state for the supervisor
recovery path; it never reports a completed start or opens a second attempt.
It then creates a fresh context,
acquires one recursive-execution slot for the exact opened attempt, and uses
`Execution_context.Internal.create` with that slot, the worker's structured
switch, and an invocation-bound `Async_submission_authority`, plus a child-Agent
authority built from those operation-local services, slot, switch,
application-runtime clock, and exact owning attempt,
commits effect and executable-terminal facts only through the operation
Journal, derives the opaque terminal reference from the committed witness, and
commits only that link through the backend under the claim's operation fence.
It closes the operation scope only after the terminal link or a typed
recovery-required result has been handed to the supervisor. It never borrows
the closed submission Tool's scope or Journal.

An OAS-native backend calls it from the application-runtime operation worker
supervisor. A host-owned backend may call it from its own lane/worker policy,
but must pass the same `Execution_runtime.t` whose exact registrations were
used for submission. Neither mode gives OAS ownership of host wake, fairness,
actor lifecycle, or terminal storage.

`Operation_runner.recover` is the corresponding explicit writer-takeover path.
It reads the complete `Operation_state`, verifies the opaque backend takeover
and proves that its current fence is the backend's strictly newer authority
where takeover is required, resolves the exact
binding, context-factory, scope-factory, and recovery-policy revisions, reopens
the same operation execution scope, and binds the exact anchor. For a
`Claimed` subject it installs a replacement claim under the new operation
fence. For `Starting` or start-pending `Cancelling`, it calls
`inspect_or_seal_start ~intent ~backend_takeover` and applies the exact
sealed-absent/existing resolution
described above; an existing start enters the same takeover protocol before
the new owner can append. For `Running` or start-linked `Cancelling` it passes
the stored current writer authority into that takeover protocol and applies
the terminal-first rule described above. It never converts
“has not changed recently” into
ownership, never replays an open external effect, never borrows the prior
finite submission scope, and never substitutes a newer executable, factory, or
effect-protocol revision.

`Operation_worker_supervisor.prepare` acquires one backend supervisor fence
behind its closed activation gate and first captures one immutable recovery
scan/high-water. After atomic registry publication,
`activate_after_publish` opens that gate without allocation. The supervisor
then pages every materialized
`Claimed`, `Starting`, `Running`, `Cancelling`,
`Commit_action_handoff_prepared`, and `Commit_action_repair_released`
candidate through the exact recovery path in pages bounded by
`operation_recovery_page_size` before claiming new work. It then continuously
drains `read_recovery_tail` from that exact high-water. After a tail page catches
up, `await_recovery_after` performs the durable high-water recheck while arming
its notification, so another process can create a recovery candidate after
startup without requiring a local callback or process restart. It takes a fresh
operation fence separately for each candidate; this
cannot stale a sibling worker. A cursor gap or corrupt recovery index fails
that supervisor; it is never treated as no recovery work. The supervisor then
captures one immutable ready scan/high-water and pages current ready operations
before tailing strictly after that high-water. Before each claim page it calls the
application runtime's one atomic `Operation_admission.acquire_or_await`, which
either returns at least one and at most `operation_claim_page_size` leases or
returns a typed close/cancellation failure. The availability check and waiter
registration are one mutex-protected state transition, so a release cannot
occur between them and become a lost wakeup. The supervisor requests no more
claims than leases already held.
Unused leases are released immediately, and each returned claim transfers one
lease to exactly one structured worker fiber until `Operation_runner` and its
operation scope settle. When the durable ready tail is caught up it uses
`Operation_backend.S.await_ready_after`. Neither path polls or guesses an
interval. Runtime shutdown closes admission before stopping supervisors,
waking every waiter with the same typed closed result.

After startup, a worker infrastructure failure or uncertain start/terminal
link calls `trigger_recovery` with the exact operation. Backend notifications
and explicit operator reconciliation enter the same path. Concurrent triggers
for one operation join one in-flight recovery; different operations share the
global operation admission and take independent operation fences. The trigger
always re-reads durable state before acting, so delivery is not authority.
Trigger delivery failure is returned and observed while the durable recovery
tail remains the cross-process liveness authority. The trigger is only a local
latency optimization. No operation-local polling fiber, age threshold, or
timeout is introduced.

Activation, predecessor release, cancellation repair, and recovery can all
advance the backend ready stream. The backend's await implementation rechecks
its durable high-water while arming notification, so a cross-process ready
transition cannot be lost. Local notification is only a latency optimization.
A worker's domain failure closes that operation and releases its lease; an
infrastructure failure is recorded against that backend/supervisor and does not
cancel root Agent scopes or workers for a different backend. A backend read gap
or corrupted ready index is a typed supervisor failure requiring operator
repair, never an empty queue. Explicit application-runtime shutdown stops new
claims and durably leaves nonterminal operations resumable by the next owner.

`Async_executor` and `Async_runtime_registry` are Dune-private scope wiring.
`Async_submission_client` is likewise the private adapter façade created from
`Execution_context.Internal.async_submissions` and one exact runtime
reference. Its incremental `begin_submission`/`append`/`seal` path derives the causal
attempt, immediately writes each canonical operation, and mints the submission
occurrence from the runtime-owned identity source. Callers never supply or
parse a `Submission.Id.t`. The returned opaque token binds that identity,
operation-manifest root, digest material, causal attempt, and runtime revision
before any backend effect. Retrying `submit_prepared` reuses only that token.
The ordinary public entry is `Async_tool_batch.expose`, which owns this adapter
and its declared runtime dependency.

The programmatic facade performs submission in this order:

1. mint the submission occurrence, canonicalize every call, and compute the
   exact request digest in an opaque prepared value;
2. commit `Submission_intent` in the OAS Journal;
3. call `prepare_submission`;
4. commit one generic `Submission_accepted` Journal fact idempotently by
   `(submission_id, request_digest)`;
5. call `activate_submission` with that committed event identity;
6. return `Accepted`; if activation confirmation is unavailable, the same
   receipt remains accepted and reconciliation reports
   `Publication_committed_activation_pending`.

Prepared operations remain `Pending_publication` and cannot be claimed.
Publication repair and activation repair are idempotent. A second payload under
the same submission identity remains `Idempotency_conflict`.
Activation has no semantic rejection branch: invalid identity/digest/proof is
corruption, and store/response uncertainty is repairable. Any semantic or
capacity rejection must happen before backend preparation commits.
Cancellation may win before activation. In that ordering the cancellation
terminal remains authoritative, the later exact activation proof completes as
an idempotent no-op for the cancelled operation, and the operation never
returns to `Accepted`. Activation may win first as well; the ordinary
post-activation cancellation rules then apply. Both orders are durable reducer
outcomes, not timing guesses.

Only canonical call input and causal references are durable. Mutable
`Context.t` is never serialized as an opaque process value. Each operation
attempt receives a fresh context from the explicitly injected factory; restart
uses the same registered factory contract. Failure to reconstruct context is
the terminal `Recovery_failed`, not an empty/default context fallback.

Cancellation is a durable request and is idempotent. The cancelling intent is
committed before the worker switch is signalled. `Cancelling` is visible;
request acceptance is not misreported as terminal cancellation.

Submitting a sealed programmatic operation source containing zero operations
commits and returns an exact empty submission receipt. A provider-visible
asynchronous batch Tool retains the `minItems: 1` schema rule.

### 10.4 Durable Serial semantics

For `Serial`, every operation intent stores its predecessor operation identity.
Only the first operation is initially eligible. Operation `n + 1` becomes
eligible after operation `n` reaches any ordinary terminal outcome, including
a declared failure.

The dependency is durable and reconstructible after restart. It is not an
in-memory callback chain.

Infrastructure barriers never leave a successor indefinitely in generic
`Accepted`:

- submission or parent cancellation durably cancels every unfinished
  successor;
- `Outcome_unknown` moves the successor to
  `Waiting_on_predecessor
  { reason = Predecessor_outcome_unknown predecessor }` until the embedding
  owner reconciles or cancels that exact predecessor;
- `Recovery_failed` moves the successor to
  `Waiting_on_predecessor
  { reason = Predecessor_recovery_failed predecessor }`;
- a repaired ordinary terminal releases the successor exactly once;
- no elapsed-time rule guesses that a barrier is cleared.

The waiting operation consumes no worker or actor-lane claim. The embedding
application may continue unrelated work while the exact serial plan remains
observable and reconcilable.

For `Concurrent`, all operation intents become eligible after the submission
is prepared, its OAS publication is committed, and activation succeeds.

This is generic ordering within one submitted execution plan. It does not
define application scheduling priority, wake policy, fairness, calendar time,
or actor lane ownership.

### 10.5 Lifetime and recovery ownership

The asynchronous facade is owned by `Execution_runtime.t` under its
application-lifetime switch. Its injected backend owns only the durable
operation namespace. Worker supervision follows the exact
`Async_runtime_config.worker_ownership`: OAS owns one supervisor for each
`Oas_supervised` backend authority, while the embedding application owns it for
`Embedding_supervised`. Neither backend type, storage path, nor notification
behavior can change that choice. No such lifetime is owned by the finite Agent
run, Tool invocation, or provider request switch.

An accepted operation opens a separate execution scope linked by a typed
`caused_by` edge:

```text
finite submission Tool invocation
└─ durable receipt ──caused_by──> AsyncOperationScope
                                └─ ExecutableInvocation
                                   └─ ExecutableAttempt
```

The operation is not represented as a structural child kept open beneath an
already closed finite Tool invocation.

On restart:

- the new owner obtains the backend’s supervisor fence, then takes independent
  operation fences only for candidates it recovers;
- accepted operations are reconstructed from durable facts;
- an operation with no attempt may start;
- an operation whose attempt entered an external effect but has no durable
  result is `Outcome_unknown` unless its typed receipt protocol can reconcile
  the effect;
- unknown external effects are never automatically executed again.

The generic executor does not claim exactly-once external side effects.
Every claim, effect-entry transition, cancellation transition, and terminal
commit carries the current full operation fence. The backend applies an exact
backend/operation/owner/epoch compare-and-set and rejects a stale owner.

Immediately before crossing an external effect boundary, the worker
revalidates its fence. If the external system supports fencing or idempotency,
the typed `Effect_protocol` carries that token across the boundary. If it does
not, ownership loss after effect entry becomes `Outcome_unknown`; no second
worker is started merely because elapsed time passed.

Recovery never guesses that another owner is dead from elapsed time alone.

### 10.6 Event cursor and gap law

An operation cursor is scoped to one durable event stream and is monotonic.
`read_events` returns either:

- an ordered page with stream identity, `next_cursor`, earliest retained
  cursor, and `caught_up`;
- a typed `Cursor_gap` containing stream identity, requested cursor, and
  earliest retained cursor;
- a typed store/protocol failure.

It never jumps to the newest event or silently performs a full-history replay.
`Read_error.t` contains `Cursor_gap of Operation.cursor_gap`; a retention gap is
not collapsed into a generic lookup error.

OAS exposes terminal events and cursors. The embedding application decides
whether an event wakes an actor, enqueues work, emits a notification, or is
consumed by polling.

### 10.7 Asynchronous collection Tool adapter

`AsyncExecutable[] as Tool` is a real adapter, not an instruction for an
application to hand-write submission JSON:

```ocaml
module Async_tool_batch : sig
  val expose
    :  runtime:Async_runtime_config.reference
    -> id:Tool.Id.t
    -> revision:Tool.Revision.t
    -> executable_id:Executable.Id.t
    -> executable_revision:Executable.Revision.t
    -> name:string
    -> description:string
    -> sibling_schedule:Provider_sibling_schedule.t
    -> mode:Executable_plan.mode
    -> members:Tool_member_catalog.t
    -> (Tool.t, Construction_error.t) result
end
```

`Async_tool_batch` is the collection constructor and consumes the same sealed
member catalog as `Tool_batch`; no whole-list or legacy `Tool.batch_async`
wrapper reintroduces another registry, codec, or submission identity.

The generated outer binding uses a typed submission effect protocol and a
stable `Submission.Id.t` stored beneath the outer invocation before backend
preparation. Retrying or recovering that same invocation resolves the existing
submission edge; it never calls `fresh` again.

The provider schema has `minItems: 1` and the same closed discriminated union as
the synchronous collection. The ToolResult is exactly one of the structured
submission outcomes in §9. The later child operation events are never appended
to the closed ToolUse.

The adapter uses a Dune-private prepared-submission path rather than the
programmatic `Async_executor.submit` publication path:

1. commit `Submission_intent`, prepare the backend submission, and return its
   typed caller receipt plus `Async_submission_commit_action`, a validated
   `prepared_rejection`, or exact reconciliation identity;
2. the generated handler returns
   `Executable.Internal.domain_succeeded_after_commit` with
   the receipt as its native success value, or
   `Executable.Internal.terminal_prepared
   (Submission.Internal.terminal_preparation ~opened rejection)` for the
   rejection;
3. Tool dispatch commits the native terminal, authoritative invocation result,
   and optional `Commit_action_pending` atomically; for provider origin the
   result is the outer ToolResult. The rejection branch uses
   `Invocation_terminal_commit.commit_prepared`, so its submission lifecycle
   fact and ToolResult share this transaction;
4. `Commit_action_executor` activates the backend with that actual publication
   event identity;
5. it commits completion or an explicit activation-reconciliation state, then
   Tool dispatch runs post observers.

The committed intent is itself an indexed open-repair item until step 3 commits.
Scope readiness first drains a fixed `Submission_intent_repair` scan and then
continuously tails it with the same ordinal-aware, lost-wakeup-safe law as
commit-action repair. Crash after intent, during backend preparation, or after
backend prepare but before the outer result therefore reopens the same owning
invocation and exact runtime revision, calls backend reconciliation with the
stored submission identity/digest, and either commits the one authoritative
result plus pending action or records typed durable uncertainty. If the backend
proves absence it may prepare only the same stored durable request. It never
mints another submission, performs a heuristic backend orphan scan, or calls
fresh preparation while the prior outcome is ambiguous.

Crash before the combined rejection transaction leaves only the open intent
and therefore resumes reconciliation. Crash or reply loss after it returns
`Rejected_prior` and the already committed ToolResult. No repair path calls
the programmatic rejection writer for a provider-origin invocation, and no
post hook observes a rejection until the combined terminal is durable.

Thus the async executor does not commit a ToolResult behind Tool dispatch, and
the outer invocation still has one result writer and one post-hook lifecycle.
At handler entry the generated adapter obtains
the invocation-bound `Execution_context.Internal.async_submissions`, resolves
its exact `Async_runtime_reference`, and passes one canonical request through
`Async_executor_internal.prepare_for_tool`. There is no process global,
mutable late-binding cell, or `Context.t` string lookup.
The generated async receipt projector is total by construction. A handwritten
success projector cannot manufacture this private commit action, so a generic
projection failure can never strand an internally prepared async submission.

The ordinary public façade is `Async_tool_batch.expose` with one explicit
stable Tool/executable definition and `~runtime ~mode ~members`.
Supplying or implementing `Operation_backend.S` is an advanced embedding path,
not a prerequisite for users of `Async_runtime_config.create_journal`.

`Execution_runtime.build_agent` collects the definition's Tools, calls
`Tool_catalog.build`, and atomically registers its executable fragments and
explicit `Async_runtime_config` values in the application runtime. An async Tool
stores only the exact runtime reference. Construction fails if the referenced
runtime configuration, context-factory revision, or execution-runtime authority
is missing, duplicated, or conflicting. The runtime updates one monotonic
immutable registry snapshot; no executor captures a private per-Agent registry
and adding a composite Tool never creates another pool or recovery supervisor.
The example below receives the one application-lifetime runtime created during
process bootstrap; it never creates a runtime, pool, or supervisor per Agent.

```ocaml
let make_async_agent
    ~sw
    ~runtime
    ~operation_journal
    ~execution_journal
    ~net
    ~binding
    ~members
    ~create_context
  =
  let context_factory =
    Operation_context_factory.of_function
      ~id:Context_factory_ids.background
      ~revision:Context_factory_revisions.background_v1
      ~create_context
  in
  let* async_runtime =
    Async_runtime_config.create_journal
      ~id:Runtime_ids.background
      ~revision:Runtime_revisions.background_v1
      ~backend_id:Backend_ids.background
      ~storage:operation_journal
      ~context_factory
  in
  let* batch =
    Async_tool_batch.expose
      ~runtime:(Async_runtime_config.reference async_runtime)
      ~id:Tool_ids.background_batch
      ~revision:Tool_revisions.background_batch_v1
      ~executable_id:Executable_ids.background_batch
      ~executable_revision:Executable_revisions.background_batch_v1
      ~name:"background_batch"
      ~description:"Accept a durable asynchronous executable batch"
      ~sibling_schedule:Provider_sibling_schedule.Must_serialize
      ~mode:Execution_mode.Concurrent
      ~members
  in
  let builder =
    Builder.create ~net ~binding
    |> Builder.with_tool batch
  in
  Execution_scope.build_direct
    ~sw
    ~runtime
    ~journal:execution_journal
    ~async_runtimes:[ async_runtime ]
    builder
```

## 11. Side-effect uncertainty

Every executable binding declares a typed recovery class:

The acyclic prerequisite interfaces are defined in §2.3. `Replay_safe` permits
a fresh attempt, `External_effect` requires typed receipt reconciliation, and
`Journal_resumable_composite` recovers the existing child plan rather than
replaying it.

This is an explicit executable contract, not an inference from tool name,
input hash, HTTP method, path, or output text.

`Effect_boundary.perform` is the only supported entry for a declared external
effect. It commits effect entry before invoking `execute`, supplies the current
fencing/idempotency values, and commits the typed receipt before returning.
Application code never receives the raw journal writer.

The fence is not a correlation-only token. An OAS Journal attempt commits a
monotonic `Execution_fence` epoch under
`Journal_invocation invocation_id` before handler entry. An async operation
uses the stable source `Operation { backend; operation }`; writer owner changes
do not change that source, and the execution epoch is derived from the active
`Operation_fence.epoch`. Recovery installs a strictly newer per-operation
epoch before it may
cross an external boundary. External integrations can compare epochs only
after exact source equality; comparing unrelated invocation or operation
sources is a typed error.

For an external effect:

1. the attempt commits before handler entry;
2. effect entry commits before crossing the external boundary;
3. a durable typed receipt commits after a known effect result;
4. the exact execution result commits before provider projection;
5. crash/cancellation after effect entry but before a reconcilable receipt is
   `Outcome_unknown`;
6. for an `External_effect` binding, a reconciled or committed effect receipt
   without an executable terminal is also `Outcome_unknown`; only a separately
   declared `Journal_resumable_composite` can resume its own exact checkpoints;
7. `Outcome_unknown` is never automatically retried.

For `Replay_safe`, recovery may start a fresh attempt after an incomplete
attempt. The new attempt receives a fresh `Attempt.Id.t`; the invocation and
operation identities remain unchanged.

## 12. Conversation and execution truth

The execution record preserves:

- selected provider binding and every provider attempt;
- ordered Thinking, Reasoning, Text, multimodal, ToolUse, and ToolResult facts;
- every executable invocation and attempt;
- exact canonical inputs, successes, and declared failures;
- hook decisions and observer failures;
- synchronous parent-child edges;
- asynchronous submission, operation, and causal edges;
- cancellation, recovery, effect uncertainty, and terminal provenance.

Provider replay uses typed provider-specific history projection. The execution
record is not flattened into prose and fed back wholesale.

Repeated model output remains repeated evidence. OAS does not use repeated
text, repeated input, cost, turns, elapsed time, or recursion depth as a
heuristic loop breaker. Malformed causal history is a typed protocol failure.
Higher-level intervention is an embedding policy.

### 12.1 Lossless hierarchical read projection

The generic read model preserves execution structure instead of emitting one
flat `event array` that a dashboard must pair by string or position:

```ocaml
module Agent_run_lifecycle_fact : sig
  type origin =
    | Root_agent
    | Child_agent of Invocation_attempt_reference.t

  type terminal =
    | Succeeded of Agent_response.t
    | Failed of Agent.detailed_error
    | Cancelled of Cancellation.cause
    | Infrastructure_failed of Execution_value.t

  type view =
    | Opened of
        { agent_run : Agent_run.Id.t
        ; origin : origin
        ; binding : Executable.reference option
        ; definition : Agent_definition_digest.t
        ; prelude :
            ((Execution_manifest_purpose.agent_prelude,
              Execution_manifest.committed)
               Execution_manifest.t)
              option
        ; input_fact : Execution_fact_ref.t
        }
    | Checkpoint_committed of
        { agent_run : Agent_run.Id.t
        ; previous : Execution_fact_ref.t option
        ; checkpoint : Agent_checkpoint_frontier.t
        }
    | Terminal_committed of
        { agent_run : Agent_run.Id.t
        ; outcome : terminal
        }

  type t

  val fact : t -> Execution_fact_ref.t
  val view : t -> view
end

module Agent_turn_lifecycle_fact : sig
  type terminal =
    | Completed
    | Failed of Agent.detailed_error
    | Cancelled of Cancellation.cause

  type view =
    | Opened of
        { agent_run : Agent_run.Id.t
        ; turn : Agent_turn.Id.t
        }
    | Terminal_committed of
        { agent_run : Agent_run.Id.t
        ; turn : Agent_turn.Id.t
        ; outcome : terminal
        }

  type t

  val fact : t -> Execution_fact_ref.t
  val view : t -> view
end

module Provider_exchange_lifecycle_fact : sig
  type origin =
    | Initial_request
    | Continuation_after_tools of
        { previous : Provider_exchange.Id.t
        ; selected : Provider_attempt_selected.t
        ; tool_results :
            (Execution_manifest_purpose.executable_results,
             Execution_manifest.committed)
              Execution_manifest.t
        }

  type terminal =
    | Selected_for_tools of Provider_attempt_selected.t
    | Selected_final_output of Provider_attempt_selected.t
    | Failed of Provider_attempt_failure.t
    | Cancelled of Cancellation.cause

  type view =
    | Opened of
        { agent_run : Agent_run.Id.t
        ; turn : Agent_turn.Id.t
        ; exchange : Provider_exchange.Id.t
        ; origin : origin
        ; conversation : Conversation_snapshot.t
        ; input_truncation : Provider_input_truncation_selection.t
        }
    | Terminal_committed of
        { exchange : Provider_exchange.Id.t
        ; outcome : terminal
        }

  type t

  val fact : t -> Execution_fact_ref.t
  val view : t -> view
end

module Conversation_input_fact : sig
  type t

  val fact : t -> Execution_fact_ref.t
  val input : t -> Conversation_snapshot.input
end

module Executable_plan_fact : sig
  type view =
    | Opened of
        { owner : Invocation_attempt_reference.t
        ; mode : Executable_plan.mode
        ; calls : Executable_call_source.t
        }
    | Member_started of
        { owner : Invocation_attempt_reference.t
        ; ordinal : int64
        ; invocation : Invocation_open_reference.t
        }
    | Member_settled of
        { owner : Invocation_attempt_reference.t
        ; ordinal : int64
        ; terminal_fact : Execution_fact_ref.t
        }

  type t

  val fact : t -> Execution_fact_ref.t
  val view : t -> view
end

module Submission_lifecycle_fact : sig
  type view =
    | Intent of Submission.durable_request
    | Rejected_before_backend_commit of Submission.definitive_rejection
    | Publication_committed of Submission.published_receipt
    | Reconciliation of Submission.reconciliation

  type t

  val fact : t -> Execution_fact_ref.t
  val view : t -> view
end

module Provider_delta_payload : sig
  type field =
    | Thinking
    | Reasoning
    | Reasoning_summary
    | Text
    | Refusal
    | Multimodal
    | Tool_arguments

  type t =
    | Content_fragment of
        { field : field
        ; content : Provider_content.committed Provider_content.t
        }
    | Metadata_only of
        Provider_native_evidence.committed Provider_native_evidence.t
end

module Execution_read_model : sig
  type node_id =
    | Agent_run of Agent_run.Id.t
    | Turn of Agent_turn.Id.t
    | Provider_exchange of Provider_exchange.Id.t
    | Provider_attempt of Provider_attempt.Id.t
    | Invocation of Invocation.Id.t
    | Attempt of Attempt.Id.t
    | Submission of Submission.Id.t
    | Operation of Operation.Id.t

  type node_ref =
    { stream : Execution_stream_reference.t
    ; id : node_id
    }

  type edge_role =
    | Turn_of_agent_run
    | Provider_exchange_of_turn
    | Provider_attempt_of_exchange
    | Continuation_of_exchange of
        { previous : Provider_exchange.Id.t
        ; selected : Provider_attempt_selected.t
        }
    | Tool_result_causes_exchange of
        { ordinal : int64
        ; invocation : Invocation.Id.t
        }
    | Attempt_of_invocation
    | Nested_executable
    | Child_agent_run
    | Composite_member of
        { ordinal : int
        ; mode : Executable_plan.mode
        ; binding : Executable.reference
        ; exposure : Executable.exposure_reference option
        }
    | Async_submission
    | Async_operation
    | Provider_origin of Provider_tool_call_reference.t

  type edge =
    | Structural_child of edge_role
    | Caused_by of edge_role
    | Originated_from of edge_role

  type edge_record =
    { source : node_ref
    ; kind : edge
    ; target : node_ref
    ; established_by : Execution_fact_ref.t
    ; edge_ordinal : int
    }

  type edge_direction =
    | Incoming
    | Outgoing
    | Both

  type edge_page_cursor

  type edge_page =
    { edges : edge_record list
    ; next : edge_page_cursor
    ; caught_up : bool
    }

  module Edge_index : sig
    type t
    type generation
    type high_water
    type source_checkpoint

    val generation : t -> generation
    val source_checkpoint
      :  t
      -> Execution_stream_reference.t
      -> (source_checkpoint, Lookup_error.t) result

    module Internal : sig
      val open_
        :  sw:Eio.Switch.t
        -> bootstrap:Execution_edge_index_bootstrap.t
        -> (t, Edge_index_open_error.t) result

      val close_and_await
        :  t
        -> (unit, Edge_index_close_error.t) result
    end
  end

  type node =
    { reference : node_ref
    ; opened_by : Execution_fact_ref.t
    }

  type fact_id = Execution_fact_ref.t

  type provider_item_id =
    { attempt : Provider_attempt.Id.t
    ; ordinal : int
    }

  type infrastructure_origin =
    | Provider_transport
    | Journal
    | Projection
    | Recovery
    | Runtime

  type provider_attempt_view =
    | Provider_attempt_opened of
        { turn : Agent_turn.Id.t
        ; exchange : Provider_exchange.Id.t
        ; attempt : Provider_attempt.Id.t
        ; binding : Provider_binding_reference.t
        ; source_adapter : Provider_adapter_reference.t
        ; wire_contract : Provider_wire_contract_reference.t
        ; conversation : Conversation_snapshot.t
        ; input_truncation : Provider_input_truncation_selection.t
        ; tool_offer : Provider_tool_offer_plan.t
        }
    | Provider_attempt_succeeded of Provider_attempt_success.t
    | Provider_attempt_failed of Provider_attempt_failure.t
    | Provider_attempt_selected of Provider_attempt_selected.t

  type hook_decision_view =
    | Pre_tool_continue of
        { exposure : Executable.exposure_reference
        ; schedule : Hooks.tool_schedule
        }
    | Pre_tool_blocked of
        { exposure : Executable.exposure_reference
        ; reason : Execution_value.t
        }

  type hook_observer_phase =
    | Post_tool_use
    | Post_tool_use_failure

  type hook_observer_outcome =
    | Observed
    | Observer_failed of Execution_value.t

  type hook_observation_view =
    { exposure : Executable.exposure_reference
    ; phase : hook_observer_phase
    ; observer_ordinal : int
    ; outcome : hook_observer_outcome
    }

  type cancellation_view =
    | Invocation_cancelled of
        { invocation : Invocation.Id.t
        ; cause : Cancellation.cause
        }
    | Operation_cancellation of Operation.cancellation_result

  type effect_lifecycle_view =
    | Effect_entered of
        { entry : Effect_entry.t
        ; entry_fact : Execution_fact_ref.t
        }
    | Effect_receipt_committed of
        { receipt : Effect_receipt.t
        ; receipt_fact : Execution_fact_ref.t
        }
    | Effect_outcome_unknown of Effect_unknown.t

  type recovery_view = Operation_recovery_decision.t

  type fact_view =
    | Provider_attempt_fact of provider_attempt_view
    | Provider_exchange_lifecycle of Provider_exchange_lifecycle_fact.t
    | Provider_delta of
        { item : provider_item_id
        ; delta_sequence : int
        ; payload : Provider_delta_payload.t
        }
    | Provider_finalized of Provider_finalized_item.t
    | Provider_tool_result_fact of Provider_tool_result.t
    | Conversation_input of Conversation_input_fact.t
    | Invocation_opened of Invocation_opened_fact.t
    | Executable_attempt_opened of Invocation_attempt_reference.t
    | Executable_input of Execution_value.t
    | Invocation_terminal of Invocation_result.Committed.t
    | Agent_run_lifecycle of Agent_run_lifecycle_fact.t
    | Agent_turn_lifecycle of Agent_turn_lifecycle_fact.t
    | Executable_plan_lifecycle of Executable_plan_fact.t
    | Submission_lifecycle of Submission_lifecycle_fact.t
    | Commit_action_lifecycle of Commit_action_fact.t
    | Hook_decision of hook_decision_view
    | Hook_observation of hook_observation_view
    | Cancellation of cancellation_view
    | Effect_lifecycle of effect_lifecycle_view
    | Recovery_fact of recovery_view
    | Operation_backend_event of Operation.event
    | Infrastructure_failed of
        { origin : infrastructure_origin
        ; detail : Execution_value.t
        }

  type fact

  val fact_id : fact -> fact_id
  val fact_node : fact -> node_ref
  val fact_cursor : fact -> Execution_cursor.t
  val fact_observed_at : fact -> Observation_time.t
  val view_fact : fact -> fact_view

  type page =
    { stream : Execution_stream_reference.t
    ; nodes : node list
    ; facts : fact list
    ; next : Execution_page_cursor.t
    ; caught_up : bool
    }

  module Reader : sig
    type t

    module Internal : sig
      type source
      type root_router
      type operation_router

      val journal_source
        :  reference:Execution_stream_reference.t
        -> access:Execution_journal_access.t
        -> (source, Construction_error.t) result

      val operation_backend_source
        :  reference:Execution_stream_reference.t
        -> backend:Operation_backend.t
        -> (source, Construction_error.t) result

      val root_router
        :  reference:Execution_stream_reference.t
        -> bootstrap:Execution_journal_bootstrap.t
        -> cpu_executor:Cpu_executor.t
        -> (root_router, Construction_error.t) result

      val operation_router
        :  backend:Operation_backend.t
        -> scope_factories:Operation_scope_factory_registry.t
        -> cpu_executor:Cpu_executor.t
        -> (operation_router, Construction_error.t) result

      val create
        :  sources:source list
        -> root_routers:root_router list
        -> operation_routers:operation_router list
        -> edge_index:Edge_index.t
        -> admission:Read_admission.t
        -> (t, Construction_error.t) result
    end
  end

  module Read_session : sig
    type t

    val edge_beginning
      :  t
      -> node:node_ref
      -> direction:edge_direction
      -> edge_page_cursor

    val read_node
      :  t
      -> node_ref
      -> (node, Read_error.t) result

    val read_fact
      :  t
      -> Execution_fact_ref.t
      -> (fact, Read_error.t) result

    val read_provider_content
      :  t
      -> Provider_content.committed Provider_content.t
      -> after:Provider_content_cursor.t
      -> max_bytes:Positive_byte_count.t
      -> (Provider_content.page, Read_error.t) result

    val read_provider_native_scalar
      :  t
      -> Provider_native_scalar.committed Provider_native_scalar.t
      -> after:Provider_native_scalar.cursor
      -> max_bytes:Positive_byte_count.t
      -> (Provider_native_scalar_source.page, Read_error.t) result

    val read_execution_value_items
      :  t
      -> Execution_value.t
      -> after:Execution_value.cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (Execution_value.page, Read_error.t) result

    val read_execution_value_content
      :  t
      -> Execution_value.Content.committed Execution_value.Content.t
      -> after:Execution_value.Content_cursor.t
      -> max_bytes:Positive_byte_count.t
      -> (Execution_value.Content_page.t, Read_error.t) result

    val read_execution_value_annotations
      :  t
      -> Execution_value.Annotation_source.committed
           Execution_value.Annotation_source.t
      -> after:Execution_value.Annotation_source.cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (Execution_value.Annotation_source.page, Read_error.t) result

    val read_edges
      :  t
      -> node:node_ref
      -> direction:edge_direction
      -> after:edge_page_cursor
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (edge_page, Read_error.t) result

    val read
      :  t
      -> stream:Execution_stream_reference.t
      -> after:Execution_page_cursor.t
      -> requested:Positive_int.t
      -> max_encoded_bytes:Positive_byte_count.t
      -> (page, Read_error.t) result
  end

  val with_read_session
    :  Reader.t
    -> sw:Eio.Switch.t
    -> (Read_session.t -> ('a, Read_error.t) result)
    -> ('a, Read_error.t) result
end

module Execution_inspector : sig
  type t

  val of_runtime
    :  Execution_runtime.t
    -> (t, Construction_error.t) result

  val with_read_session
    :  t
    -> sw:Eio.Switch.t
    -> (Execution_read_model.Read_session.t
        -> ('a, Read_error.t) result)
    -> ('a, Read_error.t) result
end
```

The sequential page is normalized rather than recursively materialized: nodes
and facts are emitted once by identity. It deliberately contains no inline
edge list. One continuation-open fact may establish an arbitrarily large
Tool-result edge set, so a fact cursor cannot byte-bound that expansion or
resume within it. `read_edges` and its projection-subordinal
`edge_page_cursor` are the sole graph enumerator and enforce both item and byte
bounds without duplicating edge authority in stream pages. An edge may refer
to a node outside the current page or in another stream; `node_ref` carries
both the semantic node identity and exact stream authority, so `read_node`
resolves it without forcing every page to repeat the whole ancestor chain.
Bare `node_id` values are never used for cross-stream routing. Every edge carries the exact fact that
established it plus a projection ordinal unique within that fact. The read
runtime owns one durable `Edge_index` projection by full source and target
`node_ref`; `read_edges` pages incoming/outgoing/both neighborhoods without
scanning a long-lived stream or reconstructing ancestry from embedded IDs.
The Journal/backend fact remains semantic SSOT. The index stores an idempotent
projection row keyed by `(established_by, edge_ordinal)` plus one durable
ordinal-aware source checkpoint. A source registration performs bounded paged
catch-up from that checkpoint to a fixed source high-water, persists each
checkpoint with its projection rows, arms the source tail without a lost
wakeup, and publishes the route only after catch-up. Crash resumes from the
checkpoint; a gap/corruption leaves the route typed unavailable rather than
publishing an empty neighborhood.

Each `Read_session` captures one immutable edge-index generation and
append-only projection high-water. `edge_beginning` mints a cursor bound to
that session, queried node, direction, generation, high-water, and projection
sequence. New/late history registrations append only after that high-water and
become visible in a new session; they can never insert a past canonical key
behind an existing cursor. Reusing a cursor under another generation/session
is a typed stale-cursor error. Cross-stream index sequence is only a pagination
key, never a timeline or causal total order. A retention/index gap is a typed
read error, not an empty neighborhood. Each edge source is an explicit node
reference; nested
structural calls start at the owning Attempt node. The closed roles retain the
complete AgentRun→Turn→ProviderExchange→ProviderAttempt and
Invocation→Attempt spine as well as nested-call, child-Agent, or composite
mode/ordinal/exposure metadata. Each open fact establishes its exact spine
edge; a UI never reconstructs hierarchy from embedded IDs. Structural and
causal edges can coexist for one node. An accepted async operation therefore
has a causal edge without being visually or semantically kept open as a
synchronous child.

`Reader.t` is a runtime-owned exact router, not a filesystem or backend
scanner. Root execution and operation-backend sources are registered with
their exact `Execution_stream_reference.t` and checked Journal/backend
authority; callers cannot inject arbitrary read closures. A live
`journal_source` is usable only while its owning finite scope remains open.
For durable history the runtime also registers a `root_router` from that
scope's exact immutable bootstrap and stream reference before publishing the
scope. After scope stop, a read session reopens that exact root with
`open_reader`; a volatile root instead returns a typed history-unavailable
error and is never presented as an empty stream. Runtime restart must restore
the explicit durable root bootstrap registrations supplied by the embedding
host through `Execution_runtime.register_root_history`; OAS does not discover
directories. That call performs one bounded read-only open, verifies the
bootstrap's actual stream is byte-equal to the supplied root reference, then
pages the durable edge projection through the source's fixed high-water and
arms its tail before it closes the reader and atomically installs the route.
Index catch-up failure installs neither route nor partial-success claim; the
durable checkpoint remains resumable. Its registration handle supports explicit
unregistration after active read sessions drain; a mismatched, duplicate, or
in-use route is a typed error. Duplicate live sources or routers for
one stream are rejected unless they carry the same frozen authority.
An operation-execution reference
is resolved only through an `operation_router` built from one registered
backend authority and the exact scope-factory registry. It reads that
operation's durable state, obtains its execution anchor, and opens the anchor's
exact `Operation_scope_factory` revision for
`(operation, execution_scope, event_stream)`. The router receives the
application runtime's one `Cpu_executor.t`; under the read session's switch
and one `Read_admission.with_stream` slot it converts the returned pure
bootstrap through `Execution_journal_bootstrap.Internal.open_reader`, retains
only its reader for one grouped read, and calls `close_reader` before releasing
that slot. It never starts a lane writer or recovery supervisor for a read.
The returned source must carry the byte-equal requested reference or lookup
fails. No reader derives a path
from an ID, probes directories, searches every backend, or falls back to a
stream with a matching sequence. Every lazily opened Journal is bracketed by
one `Read_session` switch, but holds a `Read_admission` slot only for one point
or bounded page read. It closes the reader and releases the slot before
another page can wait on that capacity. The session may retain immutable
routing metadata but no open reader in an unbounded global cache. Every
read/close failure remains typed.

There is deliberately no `read_facts : fact_ref list -> fact list` aggregate:
that API materializes both an unbounded request and an unbounded result after
chunking. Exact point reads use `read_fact`; sequential stream work uses the
paged `read`; graph work pages edges and then performs bounded point reads.
`read_provider_content` and `read_provider_native_scalar` use the session's
exact routed Journal reader and return at most `max_bytes`. Native correlation
IDs therefore remain byte-lossless and paged even though their typed call
reference exposes only committed scalar handles; the dashboard never turns a
committed native payload or scalar back into whole `Canonical_json.t`.
The three `read_execution_value_*` operations similarly route the exact
value's manifest stream under the session switch and shared read admission:
item metadata, referenced content bytes, and annotations remain separately
item/byte paged. They are the public inspection path for input, terminal,
hook, effect, checkpoint, and infrastructure payloads; inspector code never
obtains or retains an `Execution_journal.Reader.t` or an escaping decoder.
`read` rejects an
`after` cursor whose stream does
not equal the requested reference, and `read_fact` routes by
`Execution_fact_ref.stream_reference`; an operation fact therefore already
contains the anchor-routing identities and never requires a stream scan.

`Execution_inspector.of_runtime` is the ordinary read façade. It borrows the
runtime's one route registry, CPU executor, read admission, exact backend
authorities, and immutable scope-factory snapshots; callers do not assemble
`Reader.Internal.source` values. The inspector observes routes registered
after its creation through that shared typed registry and becomes typed closed
when the runtime stops. `with_read_session` is the only public lifetime entry,
so an opened reader cannot escape its switch.

`view_fact` is the closed dashboard eliminator. It retains typed
provider-attempt open/success/failure/selection facts,
Thinking/Reasoning/Text/multimodal deltas, finalized provider items with source
adapter/turn/replay provenance and a paged exact provider-native content
reference,
the exact binding, wire-contract reference, offered-Tool exposure snapshot,
terminal-native metadata, and committed conversation inputs for every provider
attempt, authoritative
`Provider_tool_result.t` including its source key/full call reference/outcome,
invocation-open facts including binding revision, exposure, origin-specific
provenance, provider schedule, and wire arguments, executable input, the exact
committed invocation terminal with all six closed outcomes
(`Succeeded`, `Declared_failure`, `Blocked`, `Invalid_input`, `Cancelled`, and
`Infrastructure_failed`), hook decisions and
every successful or failed post observer with full exposure revision,
AgentRun open/checkpoint/terminal by exact canonical input-fact reference,
AgentTurn open/terminal,
ExecutableAttempt-open, composite plan/member settlement, submission and
commit-action lifecycle, cancellation, effect entry/receipt/unknown facts,
recovery resolutions, and infrastructure failures. The full closed
`Operation.event` is the sole operation-backend fact authority;
`Operation.status` is derived while viewing that event/state and is not emitted
as a second progress fact.
`Provider_finalized` returns the opaque committed
`Provider_finalized_item.t` witness itself, not a record copied from it.
`Provider_tool_result_fact` is a provider-origin specialization derived from
that same `Invocation_result.Committed.t`; it never replaces the generic
terminal or commits a second outcome authority.
Each `Provider_delta` likewise contains only its lane/item sequence and a
small closed `Provider_delta_payload`: a committed provider-content fragment
reference or committed metadata evidence. It does not allocate and commit a
full `Execution_value` manifest for every token-sized delta. Final
provider-neutral `Execution_value.t` is built once when the logical item
finalizes. Delta facts are never coalesced, dropped, or reordered for
performance; paging merely keeps their metadata and content bytes bounded.
This preserves exact interleaving while avoiding O(delta-count) manifest
trees and repeated prefix copies on the dashboard/server hot path.
Ordinal, source key, adapter, turn, kind, replay eligibility, native content,
Tool arguments, and superseded-delta frontier are derived through that
witness's accessors. Consequently a restarted application can append the same
opaque provider item to a bounded conversation-selection builder without
reconstructing it from JSON or depending on an in-memory commit return value.
The read model is the only Journal decoder allowed to mint that committed
witness.
Every fact also exposes the `Observation_time.t` stamped by its sole committing
writer. The Journal lane writer obtains that value from the runtime's one
`Observation_source`; an external operation backend can obtain it only through
its narrowed `Operation_backend_protocol.observation_now`. A node's
`opened_by` is the exact opening fact rather than a second timestamp authority.
Turn, provider-attempt, executable, Tool, and child-Agent spans are derived from
their typed open/terminal fact pairs. The metric available from output tokens
and same-clock attempt-open/success facts is explicitly
`oas_observed_end_to_end_output_rate`; it includes request, queue, and transport
time and is not presented as provider generation speed. The separate
`provider_reported_generation_output_rate` is available only when the exact
adapter reports both output tokens and a generation duration. Either metric is
typed unavailable when its own evidence is absent; the renderer never falls
back from one to the other or mixes their denominators. Renderer summaries and
rates are never persisted as new facts. `rate` retains its basis, exact token
count, and nanosecond denominator; `tokens_per_second` is only a finite display
projection and cannot become a scheduling or admission input.
`Executable_input` is the single canonical decoded/durable input fact.
`Invocation_opened` does not duplicate it for programmatic or asynchronous
origins; only a provider origin additionally preserves the pre-decode wire
arguments required for audit and validation evidence.
An operation terminal projection does not replace or summarize its effect and
recovery facts; recovery carries the previous claim/attempt and current full
operation fence needed to audit takeover. Provider-native finalized payload is the
authority, and a registered exact adapter revision may derive display content
without replacing it. Composite plan facts and member edges retain the exact
mode, member ordinal, binding, optional Tool exposure, child invocation,
input-fact reference, and establishing fact; a UI
never reconstructs an array block from adjacency or display labels. A
finalized provider fact names the exact item and the last
provisional delta it
supersedes. A normal renderer shows that finalized item once; a diagnostic
renderer may also show the preserved deltas. Distinct finalized item IDs remain
distinct even when their text is equal, so presentation deduplication never
becomes semantic repetition detection.

A renderer may collapse or summarize a subtree, but that is local UI state and
never overwrites facts or edges.

Ordering comes from committed stream cursors and explicit parent/causal edges,
not wall-clock sorting. Ordering between streams is a partial order expressed
by edges and referenced commit events; a dashboard must not invent a total
wall-clock order. Observation times on one clock support duration measurement,
not topology; different clocks are incomparable. `requested` bounds committed
facts, and `page.next` is the exact ordinal-aware position after the last
returned fact. A page may end inside an event with more facts than the request
size and the next read must return the remaining ordinals. Normalized node/edge
overhead is bounded by the identities referenced by those facts. Retention loss
has one channel, `Read_error.Cursor_gap`; the projection does not also return a
page, silently jump forward, synthesize missing ToolResults, or replace missing
data with a prose error row.

This is the OAS-neutral structure consumed by an embedding dashboard. Product
concepts remain typed application executables/adapters outside OAS.

## 13. Boundary and non-goals

OAS does not define:

- long-lived actor or character lifecycle;
- scheduler, wake-up, notification, or workspace policy;
- quorum, judge, or domain-specific orchestration kinds;
- memory storage, recall, consolidation, or forgetting;
- conversation compaction;
- cost, token, turn, or recursion admission policy;
- product-specific dashboard rows.

An embedding application may adapt any of its typed operations to
an `Executable.binding`, register it exactly once, and expose the resulting
typed witness through `Tool.t`.

## 14. Hard-cut migration

The merged 0.214.0 execution foundation is evolved, not duplicated:

- `Execution_event_store` remains the physical durable byte/cursor SSOT. It
  validates physical framing, identity, sequence, and committed-prefix
  authority only; it does not implement or mint the target semantic Journal
  Writer and this RFC does not introduce another store.
- The current `Execution_journal` reducer and typed transaction layer remains
  the semantic topology authority. Its finite run-specific materialized view
  is generalized into the normalized recursive read model, rather than
  replacing the reducer with a direct store projection.
- The target `Execution_journal.Reader` is a capability over an immutable
  semantic Journal projection. The target `Execution_journal.Writer` is the
  narrow submit capability of the sole `Execution_lane_writer` actor. Both are
  minted together for one semantic Journal scope and wrapped in one
  `Execution_journal_access`; neither exposes
  `Execution_event_store.writer` or permits direct physical append.
- The current pool-only private `Execution_runtime` is renamed/absorbed as
  `Cpu_executor`. Its one-pool lifetime, reentrant-inline deadlock prevention,
  cancellation checks, and statistics are retained, then extended with the
  explicit total-admission contract above.
- `Execution_codec_executor`, `Execution_event_store`, and
  `Execution_lane_writer` are repointed to that one `Cpu_executor`; they do not
  keep a hidden old pool beside the application-lifetime target
  `Execution_runtime`.
- `Execution_journal_bootstrap` is the only construction bridge from the one
  `Cpu_executor` to store, semantic Journal, lane writer, and checked access.
  A finite `Execution_scope` consumes the pure bootstrap descriptor and owns
  the resulting scope DAG while borrowing the application runtime's one CPU
  executor; callers never have to prebuild a live pool or Journal access and
  cannot create a circular runtime dependency.
- The application-lifetime `Execution_runtime` and finite
  `Execution_scope` are distinct authorities. The former owns shared pool and
  exact registries; the latter owns one root-run Journal, lane writer, and
  repair cursor. Neither is retained as a compatibility alias for the old
  combined shape.
- The merged lane-writer and event identity/cause invariants remain the
  foundation of the one-writer Journal topology. New recursive executable facts
  extend those closed protocols rather than opening a parallel event bus.

The migration sequence is:

1. Land canonical `Json_schema.t` and schema-bound codecs.
2. Land canonical `Execution_value.t`; remove competing ToolResult authorities.
3. Land the acyclic identity/reference/context spine and exact
   `Executable_registry`.
4. Make `Tool.t` an abstract existential package over registered executables.
5. Add the one-call typed `Tool.create` façade and keep private algebra,
   registry, and eliminators out of `Agent_sdk`.
6. Delete `Typed_tool.to_untyped` and the public untyped Tool handler record.
7. Pass abstract `Invocation.context`, including the active Eio switch, through
   Tool dispatch.
8. Reshape the private Execution Journal to execution-native recursive
   invocation/attempt topology.
9. Route all Tool execution through registered executable witnesses.
10. Make application-runtime-owned Builder finalization produce immutable
    `Agent.definition`; delete standalone `Builder.build_safe` and route direct
    Agent construction through `Execution_scope.build_direct`.
11. Replace Agent-as-Tool with stable-media `Agent_input`/`Agent_response`
    codecs and the authority-bound, start-or-resume `Agent_child_runtime`
    adapter.
12. Delete duplicate Agent-as-Tool/handoff parsers and execution paths,
    including `agent_runner`, summarizers, typed-to-untyped bridges, and
    undeclared `raw_input` correlation transport.
13. Route awaited serial/concurrent collections through `Executable_plan`;
    retain race semantics only as a separately named combinator.
14. Make Tool catalog construction reject duplicate identity/name rather than
    using last-writer-wins.
15. Hard-cut flat schema/validation paths to the canonical schema codec,
    including MCP conversion.
16. Connect each finite Agent/turn/provider/tool lifecycle to one checked
    Execution Journal access and reject a second local scope claim over it.
17. Delete duplicate execution writers and flat pairing projections after
    their required facts are journal projections.
18. Add the injectable operation-backend SPI and one OAS-native reference
    backend.
19. Add atomic durable prepare/publish/activate, reconciliation, cancellation,
    fencing, and operation cursor protocols.
20. Add one application-lifetime `Execution_runtime`; register every Agent
    definition through it and delete per-Agent async registries, pools, backend
    authority tables, and repair scans.
21. Add full-fence operation recovery transitions, no-attempt claimed
    cancellation, exact predecessor release, and effect uncertainty facts.
22. Add paged readiness/repair scans and durable per-event repair claims;
    remove unbounded reference scans and process-local-only repair ownership.

No old and new Tool dispatch or execution writer remains active together.
Tests for retired compatibility behavior are deleted; behavioral invariants
remain.

## 15. Required tests

### Typed algebra and schema

- Heterogeneous calls execute without casts.
- A decoder cannot be paired with another executable’s runner.
- Duplicate executable revision registration and Tool exposure identity are
  rejected.
- Multiple Tool revisions may reference the same registered executable without
  duplicate executable registration.
- A Tool cannot forge a second binding under an existing executable reference.
- `Tool.create` binds one schema/durable input codec bundle to the runner, and
  `reexpose` cannot change that typed bundle.
- JSON Schema codecs are built from the typed combinator algebra; no public
  API accepts an unrelated schema plus handwritten decoder.
- Canonical JSON `number` round-trips through provider and MCP projections as
  `Finite_number.t`; construction rejects `NaN` and infinities before encoding.
- `Tool_catalog.seal` merges façade-created registry fragments and returns the
  exact registry used by Agent execution.
- Cooperative runners stay on the Eio fiber; CPU runners use the one bounded
  application executor at the full-worker CPU-only contract, cannot access
  `Execution_context`, and surface capacity refusal as typed infrastructure
  failure. Fractional Eio weight is not exposed by this constructor.
- Cancelling a queued CPU job prevents its body from executing while retaining
  bounded admission until the pool acknowledges it. Cancelling the submitting
  Tool switch cannot release that admission early: the runtime-owned waiter
  holds it until actual pool completion. A running job observes
  `Cpu_cancellation` at declared checkpoints; a
  non-cooperating job remains visibly occupied until return and is never
  replaced by a timeout heuristic.
- CPU snapshots always satisfy
  `admitted = queued + running <= cpu_admission_capacity` and
  `running <= cpu_workers`; the configured admission value never means
  “waiting jobs in addition to all workers”.
- Reusing a Tool exposure ID with a changed schema under the same revision is
  rejected.
- Hook policy and audit receive the full Tool exposure `(id, revision)`; two
  revisions of one Tool ID cannot collapse into one hook identity.
- Duplicate provider-visible Tool names are rejected.
- Exact identity lookup does not classify names.
- Nested array/object/`oneOf` schemas round-trip through every provider/MCP
  projection without flattening.
- Duplicate required/optional/discriminator fields fail `object_`; no exception
  or last-write-wins schema is produced.
- `const` exposure fields plus the two-field discriminator generate and decode
  the exact batch schema shown in §7. The private streaming batch codec emits
  `minItems: 1` without decoding the array to an OCaml list; an empty batch is
  unrepresentable.
- No provider Tool dispatch path accepts `Yojson.Safe.t`. A Tool argument split
  across one-event pages validates identically to a coarser page, and an
  ordinary materializing codec obtains one exact node/payload allocation lease
  before its second pass. Capacity refusal creates no partial decoded value.
- Two concurrent materializing decodes whose combined node or copied-byte
  vectors exceed one runtime capacity serialize without either holding a
  partial dimension; a small decode in another Keeper/scope proceeds after the
  next exact release. Decode error, handler error, cancellation, and terminal
  preparation error each return the same lease once. The terminal commit API
  cannot receive or retain that lease. Runtime stop wakes queued decoders and
  reaches zero accounted nodes and bytes without dropping an admitted result.
- Provider Tool decoding opens only from the exact checked
  `Invocation_opened_fact`. Substituting another invocation's committed
  arguments, provider attempt, ToolUse source, exposure, adapter, or declared
  Tool name, or passing a different Tool/codec fails before the argument cursor
  advances; a programmatic invocation cannot obtain a provider decode request.
  A successful decoded token owns one lease, can run once, and releases before
  terminal commit on every return/error/cancellation path.
- A batch larger than every page setting incrementally seals the same
  `Executable_call_source` digest and never retains a whole call/case/member
  list. Synchronous and asynchronous adapters consume that source by ordinal.
- A throwing `iso` encoder, case projector, or decoder-side injector becomes the
  corresponding typed codec error while Eio cancellation remains cancellation.
- Throwing durable input, success, failure, effect, checkpoint, and async-digest
  encoders become typed `Encode_error`; a result already returned by a handler
  remains durably attributable. Eio cancellation is never caught as an encode
  failure.
- Both success and failure disclosure projectors return `result`. A throwing
  failure projector preserves the native declared failure, emits the
  infrastructure ToolResult/projection error, and preserves Eio cancellation.
- Caller mutation of the `bytes` passed to
  `Inline_bytes.of_bytes_slice_copy` cannot
  change an already encoded/digested value or a frame crossing another fiber.
- `Inline_bytes.of_string_slice_copy` owns only the requested bounded slice;
  retaining the original larger string cannot keep its backing storage alive
  through the resulting value.
- A batch containing two revisions of one Tool ID dispatches by the full
  exposure reference and cannot collapse those cases.
- Equal inputs produce distinct invocation identities.
- The runtime identity source produces unique IDs within each closed kind,
  rejects source exhaustion/collision, and cannot exchange bytes between kinds.
  A public consumer compile test cannot call an occurrence `fresh` function,
  inject a raw provider adapter/commit handler, or supply operation/repair owner
  identities.
- An unavailable executable revision fails recovery explicitly.
- A public compile test cannot call `Executable_plan.Internal.run`, obtain
  `Execution_context.Internal.async_submissions`, or construct an
  `Async_submission_client`; public composition remains
  `Tool_batch.expose`/`Async_tool_batch.expose`.
- A `Replay_safe` executable cannot perform an external effect through an
  undeclared protocol. An `External_effect` executable can use only the exact
  protocol set bound into its attempt-scoped `Effect_boundary`; refusal occurs
  before entry commit or external I/O and is typed.

### Atomic manifests and persistent definitions

- Two owners building multi-member composite values cannot each hold a partial
  global lease while waiting for another. One aggregate lease is acquired
  before child construction; member creation never waits for more global
  bytes. FIFO/owner-round-robin admission prevents reacquisition bypass.
- Aggregate member declarations and records that exceed the explicitly
  acquired transaction capacity fail typed and abort the whole aggregate; no
  prefix becomes visible.
- Missing, unused, duplicate, wrong-purpose, and cross-transaction root
  attachments are rejected before root commit. A failed root commit exposes
  zero committed manifests/facts; a successful root exposes every owning fact
  and root together.
- Compile-negative fixtures cannot attach an
  `Execution_manifest_purpose.provider_observations` root to a Tool-result
  field. Missing/duplicate slots fail `seal_attachments`, and a complete proof
  cannot accompany another typed transaction. Exchanging fields or
  attachments between two live builders of the same phantom fact type is
  rejected by the builder-instance token, and no public code can obtain a raw
  typed transaction from the sealed semantic fact.
- The closed semantic-fact catalog produces at least one named field for each
  of all fifteen manifest purposes. Every `execution_value_owner_kind`
  constructor round-trips its exact semantic fact kind; an unknown/catch-all
  owner cannot be encoded. Sealing a typed transaction under another catalog
  family, omitting one required field, adding an unlisted root, or attaching a
  field from the same family but another live instance fails before root
  commit.
- Crash injection covers before the first pending page, mid-copy, after pending
  seal, before/after root durability, reply loss, and cleanup. An uncertain
  commit rejects a fresh commit and resolves only through its exact pending
  reference. Startup distinguishes committed, proven-aborted, corrupt, and
  uncertain pending entries without TTL/path/newest inference.
- Commit, abort, codec error, cancellation, switch exit, and runtime close
  restore all exact staging/pending charges and wake blocked waiters. A
  one-shot prepared graph cannot be reused after commit.
- Reusable Agent definitions/preludes retain no transient staging handle. Two
  AgentRuns may share one immutable definition/prelude digest but receive
  distinct one-shot run-open roots. One AgentRun commits its prelude once;
  every turn and N-hop Tool exchange references the same mandatory base.
  Duplicate run-open commit is `Already_consumed`.
- Resume requires the committed definition/prelude roots and exact digest.
  Changed primary/fallback binding, Tool/executable revision, prelude, or typed
  configuration requires a new AgentRun.
- Provider exposure, executable calls/results, and submission
  operations/receipts reject wrong-purpose roots and page corruption. Their
  full structures cannot be read or encoded through a whole-list API.
- Provider exposure construction rejects a duplicate provider Tool name even
  when the two entries straddle a manifest page boundary. Exact lookup uses
  the snapshot's committed name index; a missing or corrupt indexed page fails
  before provider dispatch and cannot fall back to the current Tool catalog.
- Tool-offer preparation accepts only one checked `Provider_bound_adapter`;
  no call can substitute a loose binding, adapter, or wire contract. A prepared
  plan is unusable until `Provider_attempt_open_preparation.commit` atomically
  promotes its exposure root, wire digest/index, opened fact, and committed
  plan. Crash/reply-loss at that root returns the same plan or proven abort,
  never an orphan snapshot or a second offer authority.
- `Provider_binding.create` accepts no ID/revision argument.
  `Provider_config`'s opaque binding reference,
  `Provider_wire_contract.binding`, and `Provider_binding.reference` are
  byte-equal by construction; a mismatched config/contract/custom adapter fails
  before registry insertion, and moving the same binding to another physical
  endpoint cannot mint another reference or change capabilities.
- The same submission operations encoded with different permitted page sizes
  produce the same canonical request digest. Receipt validation rejects an
  omitted, duplicated, reordered, or cross-page operation before publication,
  and crash/reply-loss at every builder seal/root-commit boundary resumes the
  exact prepared source rather than rebuilding a list.
- The operation effect source pages every entered/receipt/unknown transition
  in Journal order at the exact takeover high-water. A page boundary between
  entry and receipt, a corrupt ordinal, another attempt, an incomplete fold,
  and a live-tail source fail typed; no `latest effect` projection can hide an
  earlier open or uncertain effect. Every open/settled recovery view retains
  its exact entry fact and, where present, receipt fact; constructing
  `Effect_unknown` from a bare entry/receipt or a fact from another fold is
  impossible or rejected.

### Recursive topology

- Child invocation and child Agent run attach beneath the owning attempt.
- A child cannot open before the attempt.
- A parent cannot close while a structural synchronous child is open.
- Programmatic child calls require no provider ToolUse ID.
- Provider origin preserves its exact grammar-specific native correlation:
  Chat indices/call ID, distinct Responses response/item/output/call IDs, or
  declared name/order identity. Source key plus adapter/attempt remains the OAS
  identity.
- Agent input schema and durable codecs round-trip Text, Image, Document, Audio,
  and Video without flattening; reasoning and Tool protocol blocks are rejected
  as caller input by closed variant matching.
- Agent response codecs preserve reasoning signatures/details, multimodal
  blocks, usage, and telemetry; detailed failure codecs preserve provider
  failure attribution.
- URL and provider-file media cannot inhabit `Agent_input.t`; response media
  must stabilize to inline/blob identity before the total output codec runs,
  and stabilization failure is an infrastructure terminal.
- The provider wire input is recorded once on the invocation and canonical
  Agent input once on the executable call; no `raw_input` copy exists.
- Child Agent execution uses the authority-bound parent invocation/attempt,
  checked Journal access, media stabilizer, and switch. Parent cancellation
  cancels the child without converting `Eio.Cancel.Cancelled` into a domain
  failure.
- Concurrent calls from one Agent definition receive fresh Agent state and
  distinct AgentRun occurrence IDs; conversation state cannot leak between
  them.
- Recovery reuses an existing terminal child AgentRun instead of opening a new
  occurrence. Recovery of an open child resumes the same AgentRun ID and exact
  checkpoint; missing checkpoints or revisions fail rather than opening a
  second occurrence. A disclosure failure cannot erase the durable native
  result.
- AgentRun creation atomically commits the initial checkpoint. Every later
  checkpoint and terminal compares the prior checkpoint fact reference, so no created
  occurrence is uninspectable and concurrent resumptions cannot both advance.
- Child Agent success, declared failure, cancellation, and infrastructure
  failure commit distinct closed terminal variants. Cancellation commits under
  protected cleanup and re-raises the original cancellation; terminal-append
  failure remains in the scope aggregate and the durable open run stays
  recoverable rather than being reported closed.
- Definition finalization rejects captured live writers, switches, mutable
  callback accumulators, and checkpoint sinks; per-run factories create those
  resources independently for concurrent invocations.
- Direct Agent construction goes through `Execution_scope.build_direct`;
  no standalone Builder path can bypass either the application-runtime
  authority or finite-scope authority.

### Synchronous collections

- Serial children start and finish in input order.
- Concurrent children may overlap and return in input order.
- One declared child failure does not cancel siblings.
- Infrastructure failure cancels unfinished siblings and is an outer error.
- Infrastructure failure propagates through the generated composite without
  becoming a declared domain failure or an untyped exception.
- Parent cancellation settles every accepted child.
- Programmatic empty collection returns an exact empty aggregate.
- Provider batch schema rejects an empty `calls` array.
- A call spanning multiple input/content pages retains one contiguous ordinal
  and exact binding/exposure. Wrong-purpose, duplicate, missing, or
  out-of-range call/result ordinals fail typed.
- A crash immediately after plan open has allocated no child invocation.
  `Member_started` alone mints it; serial not-started ordinals remain
  identity-free. Crash after child terminal but before result append reuses
  that terminal fact and never opens another child.
- Concurrent completion may arrive out of order, but paged result reads and
  Tool projection remain input-ordinal ordered without materializing the
  entire result set.
- Provider batch wrapper and union cases reject undeclared object properties.
- Provider sibling scheduling and composite child mode remain independent.
- Recovery of a partially complete composite retains terminal child identities
  and never reruns their external effects.
- Tool collections retain each member's disclosure witness without matching an
  erased outcome back by identifier.
- Member success-projection failure preserves the native child success and
  fails the parent at the infrastructure boundary.
- With recursive-execution capacity fully occupied, Agent→Tool, Tool→Tool, and
  Agent→Agent synchronous children each receive the parent's handed-off slot;
  no admitted parent waits while retaining a valid slot. Nested concurrent
  composites refill only from returned or newly admitted slots and cannot
  exceed `execution_concurrency`.
- A stale parent slot, wrong child owner, duplicate return, or sibling-slot
  return is a typed infrastructure/corruption failure and never increases
  capacity.

### Hooks and durability

- Hook invocation identity equals the journal invocation node identity.
- Public hook code cannot obtain network/filesystem handles, an
  `Effect_boundary`, submission authority, or Journal writer. An external
  action must cross the Executable/effect/commit-action lifecycle and is not
  rerun as a hook observer during recovery.
- Provider-origin hook context carries exact `Provider_wire`; programmatic and
  nested context carries exact `Canonical_input`. Cross-origin evidence and a
  fabricated empty provider object are rejected.
- Every continued Tool commits an attempt before handler entry.
- Every provider-visible ToolResult is durable before post hooks and the next
  provider call.
- Blocked calls run no handler and open no attempt.
- Declared failure invokes `PostToolUse`, then `PostToolUseFailure`.
- The closed post-observer matrix is exercised for success, declared failure,
  blocked, invalid input, cancellation, and infrastructure failure. PreTool
  observer failure commits infrastructure failure; no path silently skips or
  duplicates the phase/ordinal observer.
- One observer failure does not suppress the next observer.
- Post-hook failure preserves the already committed ToolResult.
- A result and its optional `Commit_action_pending` fact commit atomically.
- Commit action execution receives the committed publication event identity,
  and crash recovery resumes the exact idempotent action revision until one
  durable terminal fact exists.
- Activation uncertainty preserves the committed result and records a
  nonterminal reconciliation-progress fact before post observers.
- Startup and explicit reconciliation enumerate and resume open commit actions
  without an elapsed-time retry rule.
- Open actions execute only through their exact registered handler revision;
  a missing old handler fails runtime readiness, leaves the action open, and
  never falls forward to the newest.
- Concurrent repair triggers for one exact pending fact perform one backend action;
  distinct actions respect explicit repair capacity, and terminal CAS permits
  one exact terminal fact.
- A second process cannot execute a repair without the durable pending-fact
  claim. Explicit higher-epoch takeover invalidates the old full claim; no time
  lease participates.
- Two facts sharing one transaction event ID but having different ordinals
  cannot share a repair claim, progress link, trigger, or terminal link.
- Required-reference and open-action scans page to `caught_up` using the
  runtime-owned page size under one immutable high-water cursor. Exact handler
  dependency decoding verifies async-runtime revisions, a read gap or partial
  scan fails readiness, and the tail starts at that same high-water.
- Effect entry and receipt codecs preserve the exact effect, invocation,
  attempt, protocol revision, fence, idempotency key, and entry fact reference.
- An effect entry with a mismatched fence or idempotency identity is rejected
  before journal append; a receipt cannot name another entry or protocol.
- Entry commit failure invokes the external `execute` function zero times.
  A second open entry, receipt without entry, mismatched attempt/cursor, and
  ordinary terminal with an open effect are rejected.
- Receipt commit failure and uncertain commit return the reserved
  `Effect_execution_error` path, not a declared domain failure.
- `Effect_unknown` round-trips the exact entry ID, protocol revision, entry
  fact, receipt fact when present, and evidence-specific typed uncertainty
  cause. Open and settled
  evidence cannot exchange causes.
- An `External_effect` with a reconciled receipt but no executable terminal
  cannot take the composite `Resumed` transition; it closes
  `Outcome_unknown` unless reconciliation proves the effect absent and no
  earlier receipt exists. Reconciled Completed/Failed values atomically become
  a typed receipt plus settled unknown evidence; they are never discarded.
- Replaying an identical typed transition returns its original cursor without
  a duplicate event; the same transition key with changed payload is
  corruption.

### Provider continuation

- Every normally closed provider ToolUse that is eligible for continuation has
  exactly one matching ToolResult; if result commit fails, continuation is
  forbidden.
- An async ToolResult contains receipts, not delayed child results.
- Later async completion cannot be inserted into a closed original turn.
- Required reasoning/thinking/signature items survive stateless continuation.
- Attempt selection accepts only committed per-exchange selection witnesses that
  wrap successful terminals; bare success, failed, or retried attempt IDs cannot
  authorize replay, and missing selection is an error rather than a downgrade.
- Duplicate source keys within a snapshot, within ToolResults, or across the
  snapshot/ToolResult boundary fail before adapter execution; one key cannot
  satisfy coverage for two facts.
- Dashboard text is never provider replay input.
- Rebuilding a continuation from identical attributed source fragments is
  byte-identical and never duplicates an item from the prior serialized
  request.
- Each continuation item carries a nonempty in-item attribution set; one native
  item may group sources and one source may have contiguous explicit fragments,
  without a parallel source/request list.
- The exact provider adapter assembles call/result ordering from typed finalized
  items and closed ToolResult outcomes; generic planning verifies the complete
  authorized source-key set and never parses provider JSON to place results.
- Whole-attempt finalization rejects negative, missing, duplicate, invented, or
  reordered item ordinals. Any one rejection yields zero finalized/replayable
  items and zero dispatch authority for that candidate.
- A ToolUse name absent from the exact committed exposure snapshot, an exposure
  changed under the same reference, or a context whose binding/adapter/contract
  differs from the `Provider_bound_adapter` rejects the whole attempt before
  commit.
- The exact binding/adapter/contract-qualified Tool offer plan is committed
  before dispatch. The request serializer can consume only its projection, and
  the attempt/read model retains the same snapshot plus wire-definition
  digest. A changed, omitted, extra, or current-catalog Tool definition fails
  before HTTP dispatch or response resolution.
- Binding registry lookup returns one checked bound package. Deliberately
  pairing an Ollama-native adapter with an OpenAI-chat contract, or any adapter
  that rejects the exact contract revision, is unconstructible/typed failure.
- Success-transaction abort or uncertain commit returns no finalized item,
  source key, terminal-native witness, successful-attempt witness, selection
  witness, ToolUse reference, or dispatch authority. A later exact Journal read
  is required to resolve uncertainty; failure recording cannot create success.
- Delta coverage is `None` iff that item has no committed deltas and otherwise
  equals the exact highest contiguous sequence at commit. Missing, cross-item,
  truncated-prefix, or invented coverage fails the whole success transaction.
- Request construction, synchronous parsing, streaming parsing,
  whole-attempt finalization, and continuation observe the same wire-contract
  reference for one binding; a path/payload-shape alternate cannot substitute
  another contract.
- ToolUse/ToolResult correlation uses one reference containing source adapter,
  provider attempt, finalized ToolUse source key, and the closed
  grammar-specific native correlation. Required missing IDs, reused
  cross-attempt IDs, swapped Responses item/call IDs, and cross-choice indices
  fail rather than collide.
- Provider call/response/item IDs split across arbitrarily many transport pages
  remain staged paged scalars until the success root. A near-cap scalar,
  digest-collision byte mismatch, cross-role wrapper, and scalar from another
  attempt fail typed without a whole string allocation; committed continuation
  writes the exact pages back.
- Thinking, reasoning, reasoning-summary, text, refusal, and multimodal item
  kinds each round-trip independently through synchronous parse, streaming
  finalization, committed item, continuation, and hierarchical read view.
  Equal bytes in `Reasoning` and `Reasoning_summary`, or in `Text` and
  `Refusal`, never collapse their variants.
- Fallback across provider adapters either uses an explicit tagged-source
  translation path or fails typed; the target adapter never infers a source
  provider from native JSON.
- Failed provider attempts and streaming deltas remain observable but never
  become additional replay turns.
- Provider timeout, cancellation, premature EOF, and protocol failure preserve
  the committed caller input, attempt identity, and every prior delta. Failure
  recording failure returns both causes and never collapses into a vanished
  turn or generic watchdog timeout.
- A restart with an open provider attempt first reconciles its exact pending
  root. With no root and no surviving transport it commits the same
  attempt's runtime-interruption failure, retaining prior deltas, before any
  new retry can open. No elapsed watchdog or “latest attempt” scan participates.
- A same-turn N-hop Tool loop opens
  `Exchange₀ -> ToolResults -> Exchange₁ -> ...` with one selection per
  exchange. Each continuation root atomically covers its prior-exchange edge
  and every ordinal ToolResult edge; missing/extra/duplicate/cross-turn edges
  reject the root.
- `Fail_on_overflow` preserves the complete immutable request snapshot and
  never changes the retry prompt. `Explicit_lossy_auto` makes zero HTTP calls
  when the exact binding lacks support and, when supported, records
  `Provider_may_have_truncated`; it cannot claim exact input delivery.
- Every Tool-choice mode is verified independently for the exact deployed
  binding revision. Unsupported/unverified modes perform zero HTTP requests;
  a server that silently violates an accepted forced-mode response obligation
  produces a typed protocol failure rather than successful continuation.
- Exact OpenAI-compatible fixtures cover missing/reused call IDs,
  name-versus-call-ID result carriers, partial streamed arguments,
  `reasoning_content` versus native reasoning items/signatures, malformed SSE
  followed by a terminal sentinel, finish/content mismatch, absent usage,
  stateless Responses, NDJSON termination, and source-adapter fallback. Each
  either produces the contract-declared typed value or fails the whole attempt
  while retaining the contract-declared bounded native evidence and explicit
  overflow marker; no generic payload probing or silent downgrade
  is allowed.
- SSE fixtures cover arbitrary TCP split/coalescing, CRLF (including split
  CR/LF), bare LF, bare CR, `data`/`data:` empty fields, multi-`data:`
  folding, comment heartbeats, BOM/UTF-8 and Content-Type mismatch. A buffered
  event without its blank-line delimiter is discarded at EOF and the attempt
  follows its declared premature-EOF rule. `[DONE]` is
  recognized only as the contract's exact completed data event, never as a
  substring or JSON content; duplicate terminal and data-after-terminal fail.
- Native NDJSON fixtures cover a final object carrying delta + `done:true` +
  usage/timing, valid deltas followed by the documented `{"error":...}` form,
  partial final line at EOF, duplicate done, and data-after-done. Prior facts
  and bounded evidence survive, but no success witness is minted.
- Chat Tool streams preserve parallel `tool_call.index` lanes, accept ID/name/
  type only where the exact grammar permits, and dispatch arguments only after
  terminal JSON and Tool-schema validation. Changed index/ID/name and duplicate
  JSON keys fail the whole attempt.
- Responses streams preserve distinct response ID, item ID, output index, and
  call ID across added → argument-delta → argument-done → item-done →
  response-completed events. Item ID unequal to call ID round-trips; swapped,
  duplicate, missing, reordered, or cross-output links fail.
- Multiple Chat choices are either rejected at request construction by a
  contract that supports exactly one choice or retained under typed
  `Provider_choice_id`; interleaved choice 0/1 data can never merge through a
  `choices[0]` shortcut. A usage-only `choices=[]` event is not an empty
  completion.
- The per-attempt provider frame queue preserves every contiguous FIFO frame
  under saturation, backpressures its producer, delivers terminal exactly once
  after accepted frames, and wakes blocked producer/consumer typed on
  cancellation, transport failure, and runtime close. Terminal consumption
  followed by another take is a typed closed state, not an empty frame.
- Count and byte saturation are tested independently: one giant frame, many
  near-cap frames, an unterminated SSE/NDJSON line, many small frames growing
  one Tool argument, decompression expansion, and simultaneous attempts never
  exceed the global/attempt lineage. Consumer protocol abort wakes a producer
  blocked on capacity; cancel/parse/evidence failure leaks no ingress, taken,
  retained, or whole-attempt lease. Two attempts cannot partial-reserve into a
  hold-and-wait deadlock.
- A zero-byte frame, immediate-EOF/header-only failure, zero-byte UTF-8/tool
  arguments, and zero-byte binary evidence round-trip without a fabricated
  chunk. `None,false` pages are impossible. One near-cap event and malformed
  binary body split across many transport/spool pages reconstruct byte-exactly
  without a whole-frame/body allocation; stale/cross-frame/read-after-finish
  cursors fail typed.
- Evidence overflow commits the exact admitted binary prefix, checked
  `received_at_least`, digest, and completeness. Crash before the failure root
  cleans staged evidence; crash after it reopens only committed evidence.
- Semantic values prepared under provider attempt A are rejected by attempt B.
  Success commits semantic/native/metric roots atomically; failed finalization
  publishes none of those success roots.
- Whole-attempt finalization and success commit consume bounded ordinal pages
  from the immutable spool; a 131K-class output is never rematerialized as one
  `item list`, and a spool gap/digest mismatch fails typed.
- Two or more non-Tool items split across one-item pages each commit their own
  semantic manifest through the exact `(attempt, item ordinal)` attachment
  token. Missing/duplicate slots, attaching item A's semantic root to item B,
  a ToolUse with a semantic slot, another attempt's slot, and a source
  count/digest mismatch reject the whole provider success root; no item prefix
  becomes visible.
- Exact adapters alone construct Reported/Unreported usage, finish, and timing.
  Native seconds/milliseconds/nanoseconds conversion rejects negative,
  overflow, NaN, infinity, and unsupported fractional precision as protocol
  errors. Generic code never parses terminal JSON, estimates tokens from text,
  or assumes arithmetic identities among overlapping input/output/total/cached/
  reasoning counters.
- Duplicate source fragments, missing/unauthorized source keys, unresolved
  ToolUse references, duplicate ToolResults, and provider-invalid adjacency
  fail before the next provider request.

### Asynchronous collections

- No receipt is returned before atomic durable acceptance.
- Each prepared operation stores one exact `Operation_execution_anchor` in the
  intent, receipt, digest material, backend state, claim, execution-Journal
  binding, attempt, effect, and read-routing path.
- `Submission_intent` commits before the first backend call.
- The private `Async_submission_client` mints an opaque prepared identity from
  the runtime; a public `Async_tool_batch.expose` caller cannot supply a submission
  occurrence. Replaying the same
  prepared token is idempotent, while changed canonical material is conflict.
- Crash after intent commit, backend prepare, authoritative result commit, and
  commit-action pending/activation is injected at each boundary. The indexed
  open-intent scan/tail resumes the same owning invocation and submission,
  never prepares a fresh ID, and commits at most one result/publication.
- Persist `Durable_header` plus every `Durable_operation`, terminate the whole
  process, create a new application runtime, explicitly register the original
  durable root history and exact async-runtime/backend package, then decode the
  stored values. `Reopener.with_reopened` point-loads the one committed intent;
  lockstep operation paging, `bind_operation`, and `input_decoder` reproduce
  every canonical input byte-for-byte without backend JSON-field parsing or a
  general Journal reader.
- The restart matrix independently changes the packed backend authority,
  durable backend ID, async-runtime ID/revision, submission ID, request digest,
  intent fact, root stream route, operation-source root/count/byte-count/digest,
  operation ordinal, generated operation/invocation IDs, and anchor. Every
  change fails typed before the reopen callback, receipt construction, claim
  transition, or recovery transition; combining header A with operation B can
  never expose either input.
- A missing, volatile, closing, unregistered, corrupt, or wrong-stream root
  route makes restart reconciliation/claim/recovery return the exact typed
  reopen failure and invokes the callback zero times. Registering the correct
  durable history then permits the same immutable header; no scan, path probe,
  newest-runtime fallback, or empty-input fallback occurs.
- Success, callback `Error`, cancellation, decoder failure, and route close each
  invalidate the scoped reopened request. A retained operations reader,
  rebound operation, or input decoder subsequently fails typed and retains no
  read-admission lease.
- A compile-surface test proves only durable `read_state` reconstruction,
  backend reconciliation, ready-claim, and recovery-page methods receive
  `Submission_backend_request.Reopener.t`; prepare receives the live request,
  while activation, cancellation, event, fence, and transition methods receive
  neither a request nor a general reader.
- Rejected-before-commit commits one terminal submission rejection fact and
  proves no backend operation/publication child exists. Crash/reply-loss and
  restart return that same rejection and never prepare the request again.
- Backend-prepared operations cannot be claimed before the authoritative OAS
  publication commits.
- Backend activation applies the closed `publication_activated` transition from
  `Pending_publication` to `Accepted`; duplicate proof is idempotent and a
  different proof is corruption.
- If cancellation terminally wins from `Pending_publication`, later replay of
  the exact activation proof is a successful no-op and cannot resurrect the
  operation; a different proof is corruption.
- `Operation_state.status` reports terminal cancellation both before and after
  that later activation proof, and `claim_ready` returns no claim in either
  state.
- In a mixed submission, activation atomically records one proof for both
  pending-cancelled and ready operations, preserving terminal cancellation and
  accepting only the ready operations; transaction failure changes none.
- A crash between backend prepare and OAS publication repairs that publication
  under the same submission identity and digest.
- A crash between OAS publication and backend activation repairs activation
  without writing a second publication fact or, for a provider Tool, a second
  ToolResult.
- Activation reply uncertainty leaves the immutable submission result
  `Accepted` and exposes a pending-activation reconciliation state.
- Backend prepare uncertainty before authoritative OAS publication yields
  `Reconciliation_required` under the same submission ID and digest.
- A volatile root rejects a definition containing async submission,
  `External_effect`, or resumable-composite durability reasons before any
  provider/backend/effect call. A durable root accepts the same definition and
  preserves its recovery evidence across restart.
- Backend or Journal reconciliation read failure returns a typed error and an
  exact failed-attempt observation; it cannot masquerade as a successfully
  established `Publication_uncertain` durable state.
- A backend receipt with a wrong submission/digest, missing/duplicate/reordered
  operation, changed ordinal/binding/exposure/context/causal attempt/anchor, or
  mismatched cursor stream fails whole-receipt validation before publication;
  zero operations become caller-authoritative.
- Crash after authoritative publication but before response or activation
  confirmation recovers `Accepted_prior` and repairs activation without a
  second publication fact.
- Reusing that submission ID with a different canonical request digest returns
  `Idempotency_conflict`.
- The same calls under another async-runtime revision produce different
  canonical material; reusing the submission ID is a conflict, and an
  intent-only crash resolves the exact stored runtime/backend revision.
- Serial eligibility survives restart and advances after declared failure.
- Concurrent operations are independently eligible.
- A finite Agent run can finish while accepted operations continue.
- Cancellation request and terminal cancellation are distinct durable states.
- An accepted but unclaimed operation can commit cancellation-before-start
  without fabricating an operation claim.
- `claim_ready` atomically persists one fresh claim identity before returning
  it; duplicate/stale claims and a second execution-start reference under one
  claim are rejected by the reducer.
- Commit-success/reply-loss of a claim page is replayed by the exact
  `Operation_claim_batch_request`; it returns the byte-equal claims even though
  they left Ready. Reusing its ID with another cursor/count/fence is corruption,
  and a process crash recovers every claimed member in the startup scan.
- With requested page size one, two ready claims, recovery candidates,
  submission intents, repair requirements, and open actions emitted as distinct
  ordinals of one atomic event resume at ordinal 0 then ordinal 1 without skip
  or duplication. The same law holds for tail pages and restart.
- Reusing a claim ID with changed receipt, input, backend, or fence is
  corruption. A strictly newer authoritative fence may supersede only the
  exact pre-attempt claim, after which every old-claim transition is stale.
- A predecessor-waiting operation cannot be claimed until a durable barrier
  release names and validates the exact predecessor terminal event, then
  returns it to ready.
- Cancellation while claimed commits terminal cancellation without fabricating
  an attempt; every later execution-start link is stale. Cancellation while
  running preserves the exact claim/start-reference ownership needed for the
  worker's Journal terminal and backend-link transitions.
- Cancellation while `Starting` before Journal open resolves
  `Cancelled_before_start` under the exact start intent. Cancellation after
  Journal open but before backend linkage commits a Journal cancellation
  terminal under takeover and links that fact; neither window leaves an orphan
  attempt or reports both outcomes.
- The sealed-absence race is injected between backend takeover, Journal
  inspection, old-worker start append, and cancellation. Exactly one of an
  existing start or the matching no-start tombstone commits; a tombstoned old
  fence can never append a start, and
  `Operation_recovery_decision.after_start_absence` rejects another intent or
  takeover.
- One asynchronous child failure does not stop siblings.
- Serial successors wait visibly on `Outcome_unknown` or `Recovery_failed`
  predecessors and consume no worker.
- Terminal facts remain readable from an `Execution_fact_ref` in the anchor's
  durable execution stream.
- Receipt `backend_event_stream` equals its accepted cursor stream; the exact
  distinct execution stream is owned by its anchor.
- Cursor gaps are explicit.
- External attempt without a reconcilable receipt becomes `Outcome_unknown`.
- `Outcome_unknown` is never automatically retried.
- Supervisor startup performs a fixed-high-water indexed scan of every
  `Claimed`, `Starting`, `Running`, `Cancelling`,
  `Commit_action_handoff_prepared`, and
  `Commit_action_repair_released` state before new claims, then continuously
  drains the lost-wakeup-safe recovery tail.
- A crash after execution-Journal start but before backend start linkage is
  repaired by `inspect_or_seal_start`; a crash after Journal terminal but before
  backend terminal linkage adopts that exact terminal before any handler or
  reconciler runs.
- Crash before and after Journal start under one durable `Starting` intent
  respectively reclaim without an attempt or link the one existing attempt;
  changed canonical input digest, second attempt, and second start reference
  are rejected.
- `begin_takeover` atomically installs a Journal fence marker and observes
  whether a prior terminal exists; an old fence cannot append after the marker.
- Commit-success/reply-loss of `take_operation_fence` returns the byte-equal
  opaque takeover on retry. Taking over operation A does not stale a claim or
  transition for sibling operation B.
- Retrying `begin_takeover` with the same prior writer establishment and new
  operation fence returns the same marker, writer authority, and terminal
  snapshot; a different lineage is corruption.
- Recovery of a running operation reads exact Journal effect facts and commits
  one closed Journal recovery outcome followed by a backend reference link.
  Replay-ready, resumed-same-attempt, reconciled terminal, outcome unknown, and
  recovery failure cannot be encoded as one another.
- `recovery_resolved` cannot be constructed from loose subject/resolution
  enums. Journal-bearing outcomes require the exact backend-fence takeover and
  matching Journal takeover witness; pre-attempt outcomes reject such a
  fabricated Journal lineage.
- A stale or owner-mismatched operation fence/current writer authority cannot
  claim, link execution,
  enter a Journal effect, cancel, recover, or link a terminal, even if its
  numeric epoch happens to match.
- Every provider-origin async submission outcome projects one authoritative
  ToolResult fact.
- The backend stores only the exact execution terminal reference; output,
  failure, effect, and recovery evidence exist once in the anchor-bound
  Journal.
- Backend-required digest, epoch, fence, claim, transition, and event values
  round-trip through their checked constructors/codecs after restart.
- Async Tool construction and its executor use the same application-runtime
  registry; a missing runtime reference fails definition registration.
- `Async_runtime_config.create_journal` plus
  `Operation_context_factory.of_function` builds and runs an async batch
  without an application implementation of `Operation_backend.S`; finalization
  rejects a conflicting operation-store descriptor or packed backend authority.
  The operation backend store remains distinct from every finite execution
  scope's causal Journal.
- Async runtime references include an exact revision and resolve only through
  the immutable resolver installed in `Execution_context.Internal`.
- Old and new revisions of one async runtime ID may coexist; duplicate
  implementations of the same `(ID, revision)` are rejected.
- Runtime revisions sharing one operation-backend durable ID must reuse one
  packed backend authority; a distinct packed value under that ID fails
  finalization.
- Two Agent definitions registered in one application runtime cannot evade that
  check with separate local registries. Mutating an implementation's source ID
  after `Operation_backend.pack` does not change the package's frozen ID.
- A reader and writer from different Journal authorities cannot form an
  `Execution_journal_access`; a second live scope claim over one checked
  access is rejected before another lane writer or repair supervisor starts.
- Failed scope bootstrap unwinds only its inner switch and scope claim. Scope
  stop does not close the shared CPU pool; explicit application-runtime stop
  joins all scopes independently before joining CPU domains.
- Root route close rejects new readers, drains existing readers, quiesces
  semantic producers before the final scan, and never leaves a route in
  `Closing`. A pending action queued immediately before quiescence is included
  in the empty/handoff disposition.
- Operation terminal, recovery-adopted terminal, and cancellation-before-start
  use the three distinct disposition paths. Every prepared handoff reaches
  source release and one completion or remains indexed; crashes before/after
  quiesce, close, release, reopen, nested activation, and completion retain
  exactly one writer and one durable recovery item.
- Exclusive-reopen release requires acquire, complete close, and the opaque
  close proof. Root repair cannot release its handle without the matching
  completion, and completion is preserved in terminal operation state.
- Publishing another async-runtime revision on an already supervised backend
  is visible to later claims through the current registry generation without
  replacing the backend authority or restarting in-flight operations.
- Old context-factory revisions remain explicitly registered for durable
  operation recovery; a missing revision is `Recovery_failed`.
- Effect recovery resolves the exact protocol revision and typed request codec;
  it never invokes the newest reconciler by fallback.

### Observation, paging, and server isolation

- The generic invocation-terminal read fact round-trips each of
  `Succeeded`, `Declared_failure`, `Blocked`, `Invalid_input`, `Cancelled`,
  and `Infrastructure_failed`. A provider-origin ToolResult is derived from
  the same committed terminal, while a programmatic terminal remains fully
  visible without fabricating provider provenance.
- Provider-native correlation scalars larger than one page are readable
  byte-for-byte through `Read_session.read_provider_native_scalar`; stale,
  cross-stream, post-session, digest-mismatched, and gap cursors fail typed.
  The read path never exposes a general Journal reader or materializes the
  complete scalar.
- Every `Execution_value.t` reachable from an input, terminal, hook, effect,
  checkpoint, or infrastructure fact pages its items, referenced content, and
  annotations through the same read session. One-item/one-byte pages reproduce
  the exact value; cross-value, cross-session, stale, corrupt, and post-session
  cursors fail typed without exposing a raw Journal reader.
- A single continuation-open fact establishing more Tool-result edges than
  every page limit leaves the sequential fact page bounded; `read_edges`
  resumes by projection subordinal with no omission, duplication, or inline
  edge copy. The same fact cursor cannot be reused as an edge cursor.
- Two facts committed by one semantic event carry the exact same observation
  sample. Same-clock open/terminal facts produce exact nanosecond duration;
  reverse time, another clock, restart under a new monotonic clock, or wall-UTC
  coincidence returns typed unavailable and never estimates elapsed time.
- Exact adapters alone construct usage/finish/timing. Missing output tokens or
  generation duration stays `Unreported`; negative, overflow, NaN/infinity, and
  unsupported fractional native units fail. Overlapping total/input/output/
  cached/reasoning counters are never generically summed.
- Core audio/prediction/token and total/load/prompt-eval/eval-duration metrics
  page through one committed observation manifest. Adapter-native metrics keep
  their exact adapter revision, name reference, unit, and checked nonnegative
  value. Duplicate core kind, wrong unit, corrupt page, or overflow is typed;
  usage/timing convenience views derive from that manifest and cannot drift as
  a second encoded record.
- Provider-reported generation rate and OAS end-to-end rate preserve their
  exact numerator, nanosecond denominator, and basis. No live rate is invented
  before the exact contract reports sufficient usage/timing, and display
  conversion cannot produce NaN/infinity.
- Changing only usage, cost, turn, elapsed, or queue/group statistics in two
  otherwise isomorphic executions cannot change admission, dispatch,
  continuation, recovery, or terminal outcome.
- Every lifecycle open/terminal observation remains readable after terminal.
  Observation/group-commit/provider-queue statistics are projections only and
  never feed the next batch size, model control, or scheduling decision.
- With `N > journal_group_max_transactions`, every semantic transaction/fact
  commits exactly once and in order; each physical barrier satisfies both its
  transaction and byte capacities and at least one multi-transaction group is
  observed when input permits it. A hot lane cannot finish its backlog before
  an already-ready cold lane receives a turn.
- Group failure/uncertainty reports zero false-success tickets and recovery
  yields the exact durable prefix. Blocked submitters wake typed on lane/global
  close or cancellation; double, stale, and wrong-lane permit releases fail.
  Capacity matrices violating transaction ≤ group bytes or read-chunk ≤
  per-attempt ≤ global provider bytes fail `Runtime_capacity.create`.
- Provider queue terminal is delivered exactly once after all accepted frames.
  Producer/consumer close, consumer protocol abort, runtime close, and a take
  after terminal all have distinct typed outcomes and leak no byte lineage.
- A hot stream whose every bounded page requires JSON/UTF-8/hash work submits
  each pure step to the shared CPU executor while a cold Keeper continues on
  the Eio domain. The CPU worker receives no I/O resource, and changing page
  size never changes provider semantics or introduces an inline threshold.
- With requested page size one, ordinal siblings in every generic/readiness/
  recovery/repair page resume without skip or duplication. Edge neighborhood
  lookup uses the durable source+target projection index, not a source scan.
- Edge-index crash at projection-row/checkpoint boundaries resumes
  idempotently. Restart and late root-history registration catch up before
  route publication; an incoming cross-stream async edge is visible from its
  target. A read session keeps one index generation/high-water while a newly
  registered historical source becomes visible only to the next session, so
  pagination cannot skip a back-inserted key.

### Hierarchical projection

- Recursive nodes preserve structural and causal edges without flat positional
  pairing.
- Open facts establish explicit AgentRun→Turn→ProviderExchange→ProviderAttempt,
  Invocation→Attempt, and parent-Attempt→child relations; retrying a parent
  invocation cannot attach a child to the wrong parent attempt.
- Streaming deltas and finalized provider items remain distinguishable typed
  facts.
- A collapsed dashboard subtree does not change the underlying read model.
- Normalized node/edge/fact pages allow multiple causal and structural edges
  without recursively duplicating ancestors; unresolved node IDs are fetched
  by exact `read_node`.
- Finalized provider facts explicitly supersede only their own provisional
  delta sequence. Equal text under a different item ID remains distinct.
- Provider attempt open/success/failure/selection and finalized-item source
  adapter, turn, replay eligibility, and exact native canonical payload are
  available from closed fact views.
- Provider attempt open exposes the exact wire-contract reference and committed
  offered-Tool snapshot; success exposes terminal-native finish/usage evidence.
  Conversation input and delta-supersession coverage remain exact typed facts.
- Effect entry/receipt/unknown, operation recovery resolution, and successful
  or failed PostTool observers remain separate typed facts after the operation
  reaches terminal state.
- Recovery facts retain the previous claim, previous attempt, and current full
  operation fence plus Journal writer authority; dashboard projection cannot
  erase takeover provenance.
- Success, declared failure, and infrastructure failure provenance are distinct
  closed fact views.
- Retention gaps remain explicit and never synthesize missing ToolResults.
- `Execution_fact_ref` alone routes a root or operation fact to its exact
  stream; a mismatched embedded stream reference/cursor is rejected before
  read, with no backend or directory scan.
- `Execution_inspector.of_runtime` observes new typed routes without arbitrary
  closures. Scope stop atomically changes a durable root from live access to
  exact read-only reopen, while a volatile root becomes typed unavailable and
  never points to a closed reader.
- After process restart, `register_root_history` verifies and installs the
  exact durable root without starting a writer/scope; a mismatched stream,
  duplicate authority, or unregister during an active session is typed failure.
- Point, stream-page, edge-page, and provider-content reads release their
  admission slot and routed reader before another page waits. Traversing more
  streams than `read_stream_concurrency` therefore cannot deadlock by retaining
  earlier readers, and no public multi-ref aggregate rematerializes all facts.

### Boundaries

- OAS source contains no application-specific executable variant or path.
- Dune-private Tool/Executable eliminators are not exported by `Agent_sdk`.
- No default turn, cost, token, recursion, timeout, or queue-depth gate exists.
- No compaction or memory policy is introduced.
- One checked Journal access owns each OAS-native finite execution scope, and one
  backend owns each external operation namespace.
- Event bus, trace, checkpoint, and dashboard surfaces are read projections.

## 16. Performance properties

- One `Execution_runtime.t` owns the application-lifetime CPU pool, exact
  registries, backend authority table, commit-action handler registry, and
  commit-action repair registry; its identity/observation sources, global
  Journal-append admission, recursive-execution/operation/repair/read
  admissions, atomic Tool-decode node/byte admission, global provider-stream
  byte admission, provider
  frame/per-attempt-byte/read-chunk capacities, and the staging spool with its
  global/per-attempt/page byte capacities are also single runtime
  authorities. Agents and scopes do not duplicate them.
- Each `Execution_scope.t` owns one Journal/lane writer and one repair cursor.
  Scope supervisors borrow the global repair admission, so the total number of
  simultaneously active repair jobs remains bounded across all scopes.
- CPU worker, total CPU admission, global Journal-append admission, per-lane
  Journal queue, physical group-commit, manifest-staging
  global/per-transaction/record/page
  bytes, provider-stream frame/global-byte/
  per-attempt-byte/read-chunk, provider-spool global/per-attempt/page bytes,
  Tool-decode value-node/payload-byte,
  recursive-execution-concurrency, operation-concurrency,
  operation-claim-page, operation-recovery-page, repair-concurrency, and
  repair-page, read-stream-concurrency, read-batch, and read-page-byte
  capacities are
  explicit positive runtime-owner decisions in
  `Runtime_capacity.t`. The
  CPU-only runner is a full-worker contract rather than a caller-supplied
  fractional weight. OAS supplies no numeric defaults.
- Tool-decode node and payload-byte capacity are independent resource units,
  never converted through a guessed object-size ratio. A materializing decode
  obtains the complete two-dimensional vector atomically and retains it only
  for the exact handler lifetime. Streaming Tool collections externalize each
  validated call to their manifest and never acquire a whole-collection
  materialization vector. Saturation is cooperative backpressure and typed
  observability, never a Tool, turn, token, cost, or recursion budget.
- `Runtime_capacity.create` rejects an inconsistent matrix:
  `provider_transport_read_chunk_capacity <=
  provider_stream_per_attempt_byte_capacity <=
  provider_stream_global_byte_capacity`,
  `provider_spool_page_byte_capacity <=
  provider_spool_per_attempt_byte_capacity <=
  provider_spool_global_byte_capacity`, and
  `manifest_record_byte_capacity <=
  manifest_page_byte_capacity <=
  manifest_staging_per_transaction_byte_capacity <=
  manifest_staging_global_byte_capacity`, and
  `journal_transaction_byte_capacity <= journal_group_byte_capacity` are
  mandatory. Equality is valid. No waiter can therefore request a unit that
  can never fit its owning admission, and a valid standalone transaction
  always fits one physical group.
- Every accepted semantic append holds one global and one lane permit until its
  ticket settles as durable success, typed rejection, or explicit commit
  uncertainty/reconciliation state. Saturation waits cooperatively. It never
  drops, coalesces, runs inline, or converts capacity into a behavioral budget.
  Closing or cancellation wakes blocked submitters with typed results. A stale,
  duplicate, or wrong-lane permit release is rejected and cannot inflate
  capacity.
- Global append admission is deterministic, work-conserving, and fair across
  nonempty registered lanes: lane-ready epochs are queued FIFO and each lane may
  commit at most one bounded group before another already-ready lane receives a
  turn. This uses no elapsed-time, token, payload-size, weight, or adaptive
  threshold. A hot 131K-context stream therefore cannot indefinitely postpone a
  cold scope's terminal append.
- On dequeuing the first accepted semantic transaction, a lane writer
  atomically takes only the currently queued chronological prefix bounded by
  both `journal_group_max_transactions` and
  `journal_group_byte_capacity`; it never waits for the group to fill. It
  encodes that prefix as one physical store batch/durability barrier, settles
  its tickets, performs `Eio.Fiber.yield`, and only then competes for another
  turn. At most one codec/flush group for a lane is in flight.
- A physical group contains separately framed semantic transactions/events.
  Each keeps its event identity, stream cursor, fact ordinals, per-event
  observation sample, atomic reducer boundary, and individual result. Grouping
  never merges, splits, renumbers, reorders, or deduplicates equal deltas.
  Transaction count, encoded transaction bytes, and encoded group bytes are
  separate units. One transaction may contain any number of semantic facts but
  must fit the explicit `journal_transaction_byte_capacity`; otherwise storage
  refuses it with a typed infrastructure result before admission success. A
  valid transaction larger than the remaining group-byte room commits alone
  in the next group and is never split. No ticket reports success before
  durability; group failure or uncertainty fabricates no successful member.
- Each provider attempt atomically acquires its staging-spool lease before its
  stream lease. The stream then uses its frame-bounded FIFO and one
  whole-attempt byte lease under the runtime-global stream admission. Bounded
  ingress, decompression, partial-item retention, canonical scalar chunks,
  ordinal/finalization manifests, and request projection keep one accounted
  byte lineage until durable externalization or typed cleanup. Full capacity
  backpressures before network dispatch/read/copy; terminal, cancellation, or
  failure follows every already accepted frame and wakes both sides typed.
  Parsed deltas enter the same semantic lane/group-commit path, with no second
  transcript authority, timer batch, silent truncation, or coalescing.
- A CPU worker is occupied only while executing CPU work. It does not wait on
  file I/O, callback completion, a response stream, or a composite lifetime.
- Cooperative Eio runners and CPU-only runners are distinct constructors, so
  placement is static and no runtime profiler or elapsed-time heuristic moves
  work across concurrency models.
- Durable codec work is submitted in structural batches: one committed WAL
  batch, one requested page, or one exact comparison range.
- Synchronous collections use structured concurrency. Every synchronously
  awaited structural child receives its parent's execution slot, and the
  suspended parent owns no valid slot; additional concurrent children require
  explicit slots. Active recursive execution therefore never exceeds
  `execution_concurrency` without imposing a turn/depth budget.
- Asynchronous work uses durable queueing and a long-lived owner, not detached
  fibers from a finite Tool scope.
- Operation supervisors acquire global leases before claiming work, claim no
  more than the explicit page size or held leases, and use atomic
  check-and-wait admission. Adding backends therefore cannot multiply active
  operation execution beyond `operation_concurrency`, and a released lease
  cannot be lost between availability check and waiter registration. A
  supervisor retains no execution lease while waiting on unrelated backend
  notification or Journal I/O.
- Event readers use incremental cursors, pages independently bounded by
  `read_batch_size` items and `read_page_byte_capacity` encoded metadata
  bytes, and at most
  `read_stream_concurrency` simultaneously open read-only Journals. A slot is
  released after each grouped stream read rather than retained for a whole
  cross-stream session.
- Commit-action startup uses paged Journal requirement and open-action scans
  under one ordinal-aware high-water position with durable per-fact repair
  claims; it performs
  no unbounded list read and does not let every Agent rescan the Journal.
- Commit-action tailing starts strictly after that same high-water position,
  pages with an explicit request size, reports retention gaps, and receives an
  exact post-commit pending-action trigger without treating trigger delivery as
  completion authority. After `caught_up`, `await_after` blocks without polling,
  detects cross-process commits, and cannot lose an append between cursor check
  and notification arming.
- Observability snapshots do not participate in admission or termination.

## 17. Implementation order and acceptance gate

The first production vertical slice is:

```text
canonical schema
→ existential Tool.t
→ Invocation.context with active switch
→ recursive execution-native journal topology
→ one Agent adapter
→ one heterogeneous synchronous Tool batch
→ provider ToolUse/ToolResult continuation
→ durable, ordered, gap-explicit hierarchical read projection
```

Durable async execution starts only after stable codecs, atomic submission
reconciliation, cancellation, and effect uncertainty are implemented.

This RFC is not accepted by documentation alone. Acceptance requires a real
vertical slice in which a provider calls one Tool that runs a heterogeneous
nested collection, a child performs another Tool call, every node is visible in
the journal hierarchy, the exact ToolResult is projected once in each
continuation request, and no untyped dispatch path is used.

## 18. Evidence

Checked 2026-07-17 05:34 KST:

- [근거] [OCaml 5.4 manual](https://ocaml.org/manual/5.4/index.html) — GADTs,
  modules, variants, domains — confidence High.
- [근거] [Eio Executor_pool](https://ocaml-multicore.github.io/eio/eio/Eio/Executor_pool/index.html)
  — application-owned reusable workers — confidence High.
- [근거] [Eio Fiber](https://ocaml-multicore.github.io/eio/eio/Eio/Fiber/index.html)
  — structured concurrency and cancellation — confidence High.
- [근거] [Eio Switch](https://ocaml-multicore.github.io/eio/eio/Eio/Switch/index.html)
  — explicit fiber/resource lifetime ownership — confidence High.
- [근거] [OpenAI function calling](https://developers.openai.com/api/docs/guides/function-calling)
  and [Responses API reference](https://platform.openai.com/docs/api-reference/responses)
  — typed function-call/output correlation and reasoning-item continuation —
  confidence High.
- [근거] [Anthropic handle tool calls](https://platform.claude.com/docs/en/agents-and-tools/tool-use/handle-tool-calls)
  — ToolUse/ToolResult correlation and adjacency — confidence High.
- [근거] [Gemini function calling](https://ai.google.dev/gemini-api/docs/function-calling)
  — function response and multimodal continuation — confidence High.
- [근거] [Qwen function calling](https://qwen.readthedocs.io/en/stable/framework/function_call.html)
  — thinking/tool-call separation and multi-call continuation — confidence
  High.
- [근거] [Ollama OpenAI compatibility](https://docs.ollama.com/api/openai-compatibility),
  [Streaming](https://docs.ollama.com/api/streaming),
  [Errors](https://docs.ollama.com/api/errors), and
  [Usage](https://docs.ollama.com/api/usage) — partial OpenAI compatibility,
  native NDJSON terminal/error/usage grammar, and provider-scoped request
  vocabulary; checked 2026-07-17 — confidence High.
- [근거] [Ollama Thinking](https://docs.ollama.com/capabilities/thinking) and
  [Structured Outputs](https://docs.ollama.com/capabilities/structured-outputs)
  — GPT-OSS level controls and current Cloud structured-output limitation;
  checked 2026-07-17 — confidence High.
- [근거] [WHATWG Server-Sent Events](https://html.spec.whatwg.org/dev/server-sent-events.html)
  — exact SSE line/event framing rather than substring sentinel detection;
  checked 2026-07-17 — confidence High.
