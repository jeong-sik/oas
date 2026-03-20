(** Structured SDK error types — 2-level hierarchy.

    Domain-specific inner types wrap context-rich payloads.
    The top-level [sdk_error] sum type unifies all error domains
    for a single [(_, sdk_error) result] return type across the SDK.

    Design decisions:
    - [api_error] is a type alias for {!Retry.api_error} (no duplication).
    - [Internal] is reserved for unreachable sentinel handlers (handoff.ml).
    - [to_string] produces human-readable messages compatible with v0.8.x. *)

(** API errors — alias for {!Retry.api_error}. *)
type api_error = Retry.api_error

(** Agent runtime errors. *)
type agent_error =
  | MaxTurnsExceeded of { turns: int; limit: int }
  | TokenBudgetExceeded of { kind: string; used: int; limit: int }
  | CostBudgetExceeded of { spent_usd: float; limit_usd: float }
  | UnrecognizedStopReason of { reason: string }
  | IdleDetected of { consecutive_idle_turns: int }
  | GuardrailViolation of { validator: string; reason: string }

(** MCP client errors. *)
type mcp_error =
  | ServerStartFailed of { command: string; detail: string }
  | InitializeFailed of { detail: string }
  | ToolListFailed of { detail: string }
  | ToolCallFailed of { tool_name: string; detail: string }
  | HttpTransportFailed of { url: string; detail: string }

(** Configuration errors. *)
type config_error =
  | MissingEnvVar of { var_name: string }
  | UnsupportedProvider of { detail: string }
  | InvalidConfig of { field: string; detail: string }

(** Serialization / deserialization errors. *)
type serialization_error =
  | JsonParseError of { detail: string }
  | VersionMismatch of { expected: int; got: int }
  | UnknownVariant of { type_name: string; value: string }

(** File I/O and validation errors. *)
type io_error =
  | FileOpFailed of { op: string; path: string; detail: string }
  | ValidationFailed of { detail: string }

(** Multi-agent orchestration errors. *)
type orchestration_error =
  | UnknownAgent of { name: string }
  | TaskTimeout of { task_id: string }
  | DiscoveryFailed of { url: string; detail: string }

(** A2A protocol errors. *)
type a2a_error =
  | TaskNotFound of { task_id: string }
  | InvalidTransition of { task_id: string; from_state: string; to_state: string }
  | MessageSendFailed of { task_id: string; detail: string }
  | ProtocolError of { detail: string }
  | StoreCapacityExceeded of { current: int; max: int }

(** Top-level SDK error. *)
type sdk_error =
  | Api of api_error
  | Agent of agent_error
  | Mcp of mcp_error
  | Config of config_error
  | Serialization of serialization_error
  | Io of io_error
  | Orchestration of orchestration_error
  | A2a of a2a_error
  | Internal of string

(* ── Human-readable messages ──────────────────────────────────────── *)

let agent_error_to_string = function
  | MaxTurnsExceeded r ->
    Printf.sprintf "Max turns exceeded (turn %d, limit %d)" r.turns r.limit
  | TokenBudgetExceeded r ->
    Printf.sprintf "%s token budget exceeded: %d/%d" r.kind r.used r.limit
  | CostBudgetExceeded r ->
    Printf.sprintf "Cost budget exceeded: $%.4f spent (limit $%.4f)" r.spent_usd r.limit_usd
  | UnrecognizedStopReason r ->
    Printf.sprintf "Unrecognized stop_reason from API: %s" r.reason
  | IdleDetected r ->
    Printf.sprintf "Idle detected: %d consecutive identical tool call turns" r.consecutive_idle_turns
  | GuardrailViolation r ->
    Printf.sprintf "Guardrail violation [%s]: %s" r.validator r.reason

let mcp_error_to_string = function
  | ServerStartFailed r ->
    Printf.sprintf "Failed to start MCP server '%s': %s" r.command r.detail
  | InitializeFailed r ->
    Printf.sprintf "MCP initialize failed: %s" r.detail
  | ToolListFailed r ->
    Printf.sprintf "MCP tools/list failed: %s" r.detail
  | ToolCallFailed r ->
    Printf.sprintf "MCP tools/call '%s' failed: %s" r.tool_name r.detail
  | HttpTransportFailed r ->
    Printf.sprintf "MCP HTTP transport failed for %s: %s" r.url r.detail

let config_error_to_string = function
  | MissingEnvVar r ->
    Printf.sprintf "Missing env var: %s" r.var_name
  | UnsupportedProvider r ->
    Printf.sprintf "Unsupported provider: %s" r.detail
  | InvalidConfig r ->
    Printf.sprintf "Invalid config '%s': %s" r.field r.detail

let serialization_error_to_string = function
  | JsonParseError r ->
    Printf.sprintf "JSON parse error: %s" r.detail
  | VersionMismatch r ->
    Printf.sprintf "Version mismatch: expected %d, got %d" r.expected r.got
  | UnknownVariant r ->
    Printf.sprintf "Unknown %s variant: %s" r.type_name r.value

let io_error_to_string = function
  | FileOpFailed r ->
    Printf.sprintf "File %s failed on %s: %s" r.op r.path r.detail
  | ValidationFailed r ->
    Printf.sprintf "Validation failed: %s" r.detail

let orchestration_error_to_string = function
  | UnknownAgent r ->
    Printf.sprintf "Unknown agent: %s" r.name
  | TaskTimeout r ->
    Printf.sprintf "Task timed out: %s" r.task_id
  | DiscoveryFailed r ->
    Printf.sprintf "Agent discovery failed for %s: %s" r.url r.detail

let a2a_error_to_string = function
  | TaskNotFound r ->
    Printf.sprintf "A2A task not found: %s" r.task_id
  | InvalidTransition r ->
    Printf.sprintf "A2A invalid transition: %s -> %s (task %s)" r.from_state r.to_state r.task_id
  | MessageSendFailed r ->
    Printf.sprintf "A2A message send failed for task %s: %s" r.task_id r.detail
  | ProtocolError r ->
    Printf.sprintf "A2A protocol error: %s" r.detail
  | StoreCapacityExceeded r ->
    Printf.sprintf "A2A store capacity exceeded: %d/%d" r.current r.max

let to_string = function
  | Api err -> Retry.error_message err
  | Agent err -> agent_error_to_string err
  | Mcp err -> mcp_error_to_string err
  | Config err -> config_error_to_string err
  | Serialization err -> serialization_error_to_string err
  | Io err -> io_error_to_string err
  | Orchestration err -> orchestration_error_to_string err
  | A2a err -> a2a_error_to_string err
  | Internal msg -> Printf.sprintf "Internal error: %s" msg

(* ── Retryability ─────────────────────────────────────────────────── *)

let is_retryable = function
  | Api err -> Retry.is_retryable err
  | Mcp (ServerStartFailed _) -> false
  | Mcp _ -> true
  | Agent _ | Config _ | Serialization _ | Io _ | Orchestration _
  | A2a _ | Internal _ -> false

(* ── A2A convenience constructors ────────────────────────────────── *)

let a2a_protocol detail = A2a (ProtocolError { detail })
let a2a_task_not_found task_id = A2a (TaskNotFound { task_id })
let a2a_invalid_transition ~task_id ~from_state ~to_state =
  A2a (InvalidTransition { task_id; from_state; to_state })
let a2a_message_send_failed ~task_id ~detail =
  A2a (MessageSendFailed { task_id; detail })
let a2a_store_capacity_exceeded ~current ~max =
  A2a (StoreCapacityExceeded { current; max })
