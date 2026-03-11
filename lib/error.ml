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
  | UnrecognizedStopReason of { reason: string }

(** MCP client errors. *)
type mcp_error =
  | ServerStartFailed of { command: string; detail: string }
  | InitializeFailed of { detail: string }
  | ToolListFailed of { detail: string }
  | ToolCallFailed of { tool_name: string; detail: string }

(** Configuration errors. *)
type config_error =
  | MissingEnvVar of { var_name: string }
  | UnsupportedProvider of { detail: string }

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

(** Top-level SDK error. *)
type sdk_error =
  | Api of api_error
  | Agent of agent_error
  | Mcp of mcp_error
  | Config of config_error
  | Serialization of serialization_error
  | Io of io_error
  | Orchestration of orchestration_error
  | Internal of string

(* ── Human-readable messages ──────────────────────────────────────── *)

let agent_error_to_string = function
  | MaxTurnsExceeded r ->
    Printf.sprintf "Max turns exceeded (turn %d, limit %d)" r.turns r.limit
  | UnrecognizedStopReason r ->
    Printf.sprintf "Unrecognized stop_reason from API: %s" r.reason

let mcp_error_to_string = function
  | ServerStartFailed r ->
    Printf.sprintf "Failed to start MCP server '%s': %s" r.command r.detail
  | InitializeFailed r ->
    Printf.sprintf "MCP initialize failed: %s" r.detail
  | ToolListFailed r ->
    Printf.sprintf "MCP tools/list failed: %s" r.detail
  | ToolCallFailed r ->
    Printf.sprintf "MCP tools/call '%s' failed: %s" r.tool_name r.detail

let config_error_to_string = function
  | MissingEnvVar r ->
    Printf.sprintf "Missing env var: %s" r.var_name
  | UnsupportedProvider r ->
    Printf.sprintf "Unsupported provider: %s" r.detail

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

let to_string = function
  | Api err -> Retry.error_message err
  | Agent err -> agent_error_to_string err
  | Mcp err -> mcp_error_to_string err
  | Config err -> config_error_to_string err
  | Serialization err -> serialization_error_to_string err
  | Io err -> io_error_to_string err
  | Orchestration err -> orchestration_error_to_string err
  | Internal msg -> Printf.sprintf "Internal error: %s" msg

(* ── Retryability ─────────────────────────────────────────────────── *)

let is_retryable = function
  | Api err -> Retry.is_retryable err
  | Mcp (ServerStartFailed _) -> false
  | Mcp _ -> true
  | Agent _ | Config _ | Serialization _ | Io _ | Orchestration _
  | Internal _ -> false
