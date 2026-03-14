(** Structured SDK error types.

    Replaces [(_, string) result] with [(_, sdk_error) result] across the SDK.
    Provides human-readable [to_string] for backward-compatible error messages
    and [is_retryable] for automated retry decisions. *)

(** {1 Domain error types} *)

(** API errors — same type as {!Retry.api_error}. *)
type api_error = Retry.api_error

type agent_error =
  | MaxTurnsExceeded of { turns: int; limit: int }
  | TokenBudgetExceeded of { kind: string; used: int; limit: int }
  | UnrecognizedStopReason of { reason: string }

type mcp_error =
  | ServerStartFailed of { command: string; detail: string }
  | InitializeFailed of { detail: string }
  | ToolListFailed of { detail: string }
  | ToolCallFailed of { tool_name: string; detail: string }

type config_error =
  | MissingEnvVar of { var_name: string }
  | UnsupportedProvider of { detail: string }
  | InvalidConfig of { field: string; detail: string }

type serialization_error =
  | JsonParseError of { detail: string }
  | VersionMismatch of { expected: int; got: int }
  | UnknownVariant of { type_name: string; value: string }

type io_error =
  | FileOpFailed of { op: string; path: string; detail: string }
  | ValidationFailed of { detail: string }

type orchestration_error =
  | UnknownAgent of { name: string }
  | TaskTimeout of { task_id: string }

(** {1 Top-level error} *)

type sdk_error =
  | Api of api_error
  | Agent of agent_error
  | Mcp of mcp_error
  | Config of config_error
  | Serialization of serialization_error
  | Io of io_error
  | Orchestration of orchestration_error
  | Internal of string

(** {1 Operations} *)

val to_string : sdk_error -> string
(** Human-readable error message.  Output is compatible with v0.8.x
    string error messages for smooth migration. *)

val is_retryable : sdk_error -> bool
(** Whether the error is transient and the operation can be retried.
    Delegates to {!Retry.is_retryable} for [Api] errors.
    MCP errors except [ServerStartFailed] are retryable.
    All other domains return [false]. *)
