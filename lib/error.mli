(** Structured SDK error types.

    Replaces [(_, string) result] with [(_, sdk_error) result] across the SDK.
    Provides human-readable [to_string] for backward-compatible error messages
    and [is_retryable] for automated retry decisions.

    @stability Stable
    @since 0.93.1 *)

(** {1 Domain error types} *)

(** API errors — same type as {!Retry.api_error}. *)
type api_error = Retry.api_error

type agent_error =
  | MaxTurnsExceeded of { turns: int; limit: int }
  | TokenBudgetExceeded of { kind: string; used: int; limit: int }
  | CostBudgetExceeded of { spent_usd: float; limit_usd: float }
  | UnrecognizedStopReason of { reason: string }
  | IdleDetected of { consecutive_idle_turns: int }
  | ToolRetryExhausted of { attempts: int; limit: int; detail: string }
  | CompletionContractViolation of { contract: string; reason: string }
  | GuardrailViolation of { validator: string; reason: string }
  | TripwireViolation of { tripwire: string; reason: string }
  | ExitConditionMet of { turn: int }

type mcp_error =
  | ServerStartFailed of { command: string; detail: string }
  | InitializeFailed of { detail: string }
  | ToolListFailed of { detail: string }
  | ToolCallFailed of { tool_name: string; detail: string }
  | HttpTransportFailed of { url: string; detail: string }

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
  | DiscoveryFailed of { url: string; detail: string }

(** A2A protocol errors. *)
type a2a_error =
  | TaskNotFound of { task_id: string }
  | InvalidTransition of { task_id: string; from_state: string; to_state: string }
  | MessageSendFailed of { task_id: string; detail: string }
  | ProtocolError of { detail: string }
  | StoreCapacityExceeded of { current: int; max: int }

(** {1 Top-level error} *)

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

(** {1 Operations} *)

val to_string : sdk_error -> string
(** Human-readable error message. *)

val is_retryable : sdk_error -> bool
(** Whether the error is transient and the operation can be retried. *)

(** {1 A2A convenience constructors} *)

val a2a_protocol : string -> sdk_error
val a2a_task_not_found : string -> sdk_error
val a2a_invalid_transition :
  task_id:string -> from_state:string -> to_state:string -> sdk_error
val a2a_message_send_failed : task_id:string -> detail:string -> sdk_error
val a2a_store_capacity_exceeded : current:int -> max:int -> sdk_error
