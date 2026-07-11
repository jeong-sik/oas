(** Structured SDK error types.

    Replaces [(_, string) result] with [(_, sdk_error) result] across the SDK.
    Provides human-readable [to_string] for backward-compatible error messages
    and [is_retryable] for automated retry decisions.

    @stability Stable
    @since 0.93.1 *)

module Retry = Llm_provider.Retry

(** {1 Domain error types} *)

(** API errors — same type as {!Retry.api_error}. *)
type api_error = Retry.api_error

(** Provider/runtime errors — same type as {!Llm_provider.Error.provider_error}. *)
type provider_error = Llm_provider.Error.provider_error

type input_required =
  { request_id : string
  ; participant_name : string option
  ; question : string
  ; schema : Yojson.Safe.t option
  ; timeout_s : float option
  ; created_at : float
  }

type tool_failure_recovery_stage =
  | Round_projection
  | Episode_detection
  | Judge_response
  | Decision_persistence
  | Resume_restore

val tool_failure_recovery_stage_to_string : tool_failure_recovery_stage -> string

type agent_error =
  | MaxTurnsExceeded of
      { turns : int
      ; limit : int
      }
  | UnrecognizedStopReason of { reason : string }
  | IdleDetected of { consecutive_idle_turns : int }
  | AgentExecutionTimeout of
      { elapsed_sec : float
      ; timeout_sec : float
      ; turn_count : int
      ; max_turns : int
      }
  | AgentExecutionIdleTimeout of
      { idle_sec : float
      ; idle_timeout_sec : float
      ; turn_count : int
      ; max_turns : int
      }
  (** No execution activity (streamed token or completed turn) was
          observed for [idle_timeout_sec]. Distinct from
          [AgentExecutionTimeout], which caps total wall-clock regardless
          of progress: the idle deadline resets on each unit of progress
          and fires only on observed silence, so it does not cancel a run
          that is still streaming output. For non-streaming [run] activity
          is seen only at turn boundaries, so a long single turn can trip
          this without the run being hung.
          @since 0.201.0 *)
  | GuardrailViolation of
      { validator : string
      ; reason : string
      }
  | TripwireViolation of
      { tripwire : string
      ; reason : string
      }
  | InputRequired of input_required
  | ToolFailureRecoveryFailed of
      { stage : tool_failure_recovery_stage
      ; detail : string
      }
  | ToolFailureRecoveryDeferred of
      { reason : string
      ; tool_names : string list
      }
  | ExitConditionMet of { turn : int }

type mcp_error =
  | ServerStartFailed of
      { command : string
      ; detail : string
      }
  | InitializeFailed of { detail : string }
  | ToolListFailed of { detail : string }
  | ToolCallFailed of
      { tool_name : string
      ; detail : string
      }
  | HttpTransportFailed of
      { url : string
      ; detail : string
      }

type config_error =
  | MissingEnvVar of { var_name : string }
  | UnsupportedProvider of { detail : string }
  | InvalidConfig of
      { field : string
      ; detail : string
      }
  | SensitiveValueInConfig of { detail : string }

type serialization_error =
  | JsonParseError of { detail : string }
  | VersionMismatch of
      { expected : int
      ; got : int
      }
  | UnknownVariant of
      { type_name : string
      ; value : string
      }

type io_error =
  | FileOpFailed of
      { op : string
      ; path : string
      ; detail : string
      }
  | ValidationFailed of { detail : string }

type orchestration_error =
  | UnknownAgent of { name : string }
  | TaskTimeout of { task_id : string }
  | DiscoveryFailed of
      { url : string
      ; detail : string
      }

(** {1 Top-level error} *)

type sdk_error =
  | Api of api_error
  | Provider of provider_error
  | Agent of agent_error
  | Mcp of mcp_error
  | Config of config_error
  | Serialization of serialization_error
  | Io of io_error
  | Orchestration of orchestration_error
  | Internal of string

(** {1 Operations} *)

(** Human-readable error message. *)
val to_string : sdk_error -> string

(** Whether the error is transient and the operation can be retried. *)
val is_retryable : sdk_error -> bool
