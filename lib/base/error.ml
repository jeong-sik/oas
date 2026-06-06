(** Structured SDK error types — 2-level hierarchy.

    Domain-specific inner types wrap context-rich payloads.
    The top-level [sdk_error] sum type unifies all error domains
    for a single [(_, sdk_error) result] return type across the SDK.

    Design decisions:
    - [api_error] is a type alias for {!Retry.api_error} (no duplication).
    - [Internal] is reserved for unreachable sentinel handlers (handoff.ml).
    - [to_string] produces human-readable messages compatible with v0.8.x. *)

module Retry = Llm_provider.Retry

(** API errors — alias for {!Retry.api_error}. *)
type api_error = Retry.api_error

(** Provider/runtime errors — alias for {!Llm_provider.Error.provider_error}. *)
type provider_error = Llm_provider.Error.provider_error

type input_required =
  { request_id : string
  ; participant_name : string option
  ; question : string
  ; schema : Yojson.Safe.t option
  ; timeout_s : float option
  ; created_at : float
  }

(** Agent runtime errors. *)
type agent_error =
  | MaxTurnsExceeded of
      { turns : int
      ; limit : int
      }
  | TokenBudgetExceeded of
      { kind : string
      ; used : int
      ; limit : int
      }
  | CostBudgetExceeded of
      { spent_usd : float
      ; limit_usd : float
      }
  | CostBudgetUnenforceable of
      { model_id : string
      ; limit_usd : float
      }
  | UnrecognizedStopReason of { reason : string }
  | IdleDetected of { consecutive_idle_turns : int }
  | ToolRetryExhausted of
      { attempts : int
      ; limit : int
      ; detail : string
      }
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
  | ExitConditionMet of { turn : int }

(** MCP client errors. *)
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

(** Configuration errors. *)
type config_error =
  | MissingEnvVar of { var_name : string }
  | UnsupportedProvider of { detail : string }
  | InvalidConfig of
      { field : string
      ; detail : string
      }

(** Serialization / deserialization errors. *)
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

(** File I/O and validation errors. *)
type io_error =
  | FileOpFailed of
      { op : string
      ; path : string
      ; detail : string
      }
  | ValidationFailed of { detail : string }

(** Multi-agent orchestration errors. *)
type orchestration_error =
  | UnknownAgent of { name : string }
  | TaskTimeout of { task_id : string }
  | DiscoveryFailed of
      { url : string
      ; detail : string
      }

(** Top-level SDK error. *)
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

(* ── Human-readable messages ──────────────────────────────────────── *)

let agent_error_to_string = function
  | MaxTurnsExceeded r ->
    Printf.sprintf "Max turns exceeded (turn %d, limit %d)" r.turns r.limit
  | TokenBudgetExceeded r ->
    Printf.sprintf "%s token budget exceeded: %d/%d" r.kind r.used r.limit
  | CostBudgetExceeded r ->
    Printf.sprintf
      "Legacy cost threshold exceeded: $%.4f spent (threshold $%.4f); cost is advisory"
      r.spent_usd
      r.limit_usd
  | CostBudgetUnenforceable r ->
    Printf.sprintf
      "Legacy cost threshold ($%.4f) saw unpriced model %S; cost is advisory and does \
       not gate execution"
      r.limit_usd
      r.model_id
  | UnrecognizedStopReason r ->
    Printf.sprintf "Unrecognized stop_reason from API: %s" r.reason
  | IdleDetected r ->
    Printf.sprintf
      "Idle detected: %d consecutive identical tool call turns"
      r.consecutive_idle_turns
  | ToolRetryExhausted r ->
    Printf.sprintf
      "Tool retry budget exhausted after %d/%d retries: %s"
      r.attempts
      r.limit
      r.detail
  | AgentExecutionTimeout r ->
    Printf.sprintf
      "Agent execution timed out after %.1fs (max_execution_time_s=%.1fs, turns=%d/%d)"
      r.elapsed_sec
      r.timeout_sec
      r.turn_count
      r.max_turns
  | AgentExecutionIdleTimeout r ->
    Printf.sprintf
      "Agent stalled: no progress for %.1fs (execution_idle_timeout_s=%.1fs, turns=%d/%d)"
      r.idle_sec
      r.idle_timeout_sec
      r.turn_count
      r.max_turns
  | GuardrailViolation r ->
    Printf.sprintf "Guardrail violation [%s]: %s" r.validator r.reason
  | TripwireViolation r ->
    Printf.sprintf "Tripwire violation [%s]: %s" r.tripwire r.reason
  | InputRequired r ->
    Printf.sprintf
      "Input required%s: %s (request %s)"
      (match r.participant_name with
       | Some participant -> Printf.sprintf " for %s" participant
       | None -> "")
      r.question
      r.request_id
  | ExitConditionMet r -> Printf.sprintf "Exit condition met at turn %d" r.turn
;;

let mcp_error_to_string = function
  | ServerStartFailed r ->
    Printf.sprintf "Failed to start MCP server '%s': %s" r.command r.detail
  | InitializeFailed r -> Printf.sprintf "MCP initialize failed: %s" r.detail
  | ToolListFailed r -> Printf.sprintf "MCP tools/list failed: %s" r.detail
  | ToolCallFailed r ->
    Printf.sprintf "MCP tools/call '%s' failed: %s" r.tool_name r.detail
  | HttpTransportFailed r ->
    Printf.sprintf "MCP HTTP transport failed for %s: %s" r.url r.detail
;;

let config_error_to_string = function
  | MissingEnvVar r -> Printf.sprintf "Missing env var: %s" r.var_name
  | UnsupportedProvider r -> Printf.sprintf "Unsupported provider: %s" r.detail
  | InvalidConfig r -> Printf.sprintf "Invalid config '%s': %s" r.field r.detail
;;

let serialization_error_to_string = function
  | JsonParseError r -> Printf.sprintf "JSON parse error: %s" r.detail
  | VersionMismatch r ->
    Printf.sprintf "Version mismatch: expected %d, got %d" r.expected r.got
  | UnknownVariant r -> Printf.sprintf "Unknown %s variant: %s" r.type_name r.value
;;

let io_error_to_string = function
  | FileOpFailed r -> Printf.sprintf "File %s failed on %s: %s" r.op r.path r.detail
  | ValidationFailed r -> Printf.sprintf "Validation failed: %s" r.detail
;;

let orchestration_error_to_string = function
  | UnknownAgent r -> Printf.sprintf "Unknown agent: %s" r.name
  | TaskTimeout r -> Printf.sprintf "Task timed out: %s" r.task_id
  | DiscoveryFailed r -> Printf.sprintf "Agent discovery failed for %s: %s" r.url r.detail
;;

let to_string = function
  | Api err -> Retry.error_message err
  | Provider err -> Llm_provider.Error.to_string err
  | Agent err -> agent_error_to_string err
  | Mcp err -> mcp_error_to_string err
  | Config err -> config_error_to_string err
  | Serialization err -> serialization_error_to_string err
  | Io err -> io_error_to_string err
  | Orchestration err -> orchestration_error_to_string err
  | Internal msg -> Printf.sprintf "Internal error: %s" msg
;;

(* ── Retryability ─────────────────────────────────────────────────── *)

let is_retryable = function
  | Api err -> Retry.is_retryable err
  | Provider err -> Llm_provider.Error.is_retryable err
  | Mcp (ServerStartFailed _) -> false
  | Mcp _ -> true
  | Agent _ | Config _ | Serialization _ | Io _ | Orchestration _ | Internal _ -> false
;;
