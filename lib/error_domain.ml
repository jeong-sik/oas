(** Fine-grained error domains using polymorphic variants. *)

type provider_error = [
  | `Rate_limited of float option
  | `Auth_error of string
  | `Server_error of int * string
  | `Network_error of string
  | `Provider_timeout of string
  | `Overloaded
  | `Invalid_request of string
  | `Context_overflow of string * int option
]

type tool_error = [
  | `Tool_exec_failed of string * string
  | `Tool_timeout of string * float
]

type agent_error = [
  | `Max_turns_exceeded of int * int
  | `Token_budget_exceeded of int * int
  | `Cost_budget_exceeded
  | `Idle_detected of int
  | `Tool_retry_exhausted of int * int * string
  | `Completion_contract_violation of Completion_contract_id.t * string
  | `Guardrail_violation of string * string
  | `Tripwire_violation of string * string
  | `Unrecognized_stop_reason of string
  | `Exit_condition_met of int
]

type config_error = [
  | `Missing_env_var of string
  | `Unsupported_provider of string
  | `Invalid_config of string * string
]

type mcp_error = [
  | `Mcp_server_start_failed of string * string
  | `Mcp_init_failed of string
  | `Mcp_tool_list_failed of string
  | `Mcp_tool_call_failed of string * string
  | `Mcp_http_failed of string * string
]

type sdk_error_poly = [
  | provider_error
  | tool_error
  | agent_error
  | config_error
  | mcp_error
  | `Serialization of string
  | `Io of string
  | `Orchestration of string
  | `A2a_task_not_found of string
  | `A2a_invalid_transition of string * string * string
  | `A2a_message_send_failed of string * string
  | `A2a_protocol_error of string
  | `A2a_store_capacity_exceeded of int * int
  | `Internal of string
]

(* ── Conversion from Error.sdk_error ────────────────────── *)

let of_api_error (err : Retry.api_error) : provider_error =
  match err with
  | Retry.RateLimited r -> `Rate_limited r.retry_after
  | Retry.AuthError r -> `Auth_error r.message
  | Retry.ServerError r -> `Server_error (r.status, r.message)
  | Retry.NetworkError r -> `Network_error r.message
  | Retry.Timeout r -> `Provider_timeout r.message
  | Retry.Overloaded _ -> `Overloaded
  | Retry.InvalidRequest r -> `Invalid_request r.message
  | Retry.ContextOverflow r -> `Context_overflow (r.message, r.limit)

let of_sdk_error (err : Error.sdk_error) : sdk_error_poly =
  match err with
  | Error.Api err -> (of_api_error err :> sdk_error_poly)
  | Error.Agent (MaxTurnsExceeded r) -> `Max_turns_exceeded (r.turns, r.limit)
  | Error.Agent (TokenBudgetExceeded r) -> `Token_budget_exceeded (r.used, r.limit)
  | Error.Agent (CostBudgetExceeded _) -> `Cost_budget_exceeded
  | Error.Agent (IdleDetected r) -> `Idle_detected r.consecutive_idle_turns
  | Error.Agent (ToolRetryExhausted r) ->
      `Tool_retry_exhausted (r.attempts, r.limit, r.detail)
  | Error.Agent (CompletionContractViolation r) ->
      `Completion_contract_violation (r.contract, r.reason)
  | Error.Agent (GuardrailViolation r) -> `Guardrail_violation (r.validator, r.reason)
  | Error.Agent (TripwireViolation r) -> `Tripwire_violation (r.tripwire, r.reason)
  | Error.Agent (UnrecognizedStopReason r) -> `Unrecognized_stop_reason r.reason
  | Error.Agent (ExitConditionMet r) -> `Exit_condition_met r.turn
  | Error.Config (MissingEnvVar r) -> `Missing_env_var r.var_name
  | Error.Config (UnsupportedProvider r) -> `Unsupported_provider r.detail
  | Error.Config (InvalidConfig r) -> `Invalid_config (r.field, r.detail)
  | Error.Mcp (ServerStartFailed r) -> `Mcp_server_start_failed (r.command, r.detail)
  | Error.Mcp (InitializeFailed r) -> `Mcp_init_failed r.detail
  | Error.Mcp (ToolListFailed r) -> `Mcp_tool_list_failed r.detail
  | Error.Mcp (ToolCallFailed r) -> `Mcp_tool_call_failed (r.tool_name, r.detail)
  | Error.Mcp (HttpTransportFailed r) -> `Mcp_http_failed (r.url, r.detail)
  | Error.Serialization e -> `Serialization (Error.to_string (Error.Serialization e))
  | Error.Io e -> `Io (Error.to_string (Error.Io e))
  | Error.Orchestration e -> `Orchestration (Error.to_string (Error.Orchestration e))
  | Error.A2a (Error.TaskNotFound r) -> `A2a_task_not_found r.task_id
  | Error.A2a (Error.InvalidTransition r) -> `A2a_invalid_transition (r.task_id, r.from_state, r.to_state)
  | Error.A2a (Error.MessageSendFailed r) -> `A2a_message_send_failed (r.task_id, r.detail)
  | Error.A2a (Error.ProtocolError r) -> `A2a_protocol_error r.detail
  | Error.A2a (Error.StoreCapacityExceeded r) -> `A2a_store_capacity_exceeded (r.current, r.max)
  | Error.Internal s -> `Internal s

(* ── Conversion back to Error.sdk_error ─────────────────── *)

let provider_to_api : provider_error -> Retry.api_error = function
  | `Rate_limited after -> Retry.RateLimited { retry_after = after; message = "rate limited" }
  | `Auth_error msg -> Retry.AuthError { message = msg }
  | `Server_error (status, msg) -> Retry.ServerError { status; message = msg }
  | `Network_error msg -> Retry.NetworkError { message = msg }
  | `Provider_timeout msg -> Retry.Timeout { message = msg }
  | `Overloaded -> Retry.Overloaded { message = "overloaded" }
  | `Invalid_request msg -> Retry.InvalidRequest { message = msg }
  | `Context_overflow (msg, limit) -> Retry.ContextOverflow { message = msg; limit }

let to_sdk_error (err : sdk_error_poly) : Error.sdk_error =
  match err with
  | #provider_error as e -> Error.Api (provider_to_api e)
  | `Max_turns_exceeded (turns, limit) ->
    Error.Agent (MaxTurnsExceeded { turns; limit })
  | `Token_budget_exceeded (used, limit) ->
    Error.Agent (TokenBudgetExceeded { kind = "total"; used; limit })
  | `Cost_budget_exceeded ->
    Error.Agent (CostBudgetExceeded { spent_usd = 0.0; limit_usd = 0.0 })
  | `Idle_detected n ->
    Error.Agent (IdleDetected { consecutive_idle_turns = n })
  | `Tool_retry_exhausted (attempts, limit, detail) ->
    Error.Agent (ToolRetryExhausted { attempts; limit; detail })
  | `Completion_contract_violation (contract, reason) ->
    Error.Agent (CompletionContractViolation { contract; reason })
  | `Guardrail_violation (validator, reason) ->
    Error.Agent (GuardrailViolation { validator; reason })
  | `Tripwire_violation (tripwire, reason) ->
    Error.Agent (TripwireViolation { tripwire; reason })
  | `Unrecognized_stop_reason reason ->
    Error.Agent (UnrecognizedStopReason { reason })
  | `Exit_condition_met turn ->
    Error.Agent (ExitConditionMet { turn })
  | `Missing_env_var var ->
    Error.Config (MissingEnvVar { var_name = var })
  | `Unsupported_provider detail ->
    Error.Config (UnsupportedProvider { detail })
  | `Invalid_config (field, detail) ->
    Error.Config (InvalidConfig { field; detail })
  | `Mcp_server_start_failed (command, detail) ->
    Error.Mcp (ServerStartFailed { command; detail })
  | `Mcp_init_failed detail ->
    Error.Mcp (InitializeFailed { detail })
  | `Mcp_tool_list_failed detail ->
    Error.Mcp (ToolListFailed { detail })
  | `Mcp_tool_call_failed (tool_name, detail) ->
    Error.Mcp (ToolCallFailed { tool_name; detail })
  | `Mcp_http_failed (url, detail) ->
    Error.Mcp (HttpTransportFailed { url; detail })
  | `Tool_exec_failed (_name, detail) ->
    Error.Internal (Printf.sprintf "Tool execution failed: %s" detail)
  | `Tool_timeout (name, elapsed) ->
    Error.Internal (Printf.sprintf "Tool %s timed out after %.1fs" name elapsed)
  | `Serialization detail -> Error.Serialization (JsonParseError { detail })
  | `Io detail -> Error.Io (ValidationFailed { detail })
  | `Orchestration detail -> Error.Internal detail
  | `A2a_task_not_found task_id -> Error.a2a_task_not_found task_id
  | `A2a_invalid_transition (task_id, from_state, to_state) ->
    Error.a2a_invalid_transition ~task_id ~from_state ~to_state
  | `A2a_message_send_failed (task_id, detail) ->
    Error.a2a_message_send_failed ~task_id ~detail
  | `A2a_protocol_error detail -> Error.a2a_protocol detail
  | `A2a_store_capacity_exceeded (current, max) ->
    Error.a2a_store_capacity_exceeded ~current ~max
  | `Internal s -> Error.Internal s

(* ── Error with context (moonpool Exn_bt.t inspired) ────── *)

type error_ctx = {
  error: sdk_error_poly;
  stage: string option;
  backtrace: string option;
}

let with_stage stage error =
  { error; stage = Some stage; backtrace = None }

let with_backtrace error =
  { error; stage = None; backtrace = Some (Printexc.get_backtrace ()) }

(* ── String / retryable ─────────────────────────────────── *)

let to_string (err : [< sdk_error_poly]) : string =
  to_sdk_error (err :> sdk_error_poly) |> Error.to_string

let is_retryable (err : [< sdk_error_poly]) : bool =
  match (err :> sdk_error_poly) with
  | `Rate_limited _ | `Server_error _ | `Overloaded | `Provider_timeout _
  | `Network_error _ -> true
  | `Mcp_init_failed _ | `Mcp_tool_list_failed _
  | `Mcp_tool_call_failed _ | `Mcp_http_failed _ -> true
  | _ -> false

let ctx_to_string (ctx : error_ctx) : string =
  let base = to_string ctx.error in
  match ctx.stage with
  | Some stage -> Printf.sprintf "[%s] %s" stage base
  | None -> base
