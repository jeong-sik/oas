(** Fine-grained error domains using polymorphic variants.

    Each domain constrains which errors a function can produce,
    eliminating defensive [| _ -> assert false] patterns.

    Usage:
    {[
      (* Provider function: only produces provider errors

    @stability Internal
    @since 0.93.1 *)
      val create_message : ... -> (api_response, [> provider_error]) result

      (* Callers only handle relevant variants *)
      match result with
      | Error (`Rate_limited r) -> retry r
      | Error (`Auth_error msg) -> fail msg
      | Ok response -> ...
    ]}
*)

(** {1 Provider errors} *)

type provider_error = [
  | `Rate_limited of float option  (** retry_after seconds *)
  | `Auth_error of string
  | `Server_error of int * string  (** status, message *)
  | `Network_error of string
  | `Provider_timeout of string
  | `Overloaded
  | `Invalid_request of string
  | `Context_overflow of string * int option
]

(** {1 Tool errors} *)

type tool_error = [
  | `Tool_exec_failed of string * string  (** tool_name, detail *)
  | `Tool_timeout of string * float       (** tool_name, elapsed *)
]

(** {1 Agent errors} *)

type agent_error = [
  | `Max_turns_exceeded of int * int  (** turns, limit *)
  | `Token_budget_exceeded of int * int  (** used, limit *)
  | `Cost_budget_exceeded
  | `Idle_detected of int  (** consecutive_idle_turns *)
  | `Tool_retry_exhausted of int * int * string  (** attempts, limit, detail *)
  | `Completion_contract_violation of Completion_contract_id.t * string  (** contract, reason *)
  | `Guardrail_violation of string * string  (** validator, reason *)
  | `Tripwire_violation of string * string  (** tripwire, reason *)
  | `Unrecognized_stop_reason of string
  | `Exit_condition_met of int  (** turn at which exit condition triggered *)
]

(** {1 Infrastructure errors} *)

type config_error = [
  | `Missing_env_var of string
  | `Unsupported_provider of string
  | `Invalid_config of string * string  (** field, detail *)
]

type mcp_error = [
  | `Mcp_server_start_failed of string * string  (** command, detail *)
  | `Mcp_init_failed of string
  | `Mcp_tool_list_failed of string
  | `Mcp_tool_call_failed of string * string  (** tool_name, detail *)
  | `Mcp_http_failed of string * string  (** url, detail *)
]

(** {1 Union type} *)

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

(** {1 Error with context}

    Inspired by moonpool's [Exn_bt.t] pattern: bundle errors with
    execution context (pipeline stage, backtrace) for debuggability.
    Polymorphic variants are lightweight but lose context — this
    compensates. *)

type error_ctx = {
  error: sdk_error_poly;
  stage: string option;      (** pipeline stage where error occurred *)
  backtrace: string option;  (** Printexc.get_backtrace snapshot *)
}

(** Wrap an error with pipeline stage context. *)
val with_stage : string -> sdk_error_poly -> error_ctx

(** Wrap an error capturing the current backtrace. *)
val with_backtrace : sdk_error_poly -> error_ctx

(** {1 Conversion} *)

val of_sdk_error : Error.sdk_error -> sdk_error_poly
val to_sdk_error : sdk_error_poly -> Error.sdk_error
val to_string : [< sdk_error_poly] -> string
val is_retryable : [< sdk_error_poly] -> bool

(** Context-aware to_string: includes stage if present. *)
val ctx_to_string : error_ctx -> string
