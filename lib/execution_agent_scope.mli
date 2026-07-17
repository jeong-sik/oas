(** Private typed ownership chain for one Agent execution journal.
    Construction requires an owned writer and never discovers paths or product
    policy. Abstract descendants cannot be moved across another scope. *)

type t
type turn
type provider_attempt
type invocation_locator
type invocation

type abort_reason =
  | Failed of Execution_event.failure
  | Cancelled of
      { reason : string option
      ; data : Yojson.Safe.t option
      }

type error =
  | Admission_failed of Execution_lane_writer.submit_error
  | Mutation_failed of Execution_lane_writer.ticket_error
  | Invalid_provider_attempt of string
  | Settlement_failed of Execution_tool_settlement.error

val error_to_string : error -> string
val start : writer:Execution_lane_writer.t -> agent_name:string -> (t, error) result
val open_turn : t -> ordinal:int -> (turn, error) result

(** Materialize the exact binding selected for provider dispatch. *)
val open_provider_attempt
  :  turn
  -> ordinal:int
  -> Binding_identity.t
  -> (provider_attempt, error) result

(** Atomically open an invocation and materialize the exact ToolUse input. *)
val open_invocation
  :  provider_attempt
  -> invocation:Tool.Invocation.t
  -> tool_name:string
  -> input:Yojson.Safe.t
  -> (invocation, error) result

(** Stable opaque coordinates for rebinding after the writer is reopened. *)
val invocation_locator : invocation -> invocation_locator

val rebind_invocation
  :  writer:Execution_lane_writer.t
  -> invocation_locator
  -> (invocation, error) result

(** Commit the attempt before the effect and atomically settle its result. *)
val execute
  :  invocation
  -> invoke:(unit -> Llm_provider.Types.content_block)
  -> (Execution_tool_settlement.execution, error) result

val close_provider_attempt
  :  provider_attempt
  -> Execution_event.terminal
  -> (unit, error) result

val close_turn : turn -> Execution_event.terminal -> (unit, error) result
val finish : t -> Execution_event.terminal -> (unit, error) result

(** Atomically terminate every open descendant and the run root. *)
val abort : t -> abort_reason -> (unit, error) result
