(** Private typed ownership chain for one Agent execution journal. Construction
    requires an owned durable writer and never discovers paths or product policy.
    Abstract descendants cannot be moved across another scope. *)

type t
type turn
type provider_attempt
type invocation

type error =
  | Admission_failed of Execution_lane_writer.submit_error
  | Mutation_failed of Execution_lane_writer.ticket_error
  | Invalid_provider_attempt of string
  | Settlement_failed of Execution_tool_settlement.error

val error_to_string : error -> string

(** Start the sole top-level Agent run in [writer]. *)
val start : writer:Execution_lane_writer.t -> agent_name:string -> (t, error) result

val open_turn : t -> ordinal:int -> (turn, error) result

(** Durable producer for Pipeline's exact [before_provider_attempt] boundary. *)
val before_provider_attempt
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

(** The only effect boundary: durable attempt, invoke once, atomic settlement. *)
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
