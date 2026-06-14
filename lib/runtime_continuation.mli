(** Runtime continuation-boundary primitives.

    These types describe when host-provided input may be applied to an active
    agent turn without violating provider/tool ordering. They are provider- and
    runtime-agnostic: OAS exposes the policy; MASC or another host owns the
    queue and UI wiring.

    @since 0.207.0 *)

type continuation_boundary =
  | Before_provider_request
  | Provider_streaming_reasoning
  | Before_assistant_tool_use
  | After_assistant_tool_use_before_results
  | After_tool_results_before_next_provider_request
  | After_final_answer
[@@deriving yojson, show]

type pending_input_policy =
  | Queue_until_safe_boundary
  | Apply_at_boundary
  | Reject_at_boundary
  | Interrupt_current_turn
  | Ignore_for_current_turn
[@@deriving yojson, show]

type pending_input_decision =
  { boundary : continuation_boundary
  ; policy : pending_input_policy
  ; accepts_input : bool
  ; applies_input : bool
  ; interrupts_turn : bool
  ; preserves_tool_result_adjacency : bool
  ; reason : string
  }
[@@deriving yojson, show]

val policy_for_boundary
  :  ?explicit_interrupt:bool
  -> continuation_boundary
  -> pending_input_policy

val decision : ?explicit_interrupt:bool -> continuation_boundary -> pending_input_decision
val pending_input_policy_to_runtime_status : pending_input_policy -> string
