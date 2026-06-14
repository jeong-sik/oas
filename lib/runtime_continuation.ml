(** Runtime continuation-boundary primitives. *)

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

let policy_for_boundary ?(explicit_interrupt = false) boundary =
  if explicit_interrupt
  then Interrupt_current_turn
  else (
    match boundary with
    | Before_provider_request
    | After_tool_results_before_next_provider_request
    | After_final_answer -> Apply_at_boundary
    | Provider_streaming_reasoning | Before_assistant_tool_use ->
      Queue_until_safe_boundary
    | After_assistant_tool_use_before_results -> Reject_at_boundary)
;;

let reason_of_boundary policy boundary =
  match policy, boundary with
  | Interrupt_current_turn, _ ->
    "explicit interrupt requested; host may cancel and checkpoint before resuming"
  | Apply_at_boundary, Before_provider_request ->
    "safe to add input before the next provider request"
  | Apply_at_boundary, After_tool_results_before_next_provider_request ->
    "tool results are complete; safe to add input before the next provider request"
  | Apply_at_boundary, After_final_answer ->
    "turn is complete; input starts or augments the next turn"
  | Queue_until_safe_boundary, Provider_streaming_reasoning ->
    "provider is still streaming reasoning; queue ordinary input until a safe boundary"
  | Queue_until_safe_boundary, Before_assistant_tool_use ->
    "assistant tool call is selected; queue ordinary input until tool results are closed"
  | Reject_at_boundary, After_assistant_tool_use_before_results ->
    "do not insert user input between assistant tool calls and their tool results"
  | Ignore_for_current_turn, _ -> "input is ignored for the current turn"
  | Apply_at_boundary, _ | Queue_until_safe_boundary, _ | Reject_at_boundary, _ ->
    "boundary policy selected"
;;

let decision ?explicit_interrupt boundary =
  let policy = policy_for_boundary ?explicit_interrupt boundary in
  let accepts_input =
    match policy with
    | Apply_at_boundary | Queue_until_safe_boundary | Interrupt_current_turn -> true
    | Reject_at_boundary | Ignore_for_current_turn -> false
  in
  let applies_input =
    match policy with
    | Apply_at_boundary -> true
    | Queue_until_safe_boundary
    | Reject_at_boundary
    | Interrupt_current_turn
    | Ignore_for_current_turn -> false
  in
  let interrupts_turn =
    match policy with
    | Interrupt_current_turn -> true
    | Apply_at_boundary
    | Queue_until_safe_boundary
    | Reject_at_boundary
    | Ignore_for_current_turn -> false
  in
  let preserves_tool_result_adjacency =
    match boundary with
    | After_assistant_tool_use_before_results -> false
    | Before_provider_request
    | Provider_streaming_reasoning
    | Before_assistant_tool_use
    | After_tool_results_before_next_provider_request
    | After_final_answer -> true
  in
  { boundary
  ; policy
  ; accepts_input
  ; applies_input
  ; interrupts_turn
  ; preserves_tool_result_adjacency
  ; reason = reason_of_boundary policy boundary
  }
;;

let pending_input_policy_to_runtime_status = function
  | Queue_until_safe_boundary -> "queued"
  | Apply_at_boundary -> "applied"
  | Reject_at_boundary -> "ignored"
  | Interrupt_current_turn -> "interrupted"
  | Ignore_for_current_turn -> "ignored"
;;
