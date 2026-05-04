(** Lifecycle hooks for agent execution.
    Inspired by Anthropic SDK PreToolUse/PostToolUse/Stop
    and Google ADK ToolContext patterns.

    All hook types use exhaustive variants for compile-time safety. *)

open Types

(** Per-turn adjustable parameters.
    Hooks can return [AdjustParams] from [BeforeTurnParams] to override
    these for a single turn. Values revert to the agent's base config
    on the next turn. *)
type turn_params =
  { temperature : float option
  ; thinking_budget : int option
  ; enable_thinking : bool option
  ; tool_choice : tool_choice option
  ; extra_system_context : string option
  ; system_prompt_override : string option
  ; tool_filter_override : Guardrails.tool_filter option
  }

let default_turn_params =
  { temperature = None
  ; thinking_budget = None
  ; enable_thinking = None
  ; tool_choice = None
  ; extra_system_context = None
  ; system_prompt_override = None
  ; tool_filter_override = None
  }
;;

(** Reasoning summary extracted from assistant messages.
    Hooks can inspect this to decide per-turn parameter adjustments. *)
type reasoning_summary =
  { thinking_blocks : string list
  ; has_uncertainty : bool
  ; tool_rationale : string option
  }

let empty_reasoning_summary =
  { thinking_blocks = []; has_uncertainty = false; tool_rationale = None }
;;

type tool_schedule =
  { planned_index : int
  ; batch_index : int
  ; batch_size : int
  ; concurrency_class : string
  ; batch_kind : string
  }

module Idle_severity = struct
  type t =
    | Nudge
    | Final_warning
    | Skip

  let to_string = function
    | Nudge -> "nudge"
    | Final_warning -> "final_warning"
    | Skip -> "skip"
  ;;
end

(** Extract reasoning summary from message list.
    Scans for Thinking blocks and heuristically detects uncertainty
    markers like "I'm not sure", "uncertain", "unclear". *)
let extract_reasoning (messages : message list) : reasoning_summary =
  let thinking_blocks =
    List.concat_map
      (fun (msg : message) ->
         List.filter_map
           (function
             | Thinking { content; _ } -> Some content
             | _ -> None)
           msg.content)
      messages
  in
  let all_text = String.concat " " thinking_blocks in
  let uncertainty_markers =
    [ "not sure"
    ; "uncertain"
    ; "unclear"
    ; "I'm not confident"
    ; "might be wrong"
    ; "unsure"
    ; "probably"
    ; "I think"
    ]
  in
  let has_uncertainty =
    List.exists
      (fun marker -> Util.regex_match (Str.regexp_string_case_fold marker) all_text)
      uncertainty_markers
  in
  let tool_rationale =
    (* Look for the last thinking block that mentions tool selection *)
    let tool_markers = [ "tool"; "function"; "call"; "use" ] in
    List.find_map
      (fun block ->
         if
           List.exists
             (fun marker -> Util.regex_match (Str.regexp_string_case_fold marker) block)
             tool_markers
         then Some block
         else None)
      (List.rev thinking_blocks)
  in
  { thinking_blocks; has_uncertainty; tool_rationale }
;;

(** Events emitted during agent execution *)
type hook_event =
  | BeforeTurn of
      { turn : int
      ; messages : message list
      }
  | BeforeTurnParams of
      { turn : int
      ; max_turns : int
      ; messages : message list
      ; last_tool_results : tool_result list
      ; current_params : turn_params
      ; reasoning : reasoning_summary
      }
  | AfterTurn of
      { turn : int
      ; response : api_response
      }
  | PreToolUse of
      { tool_use_id : string
      ; tool_name : string
      ; input : Yojson.Safe.t
      ; accumulated_cost_usd : float
      ; turn : int
      ; schedule : tool_schedule
      }
  | PostToolUse of
      { tool_use_id : string
      ; tool_name : string
      ; input : Yojson.Safe.t
      ; output : Types.tool_result
      ; result_bytes : int
      ; duration_ms : float
      ; schedule : tool_schedule
      }
  | PostToolUseFailure of
      { tool_use_id : string
      ; tool_name : string
      ; input : Yojson.Safe.t
      ; error : string
      ; schedule : tool_schedule
      }
  | OnStop of
      { reason : stop_reason
      ; response : api_response
      }
  | OnIdle of
      { consecutive_idle_turns : int
      ; tool_names : string list
      }
  | OnIdleEscalated of
      { severity : Idle_severity.t
      ; consecutive_idle_turns : int
      ; tool_names : string list
      }
  | OnError of
      { detail : string
      ; context : string
      }
  | OnToolError of
      { tool_name : string
      ; error : string
      }
  | PreCompact of
      { messages : message list
      ; estimated_tokens : int
      ; budget_tokens : int
      }
  | PostCompact of
      { before_messages : message list
      ; after_messages : message list
      ; before_tokens : int
      ; after_tokens : int
      ; phase : string
      }
  | OnContextCompacted of
      { agent_name : string
      ; before_tokens : int
      ; after_tokens : int
      ; phase : string
      }

(** Elicitation: structured request for user input during agent execution.
    Inspired by Claude SDK MCP Elicitation pattern. *)
type elicitation_request =
  { question : string
  ; schema : Yojson.Safe.t option (** JSON Schema for expected answer *)
  ; timeout_s : float option
  }

type elicitation_response =
  | Answer of Yojson.Safe.t
  | Declined
  | Timeout

(** Elicitation callback: called when a hook returns ElicitInput.
    Returns the user's response or Declined/Timeout. *)
type elicitation_callback = elicitation_request -> elicitation_response

(** Decision returned by a hook *)
type hook_decision =
  | Continue
  | Skip
  (** PreToolUse: skip this tool execution; OnIdle: gracefully stop the agent run *)
  | Override of string (** PreToolUse only: return this value instead *)
  | ApprovalRequired
  (** PreToolUse only: signals that tool needs approval before execution *)
  | AdjustParams of turn_params
  (** BeforeTurnParams only: override params for this turn *)
  | ElicitInput of elicitation_request (** Request user input before proceeding *)
  | Nudge of string
  (** OnIdle and BeforeTurn: inject a nudge message into the conversation as a User-role message and continue execution. On OnIdle the idle counter is preserved (deliberate: lets the host hook escalate via Skip). On BeforeTurn the message is appended before tool preparation and reaches the model in the same turn. *)

(** Decision from approval callback *)
type approval_decision =
  | Approve (** Proceed with original input *)
  | Reject of string (** Block execution with reason *)
  | Edit of Yojson.Safe.t (** Proceed with modified input *)

(** Approval callback: called when a hook returns ApprovalRequired.
    Receives tool name and input, returns approval decision. *)
type approval_callback = tool_name:string -> input:Yojson.Safe.t -> approval_decision

(** A hook function *)
type hook = hook_event -> hook_decision

(** Collection of optional hooks *)
type hooks =
  { before_turn : hook option
  ; before_turn_params : hook option (** Called before each turn to adjust parameters *)
  ; after_turn : hook option
  ; pre_tool_use : hook option
  ; post_tool_use : hook option
  ; post_tool_use_failure : hook option
  ; on_stop : hook option
  ; on_idle : hook option
  ; on_idle_escalated : hook option
  ; on_error : hook option
  ; on_tool_error : hook option
  ; pre_compact : hook option
  ; post_compact : hook option
  ; on_context_compacted : hook option
  }

(** Empty hooks -- no-op default *)
let empty =
  { before_turn = None
  ; before_turn_params = None
  ; after_turn = None
  ; pre_tool_use = None
  ; post_tool_use = None
  ; post_tool_use_failure = None
  ; on_stop = None
  ; on_idle = None
  ; on_idle_escalated = None
  ; on_error = None
  ; on_tool_error = None
  ; pre_compact = None
  ; post_compact = None
  ; on_context_compacted = None
  }
;;

(** Context injection: data returned by a context_injector after tool execution.
    [context_updates] are key-value pairs to set in the shared Context.
    [extra_messages] are appended to the conversation (e.g., system observations). *)
type injection =
  { context_updates : (string * Yojson.Safe.t) list
  ; extra_messages : Types.message list
  }

(** Context injector: called after tool execution to inject external state.
    Returns [Some injection] to update context/messages, [None] to skip. *)
type context_injector =
  tool_name:string -> input:Yojson.Safe.t -> output:Types.tool_result -> injection option

(** Classification of hook_decision variants for the decision matrix.
    Using a separate type avoids comparing functional values
    (AdjustParams, ElicitInput carry payloads). *)
type hook_decision_kind =
  | K_Continue
  | K_Skip
  | K_Override
  | K_ApprovalRequired
  | K_AdjustParams
  | K_ElicitInput
  | K_Nudge

let classify_decision = function
  | Continue -> K_Continue
  | Skip -> K_Skip
  | Override _ -> K_Override
  | ApprovalRequired -> K_ApprovalRequired
  | AdjustParams _ -> K_AdjustParams
  | ElicitInput _ -> K_ElicitInput
  | Nudge _ -> K_Nudge
;;

let decision_kind_to_string = function
  | K_Continue -> "Continue"
  | K_Skip -> "Skip"
  | K_Override -> "Override"
  | K_ApprovalRequired -> "ApprovalRequired"
  | K_AdjustParams -> "AdjustParams"
  | K_ElicitInput -> "ElicitInput"
  | K_Nudge -> "Nudge"
;;

(** Extract a stage name string from a hook_event. *)
let stage_of_event = function
  | BeforeTurn _ -> "before_turn"
  | BeforeTurnParams _ -> "before_turn_params"
  | AfterTurn _ -> "after_turn"
  | PreToolUse _ -> "pre_tool_use"
  | PostToolUse _ -> "post_tool_use"
  | PostToolUseFailure _ -> "post_tool_use_failure"
  | OnStop _ -> "on_stop"
  | OnIdle _ -> "on_idle"
  | OnIdleEscalated _ -> "on_idle_escalated"
  | OnError _ -> "on_error"
  | OnToolError _ -> "on_tool_error"
  | PreCompact _ -> "pre_compact"
  | PostCompact _ -> "post_compact"
  | OnContextCompacted _ -> "on_context_compacted"
;;

(** Legal decision matrix.

    {v
    Stage                | Continue | Skip | Override | ApprovalRequired | AdjustParams | ElicitInput | Nudge
    ---------------------+----------+------+----------+------------------+--------------+-------------+-------
    before_turn          |    Y     |      |          |                  |              |      Y      |   Y
    before_turn_params   |    Y     |      |          |                  |      Y       |             |
    after_turn           |    Y     |      |          |                  |              |             |
    pre_tool_use         |    Y     |  Y   |    Y     |        Y         |              |             |
    post_tool_use        |    Y     |      |          |                  |              |             |
    post_tool_use_failure|    Y     |      |          |                  |              |             |
    on_stop              |    Y     |      |          |                  |              |             |
    on_idle              |    Y     |  Y   |          |                  |              |             |   Y
    on_idle_escalated    |    Y     |  Y   |          |                  |              |             |   Y
    on_error             |    Y     |      |          |                  |              |             |
    on_tool_error        |    Y     |      |          |                  |              |             |
    pre_compact          |    Y     |  Y   |          |                  |              |             |
    post_compact         |    Y     |      |          |                  |              |             |
    v}

    Fail-closed: any decision not explicitly listed is rejected. *)
let legal_decisions_for_stage stage =
  match stage with
  | "before_turn" -> [ K_Continue; K_ElicitInput; K_Nudge ]
  | "before_turn_params" -> [ K_Continue; K_AdjustParams ]
  | "after_turn" -> [ K_Continue ]
  | "pre_tool_use" -> [ K_Continue; K_Skip; K_Override; K_ApprovalRequired ]
  | "post_tool_use" -> [ K_Continue ]
  | "post_tool_use_failure" -> [ K_Continue ]
  | "on_stop" -> [ K_Continue ]
  | "on_idle" -> [ K_Continue; K_Skip; K_Nudge ]
  | "on_idle_escalated" -> [ K_Continue; K_Skip; K_Nudge ]
  | "on_error" -> [ K_Continue ]
  | "on_tool_error" -> [ K_Continue ]
  | "pre_compact" -> [ K_Continue; K_Skip ]
  | "post_compact" -> [ K_Continue ]
  | "on_context_compacted" -> [ K_Continue ]
  | _ -> [] (* unknown stage: nothing is legal *)
;;

(** Validate that a hook_decision is legal for a given stage.
    Fail-closed: unknown stages and unlisted decisions return Error. *)
let validate_decision ~stage decision =
  let kind = classify_decision decision in
  let legal = legal_decisions_for_stage stage in
  if List.mem kind legal
  then Ok decision
  else (
    let msg =
      Printf.sprintf
        "illegal hook decision %s at stage %s; legal: [%s]"
        (decision_kind_to_string kind)
        stage
        (String.concat ", " (List.map decision_kind_to_string legal))
    in
    Error msg)
;;

(** Invoke a hook if present, returning Continue if absent *)
let invoke hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some f -> f event
;;

(** Invoke a hook with decision validation.
    If the hook returns an illegal decision for the stage,
    falls back to [Continue] and calls [on_illegal] (if provided). *)
let invoke_validated ?on_illegal hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some f ->
    let decision = f event in
    let stage = stage_of_event event in
    (match validate_decision ~stage decision with
     | Ok d -> d
     | Error msg ->
       (match on_illegal with
        | Some cb -> cb ~stage ~decision ~msg
        | None -> ());
       Continue)
;;

(** Compose a single hook slot. [outer] fires first.
    If outer returns a non-Continue decision, inner is bypassed. *)
let compose_hook (outer : hook option) (inner : hook option) : hook option =
  match outer, inner with
  | None, None -> None
  | Some _, None -> outer
  | None, Some _ -> inner
  | Some f_outer, Some f_inner ->
    Some
      (fun event ->
        match f_outer event with
        | Continue -> f_inner event
        | decision -> decision)
;;

let compose ~outer ~inner =
  { before_turn = compose_hook outer.before_turn inner.before_turn
  ; before_turn_params = compose_hook outer.before_turn_params inner.before_turn_params
  ; after_turn = compose_hook outer.after_turn inner.after_turn
  ; pre_tool_use = compose_hook outer.pre_tool_use inner.pre_tool_use
  ; post_tool_use = compose_hook outer.post_tool_use inner.post_tool_use
  ; post_tool_use_failure =
      compose_hook outer.post_tool_use_failure inner.post_tool_use_failure
  ; on_stop = compose_hook outer.on_stop inner.on_stop
  ; on_idle = compose_hook outer.on_idle inner.on_idle
  ; on_idle_escalated = compose_hook outer.on_idle_escalated inner.on_idle_escalated
  ; on_error = compose_hook outer.on_error inner.on_error
  ; on_tool_error = compose_hook outer.on_tool_error inner.on_tool_error
  ; pre_compact = compose_hook outer.pre_compact inner.pre_compact
  ; post_compact = compose_hook outer.post_compact inner.post_compact
  ; on_context_compacted =
      compose_hook outer.on_context_compacted inner.on_context_compacted
  }
;;
