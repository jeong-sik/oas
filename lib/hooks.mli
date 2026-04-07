(** Lifecycle hooks for agent execution.

    @stability Stable
    @since 0.93.1 *)

(** Per-turn adjustable parameters.
    Returned via [AdjustParams] from [BeforeTurnParams] hook. *)
type turn_params = {
  temperature: float option;
  thinking_budget: int option;
  tool_choice: Types.tool_choice option;
  extra_system_context: string option;
  system_prompt_override: string option;
  tool_filter_override: Guardrails.tool_filter option;
}

val default_turn_params : turn_params

(** Reasoning summary extracted from assistant messages. *)
type reasoning_summary = {
  thinking_blocks: string list;
  has_uncertainty: bool;
  tool_rationale: string option;
}

val empty_reasoning_summary : reasoning_summary
val extract_reasoning : Types.message list -> reasoning_summary

(** Deterministic scheduling metadata attached to a tool execution plan.
    [batch_kind] is one of ["parallel"], ["sequential"], or ["exclusive"]. *)
type tool_schedule = {
  planned_index: int;
  batch_index: int;
  batch_size: int;
  concurrency_class: string;
  batch_kind: string;
}

(** Events emitted during agent execution *)
type hook_event =
  | BeforeTurn of { turn: int; messages: Types.message list }
  | BeforeTurnParams of {
      turn: int;
      max_turns: int;
      messages: Types.message list;
      last_tool_results: Types.tool_result list;
      current_params: turn_params;
      reasoning: reasoning_summary;
    }
  | AfterTurn of { turn: int; response: Types.api_response }
  | PreToolUse of {
      tool_use_id: string;
      tool_name: string;
      input: Yojson.Safe.t;
      accumulated_cost_usd: float;
      turn: int;
      schedule: tool_schedule;
    }
  | PostToolUse of {
      tool_use_id: string;
      tool_name: string;
      input: Yojson.Safe.t;
      output: Types.tool_result;
      result_bytes: int;
      duration_ms: float;
      schedule: tool_schedule;
    }
  | PostToolUseFailure of {
      tool_use_id: string;
      tool_name: string;
      input: Yojson.Safe.t;
      error: string;
      schedule: tool_schedule;
    }
  | OnStop of { reason: Types.stop_reason; response: Types.api_response }
  | OnIdle of { consecutive_idle_turns: int; tool_names: string list }
  | OnError of { detail: string; context: string }
  | OnToolError of { tool_name: string; error: string }
  | PreCompact of {
      messages: Types.message list;
      estimated_tokens: int;
      budget_tokens: int;
    }

(** Elicitation: structured request for user input during agent execution. *)
type elicitation_request = {
  question: string;
  schema: Yojson.Safe.t option;
  timeout_s: float option;
}

type elicitation_response =
  | Answer of Yojson.Safe.t
  | Declined
  | Timeout

type elicitation_callback = elicitation_request -> elicitation_response

(** Decision returned by a hook *)
type hook_decision =
  | Continue
  | Skip
  | Override of string
  | ApprovalRequired
      (** Signals that the tool needs external approval.  If an
          {!approval_callback} is registered the callback is invoked;
          otherwise the tool is executed with a warning log. *)
  | AdjustParams of turn_params
  | ElicitInput of elicitation_request
  | Nudge of string  (** OnIdle only: inject message, reset idle counter, continue *)

(** Decision from approval callback *)
type approval_decision =
  | Approve
  | Reject of string
  | Edit of Yojson.Safe.t

(** Approval callback: called when a hook returns ApprovalRequired *)
type approval_callback =
  tool_name:string -> input:Yojson.Safe.t -> approval_decision

type hook = hook_event -> hook_decision

(** Collection of optional hooks *)
type hooks = {
  before_turn: hook option;
  before_turn_params: hook option;
  after_turn: hook option;
  pre_tool_use: hook option;
  post_tool_use: hook option;
  post_tool_use_failure: hook option;
  on_stop: hook option;
  on_idle: hook option;
  on_error: hook option;
  on_tool_error: hook option;
  pre_compact: hook option;
}

(** Context injection: data returned by a context_injector after tool execution *)
type injection = {
  context_updates: (string * Yojson.Safe.t) list;
  extra_messages: Types.message list;
}

(** Context injector: called after tool execution to inject external state *)
type context_injector =
  tool_name:string -> input:Yojson.Safe.t -> output:Types.tool_result ->
  injection option

val empty : hooks
val invoke : hook option -> hook_event -> hook_decision

(** {2 Decision validity matrix}

    Each hook stage accepts only a subset of decisions.
    Returning an unlisted decision is a programming error.

    {v
    Stage                | Continue | Skip | Override | ApprovalRequired | AdjustParams | ElicitInput | Nudge
    ---------------------+----------+------+----------+------------------+--------------+-------------+-------
    before_turn          |    Y     |      |          |                  |              |      Y
    before_turn_params   |    Y     |      |          |                  |      Y       |
    after_turn           |    Y     |      |          |                  |              |
    pre_tool_use         |    Y     |  Y   |    Y     |        Y         |              |
    post_tool_use        |    Y     |      |          |                  |              |
    post_tool_use_failure|    Y     |      |          |                  |              |
    on_stop              |    Y     |      |          |                  |              |
    on_idle              |    Y     |  Y   |          |                  |              |             |   Y
    on_error             |    Y     |      |          |                  |              |
    on_tool_error        |    Y     |      |          |                  |              |
    pre_compact          |    Y     |  Y   |          |                  |              |
    v}

    Fail-closed: unknown stages reject all decisions. *)

(** Classification tag for hook_decision, without payload. *)
type hook_decision_kind =
  | K_Continue
  | K_Skip
  | K_Override
  | K_ApprovalRequired
  | K_AdjustParams
  | K_ElicitInput
  | K_Nudge

(** Extract the kind tag from a decision value. *)
val classify_decision : hook_decision -> hook_decision_kind

(** Human-readable name for a decision kind. *)
val decision_kind_to_string : hook_decision_kind -> string

(** Extract the stage name from a hook_event. *)
val stage_of_event : hook_event -> string

(** Return the list of legal decision kinds for the named stage.
    Returns empty list for unknown stages (fail-closed). *)
val legal_decisions_for_stage : string -> hook_decision_kind list

(** Validate a decision against the matrix for the given stage.
    Returns [Ok decision] when legal, [Error msg] otherwise. *)
val validate_decision : stage:string -> hook_decision -> (hook_decision, string) result

(** Like [invoke], but validates the decision against the matrix.
    Illegal decisions fall back to [Continue]; [on_illegal] is called
    with diagnostics when a violation is detected. *)
val invoke_validated :
  ?on_illegal:(stage:string -> decision:hook_decision -> msg:string -> unit) ->
  hook option -> hook_event -> hook_decision

(** Compose two hook sets. [outer] fires first for each slot.
    If [outer] returns a non-Continue decision, [inner] is bypassed.
    If [outer] returns [Continue], [inner] fires and its decision is used. *)
val compose : outer:hooks -> inner:hooks -> hooks
