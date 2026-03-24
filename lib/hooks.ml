(** Lifecycle hooks for agent execution.
    Inspired by Anthropic SDK PreToolUse/PostToolUse/Stop
    and Google ADK ToolContext patterns.

    All hook types use exhaustive variants for compile-time safety. *)

open Types

(** Per-turn adjustable parameters.
    Hooks can return [AdjustParams] from [BeforeTurnParams] to override
    these for a single turn. Values revert to the agent's base config
    on the next turn. *)
type turn_params = {
  temperature: float option;
  thinking_budget: int option;
  tool_choice: tool_choice option;
  extra_system_context: string option;
  tool_filter_override: Guardrails.tool_filter option;
}

let default_turn_params = {
  temperature = None;
  thinking_budget = None;
  tool_choice = None;
  extra_system_context = None;
  tool_filter_override = None;
}

(** Reasoning summary extracted from assistant messages.
    Hooks can inspect this to decide per-turn parameter adjustments. *)
type reasoning_summary = {
  thinking_blocks: string list;
  has_uncertainty: bool;
  tool_rationale: string option;
}

let empty_reasoning_summary = {
  thinking_blocks = [];
  has_uncertainty = false;
  tool_rationale = None;
}

(** Extract reasoning summary from message list.
    Scans for Thinking blocks and heuristically detects uncertainty
    markers like "I'm not sure", "uncertain", "unclear". *)
let extract_reasoning (messages : message list) : reasoning_summary =
  let thinking_blocks = List.concat_map (fun (msg : message) ->
    List.filter_map (function
      | Thinking { content; _ } -> Some content
      | _ -> None
    ) msg.content
  ) messages in
  let all_text = String.concat " " thinking_blocks in
  let uncertainty_markers = [
    "not sure"; "uncertain"; "unclear"; "I'm not confident";
    "might be wrong"; "unsure"; "probably"; "I think";
  ] in
  let has_uncertainty = List.exists (fun marker ->
    try let _ = Str.search_forward (Str.regexp_string_case_fold marker) all_text 0 in true
    with Not_found -> false
  ) uncertainty_markers in
  let tool_rationale =
    (* Look for the last thinking block that mentions tool selection *)
    let tool_markers = ["tool"; "function"; "call"; "use"] in
    List.find_map (fun block ->
      if List.exists (fun marker ->
        try let _ = Str.search_forward (Str.regexp_string_case_fold marker) block 0 in true
        with Not_found -> false
      ) tool_markers then Some block
      else None
    ) (List.rev thinking_blocks)
  in
  { thinking_blocks; has_uncertainty; tool_rationale }

(** Events emitted during agent execution *)
type hook_event =
  | BeforeTurn of { turn: int; messages: message list }
  | BeforeTurnParams of {
      turn: int;
      messages: message list;
      last_tool_results: tool_result list;
      current_params: turn_params;
      reasoning: reasoning_summary;
    }
  | AfterTurn of { turn: int; response: api_response }
  | PreToolUse of { tool_name: string; input: Yojson.Safe.t;
                    accumulated_cost_usd: float; turn: int }
  | PostToolUse of { tool_name: string; input: Yojson.Safe.t;
                     output: Types.tool_result; result_bytes: int }
  | PostToolUseFailure of { tool_name: string; input: Yojson.Safe.t; error: string }
  | OnStop of { reason: stop_reason; response: api_response }
  | OnIdle of { consecutive_idle_turns: int; tool_names: string list }
  | OnError of { detail: string; context: string }
  | OnToolError of { tool_name: string; error: string }
  | PreCompact of {
      messages: message list;
      estimated_tokens: int;
      budget_tokens: int;
    }

(** Elicitation: structured request for user input during agent execution.
    Inspired by Claude SDK MCP Elicitation pattern. *)
type elicitation_request = {
  question: string;
  schema: Yojson.Safe.t option;   (** JSON Schema for expected answer *)
  timeout_s: float option;
}

type elicitation_response =
  | Answer of Yojson.Safe.t
  | Declined
  | Timeout

(** Elicitation callback: called when a hook returns ElicitInput.
    Returns the user's response or Declined/Timeout. *)
type elicitation_callback =
  elicitation_request -> elicitation_response

(** Decision returned by a hook *)
type hook_decision =
  | Continue
  | Skip           (** PreToolUse only: skip this tool execution *)
  | Override of string  (** PreToolUse only: return this value instead *)
  | ApprovalRequired  (** PreToolUse only: signals that tool needs approval before execution *)
  | AdjustParams of turn_params  (** BeforeTurnParams only: override params for this turn *)
  | ElicitInput of elicitation_request  (** Request user input before proceeding *)

(** Decision from approval callback *)
type approval_decision =
  | Approve                      (** Proceed with original input *)
  | Reject of string             (** Block execution with reason *)
  | Edit of Yojson.Safe.t        (** Proceed with modified input *)

(** Approval callback: called when a hook returns ApprovalRequired.
    Receives tool name and input, returns approval decision. *)
type approval_callback =
  tool_name:string -> input:Yojson.Safe.t -> approval_decision

(** A hook function *)
type hook = hook_event -> hook_decision

(** Collection of optional hooks *)
type hooks = {
  before_turn: hook option;
  before_turn_params: hook option;  (** Called before each turn to adjust parameters *)
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

(** Empty hooks -- no-op default *)
let empty = {
  before_turn = None;
  before_turn_params = None;
  after_turn = None;
  pre_tool_use = None;
  post_tool_use = None;
  post_tool_use_failure = None;
  on_stop = None;
  on_idle = None;
  on_error = None;
  on_tool_error = None;
  pre_compact = None;
}

(** Context injection: data returned by a context_injector after tool execution.
    [context_updates] are key-value pairs to set in the shared Context.
    [extra_messages] are appended to the conversation (e.g., system observations). *)
type injection = {
  context_updates: (string * Yojson.Safe.t) list;
  extra_messages: Types.message list;
}

(** Context injector: called after tool execution to inject external state.
    Returns [Some injection] to update context/messages, [None] to skip. *)
type context_injector =
  tool_name:string -> input:Yojson.Safe.t -> output:Types.tool_result ->
  injection option

(** Invoke a hook if present, returning Continue if absent *)
let invoke hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some f -> f event
