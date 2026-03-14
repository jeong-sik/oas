(** Lifecycle hooks for agent execution.
    Inspired by Anthropic SDK PreToolUse/PostToolUse/Stop
    and Google ADK ToolContext patterns.

    All hook types use exhaustive variants for compile-time safety. *)

open Types

(** Events emitted during agent execution *)
type hook_event =
  | BeforeTurn of { turn: int; messages: message list }
  | AfterTurn of { turn: int; response: api_response }
  | PreToolUse of { tool_name: string; input: Yojson.Safe.t }
  | PostToolUse of { tool_name: string; input: Yojson.Safe.t; output: (string, string) result }
  | PostToolUseFailure of { tool_name: string; input: Yojson.Safe.t; error: string }
  | OnStop of { reason: stop_reason; response: api_response }

(** Decision returned by a hook *)
type hook_decision =
  | Continue
  | Skip           (** PreToolUse only: skip this tool execution *)
  | Override of string  (** PreToolUse only: return this value instead *)
  | ApprovalRequired  (** PreToolUse only: signals that tool needs approval before execution *)

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
  after_turn: hook option;
  pre_tool_use: hook option;
  post_tool_use: hook option;
  post_tool_use_failure: hook option;
  on_stop: hook option;
}

(** Empty hooks -- no-op default *)
let empty = {
  before_turn = None;
  after_turn = None;
  pre_tool_use = None;
  post_tool_use = None;
  post_tool_use_failure = None;
  on_stop = None;
}

(** Invoke a hook if present, returning Continue if absent *)
let invoke hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some f -> f event
