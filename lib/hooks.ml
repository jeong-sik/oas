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
  | OnStop of { reason: stop_reason; response: api_response }

(** Decision returned by a hook *)
type hook_decision =
  | Continue
  | Skip           (** PreToolUse only: skip this tool execution *)
  | Override of string  (** PreToolUse only: return this value instead *)

(** A hook function *)
type hook = hook_event -> hook_decision

(** Collection of optional hooks *)
type hooks = {
  before_turn: hook option;
  after_turn: hook option;
  pre_tool_use: hook option;
  post_tool_use: hook option;
  on_stop: hook option;
}

(** Empty hooks -- no-op default *)
let empty = {
  before_turn = None;
  after_turn = None;
  pre_tool_use = None;
  post_tool_use = None;
  on_stop = None;
}

(** Invoke a hook if present, returning Continue if absent *)
let invoke hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some f -> f event
