(** Lifecycle hooks for agent execution.
    Inspired by Claude Code hooks and SDK runtime callbacks. *)

open Types

type permission_request = {
  tool_name : string;
  tool_kind : tool_kind;
  input : Yojson.Safe.t;
  permission_mode : Guardrails.permission_mode;
  reason : string;
}

type hook_event =
  | SessionStart of { session : Session.t; prompt : string }
  | SessionEnd of { session : Session.t; result : (api_response, string) result }
  | UserPromptSubmit of { session : Session.t; prompt : string }
  | BeforeTurn of { turn : int; messages : message list }
  | AfterTurn of { turn : int; response : api_response }
  | PermissionRequest of permission_request
  | PreToolUse of { tool_name : string; input : Yojson.Safe.t }
  | PostToolUse of {
      tool_name : string;
      input : Yojson.Safe.t;
      output : (string, string) result;
    }
  | SubagentStart of { session : Session.t; target_name : string; prompt : string }
  | SubagentStop of {
      session : Session.t;
      target_name : string;
      result : (string, string) result;
    }
  | PreCompact of { session : Session.t; reason : string }
  | ConfigChange of {
      session : Session.t;
      key : string;
      old_value : Yojson.Safe.t option;
      new_value : Yojson.Safe.t;
    }
  | OnStop of { reason : stop_reason; response : api_response }

type hook_decision =
  | Continue
  | Allow
  | Ask
  | Deny of string
  | Skip
  | Override of string

type hook = hook_event -> hook_decision

type hooks = {
  session_start : hook option;
  session_end : hook option;
  user_prompt_submit : hook option;
  before_turn : hook option;
  after_turn : hook option;
  permission_request : hook option;
  pre_tool_use : hook option;
  post_tool_use : hook option;
  subagent_start : hook option;
  subagent_stop : hook option;
  pre_compact : hook option;
  config_change : hook option;
  on_stop : hook option;
}

let empty = {
  session_start = None;
  session_end = None;
  user_prompt_submit = None;
  before_turn = None;
  after_turn = None;
  permission_request = None;
  pre_tool_use = None;
  post_tool_use = None;
  subagent_start = None;
  subagent_stop = None;
  pre_compact = None;
  config_change = None;
  on_stop = None;
}

let invoke hook_opt event =
  match hook_opt with
  | None -> Continue
  | Some fn -> fn event
