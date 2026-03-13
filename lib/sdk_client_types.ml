type permission_mode =
  | Default
  | Accept_edits
  | Plan
  | Bypass_permissions
[@@deriving show]

type setting_source =
  | User
  | Project
  | Local
[@@deriving show]

type agent_definition = {
  description: string;
  prompt: string;
  tools: string list option;
  model: string option;
}
[@@deriving show]

type options = {
  runtime_path: string option;
  session_root: string option;
  cwd: string option;
  permission_mode: permission_mode;
  model: string option;
  system_prompt: string option;
  max_turns: int option;
  provider: string option;
  agents: (string * agent_definition) list;
  include_partial_messages: bool;
  setting_sources: setting_source list;
}
[@@deriving show]

type message =
  | System_message of string
  | Session_status of Runtime.session
  | Session_events of Runtime.event list
  | Session_report of Runtime.report
  | Session_proof of Runtime.proof
[@@deriving show]

type permission_result =
  | Permission_result_allow of { message: string option }
  | Permission_result_deny of { message: string option; interrupt: bool }

type tool_permission_context = {
  suggestions: string list;
}

type can_use_tool =
  string -> Yojson.Safe.t -> tool_permission_context -> permission_result

type hook_result =
  | Hook_continue
  | Hook_block of string option

type hook_callback =
  string -> Yojson.Safe.t -> hook_result

let default_options =
  {
    runtime_path = None;
    session_root = None;
    cwd = None;
    permission_mode = Default;
    model = None;
    system_prompt = None;
    max_turns = Some 8;
    provider = None;
    agents = [];
    include_partial_messages = false;
    setting_sources = [];
  }

let default_agent =
  {
    description = "General purpose assistant";
    prompt = "";
    tools = None;
    model = None;
  }

let agent_entries options =
  match options.agents with
  | [] -> [ ("assistant", default_agent) ]
  | entries -> entries
