type permission_mode = Sdk_client_types.permission_mode =
  | Default
  | Accept_edits
  | Plan
  | Bypass_permissions
[@@deriving show]

type setting_source = Sdk_client_types.setting_source =
  | User
  | Project
  | Local
[@@deriving show]

type agent_definition = Sdk_client_types.agent_definition = {
  description: string;
  prompt: string;
  tools: string list option;
  model: string option;
}
[@@deriving show]

type options = Sdk_client_types.options = {
  runtime_path: string option;
  session_root: string option;
  session_id: string option;
  resume_session: string option;
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

type message = Sdk_client_types.message =
  | System_message of string
  | Partial_message of {
      participant_name: string;
      delta: string;
    }
  | Session_status of Runtime.session
  | Session_events of Runtime.event list
  | Session_report of Runtime.report
  | Session_proof of Runtime.proof
[@@deriving show]

type permission_result = Sdk_client_types.permission_result =
  | Permission_result_allow of { message: string option }
  | Permission_result_deny of { message: string option; interrupt: bool }

type tool_permission_context = Sdk_client_types.tool_permission_context = {
  suggestions: string list;
}

type can_use_tool = Sdk_client_types.can_use_tool

type hook_result = Sdk_client_types.hook_result =
  | Hook_continue
  | Hook_block of string option

type hook_callback = Sdk_client_types.hook_callback

type t = Internal_query_engine.t

let default_options = Sdk_client_types.default_options

let connect ~sw ~mgr = Internal_query_engine.connect ~sw ~mgr
let query = Internal_query_engine.query_turn
let has_pending_messages = Internal_query_engine.has_pending_messages
let receive_messages = Internal_query_engine.receive_messages
let receive_response = Internal_query_engine.wait_for_messages
let wait_for_messages = Internal_query_engine.wait_for_messages
let interrupt = Internal_query_engine.interrupt
let set_permission_mode = Internal_query_engine.set_permission_mode
let set_model = Internal_query_engine.set_model
let set_can_use_tool = Internal_query_engine.set_can_use_tool
let set_hook_callback = Internal_query_engine.set_hook_callback
let get_server_info state = Runtime_client.get_server_info state.Internal_query_engine.runtime
let current_session_id = Internal_query_engine.current_session_id
let finalize = Internal_query_engine.finalize
let disconnect = Internal_query_engine.close
let close = Internal_query_engine.close
