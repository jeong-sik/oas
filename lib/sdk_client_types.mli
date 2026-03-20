(** SDK client type definitions. *)

(** {1 Permission mode} *)

type permission_mode =
  | Default
  | Accept_edits
  | Plan
  | Bypass_permissions
[@@deriving show]

val string_of_permission_mode : permission_mode -> string
val permission_mode_of_string : string -> permission_mode option

(** {1 Settings} *)

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

(** {1 Messages} *)

type message =
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

(** {1 Permissions} *)

type permission_result =
  | Permission_result_allow of { message: string option }
  | Permission_result_deny of { message: string option; interrupt: bool }

type tool_permission_context = {
  suggestions: string list;
}

type can_use_tool =
  string -> Yojson.Safe.t -> tool_permission_context -> permission_result

(** {1 Hooks} *)

type hook_result =
  | Hook_continue
  | Hook_block of string option

type hook_callback =
  string -> Yojson.Safe.t -> hook_result

(** {1 Defaults} *)

val default_options : options
val default_agent : agent_definition
val agent_entries : options -> (string * agent_definition) list
