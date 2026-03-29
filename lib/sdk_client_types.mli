(** Wire types for the SDK client protocol.

    Defines options, messages, permission callbacks, and hook callbacks
    used by {!Client} and {!Internal_query_engine}.

    @stability Internal
    @since 0.93.1 *)

(** {1 Permission Mode} *)

type permission_mode =
  | Default
  | Accept_edits
  | Plan
  | Bypass_permissions
[@@deriving show]

val string_of_permission_mode : permission_mode -> string
val permission_mode_of_string : string -> permission_mode option

(** {1 Setting Source} *)

type setting_source =
  | User
  | Project
  | Local
[@@deriving show]

(** {1 Agent Definition} *)

type agent_definition = {
  description: string;
  prompt: string;
  tools: string list option;
  model: string option;
}
[@@deriving show]

(** {1 Options} *)

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

(** {1 Permission Callbacks} *)

type permission_result =
  | Permission_result_allow of { message: string option }
  | Permission_result_deny of { message: string option; interrupt: bool }

type tool_permission_context = {
  suggestions: string list;
}

type can_use_tool =
  string -> Yojson.Safe.t -> tool_permission_context -> permission_result

(** {1 Hook Callbacks} *)

type hook_result =
  | Hook_continue
  | Hook_block of string option

type hook_callback =
  string -> Yojson.Safe.t -> hook_result

(** {1 Defaults} *)

val default_options : options
val default_agent : agent_definition

(** Return the agent entries from options, defaulting to a single
    ["assistant"] entry if none are configured. *)
val agent_entries : options -> (string * agent_definition) list
