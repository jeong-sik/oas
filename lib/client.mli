(** High-level SDK client wrapper.

    Re-exports {!Sdk_client_types} and wraps {!Internal_query_engine}
    with a user-facing API for connecting to, querying, and managing
    an agent runtime session. *)

(** {1 Re-exported types} *)

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

(** {1 Client handle} *)

type t

val default_options : options

(** {1 Lifecycle} *)

val connect :
  sw:Eio.Switch.t ->
  mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t ->
  ?options:Sdk_client_types.options ->
  unit ->
  (t, Error.sdk_error) result

val disconnect : t -> unit
val close : t -> unit

(** {1 Query} *)

val query : t -> string -> (unit, Error.sdk_error) result
val interrupt : t -> (unit, Error.sdk_error) result
val finalize : t -> ?reason:string -> unit -> (unit, Error.sdk_error) result

(** {1 Messages} *)

val has_pending_messages : t -> bool
val receive_messages : t -> message list
val receive_response : ?timeout:float -> t -> message list
val wait_for_messages : ?timeout:float -> t -> message list

(** {1 Configuration} *)

val set_permission_mode : t -> permission_mode -> (unit, Error.sdk_error) result
val set_model : t -> string option -> (unit, Error.sdk_error) result
val set_can_use_tool : t -> can_use_tool -> unit
val set_hook_callback : t -> hook_callback -> unit

(** {1 Session} *)

val current_session_id : t -> string option
val get_server_info : t -> Runtime.init_response option
