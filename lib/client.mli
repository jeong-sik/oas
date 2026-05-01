(** High-level SDK client — manages runtime connection, sessions,
    permission callbacks, and message buffering.

    Re-exports types from {!Sdk_client_types} for convenience.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Types (re-exported from Sdk_client_types)} *)

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

type agent_definition = Sdk_client_types.agent_definition =
  { description : string
  ; prompt : string
  ; tools : string list option
  ; model : string option
  }
[@@deriving show]

type options = Sdk_client_types.options =
  { runtime_path : string option
  ; session_root : string option
  ; session_id : string option
  ; resume_session : string option
  ; cwd : string option
  ; permission_mode : permission_mode
  ; model : string option
  ; system_prompt : string option
  ; max_turns : int option
  ; provider : string option
  ; agents : (string * agent_definition) list
  ; include_partial_messages : bool
  ; setting_sources : setting_source list
  }
[@@deriving show]

type message = Sdk_client_types.message =
  | System_message of string
  | Partial_message of
      { participant_name : string
      ; delta : string
      }
  | Session_status of Runtime.session
  | Session_events of Runtime.event list
  | Session_report of Runtime.report
  | Session_proof of Runtime.proof
[@@deriving show]

type permission_result = Sdk_client_types.permission_result =
  | Permission_result_allow of { message : string option }
  | Permission_result_deny of
      { message : string option
      ; interrupt : bool
      }

type tool_permission_context = Sdk_client_types.tool_permission_context =
  { suggestions : string list }

type can_use_tool = Sdk_client_types.can_use_tool

type hook_result = Sdk_client_types.hook_result =
  | Hook_continue
  | Hook_block of string option

type hook_callback = Sdk_client_types.hook_callback

(** {1 Client} *)

(** Opaque client state. *)
type t

(** Default client options. *)
val default_options : options

(** {1 Connection} *)

(** Connect to an OAS runtime process. *)
val connect
  :  sw:Eio.Switch.t
  -> ?clock:float Eio.Time.clock_ty Eio.Resource.t
  -> mgr:_ Eio.Process.mgr
  -> ?options:options
  -> unit
  -> (t, Error.sdk_error) result

(** {1 Queries} *)

(** Submit a user prompt as a new turn. *)
val query : t -> string -> (unit, Error.sdk_error) result

(** Check whether buffered messages are available. *)
val has_pending_messages : t -> bool

(** Drain all buffered messages (non-blocking). *)
val receive_messages : t -> message list

(** Drain buffered messages or block until messages arrive. *)
val receive_response : ?timeout:float -> t -> message list

(** Alias for {!receive_response}. *)
val wait_for_messages : ?timeout:float -> t -> message list

(** Interrupt the current session. *)
val interrupt : t -> (unit, Error.sdk_error) result

(** {1 Configuration} *)

(** Update the permission mode for the current session. *)
val set_permission_mode : t -> permission_mode -> (unit, Error.sdk_error) result

(** Update the model for the current session. *)
val set_model : t -> string option -> (unit, Error.sdk_error) result

(** Set the tool permission callback. *)
val set_can_use_tool : t -> can_use_tool -> unit

(** Set the hook callback. *)
val set_hook_callback : t -> hook_callback -> unit

(** {1 Info} *)

(** Retrieve server information from the runtime. *)
val get_server_info : t -> Runtime.init_response option

(** Get the current session ID, if any. *)
val current_session_id : t -> string option

(** {1 Lifecycle} *)

(** Finalize the current session (generate report and proof). *)
val finalize : t -> ?reason:string -> unit -> (unit, Error.sdk_error) result

(** Disconnect from the runtime. *)
val disconnect : t -> unit

(** Alias for {!disconnect}. *)
val close : t -> unit
