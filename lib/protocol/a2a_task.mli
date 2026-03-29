(** A2A Task Lifecycle — Agent-to-Agent protocol task state machine.

    Tasks flow through a well-defined state machine:
    Submitted -> Working -> Completed/Failed/Canceled.
    Illegal transitions return [Error].

    @stability Internal
    @since 0.93.1 *)

(** {1 State machine} *)

type task_state =
  | Submitted
  | Working
  | Input_required
  | Completed
  | Failed
  | Canceled

type transition_error =
  | InvalidTransition of { from_state: task_state; to_state: task_state }
  | TaskAlreadyTerminal of { state: task_state }

val task_state_to_string : task_state -> string
val task_state_of_string : string -> (task_state, string) result
val task_state_to_yojson : task_state -> Yojson.Safe.t
val task_state_of_yojson : Yojson.Safe.t -> (task_state, string) result
val pp_task_state : Format.formatter -> task_state -> unit
val show_task_state : task_state -> string

val valid_transitions : task_state -> task_state list
val is_terminal : task_state -> bool
val transition_error_to_string : transition_error -> string

(** {1 Message and artifact types} *)

type message_part =
  | Text_part of string
  | File_part of { name: string; mime_type: string; data: string }
  | Data_part of Yojson.Safe.t

val message_part_to_yojson : message_part -> Yojson.Safe.t
val message_part_of_yojson : Yojson.Safe.t -> (message_part, string) result
val pp_message_part : Format.formatter -> message_part -> unit
val show_message_part : message_part -> string

type task_role = TaskUser | TaskAgent

type task_message = {
  role: task_role;
  parts: message_part list;
  metadata: (string * Yojson.Safe.t) list;
}

val task_message_to_yojson : task_message -> Yojson.Safe.t
val task_message_of_yojson : Yojson.Safe.t -> (task_message, string) result

type artifact = {
  name: string;
  parts: message_part list;
  metadata: (string * Yojson.Safe.t) list;
}

val artifact_to_yojson : artifact -> Yojson.Safe.t
val artifact_of_yojson : Yojson.Safe.t -> (artifact, string) result

(** {1 Task} *)

type task_id = string

type task = {
  id: task_id;
  state: task_state;
  messages: task_message list;
  artifacts: artifact list;
  metadata: (string * Yojson.Safe.t) list;
  created_at: float;
  updated_at: float;
}

val create : task_message -> task
val transition : task -> task_state -> (task, transition_error) result
val add_message : task -> task_message -> task
val add_artifact : task -> artifact -> task

val task_to_yojson : task -> Yojson.Safe.t
val task_of_yojson : Yojson.Safe.t -> (task, string) result

(** {1 In-memory store} *)

type store

val create_store : ?max_tasks:int -> unit -> store
val evict_if_needed : store -> unit
val store_task : store -> task -> unit
val get_task : store -> task_id -> task option
val list_tasks : store -> task list
