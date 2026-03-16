type record_type =
  | Run_started
  | Assistant_block
  | Tool_execution_started
  | Tool_execution_finished
  | Hook_invoked
  | Run_finished
[@@deriving yojson, show]

type run_ref = {
  worker_run_id: string;
  path: string;
  start_seq: int;
  end_seq: int;
  agent_name: string;
  session_id: string option;
}
[@@deriving yojson, show]

type run_summary = {
  run_ref: run_ref;
  record_count: int;
  assistant_block_count: int;
  tool_execution_started_count: int;
  tool_execution_finished_count: int;
  hook_invoked_count: int;
  hook_names: string list;
  tool_names: string list;
  final_text: string option;
  stop_reason: string option;
  error: string option;
  started_at: float option;
  finished_at: float option;
}
[@@deriving yojson, show]

type validation_check = {
  name: string;
  passed: bool;
}
[@@deriving yojson, show]

type run_validation = {
  run_ref: run_ref;
  ok: bool;
  checks: validation_check list;
  evidence: string list;
  paired_tool_result_count: int;
  has_file_write: bool;
  verification_pass_after_file_write: bool;
  final_text: string option;
  tool_names: string list;
  stop_reason: string option;
  failure_reason: string option;
}
[@@deriving yojson, show]

type record = {
  trace_version: int;
  worker_run_id: string;
  seq: int;
  ts: float;
  agent_name: string;
  session_id: string option;
  record_type: record_type;
  prompt: string option;
  block_index: int option;
  block_kind: string option;
  assistant_block: Yojson.Safe.t option;
  tool_use_id: string option;
  tool_name: string option;
  tool_input: Yojson.Safe.t option;
  tool_result: string option;
  tool_error: bool option;
  hook_name: string option;
  hook_decision: string option;
  hook_detail: string option;
  final_text: string option;
  stop_reason: string option;
  error: string option;
}
[@@deriving yojson, show]

type t
type active_run

exception Trace_error of Error.sdk_error

val trace_version : int
val create : ?session_id:string -> path:string -> unit -> (t, Error.sdk_error) result
val create_for_session :
  ?session_root:string ->
  session_id:string ->
  agent_name:string ->
  unit ->
  (t, Error.sdk_error) result
val file_path : t -> string
val session_id : t -> string option
val last_run : t -> run_ref option
val read_all : path:string -> unit -> (record list, Error.sdk_error) result
val record_to_json : record -> Yojson.Safe.t

(** Internal append helpers used by the direct Agent loop. *)
val start_run :
  t -> agent_name:string -> prompt:string ->
  (active_run, Error.sdk_error) result
val record_assistant_block :
  active_run -> block_index:int -> Types.content_block ->
  (unit, Error.sdk_error) result
val record_tool_execution_started :
  active_run ->
  tool_use_id:string ->
  tool_name:string ->
  tool_input:Yojson.Safe.t ->
  (unit, Error.sdk_error) result
val record_tool_execution_finished :
  active_run ->
  tool_use_id:string ->
  tool_name:string ->
  tool_result:string ->
  tool_error:bool ->
  (unit, Error.sdk_error) result
val record_hook_invoked :
  active_run ->
  hook_name:string ->
  hook_decision:string ->
  ?hook_detail:string ->
  unit ->
  (unit, Error.sdk_error) result
val finish_run :
  active_run ->
  final_text:string option ->
  stop_reason:string option ->
  error:string option ->
  (run_ref, Error.sdk_error) result
val raise_if_error : ('a, Error.sdk_error) result -> unit
val active_run_id : active_run -> string
