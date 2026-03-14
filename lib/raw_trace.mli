type record_type =
  | Run_started
  | Assistant_block
  | Tool_execution_started
  | Tool_execution_finished
  | Run_finished
[@@deriving show]

type run_ref = {
  worker_run_id: string;
  path: string;
  start_seq: int;
  end_seq: int;
  agent_name: string;
  session_id: string option;
}
[@@deriving show]

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
  final_text: string option;
  stop_reason: string option;
  error: string option;
}
[@@deriving show]

type t
type active_run

exception Trace_error of Error.sdk_error

val trace_version : int
val create : ?session_id:string -> path:string -> unit -> (t, Error.sdk_error) result
val file_path : t -> string
val session_id : t -> string option
val last_run : t -> run_ref option
val read_all : path:string -> unit -> (record list, Error.sdk_error) result
val read_run : run_ref -> (record list, Error.sdk_error) result

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
val finish_run :
  active_run ->
  final_text:string option ->
  stop_reason:string option ->
  error:string option ->
  (run_ref, Error.sdk_error) result
val raise_if_error : (unit, Error.sdk_error) result -> unit
