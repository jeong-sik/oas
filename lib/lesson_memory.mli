(** Lesson_memory — helpers for failure-driven autonomy feedback loops.

    Records a failed attempt into episodic + procedural memory and retrieves
    relevant lessons for prompt injection on the next attempt.

    @since 0.92.1 *)

type failure_record = {
  pattern: string;
  summary: string;
  action: string option;
  stdout: string option;
  stderr: string option;
  diff_summary: string option;
  trace_summary: string option;
  metric_name: string option;
  metric_error: string option;
  participants: string list;
  metadata: (string * Yojson.Safe.t) list;
}

type lesson = {
  procedure: Memory.procedure;
  recent_failures: Memory.episode list;
}

val record_failure : Memory.t -> failure_record -> string
(** Store the failure and return the procedure id that was updated. *)

val retrieve_lessons :
  Memory.t ->
  pattern:string ->
  ?limit:int ->
  unit ->
  lesson list

val render_prompt_context : lesson list -> string option
