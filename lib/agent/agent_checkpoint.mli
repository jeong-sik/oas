(** Checkpoint build/restore logic.

    Takes explicit parameters to avoid circular dependency with [Agent.t].
    The caller wraps results into [Agent.t]. *)

(** {1 Resume} *)

(** State recovered from a checkpoint. *)
type resume_state = {
  state: Types.agent_state;
  context: Context.t;
}

(** Build restored state from a checkpoint.
    Returns state + context; the caller wraps these into [Agent.t]. *)
val build_resume :
  checkpoint:Checkpoint.t ->
  ?config:Types.agent_config ->
  ?context:Context.t ->
  unit -> resume_state

(** {1 Checkpoint creation} *)

(** Build a checkpoint from explicit state parameters.
    The caller extracts fields from [Agent.t] before calling this. *)
val build_checkpoint :
  ?session_id:string ->
  ?working_context:Yojson.Safe.t ->
  state:Types.agent_state ->
  tools:Tool_set.t ->
  context:Context.t ->
  mcp_clients:Mcp.managed list ->
  unit -> Checkpoint.t
